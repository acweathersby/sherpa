#![allow(unused)]

use std::{
  future::{self, Future},
  num::NonZeroUsize,
  pin::Pin,
  sync::{
    mpsc::{channel, sync_channel, Receiver, SyncSender},
    Arc,
    LockResult,
    Mutex,
  },
  task::{Context, Poll, RawWaker, RawWakerVTable, Waker},
  thread,
  time::Duration,
};

struct Task<OutType: 'static + Send> {
  future:
    Mutex<Option<Pin<Box<dyn Future<Output = OutType> + 'static + Send>>>>,
  task_sender: SyncSender<Arc<Task<OutType>>>,
}

impl<OutType: 'static + Send> Task<OutType> {
  const RAW_VTABLE: RawWakerVTable = RawWakerVTable::new(
    Task::<OutType>::clone,
    Task::<OutType>::wake,
    Task::<OutType>::wake_by_ref,
    Task::<OutType>::drop,
  );

  unsafe fn clone(ptr: *const ()) -> RawWaker {
    let ptr = Arc::<Task<OutType>>::from_raw(ptr as *mut Task<OutType>);
    std::mem::forget(ptr.clone());
    RawWaker::new(
      Arc::<Task<OutType>>::into_raw(ptr) as *const (),
      &Task::<OutType>::RAW_VTABLE,
    )
  }

  unsafe fn wake(ptr: *const ()) {
    let ptr = Arc::<Task<OutType>>::from_raw(ptr as *mut Task<OutType>);
    let task_sender = &ptr.task_sender;
    task_sender.send(ptr.clone()).expect("too many tasks queued");
  }

  unsafe fn wake_by_ref(ptr: *const ()) {
    (*(ptr as *const Task<OutType>))
      .task_sender
      .send(Arc::<Task<OutType>>::from_raw(ptr as *mut Task<OutType>))
      .expect("too many tasks queued");
  }

  unsafe fn drop(ptr: *const ()) {
    let ptr = Arc::<Task<OutType>>::from_raw(ptr as *mut Task<OutType>);
  }
}

struct Worker {
  id:     usize,
  thread: thread::JoinHandle<()>,
}

impl Worker {
  fn new<OutType: 'static + Send>(
    id: usize,
    ready_queue: Arc<Mutex<Receiver<Arc<Task<OutType>>>>>,
  ) -> Self {
    let thread = thread::Builder::new()
      .name("thread".to_string() + &id.to_string())
      .spawn(move || {
        loop {
          let task = { ready_queue.lock().unwrap().recv() };

          if let Ok(task) = task {
            // Wait for a pending thread.

            println!("Thread {} received task", id);

            //Dispatch the task on the respective thread.
            let mut future_slot = task.future.lock().unwrap();
            if let Some(mut future) = future_slot.take() {
              let data = task.clone();
              let raw_waker = RawWaker::new(
                Arc::<Task<OutType>>::into_raw(data) as *const (),
                &Task::<OutType>::RAW_VTABLE,
              );
              let waker = unsafe { Waker::from_raw(raw_waker) };

              let context = &mut Context::from_waker(&waker);

              if future.as_mut().poll(context).is_pending() {
                *future_slot = Some(future)
              }
            }
          } else {
            break;
          }
        }
      })
      .unwrap();

    Self { id, thread }
  }

  fn join(self) {
    self.thread.join();
  }
}

pub(crate) struct Executor {
  // threads
  threads: Vec<Worker>,
}

impl Executor {
  fn new<OutType: 'static + Send>(
    ready_queue: Arc<Mutex<Receiver<Arc<Task<OutType>>>>>,
    thread_count: NonZeroUsize,
  ) -> Self {
    let size = thread_count.get();

    let mut threads = Vec::with_capacity(size);

    for id in 0..size {
      threads.push(Worker::new(id, ready_queue.clone()))
    }

    Self { threads }
  }

  pub fn join(self) {
    for thread in self.threads {
      thread.thread.join();
    }
  }

  pub fn execute<OutType: 'static + Send, R: 'static + Send + Default>(
    self,
    future: impl Future<Output = OutType> + 'static + Send,
    spawner: Spawner<R>,
  ) -> OutType {
    let mut result: Arc<Mutex<Option<OutType>>> = Arc::new(Mutex::new(None));
    let shared_result = result.clone();
    let threaded_future = async move {
      let result = future.await;

      (*shared_result.lock().unwrap()) = Some(result);

      R::default()
    };

    spawner.spawn(threaded_future);
    drop(spawner);

    let result = loop {
      {
        match result.lock() {
          LockResult::Err(err) => {
            panic!("Thread failed!");
          }
          LockResult::Ok(mut guard) => match guard.take() {
            Some(output) => break output,
            None => {}
          },
          _ => {}
        }
      }
      std::thread::sleep(Duration::from_micros(500))
    };

    self.join();

    result
  }
}

#[derive(Clone)]
pub struct Spawner<OutType: 'static + Send> {
  task_sender: SyncSender<Arc<Task<OutType>>>,
}

impl<OutType: 'static + Send> Spawner<OutType> {
  pub fn spawn(&self, future: impl Future<Output = OutType> + 'static + Send) {
    let future = Box::pin(future);
    let task = Arc::new(Task {
      future:      Mutex::new(Some(future)),
      task_sender: self.task_sender.clone(),
    });
    self.task_sender.send(task).expect("too many tasks queued");
  }
}

/// Shared state between the future and the waiting thread
struct SharedState<T: 'static + Send> {
  result: Option<T>,
  waker:  Option<Waker>,
}

pub(crate) struct ThreadedFuture<T: 'static + Send> {
  signal: Arc<Mutex<SharedState<T>>>,
}

impl<T: 'static + Send> ThreadedFuture<T> {
  pub fn new<R: 'static + Send + Default>(
    future: impl Future<Output = T> + 'static + Send,
    spawner: &Spawner<R>,
  ) -> Self {
    let shared_state =
      Arc::new(Mutex::new(SharedState { result: None, waker: None }));

    let own_signal = shared_state.clone();

    let threaded_future = async move {
      let result = future.await;

      let mut shared_state = shared_state.lock().unwrap();

      shared_state.result = Some(result);

      if let Some(waker) = shared_state.waker.take() {
        waker.wake()
      }

      R::default()
    };

    spawner.spawn(threaded_future);

    Self { signal: own_signal }
  }
}

impl<T: Send> Future for ThreadedFuture<T> {
  // Every future has to specify what type of value it returns when it resolves.
  // This particular future will return a u16.
  type Output = T;

  // The `Future` trait has only one method, named "poll".
  fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
    let mut shared_state = self.signal.lock().unwrap();
    if let Some(val) = shared_state.result.take() {
      Poll::Ready(val)
    } else {
      shared_state.waker = Some(cx.waker().clone());
      Poll::Pending
    }
  }
}

pub(crate) fn new_taskman<OutType: 'static + Send>(
  max_queued_tasks: usize,
) -> (Executor, Spawner<OutType>) {
  let (task_sender, ready_queue) = sync_channel(max_queued_tasks);

  (
    Executor::new(
      Arc::new(Mutex::new(ready_queue)),
      NonZeroUsize::new(8).unwrap(),
    ),
    Spawner { task_sender },
  )
}

#[test]
fn test_taskman() {
  // Grammar Cloud

  let (executor, spawner) = new_taskman::<()>(1000);

  let sp = spawner.clone();

  sp.spawn(async move {
    let spawner = spawner.clone();
    let grammar = crate::grammar::parse_grammar(
      r#"
      <> A > "A" "B" "C"
      <> B > "A" "B" "C"
      <> C > "A" "B" "C"
      <> D > "A" "B" "C"
      <> A > "A" "B" "C"
      <> B > "A" "B" "C"
      <> C > "A" "B" "C"
      <> D > "A" "B" "C"
      <> A > "A" "B" "C"
      <> B > "A" "B" "C"
      <> C > "A" "B" "C"
      <> D > "A" "B" "C"
      <> A > "A" "B" "C"
      <> B > "A" "B" "C"
      <> C > "A" "B" "C"
      <> D > "A" "B" "C"
    "#,
    )
    .unwrap();

    //let grammar = Arc::new(grammar);
    let len = grammar.productions.len();
    let range = (0..grammar.productions.len());
    let vec = range.collect::<Vec<_>>();

    let grammar = Arc::new(grammar);

    let mut groups =
      vec.chunks((len as f64 / 4.0).ceil() as usize).collect::<Vec<_>>();

    let chunk_1 = groups.pop().unwrap().to_vec();
    let chunk_2 = groups.pop().unwrap_or(&[]).to_vec();
    let chunk_3 = groups.pop().unwrap_or(&[]).to_vec();
    let chunk_4 = groups.pop().unwrap_or(&[]).to_vec();

    println!("{chunk_1:?}, {chunk_4:?}");

    let g = grammar.clone();
    let A = ThreadedFuture::new(
      async move {
        let data = g.productions[0].clone();
        for i in chunk_1 {
          println!("{:#?}", g.productions[i]);
        }

        data
      },
      &spawner,
    )
    .await;

    let g = grammar.clone();
    let B = ThreadedFuture::new(
      async move {
        for i in chunk_2 {
          println!("{:#?}", g.productions[i]);
        }
      },
      &spawner,
    );

    B.await;

    for i in chunk_3 {
      println!("------------------------");
    }
    for i in chunk_4 {
      println!("------------------------");
    }
  });

  drop(sp);

  executor.join();
}
