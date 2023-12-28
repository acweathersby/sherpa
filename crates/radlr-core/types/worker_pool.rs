#![allow(unused)]
use crate::{RadlrError, RadlrResult};
use std::{num::NonZeroUsize, sync::mpsc::Receiver, thread::JoinHandle};

enum Task {
  Job(Box<dyn FnOnce(usize) -> RadlrResult<()> + Send>),
  Stop,
}

struct Worker {
  thread:  JoinHandle<()>,
  channel: std::sync::mpsc::Sender<Task>,
  id:      usize,
}

impl Worker {
  fn inner_loop(
    receiver: std::sync::mpsc::Receiver<Task>,
    response: std::sync::mpsc::Sender<Result<(), RadlrError>>,
    id: usize,
  ) -> impl FnOnce() {
    move || {
      while let Ok(task) = receiver.recv() {
        match task {
          Task::Job(job) => match response.send((job)(id)) {
            Err(_) => {
              break;
            }
            _ => {}
          },
          Task::Stop => {
            break;
          }
        }
      }
    }
  }
}

// A basic, multi-threaded worker pool.
pub(crate) struct StandardPool {
  size:     usize,
  workers:  Vec<Worker>,
  c_signal: Receiver<Result<(), RadlrError>>,
}

impl StandardPool {
  pub fn new(size: usize) -> Result<Self, RadlrError> {
    let size = size.max(1).min(std::thread::available_parallelism()?.get());
    let (c_sender, receiver) = std::sync::mpsc::channel();

    Ok(Self {
      size,
      workers: (0..size)
        .into_iter()
        .map(|id| {
          let (sender, receiver) = std::sync::mpsc::channel::<Task>();
          Worker {
            channel: sender,
            thread: std::thread::spawn(Worker::inner_loop(receiver, c_sender.clone(), id)),
            id,
          }
        })
        .collect(),
      c_signal: receiver,
    })
  }
}

impl Drop for StandardPool {
  fn drop(&mut self) {
    for worker in self.workers.drain(..) {
      match worker.channel.send(Task::Stop) {
        Ok(_) => match worker.thread.join() {
          Ok(_) => {}
          Err(_) => {}
        },
        Err(e) => {
          eprintln!("Error resolving workers {e}");
        }
      }
    }
  }
}

// A worker pool that runs Jobs on the main thread.
pub(crate) struct SingleThreadPool {}

pub trait WorkerPool {
  /// Runs a closure as a job creator for the number of available workers. For
  /// each worker, the job creator closure should return a job closure which
  /// will run within that worker's thread. The total number of workers
  /// available in the pool is passed as the sole argument to the job creator
  /// closure.
  ///
  /// This blocks until all worker have returned from their closure.
  ///
  /// # Example
  /// ```
  ///  let pool = StandardPool::new(std::num::NonZeroUsize::new(200).unwrap())?;
  ///
  ///  let data = std::sync::Arc::new(std::sync::RwLock::new(0));
  ///
  ///  let job_creator = || {
  ///    let data = data.clone();
  ///    move || {
  ///      match data.write() {
  ///        Ok(mut data) => {Z
  ///          (*data) += 1;
  ///        }
  ///        _ => {
  ///          println!("Failed")
  ///        }
  ///      }
  ///      Ok(())
  ///    }
  ///  };
  ///
  ///  pool.run(job_creator)?;
  ///
  ///  println!("{data:?}");
  ///  assert_eq!(*(data.read().unwrap()), std::thread::available_parallelism().unwrap().get());
  ///
  ///  Ok(())
  /// ```
  fn run<T: FnOnce(usize) -> RadlrResult<()> + Send + 'static>(&self, job_creator: impl Fn(usize) -> T) -> RadlrResult<()>;
}

impl WorkerPool for SingleThreadPool {
  fn run<T: FnOnce(usize) -> RadlrResult<()> + Send + 'static>(&self, job_creator: impl Fn(usize) -> T) -> RadlrResult<()> {
    // Simply create the job and run it
    job_creator(1)(0)
  }
}

impl WorkerPool for StandardPool {
  fn run<T: FnOnce(usize) -> RadlrResult<()> + Send + 'static>(&self, job_creator: impl Fn(usize) -> T) -> RadlrResult<()> {
    let mut errors = vec![];

    for worker in &self.workers {
      let t = job_creator(self.size);
      let t = Box::new(t);
      match worker.channel.send(Task::Job(t)) {
        Ok(()) => {}
        Err(io) => {
          eprintln!("{}", io);
          todo!("Rebuild worker");
          // rebuild worker.
        }
      }
    }

    let mut count = 0;

    loop {
      match self.c_signal.recv_timeout(std::time::Duration::from_nanos(1000)) {
        Ok(response) => {
          count += 1;

          match response {
            Err(err) => errors.push(err),
            _ => {}
          }
          if count >= self.size {
            break;
          }
        }
        err @ Err(std::sync::mpsc::RecvTimeoutError::Disconnected) => {
          todo!("Handle worker dying. {err:?}");
        }
        Err(std::sync::mpsc::RecvTimeoutError::Timeout) => {}
      }

      for (v, worker) in self.workers.iter().enumerate() {
        if worker.thread.is_finished() {
          panic!("Worker [{v}] Died");
        }
      }
    }

    if errors.len() > 0 {
      Err(RadlrError::Multi(errors).flattened_multi())
    } else {
      Ok(())
    }
  }
}

#[test]
fn worker_pool() -> RadlrResult<()> {
  let pool = StandardPool::new(200)?;

  let data = std::sync::Arc::new(std::sync::RwLock::new(0));

  let job_creator = |number_of_threads| {
    let data = data.clone();
    println!("Number of workers: {number_of_threads}");
    move |id| {
      println!("job on worker {id} launched");
      match data.write() {
        Ok(mut data) => {
          (*data) += 1;
        }
        _ => {
          println!("Failed")
        }
      }
      Ok(())
    }
  };

  pool.run(job_creator)?;

  println!("{data:?}");
  assert_eq!(*(data.read().unwrap()), std::thread::available_parallelism().unwrap().get());

  let pool = SingleThreadPool {};

  pool.run(job_creator)?;

  assert_eq!(*(data.read().unwrap()), std::thread::available_parallelism().unwrap().get() + 1);

  Ok(())
}
