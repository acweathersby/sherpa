#![allow(unused)]
use crate::{SherpaError, SherpaResult};
use std::{num::NonZeroUsize, sync::mpsc::Receiver, thread::JoinHandle};

enum Task {
  Job(Box<dyn FnOnce() -> SherpaResult<()> + Send>),
  Stop,
}

struct Worker {
  thread:  JoinHandle<()>,
  channel: std::sync::mpsc::Sender<Task>,
}

impl Worker {
  fn inner_loop(
    receiver: std::sync::mpsc::Receiver<Task>,
    response: std::sync::mpsc::Sender<Result<(), SherpaError>>,
  ) -> impl FnOnce() {
    move || {
      while let Ok(task) = receiver.recv() {
        match task {
          Task::Job(job) => match response.send((job)()) {
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
struct StandardPool {
  size:     usize,
  workers:  Vec<Worker>,
  c_signal: Receiver<Result<(), SherpaError>>,
}

impl StandardPool {
  pub fn new(size: NonZeroUsize) -> Result<Self, SherpaError> {
    let size = size.min(std::thread::available_parallelism()?).get();
    let (c_sender, receiver) = std::sync::mpsc::channel();

    Ok(Self {
      size,
      workers: (0..size)
        .into_iter()
        .map(|_| {
          let (sender, receiver) = std::sync::mpsc::channel::<Task>();
          Worker {
            channel: sender,
            thread:  std::thread::spawn(Worker::inner_loop(receiver, c_sender.clone())),
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
          Ok(_) => {
            #[cfg(debug_assertions)]
            eprintln!("Worker thread joined");
          }
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
struct SingleThreadPool {}

trait WorkerPool {
  /// Runs a closure as a job distributed to all workers in the pool. This
  /// blocks until all worker have returned from their closure.
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
  fn run<T: FnOnce() -> SherpaResult<()> + Send + 'static>(&self, job_creator: impl Fn() -> T) -> SherpaResult<()>;
}

impl WorkerPool for SingleThreadPool {
  fn run<T: FnOnce() -> SherpaResult<()> + Send + 'static>(&self, job_creator: impl Fn() -> T) -> SherpaResult<()> {
    // Simply create the job and run it
    job_creator()()
  }
}

impl WorkerPool for StandardPool {
  fn run<T: FnOnce() -> SherpaResult<()> + Send + 'static>(&self, job_creator: impl Fn() -> T) -> SherpaResult<()> {
    let mut errors = vec![];

    for worker in &self.workers {
      let t = job_creator();
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
      match self.c_signal.recv() {
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
        Err(_) => {
          todo!("Handle working dying.")
        }
      }
    }

    if errors.len() > 0 {
      Err(SherpaError::Multi(errors).flattened_multi())
    } else {
      Ok(())
    }
  }
}

#[test]
fn worker_pool() -> SherpaResult<()> {
  let pool = StandardPool::new(std::num::NonZeroUsize::new(200).unwrap())?;

  let data = std::sync::Arc::new(std::sync::RwLock::new(0));

  let job_creator = || {
    let data = data.clone();
    move || {
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
