use std::cell::{Cell, UnsafeCell};

thread_local! {
  static ACTIVE: UnsafeCell<Option<*mut  Profiler>> = UnsafeCell::new(None);
}

struct Profiler {
  name:  &'static str,
  start: u64,
  end:   u64,
  count: u64,
  par:   Option<Cell<*mut Profiler>>,
}

impl Profiler {}

impl Drop for Profiler {
  fn drop(&mut self) {
    unsafe {
      ACTIVE.with(|val| {
        if let Some(par) = self.par.as_mut() {
          let par: *mut Profiler = *par.as_ptr();
          let par_container = val.get();
          *par_container = Some(par);
        }
      });
    }
  }
}
