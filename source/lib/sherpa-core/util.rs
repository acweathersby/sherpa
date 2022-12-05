use std::num::NonZeroUsize;

/// Retrieve the number of threads that can be reasonably
/// run concurrently on the platform
pub(crate) fn get_num_of_available_threads() -> usize {
  std::thread::available_parallelism().unwrap_or(NonZeroUsize::new(1).unwrap()).get()
}
