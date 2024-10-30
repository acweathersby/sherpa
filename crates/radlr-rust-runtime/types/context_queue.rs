use std::{alloc::Layout, cmp::Ordering, fmt::Debug};

use super::ParserError;

pub trait QueuedContext: Sized {
  fn queued_priority(&self) -> usize;
}

#[derive(Debug)]
pub struct Sled<T: Sized> {
  content:  Option<T>,
  next:     Option<u32>,
  priority: usize,
}

#[derive(Default, Clone, Copy, Debug)]
enum BufferTracker {
  #[default]
  None,
  Some {
    len:   u16,
    first: u32,
    last:  u32,
  },
}

impl BufferTracker {
  pub fn len(&self) -> usize {
    match self {
      BufferTracker::None => 0,
      BufferTracker::Some { len, .. } => *len as usize,
    }
  }
}

pub struct PopIterator<'queue, T: Sized> {
  pop_queue: &'queue mut ContextQueue<T>,
}

impl<'queue, T: Sized> Iterator for PopIterator<'queue, T> {
  type Item = T;

  fn next(&mut self) -> Option<Self::Item> {
    self.pop_queue.pop_front()
  }
}

/// A disjointed queue separating push and pop operations into two distinct
/// queues, that is a push queue and a pop queue. The push and pop queues can be
/// swapped to allow cyclic queueing processes.

pub struct ContextQueue<T: Sized> {
  slice:       *mut Sled<T>,
  capacity:    u32,
  free_list:   Option<u32>,
  pop_buffer:  BufferTracker,
  push_buffer: BufferTracker,
}

#[cfg(debug_assertions)]
impl<Context: QueuedContext + Debug> Debug for ContextQueue<Context> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut s = f.debug_struct("ContextQueue");
    s.field("pop_buffer", &self.pop_buffer);
    s.field("push_buffer", &self.push_buffer);
    s.field("free_list", &self.free_list);
    s.field("capacity", &self.capacity);
    s.field("slice", &self.slice());
    s.finish()
  }
}

impl<T: Sized> ContextQueue<T> {
  /// Create a new queue capable of holding `capacity` items. Note that this
  /// capacity is shared between both internal buffers. If the number of items
  /// stored in both buffers exceeds this capacity then the queue will be
  /// automatically grown to handle additional items.
  ///
  ///
  /// Err is returned if there is not enough memory to meet the desired
  /// capacity.
  pub fn new_with_capacity(capacity: u32) -> Result<Self, ParserError> {
    let capacity = capacity.max(1);

    let Ok(layout) = Layout::array::<Sled<T>>(capacity as usize) else {
      return Err(ParserError::OutOfMemory);
    };

    unsafe {
      let ptr = std::alloc::alloc(layout) as *mut Sled<T>;

      if ptr.is_null() {
        return Err(ParserError::OutOfMemory);
      }

      let slice = ptr;

      Self::init_buffer(slice, 0, (capacity as isize) - 1);

      Ok(Self {
        capacity,
        slice,
        free_list: Some(0),
        pop_buffer: BufferTracker::None,
        push_buffer: BufferTracker::None,
      })
    }
  }

  /// Returns the number of items in the pop buffer
  pub fn pop_len(&self) -> usize {
    self.pop_buffer.len()
  }

  /// Returns true if there are no items in the pop buffer
  pub fn pop_is_empty(&self) -> bool {
    self.pop_buffer.len() == 0
  }

  /// Returns the number of items in the push buffer
  pub fn push_len(&self) -> usize {
    self.push_buffer.len()
  }

  /// Returns true if there are no items in the push buffer
  pub fn push_is_empty(&self) -> bool {
    self.push_buffer.len() == 0
  }

  /// Returns the underlying store as a slice.
  pub fn slice(&self) -> &[Sled<T>] {
    unsafe { std::slice::from_raw_parts(self.slice, self.capacity as usize) }
  }

  ///
  /// Safety: This is only called when processing the uninitialized portions of
  /// a buffer, thus all data that is modified is garbage at worst, and
  /// zero initialized bytes at best, and can be discarded without doing any
  /// sort of cleanup.
  unsafe fn init_buffer(ptr: *mut Sled<T>, start_index: isize, end_index: isize) {
    for i in start_index..=end_index {
      let ptr = ptr.offset(i);
      let mut data = Sled {
        content:  None,
        next:     if i < end_index { Some(1 + i as u32) } else { None },
        priority: 0,
      };
      std::mem::swap(&mut *ptr, &mut data);
      std::mem::forget(data);
    }
  }

  unsafe fn increase_capacity(&mut self) -> Result<(), ParserError> {
    debug_assert!(self.free_list.is_none(), "`increase_capacity` should only be called when free list has been exhausted");

    let new_capacity = self.capacity << 1;

    let Ok(old_layout) = Layout::array::<Sled<T>>(self.capacity as usize) else {
      return Err(ParserError::OutOfMemory);
    };

    let Ok(new_layout) = Layout::array::<Sled<T>>(new_capacity as usize) else {
      return Err(ParserError::OutOfMemory);
    };

    let ptr = std::alloc::realloc(self.slice as *mut u8, old_layout, new_layout.size()) as *mut Sled<T>;

    if ptr.is_null() {
      return Err(ParserError::OutOfMemory);
    }

    self.slice = ptr;

    // Initialize the new section of the buffer.
    Self::init_buffer(self.slice, self.capacity as isize, (new_capacity - 1) as isize);

    self.free_list = Some(self.capacity);

    self.capacity = new_capacity;

    Ok(())
  }

  pub fn take_pop<'queue>(&'queue mut self) -> PopIterator<'queue, T> {
    PopIterator { pop_queue: self }
  }

  /// Pop the next item from the top of the pop buffer.
  pub fn pop_front(&mut self) -> Option<T> {
    match self.pop_buffer {
      BufferTracker::None => None,
      BufferTracker::Some { len, first, last } => unsafe {
        let mut data = Sled { content: None, next: self.free_list, priority: 0 };

        std::mem::swap(&mut *self.slice.offset(first as isize), &mut data);

        self.free_list = Some(first);

        self.pop_buffer =
          if let Some(next) = data.next { BufferTracker::Some { len: len - 1, first: next, last } } else { BufferTracker::None };

        data.content
      },
    }
  }

  pub fn push_back(&mut self, priority: usize, ctx: T) {
    if let Some(next) = self.free_list {
      unsafe {
        let data = self.slice.offset(next as isize);
        let incoming = &mut *data;

        self.free_list = std::mem::take(&mut incoming.next);

        debug_assert!(incoming.content.is_none());

        incoming.priority = priority;
        incoming.content = Some(ctx);

        match self.push_buffer {
          BufferTracker::None => self.push_buffer = BufferTracker::Some { len: 1, first: next, last: next },
          BufferTracker::Some { len, first, last } => {
            let data = self.slice.offset(last as isize);
            let data_mut = &mut *data;

            data_mut.next = Some(next);

            self.push_buffer = BufferTracker::Some { len: len + 1, first, last: next };
          }
        }
      }
    } else {
      unsafe { self.increase_capacity().expect("Ran out of memory") };
      self.push_back(priority, ctx)
    }
  }

  pub fn push_with_priority(&mut self, priority: usize, ctx: T) {
    if let Some(new) = self.free_list {
      unsafe {
        let data = self.slice.offset(new as isize);
        let incoming = &mut *data;

        self.free_list = std::mem::take(&mut incoming.next);

        debug_assert!(incoming.content.is_none());

        incoming.priority = priority;
        incoming.content = Some(ctx);

        match self.push_buffer {
          BufferTracker::None => self.push_buffer = BufferTracker::Some { len: 1, first: new, last: new },
          BufferTracker::Some { len, first, last } => {
            let mut curr = Some(first);
            let mut prev = None;
            while let Some(index) = curr {
              let ptr = self.slice.offset(index as isize);
              let curr_sled = &mut *ptr;

              match incoming.priority.cmp(&curr_sled.priority) {
                Ordering::Greater => {
                  incoming.next = curr;
                  if prev.is_some() {
                    (&mut *self.slice.offset(prev.unwrap_unchecked() as isize)).next = Some(new);
                    self.push_buffer = BufferTracker::Some { len: len + 1, first, last }
                  } else {
                    self.push_buffer = BufferTracker::Some { len: len + 1, first: new, last }
                  };
                  return;
                }
                _ => {}
              }

              prev = curr;
              curr = curr_sled.next;
            }

            (&mut *self.slice.offset(prev.unwrap_unchecked() as isize)).next = Some(new);

            self.push_buffer = BufferTracker::Some { len: len + 1, first, last: new };
          }
        }
      }
    } else {
      unsafe { self.increase_capacity().expect("Ran out of memory") };
      self.push_with_priority(priority, ctx)
    }
  }

  /// Swaps the internal buffers, making the push_buffer the new pop_buffer and
  /// vice-versa
  pub fn swap_buffers(&mut self) {
    std::mem::swap(&mut self.pop_buffer, &mut self.push_buffer);
  }
}

impl<T: Sized> Drop for ContextQueue<T> {
  fn drop(&mut self) {
    unsafe {
      debug_assert!(!self.slice.is_null(), "Slice has been corrupted");

      if let BufferTracker::Some { first, .. } = self.pop_buffer {
        let mut first = Some(first);
        while let Some(index) = first {
          let ptr = self.slice.offset(index as isize);
          let sled = &mut *ptr;
          std::mem::take(&mut sled.content);
          first = sled.next;
        }
      }

      if let BufferTracker::Some { first, .. } = self.push_buffer {
        let mut first = Some(first);
        while let Some(index) = first {
          let ptr = self.slice.offset(index as isize);
          let sled = &mut *ptr;
          std::mem::take(&mut sled.content);
          first = sled.next;
        }
      }

      std::alloc::dealloc(self.slice as *mut u8, Layout::array::<Sled<T>>(self.capacity as usize).unwrap());
    }
  }
}

#[cfg(test)]
mod test {
  use super::{super::*, *};
  use std::time::{SystemTime, UNIX_EPOCH};

  #[derive(PartialEq, Eq, Debug)]
  struct TestData {
    pub payload:  usize,
    pub priority: u32,
  }

  impl QueuedContext for TestData {
    fn queued_priority(&self) -> usize {
      self.priority as usize
    }
  }

  type TestQueue = ContextQueue<TestData>;

  #[test]
  fn invalid_capacity_request() -> Result<(), ParserError> {
    assert!(matches!(TestQueue::new_with_capacity(u32::MAX), Err(ParserError::OutOfMemory)));
    Ok(())
  }

  #[test]
  fn free_list_correctly_constructed() -> Result<(), ParserError> {
    let size = 1024;

    let queue = TestQueue::new_with_capacity(size)?;

    let slice = queue.slice();

    for i in 0..size as usize {
      if i < (size - 1) as usize {
        assert_eq!(slice[i].content, None);
        assert_eq!(slice[i].next, Some((i + 1) as u32));
      } else {
        assert_eq!(slice[i].content, None);
        assert_eq!(slice[i].next, None);
      }
    }

    assert_eq!(queue.free_list, Some(0));

    Ok(())
  }

  #[test]
  fn insert_and_retrieve_object() -> Result<(), ParserError> {
    let mut queue = TestQueue::new_with_capacity(1)?;

    queue.push_back(0, TestData { payload: 2, priority: 0 });
    queue.push_back(2, TestData { payload: 0, priority: 2 });

    assert_eq!(queue.pop_front(), None, "the pop buffer should be empty");

    queue.swap_buffers();

    assert_eq!(queue.pop_front(), Some(TestData { payload: 2, priority: 0 }));
    assert_eq!(queue.pop_front(), Some(TestData { payload: 0, priority: 2 }));
    assert_eq!(queue.pop_front(), None, "the pop buffer should now be empty");

    Ok(())
  }

  #[test]
  fn insert_and_retrieve_object_with_ascending_prority() -> Result<(), ParserError> {
    let mut queue = TestQueue::new_with_capacity(1)?;

    queue.push_with_priority(1, TestData { payload: 1, priority: 1 });
    queue.push_with_priority(2, TestData { payload: 2, priority: 2 });
    queue.push_with_priority(3, TestData { payload: 3, priority: 3 });
    queue.push_with_priority(4, TestData { payload: 4, priority: 4 });
    queue.push_with_priority(5, TestData { payload: 5, priority: 5 });

    assert_eq!(queue.pop_front(), None, "the pop buffer should be empty");

    queue.swap_buffers();

    assert_eq!(queue.pop_front(), Some(TestData { payload: 5, priority: 5 }));
    assert_eq!(queue.pop_front(), Some(TestData { payload: 4, priority: 4 }));
    assert_eq!(queue.pop_front(), Some(TestData { payload: 3, priority: 3 }));
    assert_eq!(queue.pop_front(), Some(TestData { payload: 2, priority: 2 }));
    assert_eq!(queue.pop_front(), Some(TestData { payload: 1, priority: 1 }));

    assert_eq!(queue.pop_front(), None, "the pop buffer should now be empty");

    Ok(())
  }

  #[test]
  fn insert_and_retrieve_object_with_descending_prority() -> Result<(), ParserError> {
    let mut queue = TestQueue::new_with_capacity(1)?;

    queue.push_with_priority(5, TestData { payload: 5, priority: 5 });
    queue.push_with_priority(4, TestData { payload: 4, priority: 4 });
    queue.push_with_priority(3, TestData { payload: 3, priority: 3 });
    queue.push_with_priority(2, TestData { payload: 2, priority: 2 });
    queue.push_with_priority(1, TestData { payload: 1, priority: 1 });

    assert_eq!(queue.pop_front(), None, "the pop buffer should be empty");

    queue.swap_buffers();

    assert_eq!(queue.pop_front(), Some(TestData { payload: 5, priority: 5 }));
    assert_eq!(queue.pop_front(), Some(TestData { payload: 4, priority: 4 }));
    assert_eq!(queue.pop_front(), Some(TestData { payload: 3, priority: 3 }));
    assert_eq!(queue.pop_front(), Some(TestData { payload: 2, priority: 2 }));
    assert_eq!(queue.pop_front(), Some(TestData { payload: 1, priority: 1 }));

    assert_eq!(queue.pop_front(), None, "the pop buffer should now be empty");

    Ok(())
  }

  #[test]
  fn insert_and_retrieve_object_with_mixed_prority() -> Result<(), ParserError> {
    let max_items = u16::MAX as usize;

    let mut queue = TestQueue::new_with_capacity(200)?;

    for _ in 0..max_items {
      let priority = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().subsec_nanos() % (max_items as u32);
      queue.push_with_priority(priority as usize, TestData { payload: priority as usize, priority });
    }

    let mut start = u32::MAX;

    queue.swap_buffers();

    assert_eq!(queue.pop_len(), max_items);

    while let Some(TestData { priority, .. }) = queue.pop_front() {
      assert!(priority <= start);
      start = priority;
    }

    assert!(queue.push_is_empty());
    assert!(queue.pop_is_empty());

    Ok(())
  }
}
