//! # The Radlr Runtime Library
//!
//! The Radlr Runtime Library provides the runtime functions and types
//! necessary to work with Radlr parsers within Rust code.
/// #### License:
/// Copyright (c) 2023 Anthony Weathersby
///
/// Permission is hereby granted, free of charge, to any person obtaining a copy
/// of this software and associated documentation files (the "Software"), to
/// deal in the Software without restriction, including without limitation the
/// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
/// sell copies of the Software, and to permit persons to whom the Software is
/// furnished to do so, subject to the following conditions:
///
/// The above copyright notice and this permission notice shall be included in
/// all copies or substantial portions of the Software.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
/// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
/// IN THE SOFTWARE.
pub mod deprecate;
pub mod kernel;
pub mod parsers;
pub mod types;
pub mod utf8;

#[macro_export]
macro_rules! panic_with_string {
  ($data:expr ) => {{
    let string = format!("{} at {}:{}", $data, file!(), line!());
    #[cfg(feature = "wasm-lab")]
    unsafe {
      web_sys::console::debug_1(&wasm_bindgen::JsValue::from(&string))
    };
    panic!("{string}");
  }};
}
