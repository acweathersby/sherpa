use sherpa::types::UTF8StringReader;
use test_proc_macro::parser;

pub fn main() {
  let bytes = include_bytes!("./spirv.data");

  let spirv = unsafe { String::from_utf8_unchecked(bytes.to_vec()) };

  // let start = Instant::now();

  let node = parser::Context::parse_default(&mut UTF8StringReader::new(&spirv));

  // println!("-- dur: {:?}", duration);

  // println!("{:?}", node);
}
