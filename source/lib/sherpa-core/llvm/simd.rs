use super::CTX_AGGREGATE_INDICES as CTX;
use crate::{
  llvm::{
    parser_functions::{increment_input_offset_and_ptr, ScannerData},
    LLVMParserModule,
  },
  types::*,
};
use inkwell::values::{CallableValue, FunctionValue};
use sherpa_runtime::utf8::lookup_table::{
  CodePointClass,
  UNI_ID_CONT_DISCRETE,
  UNI_ID_CONT_RANGES,
  UNI_ID_START_INDICES,
  UNI_ID_START_RANGES,
};

pub(crate) fn create_simd_dfa<'a>(
  fn_name: &str,
  module: &'a LLVMParserModule,
  data: &ScannerData,
) -> SherpaResult<FunctionValue<'a>> {
  // Load the state table globally.
  // Get a reference to said table.
  //

  let ScannerData(symbols, states) = data;
  let ctx = module.ctx;
  let i8 = ctx.i8_type();
  let i32 = ctx.i32_type();
  let b = &module.builder;
  let fun = module.module.add_function(fn_name, module.types.TAIL_CALLABLE_PARSE_FUNCTION, None);

  // Build remap table.

  let parse_ctx = fun.get_nth_param(0)?.into_pointer_value();
  let state_byte_size = states[0].len();
  let states_byte_size = states.len() * state_byte_size;
  let accept_state: u64 = (states[0].len() - 1) as u64;
  const fail_state: u64 = 0;
  const start_state: u64 = 1;

  // The first column of every row will always be the error row
  let entry = ctx.append_basic_block(fun, "entry");
  let loop_start = ctx.append_basic_block(fun, "loop_head");
  let check_join = ctx.append_basic_block(fun, "check_join");
  let accept_block = ctx.append_basic_block(fun, "accept");
  let fail_block = ctx.append_basic_block(fun, "fail");

  // Add some global data.
  let state_table =
    module.module.add_global(i8.array_type(states_byte_size as u32), None, "state_table");

  let byte_lu_table = module.module.add_global(i8.array_type(256), None, "byte_table");

  // Initialize the global data

  byte_lu_table.set_initializer(
    &i8.const_array(
      (create_char_lookup(symbols).into_iter().map(|v| i8.const_int(v, false)).collect::<Vec<_>>())
        .as_slice(),
    ),
  );

  state_table.set_initializer(
    &i8.const_array(
      (states.iter().flatten().map(|v| i8.const_int(*v as u64, false)).collect::<Vec<_>>())
        .as_slice(),
    ),
  );

  b.position_at_end(entry);

  let scan_ptr_ptr = CTX::scan_ptr.get_ptr(module, parse_ctx)?;
  let scan_off_ptr = CTX::scan_off.get_ptr(module, parse_ctx)?;

  let state_length_ptr = b.build_alloca(i32, "state_length");
  b.build_store(state_length_ptr, i32.const_int(start_state, false));

  b.build_unconditional_branch(loop_start);

  // Upon entry we need to know how far we can progress until we need to check for
  // the state we are. Part of this process will be governed by the amount of input
  // we are able to consume, which will restrict how many iterations we are able to
  // perform.  On the whole, we shall asses whether we can perform a block of iterations
  // at a time (4), or whether we have to go one iteration at time.

  b.position_at_end(loop_start);

  let scan_off = CTX::scan_off.load(module, parse_ctx)?.into_int_value();
  let scan_len = CTX::scan_len.load(module, parse_ctx)?.into_int_value();
  let input_diff = b.build_int_sub(scan_len, scan_off, "input_diff");

  // Builds the necessary instructions to load input data into an ASM context, execute the
  // context, and transfer control to the `check_join` block.
  let build_asm_path = |ASM: String| -> SherpaResult<()> {
    let scan_ptr = CTX::scan_ptr.load(module, parse_ctx)?.into_pointer_value();
    let state_table_ptr = state_table.as_pointer_value();
    let byte_table_ptr = byte_lu_table.as_pointer_value();
    let i8_ptr = i8.ptr_type(inkwell::AddressSpace::Generic);

    let asm = ctx.create_inline_asm(
      i32.fn_type(&[i32.into(), i32.into(), i8_ptr.into(), i8_ptr.into(), i8_ptr.into()], false),
      ASM,
      ("=r, r, r, r, r, r").to_string(),
      false,
      false,
      Some(inkwell::InlineAsmDialect::Intel),
      false,
    );

    let call = b.build_call(
      CallableValue::try_from(asm).unwrap(),
      &[
        input_diff.into(),
        b.build_load(state_length_ptr, "").into_int_value().into(),
        scan_ptr.const_cast(i8_ptr).into(),
        byte_table_ptr.const_cast(i8_ptr).into(),
        state_table_ptr.const_cast(i8_ptr).into(),
      ],
      "test",
    );

    let state_length_val = call.try_as_basic_value().left()?.into_int_value();
    let adjusted_state = b.build_and(state_length_val, i32.const_int(0xF, false), "");
    b.build_store(state_length_ptr, adjusted_state);

    let length = b.build_right_shift(state_length_val, i32.const_int(4, false), false, "");
    increment_input_offset_and_ptr(b, scan_ptr_ptr, scan_off_ptr, length);

    // Increment the scan_ptr

    b.build_unconditional_branch(check_join);

    SherpaResult::Ok(())
  };

  //--------------------
  // Fast Path 8 bytes -------------------------------------------------------------
  build_asm_path(format!(
    r##"
    # Remember:
    # $1 - The available input length
    # $2 - The current active state (Does not contain length info) 
    # $3 - The input pointer
    # $4 - The token LUT
    # $5 - The state LUT

    cmp             $1, 8
    jb              _4bytes

    VPMOVZXBW       xmm0, [$3]
    mov             r12,    0x80
    PINSRB          xmm2,  r12d, 0  
    vpbroadcastw    xmm2,   xmm2                    
    ptest           xmm0, xmm2
    jnz             _slow_path      # Jump to slow path if the 
                                    # input contains non-ascii 
                                    # characters

    mov             r12d, 0xf0
    PINSRB          xmm8, r12d, 0
    VPBROADCASTB    xmm8, xmm8
    
    VPMOVZXWD    xmm1, xmm0
    PCMPEQB      xmm3, xmm3
    VPGATHERDD   xmm2, [$4 + xmm1 * 1], xmm3
    VPSRLD       xmm2, xmm2, 24
    
    MOVHLPS      xmm1, xmm0
    VPMOVZXWD    xmm1, xmm1
    PCMPEQB      xmm3, xmm3
    VPGATHERDD   xmm4, [$4 + xmm1 * 1], xmm3
    VPSRLD       xmm4, xmm4, 24
    
    VPEXTRD      r12d, xmm2, 0
    VPEXTRD      r13d, xmm2, 1
    VPEXTRD      r14d, xmm2, 2
    VPEXTRD      r15d, xmm2, 3
    
    movq         xmm0,  [$5 + r12 * {0}] 
    movq         xmm1,  [$5 + r13 * {0}]    
    
    VPSHUFB      xmm1,  xmm1, xmm0
    VPAND        xmm0, xmm0, xmm8
    VPADDB       xmm1, xmm1, xmm0

    movq         xmm2,  [$5 + r14 * {0}]     
    movq         xmm3,  [$5 + r15 * {0}]   
    
    VPSHUFB      xmm3,  xmm3, xmm2
    VPAND        xmm2, xmm2, xmm8
    VPADDB       xmm3, xmm3, xmm2
    
    VPSHUFB      xmm3,  xmm3, xmm1
    VPAND        xmm1, xmm1, xmm8
    VPADDB       xmm3, xmm3, xmm1

    VPEXTRD      r12d, xmm4, 0
    VPEXTRD      r13d, xmm4, 1
    VPEXTRD      r14d, xmm4, 2
    VPEXTRD      r15d, xmm4, 3
    
    movq         xmm4,  [$5 + r12 * {0}] 
    movq         xmm5,  [$5 + r13 * {0}]    
    
    VPSHUFB      xmm5,  xmm5, xmm4
    VPAND        xmm4, xmm4, xmm8
    VPADDB       xmm5, xmm5, xmm4

    movq         xmm6,  [$5 + r14 * {0}]     
    movq         xmm7,  [$5 + r15 * {0}]   
    
    VPSHUFB      xmm7,  xmm7, xmm6
    VPAND        xmm6, xmm6, xmm8
    VPADDB       xmm7, xmm7, xmm6
    
    VPSHUFB      xmm7,  xmm7, xmm5
    VPAND        xmm5, xmm5, xmm8
    VPADDB       xmm7, xmm7, xmm5

    VPSHUFB      xmm7,  xmm7, xmm3
    VPAND        xmm3, xmm3, xmm8
    VPADDB       xmm7, xmm7, xmm3

_simd_conclusion:
    PINSRB       xmm0,  $2 , 0        # Set 0th index to active state
    VPSHUFB      xmm7,  xmm7, xmm0    # Map the active state to the latest state
    VPEXTRB      $0,  xmm7, 0         # Extract the latest state and assign to output
    jmp          _end
    
_4bytes:
    cmp             $1, 4
    jb              _slow_path

    VPMOVZXBD    xmm0, [$3]
    mov             r12,    0x80
    PINSRB          xmm2,  r12d, 0  
    vpbroadcastw    xmm2,   xmm2                    
    ptest           xmm0, xmm2
    jnz             _slow_path    # Jump to slow path if the 
                                  # input contains non-ascii 
                                  # characters

    mov          r11d, 0xf0
    PINSRB       xmm8, r11d, 0
    VPBROADCASTB xmm8, xmm8
    
    PCMPEQB      xmm4, xmm4
    VPGATHERDD   xmm2, [$4 + xmm0 * 1], xmm4
    VPSRLD       xmm2, xmm2, 24
    
    VPEXTRD      r12d, xmm2, 0
    VPEXTRD      r13d, xmm2, 1
    VPEXTRD      r14d, xmm2, 2
    VPEXTRD      r15d, xmm2, 3
    
    movq         xmm0,  [$5 + r12 * {0}] 
    movq         xmm1,  [$5 + r13 * {0}]    
    
    VPSHUFB      xmm1,  xmm1, xmm0
    VPAND        xmm0, xmm0, xmm8
    VPADDB       xmm1, xmm1, xmm0

    movq         xmm2,  [$5 + r14 * {0}]     
    movq         xmm3,  [$5 + r15 * {0}]   
    
    VPSHUFB      xmm3,  xmm3, xmm2
    VPAND        xmm2, xmm2, xmm8
    VPADDB       xmm3, xmm3, xmm2
    
    VPSHUFB      xmm3,  xmm3, xmm1
    VPAND        xmm1, xmm1, xmm8
    VPADDB       xmm7, xmm3, xmm1

    jmp         _simd_conclusion

_slow_path: 
    
    xor          r11,    r11          # Zero out r11 to remove dependencies on upper bytes
    
    mov          r11b,  [$3]          # Load byte from char_ptr    
    
    mov          r11b,  [$4 + r11]    # Use byte to index into token lookup
    
    imul         r11,   {0}           # Multiply the token by the number of states in a row

    add          r11,   $5            # Extend the state_ptr by the number of 
                                      # proceeding columns and row
    
    mov          r13d,   $2             # Load the active state
    mov          $0,   [r11 + r13 * 1]  # Extract the state cell from the state LUT and assign to output
_end:
"##,
    state_byte_size
  ));

  //--------------------
  // Check Join ---------------------------------------------------------------
  b.position_at_end(check_join);

  let state_length_val = b.build_load(state_length_ptr, "").into_int_value();

  let state = b.build_and(state_length_val, i32.const_int(0xF, false), "");

  b.build_switch(state, loop_start, &[
    (i32.const_int(accept_state, false), accept_block),
    (i32.const_int(fail_state, false), fail_block),
  ]);

  b.position_at_end(accept_block);

  b.build_return(Some(&ctx.i32_type().const_zero()));

  b.position_at_end(fail_block);

  b.build_return(Some(&ctx.i32_type().const_int(1, false)));

  if fun.verify(true) {
    SherpaResult::Ok(fun)
  } else {
    SherpaResult::Err(SherpaError::from("\n\nCould not build emit_eop function"))
  }
}

fn create_char_lookup(symbols: &Vec<(u32, u32)>) -> Vec<u64> {
  let mut byte_vec = Vec::default();
  for _ in 0..256 {
    byte_vec.push(0);
  }
  for (val, sym_val) in symbols.iter().enumerate() {
    let val = (val + 1) as u64;
    match sym_val {
      (INPUT_TYPE::T03_CLASS, v) if *v == CodePointClass::ANY as u32 => {
        for i in 0..256 {
          byte_vec[i] = val;
        }
      }
      (INPUT_TYPE::T03_CLASS, v) if *v == CodePointClass::SYMBOL as u32 => {
        air(&mut byte_vec, val, &[0x21, 0x2F, 0x3A, 0x40, 0x5B, 0x60, 0x7B, 0x7E], 8);
      }
      (INPUT_TYPE::T03_CLASS, v) if *v == CodePointClass::NEW_LINE as u32 => {
        aii(&mut byte_vec, val, &[0xA], 1);
      }
      (INPUT_TYPE::T03_CLASS, v) if *v == CodePointClass::SPACE as u32 => {
        aii(&mut byte_vec, val, &[0x20], 1);
      }
      (INPUT_TYPE::T03_CLASS, v) if *v == CodePointClass::IDENTIFIER as u32 => {
        air(&mut byte_vec, val, &UNI_ID_START_RANGES, UNI_ID_START_RANGES.len());
        aii(&mut byte_vec, val, &UNI_ID_START_INDICES, UNI_ID_START_INDICES.len());
        air(&mut byte_vec, val, &UNI_ID_CONT_RANGES, UNI_ID_CONT_RANGES.len());
        aii(&mut byte_vec, val, &UNI_ID_CONT_DISCRETE, UNI_ID_CONT_DISCRETE.len());
      }
      (INPUT_TYPE::T03_CLASS, v) if *v == CodePointClass::NUMBER as u32 => {
        air(&mut byte_vec, val, &[48, 57], 2);
      }
      (INPUT_TYPE::T05_BYTE, byte_val) => {
        byte_vec[*byte_val as usize] = val;
      }
      val => unreachable!("No other token types are supported: {:?}", val),
    }
  }
  byte_vec
}

/// This function echos the one found in sherpa-rust-rt::utf8::lookup_table, with
/// the main difference being the value that assignment is used to set the table
/// value vs. using `|=`, and bounds checking is used before assignment.
fn aii(table: &mut [u64], value: u64, indices: &[usize], indice_len: usize) {
  let mut i: usize = 0;

  while i < indice_len {
    let indice = indices[i];

    if indice < table.len() {
      table[indice] = value;
    }

    i += 1;
  }
}

/// This function echos the one found in sherpa-rust-rt::utf8::lookup_table, with
/// the main difference being the value that assignment is used to set the table
/// value vs. using `|=`, and bounds checking is used before assignment.
pub fn air(table: &mut [u64], value: u64, indices: &[usize], indice_len: usize) {
  let mut i: usize = 0;

  while i < indice_len {
    let mut start = indices[i];
    let end = indices[i + 1];

    while start <= end {
      if start < table.len() {
        table[start] = value;
      } else {
        break;
      }
      start += 1;
    }

    i += 2;
  }
}
