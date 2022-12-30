use std::collections::{BTreeMap, BTreeSet, VecDeque};

use super::CTX_AGGREGATE_INDICES as CTX;
use crate::{
  build::table::BranchTableData,
  compile::BytecodeOutput,
  llvm::{parser_functions::increment_input_offset_and_ptr, LLVMParserModule},
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

  let ScannerData { symbols, states_table: states, accept_col } = data;
  let ctx = module.ctx;
  let i8 = ctx.i8_type();
  let i32 = ctx.i32_type();
  let b = &module.builder;
  let fun = module.module.add_function(fn_name, module.types.TAIL_CALLABLE_PARSE_FUNCTION, None);

  let parse_ctx = fun.get_nth_param(0)?.into_pointer_value();
  let states_table_byte_size = states.len();
  let accept_state = *accept_col as u64;
  const state_table_byte_size: u64 = 8;
  const fail_state: u64 = 0;
  const start_state: u64 = 1;

  // The first column of every row will always be the error row
  let entry = ctx.append_basic_block(fun, "entry");
  let loop_start = ctx.append_basic_block(fun, "loop_head");
  let slow_path = ctx.append_basic_block(fun, "slow_path");
  let class_lookup = ctx.append_basic_block(fun, "class_lookup");
  let byte_lookup = ctx.append_basic_block(fun, "byte_lookup");
  let slow_path_resolve = ctx.append_basic_block(fun, "slow_path_resolve");
  let fast_path = ctx.append_basic_block(fun, "fast_path");
  let check_join = ctx.append_basic_block(fun, "check_join");
  let accept_block = ctx.append_basic_block(fun, "accept");
  let fail_block = ctx.append_basic_block(fun, "fail");

  // Add some global data.
  let state_table =
    module.module.add_global(i8.array_type(states_table_byte_size as u32), None, "state_table");

  let byte_lu_table = module.module.add_global(i8.array_type(256), None, "byte_table");

  // Initialize the global data

  byte_lu_table.set_initializer(
    &i8.const_array(
      (create_char_lookup(symbols).into_iter().map(|v| i8.const_int(v, false)).collect::<Vec<_>>())
        .as_slice(),
    ),
  );

  state_table.set_initializer(&i8.const_array(
    (states.iter().map(|v| i8.const_int(*v as u64, false)).collect::<Vec<_>>()).as_slice(),
  ));

  b.position_at_end(entry);
  let state_table_ptr = state_table.as_pointer_value();
  let token_id_ptr = byte_lu_table.as_pointer_value();
  let i8_ptr = i8.ptr_type(inkwell::AddressSpace::Generic);
  let token_value_ptr = b.build_alloca(i32, "val_ptr");

  let state_length_val_ptr = b.build_alloca(i32, "state_value_length");
  let scan_ptr_ptr = CTX::scan_ptr.get_ptr(module, parse_ctx)?;
  let scan_off_ptr = CTX::scan_off.get_ptr(module, parse_ctx)?;
  let state_ptr = b.build_alloca(i32, "state_length");

  b.build_store(state_ptr, i32.const_int(start_state, false));

  b.build_unconditional_branch(loop_start);

  // Upon entry we need to know how far we can progress until we need to check for
  // the state we are. Part of this process will be governed by the amount of input
  // we are able to consume, which will restrict how many iterations we are able to
  // perform.  On the whole, we shall asses whether we can perform a block of iterations
  // at a time (4), or whether we have to go one iteration at time.

  b.position_at_end(loop_start);

  let scan_ptr = CTX::scan_ptr.load(module, parse_ctx)?.into_pointer_value();
  let scan_off = CTX::scan_off.load(module, parse_ctx)?.into_int_value();
  let scan_len = CTX::scan_len.load(module, parse_ctx)?.into_int_value();
  let input_diff = b.build_int_sub(scan_len, scan_off, "input_diff");

  // Builds the necessary instructions to load input data into an ASM context, execute the
  // context, and transfer control to the `check_join` block.
  let build_asm_path = |CONSTRAINTS: String, ASM: String| -> SherpaResult<()> {
    let asm = ctx.create_inline_asm(
      i32.fn_type(
        &[
          i32.into(),
          i32.into(),
          i8_ptr.into(),
          i8_ptr.into(),
          i8_ptr.into(),
          i32.ptr_type(inkwell::AddressSpace::Generic).into(),
        ],
        false,
      ),
      ASM,
      CONSTRAINTS,
      false,
      false,
      Some(inkwell::InlineAsmDialect::Intel),
      false,
    );

    let call = b.build_call(
      CallableValue::try_from(asm).unwrap(),
      &[
        input_diff.into(),
        b.build_load(state_ptr, "").into_int_value().into(),
        scan_ptr.const_cast(i8_ptr).into(),
        token_id_ptr.const_cast(i8_ptr).into(),
        state_table_ptr.const_cast(i8_ptr).into(),
        state_length_val_ptr.into(),
      ],
      "test",
    );

    let do_slow_path = call.try_as_basic_value().left()?.into_int_value();
    let c = b.build_int_compare(inkwell::IntPredicate::EQ, do_slow_path, i32.const_zero(), "");
    b.build_conditional_branch(c, fast_path, slow_path);

    b.position_at_end(fast_path);

    let state_length_val = b.build_load(state_length_val_ptr, "").into_int_value();
    let state_length_val = b.build_and(state_length_val, i32.const_int(0xFF, false), "");
    b.build_store(state_ptr, b.build_and(state_length_val, i32.const_int(0xF, false), ""));

    let length = b.build_right_shift(state_length_val, i32.const_int(4, false), false, "");
    increment_input_offset_and_ptr(b, scan_ptr_ptr, scan_off_ptr, length);

    // Increment the scan_ptr

    b.build_unconditional_branch(check_join);

    SherpaResult::Ok(())
  };

  //--------------------
  // Fast Path 8 bytes -------------------------------------------------------------
  build_asm_path(
    ("=r, {r8}, {rbx}, {r12}, {r13}, {r14}, {rax}").to_string(),
    format!(
      r##"
    # Remember:
    # $1 - The available input length
    # $2 - The current active state (Does not contain length info) 
    # $3 - The input pointer
    # $4 - The token LUT
    # $5 - The state LUT
    # $6 - Output 

    cmp             r8, 8
    jb              _4bytes

    VPMOVZXBW       xmm0,  [$3]
    mov             r8,   0x80
    PINSRB          xmm2,  r8d, 0  
    vpbroadcastw    xmm2,   xmm2                    
    ptest           xmm0, xmm2
    jnz             _4bytes         # Jump to slow path if the 
                                    # input contains non-ascii 
                                    # characters

    mov             r8d, 0xf0
    PINSRB          xmm8, r8d, 0
    VPBROADCASTB    xmm8, xmm8


    VPMOVZXWD    xmm1, xmm0
    PCMPEQB      xmm3, xmm3
    VPGATHERDD   xmm9, [$4 + xmm1 * 1], xmm3
    VPSRLD       xmm9, xmm9, 24

    MOVHLPS      xmm1, xmm0
    VPMOVZXWD    xmm1, xmm1
    PCMPEQB      xmm3, xmm3
    VPGATHERDD   xmm10, [$4 + xmm1 * 1], xmm3
    VPSRLD       xmm10, xmm10, 24

    VPEXTRD      r8d, xmm9, 0
    movq         xmm0,  [$5 + r8 * {0}] 
    VPEXTRD      r8d, xmm9, 1
    movq         xmm1,  [$5 + r8 * {0}]  
    VPEXTRD      r8d, xmm9, 2
    movq         xmm2,  [$5 + r8 * {0}]     
    VPEXTRD      r8d, xmm9, 3
    movq         xmm3,  [$5 + r8 * {0}]     

    VPSHUFB      xmm1,  xmm1, xmm0
    VPAND        xmm0, xmm0, xmm8
    VPADDB       xmm1, xmm1, xmm0

    VPSHUFB      xmm3,  xmm3, xmm2
    VPAND        xmm2, xmm2, xmm8
    VPADDB       xmm3, xmm3, xmm2

    VPSHUFB      xmm3,  xmm3, xmm1
    VPAND        xmm1, xmm1, xmm8
    VPADDB       xmm3, xmm3, xmm1

    VPEXTRD      r8d, xmm10, 0
    movq         xmm4,  [$5 + r8 * {0}] 
    VPEXTRD      r8d, xmm10, 1
    movq         xmm5,  [$5 + r8 * {0}]  
    VPEXTRD      r8d, xmm10, 2
    movq         xmm6,  [$5 + r8 * {0}]     
    VPEXTRD      r8d, xmm10, 3
    movq         xmm7,  [$5 + r8 * {0}]   

    VPSHUFB      xmm5,  xmm5, xmm4
    VPAND        xmm4, xmm4, xmm8
    VPADDB       xmm5, xmm5, xmm4

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
    PINSRB       xmm0,  $2, 0        # Set 0th index to active state
    VPSHUFB      xmm7,  xmm7, xmm0    # Map the active state to the latest state
    VPEXTRB      [$6],  xmm7, 0         # Extract the latest state and assign to output
    mov         $0, 0
    jmp          _end

_4bytes:
    cmp             r8, 4
    jb              _slow_path

    VPMOVZXBD       xmm0,   [$3]
    mov             r8,    0x80
    PINSRB          xmm2,   r8d, 0  
    vpbroadcastw    xmm2,   xmm2                    
    ptest           xmm0,   xmm2
    jnz             _slow_path    # Jump to slow path if the 
                                  # input contains non-ascii 
                                  # characters

    mov          r11d, 0xf0
    PINSRB       xmm8, r11d, 0
    VPBROADCASTB xmm8, xmm8

    PCMPEQB      xmm4, xmm4
    VPGATHERDD   xmm5, [$4 + xmm0 * 1], xmm4
    VPSRLD       xmm5, xmm5, 24

    VPEXTRD      r8d, xmm5, 0
    movq         xmm0,  [$5 + r8 * {0}] 
    VPEXTRD      r8d, xmm5, 1
    movq         xmm1,  [$5 + r8 * {0}]    
    VPEXTRD      r8d, xmm5, 2
    movq         xmm2,  [$5 + r8 * {0}]   
    VPEXTRD      r8d, xmm5, 3  
    movq         xmm3,  [$5 + r8 * {0}]   

    VPSHUFB      xmm1,  xmm1, xmm0
    VPAND        xmm0, xmm0, xmm8
    VPADDB       xmm1, xmm1, xmm0

    VPSHUFB      xmm3,  xmm3, xmm2
    VPAND        xmm2, xmm2, xmm8
    VPADDB       xmm3, xmm3, xmm2

    VPSHUFB      xmm3,  xmm3, xmm1
    VPAND        xmm1, xmm1, xmm8
    VPADDB       xmm7, xmm3, xmm1

    jmp         _simd_conclusion
_slow_path:
    mov         $0, 1
_end:
"##,
      state_table_byte_size
    ),
  );

  //--------------------
  // Check Join ---------------------------------------------------------------

  //--------------------
  // Slow Path 1-4 bytes -------------------------------------------------------------
  b.position_at_end(slow_path);

  // Get codepoint info
  let cp_info = b
    // load byte from char ptr
    .build_call(module.fun.get_utf8_codepoint_info, &[scan_ptr.into()], "")
    .try_as_basic_value()
    .left()?
    .into_struct_value();

  let cp_val = b.build_extract_value(cp_info, 0, "cp_val")?.into_int_value();
  b.build_store(token_value_ptr, cp_val);

  let cp_byte_len = b.build_extract_value(cp_info, 1, "cp_len")?.into_int_value();

  let c = b.build_int_compare(inkwell::IntPredicate::ULT, cp_val, i32.const_int(128, false), "");
  b.build_conditional_branch(c, byte_lookup, class_lookup);

  // -------------------------------------------------------
  b.position_at_end(byte_lookup);

  // Lookup the correct token value using the token lookup table;
  let cp_val = b.build_load(token_value_ptr, "cp_val").into_int_value();
  let token_ptr = unsafe { b.build_gep(token_id_ptr, &[i32.const_zero(), cp_val.into()], "") };
  let token_ptr = b.build_pointer_cast(token_ptr, i8.ptr_type(inkwell::AddressSpace::Generic), "");
  let token_val = b.build_load(token_ptr, "").into_int_value();
  // Store class value after clamping the range from 0-15.
  b.build_store(token_value_ptr, b.build_int_z_extend(token_val, i32, ""));

  b.build_unconditional_branch(slow_path_resolve);

  // -------------------------------------------------------
  b.position_at_end(slow_path_resolve);

  let cp_val = b.build_load(token_value_ptr, "cp_val").into_int_value();
  let state = b.build_load(state_ptr, "state").into_int_value();
  let state_offset = b.build_int_mul(cp_val, i32.const_int(8, false), "state_offset");
  let state_offset = b.build_int_add(state_offset, state, "state_offset");

  let state_cell_ptr =
    unsafe { b.build_gep(state_table_ptr, &[i32.const_zero(), state_offset.into()], "") };

  let state_cell_ptr =
    b.build_pointer_cast(state_cell_ptr, i8.ptr_type(inkwell::AddressSpace::Generic), "");
  let state = b.build_load(state_cell_ptr, "").into_int_value();
  let state = b.build_int_z_extend(state, i32, "");
  b.build_store(state_ptr, b.build_and(state, i32.const_int(0xF, false), ""));

  let state_len = b.build_and(state, i32.const_int(0xF0, false), "");
  let state_len = b.build_int_mul(state_len, cp_byte_len, "");

  let length = b.build_right_shift(state_len, i32.const_int(4, false), false, "");
  increment_input_offset_and_ptr(b, scan_ptr_ptr, scan_off_ptr, length);

  b.build_unconditional_branch(check_join);

  // -------------------------------------------------------
  b.position_at_end(class_lookup);

  let cp_val = b.build_load(token_value_ptr, "cp_val").into_int_value();

  let token_val = b
    .build_call(module.fun.get_token_class_from_codepoint, &[cp_val.into()], "")
    .try_as_basic_value()
    .left()?
    .into_int_value();

  // Store class value after clamping the range from 0-15.
  b.build_store(token_value_ptr, b.build_and(token_val, i32.const_int(0xF, false), ""));

  b.build_unconditional_branch(slow_path_resolve);

  b.position_at_end(check_join);

  let state_length_val = b.build_load(state_ptr, "").into_int_value();

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

fn create_char_lookup(symbols: &Vec<TokenData>) -> Vec<u64> {
  let mut byte_vec = Vec::default();
  for _ in 0..256 {
    byte_vec.push(0);
  }
  for TokenData { class, tok_val, val } in symbols {
    let tok_val = *tok_val as u64;
    match (*class, *val) {
      (InputType::T03_CLASS, v) if v == CodePointClass::Any as u32 => {
        for i in 0..256 {
          byte_vec[i] = tok_val;
        }
      }
      (InputType::T03_CLASS, v) if v == CodePointClass::Symbol as u32 => {
        air(&mut byte_vec, tok_val, &[0x21, 0x2F, 0x3A, 0x40, 0x5B, 0x60, 0x7B, 0x7E], 8);
      }
      (InputType::T03_CLASS, v) if v == CodePointClass::NewLine as u32 => {
        aii(&mut byte_vec, tok_val, &[0xA], 1);
      }
      (InputType::T03_CLASS, v) if v == CodePointClass::Space as u32 => {
        aii(&mut byte_vec, tok_val, &[0x20], 1);
      }
      (InputType::T03_CLASS, v) if v == CodePointClass::Identifier as u32 => {
        air(&mut byte_vec, tok_val, &UNI_ID_START_RANGES, UNI_ID_START_RANGES.len());
        aii(&mut byte_vec, tok_val, &UNI_ID_START_INDICES, UNI_ID_START_INDICES.len());
        air(&mut byte_vec, tok_val, &UNI_ID_CONT_RANGES, UNI_ID_CONT_RANGES.len());
        aii(&mut byte_vec, tok_val, &UNI_ID_CONT_DISCRETE, UNI_ID_CONT_DISCRETE.len());
      }
      (InputType::T03_CLASS, v) if v == CodePointClass::Number as u32 => {
        air(&mut byte_vec, tok_val, &[48, 57], 2);
      }
      (InputType::T05_BYTE, byte_val) => {
        byte_vec[byte_val as usize] = tok_val;
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

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct TokenData {
  pub(crate) class:   u32,
  pub(crate) val:     u32,
  pub(crate) tok_val: u32,
}

#[derive(Clone, Debug)]
pub(crate) struct ScannerData {
  /// A Vector of Symbol Class and Symbol Id pairs
  pub(crate) symbols:      Vec<TokenData>,
  pub(crate) states_table: Vec<u8>,
  pub(crate) accept_col:   u8,
}

pub(crate) fn can_simd(
  instruction: INSTRUCTION,
  g: &GrammarStore,
  output: &BytecodeOutput,
) -> SherpaResult<ScannerData> {
  // Evaluate whether we can use a SIMD operations to construct this state.
  // The criteria for SIMD rolling are:
  // - All branches are a mixture of either bytes or classes
  // -* Branches that goto other states lead back to this state
  // -
  let bc = &output.bytecode;
  let mut state_queue = VecDeque::from_iter(vec![(instruction.get_address(), instruction)]);

  let mut seen_instructions = BTreeSet::new();
  let mut known_states = BTreeMap::new();
  let mut symbols_map = BTreeMap::new();
  let mut byte_to_token_map = BTreeMap::new();
  let mut state_number = 0;
  let mut token_count = 5; // Offset the token id by the number of
                           // default symbol classes (5 at this point)
  let mut have_pass = false;

  while let Some((entry, instruction)) = state_queue.pop_front() {
    use InstructionType::*;
    if seen_instructions.insert(instruction) {
      match instruction.to_type() {
        VectorBranch | HashBranch => {
          if !known_states.contains_key(&entry) {
            known_states.insert(entry, state_number);
            state_number += 1;
          }

          let BranchTableData { data, branches, .. } =
            BranchTableData::from_bytecode(instruction, g, output)?;

          for (mut value, branch) in branches {
            match data.input_type {
              InputType::T04_CODEPOINT => return SherpaResult::None,
              InputType::T05_BYTE | InputType::T03_CLASS => {
                let mut instruction = INSTRUCTION::from(bc, branch.address);
                let mut goto = 0;
                let mut have_shift = false;
                let mut have_token = false;
                let mut value = value as u32;
                let mut tok_val = value;

                if matches!(data.input_type, InputType::T05_BYTE) {
                  if value > 127 {
                    return SherpaResult::Err(
                      "Invalid byte value: All byte values must be in the ASCII range 0-127".into(),
                    );
                  }
                  tok_val = (*byte_to_token_map.entry(value).or_insert_with(|| {
                    token_count += 1;
                    token_count
                  })) as u32;
                }

                loop {
                  match instruction.to_type() {
                    Token => {
                      have_token = true;
                      //_token_val = instruction.get_token_value();
                      instruction = instruction.next(bc);
                    }
                    Shift => {
                      have_shift = true;
                      instruction = instruction.next(bc);
                    }
                    Fail => {
                      break;
                    }
                    Pass => {
                      have_pass = true;
                      break;
                    }
                    Goto => {
                      if !have_shift {
                        return SherpaResult::None;
                      }
                      let goto_state = instruction.goto(bc);
                      goto = goto_state.get_address();
                      state_queue.push_back((goto, goto_state));
                      if !instruction.next(bc).is_pass() {
                        return SherpaResult::None;
                      } else {
                        break;
                      }
                    }
                    _ => {
                      return SherpaResult::None;
                    }
                  }
                }

                // All branches need to shift on token or set the current token.
                if !have_shift && !have_token {
                  //return SherpaResult::None;
                }

                // Accept states should not continue

                symbols_map
                  .entry(TokenData { val: value, class: data.input_type, tok_val })
                  .or_insert_with(|| vec![])
                  .push((entry, goto, instruction.to_type(), have_shift))
              }
              _ => panic!("lexer type should not exist in this context {}", data.input_type),
            }
          }

          state_queue.push_back((entry, instruction.branch_default(bc)));
        }
        Pass => {}
        Fail => {}
        _ => state_queue.push_back((entry, instruction.next(bc))),
      }
    }
  }

  if !have_pass || symbols_map.len() < 2 || state_number < 2 {
    SherpaResult::None
  } else {
    let symbols = symbols_map.keys().cloned().collect::<Vec<_>>();

    let column_count = 8;
    let accept_col = (column_count - 1) as u8;

    let mut states_table = Vec::with_capacity(column_count * token_count);
    states_table.resize_with(column_count * token_count, || 0);

    for (TokenData { tok_val, .. }, states) in symbols_map {
      let offset = tok_val as usize * column_count;
      // prefill
      let state_table = &mut states_table[(offset)..(offset + column_count)];

      for state in states {
        let s = (*known_states.get(&state.0)? as usize) + 1;
        let shift = (state.3 as u8) << 4;
        if state.2 == InstructionType::Pass {
          state_table[s] = (shift | accept_col) as u8;
        } else {
          state_table[s] = shift | ((*known_states.get(&state.1)? as u8) + 1);
        }
      }
      state_table[accept_col as usize] = accept_col;
    }

    println!("{:#?} {:#?}", states_table, symbols);

    SherpaResult::Ok(ScannerData { symbols, states_table, accept_col })
  }
}
