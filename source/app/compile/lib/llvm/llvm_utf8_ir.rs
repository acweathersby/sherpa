use std::io::Result;
use std::io::Write;

use crate::writer::code_writer::CodeWriter;

pub fn write_uft8_functions<W: Write>(writer: &mut CodeWriter<W>) -> Result<()>
{
  writer.wrt(
      "  
  define i32 @getUTF8CodePoint( i8 * %input ){
    
   %header_byte = load i8, i8 * %input
   %inverted_header_byte = xor i8 255, %header_byte
   %header_bit_count = call i8 @llvm.ctlz.i8( i8 %inverted_header_byte, i1 0 )
  
   %cond1 = icmp eq i8 %header_bit_count, 0
   br i1 %cond1, label %return_ascii, label %build_code_point
  
  return_ascii:
    %ascii_value = zext i8 %header_byte to i32
    ret i32 %ascii_value
  
  build_code_point:
    %off_ptr = alloca i64
    %val_ptr = alloca i32
      
    %mask = lshr i8 255, %header_bit_count
    %base_value8 = and i8 %header_byte, %mask
    %valAout =  zext i8 %base_value8 to i32
  
    store i32 %valAout, i32 * %val_ptr
    store i64 1, i64 * %off_ptr
  
    switch i8 %header_bit_count, label %invalid [
      i8 2, label %_2bytes
      i8 3, label %_3bytes
      i8 4, label %_4bytes
    ]
  
  _4bytes:
    %offA = load i64, i64 *%off_ptr
    %valA = load i32, i32 *%val_ptr
    %valA_shl = shl i32 %valA, 6
    %valB_out = call i32 @setUT8Byte( i32 %valA_shl, i8 * %input, i32 0, i64 %offA )
    %offAa = add i64 %offA, 1
    store i64 %offAa, i64 * %off_ptr
    store i32 %valB_out, i32 * %val_ptr
    br label %_3bytes
  
  _3bytes:
    %offB = load i64, i64 *%off_ptr
    %valB = load i32, i32 *%val_ptr
    %valB_shl = shl i32 %valB, 6
    %valC_out = call i32 @setUT8Byte( i32 %valB_shl, i8 * %input, i32 0, i64 %offB )
    %offBa = add i64 %offB, 1
    store i64 %offBa, i64 * %off_ptr
    store i32 %valC_out, i32 * %val_ptr
    br label %_2bytes
  
  _2bytes:
    %offC = load i64, i64 *%off_ptr
    %valC = load i32, i32 *%val_ptr
    %valC_shl = shl i32 %valC, 6
    %valD = call i32 @setUT8Byte( i32 %valC_shl, i8 * %input, i32 0, i64 %offC )
  
    ret i32 %valD
  
  invalid:
    ret i32 0
  }

  define i32 @setUT8Byte( i32 %base_value, i8 * %input, i32 %shl, i64 %off ) alwaysinline {
    
    %byte_ele_ptr = getelementptr i8, i8 * %input, i64 %off
    %byte_ele = load i8, i8 * %byte_ele_ptr
    
    %dword_ele = zext i8 %byte_ele to i32
    %dword_ele_m = and i32 %dword_ele, 63
    %dword_ele_sl = shl i32 %dword_ele_m, %shl
    
    %partial_cp = or i32 %base_value, %dword_ele_sl
  
    ret i32 %partial_cp
  }")?;

  Ok(())
}

fn construct_class_lookup() {}
