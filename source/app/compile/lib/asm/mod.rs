pub mod x86_64_asm;

#[cfg(test)]
mod test_x86_generation
{
    use crate::asm::x86_64_asm::compile_from_bytecode;
    use crate::builder::table::create_table;
    use crate::builder::table::BranchTableData;
    use crate::options::Architecture;
    use crate::options::BuildOptions;
    use crate::writer::nasm_writer::NasmWriter;
    use crate::writer::x86_64_writer::X8664Writer;
    use hctk::bytecode::compile_bytecode;
    use hctk::debug::generate_disassembly;

    #[test]
    fn test_generate_transition_table_from_scanner_state()
    {
        use hctk::debug::compile_test_grammar;
        use hctk::types::*;

        let grammar = compile_test_grammar(
            "
@IGNORE g:sp

@EXPORT test as entry

@EXPORT test as banner

@NAME nasm_test

<> test > \\hello \\world",
        );

        let output = compile_bytecode(&grammar, 1);

        for (offset, name) in output.offset_to_state_name.iter() {
            if let Some(state) = output.ir_states.get(name) {
                if matches!(state.get_type(), IRStateType::ProductionStart) {
                    let data = BranchTableData::from_bytecode(
                        *offset as usize,
                        &output,
                    );

                    println!("{:#?}", data.unwrap());
                }/* 
                if state.is_scanner()
                    && matches!(state.get_type(), IRStateType::ScannerStart)
                {
                    create_table(*offset, &output);
                } */
            }
        }
    }

    #[test]
    fn test_nasm_output_on_trivial_grammar()
    {
        use hctk::debug::compile_test_grammar;

        let grammar = compile_test_grammar(
            "
@IGNORE g:sp

@EXPORT test as entry

@EXPORT test as banner

@NAME nasm_test

<> test > \\hello \\world 
",
        );

        let output = compile_bytecode(&grammar, 1);

        let mut writer = NasmWriter::new(Vec::<u8>::new());

        let build_options = BuildOptions {
            architecture: Architecture::X8664,
            ..Default::default()
        };

        let result = compile_from_bytecode(&build_options, &output, &mut writer);

        // println!("{:#?}", output.state_name_to_offset);

        assert!(result.is_ok());

        println!("\n\n{}\n\n", generate_disassembly(&output, None));
        println!("\n\n{}\n\n", String::from_utf8(writer.into_writer()).unwrap());
    }
}
