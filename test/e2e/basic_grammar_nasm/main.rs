mod nasm_test_parser;

#[cfg(test)]
mod test
{
    use crate::nasm_test_parser;
    use hctk::types::UTF8StringReader;

    #[test]
    pub fn test_build()
    {
        let mut reader = UTF8StringReader::new("test".to_string());
        let mut ctx = nasm_test_parser::Context::new(&mut reader);

        ctx.set_start_point(nasm_test_parser::StartPoint::BANNER);

        let action = ctx.next();

        println!("{:?}", action)
    }
}
