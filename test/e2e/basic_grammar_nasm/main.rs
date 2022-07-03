mod parser;

#[cfg(test)]
mod test
{
    use hctk::types::UTF8StringReader;

    use crate::parser;

    #[test]
    pub fn test_build()
    {
        let mut reader = UTF8StringReader::new("test".to_string());
        let mut parse = parser::MyParser::new(&mut reader);

        let action = parse.next();

        println!("{:?}", action)
    }
}
