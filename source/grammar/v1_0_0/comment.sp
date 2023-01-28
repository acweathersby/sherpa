<> comment > line_comment | block_comment

<> line_comment > "//" ( c:id | c:sp | c:sym | c:num ) c:nl

<> block_comment > "/*"  ( c:id | c:sp | c:sym | c:num ) "*/"