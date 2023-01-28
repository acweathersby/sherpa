NAME syntax

IGNORE { c:sp c:nl }

IMPORT ./ascript as ast

<> declaration >  '{' field(+,) '}'

    :ast { t_Syntax, specs:$2 }

<> field > ast::reference ':' syntax_spec

    :ast { t_SyntaxField, reference:$1, spec:$2 }

<> syntax_spec > [ ast::identifier?  color? ]

    :ast { t_SyntaxSpec, id:$1, rgb:$2 }

<> color > "rgb" '(' [unordered red^r green^g blue^b alpha?^a ] ')'

    :ast { t_RGBA, r:$r, g:$g, b:$b, a:$a }

<> red > "r"  tk:ast::integer  :ast u32($2)

<> green > "g"  tk:ast::integer  :ast u32($2)

<> blue > "b"  tk:ast::integer  :ast u32($2)

<> alpha > "a" tk:ast::integer  :ast u32($2)
