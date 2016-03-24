Nonterminals 
sections section section_header statements statement assignment
variable_ref expression single_expression.

Terminals '[' ']' '{' '}' '.' ',' ';' '*' '$' ':='
identifier integer string uppercase_identifier.

Rootsymbol sections.

sections -> section : '$1'.
sections -> section sections : ['$1'] ++ '$2'.

section -> section_header statements : section('$1', '$2').

section_header -> '[' uppercase_identifier ']' : '$2'.

statements -> statement : '$1'.
statements -> statement statements : ['$1'] ++ '$2'.

statement -> assignment : '$1'.

assignment -> variable_ref ':=' expression : assignment('$1', '$3').

variable_ref -> identifier : variable('$1').
variable_ref -> uppercase_identifier : variable('$1').

expression -> single_expression : '$1'.
%expression -> compound_expression : '$1'.

single_expression -> integer : '$1'.
single_expression -> string : '$1'.

Erlang code.

section({_,Line,Val}, Block) -> {'section', Val, Block, Line}.
assignment({_,Line,Val}, Right) -> {'assignment', Val, Right, Line}.
variable({_,Line,Val}) -> {'variable', Val, Line}.
