Nonterminals 
sections section section_header statements statement assignment
variable_ref expression single_expression compound_expression
array_expression array_element_expr_list not_used_or_expr
field_expression_list field_expr_specs field_expr_spec field_ref
value float_value extended_identifier identifier
macro identifier_list parameter_template wildcard_variable wildcard_ident.

Terminals '[' ']' '{' '}' '.' ',' ';' '*' '$' ':=' '-'
mix_identifier integer float string uppercase_identifier not_a_number.

Rootsymbol sections.

%% Define operator associativity to prevent shift/reduce conflict warning
Left 100 '.'.

sections -> section : ['$1'].
sections -> section sections : ['$1'] ++ '$2'.

section -> section_header statements : section('$1', '$2').

section_header -> '[' uppercase_identifier ']' : '$2'.

statements -> statement : ['$1'].
statements -> statement statements : ['$1'] ++ '$2'.

statement -> statement ';' : '$1'.
statement -> assignment : '$1'.
%% [INCLUDE] specific statements?
statement -> string : '$1'.
%% [EXECUTE] specific statements?
statement -> extended_identifier : '$1'.
%% [TESTPORT_PARAMETERS] specific statements?
statement -> parameter_template : '$1'.

parameter_template -> wildcard_variable ':=' expression : {template,'$1','$3'}.

assignment -> variable_ref ':=' expression : assignment('$1', '$3').

variable_ref -> identifier : variable('$1').

expression -> single_expression : '$1'.
expression -> compound_expression : '$1'.

compound_expression -> field_expression_list : '$1'.
compound_expression -> array_expression : '$1'.
compound_expression -> macro: '$1'.

macro -> '$' identifier : {'macro', '$2'}.
macro -> '$' '{' identifier_list '}' : {'macro', '$3'}.

identifier_list -> identifier : ['$1'].
identifier_list -> identifier ',' identifier_list : ['$1'] ++ '$2'.

field_expression_list -> '{' field_expr_specs '}': '$2'.

field_expr_specs -> field_expr_spec : ['$1'].
field_expr_specs -> field_expr_specs ',' field_expr_spec : '$1' ++ ['$3'].

field_expr_spec -> field_ref ':=' not_used_or_expr : {'$1', '$3'}.

field_ref -> identifier : field('$1').

array_expression -> '{' '}' : [].
array_expression -> '{' array_element_expr_list '}' : '$2'.

array_element_expr_list -> not_used_or_expr : ['$1'].
array_element_expr_list -> array_element_expr_list ',' not_used_or_expr : '$1'.

not_used_or_expr -> '-' : '$1'.
not_used_or_expr -> expression : '$1'.

single_expression -> value : '$1'.

wildcard_variable -> wildcard_ident '.' wildcard_ident : ['$1', '$3'].
wildcard_variable -> wildcard_variable '.' wildcard_ident : '$1' ++ ['$3'].

wildcard_ident -> identifier : '$1'.
wildcard_ident -> '*': wildcard('$1').

extended_identifier -> identifier '.' identifier : ['$1', '$3'].
extended_identifier -> extended_identifier '.' identifier : '$1' ++ ['$3'].

identifier -> uppercase_identifier: '$1'.
identifier -> mix_identifier: '$1'.

value -> integer : '$1'.
value -> float_value : '$1'.
value -> string : '$1'.
value -> identifier : '$1'.

float_value -> float: '$1'.
float_value -> not_a_number: nan('$1').

Erlang code.

nan({not_a_number,Line,Val}) -> {'float', Val, Line}.
section({_,Line,Val}, Block) -> {'section', Val, Block, Line}.
assignment({_,Line,Val}, Right) -> {'assignment', Val, Right, Line}.
variable({_,Line,Val}) -> {'variable', Val, Line}.
field({_,Line,Val}) -> {'field', Val, Line}.
wildcard({WC,Line}) -> {wildcard, Line, WC}.