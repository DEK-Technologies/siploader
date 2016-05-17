Definitions.

Non0 = [1-9]
D   = [0-9]
B   = [01]
H   = [0-9A-Fa-f]
A   = [A-Za-z]
L   = [A-Za-z0-9_]
UA  = [A-Z]
UL  = [A-Z0-9_]
WS  = ([\000-\s]|#.*|//.*|/\*.*\*/)
S = [^\"]

Rules.

true      : {token, {boolean,TokenLine,true}}.
false     : {token, {boolean,TokenLine,false}}.
"{S}*"    : S = strip(TokenChars,TokenLen),
       	     {token,{string,TokenLine,S}}.
{UA}{UL}* : {token,{uppercase_identifier,TokenLine,list_to_atom(TokenChars)}}.
{A}{L}*   : {token,{mix_identifier,TokenLine,list_to_atom(TokenChars)}}.
(\-)?({Non0}{D}*|0)\.{D}+((E)(\-)?{D}+)? :
  {token,{float,TokenLine,list_to_float(TokenChars)}}.
(\-)?({Non0}{D}*|0) :
  {token,{integer,TokenLine,list_to_integer(TokenChars)}}.
{WS}+     : skip_token.
[\[\],;]  : {token,{list_to_atom(TokenChars),TokenLine}}.
[{}.$*-]  : {token,{list_to_atom(TokenChars),TokenLine}}.
:=        : {token,{list_to_atom(TokenChars),TokenLine}}.
not_a_number : {token,{list_to_atom(TokenChars),TokenLine}}.
'{B}*'B   : {token, {bitstring,TokenLine,strip2(TokenChars,TokenLen)}}.
'{H}*'H   : {token, {hexstring,TokenLine,strip2(TokenChars,TokenLen)}}.
'({H}{H})*'O : {token, {octetstring,TokenLine,strip2(TokenChars,TokenLen)}}.

Erlang code.

% Remove first and last
strip(TokenChars,TokenLen) -> 
    lists:sublist(TokenChars, 2, TokenLen - 2).

% Remove first and 2 last
strip2(TokenChars,TokenLen) -> 
    lists:sublist(TokenChars, 2, TokenLen - 3).
