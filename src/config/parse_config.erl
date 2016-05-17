-module(parse_config).

-export([file/1]).

file(FileName) ->
    {ok, File} = file:read_file(FileName),
    S1 = unicode:characters_to_list(File),
    S2 = remove_multiline_comments(S1),
    {ok, Tokens, _EndLineNr} = config_scanner:string(S2),
%    io:format("Tokens: ~p\n", [Tokens]),
    check(config_parser:parse(Tokens)).

check({error, Msg}) ->
    {parse_error, Msg};
check({ok, ParseTree}) ->
    config_data(ParseTree).

% Remove comments on the form /* ... */
remove_multiline_comments(String) ->
    remove_multiline_comments(String, []).

remove_multiline_comments([], Handled) ->    
    lists:reverse(Handled);
remove_multiline_comments([$/, $* | Rest], Handled) ->
    remove_comment(Rest, Handled);
remove_multiline_comments([Char | Rest], Handled) ->
    remove_multiline_comments(Rest, [Char | Handled]).

remove_comment([$*, $/ | Rest], Handled) ->
    remove_multiline_comments(Rest, Handled);
remove_comment([$\n | Rest], Handled) ->
    remove_comment(Rest, [$\n | Handled]); % Preserve the line numbering
remove_comment([_Char | Rest], Handled) ->
    remove_comment(Rest, Handled).

% Take a parse tree and output config date without line information
config_data({template, _Line, LH, RH}) ->
    {template, config_data(LH), config_data(RH)};
config_data({not_a_number, _Line, _}) ->
    {float, not_a_number};
config_data({'assignment', _Line, LH, RH}) ->
    {LH, config_data(RH)};
config_data({'field_spec', _Line, LH, RH}) ->
    {config_data(LH), config_data(RH)};
config_data({'section', _Line, Val, Block}) ->
    {Val, config_data(Block)};
config_data({macro, _Line, List = [_|_]}) ->
    {macro, [Ident || {identifier,_,Ident} <- List]};
config_data({macro, _Line, {identifier,_,Ident}}) ->
    {macro, Ident};
config_data({wildcard, _Line, Val}) ->
    {wildcard, Val};
config_data({_, _Line, Val}) ->
    Val;
config_data(List = [_|_]) ->
    [config_data(Elem) || Elem <- List];
config_data(Val) ->
    Val.
