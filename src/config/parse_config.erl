-module(parse_config).

-export([file/1, parse/1]).

% Parse file and included files recursively
parse(FileName) ->
    Absolute = filename:absname(FileName),
    Path = filename:dirname(Absolute),
    File = filename:basename(Absolute),
    recursive_parse([File], Path, [], maps:new()).

recursive_parse([], _Path, _ParsedFiles, ConfigData) ->
    ConfigData;
recursive_parse([File|Files0], Path, ParsedFiles0, ConfigData0) ->
    ConfigData1 = file(Path ++ "/" ++ File),
    ParsedFiles1 = [File|ParsedFiles0],
    Files1 = update_file_list(Files0, ConfigData1, ParsedFiles1),
    ConfigData2 = merge_data(ConfigData0, ConfigData1),
    recursive_parse(Files1, Path, ParsedFiles1, ConfigData2).

update_file_list(Files0, ConfigData, ParsedFiles) ->
    Files1 = maps:get('INCLUDE', ConfigData, []),
    Files2 = lists:dropwhile(fun(F) -> lists:member(F, ParsedFiles) end, 
			     Files1),
    Files2 ++ Files0.

merge_data(NewMap, OldMap) ->
    Pairs = maps:to_list(NewMap),
    lists:foldl(fun({Key, Value}, Acc) ->
			maps:put(Key, merge_data(Key, Value, Acc), Acc)
		end, OldMap, Pairs).

merge_data(Key, Val1, Acc) ->
    case maps:find(Key, Acc) of
	error ->
	    Val1;
	{ok, Val2} when is_list(Val2) ->
	    Val1 ++ Val2;
	{ok, Val2} when is_map(Val2) ->
	    maps:merge(Val1, Val2)
    end.

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
    maps:from_list(config_data(ParseTree)).

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
    {{template, config_data(LH)}, config_data(RH)};
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
config_data({'field_expression_list', _Line, Val}) ->
    config_data(Val);
config_data({_, _Line, Val}) ->
    Val;
config_data(List0 = [First|_]) ->
    List1 = config_data_list(List0),
    case is_assignment(First) of
	true -> maps:from_list(List1);
	false-> List1
    end;
config_data(Val) ->
    Val.

% If the first element is an assignment, field_expr_spec
% or parameter_template, we will assume all of them are
% and create a map out of the list.
% If they are maybe we should catch the error to give
% a better error description.
is_assignment({'assignment', _Line, _Val, _Right}) ->
    true;
is_assignment({'field_spec', _Line, _Val, _Right}) ->
    true;
is_assignment({'template', _Line, _WC, _Val}) ->
    true;
is_assignment(_) ->
    false.

config_data_list(List = [_|_]) ->
    [config_data(Elem) || Elem <- List].
