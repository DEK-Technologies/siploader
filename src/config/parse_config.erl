-module(parse_config).

-export([file/1]).

file(FileName) ->
    {ok, File} = file:read_file(FileName),
    S1 = unicode:characters_to_list(File),
    S2 = remove_multiline_comments(S1),
    config_scanner:string(S2).

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
remove_comment([_Char | Rest], Handled) ->
    remove_comment(Rest, Handled).
