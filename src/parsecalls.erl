-module(parsecalls).

-export([test/0]).

test() ->
    CallGroup = "\n\t\tpublicIdPrefix=acc1;
\n\t\tIdLength=5;\n\t\tIdRange=00000..04999;\n                homeDomain=system.test;\n                password=imt40imt40;\n                privateIdPrefix=acc1;\n                privateDomain=system.test;\n                telIdPrefix=123456;",
    callgroup_to_map(CallGroup).

callgroup_to_map(CallGroup) ->
    Lines = string:tokens(CallGroup, "\n\t ;"),
    DefList = [string:tokens(Line, "=") || Line <- Lines],
    maps:from_list(to_fixed_tuple(DefList)).

to_fixed_tuple(DefList) ->
    [{list_to_atom(Key), fix_value(Value)} || [Key, Value] <- DefList].

fix_value(Value) ->
    case string:to_integer(Value) of
	{Int, ""} ->
	    Int;
	{Int, ".."++Rest} ->
	    case string:to_integer(Rest) of
		{Int2, ""} ->
		    {Int, Int2};
		_ ->
		    Value
	    end;
	_ ->
	    Value
    end.
