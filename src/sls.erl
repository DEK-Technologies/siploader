-module(sls).

-behaviour(application).

-export([start/2, stop/1]).
% -export([get/1, get/2, put/2, del/1]).
% -export([profile_output/0]).

%% ===================================================================
%% Private
%% ===================================================================

%% @doc Starts stand alone.

start(_Type, _Startargs) ->
    sls_sup:start_link().

%% @private OTP standard stop callback
stop(_) ->
    ok.
