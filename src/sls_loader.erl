%% 
-module(sls_loader).

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2]).

-export([start_link/0]).

-export([config/1, start/0, stop/0]).
%% ===================================================================
%% Public
%% ===================================================================
%%% interface functions

config(File) ->
    gen_server:call(?MODULE, {read, File}, infinity).

start() ->
    gen_server:call(?MODULE, start, infinity).

stop() ->
    gen_server:stop(?MODULE).

%% ===================================================================
%% gen_server
%% ===================================================================

start_link() ->
    io:format("sls_loader:sls_sup~n"),
%%%    {ok, spawn(fun test/0)}.
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%%% callback functions

init(no_args) ->
    {ok, no_configuration}.

handle_call({read, File}, _From, _State) ->
    Config = parse_config:parse(File),
    io:format("Config= ~p~n", [Config]),
    {reply, ok, Config};  %% not always ok!
handle_call(start, _From, Config) ->
    io:format("STARTING ***********************~n"),
    Scenarios = maps:get(tsp_IMS_Configuration_Scenarios, maps:get('MODULE_PARAMETERS', Config)),
    nksip:stop_all(),
    start_server(),
    scenarios(Scenarios),
    {reply, ok, Config}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

scenarios(Scenarios) ->
    [scenario(Scenario) || Scenario <- Scenarios].

scenario(Scenario) ->
    % ScenarioNames in test file "Call", "Tls", "NNI"
    ScenarioName = maps:get(scenarioName, Scenario), % string()
    TrafficCases = maps:get(trafficCases, Scenario), % list() T1
    [trafficCase(ScenarioName, TrafficCase) || TrafficCase <- TrafficCases].

trafficCase(ScenarioName, TrafficCase) ->
    [callGroup,generic,name,trafficCase,trafficSpecific,
     transport] =  lists:sort(maps:keys(TrafficCase)),
    CallGroup = string_to_map(maps:get(callGroup, TrafficCase)),
    Transport = maps:get(transport, TrafficCase),

    io:format("Scenaio: ~s~n", [ScenarioName]),
    start_clients(CallGroup, Transport).
%    start_registrations(Clients),
%    start_calls(maps:get(trafficSpecific, TrafficCase)),
			
%    callGroup(maps:keys(callGroup, TrafficCase)),

%%%     maps:get(callGroup, hd(T1)) is
%%% "\n\t\tpublicIdPrefix=acc1;\n\t\tIdLength=5;\n\t\tIdRange=00000..00009;\n                homeDomain=system.test;\n                password=imt40imt40;\n                privateIdPrefix=acc1;\n                privateDomain=system.test;\n                telIdPrefix=123456;"

%%% maps:get(generic, hd(T1)) is
%%%#{burstCalcMethod => "noMemCs",userWatchdog => 350.0}

%%% maps:get(name, hd(T1)).     
%%% "acc1Aacc1B"

%%% maps:get(trafficCase, TrafficCase).
%%% "CallOrig"
start_calls(TrafficSpecific) ->
%% start all terminating first
    [OrigOrTerm, media, registration] = lists:sort(maps:keys(TrafficSpecific)),
    case OrigOrTerm of
	callOrig ->
	    CallOrig = maps:get(callOrig, TrafficSpecific),
	    CalledGroupString = maps:get(calledGroup, CallOrig),
	    CalledGroup = string_to_map(CalledGroupString),
	    io:format("callOrig/CalledGroup: ~p~n", [CalledGroup]);
	callTerm ->
	    CallTerm = maps:get(callTerm, TrafficSpecific),
	    io:format("callTerm: answeringTimeMax: ~p~n", 
		      [maps:get(answeringTimeMax, CallTerm)])
    end.
    
start_clients(CallGroup, Transport) ->
%%% "\n\t\tpublicIdPrefix=acc1;\n\t\tIdLength=5;\n\t\tIdRange=00000..00009;\n                homeDomain=system.test;\n                password=imt40imt40;\n                privateIdPrefix=acc1;\n                privateDomain=system.test;\n                telIdPrefix=123456;
    io:format("CallGroup: ~p~n", [CallGroup]),
    {StartId, EndId} = maps:get('IdRange', CallGroup),
    StartPort = maps:get(uePortLow, Transport), 
    EndPort = maps:get(uePortHigh, Transport), 
    start_client_loop(StartId, EndId, StartPort, EndPort, CallGroup).

start_client_loop(Id, EndId, _Port, _EndPort, _CallGroup) when Id > EndId ->
    ok;
start_client_loop(Id, EndId, Port, EndPort, CallGroup) ->
    true = (Port =< EndPort), % just checking
    ClientName =  client_name(Id, CallGroup),
    io:format("client ~p started~n", [ClientName]),
    {ok, _} = nksip:start(ClientName, #{
			    sip_local_host => "localhost",
			    sip_from => "sip:"++ClientName,
			    callback => nksip_tutorial_client_callbacks,
			    plugins => [nksip_uac_auto_auth],
			    transports => "sip:127.0.0.1:"++    %%%!! Completely ignores IP address settings
				integer_to_list(Port)
			   }),
    start_client_loop(Id+1, EndId, Port+1, EndPort, CallGroup).

client_name(Id, CallGroup) ->
    IdLength = maps:get('IdLength', CallGroup),
    maps:get(privatePrefix, CallGroup) ++
	maps:get(telIdPrefix, CallGroup) ++ % may not both prefixes?
	iolib:format("~"++IdLength++"..0w", [Id]) ++ % prints with leading zeros up to length IdLength
	"@" ++
	maps:get(homeDomain, CallGroup). % or should it be privateDomain?



do_registrations(_CallGroup, Registration) ->
    
%    step_up(RangeStart, RangeEnd, 0, fun do_registration/1),

    timer:sleep(round(list_to_float(maps:get(delayInitialRegister, Registration))*1000)),
    _Delay_ms = round(1000/list_to_float(maps:get(rps, Registration))),
%    step_up(RangeStart, RangeEnd, 0, fun do_registration/1).
    ok.
    

%% #{callOrig => #{allowMultipleEarlyDialogs => false,
%%     callHoldMax => 180.0,
%%     callHoldMin => 180.0,
%%     calledGroup => "publicIdPrefix=acc1;IdLength=5;IdRange=05000..05009;homeDomain=system.test;",
%%     cancelTimeMax => 0.0,
%%     cancelTimeMin => 0.0,
%%     cps => 0.0,
%%     maxRetryAfter => 3,
%%     useTelUri => false},
%%   media => #{enabled => true,
%%     generator => #{mlsim => #{enableMLSimPlus => true,
%%         enableSessionCriteria => true,
%%         enabled => true,
%%         maxNumberOfSessions => 2,
%%         nrOfNormalSessions => 0,
%%         simplificationLevel => 0,
%%         startNumOfSessions => 1,
%%         supportedCodecs => ['AMR_122_OA','H_263']}}},
%%   registration => #{'PAccessNetworkInfo' => [],
%%     authHeaderInInitialReg => false,
%%     delayInitialRegister => 0.0,
%%     deregisterCurrentContactOnly => false,
%%     distributeRegister => false,
%%     enableSTUN => false,
%%     halfcall => false,
%%     includeTransportInContact => true,
%%     keepAliveSTUN => 30.0,
%%     maxRetryAfter => 10,
%%     reRegMargin => 5.0,
%%     reRegistration => true,
%%     regWatchdog => 40.0,
%%     registrationExpires => 3.6e3,
%%     retryAfterFail => 30.0,
%%     rps => 1.0,
%%     sbgRegMinExpires => 0.0,
%%     tempId => false}}

%% maps:get(transport, TrafficCase).      
%% [#{host => [],
%%    hostTransport => [#{proxyTransport => #{preferredTransport => "udp",
%%         proxyList => [#{proxyName => "3001:21::4",proxyPort => 5060}],
%%         transportType => 'IPL4'},
%%       simulationType => #{ueTransport => #{ueIpHigh => "3001:100:16::6",
%%           ueIpLow => "3001:100:16::2",
%%           uePortHigh => 30000,
%%           uePortLow => 10000}}}]}]




% test() ->
%    X=parse_config:parse("/home/dansahlin/proj/siploader/src/titansim/titan1.cfg"),
%    io:format("~p~n", [X]).


string_to_map(CallGroup) ->
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

start_server() ->
    {ok, _} = 
	nksip:start(server, #{
		      sip_local_host => "localhost",
		      callback => nksip_tutorial_server_callbacks,
		      plugins => [nksip_registrar],
		      transports => "sip:127.0.0.1:5060, sips:127.0.0.1:5061"
%%%			    transports => "sip:all:5060, <sip:all:5061;transport=tls>"
		     }).
