-module(call_demo).

-export([start/0]).


% cd /home/dansahlin/proj/siploader/submodules/nksip
% make shell-sls
% c('../../src/call_demo').
% call_demo:start().
%
% nksip:stop_all().

start() ->
    nksip:stop_all(),
    start_server(),
    NumCalls = 3,
    io:format("Starting with ~p calls~n", [NumCalls]),
    start_and_register_clients(caller, NumCalls, 5070),
    start_and_register_clients(callee, NumCalls, 6070),
%    nksip_trace:start(),
    Dialogs = make_calls(caller, callee, NumCalls),
    timer:sleep(5000),
    io:format("Dialogs: ~p~n", [Dialogs]),
    make_bye(Dialogs).
    
start_server() ->
    {ok, _} = 
	nksip:start(server, #{
		      sip_local_host => "localhost",
		      callback => nksip_tutorial_server_callbacks,
		      plugins => [nksip_registrar],
		      transports => "sip:127.0.0.1:5060, sips:127.0.0.1:5061"
%%%			    transports => "sip:all:5060, <sip:all:5061;transport=tls>"
		     }).

start_and_register_clients(Client, NumCalls, Port) ->
    [begin
	 ClientName = client_name(Client, Num),
	 start_client(ClientName, Port+2*Num-2, Port+2*Num-1),
	 {ok, 200, _} = nksip_uac:register(ClientName, "sip:127.0.0.1", 
					   [{sip_pass, "1234"}, contact]) 
     end ||
	Num <- lists:seq(1, NumCalls)].

client_name(Client, Num) ->
    list_to_atom(atom_to_list(Client)++integer_to_list(Num)).

start_client(ClientName, SIPport, SIPSport) ->
    io:format("client ~p started~n", [ClientName]),
    {ok, _} = nksip:start(ClientName, #{
			    sip_local_host => "localhost",
			    sip_from => "sip:"++atom_to_list(ClientName)++"@nksip",
			    callback => nksip_tutorial_client_callbacks,
			    plugins => [nksip_uac_auto_auth],
			    transports => "sip:127.0.0.1:"++
				integer_to_list(SIPport)++
				", sips:127.0.0.1:"++
				integer_to_list(SIPSport)
			   }).

make_calls(Caller, Callee, NumCalls) ->
    Pids = [spawn_invite(client_name(Caller, Num), 
			 client_name(Callee, Num)) ||
	       Num <- lists:seq(1, NumCalls)],
    collect_dialogs(Pids).

spawn_invite(Client1, Client2) ->
    ReturnPid = self(),
    spawn(fun () ->
		  spawn_invite2(ReturnPid, Client1, Client2)
	  end).

spawn_invite2(ReturnPid, Client1, Client2) ->
    {ok,200,[{dialog, DlgId}]} =
	nksip_uac:invite(Client1, "sip:"++atom_to_list(Client2)++"@nksip", 
			 [{route, "<sip:127.0.0.1;lr>"}, 
			  {body, nksip_sdp:new()},
			  auto_2xx_ack]),
    ReturnPid ! {self(), DlgId}.

make_bye(Dialogs) ->
    [{ok, 200, []} = 
	 nksip_uac:bye(Dialog, []) || Dialog <- Dialogs].

collect_dialogs(Pids) ->
    [receive 
	 {Pid, DialogId} -> DialogId
     end || Pid <- Pids].
