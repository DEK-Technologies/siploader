-module(sls_sup).

-behaviour(supervisor).

% call back
-export([init/1]).

% interface
-export([start_link/0]).

% call back function

init(ChildSpecs) ->
    {ok, ChildSpecs}.

% interface functions

start_link() ->
    ChildsSpec = [
		  {sls_loader,
		   {sls_loader, start_link, []},
		   permanent,
		   5000,
		   worker,
		   [sls_loader]}
	],
    supervisor:start_link({local, ?MODULE}, ?MODULE, {{one_for_one, 0, 1}, ChildsSpec}).

