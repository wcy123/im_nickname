%%%-------------------------------------------------------------------
%% @doc im_nickname top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(im_nickname_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Web = web_specs(im_nickname_web, 8080),
    Processes = [Web, handler_spec()],
    Strategy = {one_for_one, 10, 10},
    {ok,
     {Strategy, lists:flatten(Processes)}}.


%%====================================================================
%% Internal functions
%%====================================================================

web_specs(Mod, Port) ->
    WebConfig = [{ip, {0,0,0,0}},
                 {port, Port},
                 {docroot, filename:join([code:priv_dir(im_nickname), "www"])}],
    {Mod,
     {Mod, start, [WebConfig]},
     permanent, 5000, worker, dynamic}.
handler_spec() ->
    {im_nickname_handler,
     {im_nickname_handler, start_link, []},
     permanent, 5000, worker, [im_nickname_handler]}.
