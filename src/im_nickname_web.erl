-module(im_nickname_web).
-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    try
        handle_request(Req:get(method), Req, DocRoot)
    catch
        Type:What ->
            Report = ["web request failed",
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.


%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

handle_request(Method, Req, DocRoot) ->
    Path = Req:get(path),
    case im_nickname_handler:get(Path) of
        {M,F} ->
            M:F(Method, Req, DocRoot);
        {M,F,A} ->
            M:F(Method, Req, DocRoot, A);
        _ ->
            default_handler(Method, Req, DocRoot)
    end.

default_handler(Method, Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                "hello_world" ->
                    Req:respond({200, [{"Content-Type", "text/plain"}],
                                 "Hello world!\n"});
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

hello_om(_Method, Req, _DocRoot) ->
    Req:respond({200, [{"Content-Type", "text/plain"}],
                 "Hello World!\n"}).
