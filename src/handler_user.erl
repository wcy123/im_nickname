-module(handler_user).
-export([handle/4]).
-compile([{parse_transform, lager_transform}]).
handle(Method, Req, DocRoot, Options) ->
    Path = Req:get(path),
    Components = filename:split(Path),
    Drop = proplists:get_value(drop, Options, 0),
    _Root = proplists:get_value(root, Options, DocRoot),
    lager:debug("~s~n", [erlang:iolist_to_binary(["debugging: \n"
                                                 , "\tComponents = ", io_lib_pretty:print(Components), "\n"
                                                 ])]),
    [UserName|RestArgs] = lists:nthtail(Drop, Components),
    Headers = [{"Content-Type", "application/json"}],
    Req:respond({200, Headers,
                 ["{\"a\":\"Hello ", UserName,  "\n\"}"]}).
