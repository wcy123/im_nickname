-module(handler_user).
-export([handle/4]).
-compile([{parse_transform, lager_transform}]).
max_recv_body() -> 1024* 1024.
handle(Method, Req, DocRoot, Options) ->
    Path = Req:get(path),
    Components = filename:split(Path),
    Drop = proplists:get_value(drop, Options, 0),
    _Root = proplists:get_value(root, Options, DocRoot),
    [UserName|_RestArgs] = lists:nthtail(Drop, Components),
    case Method of
        'GET' ->
            handle_get(Req, UserName);
        'POST' ->
            handle_post(Req, UserName)
    end.

handle_get(Req,UserName) ->
    case im_redis:q(nickname_redis_pool, [hgetall, ["i:", UserName]]) of
        {ok, List} when is_list(List) ->
            PList = im_redis:list2plist(List),
            Headers = [{"Content-Type", "application/json"}],
            Req:respond({200, Headers, jsx:encode(PList)});
        {error, Error}->
            Headers = [{"Content-Type", "text/plain"}],
            Req:respond({502, Headers, ["internal error:", io_lib:format("~p~n", [Error])]})
    end.

handle_post(Req,UserName) ->
    Data = Req:recv_body(max_recv_body()),
    lager:debug("~s~n", [erlang:iolist_to_binary(["debugging: \n"
                                                 , "\tData = ", io_lib_pretty:print(Data), "\n"
                                                 ])]),
    try process_json_map(jsx:decode(Data, [return_maps]), UserName) of
        {ok, _} ->
            Headers = [{"Content-Type", "text/plain"}],
            Req:respond({200, Headers, ["ok"]});
        {error, Error}->
            Headers = [{"Content-Type", "text/plain"}],
            Req:respond({502, Headers, ["internal error:", io_lib:format("~p~n", [Error])]})
    catch
        C:E ->
            Headers = [{"Content-Type", "text/plain"}],
            Req:respond({502, Headers, ["exception ", io_lib:format("~p:~p ~p~n", [C,E, erlang:get_stacktrace()])]})
    end.


process_json_map(#{
                    <<"dl_userid">>:= UID,
                    <<"dl_nickname">>:= NickName
                  }, UserName) ->

    im_redis:q(nickname_redis_pool, [hmset, ["i:", UserName],
                                          dl_nickname, NickName,
                                          dl_userid, UID
                                    ]);
process_json_map(Other, _UserName) ->
    {error, {not_match, Other}}.
