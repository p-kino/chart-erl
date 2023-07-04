-module(client).
-expoer().

login(Name, Pid) ->
    ClientPid = spawn(?MODULE, init_loop, [Name]),


    case Pid ! {login, self(), Name} of
        {ok, ChatHistory} ->
            lists:foreach(fun({N, C}) -> io:format("~s> ~s~n", [N, C]) end,
                          lists:reverse(ChatHistory)),
            loop();

        used_name ->
            io:format("User name \"~s\" is already used.", [Name])
    end.

init_loop() ->
    Pid = whereis(chat_server),
    monitor(process, Pid),
    loop().

loop() ->
    receive
        logout ->
            Pid = whereis(chat_server),
            Pid ! {logout, self()},
            exit(normal);
        {'DOWN', _, _, _, _} ->
            io:format("Server downed."),
            exit(normal);
        Unknown ->
            io:format("Unknown message: ~p~n", [Unknown]),
            loop()
    end.

