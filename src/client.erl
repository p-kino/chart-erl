-module(client).
-expoer().

login(Name, Pid) ->
    case Pid ! {login, self(), Name} of
        {ok, ChatHistory} ->
            lists:foreach(fun({N, C}) -> io:format("~s> ~s~n", [N, C]) end,
                          ChatHistory),
            % クライアントのプロセスをspawnして返す

        used_name ->
            io:format("User name ~"~s~" is already used.", [Name]).

