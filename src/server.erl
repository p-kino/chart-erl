-module(server).
-export().

-record(state, {users=orddict:new(),
                chat_history=[]}).

-record(message, {user_name,
                  content}).
                

server(S) ->
    receive
        {login, Pid, Name} ->
            case is_valid_new_user(Pid, Name, S) of
                false ->
                    monitor(process, Pid),
                    Pid ! {login, ok, S#state.chat_history},
                    Notification = {admin, Name + "が入室しました"},
                    broadcast(Notification, S#state.users),
                    server({
                        orddict:append(Pid, Name, S#state.users),
                        [Notification | S#state.chat_history]
                    });
                true ->
                    %% 名前が使用中であることを通知する
                    server(S)
            end;
        {logout, Pid} ->
            case orddict:find(Pid, S#state.users) of
                {ok, Name} ->
                    NewUsers = orddict:erase(Pid, S#state.users),
                    exit(Pid, logout),
                    Notification = {admin, Name + "が退室しました"},
                    broadcast(Notification, NewUsers),

                    server({
                        NewUsers,
                        [Notification | S#state.chat_history]
                    })
            end;
        {'DOWN', _, _, Pid, _} ->
            case orddict:find(Pid, S#state.users) of
                {ok, Name} ->
                    NewUsers = orddict:erase(Pid, S#state.users),
                    Notification = {admin, Name + "が退室しました"},
                    broadcast(Notification, NewUsers),

                    server({
                        NewUsers,
                        [Notification | S#state.chat_history]
                    })
            end;
        Unknown ->
            io:format("Unknown message: ~p~n", [Unknown]),
            server(S)
        end.

is_valid_new_user(Pid, Name, S) ->
    not orddict:is_key(Pid, S#state.users) andalso
        orddict:is_empty(orddict:filter(fun (_, V) -> V == Name end, S#state.users)).

broadcast(Message, Users) ->
    lists:foreach(
      fun({Pid, _}) -> Pid ! {broadcast, Message} end,
      orddict:to_list(Users)
    ).

