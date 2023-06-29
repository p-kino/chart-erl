-module(server).
-export().

-record(state, {users=orddict:new(),
                chat_log=[]}).

-record(message, {user_name,
                  content}).
                

server(S) ->
    receive
        {login, Pid, Name} ->
            case orddict:is_key(Pid, S#state.users) of
                false ->
                    %% clientのプロセスをspawn_monitorしてPidを返す
                    %% clientのPidにchat_logを送る
                    Notification = {admin, Name + "が入室しました"},
                    broadcast(Notification, S#state.users),
                    server({
                        orddict:append(Name, Pid, S#state.users),
                        [Notification | S#state.chat_log]
                    });
                true ->
                    %% 名前が使用中であることを通知する
                    server(S)
            end;
        {logout, Pid, Name} ->
            case orddict:find(Name, S#state.users) of
                {ok, Pid} ->
                    NewUsers = orddict:erase(Name, S#state.users),
                    %% clientのプロセスを殺す
                    Notification = {admin, Name + "が退室しました"},
                    broadcast(Notification, NewUsers),
                    server({
                        NewUsers,
                        [Notification | S#state.chat_log]
                    })
            end
    end.

broadcast(Message, Users) ->
    lists:foreach(
      fun({_, Pid}) -> Pid ! {broadcast, Message} end,
      orddict:to_list(Users)
    ).

