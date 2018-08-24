-module(server).

-export([try_to_connect/1]).
-export([disconnect/0]).
-export([get_rooms/0]).
-export([goto_room/1]).
-export([send_message/1]).
-export([get_users_in_room/1]).

-export([start_link/0, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).

-behavior(gen_server).
-include("chat.hrl").

-define(LOBBY, "Lobby").

%% Exported Client Functions %% Operation & Maintenance API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:cast(?MODULE, stop).

%% Callback Functions
init(_) ->
    {ok, [  {rooms, [?LOBBY, "Room1", "Room2"]},
            {users, [#user{name = "TestUser", room = ?LOBBY}]}]}.

terminate(_Reason, _LoopData) -> ok.

handle_cast(stop, LoopData) -> {stop, normal, LoopData}.

handle_call({try_to_connect, Params}, {From, _}, LoopData) ->
    {Reply, NewLoopData} = chat_connect_user(LoopData, Params, From),
    ?Print("NewLoopData - ~p~n",[NewLoopData], #en_trace_level.trace),
    {reply, Reply, NewLoopData};

handle_call(disconnect, {From, _}, LoopData) ->
    {Reply, NewLoopData} = chat_disconnect_user(LoopData, From),
    {reply, Reply, NewLoopData};

handle_call(get_rooms, {_From, _}, LoopData) ->
    Reply = case lists:keyfind(rooms, 1, LoopData) of
        {rooms, RoomList} ->
            {ok, RoomList};
        _ ->
            {error, rooms_not_exist}
    end,
    {reply, Reply, LoopData};

handle_call({goto_room, Room}, {From, _}, LoopData) ->
    {Reply, NewLoopData} = chat_user_change_room(LoopData, From, Room),
    {reply, Reply, NewLoopData};

handle_call({send_message, Message}, {From, _}, LoopData) ->
    {Reply, NewLoopData} = chat_new_message(LoopData, From, Message),
    {reply, Reply, NewLoopData};

handle_call({get_users_in_room, Room}, {_From, _}, LoopData) ->
    Reply = case lists:keyfind(users, 1, LoopData) of
        {users, UserList} ->
            {ok, lists:filter(fun(#user{room=OldRoom}) ->
                OldRoom == Room end,UserList)};
        _ ->
            {error, users_not_exist}
    end,
    {reply, Reply, LoopData}.

%% Server API

try_to_connect(Params) ->
    ?Print("connect to server with params ~p~n", [Params], #en_trace_level.trace),
    gen_server:call(?MODULE, {try_to_connect, Params}).

disconnect() ->
    ?Print("disconnect from server ~n", [], #en_trace_level.trace),
    gen_server:call(?MODULE, disconnect).

get_rooms() ->
    ?Print("get rooms from server ~n", [], #en_trace_level.trace),
    case gen_server:call(?MODULE, get_rooms) of
        {ok, List} -> List;
        _ -> {error, no_room}
    end.

get_users_in_room(Room) ->
    ?Print("get users from room ~p on server~n", [Room], #en_trace_level.trace),
    case gen_server:call(?MODULE, {get_users_in_room, Room}) of
        {ok, List} -> List;
        _ -> {error, badarg}
    end.

goto_room(Room) ->
    ?Print("change room on server to ~p~n", [Room], #en_trace_level.trace),
    gen_server:call(?MODULE, {goto_room, Room}).

send_message(Message) ->
    ?Print("send message to server ~p~n", [Message], #en_trace_level.trace),
    gen_server:call(?MODULE, {send_message, Message}).

%% Utils

chat_check_user([], _, _) -> false;
chat_check_user([{users, UserList} | _], Val, N) ->
    %?Print("UserList - ~p Val - ~p N - ~p~n",[UserList, Val, N]),
    %User = lists:nth(1, UserList),
    %?Print("User - ~p N - ~p Element - ~p~n",[User, N, erlang:element(N,User)]),
    lists:keyfind(Val, N, UserList);
chat_check_user([_ | Rest], Val, N) ->
    chat_check_user(Rest, Val, N).

chat_connect_user(LoopData, Params, From) ->
    SameName = chat_check_user(LoopData, Params#user_init.name, #user.name),
    %?Print("SameName - ~p~n", [SameName]),
    SamePid = chat_check_user(LoopData, From, #user.pid),
    %?Print("SamePid - ~p~n", [SamePid]),
    case erlang:is_boolean(SameName) and erlang:is_boolean(SamePid) of
        true ->
            chat_add_user(LoopData, Params, From);
        _ ->
            {{error, name_or_pid_used}, LoopData}
    end.

chat_disconnect_user(LoopData, From) ->
    case chat_check_user(LoopData, From, #user.pid) of
        false ->
            {{error, user_not_exist}, LoopData};
        User ->
            chat_delete_user(LoopData, User#user.name)
    end.

chat_user_change_room(LoopData, From, Room) ->
    case lists:keyfind(users,1,LoopData) of
        {users, UserList} ->
            case chat_check_user(LoopData, From, #user.pid) of
                false ->
                    {{error, user_not_exist}, LoopData};
                User ->
                    NewUser = User#user{room = Room},
                    room_send_history(LoopData, NewUser),
                    NewUserList = lists:keyreplace(NewUser#user.name, #user.name, UserList, NewUser),
                    {ok, lists:keyreplace(users,1,LoopData, {users, NewUserList})}
            end;
        _ ->
            {{error, users_not_found}, LoopData}
    end.

chat_add_user(LoopData, Params, From) ->
    case lists:keyfind(users,1,LoopData) of
        {users, UserList} ->
            NewUser = #user{name    = Params#user_init.name,
                            room    = ?LOBBY,
                            pid     = From,
                            module  = Params#user_init.module,
                            send_cb = Params#user_init.send_cb},
            room_send_history(LoopData, NewUser),
            {ok, lists:keyreplace(users,1,LoopData,
                                 {users, [NewUser | UserList]})};
        _ ->
            {{error, users_not_found}, LoopData}
    end.

chat_delete_user(LoopData, Name) ->
    case lists:keyfind(users,1,LoopData) of
        {users, UserList} ->
            {ok, lists:keyreplace(users,1,LoopData,
                                 {users, lists:keydelete(Name, #user.name, UserList)})};
        _ ->
            {{error, users_not_found}, LoopData}
    end.

chat_update_room(RoomList, Name, Val, N) ->
    case lists:keyfind(Name, #rm.name, RoomList) of
        false ->
            RoomList;
        Room ->
            NewRoom = erlang:setelement(N, Room, Val),
            lists:keyreplace(Name, #rm.name, RoomList, NewRoom)
    end.

chat_new_message(LoopData, From, Message) ->
    case chat_check_user(LoopData, From, #user.pid) of
        false ->
            {{error, user_not_exist}, LoopData};
        User ->
            room_new_message(LoopData, User, Message)
    end.

room_append_message(Room, Message) ->
    case [Message | Room#rm.msgBuffer] of
        List when erlang:length(List) > 10 ->
            lists:droplast(List);
        List -> List
    end.

room_new_message(LoopData, User, Message) ->
    case lists:keyfind(rooms,1,LoopData) of
        {rooms, RoomList} ->
            UserRoom = User#user.room,
            NewRoomList =
                case lists:keyfind(UserRoom, #rm.name, RoomList) of
                    false ->
                        RoomList;
                    Room ->
                        chat_update_room(RoomList, UserRoom,
                            room_append_message(Room, Message), #rm.msgBuffer)
                end,
            case lists:keyfind(users,1,LoopData) of
                {users, UserList} ->

                    lists:foreach(
                        fun(SomeUser) when SomeUser#user.room == UserRoom ->
                                send_back_to_user(SomeUser,Message)
                        end,
                        UserList);
                _ -> ok
            end,
            {ok, lists:keyreplace(rooms,1,LoopData,
                                 {rooms, NewRoomList})};
        _ ->
            {{error, rooms_not_found}, LoopData}
    end.

room_send_history(LoopData, User) ->
    UserRoom = User#user.room,
    case lists:keyfind(rooms,1,LoopData) of
        {rooms, RoomList} ->
            case lists:keyfind(UserRoom, #rm.name, RoomList) of
                false ->
                    RoomList;
                Room ->
                    lists:foreach(
                        fun(Message) ->
                            send_back_to_user(User, Message)
                        end,
                    Room#rm.msgBuffer)
            end;
        _ -> ok
    end.

send_back_to_user(#user{module=Module,send_cb=Callback} = _User, Message) ->
    try Module:Callback(Message) of
        _ -> ok
    catch
        _:_ -> ?Print("User - ~p send callback error~n",[_User], #en_trace_level.debug)
    end.



