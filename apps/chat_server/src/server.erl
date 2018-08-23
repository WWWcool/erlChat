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
        %io:format("NewLoopData - ~p~n",[NewLoopData]),
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
    gen_server:call(?MODULE, {try_to_connect, Params}).

disconnect() ->
    gen_server:call(?MODULE, disconnect).

get_rooms() ->
    case gen_server:call(?MODULE, get_rooms) of
        {ok, List} -> List;
        _ -> {error, no_room}
    end.

get_users_in_room(Room) ->
    case gen_server:call(?MODULE, {get_users_in_room, Room}) of
        {ok, List} -> List;
        _ -> {error, badarg}
    end.

goto_room(Room) ->
    gen_server:call(?MODULE, {goto_room, Room}).

send_message(Message) ->
    gen_server:call(?MODULE, {send_message, Message}).

%% Utils

chat_check_user([], _, _) -> false;
chat_check_user([{users, UserList} | _], Val, N) ->
    %io:format("UserList - ~p Val - ~p N - ~p~n",[UserList, Val, N]),
    %User = lists:nth(1, UserList),
    %io:format("User - ~p N - ~p Element - ~p~n",[User, N, erlang:element(N,User)]),
    lists:keyfind(Val, N, UserList);
chat_check_user([_ | Rest], Val, N) ->
    chat_check_user(Rest, Val, N).

chat_connect_user(LoopData, Params, From) ->
    case chat_check_user(LoopData, Params#user_init.name, #user.name) of
        false ->
            chat_add_user(LoopData, Params, From);
        _ ->
            {{error, name_already_used}, LoopData}
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
                    {ok, lists:keyreplace(users,1,LoopData,
                                         {users, lists:keyreplace(NewUser#user.name, #user.name, UserList, NewUser)})}
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

%chat_update_user(UserList, Name, Val, N) ->
%    case lists:keyfind(Name, #user.name, UserList) of
%        false ->
%            UserList;
%        User ->
%            NewUser = erlang:setelement(N, User, Val),
%            chat_update_user(UserList, NewUser)
%    end.
%
%chat_update_user(UserList, NewUser) ->
%            lists:keyreplace(NewUser#user.name, #user.name, UserList, NewUser).

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

room_new_message(LoopData, User, Message) ->
    case lists:keyfind(rooms,1,LoopData) of
        {rooms, RoomList} ->
            UserRoom = User#user.room,
            NewRoomList =
                case lists:keyfind(UserRoom, #rm.name, RoomList) of
                    false ->
                        RoomList;
                    Room ->
                        NewBuffer = [Message | Room#rm.msgBuffer],
                        case erlang:length(NewBuffer) of
                            Len when Len > 10 ->
                                chat_update_room(RoomList, UserRoom,
                                                 lists:droplast(NewBuffer), #rm.msgBuffer);
                            _ ->
                                chat_update_room(RoomList, UserRoom, NewBuffer, #rm.msgBuffer)
                        end
                end,
            case lists:keyfind(users,1,LoopData) of
                {users, UserList} ->
                    lists:foreach(
                        fun(#user{module=Module,send_cb=Callback} = X)
                            when X#user.room == UserRoom ->
                                case {Module,Callback} of
                                    {undefined, _} -> ok;
                                    {_, undefined} -> ok;
                                    _ ->
                                        Module:Callback(Message)
                                end
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
                        #user{module=Module,send_cb=Callback} = User,
                        lists:foreach(
                            fun(Message) ->
                                    Module:Callback(Message)
                            end,
                        Room#rm.msgBuffer)
            end;
        _ -> ok
    end.





