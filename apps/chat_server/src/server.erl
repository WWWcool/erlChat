-module(server).

-export([try_to_connect/1]).
-export([disconnect/0]).
-export([get_rooms/0]).
-export([goto_room/1]).
-export([send_message/1]).
-export([get_users_in_room/1]).
-export([new_connect_data/3]).

-export([start_link/0, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).

-behavior(gen_server).

-define(LOBBY, "Lobby").

-record(rm, {name :: string(), msg_buffer = [] :: list()}).
-record(user_init, {name :: string(), module :: module(), send_cb :: atom()}).
-record(user, {name :: string(), room :: string(), pid :: pid(), module :: module(), send_cb :: atom()}).
-record(srv_context, {rooms = [] :: list(), users = [] :: list()}).

-type state() :: #srv_context{}.
-type connect_data() :: #user_init{}.
-type rm_params() :: #rm{}.
-type user_params() :: #user{}.

%% Server API
-spec try_to_connect(connect_data()) -> {ok, connected} | {error, atom()}.
try_to_connect(Params) ->
    %?Print("connect to server with params ~p~n", [Params], #en_trace_level.trace),
    gen_server:call(?MODULE, {try_to_connect, Params}).

-spec disconnect() -> {ok, disconnected} | {error, atom()}.
disconnect() ->
    %?Print("disconnect from server ~n", [], #en_trace_level.trace),
    gen_server:call(?MODULE, disconnect).

-spec get_rooms() -> {ok, list()}.
get_rooms() ->
    gen_server:call(?MODULE, get_rooms).

-spec get_users_in_room(string()) -> {ok, list()}.
get_users_in_room(Room) ->
    gen_server:call(?MODULE, {get_users_in_room, Room}).

-spec goto_room(string()) -> {ok, string()} | {error, atom()}.
goto_room(Room) ->
    %?Print("change room on server to ~p~n", [Room], #en_trace_level.trace),
    gen_server:call(?MODULE, {goto_room, Room}).

-spec send_message(string()) -> {ok, sended} | {error, atom()}.
send_message(Message) ->
    %?Print("send message to server ~p~n", [Message], #en_trace_level.trace),
    gen_server:call(?MODULE, {send_message, Message}).

-spec new_connect_data(string(), module(), atom()) -> connect_data().
new_connect_data(Name, Module, Send_cb) ->
    #user_init{name = Name, module = Module, send_cb = Send_cb}.

%% Exported Client Functions %% Operation & Maintenance API
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() -> gen_server:cast(?MODULE, stop).

%% Callback Functions
-spec init(any()) -> {ok, state()}.
init(_) ->
    {ok, #srv_context{rooms = [?LOBBY, "Room1", "Room2"]}}.

-spec terminate(normal | shutdown | {shutdown, term()} | term(), state()) -> 'ok'.
terminate(_Reason, _State) -> ok.

-spec handle_cast(stop, state()) -> {stop, normal, state()}.
handle_cast(stop, State) -> {stop, normal, State}.

-spec handle_call(  'disconnect' |
                    'get_rooms' |
                    {'get_users_in_room', _} |
                    {'goto_room', string()} |
                    {'send_message', string()} |
                    {'try_to_connect', connect_data()},
                    {_, _}, state()) -> {'reply',{'error' | ok, _}, state()}.
handle_call({try_to_connect, Params}, {From, _}, State) ->
    {Reply, NewState} = chat_connect_user(Params, From, State),
    %?Print("NewState - ~p~n",[NewState], #en_trace_level.trace),
    {reply, Reply, NewState};

handle_call(disconnect, {From, _}, State) ->
    {Reply, NewState} = chat_disconnect_user(From, State),
    {reply, Reply, NewState};

handle_call(get_rooms, {_From, _}, #srv_context{rooms = RoomList} = State) ->
    {reply, {ok, RoomList}, State};

handle_call({goto_room, Room}, {From, _}, State) ->
    {Reply, NewState} = chat_user_change_room(From, Room, State),
    {reply, Reply, NewState};

handle_call({send_message, Message}, {From, _}, State) ->
    {Reply, NewState} = chat_new_message(From, Message, State),
    {reply, Reply, NewState};

handle_call({get_users_in_room, Room}, {_From, _}, #srv_context{users = UserList} = State) ->
    Reply = {ok, lists:filter(fun(#user{room=OldRoom}) ->
                OldRoom == Room end,UserList)},
    {reply, Reply, State}.

%% Utils
-spec chat_connect_user(connect_data(), pid(), state()) -> {{ok, connected}, state()} | {{error, term()}, state()}.
chat_connect_user(#user_init{name = Name} = Params, From, State) ->
    case [User || User <- State#srv_context.users,
            (User#user.name == Name) and (User#user.pid == From)] of
        [] ->
            chat_add_user(Params, From, State);
        _ ->
            {{error, name_or_pid_used}, State}
    end.

-spec chat_disconnect_user(pid(), state()) -> {{ok, disconnected}, state()} | {{error, term()}, state()}.
chat_disconnect_user(From, #srv_context{users = UserList} = State) ->
    case lists:keyfind(From, #user.pid, UserList)of
        false ->
            {{error, user_not_exist}, State};
        User ->
            chat_delete_user(User#user.name, State)
    end.

-spec chat_user_change_room(pid(), string(), state()) -> {{ok, string()}, state()} | {{error, term()}, state()}.
chat_user_change_room(From, Room, #srv_context{users = UserList} = State) ->
    case lists:keyfind(From, #user.pid, UserList) of
        false ->
            {{error, user_not_exist}, State};
        User ->
            NewUser = User#user{room = Room},
            % some handle of res
            room_send_history(NewUser, State),
            NewUserList = lists:keyreplace(NewUser#user.name, #user.name, UserList, NewUser),
            {{ok, Room}, State#srv_context{users = NewUserList}}
    end.

-spec chat_add_user(connect_data(), pid(), state()) -> {{ok, connected}, state()}.
chat_add_user(Params, From, #srv_context{users = UserList} = State) ->
    NewUser = #user{name    = Params#user_init.name,
                    room    = ?LOBBY,
                    pid     = From,
                    module  = Params#user_init.module,
                    send_cb = Params#user_init.send_cb},
    % add monitor
    % some handle of res
    room_send_history(NewUser, State),
    {{ok, connected}, State#srv_context{users = [NewUser | UserList]}}.

-spec chat_delete_user(string(), state()) -> {{ok, disconnected}, state()}.
chat_delete_user(Name, #srv_context{users = UserList} = State) ->
    % delete monitor
    {{ok, disconnected}, State#srv_context{users = lists:keydelete(Name, #user.name, UserList)}}.

-spec chat_new_message(pid(), string(), state()) -> {{ok, sended}, state()} | {{error, term()}, state()}.
chat_new_message(From, Message, #srv_context{users = UserList} = State) ->
    case lists:keyfind(From, #user.pid, UserList) of
        false ->
            {{error, user_not_exist}, State};
        User ->
            room_new_message(User, Message, State)
    end.

-spec room_append_message(rm_params(), string()) -> rm_params().
room_append_message(Room, Message) ->
    NewBuf = case [Message | Room#rm.msg_buffer] of
        List when erlang:length(List) > 10 ->
            lists:droplast(List);
        List -> List
    end,
    Room#rm{msg_buffer = NewBuf}.

-spec room_new_message(user_params(), string(), state()) -> {{ok, sended}, state()}.
room_new_message(User, Message, #srv_context{rooms = RoomList, users = UserList} = State) ->
    UserRoom = User#user.room,
    NewRoomList =
        case lists:keyfind(UserRoom, #rm.name, RoomList) of
            false ->
                RoomList;
            Room ->
                lists:keyreplace(Room#rm.name, #rm.name, RoomList,
                    room_append_message(Room, Message))
        end,
    lists:foreach(
        fun(SomeUser) when SomeUser#user.room == UserRoom ->
                send_back_to_user(SomeUser,Message)
        end,
        UserList),
    {{ok, sended}, State#srv_context{rooms = NewRoomList}}.

-spec room_send_history(user_params(), state()) -> {ok, sended} | {error, term()}.
room_send_history(User, #srv_context{rooms = RoomList}) ->
    UserRoom = User#user.room,
    case lists:keyfind(UserRoom, #rm.name, RoomList) of
        false ->
            {error, room_not_exist};
        Room ->
            ResendFunc = fun(Message) -> send_back_to_user(User, Message) end,
            lists:foreach(ResendFunc, Room#rm.msg_buffer),
            {ok, sended}
    end.

-spec send_back_to_user(user_params(), string()) -> {ok, sended} | {error, term()}.
send_back_to_user(#user{module=Module,send_cb=Callback} = _User, Message) ->
    try Module:Callback(Message) of
        _ -> {ok, sended}
    catch
        _:Replay ->
            %?Print("User - ~p send callback error - ~p~n",[User, Replay], #en_trace_level.debug),
            {error,Replay}
    end.



