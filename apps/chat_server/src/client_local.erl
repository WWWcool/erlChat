-module(client_local).

-export([send_cb/1]).
-export([connect/1]).
-export([disconnect/0]).
-export([select_room/1]).
-export([send_message/1]).
-export([get_rooms/0]).

-export([start_link/0, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).

-behavior(gen_server).

-record(cl_context, {name :: string() | undefined, state :: cl_states(), msg_buffer = [] :: list()}).

-type cl_states() :: online | offline.
-type state() :: #cl_context{}.

%% Client API
-spec send_cb(string()) -> no_return().
send_cb(Message) ->
    gen_server:cast(?MODULE, {send_cb, Message}).

-spec connect(string()) -> {ok, connected} | {error, _}.
connect(Name) ->
    gen_server:call(?MODULE, {connect, Name}).

-spec disconnect() -> {ok, disconnected} | {error, _}.
disconnect() ->
    gen_server:call(?MODULE, disconnect).

-spec get_rooms() -> {ok, list()} | {error, _}.
get_rooms() ->
    gen_server:call(?MODULE, get_rooms).

-spec select_room(string()) -> {ok, string()} | {error, _}.
select_room(Room) ->
    gen_server:call(?MODULE, {select_room, Room}).

-spec send_message(string()) -> {ok, sended} | {error, _}.
send_message(Message) ->
    gen_server:call(?MODULE, {send_message, Message}).

%% Exported Client Functions %% Operation & Maintenance API
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() -> gen_server:cast(?MODULE, stop).

%% Callback Functions
-spec init(any()) -> {ok, state()}.
init(_) ->
    {ok, init_client()}.

-spec terminate(normal | shutdown | {shutdown, term()} | term(), state()) -> 'ok'.
terminate(_Reason, _State) -> ok.

-spec handle_cast(stop, state()) -> {stop, normal, state()} | {noreply, _}.
handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_, #cl_context{state = offline} = State) ->
    {noreply, State};
handle_cast({send_cb, Message}, State) ->
    io:format("Receive message - ~p~n",[Message]),
    {noreply, message_append(State, Message)}.

-spec handle_call(  {'connect', string()} |
                    'disconnect' |
                    'get_rooms' |
                    {'select_room', string()} |
                    {'send_message', string()},
                    {_, _}, state()) -> {'reply',{'error' | ok, _}, state()}.
handle_call({connect, Name}, {_From, _}, State) ->
    try server:try_to_connect(server:new_connect_data(Name, ?MODULE, send_cb)) of
        {error, Replay} ->
            error_handler({server_error, Replay}, State);
        _ ->
            %add monitor
            NewState = State#cl_context{    name = Name,
                                            state = online},
            {reply, {ok, connected}, NewState}
    catch
        error:Error -> error_handler(Error, State)
    end;

handle_call(_, _, #cl_context{state = offline} = State) ->
    {reply, {error, not_connected}, State};

handle_call(disconnect, {_From, _}, State) ->
    try server:disconnect() of
        {error, Replay} -> error_handler({server_error, Replay}, State);
        _ -> {reply, {ok, disconnected}, init_client()}
    catch
        error:Error -> error_handler(Error, State)
    end;

handle_call(get_rooms, {_From, _}, State) ->
    try server:get_rooms() of
        {ok, List} -> {reply, {ok, List}, State}
    catch
        error:Error -> error_handler(Error, State)
    end;

handle_call({select_room, Room}, {_From, _}, State) ->
    try server:goto_room(Room) of
        {error, Replay} -> error_handler({server_error, Replay}, State);
        _ -> {reply, {ok, Room}, State}
    catch
        error:Error -> error_handler(Error, State)
    end;

handle_call({send_message, Message}, {_From, _}, State) ->
    try server:send_message(Message) of
        {error, Replay} -> error_handler({server_error, Replay}, State);
        _ -> {reply, {ok, sended}, State}
    catch
        error:Error -> error_handler(Error, State)
    end.

%% Utils
-spec init_client() -> state().
init_client() -> #cl_context{name = undefined, state = offline}.

-spec message_append(state(), string()) -> state().
message_append(State, Message) ->
    NewList = case [Message | State#cl_context.msg_buffer] of
        List when length(List) > 10 ->
            lists:droplast(List);
        List -> List
    end,
    State#cl_context{msg_buffer = NewList}.

-spec error_handler(_, state()) -> {replay, {error | server_error, _}, state()}.
error_handler({server_error, Error}, State) -> {replay, {error, Error}, State};
error_handler(Error, _) -> {reply, Error, init_client()}.




