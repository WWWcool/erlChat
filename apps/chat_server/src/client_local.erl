-module(client_local).

-export([send_cb/1]).
-export([connect/2]).
-export([disconnect/0]).
-export([select_room/1]).
-export([send_message/1]).

-export([start_link/0, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).

-behavior(gen_server).

-define(EMPTY, "Empty").

-record(cl_context, {server = undefined, name, state, msgBuffer = []}).
-record(en_cl_states, {offline, online}).
-record(user_init, {name, module, send_cb}).

-type connect_params() :: server:connect_params().

%% Exported Client Functions %% Operation & Maintenance API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:cast(?MODULE, stop).

%% Callback Functions
init(_) ->
    {ok, init_client()}.

terminate(_Reason, _State) -> ok.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_, #cl_context{state = #en_cl_states.offline} = State) ->
    {noreply, State};
handle_cast({send_cb, Message}, State) ->
    io:format("Receive message - ~p~n",[Message]),
    {noreply, message_append(State, Message)}.

handle_call({connect, {Server, Name}}, {_From, _}, State) ->
    try Server:try_to_connect(#user_init{   name = Name,
                                            module = ?MODULE,
                                            send_cb = send_cb}) of
        {error, Replay} ->
            error_handler({server_error, Replay}, State);
        _ ->
            ServerReplay = Server:get_rooms(),
            %?Print("ServerReplay = ~p~n",[ServerReplay], #en_trace_level.debug),
            case ServerReplay of
            %case Server:get_rooms() of
                {error, Replay} -> error_handler({server_error, Replay}, State);
                _ ->
                    NewState = State#cl_context{  name = Name,
                                                        server = Server,
                                                        state = #en_cl_states.online},
                    {reply, ok, NewState}
            end
    catch
        error:Error -> error_handler({error, Error}, State)
    end;

handle_call(_, _, #cl_context{state = #en_cl_states.offline} = State) ->
    {reply, {error, not_connected}, State};

handle_call(disconnect, {_From, _}, State) ->
    try (State#cl_context.server):disconnect() of
        {error, Replay} -> error_handler({server_error, Replay}, State);
        _ -> {reply, ok, init_client()}
    catch
        error:Error -> error_handler({error, Error}, State)
    end;

handle_call({select_room, Room}, {_From, _}, State) ->
    try (State#cl_context.server):goto_room(Room) of
        {error, Replay} -> error_handler({server_error, Replay}, State);
        _ -> {reply, ok, State}
    catch
        error:Error -> error_handler({error, Error}, State)
    end;

handle_call({send_message, Message}, {_From, _}, State) ->
    try (State#cl_context.server):send_message(Message) of
        {error, Replay} -> error_handler({server_error, Replay}, State);
        _ -> {reply, ok, State}
    catch
        error:Error -> error_handler({error, Error}, State)
    end.

%% Client API
send_cb(Message) ->
    gen_server:cast(?MODULE, {send_cb, Message}).

connect(Server, Name) ->
    gen_server:call(?MODULE, {connect, {Server, Name}}).

disconnect() ->
    gen_server:call(?MODULE, disconnect).

select_room(Room) ->
    gen_server:call(?MODULE, {select_room, Room}).

send_message(Message) ->
    gen_server:call(?MODULE, {send_message, Message}).

%% Utils
init_client() -> #cl_context{name = ?EMPTY, state = #en_cl_states.offline}.

message_append(State, Message) ->
    NewList = case [Message | State#cl_context.msgBuffer] of
        List when length(List) > 10 ->
            lists:droplast(List);
        List -> List
    end,
    State#cl_context{msgBuffer = NewList}.

error_handler(Error, State) ->
    %?Print("Error - ~p~n",[Error], #en_trace_level.debug),
    {NewReplay, NewState} = case Error of
        {server_error, _} -> {Error, State};
        {error, Error} -> {{error, Error}, init_client()}
    end,
    {reply, NewReplay, NewState}.




