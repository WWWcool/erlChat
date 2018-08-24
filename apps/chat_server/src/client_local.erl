-module(client_local).

-export([send_cb/1]).
-export([connect/2]).
-export([disconnect/0]).
-export([select_room/1]).
-export([send_message/1]).

-export([start_link/0, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).

-behavior(gen_server).
-include("chat.hrl").

-define(EMPTY, "Empty").

%% Exported Client Functions %% Operation & Maintenance API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:cast(?MODULE, stop).

%% Callback Functions
init(_) ->
    {ok, init_client()}.

terminate(_Reason, _LoopData) -> ok.

handle_cast(stop, LoopData) -> {stop, normal, LoopData};
handle_cast(_, #cl_context{state = #en_cl_states.offline} = LoopData) ->
    {noreply, LoopData};
handle_cast({send_cb, Message}, LoopData) ->
    io:format("Receive message - ~p~n",[Message]),
    {noreply, message_append(LoopData, Message)}.

handle_call({connect, {Server, Name}}, {_From, _}, LoopData) ->
    try Server:try_to_connect(#user_init{   name = Name,
                                            module = ?MODULE,
                                            send_cb = send_cb}) of
        {error, Replay} ->
            error_handler({server_error, Replay}, LoopData);
        _ ->
            ServerReplay = Server:get_rooms(),
            ?Print("ServerReplay = ~p~n",[ServerReplay], #en_trace_level.debug),
            case ServerReplay of
            %case Server:get_rooms() of
                {error, Replay} -> error_handler({server_error, Replay}, LoopData);
                _ ->
                    NewLoopData = LoopData#cl_context{  name = Name,
                                                        server = Server,
                                                        state = #en_cl_states.online},
                    {reply, ok, NewLoopData}
            end
    catch
        error:Error -> error_handler({error, Error}, LoopData)
    end;

handle_call(_, _, #cl_context{state = #en_cl_states.offline} = LoopData) ->
    {reply, {error, not_connected}, LoopData};

handle_call(disconnect, {_From, _}, LoopData) ->
    try (LoopData#cl_context.server):disconnect() of
        {error, Replay} -> error_handler({server_error, Replay}, LoopData);
        _ -> {reply, ok, init_client()}
    catch
        error:Error -> error_handler({error, Error}, LoopData)
    end;

handle_call({select_room, Room}, {_From, _}, LoopData) ->
    try (LoopData#cl_context.server):goto_room(Room) of
        {error, Replay} -> error_handler({server_error, Replay}, LoopData);
        _ -> {reply, ok, LoopData}
    catch
        error:Error -> error_handler({error, Error}, LoopData)
    end;

handle_call({send_message, Message}, {_From, _}, LoopData) ->
    try (LoopData#cl_context.server):send_message(Message) of
        {error, Replay} -> error_handler({server_error, Replay}, LoopData);
        _ -> {reply, ok, LoopData}
    catch
        error:Error -> error_handler({error, Error}, LoopData)
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

message_append(LoopData, Message) ->
    NewList = case [Message | LoopData#cl_context.msgBuffer] of
        List when length(List) > 10 ->
            lists:droplast(List);
        List -> List
    end,
    LoopData#cl_context{msgBuffer = NewList}.

error_handler(Error, LoopData) ->
    ?Print("Error - ~p~n",[Error], #en_trace_level.debug),
    {NewReplay, NewLoopData} = case Error of
        {server_error, _} -> {Error, LoopData};
        {error, Error} -> {{error, Error}, init_client()}
    end,
    {reply, NewReplay, NewLoopData}.




