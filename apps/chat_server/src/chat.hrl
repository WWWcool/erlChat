%%% File : chat.hrl
%%% Description : Include file for chat server

-record(user_init, {name, module, send_cb}).
-record(user, {name, room, pid = undefined, module = undefined, send_cb = undefined}).
-record(rm, {name, msgBuffer = []}).
-record(msg, {type, from, time, content}).

-record(cl_context, {server = undefined, name, state, msgBuffer = []}).

%% Enums
-record(en_cl_states, {offline, online}).
-record(en_trace_level, {error, warning, debug, trace}).


-define(DEBUG, #en_trace_level.debug).

-ifdef(DEBUG).
    -define(Print(Message, Args, Level),
        case Level =< ?debug of
            true -> io:format(Message, Args);
            _ -> ok
        end).
-else.
    -define(Print(Message, Args, Level),ok).
-endif.
