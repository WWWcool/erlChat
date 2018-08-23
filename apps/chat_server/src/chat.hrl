%%% File : chat.hrl
%%% Description : Include file for chat server

-record(user_init, {name, module, send_cb}).
-record(user, {name, room, pid = undefined, module = undefined, send_cb = undefined}).
-record(rm, {name, msgBuffer = []}).
-record(msg, {type, from, time, content}).

-record(cl_context, {server = undefined, name, state, msgBuffer = []}).
-record(en_cl_states, {offline, online}).

%-define(debug, 1).

-ifdef(debug).
    -define(Print(Message,Args),io:format(Message,Args)).
-else.
    -define(Print(Message,Args),ok).
-endif.
