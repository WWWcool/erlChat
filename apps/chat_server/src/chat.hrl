%%% File : chat.hrl
%%% Description : Include file for chat server
-record(user_init, {name, module, send_cb}).
-record(user, {name, room, pid = undefined, module = undefined, send_cb = undefined}).
-record(rm, {name, msgBuffer = []}).
-record(msg, {type, from, time, content}).

%int()
%term()
%atom(), enabled | disabled %atom(), prepay | postpay %[atom()], service flag list
