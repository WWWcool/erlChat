-module (basic_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([basic_tests/1]).

all() -> [basic_tests].

init_per_testcase(basic_tests, Config) ->
    Config.

end_per_testcase(basic_tests, _Config) ->
    ok.

basic_tests(_Config) ->
    {ok, _Pid} = chat_server_sup:start_link(),
    {ok, ["Lobby", "Room1", "Room2"]} = server:get_rooms(),
    {ok, []} = server:get_users_in_room("Lobby"),
    {ok, connected} = client_local:connect("Test"),
    {error, name_or_pid_used} = client_local:connect("Test"),
    {ok, disconnected} = client_local:disconnect(),
    {error, not_connected} = client_local:get_rooms(),
    {ok, connected} = client_local:connect("Test"),
    {ok, ["Lobby", "Room1", "Room2"]} = client_local:get_rooms(),
    {ok, connected} = client_local:connect("Test2"),
    {ok, [_, _]} = server:get_users_in_room("Lobby"),
    {ok, sended} = client_local:send_message("Some test..."),
    ok.
