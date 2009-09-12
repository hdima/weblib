%%
%% @doc Test for HTTP client
%%
-module(test_http_client).

-export([test/0]).

% Behaviour callbacks
-export([prepare_request/2, handle_headers/3, handle_data/2, end_request/2]).

-behaviour(http_client).


%%
%% Behaviour callbacks
%%

prepare_request(Url, [Pid]) ->
    Pid ! {prepare_request, Url},
    {ok, Url, [], Pid}.

handle_headers(Status, Headers, Pid) ->
    Pid ! {handle_headers, Status, Headers},
    Pid.

handle_data(Chunk, Pid) ->
    Pid ! {handle_data, Chunk},
    Pid.

end_request(Info, Pid) ->
    Pid ! {end_request, Info},
    ok.


%%
%% Test methods
%%

start_test_server(Pid) ->
    {ok, Listen} = gen_tcp:listen(0,
        [binary, {ip, {127, 0, 0, 1}}, {active, false}, {packet, http_bin}]),
    {ok, Port} = inet:port(Listen),
    spawn(fun () -> start_test_client(Port, Pid) end),
    {ok, Socket} = gen_tcp:accept(Listen, 3 * 1000),
    {ok, {http_request, 'GET', {abs_path, <<"/">>}, {1, 0}}}
        = gen_tcp:recv(Socket, 0, 3 * 1000),
    {ok, {http_header, _, 'Host', _, <<"localhost">>}}
        = gen_tcp:recv(Socket, 0, 3 * 1000),
    {ok, http_eoh} = gen_tcp:recv(Socket, 0, 3 * 1000),
    ok = inet:setopts(Socket, [{packet, raw}]),
    ok = gen_tcp:send(Socket,
        <<"HTTP/1.0 200 OK\r\nContent-Length: 2\r\n\r\nOK">>),
    ok = gen_tcp:close(Socket),
    ok = gen_tcp:close(Listen).


start_test_client(Port, Pid) ->
    http_client:http_connect("http://localhost:" ++ integer_to_list(Port),
        test_http_client, [Pid]).


test_http_client([4, 3, 2, 1]) ->
    ok;
test_http_client(Result) ->
    receive
        {prepare_request, {http, "localhost", _Port, "/"}} ->
            test_http_client([1 | Result]);
        {handle_headers, {{1, 0}, 200, <<"OK">>},
                [{'Content-Length', <<"2">>}]} ->
            test_http_client([2 | Result]);
        {handle_data, <<"OK">>} ->
            test_http_client([3 | Result]);
        {end_request, ok} ->
            test_http_client([4 | Result])
    end.


test() ->
    Pid = self(),
    spawn(fun () -> start_test_server(Pid) end),
    ok = test_http_client([]),
    ok.
