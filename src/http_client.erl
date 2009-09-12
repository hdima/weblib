%%
%% @doc HTTP client
%%
%% @author Dmitry Vasiliev <dima@hlabs.spb.ru>
%% @version 0.1
%%
-module(http_client).
-vsn(0.1).

%% Public interface
-export([http_connect/1]).

%% Behaviour information
-export([behaviour_info/1]).

-define(TIMEOUT, 30 * 1000).


%%
%% @doc Behaviour callbacks
%%
behaviour_info(callbacks) ->
    [{init, 1}, {prepare_request, 1}, {handle_headers, 1},
        {handle_body_chunk, 2}];
behaviour_info(_Other) ->
    undefined.


%%
%% @doc Start HTTP client with callback module
%% @spec start(Module, Args) -> ok
%%      Module = atom()
%%      Args = list()
%%
%% TODO: Options?
%%
start(Module, Args) ->
    ok.


http_connect(Host) ->
    {ok, Sock} = gen_tcp:connect(Host, 80,
        [binary, inet, {packet, http_bin}, {send_timeout, ?TIMEOUT},
        {send_timeout_close, true}, {active, false}], ?TIMEOUT),
    Request = get_http_request(<<"GET">>, <<"/">>, <<"1.1">>,
        [{<<"HOST">>, list_to_binary(Host)}]),
    ok = gen_tcp:send(Sock, Request),
    R = recv(Sock),
    gen_tcp:close(Sock),
    R.


get_http_request(Method, Path, Version, Headers) ->
    Header = <<Method/binary," ",Path/binary," HTTP/",Version/binary,"\r\n">>,
    HData = get_http_headers(Headers, <<>>),
    <<Header/binary,HData/binary,"\r\n">>.

get_http_headers([], Data) ->
    Data;
get_http_headers([{Key, Value} | Headers], Data) ->
    get_http_headers(Headers,
        <<Data/binary,Key/binary,": ",Value/binary,"\r\n">>).


recv(Sock) ->
    {Status, Headers, Size} = recv_headers(Sock, none, [], 0),
    inet:setopts(Sock, [{packet, raw}]),
    {Status, Headers, recv_data(Sock, Size, <<>>)}.

recv_headers(Sock, HTTPHeader, Headers, Size) ->
    case gen_tcp:recv(Sock, 0, ?TIMEOUT) of
        {ok, {http_response, Version, Status, Comment}} ->
            recv_headers(Sock, {Version, Status, Comment}, Headers, Size);
        {ok, {http_header, _, 'Content-Length', _, Value}} ->
            recv_headers(Sock, HTTPHeader,
                [{'Content-Length', Value} | Headers],
                list_to_integer(binary_to_list(Value)));
        {ok, {http_header, _, Name, _, Value}} ->
            recv_headers(Sock, HTTPHeader, [{Name, Value} | Headers], Size);
        {ok, http_eoh} ->
            {HTTPHeader, lists:reverse(Headers), Size}
    end.

recv_data(_, 0, Data) ->
    Data;
recv_data(Sock, Size, Data) ->
    case gen_tcp:recv(Sock, 0, 20 * 1000) of
        {ok, Batch} ->
            recv_data(Sock, Size - size(Batch), <<Data/binary,Batch/binary>>)
    end.
