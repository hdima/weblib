%%
%% @doc HTTP client
%%
%% @author Dmitry Vasiliev <dima@hlabs.spb.ru>
%% @version 0.1
%%
%% Callback module interface:
%%
%%      prepare_request(UrlParts, Args) -> Result
%%          UrlParts = {Scheme, Host, Port, Path}
%%          Scheme = http
%%          Host = binary()
%%          Port = integer()
%%          Path = binary()
%%          Args = list()
%%          Result = {ok, UrlParts, Headers, State}
%%              | {stop, Reason, State} | {error, Reason, State}
%%
%%      handle_headers(Status, Headers, State) -> NewState
%%          Status = {Version, Status, Comment}
%%          Headers = [{Key, Value} | ...]
%%          Key = atom() | binary()
%%          Value = binary()
%%
%%      handle_data(Chunk, State) -> NewState
%%          Chunk = binary()
%%
%%      end_request(Info, State)
%%          Info = ok | {stop, Reason} | {error, Reason}
%%
-module(http_client).
-vsn(0.1).

%% Public interface
-export([http_connect/3]).

%% Behaviour information
-export([behaviour_info/1]).

-define(CONNECT_TIMEOUT, 30 * 1000).
-define(SEND_TIMEOUT, 20 * 1000).
-define(RECV_TIMEOUT, 60 * 1000).


%%
%% @doc Behaviour callbacks
%%
behaviour_info(callbacks) ->
    [{prepare_request, 2}, {end_request, 2},
        {handle_headers, 3}, {handle_data, 2}];
behaviour_info(_Other) ->
    undefined.


%%
%% @doc Connect to Url
%% @spec http_connect(Url, Behaviour, Args) -> Result
%%      Url = string()
%%      Behaviour = atom()
%%      Args = list()
%%      Result = ok | {stop, Reason} | {error, Reason}
%%
%% TODO: Add Options?
%% TODO: All errors must be handled by behaviour module
%%
http_connect(Url, Behaviour, Args) ->
    case url:urlsplit(Url) of
        {error, Reason} ->
            {error, Reason};
        {ok, UrlParts} ->
            case Behaviour:prepare_request(UrlParts, Args) of
                {ok, U, Headers, State} ->
                    http_connect(U, Headers, Behaviour, State);
                {stop, Reason, State} ->
                    Behaviour:end_request({stop, Reason}, State);
                {error, Reason, State} ->
                    Behaviour:end_request({error, Reason}, State)
            end
    end.

http_connect({http, Host, Port, Path}, Headers, Behaviour, State) ->
    Options = [
        binary,
        inet,
        {packet, http_bin},
        {send_timeout, ?SEND_TIMEOUT},
        {send_timeout_close, true},
        {active, false}
    ],
    case gen_tcp:connect(Host, Port, Options, ?CONNECT_TIMEOUT) of
        {ok, Sock} ->
            Request = http_request('GET', Path,
                http_headers(Headers, Host, [])),
            case gen_tcp:send(Sock, Request) of
                ok ->
                    Result = recv_response(Sock, Behaviour, State),
                    gen_tcp:close(Sock),
                    Result;
                Other ->
                    Behaviour:end_request(Other, State)
            end;
        Other ->
            Behaviour:end_request(Other, State)
    end.


%%
%% @doc Normalize HTTP headers
%% @spec http_headers(Headers, Host, []) -> [{binary(), binary()} | ...]
%%      Headers = [{atom(), binary()} | ...]
%%      Host = binary()
%%
%% TODO: Add User-Agent
%%
http_headers([{'Host', Host} | Headers], _, Collected) ->
    http_headers(Headers, seen, [{<<"HOST">>, Host} | Collected]);
http_headers([{Key, Value} | Headers], Host, Collected) ->
    http_headers(Headers, Host,
        [{atom_to_binary(Key, ascii), Value} | Collected]);
http_headers([], seen, Collected) ->
    Collected;
http_headers([], Host, Collected) ->
    http_headers([], seen, [{<<"HOST">>, list_to_binary(Host)} | Collected]).


%%
%% @doc Create and return HTTP request
%% @spec http_request(Method, Path, Headers) -> Request
%%      Method = 'GET'
%%      Path = binary()
%%      Headers = [{Key, Value} | ...]
%%      Key = atom()
%%      Value = binary()
%%
%% TODO: 'POST', 'HEAD' and maybe other methods?
%%
http_request('GET', Path, Headers) ->
    P = list_to_binary(Path),
    Header = <<"GET ",P/binary," HTTP/1.0\r\n">>,
    HeadersData = format_headers(Headers, <<>>),
    <<Header/binary,HeadersData/binary,"\r\n">>.

% TODO: Need to check corner cases
format_headers([], Data) ->
    Data;
format_headers([{Key, Value} | Headers], Data) ->
    format_headers(Headers,
        <<Data/binary,Key/binary,": ",Value/binary,"\r\n">>).


%%
%% @doc Receive HTTP response
%% @spec recv_response(Sock, Behaviour, State)
%%      Sock = socket()
%%      Behaviour = atom()
%%      State = term()
%%
recv_response(Sock, Behaviour, State) ->
    case recv_headers(Sock, none, [], 0) of
        {ok, Status, Headers, Size} ->
            NewState = Behaviour:handle_headers(Status, Headers, State),
            inet:setopts(Sock, [{packet, raw}]),
            recv_data(Sock, Size, Behaviour, NewState);
        Other ->
            Behaviour:end_request(Other, State)
    end.

recv_headers(Sock, HTTPHeader, Headers, Size) ->
    case gen_tcp:recv(Sock, 0, ?RECV_TIMEOUT) of
        {ok, {http_response, Version, Status, Comment}} ->
            recv_headers(Sock, {Version, Status, Comment}, Headers, Size);
        {ok, {http_header, _, 'Content-Length', _, Value}} ->
            recv_headers(Sock, HTTPHeader,
                [{'Content-Length', Value} | Headers],
                list_to_integer(binary_to_list(Value)));
        {ok, {http_header, _, Name, _, Value}} ->
            recv_headers(Sock, HTTPHeader, [{Name, Value} | Headers], Size);
        {ok, http_eoh} ->
            {ok, HTTPHeader, lists:reverse(Headers), Size};
        % TODO: Handle HTTP errors
        Other ->
            Other
    end.

recv_data(_Sock, 0, Behaviour, State) ->
    Behaviour:end_request(ok, State);
recv_data(Sock, Size, Behaviour, State) ->
    case gen_tcp:recv(Sock, 0, ?RECV_TIMEOUT) of
        {ok, Batch} ->
            NewState = Behaviour:handle_data(Batch, State),
            recv_data(Sock, Size - size(Batch), Behaviour, NewState);
        Other ->
            Behaviour:end_request(Other, State)
    end.
