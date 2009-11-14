%% Copyright (c) 2009, Dmitry Vasiliev <dima@hlabs.spb.ru>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% * Redistributions of source code must retain the above copyright notice,
%%   this list of conditions and the following disclaimer.
%% * Redistributions in binary form must reproduce the above copyright notice,
%%   this list of conditions and the following disclaimer in the documentation
%%   and/or other materials provided with the distribution.
%% * Neither the name of the copyright holders nor the names of its
%%   contributors may be used to endorse or promote products derived from this
%%   software without specific prior written permission. 
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
%%
%% @doc HTTP client
%%
%% Callback module interface:
%%
%% <pre>
%%      handle_headers(Method, Status, Headers, Args) -> Result
%%          Status = {Version, Status, Comment}
%%          Version = {Major=int(), Minor=int()}
%%          Headers = [{Key, Value}]
%%          Key = atom() | binary()
%%          Value = binary()
%%          Args = term()
%%          Result = {ok, State} | {stop, State}
%%
%%      handle_body(Chunk, State) -> Result
%%          Chunk = binary() | eof | closed
%%          Result = {ok, NewState} | {stop, State}
%% </pre>
%%
-module(http_client).
-author("Dmitry Vasiliev <dima@hlabs.spb.ru>").
-vsn("0.4").

%% Public interface
-export([http_request/6]).

%% Behaviour information
-export([behaviour_info/1]).

-record(state, {
    connect_timeout=60000,
    send_timeout=30000,
    recv_timeout=60000
    }).


%%
%% @doc Behaviour information
%% @spec behaviour_info(callbacks) -> Callbacks
%%      Callbacks = [{module(), Arity}]
%%      Arity = integer()
%%
behaviour_info(callbacks) ->
    [{handle_headers, 4}, {handle_body, 2}];
behaviour_info(_Other) ->
    undefined.


parse_options([], Options) ->
    Options;
parse_options([{connect_timeout, Timeout} | Tail], Options) ->
    parse_options(Tail, Options#state{connect_timeout=Timeout});
parse_options([{send_timeout, Timeout} | Tail], Options) ->
    parse_options(Tail, Options#state{send_timeout=Timeout});
parse_options([{recv_timeout, Timeout} | Tail], Options) ->
    parse_options(Tail, Options#state{recv_timeout=Timeout});
parse_options([Option | _], _) ->
    erlang:error({badarg, Option}).


%%
%% @doc Send HTTP request
%% @spec http_request(Method, Url, Headers, Behaviour, Args, Opts) -> ok
%%      Method = 'GET' | 'HEAD'
%%      Url = string() | tuple()
%%      Headers = [{Key, Value}]
%%      Key = atom()
%%      Value = binary()
%%      Behaviour = module()
%%      Args = term()
%%      Opts = record()
%%
http_request(Method, Url, Headers, Behaviour, Args, Opts) when is_list(Url) ->
    http_request(Method, url:urlsplit(Url), Headers, Behaviour, Args, Opts);
http_request(Method, Url, Headers, Behaviour, Args, Opts) when is_tuple(Url) ->
    try
        O = parse_options(Opts, #state{}),
        http_connect(Url, Headers, Method, Behaviour, Args, O),
        ok
    catch
        throw:stop ->
            ok
    end.


http_connect({http, Host, Port, _}=Url, Headers, Method, Behaviour, Args, O) ->
    Options = [
        {active, false},
        binary,
        inet,
        {nodelay, true},
        {packet, http_bin},
        {send_timeout, O#state.send_timeout},
        {send_timeout_close, true}
    ],
    case gen_tcp:connect(Host, Port, Options, O#state.connect_timeout) of
        {ok, Sock} ->
            try send_request(Sock, Url, Headers, Method, Behaviour, Args, O)
            after
                gen_tcp:close(Sock)
            end;
        {error, Reason} ->
            erlang:error(http_connect_error, [Reason])
    end.


send_request(Sock, {http, Host, Port, Path},
        Headers, Method, Behaviour, Args, O) ->
    Request = create_request(Method, Path,
        http_headers(Headers, {http, Host, Port}, [])),
    case gen_tcp:send(Sock, Request) of
        ok ->
            recv_response(Sock, Method, Behaviour, Args, O);
        {error, Reason} ->
            erlang:error(http_send_error, [Reason])
    end.


%%
%% @doc Normalize HTTP headers
%% @spec http_headers(Headers, Host, []) -> [{binary(), binary()}]
%%      Headers = [{atom(), binary()}]
%%      Host = {http, list(), integer()} | seen
%%
http_headers([{'Host', Host} | Headers], _, Collected) ->
    http_headers(Headers, seen, [{<<"Host">>, Host} | Collected]);
http_headers([{Key, Value} | Headers], Host, Collected) ->
    http_headers(Headers, Host,
        [{atom_to_binary(Key, latin1), Value} | Collected]);
http_headers([], seen, Collected) ->
    Collected;
http_headers([], {http, Host, Port}, Collected) ->
    H = list_to_binary(Host),
    case Port of
        80 ->
            % Default HTTP port
            Val = H;
        Port ->
            P = list_to_binary(integer_to_list(Port)),
            Val = <<H/binary,":",P/binary>>
    end,
    http_headers([], seen, [{<<"Host">>, Val} | Collected]).


%%
%% @doc Create and return HTTP request
%% @spec create_request(Method, Path, Headers) -> Request
%%      Method = atom()
%%      Path = string()
%%      Headers = [{Key, Value}]
%%      Key = atom()
%%      Value = binary()
%%      Request = binary()
%%
create_request(Method, Path, Headers)
        when Method =:= 'GET'; Method =:= 'HEAD' ->
    M = atom_to_binary(Method, latin1),
    P = list_to_binary(Path),
    Status = <<M/binary," ",P/binary," HTTP/1.0\r\n">>,
    HeadersData = format_headers(Headers, <<>>),
    <<Status/binary,HeadersData/binary,"\r\n">>.

format_headers([], Data) ->
    Data;
format_headers([{Key, Value} | Headers], Data) ->
    format_headers(Headers,
        <<Data/binary,Key/binary,": ",Value/binary,"\r\n">>).


%%
%% @doc Receive HTTP response
%% @spec recv_response(Sock, Method, Behaviour, Args, Opts) -> term()
%%      Sock = socket()
%%      Method = atom()
%%      Behaviour = module()
%%      Args = term()
%%      Opts = record()
%%
-spec recv_response(term(), atom(), module(), term(), tuple()) -> no_return().

recv_response(Sock, Method, Behaviour, Args, O) ->
    {Status, Headers, Size} = recv_headers(Sock, none, [], unknown, O), 
    case Behaviour:handle_headers(Method, Status, Headers, Args) of
        {ok, State} ->
            {_, S, _} = Status,
            case Method of
                'HEAD' ->
                    % Ignore body per RFC2616
                    Behaviour:handle_body(eof, State);
                _ when S < 200; S =:= 204; S =:= 304 ->
                    % Ignore body per RFC2616
                    Behaviour:handle_body(eof, State);
                _ ->
                    inet:setopts(Sock, [{packet, raw}]),
                    recv_data(Sock, Size, Behaviour, State, O)
            end;
        {stop, State} ->
            Behaviour:handle_body(eof, State),
            throw(stop)
    end.


%%
%% @doc Receive HTTP headers
%% @spec recv_headers(Sock, HTTPHeader, Headers, Size, Opts) -> none()
%%      Sock = socket()
%%      HTTPHeader = none | {Version, Status, Comment}
%%      Version = tuple()
%%      Status = integer()
%%      Comment = binary()
%%      Headers = [{Key, Value}]
%%      Key = atom() | binary()
%%      Value = binary()
%%      Size = integer() | unknown
%%      Opts = record()
%%
-spec recv_headers(term(), none | tuple(), list(),
    integer() | unknown, tuple()) -> no_return().

recv_headers(Sock, HTTPHeader, Headers, Size, O) ->
    case gen_tcp:recv(Sock, 0, O#state.recv_timeout) of
        {ok, {http_response, Version, Status, Comment}} ->
            recv_headers(Sock, {Version, Status, Comment}, Headers, Size, O);
        {ok, {http_header, _, 'Content-Length', _, Value}} ->
            recv_headers(Sock, HTTPHeader,
                [{'Content-Length', Value} | Headers],
                list_to_integer(binary_to_list(Value)), O);
        {ok, {http_header, _, Name, _, Value}} ->
            recv_headers(Sock, HTTPHeader, [{Name, Value} | Headers], Size, O);
        {ok, http_eoh} ->
            {HTTPHeader, lists:reverse(Headers), Size};
        {ok, {http_error, Reason}} ->
            erlang:error(http_error, [Reason]);
        {error, Reason} ->
            erlang:error(http_receive_error, [Reason])
    end.


%%
%% @doc Receive HTTP body data
%% @spec recv_data(Sock, Size, Behaviour, State, Opts) -> none()
%%      Sock = socket()
%%      Size = integer() | unknown
%%      Behaviour = module()
%%      State = term()
%%      Opts = record()
%%
recv_data(_Sock, 0, Behaviour, State, _O) ->
    Behaviour:handle_body(eof, State);
recv_data(Sock, Size, Behaviour, State, O) ->
    case gen_tcp:recv(Sock, 0, O#state.recv_timeout) of
        {ok, Batch} ->
            case Behaviour:handle_body(Batch, State) of
                {ok, NewState} ->
                    case Size of
                        unknown ->
                            recv_data(Sock, unknown, Behaviour, NewState, O);
                        Num ->
                            S = Num - size(Batch),
                            recv_data(Sock, S, Behaviour, NewState, O)
                    end;
                {stop, State} ->
                    Behaviour:handle_body(eof, State),
                    throw(stop)
            end;
        {error, closed} ->
            case Size of
                unknown ->
                    Behaviour:handle_body(eof, State);
                _ ->
                    Behaviour:handle_body(closed, State),
                    erlang:error(http_receive_error, [closed])
            end;
        {error, Reason} ->
            Behaviour:handle_body(closed, State),
            erlang:error(http_receive_error, [Reason])
    end.
