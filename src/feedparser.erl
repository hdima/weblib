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
%% @doc Feed parser
%%
%% Callbak module interface:
%%
%%      news_channel(ChannelInfo, Args) -> Result
%%          ChannelInfo = #newsChannel
%%          Args = term()
%%          Result = {ok, State} | {stop, State}
%%
-module(feedparser).

%% Public interface
-export([feed/3, feed/2]).

%% Behaviour information
-export([behaviour_info/1]).

-include("feedparser.hrl").


%%
%% @doc Behaviour information
%%
behaviour_info(callbacks) ->
    [{news_channel, 2}];
behaviour_info(_Other) ->
    undefined.


%%
%% @doc Start parsing process
%% @spec feed(Chunk, Behaviour, Args) -> DocId
%%      Chunk = binary()
%%      Behaviour = atom()
%%      Args = term()
%%      DocId = term()
%%
feed(Chunk, Behaviour, Args) when is_binary(Chunk) ->
    Client = self(),
    spawn_monitor(fun () -> parse(Client, Chunk, Behaviour, Args) end).


%%
%% @doc Continue parsing process
%% @spec feed(Chunk, DocId) -> Result
%%      Chunk = binary() | eof
%%      DocId = term()
%%      Result = ok | {fatal_error, Reason}
%%      Reason = term()
%%
feed(eof, {Server, Monitor}) ->
    Server ! {self(), <<>>},
    receive
        {Server, Result} ->
            Result;
        {'DOWN', Monitor, process, Server, Reason} ->
            {fatal_error, Reason}
    after
        500 ->
            {fatal_error, no_response}
    end;
feed(Chunk, {Server, Monitor}) when is_binary(Chunk) ->
    Server ! {self(), Chunk},
    receive
        {'DOWN', Monitor, process, Server, Reason} ->
            {fatal_error, Reason}
    after
        0 ->
            ok
    end.


%%
%% @doc Start parsing, now for real
%%
parse(Client, Chunk, Behaviour, Args) ->
    Result = xmerl_sax_parser:stream(Chunk, [
        {continuation_fun, fun next/1},
        {continuation_state, Client},
        {event_fun, fun handle_event/3},
        {event_state, {Behaviour, Args}}
    ]),
    Client ! {self(), Result}.


%%
%% @doc Return new chunk of XML
%%
next(Client) ->
    receive
        {Client, Chunk} ->
            {Chunk, Client}
    end.


%%
%% @doc Handle events
%%
handle_event(Event, _Location, {_Behaviour, _Args}=State) ->
    io:format("~p~n", [Event]),
    State.
