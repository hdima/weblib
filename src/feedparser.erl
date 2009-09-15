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
