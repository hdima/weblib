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
%% <pre>
%%      news_channel(ChannelInfo, Args) -> Result
%%          ChannelInfo = #newsChannel
%%          Args = term()
%%          Result = {ok, State}
%% </pre>
%%
-module(feedparser).
-author("Dmitry Vasiliev <dima@hlabs.spb.ru>").
-vsn("0.1").

%% Public interface
-export([parse/4, parse/2]).

%% Behaviour information
-export([behaviour_info/1]).

-behaviour(simplexml).

%% Behaviour callbacks
-export([start_document/2, end_document/2,
    start_element/4, end_element/3, characters/3]).

-include("feedparser.hrl").

%% Parser state
-record(state, {
    behaviour,
    state,
    stack=[],
    module=unknown
    }).


%%
%% @doc Behaviour information
%% @spec behaviour_info(callbacks) -> Callbacks
%%      Callbacks = [{module(), Arity}]
%%      Arity = integer()
%%
behaviour_info(callbacks) ->
    [{news_channel, 2}];
behaviour_info(_Other) ->
    undefined.


%%
%% @doc Start parse feed source
%% @spec parse(Chunk, Source, Behaviour, State) -> Result
%%      Chunk = binary()
%%      Source = string() | unknown
%%      Behaviour = module()
%%      State = term()
%%      Result = {continue, ParserState} | {ok, NewState}
%%      ParserState = term()
%%      NewState = term()
%%
parse(Chunk, Source, Behaviour, State) when is_binary(Chunk) ->
    ParserState = #state{behaviour=Behaviour, state=State},
    simplexml:parse(Chunk, Source, ?MODULE, ParserState).


%%
%% @doc Continue parse feed source
%% @spec parse(Chunk, ParserState) -> Result
%%      Chunk = binary() | eof
%%      ParserState = term()
%%      Result = {continue, ParserState} | {ok, State}
%%      Reason = term()
%%
parse(Chunk, ParserState) ->
    simplexml:parse(Chunk, ParserState).


start_document(Location, State) ->
    {ok, State}.

end_document(Location, State) ->
    {ok, State}.

start_element({"", "rss", QTag}=Tag, Attributes, Location, State) ->
    NewState = case State#state.stack of
        [] ->
            % TODO: Call Behaviour:start_feed (or start_document). Or we need
            % to call it at start_document?
            % TODO: Pass #state to the submodule?
            State#state{module=rss_feed, stack=[QTag]};
        Other ->
            Module = State#state.module,
            {ok, State2} = Module:start_element(Tag, Attributes,
                Location, State),
            State2
    end,
    {ok, NewState};
start_element({"http://www.w3.org/2005/Atom", "feed", Tag},
        Attributes, Location, State) ->
    % TODO: Call Behaviour:start_feed (or start_document). Or we need to
    % call it at start_document?
    NewState = case State#state.stack of
        [] ->
            State#state{module=atom_feed, stack=[Tag]};
        Other ->
            %% TODO: Pass element to the submodule
            State
    end,
    {ok, NewState};
start_element(_Tag, _Attributes, Location, #state{stack=[]}) ->
    erlang:error({bad_feed, Location});
start_element(Tag, Attributes, Location, State) ->
    {ok, State}.

end_element(Tag, Location, State) ->
    {ok, State}.

characters(Chunk, Location, State) ->
    {ok, State}.
