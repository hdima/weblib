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
%% @doc RSS feed parser
%%
-module(rss_feed).
-author("Dmitry Vasiliev <dima@hlabs.spb.ru>").
-vsn("0.1").

%% Internal callbacks
-export([start_element/4, end_element/3, characters/3]).

-include("feedparser.hrl").
-include("feedparser_priv.hrl").


%%
%% @doc Start element callback function
%% @spec start_element(Tag, Attributes, Location, State) -> Result
%%      Tag = {Uri, LTag, QTag}
%%      Uri = string()
%%      LTag = string()
%%      QTag = string()
%%      Attributes = list()
%%      Location = record()
%%      State = term()
%%      Result = {ok, State}
%%
start_element(_Tag, _Attributes, _Location, State) ->
    {ok, State#state{data=""}}.


%%
%% @doc End element callback function
%% @spec end_element(Tag, Location, State) -> Result
%%      Tag = {Uri, LTag, QTag}
%%      Uri = string()
%%      LTag = string()
%%      QTag = string()
%%      Location = record()
%%      State = term()
%%      Result = {ok, State}
%%
end_element(_Tag, _Location, #state{stack=["channel", "rss"]}=State) ->
    UState2 = case State#state.channel_part of
        unknown ->
            Behaviour = State#state.behaviour,
            Info = State#state.channel_info,
            {ok, UState} = Behaviour:channel_info(Info, State#state.state),
            UState;
        _ ->
            % TODO: Need to check for news items
            State#state.state
    end,
    {ok, State#state{state=UState2}};
end_element(_Tag, _Location, #state{stack=[Tag | ["channel", "rss"]]}=State) ->
    Info = State#state.channel_info,
    Data = lists:flatten(lists:reverse(State#state.data)),
    NewInfo = case Tag of
        "title" ->
            Info#channel_info{title=Data};
        "link" ->
            Info#channel_info{link=Data, id=Data};
        "description" ->
            Info#channel_info{description=Data};
        "language" ->
            Info#channel_info{language=Data};
        "copyright" ->
            Info#channel_info{copyright=Data};
        "webMaster" ->
            Info#channel_info{webmaster=Data};
        "generator" ->
            Info#channel_info{generator=Data};
        "managingEditor" ->
            Info#channel_info{editor=Data};
        "category" ->
            Info#channel_info{categories=[Data
                | Info#channel_info.categories]};
        _ ->
            Info
    end,
    {ok, State#state{channel_info=NewInfo}};
end_element(_Tag, _Location, State) ->
    {ok, State}.


%%
%% @doc Character data callback function
%% @spec characters(Chunk, Location, State) -> Result
%%      Chunk = string()
%%      Location = record()
%%      State = term()
%%      Result = {ok, State}
%%
characters(Chunk, _Location, State) ->
    Data = [Chunk | State#state.data],
    {ok, State#state{data=Data}}.
