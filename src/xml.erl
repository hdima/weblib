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
%% @doc Relaxed XML parser
%%
%% Callbak module interface:
%%
%%      start_document(Args) -> Result
%%          Args = term()
%%          Result = {ok, State} | {stop, State} | {error, Reason}
%%
%%      end_document(State) -> Result
%%          State = term()
%%          Result = {ok, State}
%%
%%      start_element(Tag, Attributes, State) -> Result
%%          Tag = string()
%%          Attributes = [{Key, Value} | ...]
%%          Key = string()
%%          Value = string()
%%          State = term()
%%          Result = {ok, State}
%%
%%      end_element(Tag, State) -> Result
%%          Tag = string()
%%          State = term()
%%          Result = {ok, State}
%%
%%      characters(Chunk, State) -> Result
%%          Chunk = string()
%%          State = term()
%%          Result = {ok, State}
%%
-module(xml).
-author("Dmitry Vasiliev <dima@hlabs.spb.ru>").
-vsn("0.4").

%% Public interface
-export([parse/3, parse/2]).

%% Behaviour information
-export([behaviour_info/1]).


-record(state, {
    tail,
    behaviour,
    state
    }).


%%
%% @doc Behaviour information
%%
behaviour_info(callbacks) ->
    [{start_document, 1}, {end_document, 1}, {start_element, 3},
        {end_element, 2}, {characters, 2}];
behaviour_info(_Other) ->
    undefined.


%%
%% @doc Start parse XML
%% @spec parse(Chunk, Behaviour, Args) -> Result
%%      Chunk = binary()
%%      Behaviour = atom()
%%      Args = term()
%%      Result = {continue, DocId} | {eof, State} | {error, Reason}
%%      DocId = term()
%%
parse(<<>>, _, _) ->
    {error, nodata};
parse(Chunk, Behaviour, Args) when is_binary(Chunk) ->
    {ok, State} = Behaviour:start_document(Args),
    {ok, NewsState} = parse_element(Chunk, Behaviour, State),
    {ok, NextState} = Behaviour:end_document(NewsState),
    {eof, NextState}.


%%
%% @doc Continue parse XML
%% @spec parse(Chunk, DocId) -> Result
%%      Chunk = binary() | eof
%%      DocId = term()
%%      Result = {continue, DocId} | {eof, State) | {error, Reason}
%%      Reason = term()
%%
parse(eof, DocId) ->
    {eof, DocId#state.state};
parse(Chunk, DocId) when is_binary(Chunk) ->
    {continue, DocId}.


parse_element(<<"<", Tail/binary>>, Behaviour, State) ->
    parse_tag(Tail, Behaviour, State, []).


parse_tag(<<"/>", _/binary>>, Behaviour, State, TagName) ->
    Tag = lists:reverse(TagName),
    {ok, NewState} = Behaviour:start_element(Tag, [], State),
    Behaviour:end_element(Tag, NewState);
parse_tag(<<C, Tail/binary>>, Behaviour, State, TagName) ->
    parse_tag(Tail, Behaviour, State, [C | TagName]).