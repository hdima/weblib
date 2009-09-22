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
%% @doc Relaxed parser for a subset of XML
%%
%% Callbak module interface:
%%
%%      start_document(Args) -> Result
%%          Args = term()
%%          Result = {ok, State}
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
-vsn("0.1").

%% Public interface
-export([parse/3, parse/2]).

%% Behaviour information
-export([behaviour_info/1]).

-include("xml.hrl").

%% Parser state
-record(state, {
    tail,
    stack=[],
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
%%      Result = {continue, DocId} | {ok, State}
%%      DocId = term()
%%
parse(<<>>, _, _) ->
    erlang:error(xml_nodata);
parse(Chunk, Behaviour, Args) when is_binary(Chunk) ->
    {ok, State} = Behaviour:start_document(Args),
    Info = #state{behaviour=Behaviour, state=State},
    start_parse(Chunk, Info).


%%
%% @doc Continue parse XML
%% @spec parse(Chunk, DocId) -> Result
%%      Chunk = binary() | eof
%%      DocId = term()
%%      Result = {continue, DocId} | {ok, State)
%%      Reason = term()
%%
parse(eof, DocId) ->
    {ok, DocId#state.state};
parse(Chunk, DocId) when is_binary(Chunk) ->
    {continue, DocId}.


start_parse(Chunk, Info) ->
    % TODO: Return Tail
    {Info2, Tail} = parse_element(Chunk, Info),
    (Info2#state.behaviour):end_document(Info2#state.state).


parse_element(<<"<", Tail/binary>>, Info) ->
    case parse_name(Tail, <<>>) of
        {"", Tail} ->
            erlang:error(xml_badtag);
        {Tag, Tail2} ->
            {Attributes, Tail3} = parse_attributes(Tail2, []),
            case skip_whitespace(Tail3) of
                <<"/>", Tail4/binary>> ->
                    B = Info#state.behaviour,
                    {ok, State} = B:start_element(Tag, Attributes,
                        Info#state.state),
                    {ok, State2} = B:end_element(Tag, State),
                    {Info#state{state=State2}, Tail4};
                _ ->
                    erlang:error(xml_badattr)
            end
    end.


skip_whitespace(<<C, Tail/binary>>) when ?is_whitespace(C) ->
    skip_whitespace(Tail);
skip_whitespace(Tail) ->
    Tail.


parse_name(<<C, Tail/binary>>, <<>>) when ?is_namestartchar(C) ->
    parse_name(Tail, <<C>>);
parse_name(<<C, Tail/binary>>, Name) when ?is_namechar(C) ->
    parse_name(Tail, <<Name/binary, C>>);
parse_name(Tail, Name) ->
    % TODO: Need to decode name
    {binary_to_list(Name), Tail}.


parse_attributes(Chunk, Attributes) ->
    case skip_whitespace(Chunk) of
        Chunk ->
            {lists:reverse(Attributes), Chunk};
        Tail ->
            case parse_name(Tail, <<>>) of
                {"", Tail} ->
                    {lists:reverse(Attributes), Tail};
                {Name, Tail2} ->
                    Tail3 = parse_eq(Tail2),
                    {Value, Tail4} = parse_value(Tail3, <<>>, none),
                    parse_attributes(Tail4, [{Name, Value} | Attributes])
            end
    end.


parse_eq(Chunk) ->
    case skip_whitespace(Chunk) of
        <<"=", Tail/binary>> ->
            skip_whitespace(Tail);
        _ ->
            erlang:error(xml_badattr)
    end.


parse_value(<<C, Tail/binary>>, <<>>, none) when ?is_quote(C) ->
    parse_value(Tail, <<>>, C);
parse_value(<<Q, Tail/binary>>, Value, Q) ->
    % TODO: Need to decode bytes
    {binary_to_list(Value), Tail};
parse_value(<<C, Tail/binary>>, Value, Q) when ?is_attrvaluechar(C, Q) ->
    parse_value(Tail, <<Value/binary, C>>, Q);
parse_value(_, _, _) ->
    erlang:error(xml_badattr).
