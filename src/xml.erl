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
-vsn("0.2").

%% Public interface
-export([parse/3, parse/2]).

%% Behaviour information
-export([behaviour_info/1]).

-include("xml.hrl").

%% Parser state
-record(state, {
    tail=(<<>>),
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
parse(eof, #state{tail=(<<>>), stack=[]}=DocId) ->
    {ok, DocId#state.state};
parse(eof, _) ->
    erlang:error(xml_error);
parse(Chunk, DocId) when is_binary(Chunk) ->
    Tail = DocId#state.tail,
    start_parse(<<Tail/binary, Chunk/binary>>, DocId#state{tail=(<<>>)}).


start_parse(Chunk, Info) ->
    try parse_element(Chunk, Info) of
        #state{stack=[], tail=(<<>>)}=Info2 ->
            (Info2#state.behaviour):end_document(Info2#state.state);
        Info2 ->
            {continue, Info2}
    catch
        throw:empty ->
            {continue, Info#state{tail=Chunk}}
    end.


parse_element(<<>>, Info) ->
    Info;
parse_element(<<"</", Tail/binary>>, Info) ->
    try parse_name(Tail, <<>>) of
        {Tag, Tail2} ->
            case skip_whitespace(Tail2) of
                <<">", Tail3/binary>> ->
                    B = Info#state.behaviour,
                    {ok, State} = B:end_element(Tag, Info#state.state),
                    case Info#state.stack of
                        [Tag | Stack] ->
                            parse_element(Tail3,
                                Info#state{state=State, stack=Stack});
                        _ ->
                            erlang:error(xml_badtag)
                    end;
                _ ->
                    erlang:error(xml_badtag)
            end
    catch
        throw:empty ->
            erlang:error(xml_badtag)
    end;
parse_element(<<"<", Tail/binary>>, Info) ->
    try parse_name(Tail, <<>>) of
        {Tag, Tail2} ->
            parse_open_tag(Tag, Tail2, Info)
    catch
        throw:empty ->
            erlang:error(xml_badtag)
    end;
parse_element(Chunk, Info) ->
    {Data, Tail} = parse_data(Chunk, <<>>),
    {ok, State} = (Info#state.behaviour):characters(Data, Info#state.state),
    parse_element(Tail, Info#state{state=State}).


parse_data(<<>>, Data) ->
    % TODO: Decode binary data
    {binary_to_list(Data), <<>>};
parse_data(<<"<", _/binary>>=Tail, Data) ->
    % TODO: Decode binary data
    {binary_to_list(Data), Tail};
parse_data(<<C, Tail/binary>>, Data) ->
    parse_data(Tail, <<Data/binary, C>>).


parse_open_tag(Tag, Tail, Info) ->
    {Attributes, Tail2} = parse_attributes(Tail, []),
    case skip_whitespace(Tail2) of
        <<"/>", Tail3/binary>> ->
            B = Info#state.behaviour,
            {ok, State} = B:start_element(Tag, Attributes,
                Info#state.state),
            {ok, State2} = B:end_element(Tag, State),
            parse_element(Tail3, Info#state{state=State2});
        <<">", Tail3/binary>> ->
            B = Info#state.behaviour,
            {ok, State} = B:start_element(Tag, Attributes,
                Info#state.state),
            Stack = Info#state.stack,
            parse_element(Tail3, Info#state{state=State, stack=[Tag | Stack]});
        _ ->
            erlang:error(xml_badattr)
    end.


skip_whitespace(<<C, Tail/binary>>) when ?is_whitespace(C) ->
    skip_whitespace(Tail);
skip_whitespace(Tail) ->
    Tail.


%%
%% @doc Parse tag and attribute names
%% @spec parse_name(Chunk, Acc) -> {Tag, Tail}
%%      Chunk = binary()
%%      Acc = binary()
%%      Tag = string()
%%      Tail = binary()
%% @throws empty
%%
parse_name(<<C, Tail/binary>>, <<>>) when ?is_namestartchar(C) ->
    parse_name(Tail, <<C>>);
parse_name(_, <<>>) ->
    throw(empty);
parse_name(<<C, Tail/binary>>, Name) when ?is_namechar(C) ->
    parse_name(Tail, <<Name/binary, C>>);
parse_name(Tail, Name) ->
    % TODO: Need to decode name
    {binary_to_list(Name), Tail}.


%%
%% @doc Parse tag attributes
%% @spec parse_attributes(Chunk, Acc) -> {Attributes, Tail}
%%      Chunk = binary()
%%      Acc = list()
%%      Attributes = list()
%%      Tail = binary()
%% @throws empty
%%
parse_attributes(Chunk, Attributes) ->
    case skip_whitespace(Chunk) of
        <<>> ->
            throw(empty);
        Chunk ->
            {lists:reverse(Attributes), Chunk};
        Tail ->
            try parse_name(Tail, <<>>) of
                {Name, Tail2} ->
                    Tail3 = parse_eq(Tail2),
                    {Value, Tail4} = parse_value(Tail3, <<>>, none),
                    parse_attributes(Tail4, [{Name, Value} | Attributes])
            catch
                throw:empty ->
                    {lists:reverse(Attributes), Tail}
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
