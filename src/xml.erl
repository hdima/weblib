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
-vsn("0.3").

%% Public interface
-export([parse/3, parse/2]).

%% Behaviour information
-export([behaviour_info/1]).

-include("xml.hrl").

%% Parser state
-record(state, {
    behaviour,
    state,
    tail=(<<>>),
    stack=[],
    decoder
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
    % TODO: Need to be replaced with the real one
    Decoder = fun (S) -> binary_to_list(S) end,
    Info = #state{behaviour=Behaviour, state=State, decoder=Decoder},
    parse_document(Chunk, Info).


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
    parse_document(<<Tail/binary, Chunk/binary>>, DocId#state{tail=(<<>>)}).


%%
%% @doc Start or continue document parsing
%% @spec parse_document(Chunk, Info) -> Result
%%      Chunk = binary()
%%      Info = term()
%%      Result = {continue, NewInfo} | {ok, State}
%%      NewInfo = term()
%%      State = term()
%%
parse_document(Chunk, Info) ->
    case parse_element(Chunk, Info) of
        #state{stack=[], tail=(<<>>)}=NewInfo ->
            B = NewInfo#state.behaviour,
            B:end_document(NewInfo#state.state);
        NewInfo ->
            {continue, NewInfo}
    end.


%%
%% @doc Parse XML element
%% @spec parse_element(Chunk, Info) -> NewInfo
%%      Chunk = binary()
%%      Info = term()
%%      NewInfo = term()
%%
parse_element(<<>>, Info) ->
    Info;
parse_element(Chunk, Info) ->
    B = Info#state.behaviour,
    try parse_term(Chunk, Info#state.decoder) of
        {{open_tag, Tag, Attributes}, Tail} ->
            {ok, State} = B:start_element(Tag, Attributes, Info#state.state),
            Stack = Info#state.stack,
            parse_element(Tail, Info#state{state=State, stack=[Tag | Stack]});
        {{open_close_tag, Tag, Attributes}, Tail} ->
            {ok, State} = B:start_element(Tag, Attributes, Info#state.state),
            {ok, State2} = B:end_element(Tag, State),
            parse_element(Tail, Info#state{state=State2});
        {{close_tag, Tag}, Tail} ->
            {ok, State} = B:end_element(Tag, Info#state.state),
            case Info#state.stack of
                [Tag | Stack] ->
                    parse_element(Tail, Info#state{state=State, stack=Stack});
                _ ->
                    erlang:error(xml_badtag)
            end;
        {{characters, Data}, Tail} ->
            {ok, State} = B:characters(Data, Info#state.state),
            parse_element(Tail, Info#state{state=State});
        {_, Tail} ->
            % Skip other elements
            parse_element(Tail, Info)
    catch
        throw:bad_name ->
            erlang:error(xml_badtag);
        throw:need_more_data ->
            Info#state{tail=Chunk}
    end.


%%
%% @doc Parse single XML term
%% @throws need_more_data
%% @spec parse_term(Chunk, Decoder) -> Result
%%      Chunk = binary()
%%      Decoder = function()
%%      Result = {TermInfo, Tail}
%%      Tail = binary()
%%      TermInfo = {open_tag, Tag, Attributes}
%%          | {open_close_tag, Tag, Attributes}
%%          | {close_tag, Tag}
%%          | {characters, Data}
%%          | comment
%%      Tag = string()
%%      Attributes = [{Key, Value} | ...]
%%      Key = string()
%%      Value = string()
%%      Data = string()
%%
parse_term(<<"<!--", Tail/binary>>, _) ->
    {comment, skip_over(Tail, <<"-->">>)};
parse_term(<<"</", Tail/binary>>, Decoder) ->
    {Tag, Tail2} = parse_name(Tail, Decoder, <<>>),
    case skip_whitespace(Tail2) of
        <<">", Tail3/binary>> ->
            {{close_tag, Tag}, Tail3};
        <<>> ->
            throw(need_more_data);
        _ ->
            erlang:error(xml_badtag)
    end;
parse_term(<<"<", Tail/binary>>, Decoder) ->
    {Tag, Tail2} = parse_name(Tail, Decoder, <<>>),
    {Attributes, Tail3} = parse_attributes(Tail2, Decoder, []),
    case skip_whitespace(Tail3) of
        <<"/>", Tail4/binary>> ->
            {{open_close_tag, Tag, Attributes}, Tail4};
        <<">", Tail4/binary>> ->
            {{open_tag, Tag, Attributes}, Tail4};
        _ ->
            erlang:error(xml_badattr)
    end;
parse_term(Chunk, Decoder) ->
    {Data, Tail} = parse_data(Chunk, Decoder, <<>>),
    {{characters, Data}, Tail}.


%%
%% @doc Parse character data
%% @spec parse_data(Chunk, Decoder, Acc) -> {Data, Tail}
%%      Chunk = binary()
%%      Decoder = function()
%%      Acc = binary()
%%      Data = string()
%%      Tail = binary()
%%
parse_data(<<>>, Decoder, Data) ->
    {Decoder(Data), <<>>};
parse_data(<<"<", _/binary>>=Tail, Decoder, Data) ->
    {Decoder(Data), Tail};
parse_data(<<C, Tail/binary>>, Decoder, Data) ->
    parse_data(Tail, Decoder, <<Data/binary, C>>).


%%
%% @doc Skip at the end of the pattern
%% @throws need_more_data
%% @spec skip_over(Chunk, Pattern) -> Tail
%%      Chunk = binary()
%%      Pattern = binary()
%%      Tail = binary()
%%
skip_over(Chunk, Pattern) ->
    skip_over(Chunk, Pattern, size(Pattern)).

skip_over(<<>>, _, _) ->
    throw(need_more_data);
skip_over(Chunk, Pattern, Size) ->
    case Chunk of
        <<Pattern:Size/binary, Tail/binary>> ->
            Tail;
        <<_, Tail/binary>> ->
            skip_over(Tail, Pattern, Size)
    end.


%%
%% @doc Skip whitespace characters
%% @spec skip_whitespace(Chunk) -> Tail
%%      Chunk = binary()
%%      Tail = binary()
%%
skip_whitespace(<<C, Tail/binary>>) when ?is_whitespace(C) ->
    skip_whitespace(Tail);
skip_whitespace(Tail) ->
    Tail.


%%
%% @doc Parse tag and attribute names
%% @throws bad_name | need_more_data
%% @spec parse_name(Chunk, Decoder, Acc) -> {Tag, Tail}
%%      Chunk = binary()
%%      Decoder = function()
%%      Acc = binary()
%%      Tag = string()
%%      Tail = binary()
%%
parse_name(<<>>, _, _) ->
    throw(need_more_data);
parse_name(<<C, Tail/binary>>, Decoder, <<>>) when ?is_namestartchar(C) ->
    parse_name(Tail, Decoder, <<C>>);
parse_name(_, _, <<>>) ->
    throw(bad_name);
parse_name(<<C, Tail/binary>>, Decoder, Name) when ?is_namechar(C) ->
    parse_name(Tail, Decoder, <<Name/binary, C>>);
parse_name(Tail, Decoder, Name) ->
    {Decoder(Name), Tail}.


%%
%% @doc Parse tag attributes
%% @throws need_more_data
%% @spec parse_attributes(Chunk, Acc) -> {Attributes, Tail}
%%      Chunk = binary()
%%      Acc = list()
%%      Attributes = [{Key, Value} | ...]
%%      Key = string()
%%      Value = string()
%%      Tail = binary()
%%
parse_attributes(Chunk, Decoder, Attributes) ->
    case skip_whitespace(Chunk) of
        <<>> ->
            throw(need_more_data);
        Chunk ->
            {lists:reverse(Attributes), Chunk};
        Tail ->
            try parse_name(Tail, Decoder, <<>>) of
                {Name, Tail2} ->
                    Tail3 = parse_eq(Tail2),
                    {Value, Tail4} = parse_value(Tail3, Decoder, <<>>, none),
                    parse_attributes(Tail4, Decoder,
                        [{Name, Value} | Attributes])
            catch
                throw:bad_name ->
                    {lists:reverse(Attributes), Tail}
            end
    end.


%%
%% @doc Parse equality sign
%% @spec parse_eq(Chunk) -> Tail
%%      Chunk = binary()
%%      Tail = binary()
%%
parse_eq(Chunk) ->
    case skip_whitespace(Chunk) of
        <<"=", Tail/binary>> ->
            skip_whitespace(Tail);
        _ ->
            erlang:error(xml_badattr)
    end.


%%
%% @doc Parse attribute value
%% @throws need_more_data
%% @spec parse_value(Chunk, Decoder, Acc, Quote) -> {Value, Tail}
%%      Chunk = binary()
%%      Decoder = function()
%%      Acc = binary()
%%      Quote = none | $" | $'
%%      Value = string()
%%      Tail = binary()
%%
parse_value(<<>>, _, _, _) ->
    throw(need_more_data);
parse_value(<<C, Tail/binary>>, Decoder, <<>>, none) when ?is_quote(C) ->
    parse_value(Tail, Decoder, <<>>, C);
parse_value(<<Q, Tail/binary>>, Decoder, Value, Q) ->
    {Decoder(Value), Tail};
parse_value(<<C, Tail/binary>>, Decoder, Value, Q)
        when ?is_attrvaluechar(C, Q) ->
    parse_value(Tail, Decoder, <<Value/binary, C>>, Q);
parse_value(_, _, _, _) ->
    erlang:error(xml_badattr).
