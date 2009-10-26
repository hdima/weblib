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
%% <pre>
%%      start_document(Location, State) -> Result
%%          Location = record()
%%          State = term()
%%          Result = {ok, NewState}
%%          NewState = term()
%%
%%      end_document(Location, State) -> Result
%%          Location = record()
%%          State = term()
%%          Result = {ok, State}
%%
%%      start_element(Tag, Attributes, Location, State) -> Result
%%          Tag = string()
%%          Attributes = [{Key, Value} | ...]
%%          Key = string()
%%          Value = string()
%%          Location = record()
%%          State = term()
%%          Result = {ok, State}
%%
%%      end_element(Tag, Location, State) -> Result
%%          Tag = string()
%%          Location = record()
%%          State = term()
%%          Result = {ok, State}
%%
%%      characters(Chunk, Location, State) -> Result
%%          Chunk = string()
%%          Location = record()
%%          State = term()
%%          Result = {ok, State}
%% </pre>
%%
-module(simplexml).
-author("Dmitry Vasiliev <dima@hlabs.spb.ru>").
-vsn("0.4").

%% Public interface
-export([parse/4, parse/2]).

%% Behaviour information
-export([behaviour_info/1]).

-include("simplexml.hrl").

%% Parser state
-record(state, {
    behaviour,
    state,
    location,
    tail=(<<>>),
    stack=[],
    decoder
    }).


%%
%% @doc Behaviour information
%% @spec behaviour_info(callbacks) -> Callbacks
%%      Callbacks = [{module(), Arity}]
%%      Arity = integer()
%%
behaviour_info(callbacks) ->
    [{start_document, 2}, {end_document, 2}, {start_element, 4},
        {end_element, 3}, {characters, 3}];
behaviour_info(_Other) ->
    undefined.


%%
%% @doc Start parse XML
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
    Location = #location{source=Source},
    {ok, NewState} = Behaviour:start_document(Location, State),
    % TODO: Need to be replaced with the real one
    Decoder = fun (S) -> binary_to_list(S) end,
    ParserState = #state{behaviour=Behaviour, state=NewState, decoder=Decoder,
        location=Location},
    case Chunk of
        <<>> ->
            {continue, ParserState};
        Chunk ->
            parse_document(Chunk, ParserState)
    end.


%%
%% @doc Continue parse XML
%% @spec parse(Chunk, ParserState) -> Result
%%      Chunk = binary() | eof
%%      ParserState = term()
%%      Result = {continue, ParserState} | {ok, State}
%%      Reason = term()
%%
parse(eof, #state{tail=(<<>>), stack=[]}=ParserState) ->
    {ok, ParserState#state.state};
parse(eof, ParserState) ->
    erlang:error({incomplete, ParserState#state.location});
parse(Chunk, ParserState) when is_binary(Chunk) ->
    Tail = ParserState#state.tail,
    parse_document(<<Tail/binary, Chunk/binary>>,
        ParserState#state{tail=(<<>>)}).


%%
%% @doc Start or continue document parsing
%% @spec parse_document(Chunk, ParserState) -> Result
%%      Chunk = binary()
%%      ParserState = term()
%%      Result = {continue, NewParserState} | {ok, State}
%%      NewParserState = term()
%%      State = term()
%%
parse_document(Chunk, ParserState) ->
    case parse_element(Chunk, ParserState) of
        #state{stack=[], tail=(<<>>)}=NewParserState ->
            B = NewParserState#state.behaviour,
            B:end_document(NewParserState#state.location,
                NewParserState#state.state);
        NewParserState ->
            {continue, NewParserState}
    end.


%%
%% @doc Parse XML element
%% @spec parse_element(Chunk, ParserState) -> NewParserState
%%      Chunk = binary()
%%      ParserState = term()
%%      NewParserState = term()
%%
parse_element(<<>>, ParserState) ->
    ParserState;
parse_element(Chunk, ParserState) ->
    B = ParserState#state.behaviour,
    try parse_term(Chunk, ParserState#state.location,
            ParserState#state.decoder) of
        {{open_tag, Tag, Attributes}, Tail, Location} ->
            {ok, State} = B:start_element(Tag, Attributes,
                ParserState#state.location, ParserState#state.state),
            Stack = ParserState#state.stack,
            parse_element(Tail, ParserState#state{state=State,
                stack=[Tag | Stack], location=Location});
        {{open_close_tag, Tag, Attributes}, Tail, Location} ->
            {ok, State} = B:start_element(Tag, Attributes,
                ParserState#state.location, ParserState#state.state),
            {ok, State2} = B:end_element(
                Tag, ParserState#state.location, State),
            parse_element(Tail, ParserState#state{state=State2,
                location=Location});
        {{close_tag, Tag}, Tail, Location} ->
            {ok, State} = B:end_element(
                Tag, ParserState#state.location, ParserState#state.state),
            case ParserState#state.stack of
                [Tag | Stack] ->
                    parse_element(Tail, ParserState#state{state=State,
                        stack=Stack, location=Location});
                _ ->
                    erlang:error({badtag, Location})
            end;
        {{characters, Data}, Tail, Location} ->
            {ok, State} = B:characters(
                Data, ParserState#state.location, ParserState#state.state),
            parse_element(Tail, ParserState#state{state=State,
                location=Location});
        {_, Tail, Location} ->
            % Skip other elements
            parse_element(Tail, ParserState#state{location=Location})
    catch
        throw:bad_name ->
            erlang:error({badtag, ParserState#state.location});
        throw:need_more_data ->
            ParserState#state{tail=Chunk}
    end.


%%
%% @doc Parse single XML term
%% @throws need_more_data
%% @spec parse_term(Chunk, Location, Decoder) -> Result
%%      Chunk = binary()
%%      Location = record()
%%      Source = string()
%%      Line = integer()
%%      Col = integer()
%%      Decoder = function()
%%      Result = {TermInfo, Tail}
%%      Tail = binary()
%%      TermInfo = {open_tag, Tag, Attributes}
%%          | {open_close_tag, Tag, Attributes}
%%          | {close_tag, Tag}
%%          | {characters, Data}
%%          | comment
%%          | processing_instruction
%%      Tag = string()
%%      Attributes = [{Key, Value}]
%%      Key = string()
%%      Value = string()
%%      Data = string()
%%
parse_term(<<"<!--", Tail/binary>>, Location, _) ->
    {NewTail, NewLocation} = skip_over(Tail, <<"-->">>, ?inc_col(Location, 4)),
    {comment, NewTail, NewLocation};
parse_term(<<"<?", Tail/binary>>, Location, _) ->
    {NewTail, NewLocation} = skip_over(Tail, <<"?>">>, ?inc_col(Location, 2)),
    {processing_instruction, NewTail, NewLocation};
parse_term(<<"<![CDATA[", Chunk/binary>>, Location, Decoder) ->
    {Data, Tail, NewLocation} = parse_cdata(Chunk, ?inc_col(Location, 9),
        Decoder, <<>>),
    {{characters, Data}, Tail, NewLocation};
parse_term(<<"</", Tail/binary>>, Location, Decoder) ->
    {Tag, Tail2, Location2} = parse_name(Tail, ?inc_col(Location, 2),
        Decoder, <<>>),
    case skip_whitespace(Tail2, Location2) of
        {<<">", Tail3/binary>>, Location3} ->
            {{close_tag, Tag}, Tail3, ?inc_col(Location3, 1)};
        {<<>>, _Location3} ->
            throw(need_more_data);
        {_, Location3} ->
            erlang:error({badtag, Location3})
    end;
parse_term(<<"<", Tail/binary>>, Location, Decoder) ->
    {Tag, Tail2, Location2} = parse_name(Tail, ?inc_col(Location, 1),
        Decoder, <<>>),
    {Attributes, Tail3, Location3} = parse_attributes(
        Tail2, Location2, Decoder, []),
    case skip_whitespace(Tail3, Location3) of
        {<<"/>", Tail4/binary>>, Location4} ->
            {{open_close_tag, Tag, Attributes}, Tail4, ?inc_col(Location4, 2)};
        {<<">", Tail4/binary>>, Location4} ->
            {{open_tag, Tag, Attributes}, Tail4, ?inc_col(Location4, 1)};
        {_, Location4} ->
            erlang:error({badattr, Location4})
    end;
parse_term(Chunk, Location, Decoder) ->
    {Data, Tail, NewLocation} = parse_data(Chunk, Location, Decoder, []),
    {{characters, Data}, Tail, NewLocation}.


%%
%% @doc Parse character data
%% @spec parse_data(Chunk, Location, Decoder, Parts) -> Result
%%      Chunk = binary()
%%      Location = record()
%%      Decoder = function()
%%      Parts = list()
%%      Result = {String, Tail, NewLocation}
%%      String = string()
%%      Tail = binary()
%%      NewLocation = record()
%%
parse_data(<<>>, Location, _Decoder, Parts) ->
    {lists:flatten(lists:reverse(Parts)), <<>>, Location};
parse_data(<<"<", _/binary>>=Tail, Location, _Decoder, Parts) ->
    {lists:flatten(lists:reverse(Parts)), Tail, Location};
parse_data(<<"&", Ref/binary>>, Location, Decoder, Parts) ->
    {String, Tail, NewLocation} = parse_reference(Ref, ?inc_col(Location, 1)),
    parse_data(Tail, NewLocation, Decoder, [String | Parts]);
parse_data(Data, Location, Decoder, Parts) ->
    {Binary, Tail, NewLocation} = parse_binary_data(Data, Location, <<>>),
    parse_data(Tail, NewLocation, Decoder, [Decoder(Binary) | Parts]).


%%
%% @doc Parse reference
%% @spec parse_reference(Chunk, Location) -> Result
%%      Chunk = binary()
%%      Location = record()
%%      Result = {Reference, Tail, NewLocation}
%%      Reference = list()
%%      Tail = binary()
%%      NewLocation = record()
parse_reference(<<"#x", HexNumber/binary>>, Location) ->
    parse_hex(HexNumber, ?inc_col(Location, 2), 0);
parse_reference(<<"#", Number/binary>>, Location) ->
    parse_decimal(Number, ?inc_col(Location, 1), 0);
parse_reference(Data, Location) ->
    parse_entity_ref(Data, Location, <<>>).


%%
%% @doc Parse entity reference
%% @throws need_more_data
%% @spec parse_entity_ref(Chunk, Location, Acc) -> Result
%%      Chunk = binary()
%%      Location = record()
%%      Acc = binary()
%%      Result = {String, Tail, NewLocation}
%%      String = string()
%%      Tail = binary()
%%      NewLocation = record()
%%
parse_entity_ref(<<>>, _Location, _Acc) ->
    throw(need_more_data);
parse_entity_ref(<<";", Tail/binary>>, Location, <<"lt">>) ->
    {"<", Tail, ?inc_col(Location, 1)};
parse_entity_ref(<<";", Tail/binary>>, Location, <<"gt">>) ->
    {">", Tail, ?inc_col(Location, 1)};
parse_entity_ref(<<";", Tail/binary>>, Location, <<"amp">>) ->
    {"&", Tail, ?inc_col(Location, 1)};
parse_entity_ref(<<";", Tail/binary>>, Location, <<"apos">>) ->
    {"'", Tail, ?inc_col(Location, 1)};
parse_entity_ref(<<";", Tail/binary>>, Location, <<"quot">>) ->
    {"\"", Tail, ?inc_col(Location, 1)};
parse_entity_ref(<<";", _/binary>>, Location, _Acc) ->
    erlang:error({badref, Location});
parse_entity_ref(<<C, Tail/binary>>, Location, <<>>)
        when ?is_namestartchar(C) ->
    parse_entity_ref(Tail, ?inc_col(Location, 1), <<C>>);
parse_entity_ref(<<_C, _Tail/binary>>, Location, <<>>) ->
    erlang:error({badref, Location});
parse_entity_ref(<<C, Tail/binary>>, Location, Acc) when ?is_namechar(C) ->
    parse_entity_ref(Tail, ?inc_col(Location, 1), <<Acc/binary, C>>);
parse_entity_ref(_Data, Location, _Acc) ->
    erlang:error({badref, Location}).


%%
%% @doc Parse decimal number
%% @throws need_more_data
%% @spec parse_decimal(Chunk, Location, Acc) -> Result
%%      Chunk = binary()
%%      Location = record()
%%      Acc = integer()
%%      Result = {String, Tail, NewLocation}
%%      String = string()
%%      Tail = binary()
%%      NewLocation = record()
%%
parse_decimal(<<>>, _Location, _Acc) ->
    throw(need_more_data);
parse_decimal(<<";", _Tail/binary>>, Location, 0) ->
    erlang:error({badref, Location});
parse_decimal(<<";", Tail/binary>>, Location, Acc) ->
    {[Acc], Tail, ?inc_col(Location, 1)};
parse_decimal(<<C, Tail/binary>>, Location, Acc) when C >= $0, C =< $9 ->
    parse_decimal(Tail, ?inc_col(Location, 1), Acc * 10 + C - $0);
parse_decimal(_Tail, Location, _Acc) ->
    erlang:error({badref, Location}).


%%
%% @doc Parse hexadecimal number
%% @throws need_more_data
%% @spec parse_hex(Chunk, Location, Acc) -> Result
%%      Chunk = binary()
%%      Location = record()
%%      Acc = integer()
%%      Result = {String, Tail, NewLocation}
%%      String = string()
%%      Tail = binary()
%%      NewLocation = record()
%%
parse_hex(<<>>, _Location, _Acc) ->
    throw(need_more_data);
parse_hex(<<";", _Tail/binary>>, Location, 0) ->
    erlang:error({badref, Location});
parse_hex(<<";", Tail/binary>>, Location, Acc) ->
    {[Acc], Tail, ?inc_col(Location, 1)};
parse_hex(<<C, Tail/binary>>, Location, Acc) when C >= $0, C =< $9 ->
    parse_hex(Tail, ?inc_col(Location, 1), Acc * 16 + C - $0);
parse_hex(<<C, Tail/binary>>, Location, Acc) when C >= $a, C =< $f ->
    parse_hex(Tail, ?inc_col(Location, 1), Acc * 16 + C - $a + 10);
parse_hex(<<C, Tail/binary>>, Location, Acc) when C >= $a, C =< $F ->
    parse_hex(Tail, ?inc_col(Location, 1), Acc * 16 + C - $A + 10);
parse_hex(_Tail, Location, _Acc) ->
    erlang:error({badref, Location}).


%%
%% @doc Parse binary data
%% @spec parse_binary_data(Chunk, Location, Acc) -> Result
%%      Chunk = binary()
%%      Location = record()
%%      Acc = binary()
%%      Result = {Data, Tail, NewLocation}
%%      Data = binary()
%%      Tail = binary()
%%      NewLocation = record()
%%
parse_binary_data(<<>>, Location, Data) ->
    {Data, <<>>, Location};
parse_binary_data(<<"<", _/binary>>=Tail, Location, Data) ->
    {Data, Tail, Location};
parse_binary_data(<<"&", _/binary>>=Tail, Location, Data) ->
    {Data, Tail, Location};
parse_binary_data(<<"\r\n", Tail/binary>>, Location, Data) ->
    parse_binary_data(Tail, ?inc_line(Location), <<Data/binary, "\r\n">>);
parse_binary_data(<<"\n", Tail/binary>>, Location, Data) ->
    parse_binary_data(Tail, ?inc_line(Location), <<Data/binary, "\n">>);
parse_binary_data(<<"\r", Tail/binary>>, Location, Data) ->
    parse_binary_data(Tail, ?inc_line(Location), <<Data/binary, "\r">>);
parse_binary_data(<<C, Tail/binary>>, Location, Data) ->
    parse_binary_data(Tail, ?inc_col(Location, 1), <<Data/binary, C>>).


%%
%% @doc Parse CDATA characters
%% @spec parse_cdata(Chunk, Location, Decoder, Acc) -> Result
%%      Chunk = binary()
%%      Location = record()
%%      Decoder = function()
%%      Acc = binary()
%%      Result = {Data, Tail, NewLocation}
%%      Data = string()
%%      Tail = binary()
%%      NewLocation = record()
%%
parse_cdata(<<>>, Location, Decoder, Data) ->
    % Don't try to return all possible data at once
    {Decoder(Data), <<>>, Location};
parse_cdata(<<"]]>", Tail/binary>>, Location, Decoder, Data) ->
    {Decoder(Data), Tail, ?inc_col(Location, 3)};
parse_cdata(<<"\r\n", Tail/binary>>, Location, Decoder, Data) ->
    parse_cdata(Tail, ?inc_line(Location), Decoder, <<Data/binary, "\r\n">>);
parse_cdata(<<"\n", Tail/binary>>, Location, Decoder, Data) ->
    parse_cdata(Tail, ?inc_line(Location), Decoder, <<Data/binary, "\n">>);
parse_cdata(<<"\r", Tail/binary>>, Location, Decoder, Data) ->
    parse_cdata(Tail, ?inc_line(Location), Decoder, <<Data/binary, "\r">>);
parse_cdata(<<C, Tail/binary>>, Location, Decoder, Data) ->
    parse_cdata(Tail, ?inc_col(Location, 1), Decoder, <<Data/binary, C>>).


%%
%% @doc Skip at the end of the pattern
%% @throws need_more_data
%% @spec skip_over(Chunk, EndPattern, Location) -> Result
%%      Chunk = binary()
%%      EndPattern = binary()
%%      Location = record()
%%      Result = {Tail, NewLocation}
%%      Tail = binary()
%%      NewLocation = record()
%%
skip_over(Chunk, EndPattern, Location) ->
    skip_over(Chunk, EndPattern, size(EndPattern), Location).

skip_over(<<>>, _EndPattern, _Size, _Location) ->
    throw(need_more_data);
skip_over(Chunk, EndPattern, Size, Location) ->
    case Chunk of
        <<EndPattern:Size/binary, Tail/binary>> ->
            {Tail, ?inc_col(Location, Size)};
        <<"\r\n", Tail/binary>> ->
            skip_over(Tail, EndPattern, Size, ?inc_line(Location));
        <<"\n", Tail/binary>> ->
            skip_over(Tail, EndPattern, Size, ?inc_line(Location));
        <<"\r", Tail/binary>> ->
            skip_over(Tail, EndPattern, Size, ?inc_line(Location));
        <<_, Tail/binary>> ->
            skip_over(Tail, EndPattern, Size, ?inc_col(Location, 1))
    end.


%%
%% @doc Skip whitespace characters
%% @spec skip_whitespace(Chunk, Location) -> Result
%%      Chunk = binary()
%%      Result = {Tail, NewLocation}
%%      Tail = binary()
%%      NewLocation = record()
%%
skip_whitespace(<<"\r\n", Tail/binary>>, Location) ->
    skip_whitespace(Tail, ?inc_line(Location));
skip_whitespace(<<"\n", Tail/binary>>, Location) ->
    skip_whitespace(Tail, ?inc_line(Location));
skip_whitespace(<<"\r", Tail/binary>>, Location) ->
    skip_whitespace(Tail, ?inc_line(Location));
skip_whitespace(<<" ", Tail/binary>>, Location) ->
    skip_whitespace(Tail, ?inc_col(Location, 1));
skip_whitespace(<<"\t", Tail/binary>>, Location) ->
    skip_whitespace(Tail, ?inc_col(Location, 1));
skip_whitespace(Tail, Location) ->
    {Tail, Location}.


%%
%% @doc Parse tag and attribute names
%% @throws bad_name | need_more_data
%% @spec parse_name(Chunk, Location, Decoder, Acc) -> Result
%%      Chunk = binary()
%%      Location = record()
%%      Decoder = function()
%%      Acc = binary()
%%      Result = {Tag, Tail, NewLocation}
%%      Tag = string()
%%      Tail = binary()
%%      NewLocation = record()
%%
parse_name(<<>>, _Location, _Decoder, _Acc) ->
    throw(need_more_data);
parse_name(<<C, Tail/binary>>, Location, Decoder, <<>>)
        when ?is_namestartchar(C) ->
    parse_name(Tail, ?inc_col(Location, 1), Decoder, <<C>>);
parse_name(_Chunk, _Location, _Decoder, <<>>) ->
    throw(bad_name);
parse_name(<<C, Tail/binary>>, Location, Decoder, Name) when ?is_namechar(C) ->
    parse_name(Tail, ?inc_col(Location, 1), Decoder, <<Name/binary, C>>);
parse_name(Tail, Location, Decoder, Name) ->
    {Decoder(Name), Tail, Location}.


%%
%% @doc Parse tag attributes
%% @throws need_more_data
%% @spec parse_attributes(Chunk, Location, Decoder, Acc) -> Result
%%      Chunk = binary()
%%      Location = record()
%%      Decoder = function()
%%      Acc = list()
%%      Result = {Attributes, Tail, NewLocation}
%%      Attributes = [{Key, Value}]
%%      Key = string()
%%      Value = string()
%%      Tail = binary()
%%      NewLocation = record()
%%
parse_attributes(Chunk, Location, Decoder, Attributes) ->
    case skip_whitespace(Chunk, Location) of
        {<<>>, _Location2} ->
            throw(need_more_data);
        {Chunk, Location2} ->
            {lists:reverse(Attributes), Chunk, Location2};
        {Tail, Location2} ->
            try parse_name(Tail, Location2, Decoder, <<>>) of
                {Name, Tail2, Location3} ->
                    {Tail3, Location4} = parse_eq(Tail2, Location3),
                    {Value, Tail4, Location5} = parse_attr_value(
                        Tail3, Location4, Decoder, [], none),
                    parse_attributes(Tail4, Location5, Decoder,
                        [{Name, Value} | Attributes])
            catch
                throw:bad_name ->
                    {lists:reverse(Attributes), Tail, Location2}
            end
    end.


%%
%% @doc Parse equality sign
%% @spec parse_eq(Chunk, Location) -> Result
%%      Chunk = binary()
%%      Locaiton = record()
%%      Result = {Tail, NewLocation}
%%      Tail = binary()
%%      NewLocation = record()
%%
parse_eq(Chunk, Location) ->
    case skip_whitespace(Chunk, Location) of
        {<<"=", Tail/binary>>, Location2} ->
            skip_whitespace(Tail, ?inc_col(Location2, 1));
        _ ->
            erlang:error({badattr, Location})
    end.


%%
%% @doc Parse attribute value
%% @throws need_more_data
%% @spec parse_attr_value(Chunk, Location, Decoder, Acc, Quote) -> Result
%%      Chunk = binary()
%%      Location = record()
%%      Decoder = function()
%%      Acc = list()
%%      Quote = none | 34 | 39
%%      Result = {Value, Tail, NewLocation}
%%      Value = string()
%%      Tail = binary()
%%      NewLocation = record()
%%
parse_attr_value(<<>>, _Location, _Decoder, _Acc, _Quote) ->
    throw(need_more_data);
parse_attr_value(<<C, Tail/binary>>, Location, Decoder, [], none)
        when ?is_quote(C) ->
    parse_attr_value(Tail, ?inc_col(Location, 1), Decoder, [], C);
parse_attr_value(<<Q, Tail/binary>>, Location, _Decoder, Parts, Q) ->
    {lists:flatten(lists:reverse(Parts)), Tail, ?inc_col(Location, 1)};
parse_attr_value(<<"&", Ref/binary>>, Location, Decoder, Parts, Q) ->
    {String, Tail, NewLocation} = parse_reference(Ref, ?inc_col(Location, 1)),
    parse_attr_value(Tail, NewLocation, Decoder, [String | Parts], Q);
parse_attr_value(Data, Location, Decoder, Parts, Q) ->
    {Binary, Tail, NewLocation} = parse_binary_value(Data, Location, <<>>, Q),
    parse_attr_value(Tail, NewLocation, Decoder, [Decoder(Binary) | Parts], Q).


%%
%% @doc Parse binary value
%% @throws need_more_data
%% @spec parse_binary_value(Chunk, Location, Acc, Quote) -> Result
%%      Chunk = binary()
%%      Location = record()
%%      Acc = binary()
%%      Quote = 34 | 39
%%      Result = {Value, Tail, NewLocation}
%%      Value = binary()
%%      Tail = binary()
%%      NewLocation = record()
%%
parse_binary_value(<<>>, _Location, _Acc, _Quote) ->
    throw(need_more_data);
parse_binary_value(<<Q, _/binary>>=Tail, Location, Value, Q) ->
    {Value, Tail, Location};
parse_binary_value(<<"\r\n", Tail/binary>>, Location, Value, Q) ->
    parse_binary_value(Tail, ?inc_line(Location), Value, Q);
parse_binary_value(<<"\n", Tail/binary>>, Location, Value, Q) ->
    parse_binary_value(Tail, ?inc_line(Location), Value, Q);
parse_binary_value(<<"\r", Tail/binary>>, Location, Value, Q) ->
    parse_binary_value(Tail, ?inc_line(Location), Value, Q);
parse_binary_value(<<C, Tail/binary>>, Location, Value, Q)
        when ?is_attrvaluechar(C, Q) ->
    parse_binary_value(Tail, ?inc_col(Location, 1), <<Value/binary, C>>, Q);
parse_binary_value(_Chunk, Location, _Acc, _Quote) ->
    erlang:error({badattr, Location}).
