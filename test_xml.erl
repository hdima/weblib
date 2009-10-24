-module(test_xml).

-export([parse_file/1]).

-behaviour(simplexml).

-export([start_document/2, end_document/2, start_element/4, end_element/3,
    characters/3]).


parse_file(Filename) ->
    {ok, File} = file:open(Filename, [read, raw, binary]),
    {continue, ParseState} = simplexml:parse(<<>>, Filename, ?MODULE, none),
    read_file(File, ParseState).


read_file(File, ParseState) ->
    case file:read(File, 4096) of
        {ok, Data} ->
            case simplexml:parse(Data, ParseState) of
                {continue, NewParseState} ->
                    read_file(File, NewParseState);
                {ok, _} ->
                    file:close(File)
            end;
        eof ->
            simplexml:parse(eof, ParseState),
            file:close(File)
    end.


start_document(Location, Args) ->
    io:format("start document (~p)~n", [Location]),
    {ok, Args}.


end_document(Location, State) ->
    io:format("end document (~p)~n", [Location]),
    {ok, State}.

start_element(Tag, Attributes, Location, State) ->
    io:format("start element (~p): ~p ~p~n", [Location, Tag, Attributes]),
    {ok, State}.


end_element(Tag, Location, State) ->
    io:format("end element (~p): ~p~n", [Location, Tag]),
    {ok, State}.


characters(Chunk, Location, State) ->
    io:format("characters (~p): ~p~n", [Location, Chunk]),
    {ok, State}.
