-module(xml).

-export([test/1]).


test(Xml) ->
    xmerl_sax_parser:stream(Xml, [
        {continuation_fun, fun next/1},
        {event_fun, fun handle_event/3}
    ]).


handle_event(Event, _Location, _State) ->
    io:format("~p~n", [Event]),
    ok.


next(_State) ->
    {<<>>, ok}.
