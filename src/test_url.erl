%%
%% @doc Tests for URL module
%%
-module(test_url).

-export([test/0]).


test_urlsplit_http() ->
    {ok, {http, <<"host.domain">>, 80, <<>>}}
        = url:urlsplit("host.domain"),
    {ok, {http, <<"host.domain">>, 80, <<"/folder">>}}
        = url:urlsplit("host.domain/folder"),
    {ok, {http, <<"host.domain">>, 80, <<>>}}
        = url:urlsplit("http://host.domain"),
    {ok, {http, <<"host.domain">>, 80, <<>>}}
        = url:urlsplit("HTTP://host.domain"),
    {ok, {http, <<"host.domain">>, 80, <<"/folder">>}}
        = url:urlsplit("http://host.domain/folder"),
    {ok, {http, <<"host.domain">>, 8080, <<>>}}
        = url:urlsplit("host.domain:8080"),
    {ok, {http, <<"host.domain">>, 8080, <<"/folder">>}}
        = url:urlsplit("host.domain:8080/folder"),
    {ok, {http, <<"host.domain">>, 8080, <<>>}}
        = url:urlsplit("http://host.domain:8080"),
    {ok, {http, <<"host.domain">>, 8080, <<"/folder">>}}
        = url:urlsplit("http://host.domain:8080/folder"),
    ok.


test_urlsplit_https() ->
    {ok, {https, <<"host.domain">>, 443, <<>>}}
        = url:urlsplit("https://host.domain"),
    {ok, {https, <<"host.domain">>, 443, <<"/folder">>}}
        = url:urlsplit("https://host.domain/folder"),
    {ok, {https, <<"host.domain">>, 443, <<"/folder">>}}
        = url:urlsplit("HTTPS://host.domain/folder"),
    {ok, {https, <<"host.domain">>, 4430, <<>>}}
        = url:urlsplit("https://host.domain:4430"),
    {ok, {https, <<"host.domain">>, 4430, <<"/folder">>}}
        = url:urlsplit("https://host.domain:4430/folder"),
    ok.


test_urlsplit_errors() ->
    {error, url} = url:urlsplit(""),
    {error, scheme} = url:urlsplit("ftp://ftp.host/"),
    ok.


test() ->
    test_urlsplit_http(),
    test_urlsplit_https(),
    test_urlsplit_errors(),
    ok.
