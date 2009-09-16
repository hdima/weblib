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
%% @doc Tests for URL module
%%
-module(test_url).

-export([test/0]).


test_urlsplit_http() ->
    {ok, {http, "host.domain", 80, "/"}}
        = url:urlsplit("host.domain"),
    {ok, {http, "host.domain", 80, "/folder"}}
        = url:urlsplit("host.domain/folder"),
    {ok, {http, "host.domain", 80, "/"}}
        = url:urlsplit("http://host.domain"),
    {ok, {http, "host.domain", 80, "/"}}
        = url:urlsplit("HTTP://host.domain"),
    {ok, {http, "host.domain", 80, "/folder"}}
        = url:urlsplit("http://host.domain/folder"),
    {ok, {http, "host.domain", 8080, "/"}}
        = url:urlsplit("host.domain:8080"),
    {ok, {http, "host.domain", 8080, "/folder"}}
        = url:urlsplit("host.domain:8080/folder"),
    {ok, {http, "host.domain", 8080, "/"}}
        = url:urlsplit("http://host.domain:8080"),
    {ok, {http, "host.domain", 8080, "/folder"}}
        = url:urlsplit("http://host.domain:8080/folder"),
    ok.


test_urlsplit_https() ->
    {ok, {https, "host.domain", 443, "/"}}
        = url:urlsplit("https://host.domain"),
    {ok, {https, "host.domain", 443, "/folder"}}
        = url:urlsplit("https://host.domain/folder"),
    {ok, {https, "host.domain", 443, "/folder"}}
        = url:urlsplit("HTTPS://host.domain/folder"),
    {ok, {https, "host.domain", 4430, "/"}}
        = url:urlsplit("https://host.domain:4430"),
    {ok, {https, "host.domain", 4430, "/folder"}}
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
