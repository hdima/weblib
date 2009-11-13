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
%% @doc Crawler
%%
-module(crawler).
-author("Dmitry Vasiliev <dima@hlabs.spb.ru>").
-vsn("0.1").

%% Public interface
-export([crawl/3, start/0, start_link/0, stop/0]).

%% Protected interface
-export([get_next_url/1]).

%% Behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).


% TODO: Move to crawl/3 options
-define(SLEEP_TIMEOUT, 3 * 60 * 1000).


%%
%% @doc Crawl URL use handler
%% @spec crawl(Url, Handler, Args) -> ok
%%      Url = string()
%%      Handler = function()
%%      Args = list()
%%
crawl(Url, Handler, Args) when is_list(Url) ->
    crawl(url:urlsplit(Url), Handler, Args);
crawl(Url, Handler, Args) when is_tuple(Url) ->
    gen_server:call(?MODULE, {crawl, Url, Handler, Args}).


%%
%% @doc Get next URL to crawl (only for internal subprocesses)
%%
get_next_url(Host) ->
    gen_server:call(?MODULE, {get_next_url, Host}).


%%
%% @doc Start crawler
%%
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).


%%
%% @doc Start crawler
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%
%% @doc Stop crawler
%%
stop() ->
    gen_server:cast(?MODULE, stop).


%%
%% @doc Initialise process
%%
init([]) ->
    process_flag(trap_exit, true),
    % Ignore duplicate values
    ets:new(?MODULE, [bag, private, named_table]),
    {ok, none}.

terminate(_Reason, _State) ->
    ets:delete(?MODULE),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
%% Message handling
%%

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


handle_call({crawl, {_, Host, _, _}, Handler, Args}, _From, State) ->
    case ets:lookup(?MODULE, Host) of
        [{_, Pid, _, _} | _] ->
            ets:insert(?MODULE, {Host, Pid, Handler, Args});
        [] ->
            Proc = fun () -> handler(Host) end,
            Pid = proc_lib:spawn_link(Proc),
            ets:insert(?MODULE, {Host, Pid, Handler, Args})
    end,
    {reply, ok, State};
handle_call({get_next_url, Host}, From, State) ->
    Result = case ets:lookup(?MODULE, Host) of
        [{Host, From, Handler, Args}=Info | _] ->
            ets:delete_object(?MODULE, Info),
            {Handler, Args};
        [] ->
            none;
        _ ->
            % Request from unknown process
            badarg
    end,
    {reply, Result, State};
handle_call({'EXIT', From, normal}, From, State) ->
    ok;
handle_call({'EXIT', From, Reason}, From, State) ->
    % TODO: How we can restart this process - we don't know Host value?
    ok;
handle_call(_, _, State) ->
    {reply, badarg, State}.


handler(Host) ->
    timer:sleep(?SLEEP_TIMEOUT),
    case get_next_url(Host) of
        {Handler, Args} ->
            % TODO: Handle Handler return value
            Handler(Args),
            handler(Host);
        empty ->
            ok
    end.
