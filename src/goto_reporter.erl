-module(goto_reporter).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-export([clear/0, report/0]).

-record(state, {
          gotos = 0
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

clear() ->
    gen_server:call(?MODULE, clear).

report() ->
    gen_server:call(?MODULE, report).

%% gen_server.

init([]) ->
    {ok, #state{}}.

handle_call(clear, _From, State) ->
    {reply, ok, State#state{ gotos = 0 }};
handle_call(report, _From, State) ->
    {reply, State#state.gotos, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({gotos, X}, State) ->
    {noreply, State#state{ gotos = State#state.gotos + X }}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
