%%%-------------------------------------------------------------------
%%% @author XKLEST
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 7??2016 22:03
%%%-------------------------------------------------------------------
-module(mytcg_app).
-author("XKLEST").

-behaviour(application).

%% Application callbacks
-export([start/2,
  stop/1,
  stop_server/0,
  start_server/0]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).

start(_StartType, _StartArgs) ->

  start_server(),

  %% Run Code reloader
  mytcg_reloader:start(),

  ets:new(session_list, [public, named_table]),

  observer:start(),

  case mytcg_sup:start_link() of
    {ok, Pid} ->
      io:format("Start ok~n"),
      {ok, Pid};
    Error ->
      Error
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.
start_server()->
  ok = application:start(crypto),
  ok = application:start(cowlib),
  ok = application:start(ranch),
  ok = application:start(cowboy),
  ok = application:start(mnesia),


  %% Cowboy Router setting
  Dispatch = cowboy_router:compile([
    {'_',[
      {"/:api/[:what/[:opt]]", mytcg_http, []}
    ]}
  ]),

  %% HTTP server setting
  {ok, _} = cowboy:start_http(http, 100,[
    {port, 6064}
  ],[
    {env, [{dispatch, Dispatch}]}
  ]).
stop_server()->
  ok = application:stop(mnesia),
  ok = application:stop(cowboy),
  ok = application:stop(ranch),
  ok = application:stop(cowlib),
  ok = application:stop(crypto).

%%%===================================================================
%%% Internal functions
%%%===================================================================
