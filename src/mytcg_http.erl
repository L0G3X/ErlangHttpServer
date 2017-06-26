%%%-------------------------------------------------------------------
%%% @author XKLEST
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 7??2016 16:33
%%%-------------------------------------------------------------------
-module(mytcg_http).
-author("XKLEST").
-include("mytcg_record.hrl").
%% API
-export([init/3, handle/2, terminate/3]).

init(_Type, Req, [])->
  {ok, Req, no_state}.

handle(Req, State)->
  {Api, Req1} = cowboy_req:binding(api, Req),
  {What, Req2} = cowboy_req:binding(what, Req1),
  {Opt, Req3} = cowboy_req:binding(opt, Req2),

  %% Data load
  {ok, Data, Req4} = cowboy_req:body_qs(Req3),

  io:format("~n api = ~p, what = ~p, opt = ~p ~n",[Api, What, Opt]),
  Reply = handle(Api, What, Opt, Data),

  {ok, Req5} = cowboy_req:reply(200, [
    {<<"content-type">>, <<"text/plain">>}
  ], Reply, Req4),
  {ok, Req5, State}.

handle(_,_,_,Data)->
  jsx:encode([
    {<<"Email : wt2080@gmail.com">>}
  ]);

handle(<<"user">>,<<"login">>,_,Data)->
  Id = proplists:get_value(<<"id">>, Data),
  Password = proplists:get_value(<<"password">>, Data),
  Guid = proplists:get_value(<<"guid">>, Data),
  case mytcg_users:login(Id, Password, Guid) of
    {ok, SessionKey}->
      jsx:encode([
        {<<"result">>,<<"ok">>},
        {<<"session_key">>, SessionKey}
      ]);
    update->
      jsx:encode([
        {<<"result">>,<<"update">>}
      ]);
    _->
      jsx:encode([{<<"result">>,<<"fail">>}])
  end;

handle(<<"user">>,<<"logout">>,_,Data)->
  SessionKey = proplists:get_value(<<"session_key">>, Data),
  case mytcg_users:logout(SessionKey) of
    ok->
      jsx:encode([{<<"result">>, <<"ok">>}]);
    _->
      jsx:encode([{<<"result">>,<<"fail">>}])
  end;


handle(<<"user">>,<<"join">>,_,Data)->
  Id = proplists:get_value(<<"id">>, Data),
  Password = proplists:get_value(<<"password">>,Data),
  case mytcg_users:join(Id, Password) of
      fail ->
        jsx:encode([{<<"result">>,<<"duplicated">>}]);
      ok->
        jsx:encode([{<<"result">>,<<"join">>}]);
      update->
        jsx:encode([{<<"result">>,<<"update">>}])
  end;

handle(<<"send">>,<<"point">>,_,Data)->
  SessionKey = proplists:get_value(<<"session_key">>,Data),
  Point1 = proplists:get_value(<<"point">>,Data),
  Guid = proplists:get_value(<<"guid">>,Data),
  Point = binary_to_integer(Point1),
  case mytcg_users:send_point(SessionKey, Point, Guid) of
    ok->
      jsx:encode([{<<"result">>,<<"ok">>}]);
    fail->
      jsx:encode([<<"result">>, <<"fail">>]);
    update->
      jsx:encode([<<"result">>, <<"update">>])
  end;

handle(<<"send">>,<<"token">>,_,Data)->
  SessionKey = proplists:get_value(<<"session_key">>, Data),
  Token = proplists:get_value(<<"token">>,Data),
  Guid = proplists:get_value(<<"guid">>,Data),
  case mytcg_users:send_token(SessionKey, Token, Guid) of
    ok->
      jsx:encode([{<<"result">>,<<"ok">>}]);
    fail->
      jsx:encode([<<"result">>, <<"fail">>]);
    update->
      jsx:encode([<<"result">>, <<"update">>])
  end;

handle(<<"send">>, <<"level">>,_,Data)->
  SessionKey = proplists:get_value(<<"session_key">>,Data),
  Level = proplists:get_value(<<"level">>, Data),
  Guid = proplists:get_value(<<"guid">>,Data),
  Level1 = binary_to_integer(Level),
  case mytcg_users:send_level(SessionKey, Level1, Guid) of
    ok->
      jsx:encode([{<<"result">>,<<"ok">>}]);
    fail->
      jsx:encode([<<"result">>, <<"fail">>]);
    update->
      jsx:encode([<<"result">>, <<"update">>])
  end;

handle(<<"send">>, <<"exp">>, _, Data)->
  SessionKey = proplists:get_value(<<"session_key">>, Data),
  Exp = proplists:get_value(<<"exp">>, Data),
  Guid = proplists:get_value(<<"guid">>,Data),
  Exp1 = binary_to_integer(Exp),
  case mytcg_users:send_exp(SessionKey, Exp1, Guid) of
    ok->
      jsx:encode([{<<"result">>,<<"ok">>}]);
    fail->
      jsx:encode([<<"result">>, <<"fail">>]);
    update->
      jsx:encode([<<"result">>, <<"update">>])
  end;

handle(<<"get">>,<<"point">>,_,Data)->
  SessionKey = proplists:get_value(<<"session_key">>,Data),
  case mytcg_users:get_point(SessionKey) of
    {ok, Point}->
      jsx:encode([{<<"result">>,<<"ok">>},
        {<<"Point">>, Point}]);
    fail->
      jsx:encode([{<<"result">>, <<"fail">>}])
  end;

handle(<<"get">>, <<"level">>,_,Data)->
  SessionKey = proplists:get_value(<<"session_key">>, Data),
  case mytcg_users:get_level(SessionKey) of
    {ok, Level} ->
      jsx:encode([{<<"result">>, <<"ok">>},
        {<<"Level">>, Level}]);
    fail->
      jsx:encode([{<<"result">>,<<"fail">>}])
  end;

handle(<<"get">>, <<"exp">>,_,Data)->
  SessionKey = proplists:get_value(<<"session_key">>, Data),
  case mytcg_users:get_exp(SessionKey) of
    {ok, Exp}->
      jsx:encode([{<<"result">>, <<"ok">>},
        {<<"Exp">>, Exp}]);
    fail->
      jsx:encode([{<<"result">>,<<"fail">>}])
  end;

handle(<<"get">>,<<"key">>,_,Data)->
  Version = proplists:get_value(<<"version">>,Data),
  case mytcg_users:get_priv_key(Version) of
    Key->
      jsx:encode(<<"Key">>,Key);
    fail->
      jsx:encode(<<"result">>,<<"fail">>)
  end;

handle(_,_,_,_)->
  jsx:encode([{<<"result">>,<<"error">>}]).

terminate(_Reason, _Req, _State)->
  ok.