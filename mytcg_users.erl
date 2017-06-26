%%%-------------------------------------------------------------------
%%% @author XKLEST
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 7월 2016 17:01
%%%-------------------------------------------------------------------

-module(mytcg_users).
-author("XKLEST").

-include("mytcg_record.hrl").
%% API
-export([join/2, login/3, logout/1, send_point/3, send_token/3, get_point/1, loop/2, get_level/1, get_exp/1, send_level/3, send_exp/3,get_priv_key/1]).

%%Functions
get_Ivec(Id)->
  F = fun()->
    case mnesia:read(ivecs, Id) of
      [U]->
        Ivec = U#ivecs.ivec,
        Ivec;
      _->
        fail
    end
      end,
  mnesia:activity(transaction, F).

get_priv_key(Version)->
  F = fun()->
    case mnesia:read(pubkey, Version) of
      [U]->
        Key = U#pubkey.key,
        Key;
      _->
        fail
    end
      end,
  mnesia:activity(transaction, F).

check_guid(Guid)->
  F = fun()->
    case mnesia:read(guid, Guid) of
      [U]->
        ok;
      []->
        fail;
      _->
        fail
    end
      end,
  mnesia:activity(transaction, F).

new_session(Id)->
  Time = now(),
  Pid = spawn(mytcg_users, loop, [Id, Time]),
  SessionKey = make_session_key(Id, Pid),
  erlang:send_after(1000,Pid,{check}),
  SessionKey.

make_session_key(Id, Pid)->
  {A1, A2, A3} = now(),
  random:seed(A1,A2,A3),

Num = random:uniform(10000),
Hash = erlang:phash2(Id),

List = io_lib:format("~.16B~.16B", [Hash, Num]),
SessionKey = list_to_binary(lists:append(List)),

ets:insert(session_list, {SessionKey, Pid}),
SessionKey.

%%User Session
join(Id, Password)->
  Crypto = md5:md5_hex(Password),
  Rand = rand:uniform(),
  RandKey = float_to_binary(Rand),
  Lists = binary_to_list(RandKey),
  Ivec = string:substr(Lists, 1,16),
  Crypto2 = md5:aes_hex(Crypto, Ivec),
  F = fun()->
    case mnesia:read(users, Id) of
      []->
        Users = #users{id = Id, password = Crypto2},
        ok = mnesia:write(Users);
      _->
        fail
    end,
    case mnesia:read(ivecs, Id) of
      []->
        User = #ivecs{id = Id, ivec = Ivec},
        ok = mnesia:write(User);
      _->
        fail
    end
      end,
  mnesia:activity(transaction, F).

login(Id, Password, Guid)->
  Crypto = md5:md5_hex(Password),
  Ivec = get_Ivec(Id),
  Crypto2 = md5:aes_hex(Crypto, Ivec),
  case check_guid(Guid) of
    ok->
      F = fun()->
        case mnesia:read(users, Id) of
          [U = #users{password = Crypto2}]->
            SessionKey = new_session(Id),
            {ok, SessionKey};
          _->
            fail
        end
          end,
      mnesia:activity(transaction, F);
    fail->
      update
  end
.

logout(SessionKey)->
  [Obj] = ets:match_object(session_list, {SessionKey, '_'}),
  ets:delete_object(session_list, Obj),
  ok.

%%Point
send_point(SessionKey, Point, Guid)->
  case ets:lookup(session_list, SessionKey) of
    [{SessionKey, Pid}]->
      Ref = make_ref(),
      Pid ! {self(), Ref, save_point, Point, Guid},
      receive
        {Ref, saved}->
          ok;
        {Ref, failed}->
          fail;
        {Ref, update}->
          update
      end;
    _->
      fail
  end.

save_point(Id, Point, Guid)->
  case check_guid(Guid) of
    ok->
      F = fun()->
        case mnesia:read(users, Id) of
          [U]->
            Users = U#users{point = Point},
            ok = mnesia:write(Users);
          _->
            fail
        end
          end,
      mnesia:activity(transaction, F);
    fail->
      update
  end.

get_point(SessionKey)->
  case ets:lookup(session_list, SessionKey) of
    [{SessionKey, Pid}]->
      Ref = make_ref(),
      Pid ! {self(), Ref, receive_point},
      receive
        {Ref, ok, Point}->
          {ok, Point};
        _->
          fail
      after 3000->
        fail
      end;
    _->
      fail
  end.

receive_point(ID)->
  F = fun()->
    case mnesia:read(users, ID) of
      [U]->
        Point = U#users.point,
        {ok, Point};
      _->
        fail
    end
      end,
  mnesia:activity(transaction, F).

%%Level
send_level(SessionKey, Level, Guid)->
  case ets:lookup(session_list, SessionKey) of
    [{SessionKey, Pid}]->
      Ref = make_ref(),
      Pid ! {self(), Ref, save_level, Level, Guid},
      receive
        {Ref, saved}->
          ok;
        {Ref, failed}->
          fail;
        {Ref, update}->
          update
      end;
    _->
      fail
  end.

save_level(Id, Level, Guid)->
 case check_guid(Guid) of
   ok->
     F = fun()->
       case mnesia:read(users, Id) of
         [U]->
           Users = U#users{level = Level},
           ok = mnesia:write(Users);
         []->
           update;
         _->
           fail
       end
         end,
     mnesia:activity(transaction, F);
   fail->
     update
 end.

get_level(SessionKey)->
  case ets:lookup(session_list, SessionKey) of
    [{SessionKey, Pid}]->
      Ref = make_ref(),
      Pid !{self(), Ref, receive_level},
      receive
        {Ref, ok, Level}->
          {ok, Level};
        _->
          fail
      end;
    _->
      fail
  end.

receive_level(ID)->
  F = fun()->
    case mnesia:read(users, ID) of
      [U]->
        Level = U#users.level,
        {ok, Level};
      _->
        fail
    end
      end,
  mnesia:activity(transaction, F).

%%Exp
send_exp(SessionKey, Exp, Guid)->
  case ets:lookup(session_list, SessionKey) of
    [{SessionKey, Pid}]->
      Ref = make_ref(),
      Pid ! {self(), Ref, save_exp, Exp, Guid},
      receive
        {Ref, saved}->
          ok;
        {Ref, failed}->
          fail;
        {Ref, update}->
          update
      end;
    _->
      fail
  end.

save_exp(Id, Exp, Guid)->
  case check_guid(Guid) of
    ok->
      F = fun()->
        case mnesia:read(users, Id) of
          [U]->
            Users = U#users{exp = Exp},
            ok = mnesia:write(Users);
          _->
            fail
        end
          end,
      mnesia:activity(transaction, F);
    fail->
      update
  end.

get_exp(SessionKey)->
  case ets:lookup(session_list, SessionKey) of
    [{SessionKey, Pid}]->
      Ref = make_ref(),
      Pid ! {self(), Ref, receive_exp},
      receive
        {Ref, ok, Exp}->
          {ok, Exp};
        _->
          fail
      end;
    _->
      fail
  end.

receive_exp(ID)->
  F = fun()->
    case mnesia:read(users, ID) of
      [U]->
        Exp = U#users.exp,
        {ok, Exp};
      _->
        fail
    end
      end,
  mnesia:activity(transaction, F).

%%Token
send_token(SessionKey, Token, Guid)->
  case ets:lookup(session_list, SessionKey) of
    [{SessionKey, Pid}]->
      Ref = make_ref(),
      Pid ! {self(), Ref, save_token, Token, Guid},
      receive
        {Ref, saved}->
          ok;
        {Ref, failed}->
          fail;
        {Ref, update}->
          update
      end;
    _->
      fail
  end.

save_token(Id, Token, Guid)->
  case check_guid(Guid) of
    ok->
      F = fun()->
        case mnesia:read(users, Id) of
          [U]->
            Users = U#users{token = Token},
            ok = mnesia:write(Users);
          []->
            update;
          _->
            fail
        end
          end,
      mnesia:activity(transaction, F);
    fail->
      update
  end.

%Loop
loop(Id, Time)->
  Time1 =
    receive
      {Pid, Ref, save_point, Point, Guid}->
        case save_point(Id, Point, Guid) of
          ok->
            Pid ! {Ref, saved};
          fail->
            Pid ! {Ref, failed};
          update->
            Pid ! {Ref, update}
        end,
        now();
      {Pid, Ref, save_token, Token, Guid}->
        save_token(Id, Token,Guid),
        Pid ! {Ref, saved},
        case save_token(Id, Token,Guid) of
          ok->
            Pid ! {Ref, saved};
          fail->
            Pid ! {Ref, failed};
          update->
            Pid ! {Ref, update}
        end,
        now();
      {Pid, Ref, save_level, Level, Guid}->
        case save_level(Id, Level, Guid) of
          ok->
            Pid ! {Ref, saved};
          fail->
            Pid ! {Ref, failed};
          update->
            Pid ! {Ref, update}
        end,
        now();
      {Pid, Ref, save_exp, Exp, Guid}->
        case save_exp(Id, Exp, Guid) of
          ok->
            Pid ! {Ref, saved};
          fail->
            Pid ! {Ref, failed};
          update->
            Pid ! {Ref, update}
        end,
        now();
      {Pid, Ref, receive_point}->
        case receive_point(Id) of
          {ok, Point}->
            Pid ! {Ref, ok, Point};
          _->
            fail
        end,
        now();
      {Pid, Ref, receive_level}->
        case receive_level(Id) of
          {ok, Level}->
            Pid !{Ref, ok, Level};
          _->
            fail
        end,
        now();
      {Pid, Ref, receive_exp}->
        case receive_exp(Id) of
          {ok, Exp}->
            Pid ! {Ref, ok, Exp};
          _->
            fail
        end,
        now();
      {check}->
        Time;
      _->
        Time
    end,
  loop(Id, Time1).