%%%-------------------------------------------------------------------
%%% @author XKLEST
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 7??2016 16:53
%%%-------------------------------------------------------------------
-module(mytcg_db).
-author("XKLEST").

-include("mytcg_record.hrl").

%% API
-export([install/0, uninstall/0, insertGUID/0, insertPrivKey/0, insertPubKey/0]).

install()->
  ok = mnesia:create_schema([node()]),
  application:start(mnesia),
  mnesia:create_table(guid, [{attributes, record_info(fields, guid)}, {disc_copies, [node()]}]),
  mnesia:create_table(users,[{attributes, record_info(fields, users)}, {disc_copies, [node()]}]),
  mnesia:create_table(ivecs, [{attributes, record_info(fields, ivecs)}, {disc_copies, [node()]}]),
  mnesia:create_table(privkey, [{attributes, record_info(fields, privkey)}, {disc_copies, [node()]}]),
  mnesia:create_table(pubkey, [{attributes, record_info(fields, privkey)}, {disc_copies, [node()]}]),
  application:stop(mnesia).

uninstall()->
  application:stop(mnesia),
  mnesia:delete_schema([node()]).

insertGUID()->
  F = fun()->
    Guid = #guid{uid = "DDE75E9B-F758-4F2B-9801-04250959504A", version = "1.0.0.1"},
    ok = mnesia:write(Guid)
    end,
  mnesia:activity(transaction, F).

insertPrivKey()->
  F = fun()->
    Key = #privkey{key=
    "", version = "1.0.0.1"},
    ok = mnesia:write(Key)
  end,
  mnesia:activity(transaction, F).

insertPubKey()->
  F = fun()->
    Key = #pubkey{key=
    ", version = "1.0.0.1"},
    ok = mnesia:write(Key)
      end,
  mnesia:activity(transaction, F).
