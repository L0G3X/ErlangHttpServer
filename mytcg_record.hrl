%%%-------------------------------------------------------------------
%%% @author XKLEST
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 7월 2016 16:39
%%%-------------------------------------------------------------------
-author("XKLEST").

-record(users,
{
  id,
  password,
  level = 0,
  exp = 0,
  point = 0,
  token
}).
-record(ivecs,
{
  id,
  ivec
}).
-record(guid,
{
  uid,
  version
}).
-record(privkey,
{
  key,
  version
}).
-record(pubkey,
{
  key,
  version
}).