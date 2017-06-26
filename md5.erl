%%%-------------------------------------------------------------------
%%% @author YEGU Kwon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 10월 2016 오후 6:08
%%%-------------------------------------------------------------------
-module(md5).
-author("YEGU Kwon").

%% API
-export([md5_hex/1, aes_hex/2]).

md5_hex(S)->
  Md5_bin = erlang:md5(S),
  Md5_list = binary_to_list(Md5_bin),
  lists:flatten(list_to_hex(Md5_list)).

aes_hex(S, Rand)->
Key = <<"keouywgnngwykeou">>,
Ivec = Rand,
Aes_bin = crypto:block_encrypt(aes_cbc128, Key, Ivec, S),
Aes_list = binary_to_list(Aes_bin),
lists:flatten(list_to_hex(Aes_list)).

list_to_hex(L)->
  lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
  [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
  $0 + N;
hex(N) when N >= 10, N < 16 ->
  $a + (N-10).