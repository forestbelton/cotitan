-module(cot_packet).
-export([decode/3]).

% Packet ID table.
-define(PKT_LOGIN, 16#00).
-define(PKT_AUTH,  16#01).

decode(_Client, ?PKT_LOGIN, <<Username:8/unit:16, Password:8/unit:16>>) ->
  io:format("decoder: login attempt from ~s:~s~n", [Username, Password]);

decode(_Client, Type, _Data) ->
  io:format("decoder: unknown packet of type ~p~n", [Type]).
