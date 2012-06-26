-module(cotitand).
-export([start/0, start/1]).

-define(DEFAULT_PORT, 48581).

start() ->
  start(?DEFAULT_PORT).

start(Port) ->
  {ok, LSock} = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}, {packet, 0}]),
  io:format("listening on port ~w~n", [Port]),
  
  % Spawn the listener and client manager processes.
  Manager = spawn(fun() -> manage([]) end),
  spawn(fun() -> listen(LSock, Manager) end),
  ok.

listen(LSock, Manager) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  
  % Print out some information about the client.
  {ok, {Address, Port}} = inet:peername(Sock),
  io:format("client connected from ~w:~w~n", [Address, Port]),
  
  Manager ! {new_client, Sock},
  listen(LSock, Manager).

manage(Clients) ->
  receive
    {new_client, Socket} ->
      manage([Socket] ++ Clients)
  end.

