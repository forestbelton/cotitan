-module(cotitand).
-export([start/0, start/1]).

-define(DEFAULT_PORT, 48581).

start() ->
  start(?DEFAULT_PORT).

start(Port) ->
  {ok, LSock} = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}, {packet, 0}]),
  io:format("start: listening on port ~w~n", [Port]),
  
  % Spawn the listener and client manager processes.
  Manager = spawn(fun() -> manage([]) end),
  spawn(fun() -> listen(LSock, Manager) end),
  ok.

listen(LSock, Manager) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  
  % Print out some information about the client.
  {ok, {Address, Port}} = inet:peername(Sock),
  io:format("listener: client connected from ~w:~w~n", [Address, Port]),
  
  Client = spawn(cot_client, new, [Sock, Manager]),
  Manager ! {new_client, Client},
  listen(LSock, Manager).

manage(Clients) ->
  receive
    {new_client, Client} ->
      io:format("manager: ~w connected~n", [Client]),
      manage([Client] ++ Clients);
    
    {client_recv, Client, Bytes} ->
      io:format("manager: ~w sent ~w~n", [Client, Bytes]),
      manage(Clients);
    
    {client_close, Client, Error} ->
      io:format("manager: ~w disconnected (reason: ~w)~n", [Client, Error]),
      manage(lists:delete(Client, Clients))
  end.

