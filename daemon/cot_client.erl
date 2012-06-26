-module(cot_client).
-export([new/2]).

new(Socket, Manager) ->
  % Initialize the sender/receiever and collect PIDs.
  Controller = self(),
  Sender     = spawn(fun() -> sender(Socket) end),
  Receiver   = spawn(fun() -> receiver(Socket, Controller) end),
  loop(Manager, Sender, Receiver).

loop(Manager, Sender, Receiver) ->
  receive
    {client_send, Bytes} ->
      Sender ! {send, Bytes};
    {client_recv, Bytes} ->
      % Send it up the chain for now, until decoding is added.
      Manager ! {client_recv, self(), Bytes}
  end,
  loop(Manager, Sender, Receiver).

sender(Socket) ->
  receive
    {client_send, Bytes} ->
      % TODO: Error checking.
      gen_tcp:send(Socket, Bytes)
  end,
  sender(Socket).

receiver(Socket, Controller) ->
  {ok, Bytes} = gen_tcp:recv(Socket, 0),
  Controller ! {client_recv, Bytes},
  receiver(Socket, Controller).
