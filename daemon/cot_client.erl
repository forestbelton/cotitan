-module(cot_client).
-export([new/2]).

new(Socket, Manager) ->
  % Initialize the sender/receiever and collect PIDs.
  Controller = self(),
  Sender     = spawn(fun() -> sender(Socket, Controller) end),
  Receiver   = spawn(fun() -> receiver(Socket, Controller) end),
  loop(Manager, Sender, Receiver).

loop(Manager, Sender, Receiver) ->
  receive
    {client_send, Bytes} ->
      Sender ! {send, Bytes},
      loop(Manager, Sender, Receiver);
    
    {client_recv, Bytes} ->
      % Send it up the chain for now, until decoding is added.
      Manager ! {client_recv, self(), Bytes},
      loop(Manager, Sender, Receiver);
    
    {client_close, E} ->
      % Tell everyone that this process is going away.
      Manager  ! {client_close, self(), E},
      Sender   ! client_close,
      ok
  end.

sender(Socket, Controller) ->
  receive
    {client_send, Bytes} ->
      case gen_tcp:send(Socket, Bytes) of
        {error, E} ->
          Controller ! {client_close, E};
        _Else ->
          sender(Socket, Controller)
      end;
    
    client_close ->
      ok
  end.

receiver(Socket, Controller) ->
  case gen_tcp:recv(Socket, 0) of
    {error, E} ->
      Controller ! {client_close, E};
    {ok, Bytes} ->
      Controller ! {client_recv, Bytes},
      receiver(Socket, Controller)
  end.
