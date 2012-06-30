% Copyright (c) 2012 Forest Belton (apples)
%
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
% THE SOFTWARE.
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
