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
  % Initialize the sender/receiever.
  Client     = self(),
  Sender     = spawn_link(fun() -> sender(Socket)           end),
  Receiver   = spawn_link(fun() -> receiver(Client, Socket) end),
  
  % Catch exit signals and begin listening.
  process_flag(trap_exit, true),
  loop(Manager, Sender, Receiver).

loop(Manager, Sender, Receiver) ->
  receive
    {client_send, Bytes} ->
      Sender ! {client_send, Bytes},
      loop(Manager, Sender, Receiver);
    
    {'EXIT', _Pid, Reason} ->
      % Crash and burn!
      Manager ! {client_close, self(), Reason},
      exit(Sender,   kill),
      exit(Receiver, kill),
      ok
  end.

sender(Socket) ->
  receive
    % Send whatever we're told to.
    {client_send, Bytes} ->
      ok = gen_tcp:send(Socket, Bytes),
      sender(Socket)
  end.

receiver(Client, Socket) ->
  % Read in a well-formed packet.
  {ok, <<Length:16/little>>}            = gen_tcp:recv(Socket, 2),
  {ok, <<Type:16/little, Data/binary>>} = gen_tcp:recv(Socket, Length),
  
  % Decode and loop.
  cot_packet:decode(Client, Type, Data),
  receiver(Client, Socket).
