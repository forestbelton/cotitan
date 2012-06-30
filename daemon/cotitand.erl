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
-module(cotitand).
-export([start/0, start/1]).

-define(DEFAULT_PORT, 48581).

start() ->
  start(?DEFAULT_PORT).

start(Port) ->
  {ok, LSock} = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}, {packet, 0}]),
  io:format("start: listening on port ~w~n", [Port]),
  
  % Spawn the listener and client manager processes.
  Manager  = spawn(fun() -> manage([]) end),
  Listener = spawn(fun() -> listen(LSock, Manager) end),
  
  % Pass control over to the listener so the socket isn't closed.
  gen_tcp:controlling_process(LSock, Listener),
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

