-module(pong_handler).
-behaviour(cowboy_websocket).

-export([ init/2
        , websocket_handle/2
        , websocket_info/2
        , terminate/3
        ]
).

init(Req, State) ->
  {cowboy_websocket, Req, State}.

% Responds to Frames from client
% InFrame    :: ping | pong | {text | binary | ping | pong, binary()}
websocket_handle(InFrame, State) ->
  io:format("Received ~p~n", [InFrame]),
  io:format("Sent Pong~n", []),
  {[{text, <<"Pong">>}], State}.

% This has to be here, but isn't used.
websocket_info(_Info, State) ->
  {[], State}.

terminate({remote, Code, Message}, _PartialReq, _State) -> 
  io:format("Received ~p ~p~n", [Code, Message]),
  io:format("Pong finished~n", []),
  ok.

