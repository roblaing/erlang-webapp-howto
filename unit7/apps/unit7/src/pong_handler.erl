-module(pong_handler).
-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

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
  {[{text, <<"I've no idea what this does">>}], State}.

terminate({remote, _Code, Message}, PartialReq, State) -> 
  io:format("Received ~p~n", [Message]),
  ok.

