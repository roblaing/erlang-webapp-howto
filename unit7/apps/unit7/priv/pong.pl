:- use_module(library(http/websocket)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

:- http_handler(root(pong), http_upgrade_to_websocket(pong_handler, []), [spawn([])]).

server(Port) :-
  http_server(http_dispatch, [port(Port)]).

pong_handler(Request) :-
  ws_receive(Request, Message),
  debug(websocket, "Got ~p~n", [Message]),
  format("Received ~p~n", [Message.data]),
  ws_send(Request, text("Pong")),
  (   Message.opcode == close
  ->  format("Pong finished~n")
  ;   format("Sent Pong~n"),
      pong_handler(Request)
  ).

