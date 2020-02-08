:- use_module(library(http/websocket)).

client :-
  http_open_websocket('ws://localhost:3030/pong', Request, []),
  debug(websocket, "Got ~p~n", [Request]),
  ping(3, Request),
  ws_close(Request, 1000, "Bye").

ping(0, _) :-
  format("Ping finished~n", []).

ping(N, Request) :-
  format(string(Message), "Ping ~d", [N]),
  format("Sent ~p~n", [Message]),
  ws_send(Request, text(Message)),
  ws_receive(Request, Frames),
  format("Received ~p~n", [Frames]),  
  succ(M, N),
  ping(M, Request).

