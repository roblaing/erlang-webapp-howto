:- use_module(library(http/websocket)).
:- use_module(library(http/http_unix_daemon)).

:- initialization http_daemon.

:- consult('family.pl').

:- http_handler(root(family), http_upgrade_to_websocket(family_handler, []), [spawn([])]).

family_handler(Request) :-
  ws_receive(Request, Message),
  (   Message.opcode == close
  ->  ws_send(Request, close(1000, "normal closure"))
  ;   query(Message.data, Answer),
      ws_send(Request, binary(Answer)),
      family_handler(Request)
  ).

query(QueryString, AnswerString) :-
  term_string(Term, QueryString, [variable_names(VNames)]),
  call(Term),
  last(VNames, Answer),
  term_string(Answer, AnswerString).


