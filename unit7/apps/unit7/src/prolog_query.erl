-module(prolog_query).
-export([client/0, pl_query/3]).

client() ->
  {ok, Pid} = gun:open("localhost", 3031),
  {ok, _} = gun:await_up(Pid),
  StreamRef = gun:ws_upgrade(Pid, "/family"),
  {upgrade, [<<"websocket">>], _} = gun:await(Pid, StreamRef),
  Q1 = <<"findall(Parent, parent(Parent, bob), Parents)">>,
  io:format("Asked ~p~n", [Q1]),
  io:format("Answer ~p~n", [pl_query(Q1, Pid, StreamRef)]),
  Q2 = <<"findall(Child, parent(bob, Child), Children)">>,
  io:format("Asked ~p~n", [Q2]),
  io:format("Answer ~p~n", [pl_query(Q2, Pid, StreamRef)]),
  gun:shutdown(Pid).

pl_query(Prolog, Pid, StreamRef) ->
  gun:ws_send(Pid, [{binary, Prolog}]),
  {ws, {binary, Answer}} = gun:await(Pid, StreamRef),
  Answer.
