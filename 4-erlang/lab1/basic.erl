-module(basic).
-export([start/0]).


start() ->
  P1 = {"id: 1", 123856, [{pm2, 0.2}, {temp, 13.6}]},
  P2 = {"id: 1", 123516, [{pm2, 0.3}, {temp, 21.6}]},
  P3 = {"id: 2", 126356, [{pm10, 0.5}, {temp, 33.6}]},
  T = [P1, P2, P3],
  P4 = {"id: 4", 126356, [{pm10, 0.5}, {temp, 33.6}]},
  T2  = [P4 | T],
  {P1Name, _, _} = P1,
  io:fwrite("~p", [list_to_tuple(T2)]).
