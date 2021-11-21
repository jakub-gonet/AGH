-module(ping).

-export([play/1, start/0, stop/0]).

play(X) -> ping ! {X, 0}.

start() ->
    register(ping, spawn(fun () -> loop(ping, pong) end)),
    register(pong, spawn(fun () -> loop(pong, ping) end)).

stop() ->
    ping ! stop,
    pong ! stop.

loop(Name, Receiver) ->
    receive
        {N, Acc} when N >= 0 ->
            io:format("~w - N: ~w, acc: ~w.~n", [Name, N, Acc]),
            Receiver ! {N - 1, Acc + N},
            loop(Name, Receiver),
            ok;
        stop ->
            io:format("Stop!~n"),
            ok
        after 20 * 1000 -> io:format("Timeout ~w~n", [Name]), ok
    end.
