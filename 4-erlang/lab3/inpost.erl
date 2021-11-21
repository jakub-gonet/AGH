-module(inpost).

-export([start/0]).

dist({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2)).

start() ->
    GenPoints = fun (Range, N) ->
                        [{rand:uniform(Range) - 1, rand:uniform(N) - 1}
                         || _ <- lists:seq(1, N), _ <- lists:seq(1, N)]
                end,
    N = 10000 + 1,
    PointsLoc = GenPoints(N, 1000),
    PeopleLoc = GenPoints(N, 20000),
    timer:tc(fun () -> findMinDistance(PeopleLoc, PointsLoc)
             end).

findMinDistance(PeopleLoc, PointsLoc) ->
    lists:min([{dist(X, Y), {X, Y}}
               || X <- PeopleLoc, Y <- PointsLoc]).

findMinDistanceP ( PeopleLoc , PointsLoc ) -> findMinDistanceFor ( PersonLoc , PointsLoc ) -> lists : min ( [ { dist ( PersonLoc , X ) , { PersonLoc , X } } || X <- PointsLoc ] ) .

