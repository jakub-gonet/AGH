-module(pollution_server_test).
-include_lib("eunit/include/eunit.hrl").
-include("test_utils.hrl").

addSomeValues_test() ->
    Added = {{2021, 1, 1}, {12, 0, 0}},

    pollution_server:start_link(),
    timer:sleep(200),
    ?assertMatch(ok, pollution_server:addStation("A", {0, 0})),
    ?assertMatch(ok, pollution_server:addValue("A", Added, "T1", 1)),
    % not existing station
    ?assertMatch(
        {error, station_not_found},
        pollution_server:addValue("X", Added, "T1", 1)
    ),
    % add second time first measurement
    ?assertMatch(
        {error, duplicate_measurement},
        pollution_server:addValue("A", Added, "T1", 1)
    ),
    pollution_server:stop().

addAndGetSomeValues_test() ->
    Added = {{2021, 1, 1}, {12, 0, 0}},
    Added2 = {{2021, 2, 1}, {12, 0, 0}},

    pollution_server:start_link(),
    timer:sleep(200),
    ?assertMatch(ok, pollution_server:addStation("A", {0, 0})),
    ?assertMatch(ok, pollution_server:addValue("A", Added, "T1", 1)),
    ?assertMatch(ok, pollution_server:addValue("A", Added2, "T1", 3)),

    ?assertWithDelta(2.0, pollution_server:getStationMean("A", "T1")),
    ?assertMatch(3, pollution_server:getOneValue({0, 0}, Added2, "T1")),

    pollution_server:stop().
