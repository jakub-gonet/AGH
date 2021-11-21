-module(pollution_test).
-include_lib("eunit/include/eunit.hrl").
-include("monitor.hrl").
-include("test_utils.hrl").

addStation_test() ->
    M = pollution:addStation(
        "A",
        {0, 0},
        pollution:createMonitor()
    ),
    ?assertMatch(
        (#monitor{}),
        (pollution:addStation("B", {0, 1}, M))
    ),
    ?assertMatch(
        (#monitor{}),
        (pollution:addStation("", {0, 1}, M))
    ),
    ?assertMatch(
        {error, duplicate_station},
        (pollution:addStation("A", {0, 1}, M))
    ),
    ?assertMatch(
        {error, duplicate_station},
        (pollution:addStation("C", {0, 0}, M))
    ).

addValue_test() ->
    M = pollution:addStation(
        "A",
        {0, 0},
        pollution:createMonitor()
    ),
    {Added, Added2} = {{{2021, 1, 1}, {12, 0, 0}}, {{2021, 1, 1}, {13, 0, 0}}},
    M2 = pollution:addValue("A", Added, "T1", 1, M),
    M3 = pollution:addValue({0, 0}, Added2, "T2", 2, M2),
    ?assertMatch((#monitor{}), M2),
    ?assertMatch((#monitor{}), M3),
    ?assertMatch(
        {error, station_not_found},
        (pollution:addValue(
            "not found",
            calendar:local_time(),
            "T1",
            1,
            M
        ))
    ),
    ?assertMatch(
        {error, duplicate_measurement},
        (pollution:addValue("A", Added, "T1", -1, M2))
    ),
    ?assertMatch(
        {error, duplicate_measurement},
        (pollution:addValue({0, 0}, Added2, "T2", -2, M3))
    ).

removeValue_test() ->
    M0 = pollution:addStation(
        "A",
        {0, 0},
        pollution:createMonitor()
    ),
    Added = {{2021, 1, 1}, {12, 0, 0}},
    M = pollution:addValue("A", Added, "T1", 1, M0),

    ?assertMatch(
        (#monitor{data = #{"A" := #{}}}),
        (pollution:removeValue("A", Added, "T1", M))
    ),
    ?assertMatch(
        (#monitor{data = #{"A" := #{}}}),
        (pollution:removeValue({0, 0}, Added, "T1", M))
    ),
    ?assertMatch(
        {error, station_not_found},
        (pollution:removeValue("X", Added, "T1", M))
    ),
    ?assertMatch(
        {error, measurement_not_found},
        (pollution:removeValue("A", Added, "T2", M))
    ).

getOneValue_test() ->
    M0 = pollution:addStation(
        "A",
        {0, 0},
        pollution:createMonitor()
    ),
    Added = {{2021, 1, 1}, {12, 0, 0}},
    M = pollution:addValue("A", Added, "T1", 1, M0),

    ?assertMatch(
        1,
        (pollution:getOneValue("A", Added, "T1", M))
    ),
    ?assertMatch(
        1,
        (pollution:getOneValue({0, 0}, Added, "T1", M))
    ),
    ?assertMatch(
        {error, station_not_found},
        (pollution:getOneValue("B", Added, "T1", M))
    ),
    ?assertMatch(
        {error, measurement_not_found},
        (pollution:getOneValue(
            "A",
            calendar:local_time(),
            "T1",
            M
        ))
    ),
    ?assertMatch(
        {error, measurement_not_found},
        (pollution:getOneValue("A", Added, "T2", M))
    ).

getStationMean_test() ->
    M0 = pollution:addStation(
        "A",
        {0, 0},
        pollution:createMonitor()
    ),
    Added = {{2021, 1, 1}, {12, 0, 0}},
    M1 = pollution:addValue("A", Added, "T1", 1, M0),
    Added2 = {{2021, 1, 1}, {13, 0, 0}},
    M2 = pollution:addValue("A", Added2, "T1", 3, M1),
    M = pollution:addValue("A", Added, "T2", 3, M2),

    ?assertWithDelta(
        2.0,
        (pollution:getStationMean("A", "T1", M))
    ),
    ?assertWithDelta(
        3.0,
        (pollution:getStationMean("A", "T2", M))
    ),
    ?assertMatch(
        {error, no_measurements},
        (pollution:getStationMean("A", "T3", M))
    ).

getDailyMean_test() ->
    MS0 = pollution:addStation(
        "A",
        {0, 0},
        pollution:createMonitor()
    ),
    MS1 = pollution:addStation("B", {0, 1}, MS0),
    MS2 = pollution:addStation("C", {0, 2}, MS1),
    MS3 = pollution:addStation("D", {0, 3}, MS2),
    Added = {{2021, 1, 1}, {12, 0, 0}},
    AddedLater = {{2021, 1, 1}, {15, 0, 0}},
    M1 = pollution:addValue("A", Added, "T1", 1, MS3),
    M2 = pollution:addValue("B", Added, "T2", 100, M1),
    M3 = pollution:addValue("C", AddedLater, "T1", 11, M2),
    AddedNextDay = {{2021, 1, 2}, {18, 0, 0}},
    M = pollution:addValue("D", AddedNextDay, "T1", 4, M3),

    ?assertWithDelta(
        6.0,
        (pollution:getDailyMean(Added, "T1", M))
    ),
    ?assertWithDelta(
        1.0e+2,
        (pollution:getDailyMean(Added, "T2", M))
    ),
    ?assertWithDelta(
        4.0,
        (pollution:getDailyMean(AddedNextDay, "T1", M))
    ),
    ?assertMatch(
        {error, no_measurements},
        (pollution:getDailyMean(Added, "T3", M))
    ),
    ?assertMatch(
        {error, no_measurements},
        (pollution:getDailyMean(
            {{2022, 6, 6}, {18, 0, 0}},
            "T1",
            M
        ))
    ).

removeStation_test() ->
    M = pollution:addStation(
        "A",
        {0, 0},
        pollution:createMonitor()
    ),
    ?assertMatch((#monitor{data = #{"A" := _}}), M),
    M2 = pollution:removeStation("A", M),
    M3 = pollution:removeStation({0, 0}, M),
    ?assertNotMatch((#monitor{data = #{"A" := _}}), M2),
    ?assertNotMatch((#monitor{data = #{"A" := _}}), M3),
    ?assertMatch(
        {error, station_not_found},
        (pollution:removeStation(
            "X",
            pollution:createMonitor()
        ))
    ),
    ?assertMatch(
        {error, station_not_found},
        (pollution:removeStation(
            {-1, -1},
            pollution:createMonitor()
        ))
    ).

getMinimumPollutionStation_test() ->
    Added = {{2021, 1, 1}, {12, 0, 0}},
    Added2 = {{2021, 6, 1}, {13, 0, 0}},
    Added3 = {{2022, 1, 1}, {14, 0, 0}},
    Added4 = {{2021, 7, 1}, {16, 0, 0}},
    MA = pollution:addStation(
        "A",
        {0, 0},
        pollution:createMonitor()
    ),
    MB = pollution:addStation("B", {0, 1}, MA),
    MC = pollution:addStation("C", {0, 2}, MB),
    MD = pollution:addStation("D", {0, 3}, MC),
    MA1 = pollution:addValue("A", Added, "T1", 21, MD),
    MA2 = pollution:addValue("A", Added2, "T1", 21, MA1),
    MA3 = pollution:addValue("A", Added3, "T1", 30, MA2),
    MA4 = pollution:addValue("A", Added4, "T2", -100, MA3),

    MB1 = pollution:addValue("B", Added, "T1", 18, MA4),
    MB2 = pollution:addValue("B", Added2, "T1", 20, MB1),
    MB3 = pollution:addValue("B", Added3, "T1", 6, MB2),

    MC1 = pollution:addValue("C", Added, "T2", 5, MB3),
    MC2 = pollution:addValue("C", Added2, "T1", 1, MC1),

    % D - no measurements
    % empty
    ?assertMatch(
        {error, no_measurements},
        (pollution:getMinimumPollutionStation(
            "T1",
            pollution:createMonitor()
        ))
    ),
    % other type
    ?assertMatch(
        (-100),
        (pollution:getMinimumPollutionStation("T2", MC2))
    ),
    % basic
    ?assertMatch(
        1,
        (pollution:getMinimumPollutionStation("T1", MC2))
    ),
    % undefined type
    ?assertMatch(
        {error, no_measurements},
        (pollution:getMinimumPollutionStation("T3", MC2))
    ).

getMostActiveStation_test() ->
    Added = {{2021, 1, 1}, {12, 0, 0}},
    Added2 = {{2021, 1, 1}, {13, 0, 0}},
    Added3 = {{2021, 1, 1}, {14, 0, 0}},
    Added4 = {{2021, 1, 1}, {16, 0, 0}},
    Added5 = {{2021, 1, 2}, {16, 0, 0}},
    Added6 = {{2021, 1, 2}, {17, 0, 0}},
    MA = pollution:addStation(
        "A",
        {0, 0},
        pollution:createMonitor()
    ),
    MB = pollution:addStation("B", {0, 1}, MA),
    MC = pollution:addStation("C", {0, 2}, MB),
    MD = pollution:addStation("D", {0, 3}, MC),

    MA1 = pollution:addValue("A", Added, "T1", 21, MD),
    MA2 = pollution:addValue("A", Added2, "T1", 21, MA1),
    MA3 = pollution:addValue("A", Added3, "T3", 30, MA2),
    MA4 = pollution:addValue("A", Added4, "T2", -100, MA3),

    MB1 = pollution:addValue("B", Added, "T1", 18, MA4),
    MB2 = pollution:addValue("B", Added2, "T1", 20, MB1),
    MB3 = pollution:addValue("B", Added3, "T1", 6, MB2),
    MB4 = pollution:addValue("B", Added4, "T1", 6, MB3),
    MB5 = pollution:addValue("B", Added5, "T1", 6, MB4),

    MC1 = pollution:addValue("C", Added5, "T2", 5, MB5),
    MC2 = pollution:addValue("C", Added6, "T1", 1, MC1),

    % tie
    ?assert(
        (lists:sort(["A", "B"]) ==
            lists:sort(
                pollution:getMostActiveStation(
                    {{2021, 1, 1}, {20, 0, 0}},
                    MC2
                )
            ))
    ),
    % single winner
    ?assertMatch(
        ["C"],
        (pollution:getMostActiveStation(
            {{2021, 1, 2}, {20, 0, 0}},
            MC2
        ))
    ),
    % no activity
    ?assertMatch(
        [],
        (pollution:getMostActiveStation(
            {{2021, 6, 1}, {20, 0, 0}},
            MC2
        ))
    ).
