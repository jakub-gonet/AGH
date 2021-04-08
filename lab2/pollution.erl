-module(pollution).
-include_lib("eunit/include/eunit.hrl").
-export([createMonitor/0,
          addStation/3,
          addValue/5,
          removeValue/4,
          getOneValue/4,
          getStationMean/3,
          getDailyMean/3,
          getMinimumPollutionStation/2,
          removeStation/2,
          getMostActiveStation/2
          ]).
-define(assertWithDelta(V, Assertion), ?assertWithDelta(V, Assertion, 1.0e-6)).
-define(assertWithDelta(V, Assertion, Delta), ?assertMatch(X when abs(X - V) < Delta, Assertion)).

% We used direct mapping from names to measurement data here and additional map used to translate coordinates to names
% This allows matching duplicate name or coords easily
%
% Measurements data has following shape: #{{Date, Type} => Value}
% which prevents adding measurement with same date and type within given station
-record(monitor, {coordToName = #{}, data = #{}}).

createMonitor() -> #monitor{}.

addStation(Name, Coord, #monitor{data=D, coordToName=C})
    when is_map_key(Name, D); is_map_key(Coord, C) ->
        {error, duplicate_station};
addStation(Name, Coord, #monitor{coordToName=C, data=D}) ->
    #monitor{
            coordToName = C#{Coord => Name},
            data = D#{Name => #{}}
    }.

addValue(StationId, Timestamp, MType, Measurement, Monitor) ->
    updateStationData(StationId,
        fun (StationData) ->
            case maps:is_key({Timestamp, MType}, StationData) of
                false -> StationData#{{Timestamp, MType} => Measurement};
                true -> {error, duplicate_measurement}
            end
        end, Monitor).

removeValue(StationId, Timestamp, MType, Monitor) ->
    updateStationData(StationId,
        fun (StationData) ->
            case maps:is_key({Timestamp, MType}, StationData) of
                true -> maps:remove({Timestamp, MType}, StationData);
                false -> {error, measurement_not_found}
            end
        end, Monitor).

getOneValue(StationId, Timestamp, MType, Monitor) ->
    runOnStationData(StationId,
                fun ({_, StationData}) ->
                    maps:get({Timestamp, MType}, StationData,
                            {error, measurement_not_found})
                end,
                Monitor).

getStationMean(StationId, MType, Monitor) ->
    TypeFilter = fun ({_, T}) -> T == MType end,
    runOnStationData(
        StationId,
        fun ({_, StationData}) ->
            mean(getMeasurements(TypeFilter, StationData))
        end,
        Monitor).

getDailyMean({Date, _}, MType, #monitor{data = Data}) ->
    DayAndTypeFilter = fun ({{D, _}, T}) ->
        T == MType andalso D == Date end,
    mean(
        getMeasurements(DayAndTypeFilter, lists:flatmap(fun maps:to_list/1, maps:values(Data))) % running function working on one measurement for
                                                                                                % all measurements looks a bit nicer than flatmapping bunch of data
    ).

% suprise assignment

getMinimumPollutionStation(MType, #monitor{data=D} = M) ->
    TypeFilter = fun ({_, T}) -> T == MType end,
    MinOnStation = fun (StationId) ->
        runOnStationData(StationId,
            fun({_, StationData}) ->
                min(getMeasurements(TypeFilter, StationData))
            end, M)
    end,
    min([Min || Min <- runOnAllStations(MinOnStation, D), Min /= {error, no_measurements}]).

% extensions

% Removes station from monitoring.
%
% Returns updated nonitor if station exists, {error, station_not_found} otherwise.
removeStation(StationId, #monitor{coordToName=C, data=D} = M) ->
    case getCoordsFrom(StationId, M) of
        {error, _} = E -> E;
        {ok, Coord} ->
            {ok, Name} = getNameFrom(Coord, M),
            #monitor{
                coordToName = maps:remove(Coord, C),
                data = maps:remove(Name,D)
            }
    end.

% Finds a station with most datapoints per given day.
%
% Returns list with found station (or multiple stations in case of ties) or empty list otherwise.

getMostActiveStation({Date,_}, #monitor{data=D}=M) ->
    MeasuredOnDay = fun(StationData) ->
        getMeasurements(
                fun({{Day, _Hour}, _Type}) when Day == Date -> true;
                (_) -> false end,
            StationData)
    end,
    GetLength = fun (StationId) ->
        runOnStationData(StationId,
             fun ({Name, StationData}) -> {Name, length(MeasuredOnDay(StationData))} end,
            M)
        end,
    WithMeasurements = [X || {_, Num} = X <- runOnAllStations(GetLength, D), Num /= 0],
    [Name || {Name, _} <- minWithTies(WithMeasurements, fun ({_, V}) -> V end)].

% helpers

runOnAllStations(F, Data) -> lists:map(F, maps:keys(Data)).

getMeasurements(Predicate, StationData) when is_map(StationData) ->
    getMeasurements(Predicate, maps:to_list(StationData));
getMeasurements(Predicate, StationData) ->
    F = fun ({Key, Measurement}) ->
            case Predicate(Key) of
                true -> {true, Measurement};
                false -> false
            end
        end,
    lists:filtermap(F, StationData).

minWithTies([], _) -> [];
minWithTies(List, Extractor) ->
    Compare = fun (Min, X) ->
        V = Extractor(X),
        MinV = Extractor(Min),
        MinV - V
    end,
    lists:foldl(fun (X, []) -> [X];
                    (X, [H | _] = Mins) ->
                    case Compare(H, X) of
                        Cmp when Cmp == 0 -> [X | Mins];
                        Cmp when Cmp < 0 -> [X];
                        _ -> Mins
                    end
                end,
                [], List).

min([]) -> {error, no_measurements};
min(Measurements) -> lists:min(Measurements).

mean([]) -> {error, no_measurements};
mean(Measurements) -> lists:sum(Measurements) / length(Measurements).

updateStationData(StationId, UpdateF, #monitor{data=D} = M) ->
    runOnStationData(StationId,
        fun ({Name, StationData}) ->
            case UpdateF(StationData) of
                {error, _} = E -> E;
                Updated -> M#monitor{data = D#{ Name => Updated }}
            end
        end,
        M).

runOnStationData(StationId, F, #monitor{data=D} = M) ->
    case getNameFrom(StationId, M) of
        {error, _} = E -> E;
        {ok, Name} -> F({Name, maps:get(Name, D)})
    end.

getNameFrom(StationId, #monitor{data = D, coordToName=C})
    when not is_map_key(StationId, D), not is_map_key(StationId, C) ->
        {error, station_not_found};
getNameFrom(Name, _) when is_list(Name) -> {ok, Name};
getNameFrom(Coord, #monitor{coordToName=C})
    when is_tuple(Coord) ->
        {ok, maps:get(Coord, C)}.

% Linear search in all stations - used only in station removal, number of stations should be small.
% If that's not the case then add reverse mapping in monitor and reimplement this function
getCoordsFrom(StationId, #monitor{data=D, coordToName=C})
    when not is_map_key(StationId, D), not is_map_key(StationId, C) ->
        {error, station_not_found};
getCoordsFrom(Coord, _) when is_tuple(Coord) -> {ok, Coord};
getCoordsFrom(Name, #monitor{coordToName=C})
    when is_list(Name) ->
        {value, {Coord, _}} =
            lists:search(
                fun ({_, N}) -> N == Name end,
                maps:to_list(C)),
        {ok, Coord}.

% tests

addStation_test() ->
  M = addStation("A", {0,0}, createMonitor()),

  ?assertMatch(#monitor{}, addStation("B", {0,1}, M)),
  ?assertMatch(#monitor{}, addStation("", {0,1}, M)),
  ?assertMatch({error, duplicate_station},
                addStation("A", {0,1}, M)),
  ?assertMatch({error, duplicate_station},
                addStation("C", {0,0}, M)).

addValue_test() ->
  M = addStation("A", {0,0}, createMonitor()),
  {Added, Added2} = {{{2021,1,1}, {12,0,0}}, {{2021,1,1}, {13,0,0}}},

  M2 = addValue("A", Added, "T1", 1, M),
  M3 = addValue({0,0}, Added2, "T2", 2, M2),
  ?assertMatch(#monitor{}, M2),
  ?assertMatch(#monitor{}, M3),
  ?assertMatch({error, station_not_found},
                addValue("not found", calendar:local_time(), "T1", 1, M)),
  ?assertMatch({error, duplicate_measurement},
                addValue("A", Added, "T1", -1, M2)),
  ?assertMatch({error, duplicate_measurement},
                addValue({0,0}, Added2, "T2", -2, M3)).

removeValue_test() ->
    M0 = addStation("A", {0,0}, createMonitor()),
    Added = {{2021,1,1}, {12,0,0}},
    M = addValue("A", Added, "T1", 1, M0),

  ?assertMatch(#monitor{data = #{"A" := #{}}},
                removeValue("A", Added, "T1", M)),
  ?assertMatch(#monitor{data = #{"A" := #{}}},
                removeValue({0,0}, Added, "T1", M)),
  ?assertMatch({error, station_not_found},
                removeValue("X", Added, "T1", M)),
  ?assertMatch({error, measurement_not_found},
                removeValue("A", Added, "T2", M)).

getOneValue_test() ->
    M0 = addStation("A", {0,0}, createMonitor()),
    Added = {{2021,1,1}, {12,0,0}},
    M = addValue("A", Added, "T1", 1, M0),

    ?assertMatch(1, getOneValue("A", Added, "T1", M)),
    ?assertMatch(1, getOneValue({0,0}, Added, "T1", M)),
    ?assertMatch({error, station_not_found},
                getOneValue("B", Added, "T1", M)),
    ?assertMatch({error, measurement_not_found},
                getOneValue("A", calendar:local_time(), "T1", M)),
    ?assertMatch({error, measurement_not_found},
                getOneValue("A", Added, "T2", M)).

getStationMean_test() ->
    M0 = addStation("A", {0,0}, createMonitor()),
    Added = {{2021,1,1}, {12,0,0}},
    M1 = addValue("A", Added, "T1", 1, M0),
    Added2 = {{2021,1,1}, {13,0,0}},
    M2 = addValue("A", Added2, "T1", 3, M1),
    M = addValue("A", Added, "T2", 3, M2),

    ?assertWithDelta(2.0 , getStationMean("A", "T1", M)),
    ?assertWithDelta(3.0 , getStationMean("A", "T2", M)),
    ?assertMatch({error, no_measurements},
                getStationMean("A", "T3", M)).

getDailyMean_test() ->
    MS0 = addStation("A", {0,0}, createMonitor()),
    MS1 = addStation("B", {0,1}, MS0),
    MS2 = addStation("C", {0,2}, MS1),
    MS3 = addStation("D", {0,3}, MS2),
    Added = {{2021,1,1}, {12,0,0}},
    AddedLater = {{2021, 1, 1}, {15,0,0}},
    M1 = addValue("A", Added, "T1", 1, MS3),
    M2 = addValue("B", Added, "T2", 100, M1),
    M3 = addValue("C", AddedLater, "T1", 11, M2),
    AddedNextDay = {{2021, 1, 2}, {18,0,0}},
    M = addValue("D", AddedNextDay, "T1", 4, M3),

    ?assertWithDelta(6.0, getDailyMean(Added, "T1", M)),
    ?assertWithDelta(100.0, getDailyMean(Added, "T2", M)),
    ?assertWithDelta(4.0, getDailyMean(AddedNextDay, "T1", M)),
    ?assertMatch({error, no_measurements},
                getDailyMean(Added, "T3", M)),
    ?assertMatch({error, no_measurements},
                getDailyMean({{2022,6,6}, {18,0,0}}, "T1", M)).

removeStation_test() ->
    M = addStation("A", {0,0}, createMonitor()),

    ?assertMatch(#monitor{data=#{"A" := _}}, M),
    M2 = removeStation("A", M),
    M3 = removeStation({0,0}, M),
    ?assertNotMatch(#monitor{data=#{"A" := _}}, M2),
    ?assertNotMatch(#monitor{data=#{"A" := _}}, M3),
    ?assertMatch({error, station_not_found}, removeStation("X", createMonitor())),
    ?assertMatch({error, station_not_found}, removeStation({-1, -1}, createMonitor())).

getMinimumPollutionStation_test() ->
    Added = {{2021,1,1}, {12,0,0}},
    Added2 = {{2021,6,1}, {13,0,0}},
    Added3 = {{2022,1,1}, {14,0,0}},
    Added4 = {{2021,7,1}, {16,0,0}},
    MA = addStation("A", {0,0}, createMonitor()),
    MB = addStation("B", {0,1}, MA),
    MC = addStation("C", {0,2}, MB),
    MD = addStation("D", {0,3}, MC),
    MA1 = addValue("A", Added, "T1", 21, MD),
    MA2 = addValue("A", Added2, "T1", 21, MA1),
    MA3 = addValue("A", Added3, "T1", 30, MA2),
    MA4 = addValue("A", Added4, "T2", -100, MA3),

    MB1 = addValue("B", Added, "T1", 18, MA4),
    MB2 = addValue("B", Added2, "T1", 20, MB1),
    MB3 = addValue("B", Added3, "T1", 6, MB2),

    MC1 = addValue("C", Added, "T2", 5, MB3),
    MC2 = addValue("C", Added2, "T1", 1, MC1),

    % D - no measurements

    % empty
    ?assertMatch({error, no_measurements},
        getMinimumPollutionStation("T1", createMonitor())),
    % other type
    ?assertMatch(-100,
        getMinimumPollutionStation("T2", MC2)),
    % basic
    ?assertMatch(1,
        getMinimumPollutionStation("T1", MC2)),
    % undefined type
    ?assertMatch({error, no_measurements},
        getMinimumPollutionStation("T3", MC2)).

getMostActiveStation_test() ->
    Added = {{2021,1,1}, {12,0,0}},
    Added2 = {{2021,1,1}, {13,0,0}},
    Added3 = {{2021,1,1}, {14,0,0}},
    Added4 = {{2021,1,1}, {16,0,0}},
    Added5 = {{2021,1,2}, {16,0,0}},
    Added6 = {{2021,1,2}, {17,0,0}},
    MA = addStation("A", {0,0}, createMonitor()),
    MB = addStation("B", {0,1}, MA),
    MC = addStation("C", {0,2}, MB),
    MD = addStation("D", {0,3}, MC),

    MA1 = addValue("A", Added, "T1", 21, MD),
    MA2 = addValue("A", Added2, "T1", 21, MA1),
    MA3 = addValue("A", Added3, "T3", 30, MA2),
    MA4 = addValue("A", Added4, "T2", -100, MA3),

    MB1 = addValue("B", Added, "T1", 18, MA4),
    MB2 = addValue("B", Added2, "T1", 20, MB1),
    MB3 = addValue("B", Added3, "T1", 6, MB2),
    MB4 = addValue("B", Added4, "T1", 6, MB3),
    MB5 = addValue("B", Added5, "T1", 6, MB4),

    MC1 = addValue("C", Added5, "T2", 5, MB5),
    MC2 = addValue("C", Added6, "T1", 1, MC1),

    % tie
    ?assert(lists:sort(["A", "B"]) ==
        lists:sort(getMostActiveStation({{2021,1,1}, {20,0,0}}, MC2))),
    % single winner
    ?assertMatch(["C"],
        getMostActiveStation({{2021,1,2}, {20,0,0}}, MC2)),
    % no activity
    ?assertMatch([],
        getMostActiveStation({{2021,6,1}, {20,0,0}}, MC2)).