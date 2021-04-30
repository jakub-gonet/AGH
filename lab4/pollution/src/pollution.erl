-module(pollution).

-export([
    addStation/3,
    addValue/5,
    createMonitor/0,
    getDailyMean/3,
    getMinimumPollutionStation/2,
    getMostActiveStation/2,
    getOneValue/4,
    getStationMean/3,
    removeStation/2,
    removeValue/4
]).

-include("monitor.hrl").

createMonitor() ->
    #monitor{}.

addStation(Name, Coord, #monitor{data = D, coordToName = C}) when
    is_map_key(Name, D); is_map_key(Coord, C)
->
    {error, duplicate_station};
addStation(Name, Coord, #monitor{coordToName = C, data = D}) ->
    #monitor{coordToName = C#{Coord => Name}, data = D#{Name => #{}}}.

addValue(StationId, Timestamp, MType, Measurement, Monitor) ->
    updateStationData(
        StationId,
        fun(StationData) ->
            case maps:is_key({Timestamp, MType}, StationData) of
                false ->
                    StationData#{{Timestamp, MType} => Measurement};
                true ->
                    {error, duplicate_measurement}
            end
        end,
        Monitor
    ).

removeValue(StationId, Timestamp, MType, Monitor) ->
    updateStationData(
        StationId,
        fun(StationData) ->
            case maps:is_key({Timestamp, MType}, StationData) of
                true ->
                    maps:remove({Timestamp, MType}, StationData);
                false ->
                    {error, measurement_not_found}
            end
        end,
        Monitor
    ).

getOneValue(StationId, Timestamp, MType, Monitor) ->
    runOnStationData(
        StationId,
        fun({_, StationData}) ->
            maps:get({Timestamp, MType}, StationData, {error, measurement_not_found})
        end,
        Monitor
    ).

getStationMean(StationId, MType, Monitor) ->
    TypeFilter = fun({_, T}) -> T == MType end,
    runOnStationData(
        StationId,
        fun({_, StationData}) -> mean(getMeasurements(TypeFilter, StationData)) end,
        Monitor
    ).

getDailyMean({Date, _}, MType, #monitor{data = Data}) ->
    DayAndTypeFilter = fun({{D, _}, T}) -> T == MType andalso D == Date end,
    mean(
        getMeasurements(
            DayAndTypeFilter,
            lists:flatmap(
                fun maps:to_list/1,
                % running function working on one measurement for
                maps:values(Data)
            )
        )
    ).

% all measurements looks a bit nicer than flatmapping bunch of data

% suprise assignment

getMinimumPollutionStation(MType, #monitor{data = D} = M) ->
    TypeFilter = fun({_, T}) -> T == MType end,
    MinOnStation =
        fun(StationId) ->
            runOnStationData(
                StationId,
                fun({_, StationData}) -> min(getMeasurements(TypeFilter, StationData)) end,
                M
            )
        end,
    min([Min || Min <- runOnAllStations(MinOnStation, D), Min /= {error, no_measurements}]).

% extensions

% Removes station from monitoring.
%
% Returns updated nonitor if station exists, {error, station_not_found} otherwise.
removeStation(StationId, #monitor{coordToName = C, data = D} = M) ->
    case getCoordsFrom(StationId, M) of
        {error, _} = E ->
            E;
        {ok, Coord} ->
            {ok, Name} = getNameFrom(Coord, M),
            #monitor{coordToName = maps:remove(Coord, C), data = maps:remove(Name, D)}
    end.

% Finds a station with most datapoints per given day.
%
% Returns list with found station (or multiple stations in case of ties) or empty list otherwise.

getMostActiveStation({Date, _}, #monitor{data = D} = M) ->
    MeasuredOnDay =
        fun(StationData) ->
            getMeasurements(
                fun
                    ({{Day, _Hour}, _Type}) when Day == Date ->
                        true;
                    (_) ->
                        false
                end,
                StationData
            )
        end,
    GetLength =
        fun(StationId) ->
            runOnStationData(
                StationId,
                fun({Name, StationData}) -> {Name, length(MeasuredOnDay(StationData))} end,
                M
            )
        end,
    WithMeasurements = [X || {_, Num} = X <- runOnAllStations(GetLength, D), Num /= 0],
    [Name || {Name, _} <- minWithTies(WithMeasurements, fun({_, V}) -> V end)].

% helpers

runOnAllStations(F, Data) ->
    lists:map(F, maps:keys(Data)).

getMeasurements(Predicate, StationData) when is_map(StationData) ->
    getMeasurements(Predicate, maps:to_list(StationData));
getMeasurements(Predicate, StationData) ->
    F = fun({Key, Measurement}) ->
        case Predicate(Key) of
            true ->
                {true, Measurement};
            false ->
                false
        end
    end,
    lists:filtermap(F, StationData).

minWithTies([], _) ->
    [];
minWithTies(List, Extractor) ->
    Compare =
        fun(Min, X) ->
            V = Extractor(X),
            MinV = Extractor(Min),
            MinV - V
        end,
    lists:foldl(
        fun
            (X, []) ->
                [X];
            (X, [H | _] = Mins) ->
                case Compare(H, X) of
                    Cmp when Cmp == 0 ->
                        [X | Mins];
                    Cmp when Cmp < 0 ->
                        [X];
                    _ ->
                        Mins
                end
        end,
        [],
        List
    ).

min([]) ->
    {error, no_measurements};
min(Measurements) ->
    lists:min(Measurements).

mean([]) ->
    {error, no_measurements};
mean(Measurements) ->
    lists:sum(Measurements) / length(Measurements).

updateStationData(StationId, UpdateF, #monitor{data = D} = M) ->
    runOnStationData(
        StationId,
        fun({Name, StationData}) ->
            case UpdateF(StationData) of
                {error, _} = E ->
                    E;
                Updated ->
                    M#monitor{data = D#{Name => Updated}}
            end
        end,
        M
    ).

runOnStationData(StationId, F, #monitor{data = D} = M) ->
    case getNameFrom(StationId, M) of
        {error, _} = E ->
            E;
        {ok, Name} ->
            F({Name, maps:get(Name, D)})
    end.

getNameFrom(StationId, #monitor{data = D, coordToName = C}) when
    not is_map_key(StationId, D), not is_map_key(StationId, C)
->
    {error, station_not_found};
getNameFrom(Name, _) when is_list(Name) ->
    {ok, Name};
getNameFrom(Coord, #monitor{coordToName = C}) when is_tuple(Coord) ->
    {ok, maps:get(Coord, C)}.

% Linear search in all stations - used only in station removal, number of stations should be small.
% If that's not the case then add reverse mapping in monitor and reimplement this function
getCoordsFrom(StationId, #monitor{data = D, coordToName = C}) when
    not is_map_key(StationId, D), not is_map_key(StationId, C)
->
    {error, station_not_found};
getCoordsFrom(Coord, _) when is_tuple(Coord) ->
    {ok, Coord};
getCoordsFrom(Name, #monitor{coordToName = C}) when is_list(Name) ->
    {value, {Coord, _}} = lists:search(fun({_, N}) -> N == Name end, maps:to_list(C)),
    {ok, Coord}.
