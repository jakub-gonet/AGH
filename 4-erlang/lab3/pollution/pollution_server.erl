-module(pollution_server).

-export([
    addStation/2,
    addValue/4,
    getDailyMean/2,
    getMinimumPollutionStation/1,
    getMostActiveStation/1,
    getOneValue/3,
    getStationMean/2,
    removeStation/1,
    removeValue/3,
    start/0,
    stop/0
]).

addStation(Name, Coord) ->
    call({addStation, Name, Coord}).

addValue(StationId, Timestamp, MType, Measurement) ->
    call({addValue, StationId, Timestamp, MType, Measurement}).

getDailyMean(Date, MType) ->
    call({getDailyMean, Date, MType}).

getMinimumPollutionStation(MType) ->
    call({getMinimumPollutionStation, MType}).

getMostActiveStation(Date) ->
    call({getMostActiveStation, Date}).

getOneValue(StationId, Timestamp, MType) ->
    call({getOneValue, StationId, Timestamp, MType}).

getStationMean(StationId, MType) ->
    call({getStationMean, StationId, MType}).

removeStation(StationId) ->
    call({removeStation, StationId}).

removeValue(StationId, Timestamp, MType) ->
    call({removeValue, StationId, Timestamp, MType}).

call(Req) ->
    pollution_server ! {call, self(), Req},
    receive
        {pollution_server, Res} ->
            Res
    end.

start() ->
    spawn(fun init/0).
init() ->
    register(pollution_server, self()),
    Monitor = pollution:createMonitor(),
    loop(Monitor).

loop(Monitor) ->
    receive
        {call, From, Req} ->
            {Res, NewMonitor} = handleCall(Req, Monitor),
            From ! {pollution_server, Res},
            loop(NewMonitor);
        stop ->
            ok
    end.

stop() ->
    pollution_server ! stop,
    ok.

updateOnSuccess({error, _} = E, Monitor) ->
    {E, Monitor};
updateOnSuccess(NewMonitor, _) ->
    {ok, NewMonitor}.

% this could be cleaner by sending {funName, Args} and using apply/3 but it's slooow
% http://erlang.org/doc/efficiency_guide/functions.html#function-calls
handleCall({addStation, Name, Coord}, Monitor) ->
    updateOnSuccess(pollution:addStation(Name, Coord, Monitor), Monitor);
handleCall({addValue, StationId, Timestamp, MType, Measurement}, Monitor) ->
    updateOnSuccess(pollution:addValue(StationId, Timestamp, MType, Measurement, Monitor), Monitor);
handleCall({getDailyMean, Date, MType}, Monitor) ->
    {pollution:getDailyMean(Date, MType, Monitor)};
handleCall({getMinimumPollutionStation, MType}, Monitor) ->
    {pollution:getMinimumPollutionStation(MType, Monitor), Monitor};
handleCall({getMostActiveStation, Date}, Monitor) ->
    {pollution:getMostActiveStation(Date, Monitor), Monitor};
handleCall({getOneValue, StationId, Timestamp, MType}, Monitor) ->
    {pollution:getOneValue(StationId, Timestamp, MType, Monitor), Monitor};
handleCall({getStationMean, StationId, MType}, Monitor) ->
    {pollution:getStationMean(StationId, MType, Monitor), Monitor};
handleCall({removeStation, StationId}, Monitor) ->
    updateOnSuccess(pollution:removeStation(StationId, Monitor), Monitor);
handleCall({removeValue, StationId, Timestamp, MType}, Monitor) ->
    updateOnSuccess(pollution:removeValue(StationId, Timestamp, MType, Monitor), Monitor).
