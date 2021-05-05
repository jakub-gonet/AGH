-module(pollution_server).
-behaviour(gen_server).

-export([
    start_link/0,
    crash/0,
    stop/0,
    addStation/2,
    addValue/4,
    getDailyMean/2,
    getMinimumPollutionStation/1,
    getMostActiveStation/1,
    getOneValue/3,
    getStationMean/2,
    removeStation/1,
    removeValue/3
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, pollution:createMonitor()}.
stop() -> gen_server:stop(?MODULE).
crash() -> gen_server:cast(?MODULE, crash).

addStation(Name, Coord) ->
    gen_server:call(?MODULE, {addStation, Name, Coord}).

addValue(StationId, Timestamp, MType, Measurement) ->
    gen_server:call(?MODULE, {addValue, StationId, Timestamp, MType, Measurement}).

getDailyMean(Date, MType) ->
    gen_server:call(?MODULE, {getDailyMean, Date, MType}).

getMinimumPollutionStation(MType) ->
    gen_server:call(?MODULE, {getMinimumPollutionStation, MType}).

getMostActiveStation(Date) ->
    gen_server:call(?MODULE, {getMostActiveStation, Date}).

getOneValue(StationId, Timestamp, MType) ->
    gen_server:call(?MODULE, {getOneValue, StationId, Timestamp, MType}).

getStationMean(StationId, MType) ->
    gen_server:call(?MODULE, {getStationMean, StationId, MType}).

removeStation(StationId) ->
    gen_server:call(?MODULE, {removeStation, StationId}).

removeValue(StationId, Timestamp, MType) ->
    gen_server:call(?MODULE, {removeValue, StationId, Timestamp, MType}).

handle_cast(crash, State) ->
    throw("oh no"),
    "anyway",
    {noreply, State}.

% this could be cleaner by sending {funName, Args} and using apply/3 but it's slooow
% http://erlang.org/doc/efficiency_guide/functions.html#function-calls
handle_call({addStation, Name, Coord}, _From, Monitor) ->
    case pollution:addStation(Name, Coord, Monitor) of
        {error, _} = Error -> {reply, Error, Monitor};
        NewMonitor -> {reply, ok, NewMonitor}
    end;
handle_call({addValue, StationId, Timestamp, MType, Measurement}, _From, Monitor) ->
    case pollution:addValue(StationId, Timestamp, MType, Measurement, Monitor) of
        {error, _} = Error -> {reply, Error, Monitor};
        NewMonitor -> {reply, ok, NewMonitor}
    end;
handle_call({removeStation, StationId}, _From, Monitor) ->
    case pollution:removeStation(StationId, Monitor) of
        {error, _} = Error -> {reply, Error, Monitor};
        NewMonitor -> {reply, ok, NewMonitor}
    end;
handle_call({removeValue, StationId, Timestamp, MType}, _From, Monitor) ->
    case pollution:removeValue(StationId, Timestamp, MType, Monitor) of
        {error, _} = Error -> {reply, Error, Monitor};
        NewMonitor -> {reply, ok, NewMonitor}
    end;
handle_call({getDailyMean, Date, MType}, _From, Monitor) ->
    Reply = pollution:getDailyMean(Date, MType, Monitor),
    {reply, Reply, Monitor};
handle_call({getMinimumPollutionStation, MType}, _From, Monitor) ->
    Reply = pollution:getMinimumPollutionStation(MType, Monitor),
    {reply, Reply, Monitor};
handle_call({getMostActiveStation, Date}, _From, Monitor) ->
    Reply = pollution:getMostActiveStation(Date, Monitor),
    {reply, Reply, Monitor};
handle_call({getOneValue, StationId, Timestamp, MType}, _From, Monitor) ->
    Reply = pollution:getOneValue(StationId, Timestamp, MType, Monitor),
    {reply, Reply, Monitor};
handle_call({getStationMean, StationId, MType}, _From, Monitor) ->
    Reply = pollution:getStationMean(StationId, MType, Monitor),
    {reply, Reply, Monitor}.
