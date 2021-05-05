-module(pollution_server).
-behaviour(gen_server).

-export([
    start_link/0,
    crash/0,
    stop/0,
    add_station/2,
    add_value/4,
    get_daily_mean/2,
    get_minimum_pollution_station/1,
    get_most_active_station/1,
    get_one_value/3,
    get_station_mean/2,
    remove_station/1,
    remove_value/3
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, pollution:create_monitor()}.
stop() -> gen_server:stop(?MODULE).
crash() -> gen_server:cast(?MODULE, crash).

add_station(Name, Coord) ->
    gen_server:call(?MODULE, {add_station, Name, Coord}).

add_value(StationId, Timestamp, MType, Measurement) ->
    gen_server:call(?MODULE, {add_value, StationId, Timestamp, MType, Measurement}).

get_daily_mean(Date, MType) ->
    gen_server:call(?MODULE, {get_daily_mean, Date, MType}).

get_minimum_pollution_station(MType) ->
    gen_server:call(?MODULE, {get_minimum_pollution_station, MType}).

get_most_active_station(Date) ->
    gen_server:call(?MODULE, {get_most_active_station, Date}).

get_one_value(StationId, Timestamp, MType) ->
    gen_server:call(?MODULE, {get_one_value, StationId, Timestamp, MType}).

get_station_mean(StationId, MType) ->
    gen_server:call(?MODULE, {get_station_mean, StationId, MType}).

remove_station(StationId) ->
    gen_server:call(?MODULE, {remove_station, StationId}).

remove_value(StationId, Timestamp, MType) ->
    gen_server:call(?MODULE, {remove_value, StationId, Timestamp, MType}).

handle_cast(crash, State) ->
    throw("oh no"),
    "anyway",
    {noreply, State}.

% this could be cleaner by sending {funName, Args} and using apply/3 but it's slooow
% http://erlang.org/doc/efficiency_guide/functions.html#function-calls
handle_call({add_station, Name, Coord}, _From, Monitor) ->
    case pollution:add_station(Name, Coord, Monitor) of
        {error, _} = Error -> {reply, Error, Monitor};
        NewMonitor -> {reply, ok, NewMonitor}
    end;
handle_call({add_value, StationId, Timestamp, MType, Measurement}, _From, Monitor) ->
    case pollution:add_value(StationId, Timestamp, MType, Measurement, Monitor) of
        {error, _} = Error -> {reply, Error, Monitor};
        NewMonitor -> {reply, ok, NewMonitor}
    end;
handle_call({remove_station, StationId}, _From, Monitor) ->
    case pollution:remove_station(StationId, Monitor) of
        {error, _} = Error -> {reply, Error, Monitor};
        NewMonitor -> {reply, ok, NewMonitor}
    end;
handle_call({remove_value, StationId, Timestamp, MType}, _From, Monitor) ->
    case pollution:remove_value(StationId, Timestamp, MType, Monitor) of
        {error, _} = Error -> {reply, Error, Monitor};
        NewMonitor -> {reply, ok, NewMonitor}
    end;
handle_call({get_daily_mean, Date, MType}, _From, Monitor) ->
    Reply = pollution:get_daily_mean(Date, MType, Monitor),
    {reply, Reply, Monitor};
handle_call({get_minimum_pollution_station, MType}, _From, Monitor) ->
    Reply = pollution:get_minimum_pollution_station(MType, Monitor),
    {reply, Reply, Monitor};
handle_call({get_most_active_station, Date}, _From, Monitor) ->
    Reply = pollution:get_most_active_station(Date, Monitor),
    {reply, Reply, Monitor};
handle_call({get_one_value, StationId, Timestamp, MType}, _From, Monitor) ->
    Reply = pollution:get_one_value(StationId, Timestamp, MType, Monitor),
    {reply, Reply, Monitor};
handle_call({get_station_mean, StationId, MType}, _From, Monitor) ->
    Reply = pollution:get_station_mean(StationId, MType, Monitor),
    {reply, Reply, Monitor}.
