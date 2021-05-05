-module(pollution_value_collector).
-behavior(gen_statem).
-export([start_link/0, stop/0]).
-export([set_station/1, add_value/3, store_data/0]).
-export([init/1, callback_mode/0, station_unset/3, station_set/3]).

set_station(Station_id) -> gen_statem:call(?MODULE, {set_station, Station_id}).
add_value(Timestamp, M_type, Measurement) ->
    gen_statem:call(?MODULE, {add_value, {Timestamp, M_type, Measurement}}).
store_data() -> gen_statem:call(?MODULE, store_data).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, station_unset, {nil, []}}.
stop() -> gen_statem:stop(?MODULE).
callback_mode() -> state_functions.

station_unset({call, From}, {set_station, Station_id}, {nil, []}) ->
    {next_state, station_set, {Station_id, []}, [{reply, From, ok}]};
station_unset({call, From}, _, Data) ->
    {keep_state, Data, [{reply, From, {error, invalid_state_transition}}]}.

station_set({call, From}, {add_value, To_add}, {Station_id, Values}) ->
    {keep_state, {Station_id, [To_add | Values]}, [{reply, From, ok}]};
station_set({call, From}, store_data, {Station_id, Values}) ->
    With_errors = lists:filtermap(
        fun({Timestamp, M_type, Measurement} = X) ->
            case pollution_server:addValue(Station_id, Timestamp, M_type, Measurement) of
                ({error, Reason}) -> {true, {error, Reason, X}};
                (_) -> false
            end
        end,
        lists:reverse(Values)
    ),
    Reply =
        case With_errors of
            [] -> ok;
            Errors -> {error, Errors}
        end,
    {next_state, station_unset, {nil, []}, [{reply, From, Reply}]};
station_set({call, From}, _, Data) ->
    {keep_state, Data, [{reply, From, {error, invalid_state_transition}}]}.
