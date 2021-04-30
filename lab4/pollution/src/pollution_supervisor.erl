-module(pollution_supervisor).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(
        {local, ?MODULE},
        ?MODULE,
        []
    ).

init(_) ->
    {ok,
        {
            #{
                strategy => one_for_one,
                intensity => 100,
                period => 2
            },
            [
                #{
                    id => 'Pollution server',
                    start => {pollution_server, start_link, []},
                    restart => transient,
                    shutdown => 2000,
                    type => worker,
                    modules => []
                },
                #{
                    id => 'Pollution server value collector',
                    start => {pollution_value_collector, start_link, []},
                    restart => transient,
                    shutdown => 2000,
                    type => worker,
                    modules => []
                }
            ]
        }}.
