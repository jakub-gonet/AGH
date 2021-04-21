-module(pollution_supervisor).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(_) ->
    supervisor:start_link(
        {local, ?MODULE},
        ?MODULE,
        []
    ),
    unlink(whereis(?MODULE)).

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
                }
            ]
        }}.
