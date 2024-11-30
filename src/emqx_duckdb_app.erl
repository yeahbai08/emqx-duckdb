-module(emqx_duckdb_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_duckdb_sup:start_link(),
    emqx_duckdb:load(application:get_all_env()),

    emqx_ctl:register_command(emqx_duckdb, {emqx_duckdb_cli, cmd}),
    {ok, Sup}.

stop(_State) ->
    emqx_ctl:unregister_command(emqx_duckdb),
    emqx_duckdb:unload().
