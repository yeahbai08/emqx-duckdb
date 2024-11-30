-module(emqx_duckdb_cli).

%% This is an example on how to extend `emqx ctl` with your own commands.

-export([cmd/1]).

cmd(["query", SQL]) ->
    case emqx_duckdb:get_active_db_connection() of
        {ok, Conn} ->
            case educkdb:query(Conn, SQL) of
                {ok, Result} ->
                    emqx_ctl:print("~p", [educkdb:extract_result(Result)]);
                {error, Reason} ->
                    emqx_ctl:print("query failed: ~p", [Reason])
            end;
        {error, not_loaded} ->
            emqx_ctl:print("plugin not loaded");
        {error, Reason} ->
            emqx_ctl:print("get active db connection failed: ~p", [Reason])
    end;

cmd(_) ->
    emqx_ctl:usage([{"query <SQL>", "query 'SELECT * from messages'"}]).
