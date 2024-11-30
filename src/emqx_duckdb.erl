-module(emqx_duckdb).

-include("emqx_duckdb.hrl").

-export([ load/1
        , unload/0
        ]).

-export([ save_to_duckdb/3
        , get_active_db_connection/0
        ]).

-export([ appender_manager/1
        ]).

-define(HP_HIGHEST, 1000).
-define(DB_FILE, "emqx_duckdb_mqtt.db").
-define(TABLE_NAME, "messages").
-define(TABLE_CREATE_SQL(TABLE_NAME),
"CREATE TABLE IF NOT EXISTS "++TABLE_NAME++
"("
    "timestamp TIMESTAMP PRIMARY KEY, "
    "topic VARCHAR, "
    "payload VARCHAR, "
    "qos TINYINT, "
    "retain BOOLEAN, "
    "clientid VARCHAR, "
    "username VARCHAR"
")"
).
-define(CNT_IDX_MSGS, 1).
-define(CNT_IDX_MSGS_SUCC, 2).
-define(CNT_IDX_MSGS_DROP, 3).
-define(FLUSH_INTERVAL, 15_000).

load(Conf) ->
    DbFile = case maps:find(db_file_path, Conf) of
        {ok, Filename} -> Filename;
        error -> filename:join([emqx:data_dir(), ?DB_FILE])
    end,
    {ok, Db} = educkdb:open(DbFile),
    {ok, Conn} = educkdb:connect(Db),
    Tab = maps:get(table_name, Conf, ?TABLE_NAME),
    {ok, _} = educkdb:query(Conn, ?TABLE_CREATE_SQL(Tab)),
    {ok, Appender} = educkdb:appender_create(Conn, undefined, Tab),
    CntRef = counters:new(5, [write_concurrency]),
    ok = persistent_term:put({?MODULE, message_db}, Db),
    ok = persistent_term:put({?MODULE, message_db_conn}, Conn),
    ok = persistent_term:put({?MODULE, message_appender}, Appender),
    ok = persistent_term:put({?MODULE, message_counter}, CntRef),
    spawn(?MODULE, appender_manager, [#{
        flush_interval => maps:get(flush_interval, Conf, ?FLUSH_INTERVAL),
        parent => self()
    }]),
    receive
        {appender_manager, started, _} -> ok
    after 1000 ->
        throw({error, appender_manager_start_timeout})
    end.

unload() ->
    _ = educkdb:disconnect(persistent_term:get({?MODULE, message_db_conn})),
    _ = educkdb:close(persistent_term:get({?MODULE, message_db})),
    whereis(appender_manager) ! stop,
    ok.

save_to_duckdb(Selected, _, ActionArgs) ->
    ?LOG(debug, #{
        msg => save_to_duckdb,
        selected_data => Selected, args => ActionArgs
    }),
    CntRef = persistent_term:get({?MODULE, message_counter}),
    Appender = persistent_term:get({?MODULE, message_appender}),
    ok = educkdb:append_timestamp(Appender, erlang:system_time(microsecond)),
    ok = educkdb:append_varchar(Appender, maps:get(topic, Selected, <<"">>)),
    ok = educkdb:append_varchar(Appender, maps:get(payload, Selected, <<"">>)),
    ok = educkdb:append_int8(Appender, maps:get(qos, Selected, 0)),
    ok = educkdb:append_boolean(Appender, maps:get(retain, maps:get(flags, Selected, #{}), false)),
    ok = educkdb:append_varchar(Appender, maps:get(clientid, Selected, <<"">>)),
    ok = educkdb:append_varchar(Appender, maps:get(username, Selected, <<"">>)),
    ok = educkdb:appender_end_row(Appender),
    ok = counters:add(CntRef, ?CNT_IDX_MSGS, 1),
    ok.

appender_manager(#{parent := Parent} = Opts) ->
    erlang:register(appender_manager, self()),
    CntRef = persistent_term:get({?MODULE, message_counter}),
    Parent ! {appender_manager, started, self()},
    appender_manager_loop(Opts#{
        last_msgs_cnt => counters:get(CntRef, ?CNT_IDX_MSGS)
    }).

appender_manager_loop(#{last_msgs_cnt := LastMsgCnt, flush_interval := FlushInterval} = State) ->
    CntRef = persistent_term:get({?MODULE, message_counter}),
    NewState = case counters:get(CntRef, ?CNT_IDX_MSGS) of
        LastMsgCnt -> State;
        NewMsgCnt ->
            Appender = persistent_term:get({?MODULE, message_appender}),
            case educkdb:appender_flush(Appender) of
                ok -> counters:add(CntRef, ?CNT_IDX_MSGS_SUCC, NewMsgCnt);
                {error, {appender, "PRIMARY KEY or UNIQUE constraint violated" ++ _}} ->
                    Dropped = NewMsgCnt - LastMsgCnt,
                    counters:add(CntRef, ?CNT_IDX_MSGS_DROP, Dropped),
                    ?LOG(error, #{msg => appender_flush_duplicate_key, dropped => Dropped});
                {error, Reason} ->
                    Dropped = NewMsgCnt - LastMsgCnt,
                    counters:add(CntRef, ?CNT_IDX_MSGS_DROP, Dropped),
                    ?LOG(error, #{msg => appender_flush_error, reason => Reason, dropped => Dropped})
            end,
            State#{last_msgs_cnt => NewMsgCnt}
    end,
    receive
        stop -> ok;
        {get_state, Ref, From} ->
            From ! {Ref, NewState},
            appender_manager_loop(NewState)
    after FlushInterval ->
        appender_manager_loop(NewState)
    end.

get_active_db_connection() ->
    case persistent_term:get({?MODULE, message_db_conn}, undefined) of
        undefined -> {error, not_loaded};
        Conn -> {ok, Conn}
    end.
