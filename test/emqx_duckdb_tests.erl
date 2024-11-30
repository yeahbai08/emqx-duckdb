-module(emqx_duckdb_tests).

-include_lib("eunit/include/eunit.hrl").

save_to_duckdb_test() ->
    GetState = fun() ->
        Ref = make_ref(),
        whereis(appender_manager) ! {get_state, Ref, self()},
        receive
            {Ref, State} -> State
        after 1000 -> throw({error, get_state_timeout})
        end
    end,
    {ok, _} = application:ensure_all_started(educkdb),
    Conf = #{db_file_path => ":memory:", table_name => "msgs", flush_interval => 100},
    ok = emqx_duckdb:load(Conf),
    ?assertMatch(#{last_msgs_cnt := 0}, GetState()),
    Ts1 = erlang:system_time(microsecond),
    Ts2 = Ts1 + 1,
    Data1 = #{
        timestamp => Ts1,
        topic => <<"t/1">>,
        payload => <<"payload1">>,
        qos => 1,
        retain => true,
        clientid => <<"clientid1">>,
        username => <<"username1">>
    },
    Data2 = #{
        timestamp => Ts2,
        topic => <<"t/2">>,
        payload => <<"payload2">>,
        qos => 2,
        retain => false,
        clientid => <<"clientid2">>,
        username => <<"username2">>
    },
    ActionArgs = #{
        db => <<"db">>,
        table => <<"table">>
    },
    ?assertEqual(ok, emqx_duckdb:save_to_duckdb(Data1, #{}, ActionArgs)),
    ?assertEqual(ok, emqx_duckdb:save_to_duckdb(Data2, #{}, ActionArgs)),
    timer:sleep(200),
    ?assertMatch(#{last_msgs_cnt := 2}, GetState()),
    Conn = persistent_term:get({emqx_duckdb, message_db_conn}),
    {ok, R1} = educkdb:query(Conn, "SELECT * FROM msgs"),
    ?assertMatch([
        #{data := [_, _], name := <<"timestamp">>, type := timestamp},
        #{data := [<<"t/1">>,<<"t/2">>], name := <<"topic">>, type := varchar},
        #{data := [<<"payload1">>,<<"payload2">>], name := <<"payload">>, type := varchar},
        #{data := [1,2], name := <<"qos">>, type := tinyint},
        #{data := [true,false], name := <<"retain">>, type := boolean},
        #{data := [<<"clientid1">>,<<"clientid2">>], name := <<"clientid">>, type := varchar},
        #{data := [<<"username1">>,<<"username2">>], name := <<"username">>, type := varchar}
    ], educkdb:extract_result(R1)),
    ok = emqx_duckdb:unload().
