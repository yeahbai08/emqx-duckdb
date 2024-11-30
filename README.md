# DuckDB Plugin for EMQX

[DuckDB](https://duckdb.org/) is an in-process SQL OLAP database management system.

This plugin allows you to save MQTT messages to a DuckDB database, and query the database using SQL.

Note that this project is currently the simplest version developed to validate feasibility, and do not use it in production.

## Build the Plugin

```
make rel

_build/default/emqx_plugrel/emqx_duckdb-<vsn>.tar.gz
```

## A quick demo writing MQTT messages to DuckDB

### Load the Plugin

- Install the plugin (`*tar.gz`) to EMQX via dashboard or command line.

- Restart the EMQX broker to ensure the plugin is loaded successfully.

### Add a Rule

- Add the following configuration to the `etc/emqx.conf` file:

```conf
rule_engine.rules.duckdb_demo {
  actions = [
    {
      args: {}
      function: "emqx_duckdb:save_to_duckdb"
    }
  ]
  description = "A demo rule to save messages to DuckDB"
  enable = true
  sql = "SELECT * FROM \"t/#\""
}
```

- Reload the configuration:

```shell
./bin/emqx ctl conf reload --merge
```

### Send MQTT messages and Query the Database

- Test the plugin by publishing some messages to the topic `t/1`.

- Query the database via CLI:

```shell
./bin/emqx ctl emqx_duckdb query 'SELECT * from messages'
[#{data => [{{2024,11,30},{18,14,29.263207}},{{2024,11,30},{18,14,40.130647}}],
   name => <<"timestamp">>,type => timestamp},
 #{data => [<<"t/1">>,<<"t/1">>],name => <<"topic">>,type => varchar},
 #{data => [<<"hello">>,<<"foo">>],name => <<"payload">>,type => varchar},
 #{data => [1,2],name => <<"qos">>,type => tinyint},
 #{data => [true,false],name => <<"retain">>,type => boolean},
 #{data => [<<"emqx_MDQ2Mj">>,<<"emqx_MDQ2Mj">>],
   name => <<"clientid">>,type => varchar},
 #{data => [<<>>,<<>>],name => <<"username">>,type => varchar}]
```

- The database file is named `emqx_duckdb_mqtt.db` under the `data` directory of `emqx`, so you could open it by the DuckDB's CLI (`duckdb`). Please note that you need to stop `emqx` first to avoid file lock:

```shell
emqx stop

~/duckdb ./data/emqx_duckdb_mqtt.db
v1.1.3 19864453f7
Enter ".help" for usage hints.
D select * from messages;
┌────────────────────────────┬─────────┬─────────┬──────┬─────────┬─────────────┬──────────┐
│         timestamp          │  topic  │ payload │ qos  │ retain  │  clientid   │ username │
│         timestamp          │ varchar │ varchar │ int8 │ boolean │   varchar   │ varchar  │
├────────────────────────────┼─────────┼─────────┼──────┼─────────┼─────────────┼──────────┤
│ 2024-11-30 18:14:29.263207 │ t/1     │ hello   │    1 │ true    │ emqx_MDQ2Mj │          │
│ 2024-11-30 18:14:40.130647 │ t/1     │ foo     │    2 │ false   │ emqx_MDQ2Mj │          │
└────────────────────────────┴─────────┴─────────┴──────┴─────────┴─────────────┴──────────┘
```
