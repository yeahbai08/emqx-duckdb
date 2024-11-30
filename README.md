# DuckDB Plugin for EMQX

[DuckDB](https://duckdb.org/) is an in-process SQL OLAP database management system.

This plugin allows you to save MQTT messages to a DuckDB database, and query the database using SQL.

## Build the Plugin

```
make rel

_build/default/emqx_plugrel/emqx_duckdb-<vsn>.tar.gz
```
