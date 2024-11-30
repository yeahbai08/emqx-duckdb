-module(emqx_duckdb).

-include("emqx_duckdb.hrl").

-export([ load/1
        , unload/0
        ]).

-export([ save_to_duckdb/3
        ]).

-define(HP_HIGHEST, 1000).

%% Called when the plugin application start
load(_Env) ->
    ok.

%% Called when the plugin application stop
unload() ->
    ok.

save_to_duckdb(Selected, #{metadata := #{rule_id := RuleId}}, Args) ->
    ?LOG(info, #{
        msg => save_to_duckdb, rule_id => RuleId,
        selected_data => Selected, args => Args
    }).
