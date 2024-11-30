%% structured logging
-define(LOG(Level, Data),
    ?LOG(Level, Data, #{})
).

%% structured logging, meta is for handler's filter.
-define(LOG(Level, Data, Meta),
    %% check 'allow' here, only evaluate Data and Meta when necessary
    case logger:allow(Level, ?MODULE) of
        true ->
            logger:log(
                Level,
                (Data),
                maps:merge(Meta, #{
                    mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
                    line => ?LINE
                })
            );
        false ->
            ok
    end
).
