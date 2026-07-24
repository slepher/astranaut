-ifndef(REALBENCH_MIN_HRL).
-define(REALBENCH_MIN_HRL, true).

%% Minimal do-macro registration copied from Erlando's include/do.hrl.
-compile({parse_transform, astranaut_macro}).
-import_macro(do_macro).
-use_macro({do_macro, do/1, [{alias, do}]}).

%% Neutral constants used by the extracted benchmark workload.
-define(ENTRY, entry).
-define(EXIT, exit).
-define(LONG, long).
-define(SHORT, short).
-define(TODAY, today).
-define(PREVIOUS, previous).
-define(ROUTED, routed).
-define(LIMIT, limit).
-define(MARKET, market).

-record(state, {
    account_id,
    runtime
}).

-record(runtime, {
    requests = #{}
}).

-record(ledger, {
    route = local
}).

-record(product_ledger, {
    day = 0,
    status = ready
}).

-record(request, {
    id,
    account_id,
    resource_id,
    product_id,
    currency = <<"UNIT">>,
    operation = ?ENTRY,
    direction = ?LONG,
    position = ?TODAY,
    kind = ?LIMIT,
    route = local,
    price = 0,
    quantity = 1,
    frozen_price = 0,
    sequence,
    group,
    margin = 0,
    overnight_margin = 0,
    fee = 0,
    previous_fee = 0,
    exchange_rate = 1.0,
    unit_size = 1,
    precision = 0,
    tick = 1
}).

-record(resource, {
    id,
    product_id,
    currency = <<"UNIT">>,
    unit_size = 1,
    tick = 1,
    precision = 0,
    delivery_slot = 0,
    active = true
}).

-record(product, {
    id,
    mode = net,
    margin_mode = standard
}).

-record(pricing, {
    resource_id,
    product_id,
    currency,
    base_currency,
    session,
    calculated_price,
    unit_size,
    tick,
    precision,
    exchange_rate,
    delivery_slot,
    mode,
    margin_mode,
    route,
    route_account,
    margin = 0,
    fixed_margin_rate = 0,
    percent_margin_rate = 0,
    intraday_fixed_margin = 0,
    intraday_percent_margin = 0,
    maintenance_fixed_margin = 0,
    maintenance_percent_margin = 0,
    overnight_fixed_margin = 0,
    overnight_percent_margin = 0,
    overnight_maintenance_fixed = 0,
    overnight_maintenance_percent = 0,
    margin_per_unit = 0,
    overnight_margin_per_unit = 0,
    fee = 0,
    previous_fee = 0,
    fixed_fee_rate = 0,
    percent_fee_rate = 0,
    previous_fixed_fee_rate = 0,
    previous_percent_fee_rate = 0
}).

-record(margin_rate, {
    long_percent = 0,
    long_fixed = 0,
    short_percent = 0,
    short_fixed = 0
}).

-record(fee_rate, {
    entry_percent = 0,
    entry_fixed = 0,
    exit_percent = 0,
    exit_fixed = 0,
    previous_exit_percent = 0,
    previous_exit_fixed = 0
}).

-endif.
