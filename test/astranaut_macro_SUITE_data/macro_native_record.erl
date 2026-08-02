-module(macro_native_record).

-include("otp_vsn.hrl").

-ifdef(ASTRANAUT_OTP_AT_LEAST_29).
-record #rec{x}.
-export_record([rec]).
-endif.
