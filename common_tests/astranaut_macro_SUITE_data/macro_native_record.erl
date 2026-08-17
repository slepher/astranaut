-module(macro_native_record).

-include("otp_vsn.hrl").

-if(?ASTRANAUT_OTP_VSN_GE(29)).
-record #rec{x}.
-export_record([rec]).
-endif.
