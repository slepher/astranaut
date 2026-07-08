-include("otp_vsn.hrl").

-ifdef(ASTRANAUT_OTP_AT_LEAST_21).
%% The 'OTP_RELEASE' macro introduced at OTP-21,
%% so we can use it for detecting whether the Erlang compiler supports new try/catch syntax or not.
-define(CAPTURE_STACKTRACE, :__StackTrace).
-define(GET_STACKTRACE, __StackTrace).
-else.
-define(CAPTURE_STACKTRACE, ).
-define(GET_STACKTRACE, erlang:get_stacktrace()).
-endif.
