# Agent Notes

## Lessons

`lessons.md` records recurring error patterns and their fixes observed during development of this project. Read it before working on monad bridging, traverse state management, or scan-and-splice processing in `astranaut_macro.erl` and `astranaut.erl`.

## Codex App Sandbox

`scripts/rebar3_sandbox.ps1` is specifically for running this project inside the Codex desktop app sandbox mode on Windows.

In that environment, Erlang/OTP can fail during startup or host lookup because the VM tries to execute the external `inet_gethost` helper from the OTP installation directory. The Codex app sandbox may block that helper process, producing errors like:

```text
Can not execute .../inet_gethost : einval
```

The wrapper sets `ERL_INETRC` to `scripts/codex_inetrc` before invoking `rebar3`. That inetrc file limits Erlang name lookup to local file entries for `localhost`, so Erlang does not need to start the external `inet_gethost` helper for these sandboxed runs.

Use the wrapper when running `rebar3` commands from Codex app sandbox mode:

```powershell
powershell -ExecutionPolicy Bypass -File scripts\rebar3_sandbox.ps1 --version
powershell -ExecutionPolicy Bypass -File scripts\rebar3_sandbox.ps1 ct
```

Outside Codex app sandbox mode, normal `rebar3` commands can be used directly if the local environment allows Erlang to execute `inet_gethost`.
