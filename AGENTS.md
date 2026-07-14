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

The wrapper may be used for lightweight diagnostic commands that remain inside
the Codex app sandbox, for example:

```powershell
powershell -ExecutionPolicy Bypass -File scripts\rebar3_sandbox.ps1 --version
```

### Windows compile and Common Test

On Windows, do **not** run `rebar3 compile` or `rebar3 ct` through
`scripts/rebar3_sandbox.ps1` inside the sandbox. Rebar3 may recreate build
directory junctions by starting `cmd /c mklink` through Erlang `open_port`;
the sandbox can block that child process with `einval` even though every path
is inside the workspace.

For these commands, request escalation immediately and run rebar3 directly:

```powershell
rebar3 compile
rebar3 ct
```

Do not first retry the wrapper or a sandboxed rebar3 invocation after this
known `open_port`/`mklink` failure mode. Outside the sandbox, Erlang may execute
both `inet_gethost` and rebar3's junction helper normally.

When running the full Common Test suite, give the command a real test timeout (at least 120 seconds; use longer when the environment is slow). Do not set a short command timeout merely to make the tool yield and then expect to wait on it: the command timeout terminates `rebar3` and aborts the CT run. If the tool returns a live process or cell identifier, keep the original long command timeout and use the corresponding wait operation for incremental output. Report a command-timeout termination as an interrupted run, not as a test failure.

Outside Codex app sandbox mode, normal `rebar3` commands can be used directly if the local environment allows Erlang to execute `inet_gethost`.
