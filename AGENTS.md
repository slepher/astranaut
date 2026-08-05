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

## Tool Call Completion

If a normally short command such as `git add`, `git status`, or `git commit`
returns a live process or cell identifier, call the corresponding wait
operation immediately. Do not leave the task showing as active or assume the
underlying command is genuinely slow; the command may already have completed
while the tool result is waiting to be collected. If it still has not
completed after the immediate wait, inspect the process or lock state instead
of waiting silently.

When the user asks only to commit existing changes, keep the task scoped to
`git status`, staging, committing, and confirming the final status. Do not
rerun tests or perform unrelated audits unless the user requests them or a
newly discovered condition makes the commit unsafe.

## Codebase Map Maintenance

`codebase-map.md` at the repository root is the survey document for this
repository. Its body must describe the **committed** state only; uncommitted
work goes into the trailing "未提交增补" (uncommitted addendum) section.

### Initial survey (full-repository)

1. Record the current revision: short hash, full hash, and subject of
   `HEAD` (`git log -1 --format='%h %ad %s' --date=short`).
2. Run `git status --porcelain` and `git diff --stat` to separate committed
   state from uncommitted (modified `M` and untracked `??`) files.
3. Base every fact in the body on the committed snapshot: module lists, test
   case counts, README line counts, etc. must come from
   `git show HEAD:<path>` (or `git grep` on `HEAD`), never from the working
   tree alone.
4. Do not reference untracked files (e.g. `luna*.md`) in the body; list them
   only in the addendum.
5. Commit `codebase-map.md` as its own commit.

### Incremental survey (delta since a previous revision)

1. The previous revision is recorded in the header of the existing
   `codebase-map.md`. The file may be missing from the working tree or the
   old revision may no longer be on `HEAD`'s history (rebase/squash rewrites
   history). Restore it with `git show <old-rev>:codebase-map.md` rather than
   reconstructing it.
2. Compute the delta with `git diff <old-rev>..HEAD --stat`, then inspect
   the relevant hunks (README, macro-layer modules, openspec, tests).
3. Update the header `Revision` to the new `HEAD`.
4. Fold items from the previous addendum that are now committed into the
   body (recount test cases, README lines, module descriptions at the new
   `HEAD`).
5. Rewrite the addendum to reflect the current working tree
   (`git status --porcelain`).
6. Update module descriptions and architecture notes where the delta
   changed them.
7. Commit `codebase-map.md` as its own commit.
