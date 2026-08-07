# Agent Notes

## Erlang Formatting

In Erlang projects (rebar3), after modifying any Erlang source file
(`*.erl`, `*.hrl`, `*.app.src`) or `rebar.config`, run `rebar3 fmt -w` to
format the code with erlfmt before finishing the task. Use `rebar3 fmt --check`
to verify formatting without modifying files.

## Lessons

`lessons.md` records recurring error patterns and their fixes observed during development of this project. Read it before working on monad bridging, traverse state management, or scan-and-splice processing in `astranaut_macro.erl` and `astranaut.erl`.

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
