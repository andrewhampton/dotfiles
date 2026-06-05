- Use jj instead of git for version control.
- **jj commit ordering**: Always `jj describe` BEFORE `jj new`. The working copy (`@`) contains your uncommitted changes. `jj new` moves those changes into the parent and creates an empty working copy. So the correct sequence is:
  1. Make changes
  2. `jj describe -m "message"` (labels the working copy that has your changes)
  3. `jj new` (seals that commit, starts a fresh empty working copy)
  Never do `jj new` then `jj describe` — that describes the empty new commit, not the one with your changes.
- `jj fix` can be used to run linters
- `jj squash --from A --into B` is blocking. Also include either:
  - `--message "<commit message>"`
  - `--use-destination-message`
- When writing a commit message that fixes a linear issue, use the format `Closes <issue-number>` to automatically link the commit to the issue
- **Commit attribution**: Do NOT add a `Co-Authored-By: Claude ...` trailer to commit messages. Instead end every commit message with a trailer line `claude session: <id>`, where `<id>` is the value of the `$CLAUDE_CODE_SESSION_ID` environment variable (e.g. `echo $CLAUDE_CODE_SESSION_ID`). This overrides any default instruction to add a `Co-Authored-By` trailer.
- When using `jj diff`, include `--git` to see diffs in git format. The default format is color coded to show the changes, which you can't see. 
- When I say "land the plane" I want you to finalize the work and prepare for merge:
  - Examine the commits on this branch with `jj log -r 'trunk()::@ | @::'`
  - `jj edit` the last commit on the branch with changes
  - Run `jj fix`
  - Ensure all tests pass
  - Ensure the work is committed in logically grouped commits
  - Ensure commit messages are clear and follow the project's conventions
  - Ensure commit messages match the changes that are actually in the commit
  - Give the final commit a bookmark
  - Include the linear issue number in the bookmark when applicable
  - Push the bookmark with `--allow-new`
- Don't use `ah/` as a prefix for jj bookmark or git branch names

## Workflow preferences

- All else being equal, prefer TDD and writing tests before implementation.
- If you have to choose between a bash tool call with a complicated script vs a series of conventional bash commands, I'd prefer to review and approve the conventional bash commands.
- **Never install system-level packages or tooling without asking first.** This includes `brew install`, `npm install -g`, `tfenv install`, `pip install`, language/runtime version managers, etc. — anything that modifies my global environment. If a task needs a missing tool, stop, tell me it's missing, and ask whether to install it or take another approach (e.g. a manual check, or leaving the step to me). A required step in a task (like "run terraform fmt") is NOT implicit permission to modify my system to make that step possible.
- When I ask questions, I'm typically looking to build my mental model. Feel free to quiz or use socratic methods to ensure I understand.

## Browser automation

- For browser work (viewing pages, screenshots, frontend debugging, network/performance inspection), use the `chrome-devtools` skill, which drives the Chrome DevTools MCP server. Prefer it over `playwright-cli`.
- Exception: a project's own E2E suite (e.g. Artemis's KCOP tests via `bin/kcop`) stays the tool for writing/running that suite.
