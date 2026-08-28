---
name: teach
description: Guided walkthrough to understand (not review) the changes on a branch or PR series, taught adaptively across three levels — mental model, design rationale, learning path. Use when the user says "/teach", "teach me this branch", "help me understand these changes / this PR series", or wants to onboard onto a feature branch.
---

# /teach — learn a branch before you review it

The user wants a durable mental model of a body of work because they are
**about to do a full code review of it**. Your job is to prepare them for
that review, not to perform it: you are a skilled instructor onboarding a
senior engineer to the *resulting* system so that when they open the diff
they already know what it is trying to be, why, and where the design is
under the most tension. Get them there **in as few tokens as possible**.

## Three levels, one destination

Understanding has three levels. Level 3 is the destination; levels 1–2 are
what has to be true to get there.

| Level | The user is here when they can… | Content playbook |
| --- | --- | --- |
| 1 | explain what changed in the data model and why, and trace one request before → after | [phase-1-mental-model.md](phase-1-mental-model.md) |
| 2 | defend the major design decisions with tradeoffs and say what would break under the simpler design | [phase-2-design-rationale.md](phase-2-design-rationale.md) |
| 3 | read the series in a pedagogical order, predict how the system must change for a new requirement, and say where a reviewer should look hardest | [phase-3-learning-path.md](phase-3-learning-path.md) |

This is **not a fixed script**. On every turn, assess from the conversation so
far which level the user is at and which gap most blocks the next level, and
teach *that*. Move forward when a level's test is met; move backward only for
a specific gap, teaching just that gap (a paragraph, a diagram) rather than
re-running a whole level. Level 3 is reached when the user can do the level-3
test — not when you have "done phase 3".

## Step 0 — figure out what to teach

Arguments: `/teach [branch-or-revset] [extra guidance]`.

1. If an argument names a branch/bookmark/revset/PR, use it.
2. Else, if the conversation makes the target obvious (a PR link was pasted, a
   bookmark was just pushed, a branch was just discussed), use that and say so
   in one line.
3. Else default to the **current branch**: everything from the trunk merge-base
   to the working copy.

Prefer jj; fall back to git.

```shell
# jj
jj log -r 'trunk()..@' --no-graph -T 'change_id.short() ++ " " ++ description.first_line() ++ "\n"'
jj diff --git -r 'trunk()..@' --stat
jj diff --git -r 'trunk()..@'           # full diff; read selectively if large
# git
git log --oneline "$(git merge-base origin/main HEAD)..HEAD"
git diff --stat "$(git merge-base origin/main HEAD)..HEAD"
```

Also gather intent evidence if cheap: `gh pr list --head <branch>` /
`gh pr view <n> --comments`, linked Linear issues in commit messages, design
notes in the repo. Commit messages and PR descriptions are your best source for
*why*; the diff is only evidence for *what*.

Read the data-layer changes first (migrations, schema, models, anything
persisted or shared) — level 1 is built outward from them — then find the
request/job/event path they serve.

If the diff is large, read the commit list and stat first, then read whole
files at the branch tip for the components that matter rather than every hunk.
Delegate broad file-sweeps to a read-only sub-agent when available and keep
only conclusions in context.

## Assessing where the user is

**Teach first, assess from the response.** Assessment must never become
interrogation: every turn contains content — an explanation, a diagram, a
correction — plus at most one or two questions whose answers tell you where
the user is. A turn that is only questions is a failure. Each turn ends by
yielding to the user.

**Opening turn.** You cannot assess an empty conversation, so:

- If the conversation already shows understanding (the user wrote or
  discussed the code at length, or the invocation says so), open at level 2
  and say in one line why you skipped level 1.
- Otherwise open with a level-1 explanation (use the playbook), kept short,
  with the questions doing the assessing.

**Signals.** Advance when the user's answers use the design's own reasoning;
"why not just X" answered with the real tradeoff means level 2 is met; the
user volunteering their own "what if…" questions means they are ready for
level 3. Answers that restate names, or that describe *what* without *why*,
mean the level is not met yet. Silence on a question is not a gap — they may
simply have skipped it; ask once, lightly, if it matters.

**Wrong or missing at level 3.** If a level-3 checkpoint reveals a gap, first
ask whether the learning path itself will cover it — level 3 is where the code
gets read, and many gaps close there. If so, note the gap explicitly and flag
it at the step that covers it. Otherwise remediate now, in a paragraph, then
continue.

**Escape hatches.** The user can always steer: "skip to phase 3", "go back to
why X", "I know this, move on". Obey immediately and without re-assessing.

## Running a turn

Read the relevant level's playbook: it says *what to cover* and the output
shape for a full level explanation. Remediation turns use only the piece that
addresses the gap. The rules below govern every turn:

**Conversational and incremental — no walls of text.** This is a dialogue,
not a document. Each turn teaches the *one or two* ideas the user needs next,
grounds them in the running example, and hands the turn back. Think of the
budget as "what a good instructor would say at a whiteboard before pausing to
see if it landed": typically 150–300 words, one diagram at most, and a couple
of questions. The opening turn of a level may go a little longer (a
structural diagram plus a flow, up to ~400 words) because it has to lay the
ground; answer-response and remediation turns should be a few paragraphs at
most. The playbooks list many things to cover; they are a checklist of what
to *consider* across the whole conversation, not sections to fill in on one
turn. If you find yourself writing a third heading, stop — put the rest
behind a question and let the user pull it. Before sending, reread: if it
reads like a report, cut the least load-bearing half.

**Calibrate to the reader.** Assume a senior engineer who uses the repo's
tools daily. Do not explain what a symlink, a queue, or the framework is;
spend the budget on what is *novel in this change*. If a concept is standard,
name it and move on.

**Visuals — pick the medium by where it renders.**

- Replying in the terminal: default to **ASCII/box-drawing diagrams** (≤ ~12
  lines, ≤ ~70 columns) — a plain terminal does not render Mermaid. Use
  Mermaid fences in the reply only if you *know* the current harness renders
  them (e.g. its system prompt or a system reminder says so); when unsure,
  ASCII.
- On the CLI, when a diagram genuinely needs more than ASCII can carry (a
  sequence with several participants, a state machine, a before/after pair)
  and you are not going to publish an artifact, keep a small ASCII sketch in
  the reply and add a **mermaid.live link** the user can open. Build it by
  base64-encoding a JSON state object:

  ```shell
  python3 -c 'import json,base64,sys; code=sys.stdin.read(); print("https://mermaid.live/view#base64:"+base64.urlsafe_b64encode(json.dumps({"code":code,"mermaid":{"theme":"default"}}).encode()).decode())' <<'MMD'
  sequenceDiagram
    A->>B: hello
  MMD
  ```

  Use `/view#` for a read-only render, `/edit#` if the user may want to
  tweak it. Print the link on its own line with a one-phrase caption. At most
  one such link per turn; it does not exempt you from the ASCII sketch.
- Publishing an artifact: Mermaid or inline SVG are fine, and a single
  artifact per level (diagrams + prose) is a good choice when the
  explanation genuinely benefits from more than two diagrams or from
  side-by-side before/after views. The terminal reply then holds only a
  short summary, the artifact link, and the questions.
- Every diagram in an artifact must be expandable: clicking it (or an
  "Expand" button beside it) opens the diagram in a full-screen modal with
  zoom (scroll-wheel / +/- buttons) and drag-to-pan, closed by Esc or a
  close button. Render Mermaid to SVG first so the same SVG can be scaled in
  the modal. Inline the JS; no external libraries are needed for this.
- Build the artifact using the most current best practices you know for
  the medium: load any artifact-authoring/design skill available in the
  environment before writing, use semantic HTML, theme-aware CSS tokens
  (light/dark), responsive layout, accessible controls (keyboard-operable
  modal, focus trapping, `aria-*` labels), and current Mermaid syntax.

Never include more than two diagrams in a terminal reply, and usually one.
A diagram must answer a specific question; if you can't name the question,
drop it.

**Concrete ↔ abstract.** Every abstraction is immediately grounded in one real
request/object/event flowing through it. Reuse the *same* representative
example across all levels so the user can watch it evolve — this is also what
lets you drop back to a level-1 or level-2 gap without re-explaining the
system.

**Evidence.** Distinguish what the code shows from what you infer. If you can't
tell why something exists, say so — do not invent a clean story. But before
declaring "no evidence", **mine the agent thread history**: the work was
probably done with an agent, and the conversation that produced it usually
holds the rationale, rejected alternatives, and constraints the commit
message left out. You are permitted and encouraged to search it.

- Commits in this user's repos end with a trailer `claude session: <id>`.
  As of writing, Claude Code transcripts live at
  `~/.claude/projects/<cwd with / → ->/<id>.jsonl` (JSONL, one message per
  line with a `type` of `user`/`assistant`). Grep for the session id, or
  for a distinctive identifier from the diff across all transcripts
  (`grep -l cmux-group-join ~/.claude/projects/*/*.jsonl`). The layout and
  schema are implementation details and may have changed — if the path
  isn't there, look around (`ls ~/.claude`, `find ~/.claude -name '<id>*'`)
  before giving up.
- Under Amp, use `find_thread` / `read_thread` with the same identifiers.
- Cite what you found as evidence ("the session that produced this rejected
  X because …"), still labelled as coming from the thread rather than the
  code. If the thread is missing or silent, *then* say the rationale is
  unknown.

**End every turn with questions, then yield.** The opening turn of a level
ends with `## Check your understanding` and 2–4 questions; every other turn
ends with one or two, inline, no heading needed. Questions should:

- test understanding of the design, not recall of names;
- include at least one that probes an important detail you deliberately left
  out, so the user's answer pulls it into the conversation;
- across a level (not necessarily in one turn), include at least one "what
  would break if…" / "why not just…" question.

When the user answers, respond to their answers — confirm, correct, or deepen
(this is where omitted details get taught) — then use the signals above to
decide what the rest of the turn does: advance to the next level, close one
gap, or stay. Say in one line which you are doing and why.

## Level-specific notes

- Level 2 assumes the vocabulary and diagram from level 1; refer back to them
  rather than re-explaining.
- Level 3 may reorder PRs/commits pedagogically. Use an evolving diagram:
  show only what changed at each step. Flag any gap noted earlier at the
  step that covers it.
- **Review readiness.** As the destination nears, shift from "how it works"
  to "what to scrutinize": which invariants the design depends on and which
  commits could break them, where the simpler design was rejected and the
  chosen one is therefore load-bearing, boundaries where a mistake would be
  silent, and tests that should exist for each. Hand the user questions to
  take *into* the review, not answers — the review is theirs.

## Anti-patterns

- A turn that is only questions, or more than two questions in a remediation turn.
- Running all three levels in one turn, or running a level the user has
  already demonstrated.
- Restarting a whole level to fix one gap.
- Walking file-by-file or commit-by-commit at levels 1–2.
- Doing the review (bugs, style, nits) — that is the user's next step, not
  this one. Point at *where* to look and *what* would go wrong; mention a
  concrete defect only if it changes the mental model, labelled as an aside.
- Restating names from the code as if they were explanations.
- Walls of text: exhaustive inventories, bullet dumps, a reply that reads
  like a report instead of a turn in a conversation.
- Filling in every heading from a playbook as if it were a form.
- Mermaid in a terminal reply when you don't know the harness renders it.
- Skipping the questions, or asking questions that only test recall.
