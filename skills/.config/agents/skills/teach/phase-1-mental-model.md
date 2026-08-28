# Level 1 — Build the mental model

I need to build a durable mental model of the changes in this PR series. Do not review the code or summarize the diffs.

Treat this as onboarding a senior engineer to the resulting system. Start from the system *before* these PRs and explain the design that exists *after* them. The goal: I should be able to whiteboard the system and explain it to another engineer without reopening the code.

## Start from the data, then the request

Build the model in this order — it is how I learn best, and the core ideas should *fall out* of it rather than be asserted up front:

1. **What changed in the database, and why it was needed.** Migrations, new/altered tables and columns, indexes, constraints, enums, backfills. For each: what fact about the world does this now record that it couldn't before, and which requirement forced it? A schema change is the most honest statement of intent in a series — lead with it. If the series has no database changes, use the nearest equivalent: the persistent or shared state it adds or reshapes (config, files on disk, queues, caches, external-service state), and say that is what you're substituting.
2. **The flow of one representative request, before → after.** Pick the request/job/event the series exists to serve. Trace it through the old system, then the new one, and show exactly where the path diverges and how the new data is read or written along it. The diff between the two traces *is* the change.
   Selectivity applies to both: cover only the data changes and the request flow(s) that are *critical to the core concepts* — usually one or two tables and one request. A column rename, a supporting index, or a secondary endpoint that follows the same pattern is noise at this level; leave it for the questions or level 3.
3. **Only then, the core ideas.** Name the 3–5 concepts that the data changes and the flow diff together reveal. Each concept should be traceable back to "we needed to store X" or "the request now does Y".

Focus on:

- What problem the old design had
- The key idea behind the new design
- The 3–7 concepts or components I actually need in my head
- What responsibility each one owns and how they interact
- The important data/control flow
- The invariants the design is trying to preserve
- Where state lives and how it changes
- The boundaries between components

Do not walk file-by-file or PR-by-PR. Do not give an exhaustive architecture inventory. Compress the system into the smallest useful mental model.

## Concrete ↔ abstract rule

Whenever you introduce an abstraction, immediately ground it:

1. Explain the abstraction and why it exists.
2. Show one real object/request/event flowing through it.
3. Return to the abstraction and say what the example teaches.

If you say "X coordinates Y", also say what actually happens when a representative request passes through X. Whenever an abstraction exists because of a constraint, connect them explicitly: "We need X because without it, Y would happen." Do not introduce terminology before the problem that made it necessary.

## Output structure (spread across turns as needed; see SKILL.md)

1. **The problem** — one or two sentences: what the old system couldn't do.
2. **The data changes and why** — only the schema/state changes the core ideas depend on, each with the requirement that forced it; a small before/after data diagram if the relationships changed.
3. **The request, before → after** — the representative example traced through old and new, as a flow/sequence diagram annotated where the path diverges and where the new data is touched.
4. **The core ideas** — the 3–5 concepts that fall out of 2 and 3, one or two sentences each, with a structural diagram (major responsibilities and relationships, 5–8 boxes) only if the flow diagram doesn't already show them.
5. **What to remember** — 3–5 one-line statements (these double as the invariants).
6. **Check your understanding** — 3–5 Socratic questions (see SKILL.md).

The "Focus on" list above is what to *consider*; most of it should be folded into these sections or left for the questions, not given its own heading.

## Constraints

- Optimize for understanding, not completeness.
- Skip mechanical implementation details unless they change the mental model.
- Do not simply restate names from the code, and do not assume a well-named abstraction is self-explanatory.
- If something appears accidental or incidental, say so.
- If you cannot determine why something exists from the evidence, say so rather than inventing a rationale.
- Keep it concise enough that the important ideas remain visible.
