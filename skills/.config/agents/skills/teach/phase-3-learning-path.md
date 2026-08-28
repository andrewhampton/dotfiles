# Level 3 — The PR series as a learning path

I understand the destination architecture and the major design decisions. Walk me through the PR series in the best order for *learning* it — which may not be chronological. The goal is not to summarize each PR but to progressively modify my mental model until I understand how the final system was built.

Organize the commits/PRs into a sequence of conceptual steps. For each step, explain only:

- What new concept, capability, or constraint enters the picture
- What changes in my mental model, and why this step exists
- How it connects to what I already learned
- The 1–3 most useful files/functions/tests to read, each with the question I should be trying to answer while reading it ("Read `Foo#bar` to answer: where is ownership of this state established?")
- What I can safely ignore on a first pass
- **Checkpoint:** one or two questions I should be able to answer before continuing — about the design, not names

Skip mechanical migrations, renames, formatting, boilerplate, test churn, and generated code unless they materially affect the design. Group PRs that are conceptually one step. If a later PR is the best place to understand an earlier concept, teach from the later PR first.

Tests are especially valuable when they reveal invariants, edge cases, intended boundaries, failure behavior, concurrency assumptions, or backwards-compatibility requirements — call those out explicitly.

## Concrete ↔ abstract rule

Every step includes one concrete example. Keep the *same* representative object/request/event from levels 1–2 across steps so I can watch the system evolve: "Step 1: here's what happens when Alice does X. Step 2: now Y exists; here's how Alice's operation changes. Step 3: Z exists because the previous version fails when …". Alternate concept → concrete execution → concept rather than staying at one level.

## Output structure (spread across turns as needed; see SKILL.md)

1. **The steps** — typically 3–5, each a short paragraph: what enters the model, the example's change, 1–3 things to read with their guiding question, one checkpoint question. Maintain one **evolving architecture diagram**: at each step show only what changed (before → new concept → after), not the whole system redrawn.
2. **The reading map** — 5–8 places to read, ordered pedagogically, one line each with its guiding question. (This replaces a separate "final flow" / "one page" restatement — levels 1–2 already did that.)
3. **Take into the review** — 3–5 pointed questions the user should carry into the code review, each tied to an invariant, a rejected alternative, or a silent-failure boundary from the series ("Does X still hold after commit N? Look at …").
4. **Check your understanding** — 3–5 Socratic questions spanning the whole series (see SKILL.md), including one that asks me to predict how the system must change for a plausible new requirement.

If the series cannot fit the budget, publish the full learning path as an artifact (steps and evolving diagrams side by side) and keep the terminal reply to step titles, the link, and the questions.

Optimize throughout for building a mental model, not for proving you inspected every diff.
