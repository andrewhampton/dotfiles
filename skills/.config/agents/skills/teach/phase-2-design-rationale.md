# Level 2 — Why the design ended up this way

I understand the basic architecture (level 1). Now help me understand *why* it ended up this way. Focus on the significant design decisions in this PR series; I care far more about causality and tradeoffs than implementation details.

For each important decision, trace:

problem → constraints → obvious/simple approach → why that is insufficient → chosen design → consequences

Questions to answer for each:

- What problem were we actually solving? What constraints mattered?
- What simpler design would a reasonable engineer try first, and why would it fail, become awkward, or violate a constraint?
- What forced or motivated the current design? What complexity did we intentionally accept?
- What does this design make easier? Harder? What future change does it anticipate?
- What alternatives appear to have been considered or implicitly rejected?

Explicitly distinguish (1) requirements or unavoidable constraints, (2) deliberate design choices, and (3) incidental implementation details. Do not treat every implementation decision as architecture.

## Concrete ↔ abstract rule

Never explain a tradeoff only in the abstract. For each decision give at least one concrete scenario, ideally *without the design* vs *with the design*, using the same representative request/event/object from level 1 where possible: "Without this, when X happens we would need A, B, and C in three places. With it, …"

## Evidence and uncertainty

Be careful about reconstructing intent after the fact. Use PR descriptions, commit messages, comments, tests, linked issues, code structure, and the agent thread history that produced the commits (see SKILL.md — follow the `claude session:` trailers) as evidence. If the evidence supports a rationale, explain it. If a rationale is plausible but not demonstrated, label it **inference**. If you genuinely cannot tell, say: "The code shows what this does, but I don't have enough evidence to know why this design was chosen." Do not invent a clean story because one sounds plausible.

## Output structure (spread across turns as needed; see SKILL.md)

Cover the **2–3 decisions that matter most**, one short paragraph each; name lesser ones in a single sentence. For each major decision, in prose, not sub-headings:

**Decision: [short name]** — the obvious approach and why it fails here → what this design does instead → one concrete with/without scenario → the tradeoff accepted → confidence (evidence vs inference).

Then:

A diagram belongs here only when it answers a specific "why" — why this boundary, why this state lives here, what race or coupling is avoided. Useful forms: before/after, alternative A vs B, a sequence showing where the simple design breaks. Nothing decorative.

- **The design philosophy** — the 2–5 recurring principles that explain most of the choices.
- **The complexity budget** — where the design intentionally spends complexity and what it buys.
- **Check your understanding** — 3–5 Socratic questions (see SKILL.md). Good ones here: "what would have to change for the simpler design to become the right one?", "which constraint, if removed, collapses decision X?", and one about a decision you only mentioned in passing.
