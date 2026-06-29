---
date: 2026-04-27T22:27:44-05:00
researcher: GPT-5.5
git_commit: 69929c7d01bdc77f83394d47b5389184bd1cf308
branch: pr-7-fixes
repository: sq-sandbox
topic: "Browser software factory game on top of sq-sandbox"
tags: [research, codebase, gui, software-factory, scud, shen-backpressure, game-ui]
status: complete
last_updated: 2026-04-28
last_updated_by: GPT-5.5
last_updated_note: "Added Autopoiesis/ap findings and scoping guidance"
---

# Research: Browser Software Factory Game on Top of sq-sandbox

**Date**: 2026-04-27T22:27:44-05:00
**Researcher**: GPT-5.5
**Git Commit**: 69929c7d01bdc77f83394d47b5389184bd1cf308
**Branch**: pr-7-fixes
**Repository**: sq-sandbox

## Research Question

Explore whether sq-sandbox can support a browser-based software factory game:
human spec formation, deterministic gates where possible, LLM-guided execution,
agent NPCs, clickable interactions, VNC/browser views, Shen-Backpressure for
formal gates, SCUD for task graph generation, and inspiration from Simon
Willison's StrongDM software-factory writeup plus Dan Shapiro's Kilroy.

## Summary

sq-sandbox already has the runtime primitives for this as a separate project on
top of the daemon: isolated sandboxes, GUI/noVNC browser viewports, browser-open
commands, task runner SSE events, snapshots, restore, fork/diff in Irmin mode,
module loadouts, and control-plane accounting. The natural next layer is not a
new sandbox backend; it is an orchestrator and browser UI that treats each
sandbox as a "factory cell" and each autonomous agent as an NPC with visible
state, role, tools, gates, and save points.

Shen-Backpressure fits as the deterministic quality layer. Its stable surface is
the `sb.toml` manifest, `sb context --format json`, `sb gen`, `sb gates`, and
`sb derive`. SCUD fits as the work-formation layer. Its stable surface is the
task DAG in `.scud/tasks/tasks.scg`, plus `scud generate`, `scud next`,
`scud waves --json`, `scud show --json`, `scud set-status`, and `scud swarm`.

The external software-factory references point toward graph-first orchestration:
spec to directed pipeline, deterministic routing, human gates, agent nodes, tool
nodes, scenario holdouts, and event replay. Autopoiesis/ap adds a valuable
vocabulary and working UI patterns for agent state, approvals, time-travel,
teams, budgets, and cognitive timelines, but should be treated as an idea and
component mine rather than the base platform for the new game. The graph is the
world map, human gates are NPC dialogue, VNC/noVNC is the viewport into a
working cell, and snapshots are save states.

## Current sq-sandbox Interfaces

The daemon registers HTTP routes in `impl/go/api/handler.go`, with sandbox
lifecycle, exec, background jobs, GUI/noVNC, browser open, task runner, modules,
snapshots, restore, fork, diff, and control-plane endpoints.

Important surfaces for a game/orchestrator:

- `POST /cgi-bin/api/sandboxes` creates a sandbox with `layers`, `features`,
  `owner`, `task`, resource fields, and TTL.
- `POST /cgi-bin/api/sandboxes/{id}/gui/enable` returns a noVNC URL protected by
  a per-sandbox session token.
- `POST /cgi-bin/api/sandboxes/{id}/gui/browser/open` asks the in-desktop
  browser to open a URL through the GUI session FIFO.
- `POST /cgi-bin/api/sandboxes/{id}/task` starts an autonomous task runner.
- `GET /cgi-bin/api/sandboxes/{id}/task/events` streams structured SSE events.
- `POST /cgi-bin/api/sandboxes/{id}/task/pause|resume|kill` gives external
  control over an agent run.
- `POST /cgi-bin/api/sandboxes/{id}/snapshot` and `restore` provide save points.
- `POST /cgi-bin/api/sandboxes/{id}/fork` and `GET .../diff` provide cheap
  branching and inspection when using the Irmin backend.
- `POST /cgi-bin/api/sandboxes/{id}/activate` hot-adds capability modules.
- `GET /cgi-bin/api/modules` lists available capability packs.

The GUI/browser stack is now coherent enough for this idea. `500-gui-base`
provides Xvfb, XFCE, x11vnc, noVNC, websockify, and dbus. `510-browser-base` is
live-built on top of the GUI root and includes browser support. The noVNC reverse
proxy enters the sandbox network namespace and exposes one browser-safe URL.

The task runner already emits a game-friendly event stream: task start, batch
start, tool calls, turn boundaries, workspace state, snapshots, commits, MR
creation, terminal state. Those events are the basis for an activity log,
progress meter, NPC animation state, or replay timeline.

## Shen-Backpressure Integration

Shen-Backpressure's engine is CLI-first and manifest-driven. The useful boundary
for a game is:

```sh
sb context --format json
sb context --format markdown
sb gen
sb gates
sb derive
sb derive --regen
```

`sb context --format json` exposes project language, spec files, generated guard
types, constructors, derive coverage, configured gates, parallel groups, and
latest backpressure errors. `sb gates` is the deterministic pass/fail surface.
The game can treat each gate as an inspection station. Failure output becomes NPC
dialogue or a repair quest; pass output unlocks the next room or task wave.

The important design boundary is that Shen is not the LLM. Shen and the compiler
reject invalid states. The LLM is the guide or repair actor responding to that
backpressure.

## SCUD Integration

SCUD stores project work as a DAG in `.scud/tasks/tasks.scg`, with phases/tags,
task IDs, dependencies, complexity, priority, status, assignment, agent type,
and model tier. Useful commands:

```sh
scud generate PRD.md -t TAG -n 10
scud next -t TAG
scud next-batch -t TAG --json -n 5
scud waves -t TAG --json
scud show ID -t TAG --json
scud list -t TAG --json
scud set-status ID in-progress -t TAG
scud set-status ID done -t TAG
scud swarm -t TAG --round-size N
scud mermaid -t TAG
```

In the game, SCUD is the planner that turns the player's spec into quests,
dependencies, and waves. The graph can be rendered directly as a factory floor.
Ready tasks are unlocked stations. Blocked tasks are locked doors. Parallel waves
are simultaneous NPC assignments.

## External Reference Ideas

Simon Willison's writeup of StrongDM's software factory emphasizes:

- spec plus validation harness plus feedback loop;
- scenario holdouts outside the codebase;
- digital twin services for high-volume behavior testing;
- shift work: interactive spec formation, then unattended execution;
- filesystems and pyramid summaries as agent memory substrates.

Dan Shapiro's Kilroy implements the StrongDM Attractor idea as a Go CLI:
requirements become a Graphviz DOT pipeline, then an executor runs typed nodes.
Important node types include coding nodes, tool nodes, human wait gates,
conditionals, parallel fan-out, fan-in, and manager loops. Its experimental HTTP
server exposes pending human questions and SSE events, which is already close to
a browser UI backend.

The strongest transferable idea is graph-first orchestration. The graph is both
the execution plan and the game board.

## Autopoiesis/ap Findings

`~/projects/ap` is highly relevant but sprawling. It has a Common Lisp backend
with a datom/EAV substrate, Linda-style `take!` coordination, a conductor tick
loop, agent/cognitive primitives, blocking human-input requests, snapshot DAGs,
REST/WebSocket/SSE APIs, and a SolidJS `dag-explorer` Command Center. It also
has frozen side surfaces: Rust `holodeck/` and `nexus/`.

The parts worth borrowing directly:

- **Human gates**: `blocking-human-input` maps almost exactly to NPC dialogue
  gates. The browser `ApprovalsView` already shows prompt, context, options,
  approve/reject/respond, and age.
- **Agent visual language**: `dag-explorer` uses clear state dots, badges,
  activity timers, current-tool displays, cost/tokens/calls, context gauges, and
  idle/active/pending-human glow states.
- **DAG and timeline UI**: `DAGCanvas`, `SnapshotTimeline`, `ThoughtStream`, and
  `HolodeckView` already explore many of the visual patterns needed for a
  factory floor and replay view.
- **Coordination vocabulary**: leader-worker, parallel, pipeline, debate, and
  consensus strategies are useful as player-selectable team/org patterns.
- **Snapshot-as-reality**: content-addressed snapshot DAGs, branching, diffing,
  and "fork from here" are exactly the game mechanic we want, even if the new
  implementation should use sq-sandbox snapshots for workspace state.
- **Budget/attention as resources**: the budget dashboard, approval queue, and
  activity tracking point toward game resources: token spend, human attention,
  agent time, blocked gates, and risk.

The parts to avoid adopting wholesale:

- **Platform sprawl**: ap accumulated multiple frontends and many layers. The
  repeated failure mode in its notes is "implemented but not wired end-to-end."
  The new game should have one primary browser surface and one orchestration
  spine.
- **Self-modification as MVP**: ap has infrastructure for extension compilation,
  crystallization, learning, and persistent agent evolution, but prior research
  says the loops were not wired into real LLM cognition end-to-end. Treat this as
  later progression mechanics, not a launch dependency.
- **Using ap as the runtime base**: the game already has a cleaner runtime in
  sq-sandbox plus SCUD and Shen. ap's substrate and UI ideas are useful, but
  importing the full CL platform would pull in too much architecture before the
  product loop is proven.

Best role for ap: use it as a design mine and maybe lift/refactor selected
SolidJS UI components or visual patterns. Do not make it the central runtime for
the first version.

## Product Shape

The likely product is a separate app above sq-sandbox:

1. **Spec Lobby**: The player works with a guide NPC to clarify intent. The app
   produces a PRD, skills/pattern selections, acceptance criteria, and hidden
   scenario holdouts.
2. **Task Graph Forge**: SCUD turns the PRD into a DAG. The user can inspect,
   merge, split, reorder, or assign tasks before execution.
3. **Factory Floor**: Each ready task becomes a station. Agent NPCs are assigned
   to stations. Each station owns a sq-sandbox workspace, capability modules, and
   a visible status.
4. **Live Work Cells**: For GUI/browser tasks, the station embeds the noVNC URL.
   The user can click into the sandbox desktop or ask the browser-open endpoint
   to navigate somewhere.
5. **Gate Rooms**: Shen gates, tests, builds, lint, scenario runs, and reviews
   appear as deterministic checkpoints. Failures generate structured repair
   prompts and can rewind to the last snapshot.
6. **Human Dialogue Gates**: Ambiguity or product decisions pause execution and
   surface as NPC conversations with explicit choices.
7. **Replay and Branching**: Event logs plus snapshots let the user replay a run,
   fork alternate strategies, compare diffs, and promote the winning path.

## NPC Model

NPCs should be role-bound, not model-bound:

- **Spec Guide**: interviews the user, writes PRDs, identifies missing choices.
- **Architect**: builds the pipeline graph and SCUD task plan.
- **Pattern Librarian**: attaches skills, examples, and house patterns.
- **Builder**: runs implementation tasks inside sandboxes.
- **Gatekeeper**: runs Shen, tests, builds, scenario holdouts, and explains
  failures.
- **Reviewer**: looks at diffs and risk, but does not replace deterministic
  gates.
- **Release Manager**: packages commits, PRs, docs, and deployment evidence.

Each NPC should have a state machine visible to the user: idle, thinking,
working, blocked, waiting-human, failed-gate, retrying, done.

## Technical Spine

Minimal viable architecture:

```text
Browser UI
  -> Game/orchestrator API
    -> SCUD CLI / library for task graph
    -> sq-sandbox API for workspaces, GUI, snapshots, task runner
    -> Shen-Backpressure CLI for deterministic gates
    -> LLM adapters for guide/NPC planning
```

The orchestrator owns product concepts: projects, players, NPCs, missions,
stations, dialogue, graph state, scenario holdouts, and replay. sq-sandbox owns
runtime isolation and visual workspaces. SCUD owns task decomposition. Shen owns
proof/gate backpressure.

## MVP Slice

The smallest compelling demo:

1. User enters: "Build a tiny browser game with these rules."
2. Spec Guide creates a PRD and asks 3 clarifying questions.
3. SCUD generates a task DAG from the PRD.
4. The app creates one sandbox per ready task or one sandbox per NPC.
5. Builder NPC runs implementation through sq-sandbox task runner.
6. Gatekeeper runs `sb gates` or a normal test/build gate.
7. Browser UI shows task graph, SSE event log, noVNC iframe, and gate status.
8. On failure, the app snapshots, asks the user or Builder to repair, and re-runs.
9. On pass, the app marks the SCUD task done and unlocks the next wave.

## Open Questions

- Should the orchestrator reuse sq-sandbox's built-in task runner, or run its own
  external NPC loop that calls `exec`, `exec-bg`, `snapshot`, and `restore`
  directly?
- Should SCUD be invoked as a CLI, imported as a Go library, or exposed through
  MCP?
- Should Attractor/Kilroy DOT graphs be the top-level execution graph, with SCUD
  as task-detail generation, or should SCUD be the top-level graph?
- Where should holdout scenarios live so implementation agents cannot see them?
- Should GUI/noVNC be a viewport for every NPC, or only for tasks requiring
  visual/browser interaction?
- Which gate failures should rewind sandbox state automatically, and which should
  preserve the failed workspace for inspection?

## Related Research

- `thoughts/shared/research/2026-02-22-squashclaw-synthesis-background.md`
- `thoughts/shared/plans/2026-02-22-nanosquash-implementation.md`
- `thoughts/shared/research/2026-04-27-porteux-style-solid-layer-evaluation.md`
- `thoughts/shared/plans/plan-firecracker-and-security.md`
- `thoughts/shared/research/2026-02-15-ap-homoiconic-architecture.md`
- `/Users/reuben/projects/ap/thoughts/shared/research/2026-03-16-autopoiesis-use-case-analysis.md`
- `/Users/reuben/projects/ap/thoughts/shared/research/2026-03-16-frontend-inventory-holodeck-nexus-status.md`
- `/Users/reuben/projects/ap/thoughts/shared/research/2026-03-13-self-modification-vision-vs-implementation.md`
- `/Users/reuben/projects/ap/thoughts/shared/research/2026-03-12-agent-command-center-vision-mapping.md`
- `/Users/reuben/projects/ap/thoughts/shared/plans/2026-03-17-compelling-live-demo.md`

## External References

- Simon Willison, "How StrongDM's AI team build serious software without even
  looking at the code": https://simonwillison.net/2026/Feb/7/software-factory/
- StrongDM Factory: https://factory.strongdm.ai/
- StrongDM Attractor: https://github.com/strongdm/attractor
- Dan Shapiro's Kilroy: https://github.com/danshapiro/kilroy
- Dan Shapiro, "The Five Levels": https://www.danshapiro.com/blog/2026/01/the-five-levels-from-spicy-autocomplete-to-the-software-factory/
