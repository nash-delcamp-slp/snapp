# snapp — File Snapshot Comparison App — Design

**Date:** 2026-06-22
**Status:** Approved design, pending implementation plan

## Overview

`snapp` is an R package that ships a Shiny application for exploring how files
change over time. It pulls file contents from different points in time through
**pluggable "snapshot source" methods**. Git is supported out of the box; a
generalized filesystem-snapshot source (the canonical use being ZFS `.zfs/snapshot`)
is also built in. Users can register their own source types.

A single file can be backed by more than one source (e.g. a git repo whose working
tree also lives on ZFS snapshots). snapp merges every enabled source's snapshots for
a file into **one combined, time-ordered timeline**, and lets the user walk that
history in a **diff carousel**.

## Goals

- Explore a file's history over time across one or more snapshot sources.
- A modular source system: new methods are added by registering an R6 subclass.
- Combined timelines: multiple sources contribute to one file's history.
- A diff carousel that steps through consecutive snapshots **and** compares two
  arbitrary snapshots.
- Handle text (rich diffs) and images (visual comparison); degrade gracefully on
  other binaries.
- Configuration via `~/.snapp/config.yml`, editable in-app.

## Non-goals

- Editing or restoring files (read-only exploration).
- Hosting/multi-tenant concerns: each user runs their own instance via `run_app()`
  on the shared Posit Workbench server.
- Building snapshots — snapp only reads what sources expose.

## Deployment model

Packaged as an installable R package. Each user installs it and launches their own
instance with `run_app()` on the shared Posit Workbench (Linux) server. The user
running the app may not have direct access to the global snapshot store, so each
source does its own discovery (e.g. globbing `/misc/.zfs/snapshot/*`).

## Architecture

Two layers, with a hard boundary between them:

### Domain layer (framework-free, `R/`, unit-testable without Shiny)

Holds all the interesting logic. A Shiny module never touches git / ZFS / the
filesystem directly — it calls domain functions and renders the result.

- **`SnapshotSource` (abstract R6)** — `class-source.R`
  - Field: `name`.
  - Abstract methods:
    - `list_snapshots(path)` → `tibble(id, label, time, ...)`
    - `read_file(path, id)` → `character` (lines) or `raw`
    - `list_tree(path, id = NULL)` → `tibble(path, type)`
  - Shared helper for classifying content type (text / image / other).
- **`GitSource`** — `class-source-git.R`, `gert`-backed. `list_snapshots` = git log
  for the path → `(id = sha, label = summary, time = commit time)`; `read_file` =
  blob at `sha:relpath`; `list_tree` = `git ls-tree`. Param: `repo`.
- **`SnapshotDirSource`** — `class-source-snapshotdir.R`. Generalized
  filesystem-snapshot source driven entirely by config:
    - `dataset_root` — live path the snapshots mirror; used to compute a file's
      path relative to each snapshot root.
    - `snapshot_glob` — glob expanding to the set of point-in-time root dirs.
    - `time_from` — derive each snapshot's `time`: `{regex, format}` applied to the
      dir name, falling back to directory mtime if unspecified.
  - Given a requested file: `relpath = path − dataset_root`; for each snapshot dir,
    check `<snapshot_dir>/<relpath>`. `list_snapshots` emits one entry per dir where
    it exists; `read_file` reads that copy; `list_tree` unions the dir listings.
  - The `zfs` type is this class registered with `.zfs/snapshot` default params; the
    user can still override the globs. Generalizes to NetApp `.snapshot`, Time
    Machine, or any folder-of-timestamped-copies layout.
- **Source registry** — `registry.R`: `register_source_type(type, generator)`,
  `source_types()`, `new_source(type, params)`. Built-ins (`git`, `zfs`) registered
  in `.onLoad`. Users register custom types from their own startup code.
- **Config** — `config.R`: `config_path()` (default `~/.snapp/config.yml`,
  overridable), `read_config()`, `write_config()`, `default_config()`,
  `validate_config()`.
- **Timeline** — `timeline.R`: `build_timeline(path, sources)` →
  merged, time-sorted `tibble(source, id, label, time, type)`. Isolates per-source
  errors (a failing source is dropped with a warning; others proceed).
- **Tree** — `tree.R`: `merge_tree(path, sources)` → unioned directory tree for the
  browser.
- **Content + diff** — `content.R`, `diff.R`: `fetch_content(entry, sources)`;
  `render_text_diff(a, b)` via `diffobj`; `compare_images(a, b)`; binary fallback
  (size + hash).

### Shiny layer (golem, `R/`)

A thin reactive skin: `run_app.R`, `app_ui.R`, `app_server.R`, `app_config.R`, plus
modules:

- **`mod_sources`** — sidebar: configured sources with enable toggles; "add/edit
  source" button opens a modal (type dropdown from the registry + dynamic param
  fields).
- **`mod_file_browser`** — search box + tree (union of enabled sources); emits the
  selected file path.
- **`mod_carousel`** — the hybrid diff carousel (see UX below).
- **`mod_settings`** — settings modal that edits `config.yml` via a form, writing
  through `write_config()`.

Assets in `inst/app/www/`: CSS for the timeline/carousel; minimal JS only if the
scrubber needs it (otherwise a bslib/Shiny input).

### Stack

golem + bslib (Bootstrap 5) + R6 + `gert` (git) + `diffobj` (text diffs) + `yaml`.

## UX — the diff carousel (hybrid)

Centerpiece. Left rail = source toggles + file tree/search. Main area = two content
panes with big ◀ ▶ chevrons and a source-colored timeline scrubber beneath (each dot
is a snapshot; color encodes its source).

- **Default (stepping):** ◀ ▶ step through consecutive snapshots that touched the
  file. Left pane = previous snapshot, right pane = current. The classic carousel.
- **Pinned (arbitrary compare):** click **📌 Pin baseline** on a frame to lock the
  left pane as point A. ◀ ▶ then moves point B to *any* snapshot — comparing two
  arbitrary points. Unpin returns to consecutive stepping.
- Clicking any timeline dot jumps directly to it.
- A view toggle switches side-by-side vs unified (default from config).
- Content rendering by type: text → rich diff; image → visual compare; other binary
  → size/hash summary.

## Config schema (`~/.snapp/config.yml`)

```yaml
settings:
  default_view: side-by-side   # or "unified"
sources:
  - name: "project git"
    type: git
    enabled: true
    params: { repo: "/home/me/proj" }
  - name: "misc zfs"
    type: zfs
    enabled: true
    params:
      dataset_root: "/misc"
      snapshot_glob: "/misc/.zfs/snapshot/*"
      time_from: { regex: "snap-(.*)", format: "%Y-%m-%dT%H:%M:%S" }
```

## Data flow & reactive state

**Startup:** `run_app()` → `read_config()` (creates default if missing) → for each
`sources` entry, `new_source(type, params)` via the registry (invalid/unknown entries
skipped with a logged warning) → instances held in `sources_rv` with their `enabled`
flags.

**Core reactive chain** (in `app_server`, passed to modules):

- `active_sources()` — subset of `sources_rv` toggled on in `mod_sources`.
- `selected_path()` — `reactiveVal` set by `mod_file_browser`. The tree is
  `reactive(merge_tree(active_sources()))`, recomputed on toggle change.
- `timeline()` — `reactive(build_timeline(selected_path(), active_sources()))`.
  Empty path → `NULL` → carousel empty state.
- Carousel state (in `mod_carousel`):
  - `idx` — `reactiveVal`, current frame (defaults to newest entry on a new timeline).
  - `baseline` — `reactiveVal(NULL)`; set by pin, cleared by unpin.
  - Left pane = unpinned → `timeline()[idx-1]`; pinned → `timeline()[baseline]`.
    Right pane = `timeline()[idx]`.
  - ◀ ▶ and dot clicks update `idx` (clamped). Pinning freezes the left pane.
- Pane contents from `fetch_content(entry, active_sources())`, routed by type and
  rendered per the active view mode.

**Caching:** `fetch_content` memoizes on `(source, id, path)` for the session;
`list_snapshots` caches per `(source, path)` until sources toggle.

**Write path:** `mod_settings` / add-source modal mutate an in-memory config →
`validate_config()` → `write_config()` → refresh `sources_rv`. Config edits take
effect live, no restart.

## Error handling

Guiding rule: **source isolation — one bad source never takes down the app.**

- **Per-source failures:** `build_timeline` and `merge_tree` wrap each source call in
  `tryCatch`. A source that errors (unhandled path, not a repo, empty glob) is dropped
  from the merge and surfaced as a dismissible notification; remaining sources render.
- **Empty states:** no enabled sources → prompt to enable/add one; file not in any
  source → "no history" panel; single snapshot → content shown, diff controls disabled.
- **Content edge cases:** unreadable blob → pane placeholder; opaque binary →
  size/hash summary; image → preview/compare; very large file → truncate with notice.
- **Config errors:** `validate_config()` on read and before every write. Invalid
  config → boot on `default_config()` and surface errors in the settings modal; never
  silently overwrite a malformed user-written file.
- **Messaging:** domain layer uses `cli` / `rlang::abort` with classed conditions;
  the Shiny layer catches and translates to `showNotification`.

## Testing

testthat 3e, domain layer first (guided by the `testing-r-packages` skill). Tests are
substantive, not placeholder.

- **Sources:** a `FakeSource` (in-memory snapshots) for deterministic timeline/tree
  tests. `GitSource` against a tiny fixture repo built in-test with `withr` temp dir +
  `gert` init/commits. `SnapshotDirSource` against a temp dir tree mimicking
  `.zfs/snapshot/<stamp>/…`, asserting `relpath` mapping and `time_from` parsing.
- **Timeline:** merge ordering, multi-source interleaving, dedup, and the per-source
  error-isolation path (inject a throwing fake; assert it's dropped, others survive).
- **Config:** round-trip read/write, default creation, validation rejects.
- **Registry:** register/instantiate, unknown-type error.
- **Diff/content:** type classification and text-diff output shape.
- **Shiny modules:** `testServer` for `mod_carousel` reactive logic — index clamping,
  pin/unpin pane selection, default-to-newest — with an injected timeline.

## Open questions / deferred

- Image comparison interaction detail (side-by-side vs overlay/slider) — decide during
  carousel implementation.
- Whether to add an optional class-level `detect(path)` hook to suggest applicable
  sources when adding one (discoverability nicety) — deferred, not in core read path.
