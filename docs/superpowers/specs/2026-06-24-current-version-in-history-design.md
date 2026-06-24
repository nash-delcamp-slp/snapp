# Design: Include the current (live, on-disk) version in file history

Date: 2026-06-24
Status: Approved (pending spec review)

## Problem

The timeline shows only *historical* snapshots of a file:

- `GitSource$list_snapshots()` (`R/class-source-git.R`) uses `git log --follow`, so it lists
  only **committed** versions. The working-tree copy (uncommitted edits, or a file that simply
  differs from `HEAD`) never appears.
- `SnapshotDirSource$list_snapshots()` (`R/class-source-snapshotdir.R`) globs only the snapshot
  directories (e.g. `.zfs/snapshot/<stamp>/`); the live file at the dataset root is never
  enumerated.

`build_timeline()` (`R/timeline.R`) merges whatever the sources return, so the newest dot is the
latest commit/snapshot — not what is on disk right now. The user wants the **current on-disk
version** to appear as the most recent point in history.

## Decisions

- **Always show the live version**, even when it is byte-identical to the latest commit/snapshot.
  It is a predictable "now" anchor; the carousel's identical/differs badge already makes
  redundancy obvious.
- **One shared "Current" dot**, independent of how many sources are active. The live file is the
  same bytes on disk regardless of which source it is viewed through, so it is modeled as a
  timeline-level concept, not owned by any source.
- **Timestamp = file mtime**, not `Sys.time()`, so the dot sits truthfully on the
  time-proportional axis at the moment the file was last modified.

## Approach (chosen)

**Timeline-level live entry.** `build_timeline()` appends a single synthetic "Current" row, and
`fetch_content()` special-cases that row to read bytes directly from the live path. The live
version stays orthogonal to the source abstraction and never touches the file-browser navigator.

### Rejected alternatives

- **Dedicated `LiveSource` in `active_sources()`** — would reuse `read_file` cleanly, but the file
  browser (`R/mod_file_browser.R`) iterates `active_sources()` for `root()`, `list_children()`, and
  search. A live pseudo-source has no sensible root/children and would pollute navigation.
- **Per-source working-copy rows** — each source emits its own live row. Contradicts the
  "one shared dot" decision and duplicates identical entries when sources overlap (a path that is
  both a git repo and on a ZFS dataset).

## Detailed design

### 1. Data model & timeline (`R/timeline.R`)

Define reserved constants in one place:

```r
LIVE_SOURCE <- "—current—"   # reserved source label; must not collide with a real source name
LIVE_ID     <- "live"
```

`build_timeline(path, sources)` performs its normal merge, then **appends one row when `path`
exists and is a regular file on disk**:

- `source = LIVE_SOURCE`, `id = LIVE_ID`, `label = "Current"`
- `time = file mtime` (`file.info(path)$mtime`, as `POSIXct`)

The existing `order(time)` and `!duplicated(c("source", "id"))` logic places the row last by time
and keeps it singular. If there is no history at all but the file exists on disk, the result is a
lone "Current" dot — a sensible baseline rather than an empty timeline.

The live row is appended only after the per-source merge, so a source failure (which emits a
`snapp_source_error` warning and is skipped) does not suppress the live entry.

### 2. Reading live content (`R/content.R`)

`fetch_content(path, entry, sources)` gets a guard at the top:

```r
if (identical(entry$source, LIVE_SOURCE)) {
  bytes <- readBin(path, "raw", n = file.info(path)$size %||% 0)
  type  <- <classify>(bytes, path)
  lines <- <text lines if type == "text">
  return(list(type = type, bytes = bytes, lines = lines, hash = content_hash(bytes)))
}
# else: existing behavior — Find source by name, src$read_file(path, id)
```

- Classification reuses the base `SnapshotSource$classify()` logic (identical across subclasses):
  text / image / binary by extension and content sniffing.
- **Requirement (must not regress):** the live branch computes `hash = content_hash(bytes)`,
  the same hash function used for snapshot/commit content. This is what drives the `#hash` label
  and the identical/differs badge.
- The returned shape (`list(type, bytes, lines, hash)`) is identical to the source branch, so the
  carousel's text-diff / image / binary panes need no changes.

### 3. Carousel styling & labeling (`R/mod_carousel.R`, `inst/app/www/snapp.css`)

- **Color token:** map `LIVE_SOURCE -> "src-live"` in the `src_classes` reactive
  (`R/mod_carousel.R:42`). Add CSS rules for `.tl-dot.src-live` and `.ov-dot.src-live` in
  `inst/app/www/snapp.css` with a distinct accent (e.g. green) so "now" reads differently from the
  git/zfs dot colors. Without this the live dot would fall back to grey `src-unknown`.
- **Frame label:** the git-only SHA prefix (`R/mod_carousel.R:144`) keys off the `src-gitsource`
  token, so the live entry naturally shows just its `"Current"` label + time, with no SHA prefix.

### Hash & comparison behavior (already satisfied by sections 1–3)

- **Content hash (`#…`):** `side_label()` (`R/mod_carousel.R:207-209`) appends `#<hash>` for any
  non-null pane content. The live entry returns a standard content list with `hash`, so the
  "Current" pane shows its `#hash` exactly like commit/snapshot panes.
- **Identical/differs badge:** the badge (`R/mod_carousel.R:216-218`) compares `left$hash` vs
  `right$hash` and is source-agnostic. Pinning "Current" against a commit/snapshot yields a correct
  ✓ identical / "content differs" verdict automatically.

No changes are needed to pinning, the brush/overview density strip, or the date-range slider — they
operate on timeline rows generically, and the live row is just another row.

## Testing

Using the existing testthat 3 + withr fixtures (temp git repo, temp snapshot dirs):

- `build_timeline()`:
  - live row appended with correct sentinel `source`/`id`, `label = "Current"`, and mtime, when the
    file exists on disk;
  - live row omitted when `path` does not exist on disk (e.g. a file present in `HEAD` but deleted
    from the working tree);
  - live row ordered last by time and surviving dedup;
  - lone "Current" row when the file exists but has no source history.
- `fetch_content()`:
  - live branch reads current on-disk bytes;
  - classifies text / image / binary correctly;
  - returns a non-null `hash`.
- Hash-equality: when live bytes are identical to a snapshot's bytes, the two `hash` values are
  equal (so the badge would read "identical"); when they differ, the hashes differ.

## Out of scope

- Surfacing untracked/new files in the navigator (git lists children via `HEAD`; a brand-new
  uncommitted file would not appear in the browser regardless of this feature).
- Any change to source discovery, the add-source flow, or navigation.
