# Current Version In History Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Show the current (live, on-disk) version of a file as a single "Current" point at the end of its history timeline.

**Architecture:** The live version is modeled as a timeline-level concept, not owned by any source. `build_timeline()` appends one synthetic "Current" row when the selected file exists on disk; `fetch_content()` special-cases that row to read bytes directly from the live path; the carousel gives it a distinct color token. Nothing touches the file-browser navigator.

**Tech Stack:** R, R6, shiny, fs, tibble, testthat 3 (+ withr), rlang.

## Global Constraints

- Reserved sentinels (define once, reuse everywhere): `LIVE_SOURCE <- "—current—"`, `LIVE_ID <- "live"`. These are package-internal (no `@export`).
- The live entry's content `list` must have the **same shape** as the source branch: `list(type, bytes, lines, hash)`, with `hash = content_hash(bytes)`. This is what drives the `#hash` label and identical/differs badge — it must not regress.
- The live row's timestamp is the file mtime (`file.info(path)$mtime`), not the current wall-clock time.
- Tests are testthat 3 with self-sufficient setup; use `withr::local_tempfile()` for on-disk fixtures so cleanup is automatic.
- Run tests with: `Rscript -e 'devtools::test(filter = "<name>")'` (filter matches the test file name with the `test-` prefix and `.R` suffix removed).

---

### Task 1: Append a "Current" row in `build_timeline()`

**Files:**
- Modify: `R/timeline.R` (define the two constants at the top of the file; append the live row inside `build_timeline()`)
- Test: `tests/testthat/test-timeline.R`

**Interfaces:**
- Consumes: nothing new.
- Produces: package-internal constants `LIVE_SOURCE` (= `"—current—"`) and `LIVE_ID` (= `"live"`); `build_timeline(path, sources)` now returns an extra row `tibble(source = LIVE_SOURCE, id = LIVE_ID, label = "Current", time = <file mtime>)` whenever `path` is a regular file on disk. Used by Task 2 and Task 3.

- [ ] **Step 1: Write the failing tests**

Add to `tests/testthat/test-timeline.R`:

```r
test_that("build_timeline appends a single Current row for an on-disk file", {
  tmp <- withr::local_tempfile(fileext = ".R")
  writeBin(charToRaw("x <- 1"), tmp)
  s <- fake_history(tmp, list(
    list(id = "g1", label = "commit", time = 100, content = "old")
  ), name = "git")

  tl <- build_timeline(tmp, list(s))

  expect_equal(nrow(tl), 2L)
  expect_equal(sum(tl$source == LIVE_SOURCE), 1L)
  live <- tl[tl$source == LIVE_SOURCE, ]
  expect_equal(live$id, LIVE_ID)
  expect_equal(live$label, "Current")
  expect_equal(tl$source[nrow(tl)], LIVE_SOURCE)   # mtime is newest -> sorts last
})

test_that("build_timeline omits the Current row when the path is not on disk", {
  s <- fake_history("/p/a.R", list(
    list(id = "1", label = "x", time = 100, content = "a")
  ), name = "git")

  tl <- build_timeline("/p/a.R", list(s))

  expect_false(LIVE_SOURCE %in% tl$source)
  expect_equal(nrow(tl), 1L)
})

test_that("build_timeline shows a lone Current row when the file has no source history", {
  tmp <- withr::local_tempfile(fileext = ".txt")
  writeBin(charToRaw("hi"), tmp)
  s <- fake_history("/elsewhere/x.txt", list(), name = "git")  # no snapshots for tmp

  tl <- build_timeline(tmp, list(s))

  expect_equal(nrow(tl), 1L)
  expect_equal(tl$source, LIVE_SOURCE)
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'devtools::test(filter = "timeline")'`
Expected: FAIL — `LIVE_SOURCE`/`LIVE_ID` not found, and no Current row is appended (`object 'LIVE_SOURCE' not found`).

- [ ] **Step 3: Implement the constants and the live-row append**

Replace the entire contents of `R/timeline.R` with:

```r
# Reserved sentinels for the synthetic "current on-disk version" timeline row.
# Package-internal; not exported.
LIVE_SOURCE <- "—current—"
LIVE_ID     <- "live"

#' Build a merged, time-sorted timeline for a path across sources
#' @param path Absolute file path (or NULL).
#' @param sources List of SnapshotSource instances.
#' @return tibble(source, id, label, time), sorted ascending by time. Includes a
#'   single synthetic `LIVE_SOURCE` row (the current on-disk version) when `path`
#'   is a regular file on disk. Emits a `snapp_source_error` warning per failing
#'   source and skips it.
#' @export
build_timeline <- function(path, sources) {
  empty <- tibble::tibble(source = character(), id = character(),
                          label = character(), time = as.POSIXct(character()))
  if (is.null(path)) return(empty)

  parts <- lapply(sources, function(src) {
    tryCatch({
      snaps <- src$list_snapshots(path)
      if (nrow(snaps) == 0) return(NULL)
      tibble::tibble(source = src$name, id = snaps$id, label = snaps$label, time = snaps$time)
    }, error = function(e) {
      rlang::warn(
        sprintf("Source '%s' failed: %s", src$name, conditionMessage(e)),
        class = "snapp_source_error", source = src$name, parent = e
      )
      NULL
    })
  })

  out <- do.call(rbind, c(list(empty), parts))

  # Append the current on-disk version as one shared row, independent of sources.
  if (isTRUE(fs::is_file(path))) {
    live <- tibble::tibble(
      source = LIVE_SOURCE, id = LIVE_ID, label = "Current",
      time = as.POSIXct(file.info(path)$mtime, tz = "UTC"))
    out <- rbind(out, live)
  }

  out <- out[order(out$time), , drop = FALSE]
  out <- out[!duplicated(out[c("source", "id")]), , drop = FALSE]
  tibble::as_tibble(out)
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `Rscript -e 'devtools::test(filter = "timeline")'`
Expected: PASS — all timeline tests (existing + 3 new) green. The existing tests use the non-existent path `"/p/a.R"`, so no live row is appended and their expected `source`/`id` vectors are unchanged.

- [ ] **Step 5: Commit**

```bash
git add R/timeline.R tests/testthat/test-timeline.R
git commit -m "feat(timeline): append a Current row for the live on-disk file"
```

---

### Task 2: Read live content in `fetch_content()`

**Files:**
- Modify: `R/content.R` (`fetch_content()` gets a `LIVE_SOURCE` branch)
- Test: `tests/testthat/test-content.R`

**Interfaces:**
- Consumes: `LIVE_SOURCE`, `LIVE_ID` from Task 1; `content_hash()` from `R/diff.R`; `SnapshotSource` R6 class (its concrete `classify()` method) and `%||%` from `R/utils.R`.
- Produces: `fetch_content(path, entry, sources)` returns the same `list(type, bytes, lines, hash)` for a live entry (`entry$source == LIVE_SOURCE`) by reading bytes directly from `path` — works even when `sources` is empty.

- [ ] **Step 1: Write the failing tests**

Add to `tests/testthat/test-content.R`:

```r
test_that("fetch_content reads the live on-disk file for the Current entry", {
  tmp <- withr::local_tempfile(fileext = ".R")
  writeBin(charToRaw("line1\nline2"), tmp)

  c1 <- fetch_content(tmp, list(source = LIVE_SOURCE, id = LIVE_ID), list())

  expect_equal(c1$type, "text")
  expect_equal(c1$lines, c("line1", "line2"))
  expect_equal(c1$hash, content_hash(c1$bytes))
  expect_equal(nchar(c1$hash), 12L)
})

test_that("fetch_content classifies a live binary file (NUL bytes) and emits no lines", {
  tmp <- withr::local_tempfile(fileext = ".bin")
  writeBin(as.raw(c(0x00, 0x01, 0x02)), tmp)

  c1 <- fetch_content(tmp, list(source = LIVE_SOURCE, id = LIVE_ID), list())

  expect_equal(c1$type, "binary")
  expect_null(c1$lines)
})

test_that("live and snapshot bytes that match produce equal hashes", {
  tmp <- withr::local_tempfile(fileext = ".txt")
  writeBin(charToRaw("same"), tmp)
  s <- fake_history(tmp, list(
    list(id = "1", label = "x", time = 1, content = "same")
  ), name = "git")

  live <- fetch_content(tmp, list(source = LIVE_SOURCE, id = LIVE_ID), list(s))
  snap <- fetch_content(tmp, list(source = "git", id = "1"), list(s))

  expect_equal(live$hash, snap$hash)
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'devtools::test(filter = "content")'`
Expected: FAIL — the live entry falls into the existing branch and errors with `No active source named "—current—"` (and the empty-`sources` case has nothing to find).

- [ ] **Step 3: Implement the live branch**

Replace the entire contents of `R/content.R` with:

```r
#' Fetch and classify content for a timeline entry
#'
#' @param path Absolute file path.
#' @param entry A list/row with `source` (name) and `id`. When `source` is the
#'   reserved `LIVE_SOURCE`, bytes are read directly from the live on-disk `path`.
#' @param sources List of active SnapshotSource instances.
#' @return list(type, bytes, lines, hash).
#' @export
fetch_content <- function(path, entry, sources) {
  if (identical(entry$source, LIVE_SOURCE)) {
    bytes <- readBin(path, "raw", n = file.info(path)$size %||% 0)
    type  <- SnapshotSource$new()$classify(bytes, path)
  } else {
    src <- Find(function(s) identical(s$name, entry$source), sources)
    if (is.null(src)) cli::cli_abort("No active source named {.val {entry$source}}.")
    bytes <- src$read_file(path, entry$id)
    type  <- src$classify(bytes, path)
  }
  lines <- if (identical(type, "text")) {
    strsplit(rawToChar(bytes), "\n", fixed = TRUE)[[1]]
  } else NULL
  list(type = type, bytes = bytes, lines = lines, hash = content_hash(bytes))
}
```

(`SnapshotSource$new()` is valid: its `initialize(name = NULL)` takes no required args, and `classify()` does not use any instance state — it reuses the exact same text/image/binary logic the real sources use.)

- [ ] **Step 4: Run the tests to verify they pass**

Run: `Rscript -e 'devtools::test(filter = "content")'`
Expected: PASS — all content tests (existing + 3 new) green.

- [ ] **Step 5: Commit**

```bash
git add R/content.R tests/testthat/test-content.R
git commit -m "feat(content): read live on-disk bytes for the Current timeline entry"
```

---

### Task 3: Distinct color token for the live dot

**Files:**
- Modify: `R/mod_carousel.R` (the `src_classes` reactive, ~lines 42-49)
- Modify: `inst/app/www/snapp.css` (add `.tl-dot.src-live` and `.ov-dot.src-live` rules)
- Test: `tests/testthat/test-mod_carousel.R`

**Interfaces:**
- Consumes: `LIVE_SOURCE` from Task 1.
- Produces: `src_classes()` reactive now maps `LIVE_SOURCE -> "src-live"` (in addition to the per-source tokens), so the live dot renders with the `src-live` CSS class instead of falling back to grey `src-unknown`.

- [ ] **Step 1: Write the failing test**

Add to `tests/testthat/test-mod_carousel.R`:

```r
test_that("src_classes maps the live source to the src-live token", {
  shiny::testServer(mod_carousel_server, args = carousel_args(make_tl(3)), {
    session$flushReact()
    expect_equal(src_classes()[[LIVE_SOURCE]], "src-live")
  })
})
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `Rscript -e 'devtools::test(filter = "mod_carousel")'`
Expected: FAIL — `src_classes()[[LIVE_SOURCE]]` is `NULL` (the live source is not in the map), so `expect_equal` errors on the `NULL` result.

- [ ] **Step 3: Add the live token to `src_classes`**

In `R/mod_carousel.R`, replace the `src_classes` reactive (currently at lines 42-49):

```r
    src_classes <- shiny::reactive({
      srcs <- active_sources()
      if (length(srcs) == 0) return(stats::setNames(character(0), character(0)))
      stats::setNames(
        vapply(srcs, function(s) paste0("src-", tolower(class(s)[[1]])), character(1)),
        vapply(srcs, function(s) s$name, character(1))
      )
    })
```

with:

```r
    src_classes <- shiny::reactive({
      srcs <- active_sources()
      base <- if (length(srcs) == 0) {
        stats::setNames(character(0), character(0))
      } else {
        stats::setNames(
          vapply(srcs, function(s) paste0("src-", tolower(class(s)[[1]])), character(1)),
          vapply(srcs, function(s) s$name, character(1))
        )
      }
      c(base, stats::setNames("src-live", LIVE_SOURCE))   # one shared token for the live dot
    })
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `Rscript -e 'devtools::test(filter = "mod_carousel")'`
Expected: PASS — the new test and all existing carousel tests are green (existing tests don't assert on the size of `src_classes()`).

- [ ] **Step 5: Add the CSS color rules**

In `inst/app/www/snapp.css`, immediately after the existing `.ov-dot.src-snapshotdirsource` rule (line 10), add:

```css
.ov-dot.src-live { background: var(--snapp-live, #28a745); }
```

And immediately after the existing `.tl-dot.src-unknown` rule (line 24), add:

```css
.tl-dot.src-live { background: #28a745; }
```

(Green distinguishes the "now" dot from the git orange and zfs blue. There is no test for CSS; it is verified visually in Task 4.)

- [ ] **Step 6: Commit**

```bash
git add R/mod_carousel.R inst/app/www/snapp.css tests/testthat/test-mod_carousel.R
git commit -m "feat(carousel): distinct color token for the live Current dot"
```

---

### Task 4: Full-suite verification and manual check

**Files:** none (verification only).

- [ ] **Step 1: Run the entire test suite**

Run: `Rscript -e 'devtools::test()'`
Expected: PASS — all test files green, no warnings introduced.

- [ ] **Step 2: Manual sanity check in a real session**

Run: `Rscript -e 'devtools::load_all("."); snapp::run_app()'` and, against a git repo and/or a snapshot dir:
- select a file with history and an uncommitted edit; confirm a green "Current" dot appears at the right end of the timeline;
- pin "Current" on one side and a commit/snapshot on the other; confirm the `#hash` labels render and the identical/differs badge is correct;
- confirm the navigator (file browser) is unchanged.

Expected: the "Current" dot is present, distinctly colored, hashes/badge work, navigation unaffected.

- [ ] **Step 3: Final commit (if any cleanup was needed)**

```bash
git add -A
git commit -m "test: verify current-version-in-history end to end"
```
(Skip if there is nothing to commit.)
