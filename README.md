# snapp

<!-- badges: start -->
<!-- badges: end -->

**snapp** is a Shiny application for exploring the history of files across time using pluggable snapshot sources. It provides a diff carousel that lets you step through a file's snapshots (git commits, ZFS filesystem snapshots, or any custom source) and compare versions side-by-side or unified.

## Installation

Install from GitHub with `pak` or `remotes`:

```r
# With pak:
pak::pak("nash-delcamp-slp/snapp")

# With remotes:
remotes::install_github("nash-delcamp-slp/snapp")
```

## Usage

```r
snapp::run_app()
```

This launches the Shiny application in your browser. The app auto-discovers applicable snapshot sources for the current working directory.

## How snapshot sources work

All snapshot sources implement the `SnapshotSource` R6 class interface:

| Method | Signature | Returns |
|--------|-----------|---------|
| `root` | `()` | the source's top-level directory for navigation |
| `list_snapshots` | `(path)` | `tibble(id, label, time)` — one row per snapshot containing that file |
| `read_file` | `(path, id)` | `raw` vector of file bytes at that snapshot |
| `list_children` | `(path = NULL)` | `tibble(name, path, type)` — immediate children of a directory (one level; `type` is "dir"/"file"); `NULL` = source root |

The `id` column from `list_snapshots()` is an opaque key passed back to `read_file()`.

The file browser is a lazy directory navigator: it lists one directory level at a time via `list_children`, so large/deep datasets stay responsive.

## Registering a custom source type

Use `register_source_type()` to add a new backend:

```r
library(R6)

# Define a custom source class
MySource <- R6Class("MySource",
  inherit = snapp::SnapshotSource,
  public = list(
    root_path = NULL,
    initialize = function(root, name = NULL) {
      super$initialize(name)
      self$root_path <- root
    },
    root = function() self$root_path,
    list_snapshots = function(path) {
      # Return a tibble with columns: id, label, time
      tibble::tibble(
        id    = "v1",
        label = "Version 1",
        time  = as.POSIXct("2024-01-01")
      )
    },
    read_file = function(path, id) {
      readBin(path, "raw", n = file.info(path)$size)
    },
    list_children = function(path = NULL) {
      # Return a tibble(name, path, type) of immediate children of `path`
      # (path = NULL means the source root). type is "dir" or "file".
      tibble::tibble(name = character(), path = character(), type = character())
    }
  )
)

# Register the type so snapp can instantiate and discover it
snapp::register_source_type(
  "mytype",
  new    = function(params) MySource$new(root = params$root),
  detect = function(path) {
    # Return a list of candidate configs when this source applies to `path`
    if (file.exists(file.path(path, ".mymarker"))) {
      list(list(name = "my source", type = "mytype", params = list(root = path)))
    } else {
      list()
    }
  }
)
```

After registration, instances can be created with `snapp::new_source("mytype", list(root = "/some/path"))` and the `detect` function will run automatically when `snapp::discover_sources()` is called.

## Built-in source types

### `git`

Reads file history from a git repository. Auto-discovered when the working path is inside a git repo.

```r
src <- snapp::new_source("git", list(repo = "/path/to/repo"))
```

### `zfs` (SnapshotDirSource)

Reads point-in-time copies from ZFS `.zfs/snapshot/<stamp>/` directories. Auto-discovered when `.zfs/snapshot/` is found by ascending from the current directory.

```r
src <- snapp::new_source("zfs", list(dataset_root = "/tank/data"))
```

Snapshot versions are timed by each file's own modification time, and copies that share an mtime are collapsed to a single entry (keeping the earliest snapshot of that version); `time_from` is an optional override to parse the timestamp from the snapshot directory name instead.

`SnapshotDirSource` can also be used directly for any filesystem-snapshot layout (not just ZFS):

```r
src <- snapp::SnapshotDirSource$new(
  dataset_root  = "/data/live",
  snapshot_glob = "/data/snapshots/*/",
  time_from     = list(
    regex  = "^(\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2})",
    format = "%Y-%m-%dT%H:%M:%S"
  )
)
```

## Configuration file (`~/.snapp/config.yml`)

snapp reads a YAML config from `~/.snapp/config.yml` (overridable via the `SNAPP_CONFIG` environment variable or `options(snapp.config_path = ...)`).

```yaml
settings:
  default_view: side-by-side   # or "unified"
  auto_discover: true          # auto-detect sources on startup

sources:
  - name: my-project
    type: git
    enabled: true
    params:
      repo: /home/user/projects/my-project

  - name: data-archive
    type: zfs
    enabled: true
    params:
      dataset_root: /tank/data

  - name: custom
    type: mytype
    enabled: true
    params:
      root: /some/path
```

**`settings` keys:**

- `default_view`: Starting diff view mode — `"side-by-side"` (default) or `"unified"`.
- `auto_discover`: If `true`, snapp runs `discover_sources()` on startup to find applicable sources under the working directory.

**`sources[]` keys:**

- `name`: Human-readable label shown in the UI.
- `type`: Registered source type name (e.g. `"git"`, `"zfs"`, or a custom type).
- `enabled`: Boolean; set to `false` to disable without removing.
- `params`: Named list passed to the source constructor.

Use `snapp::read_config()`, `snapp::write_config()`, and `snapp::validate_config()` to manage config programmatically.
