# Build a dataset_root with two timestamped snapshot dirs each containing R/model.R
make_snapdir_fixture <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  snap_base <- fs::dir_create(fs::path(root, ".zfs", "snapshot"))
  s1 <- fs::dir_create(fs::path(snap_base, "snap-2026-01-01T00:00:00", "R"))
  s2 <- fs::dir_create(fs::path(snap_base, "snap-2026-02-01T00:00:00", "R"))
  writeLines("v1", fs::path(s1, "model.R"))
  writeLines("v2", fs::path(s2, "model.R"))
  # the live file
  fs::dir_create(fs::path(root, "R"))
  writeLines("v2", fs::path(root, "R", "model.R"))
  root
}

# Build a throwaway git repo with two commits to R/model.R; returns repo path.
make_fixture_repo <- function(env = parent.frame()) {
  skip_if_not(nzchar(Sys.which("git")), "git CLI not available")
  repo <- withr::local_tempdir(.local_envir = env)
  run <- function(...) system2("git", c("-C", repo, ...), stdout = TRUE, stderr = TRUE)
  run("init", "-q")
  run("config", "user.email", "t@t.t")
  run("config", "user.name", "t")
  fs::dir_create(file.path(repo, "R"))
  writeLines(c("fit <- lm(y ~ x)", "tol <- 1e-4"), file.path(repo, "R", "model.R"))
  run("add", "-A")
  withr::with_envvar(
    c(GIT_COMMITTER_DATE = "2020-01-01T00:00:00+0000",
      GIT_AUTHOR_DATE    = "2020-01-01T00:00:00+0000"),
    run("commit", "-q", "-m", "first")
  )
  writeLines(c("fit <- lm(y ~ x)", "tol <- 1e-6", "iter <- 50"), file.path(repo, "R", "model.R"))
  run("add", "-A")
  withr::with_envvar(
    c(GIT_COMMITTER_DATE = "2020-01-02T00:00:00+0000",
      GIT_AUTHOR_DATE    = "2020-01-02T00:00:00+0000"),
    run("commit", "-q", "-m", "second")
  )
  repo
}
