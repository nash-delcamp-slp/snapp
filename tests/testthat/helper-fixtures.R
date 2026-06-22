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
