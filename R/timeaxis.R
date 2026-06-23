#' Adaptive time-axis ticks for a span.
#' @param from,to POSIXct scalars (from < to).
#' @return tibble(time = POSIXct, label = character) of axis ticks.
#' @noRd
time_axis_ticks <- function(from, to) {
  from <- as.POSIXct(from); to <- as.POSIXct(to)
  if (!is.finite(as.numeric(from)) || !is.finite(as.numeric(to)) || !(to > from)) {
    return(tibble::tibble(time = from, label = format(from, "%Y-%m-%d %H:%M")))
  }
  brks <- pretty(c(from, to), n = 6)
  brks <- brks[brks >= from & brks <= to]
  if (length(brks) == 0) brks <- c(from, to)
  span <- as.numeric(to) - as.numeric(from)              # seconds
  day <- 86400
  fmt <- if (span <= 2 * day)        "%H:%M"              # hours
         else if (span <= 120 * day) "%b %d"              # days / weeks
         else if (span <= 3 * 365 * day) "%b %Y"          # months
         else                        "%Y"                 # years
  tibble::tibble(time = brks, label = format(brks, fmt))
}

#' Map timestamps to 0..100 positions along [from, to].
#' @param times POSIXct vector.
#' @param from,to POSIXct scalars bounding the axis.
#' @return numeric vector in [0, 100] (clamped); all 50 if the span is zero.
#' @noRd
time_positions <- function(times, from, to) {
  rng <- as.numeric(to) - as.numeric(from)
  if (!is.finite(rng) || rng <= 0) return(rep(50, length(times)))
  pmin(100, pmax(0, (as.numeric(times) - as.numeric(from)) / rng * 100))
}
