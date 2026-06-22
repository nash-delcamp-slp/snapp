#' Render a text diff as HTML
#' @param a,b Character vectors (lines) for the two sides.
#' @param mode "side-by-side" or "unified".
#' @return Length-1 HTML character.
#' @export
render_text_diff <- function(a, b, mode = c("side-by-side", "unified")) {
  mode <- match.arg(mode)
  fmt <- if (mode == "side-by-side") "sidebyside" else "context"
  d <- diffobj::diffChr(
    a %||% character(), b %||% character(),
    format = "html", mode = fmt, pager = "off",
    style = list(html.output = "diff.w.style")
  )
  paste(as.character(d), collapse = "\n")
}

#' Build a base64 data URI for image bytes
#' @param bytes raw vector.
#' @param path file path (for mime type).
#' @export
image_data_uri <- function(bytes, path) {
  ext <- tolower(tools::file_ext(path))
  mime <- switch(ext,
    jpg = , jpeg = "image/jpeg", gif = "image/gif", svg = "image/svg+xml",
    bmp = "image/bmp", webp = "image/webp", "image/png")
  sprintf("data:%s;base64,%s", mime, base64enc::base64encode(bytes))
}

#' One-line summary for opaque binary content
#' @param bytes raw vector.
#' @export
binary_summary <- function(bytes) {
  hash <- substr(rlang::hash(bytes), 1, 12)
  sprintf("Binary content \u2014 %d bytes, hash %s", length(bytes), hash)
}
