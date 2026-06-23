#' Render a text diff as HTML
#' @param a,b Character vectors (lines) for the two sides.
#' @param mode "side-by-side" or "unified".
#' @param a_label,b_label Optional column banner labels for the two sides.
#' @return Length-1 HTML character.
#' @export
render_text_diff <- function(a, b, mode = c("side-by-side", "unified"),
                             a_label = NULL, b_label = NULL) {
  mode <- match.arg(mode)
  fmt <- if (mode == "side-by-side") "sidebyside" else "context"
  args <- list(target = a, current = b, format = "html", mode = fmt, pager = "off",
               style = list(html.output = "diff.w.style"))
  if (!is.null(a_label)) args$tar.banner <- a_label
  if (!is.null(b_label)) args$cur.banner <- b_label
  d <- do.call(diffobj::diffChr, args)
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

#' Short content hash of raw bytes
#' @noRd
content_hash <- function(bytes) substr(rlang::hash(bytes), 1, 12)

#' One-line summary for opaque binary content
#' @param bytes raw vector.
#' @export
binary_summary <- function(bytes) {
  sprintf("Binary content \u2014 %d bytes, hash %s", length(bytes), content_hash(bytes))
}
