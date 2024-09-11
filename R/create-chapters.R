#' Create Chapters
#'
#' @param pages A data frame of pages.
#'
#' @return A summarised data frame with chapters.
#' @export
create_chapters <- function(pages) {
  pages |>
    dplyr::mutate(month = lubridate::month(ymd)) |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      tex_chapter = paste0(tex_w_heading, collapse = "\n")
    ) |>
    dplyr::mutate(
      tex_chapter = paste0("\\chapter{", lubridate::month(month, label = TRUE, abbr = FALSE), "}", tex_chapter)
    )
}
