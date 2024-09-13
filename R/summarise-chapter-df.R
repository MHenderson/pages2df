#' Summarise Chapters
#'
#' @param pages A data frame of pages.
#'
#' @return A data frame of chapters.
#' @export
summarise_chapter_df <- function(pages) {

  month <- lubridate::month(pages$ymd[1])
  month_name <- lubridate::month(pages$ymd[1], label = TRUE, abbr = FALSE) |> as.character()

  X |>
    dplyr::summarise(
      tex_chapter = paste0(tex_w_heading, collapse = "\n")
    ) |>
    dplyr::mutate(
      tex_chapter = paste0("\\chapter{", month_name, "}", tex_chapter)
    ) |>
    dplyr::mutate(
      path = file.path(paste0(month, ".tex"))
    )
}

#' @rdname summarise_chapter_df
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
