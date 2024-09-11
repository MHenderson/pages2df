#' A Data Frame of Pages
#'
#' @param pagespath Path to input pages.
#'
#' @return A data frame with columns ymd and text.
#' @export
pages_df <- function(pagespath) {
  tibble::tibble(
    paths = list.files(pagespath, full.names = TRUE)
  ) |>
  dplyr::mutate(
    ymd = lubridate::ymd(gsub(".md", "", basename(paths)))
  ) |>
  dplyr::mutate(
    text = purrr::map_chr(paths, readr::read_file)
  )
}
