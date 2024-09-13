#' Read Morning Pages from Disk
#'
#' @param pagespath Path to morning pages.
#' @param years Years to filter.
#'
#' @return A data frame with columns `ymd` and `text`.
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

#' @rdname pages_df
#' @export
read_pages <- function(pagespath, years = c(2021)) {
  tibble::tibble(
    paths = list.files(
      path = pagespath,
      recursive = TRUE,
      pattern = "[0123456789]{2}_[0123456789]{2}_[0123456789]{2}.md$",
      full.names = TRUE
    )
  ) |>
    dplyr::mutate(
      text = purrr::map_chr(pagespaths, readr::read_file)
    ) |>
    dplyr::mutate(
      ymd = lubridate::ymd(gsub(".md", "", basename(pagespaths)))
    ) |>
    dplyr::filter(lubridate::year(ymd) %in% years)
}
