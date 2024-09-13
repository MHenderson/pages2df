#' Augment a Pages Data Frame with LaTeX-friendly Text
#'
#' @param pages A data frame containing a column of 'text' and dates 'ymd'.
#'
#' @return A data frame of pages with new columns, `tex` and `tex_w_heading`
#'  containing the same data as the `text` column but chopped into strings
#'  of a near fixed length (without breaking words), with some special LaTeX
#'  characters escaped and, in the case of `tex_w_heading` with a pre-pended
#'  section heading containing date and time information.
#' @export
create_chapter_df <- function(pages) {
  pages |>
    dplyr::mutate(
      time_label = llinyn::extract_time_label(text),
            text = llinyn::strip_time_headings(text)
    ) |>
    dplyr::mutate(
      tex = llinyn::repair_latex_string(text)
    ) |>
    dplyr::mutate(
      tex = purrr::map_chr(tex, llinyn::gqg, textwidth = 72)
    ) |>
    dplyr::mutate(
      tex_w_heading = paste0("\\section{", llinyn::ymd_text_format(ymd), "}\n\n", "\\hspace*{\\fill}", time_label, "\\vspace{5mm}\n\n", tex)
    )
}
