unite_npercent <- function(.data, newcol, ncol, pcol, remove = TRUE) {
  newcol <- quo_name(enquo(newcol))
  ncol <- enquo(ncol)
  pcol <- enquo(pcol)

  #print(pcol)
  #percent <- round(substitute(.data[[pcol]], list(.data = .data)), 2)
  mutate(.data, !! newcol := paste0(!! ncol, " (", round(!! pcol, 2), ")"))
}

#' @export
pretty_table <- function(tbl, matriculation_col) {
  matriculation_col <- enquo(matriculation_col)
  pander(tbl %>%
           ungroup() %>%
           unite_npercent(`# persist (%)`, n_semester_persist, 100*persist) %>%
           unite_npercent(`# degree (% yield)`, n_year_degree, 100*yield) %>%
           mutate(grad_rate = round(100*grad_rate, 2)) %>%
           select(major = !!matriculation_col,
                  2, `#` = n,
                  `# persist (%)`,
                  `# degree (% yield)`,
                  `grad rate %` = grad_rate))
}
