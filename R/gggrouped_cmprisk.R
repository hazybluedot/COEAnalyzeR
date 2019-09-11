#' @export
gggrouped_cmprisk <- function(.events_data, groupvar, prefix = "grouped_cmprisk", labs = list(), saveTikz = FALSE) {
  groupvar = enquo(groupvar)

  # cr_fit <- with(.events_data,
  #                  cuminc(term_count, event, !! groupvar, cencode = "InProgress"))
  cuminc(.events_data$term_count,
         .events_data$event,
         .events_data[[quo_name(groupvar)]], cencode = "InProgress") ->
    cr_fit

    cr_fit_p <- ggcmprisk(cr_fit) +
    labs(!!! labs) +
    theme_minimal()
  if (saveTikz) {
    base_name <- paste(prefix, quo_name(groupvar), sep = "_")
    ggsavetikz(cr_fit_p, file.path(params$tikz_output, paste0(base_name, ".tex")), standAlone = TRUE, sanitize = TRUE, verbose = FALSE)
  }
  cr_fit_p
}

#' @export
grouped_cmprisk <- function(.events_data, groupvar) {
  groupvar = enquo(groupvar)

  cuminc(.events_data$term_count,
         .events_data$event,
         .events_data[[quo_name(groupvar)]], cencode = "InProgress")
}

#' @export
ggsinglerisk <- function(.data, .event) {
  as_tibble(.data) %>%
    filter(event == .event) %>%
    ggplot(aes(time, est * 100, color = group)) +
    geom_line() +
    #facet_grid(rows = vars(event), as.table = TRUE, scales = "free_y") +
    labs(y = "probability") +
    lims(x = c(0, 16)) +
    scale_x_continuous(breaks = c(4, 8, 12, 16)) + labs(y = "probability") +
    theme_minimal()
}
