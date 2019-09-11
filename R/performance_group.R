attempt_groups <- function(attempts) {
  fct_relevel(case_when(
    attempts == 0 ~ "Alternative",
    attempts == 1 ~ "Oneshot",
    attempts > 1 ~ "Repeat"
  ), "Oneshot")
}

#' @export
grade_attempt_groups <- function(grade, attempts, AP, transfer) {
  fct_relevel(case_when(
    attempts == 0 & (AP == "Yes" | transfer == "Yes") ~ "Alternative",
    attempts == 1 & grade == "AB" ~ "Sailer",
    grade %in% c("DF") | attempts > 1 & grade %in% c("C", "DF") ~ "Struggler",
    attempts > 1 & grade == "AB" | (attempts == 1 & grade == "C") ~ "Plodder",
  ), "Sailer")
}
