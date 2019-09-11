#' @export
#'
add_composit_vars <- function(.data, student_records) {
  .data %>%
    left_join(student_records %>%
                semi_join(.data, by = "id") %>%
                group_by(id) %>%
                summarize(first_major = dplyr::first(major, order_by = term),
                          last_major = dplyr::last(major, order_by = term),
                          last_term = dplyr::last(term, order_by = term)), by = "id") %>%
    mutate(first_major = grouped_majors(first_major),
           last_major = grouped_majors(last_major)) %>%
    mutate(origin = case_when(first_major == "GE" & !transfer & coe_admit_student_level == 10 ~ "Traditional", # 10 == "Freshman". Probably should get this from the data
                              is_eng_major(first_major) & !transfer & coe_admit_student_level == 10 ~ "Direct",
                              transfer ~ "Ext. Transfer",
                              TRUE ~ "Int. Transfer"),
           representation = if_else(first_generation == "First-Generation" | gender == "Female" | urm == "URM", "Underserved", "Represented"))
}
