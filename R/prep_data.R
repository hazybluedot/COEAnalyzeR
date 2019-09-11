#' @import tidyverse
#' @import vtir
#' @import vtirdata

library(tidyverse)

library(vtir)
#library(racadia)
library(vtirdata)

write_data <- function() {
  prep_data()
  cs_intro_data()
}

group_grades <- function(grades) {
  fct_relevel(fct_other(fct_collapse(grades, AB = c("A", "A-", "B+", "B", "B-"),
               C = c("C+", "C", "C-"),
               DF = c("D+", "D", "D-", "F", "F *", "W")),
            keep = c("AB", "C", "DF")), "AB", "C", "DF")
}

#' @export
prep_data <- function() {
  vtir_degrees %>%
    collapse_AOE() ->
    vtir_degrees

  vtir_terms %>%
    collapse_AOE() ->
    vtir_terms

  vtir_courses %>%
    filter(grade == "AP") %>%
    group_by(id, subject) %>%
    summarize(ap_credit = 1) %>%
    filter(subject %in% c("CS", "MATH", "PHYS")) %>%
    spread(subject, ap_credit) ->
    #mutate(CS = replace(CS, is.na(CS), 0)) ->
    ap_credits

  vtir_terms %>%
    filter(student_level < 69, college_code == 5 | major == "CEM") ->
    coe_student_records

  max_degree_term <- racadia::as_term(max(vtir_degrees$term_degree, na.rm = TRUE))
  max_term <-  racadia::as_term(max(vtir_terms$term))

  # students ever in the COE as an undergrad, not ever a transfer student
  vtir_students %>%
    filter_coe_students(coe_student_records) ->
    coe_undergrad_data

  # subset of the student records that match people selected as eng undergraduate students.
  vtir_terms %>%
    filter(student_level <= 69) %>% # filter by student_level only as some students may be undergraduates in the COE and then go on as a undergrad or graduate student in another department, and we want to see what departments those are
    inner_join(coe_undergrad_data %>% select(id, coe_admit_term), by = "id") %>%
    filter(term >= coe_admit_term) ->
    coe_undergrad_records

  # assign term number to each student-term
  coe_undergrad_records %>%
    racadia::enrolled_time(id, term) ->
    student_time

  coe_undergrad_records %>%
    left_join(student_time, by = c("id", "term")) ->
    #select(-gender, -urm, -first_generation, -tuition, -utime) ->
    coe_undergrad_records

  vtir_degrees %>%
    filter_coe_degrees(coe_undergrad_data) ->
    coe_vtir_degrees

  vtir_terms %>%
    semi_join(coe_undergrad_records, by = "id") %>%
    filter(major == "CS") ->
    ever_in_cs
  ## Helper closures to make certain join+filtering operations more managable
  # graduation window is a filter to restrict terms to those that are early enough to allow for a specified graduation window in years.
  graduation_window <- graduation_window_fct(max_degree_term)
  matriculation_degrees <- matriculation_degrees_fct(coe_vtir_degrees)
  has_degree <- has_degree_fct(coe_degree_data)

  save(coe_undergrad_data, file = "data/coe_undergrad_data.rda")
  save(coe_undergrad_records, file = "data/coe_undergrad_records.rda")
  save(coe_vtir_degrees, file = "data/coe_vtir_degrees.rda")
  save(ap_credits, file = "data/ap_credits.rda")
  save(graduation_window, matriculation_degrees, has_degree, max_degree_term, max_term, act2sat, file = "R/sysdata.rda")
}

#' @export
graduation_window <- function(term_enter, window) {
  graduation_window(term_enter, window);
}

cs_intro_data <- function() {
  bool2factor <- function(.x, na.value = NA) {
    if (!is.na(na.value)) {
      .x[is.na(.x)] <- na.value
    }
    factor(.x, c(TRUE, FALSE), labels = c("Yes", "No"))
  }

  valid_grades <- c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D", "D-", "F", "W")
  vtir_courses %>%
    #filter(subject == "CS", number %in% c("1114", "2114", "1XXX", "2XXX")) %>%
    #mutate(number = str_replace(number, "1XXX", "1114"),
    #       number = str_replace(number, "2XXX", "2114")) ->
    filter(subject == "CS", number %in% c("1114", "2114")) ->
    cs1_attempts

  cs1_attempts %>%
    group_by(id, number) %>%
    summarize(attempts = sum(!is.na(grade_crn)), #attempts = sum(!is.na(registered_crn) | !is.na(grade_crn)),
              grades = paste(grade, collapse = ","),
              last_grade = fct_relevel(factor(dplyr::last(grade[grade %in% valid_grades],
                                                          order_by = term_course[grade %in% valid_grades]), levels = valid_grades), "A"),
              grouped_grade = group_grades(last_grade),
              AP = any(grade == "AP", na.rm = TRUE),
              transfer = any(grade == "T", na.rm = TRUE)) %>%
    ungroup() ->
    cs1_summary

  select_ap <- function(.x) {
    grepl('_AP$', .x)
  }

  # TODO: several students are transfering in CS 1XXX and CS 2XXX. These should probably count as CS1 and CS2 transfer credits , respectively
  cs1_summary %>%
    #mutate_if(is.logical, as.numeric) %>%
    mutate(number = if_else(number == "1114", "CS1", "CS2")) %>%
    gather(key, value, attempts, grades, last_grade, grouped_grade, AP, transfer) %>%
    unite(covariate, number, key) %>%
    spread(covariate, value, convert = TRUE) %>%
    #mutate_at(vars(ends_with('_seat')), ~factor(if_else(is.na(.), "No", "Yes"), c(TRUE, FALSE), labels = c("Yes", "No"))) %>%
    mutate_at(vars(ends_with('_transfer')), ~bool2factor(., na.value = FALSE)) %>%
    mutate_at(vars(ends_with('_AP')), ~bool2factor(., na.value = FALSE)) %>%
    #mutate_at(vars(ends_with('_seat')), ~bool2factor(.)) %>%
    mutate_at(vars(ends_with('_grouped_grade')), as.factor) ->
    #mutate_at(vars(ends_with('_attempts')), ~fct_other(as.character(.), keep = c("0", "1", "2"), other_level = "3+")) ->
    cs1and2_spread

  #save(cs1_attempts, file = "data/cs1_attempts.rda")
  save(cs1and2_spread, file = "data/cs1and2_spread.rda")
}
