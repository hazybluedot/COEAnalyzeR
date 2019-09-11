grouped_majors <- function(fctdata) {
  fctdata %>%
    forcats::fct_collapse(GE = "GE", CS  = "CS", OtherEng = c("ME", "ISE", "CE", "EE", "AE", "CHE", "CPE", "MINE", "MSE", "ESM", "BSE", "OE", "CEM", "AOE")) %>%
    forcats::fct_other(keep = c("GE", "CS", "OtherEng"))
}
