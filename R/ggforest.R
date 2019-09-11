#' @export
ggforest <- function(model, ...) UseMethod("ggforest")

#' @export
ggforest.coxph <- function(model, ...) {
  survminer::ggforest(model, ...)
}

#' @export
ggforest.data.frame <- function(df) {
  str(df)
  ggplot(df, aes(x = label, y = mean, ymin = lower, ymax = upper)) +
    geom_pointrange() +
    geom_errorbar(width = 0.15) +
    geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
    coord_flip() + # flip coordinates (puts labels on y axis)
    scale_y_log10() +
    theme_minimal() +
    labs(y = "Coefficient", x = NULL, title = "Odds ratio")
}

#' @export
ggforest.glm <- function(model, varnames = NULL) {
  c <- exp(coef(model))
  ci <- exp(confint(model))

  if (is.null(varnames)) {
    varnames <- names(c)
  }

  df <- bind_cols(data.frame(names = varnames, mean = c), as.data.frame(ci))[-1,]
  names(df) <- c("label", "mean", "lower", "upper")
  df %>%
    mutate(label = paste0(label, " (", round(mean, 2), ")")) ->
    df
  str(df)
  ggplot(df, aes(x = label, y = mean, ymin = lower, ymax = upper)) +
    geom_pointrange() +
    geom_errorbar(width = 0.15) +
    geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
    coord_flip() + # flip coordinates (puts labels on y axis)
    scale_y_log10() +
    theme_minimal() +
    labs(y = "Coefficient", x = NULL, title = "Odds ratio")
}

#' @export
ggforest.multinom <- function(model) {
  library(ggplot2)
  ci <- t(exp(confint(model))[2,1:2,1:3]) %>%
    as.data.frame()
  names(ci) <- c("lower", "upper")
  df <- as.data.frame(broom::tidy(model)) %>%
    mutate(label = paste0(y.level, " (", round(estimate, 2), ")"))
  bind_cols(df[c(2, 4, 6),], as.data.frame(ci)) ->
    df
  gsub(model$term, pattern = "`", replacement = "")
  rangeb <- range(df$conf.low, df$conf.high)
  breaks <- axisTicks(rangeb/2, log = TRUE, nint = 14)
  ggplot(df, aes(x = label, y = estimate, ymin = lower, ymax = upper)) +
    geom_pointrange() + # need ymin and ymax
    geom_errorbar(width = 0.15) +
    geom_hline(yintercept = 1, lty=2) +
    coord_flip() +
    scale_y_log10(breaks = breaks,
                  labels = sprintf("%g", breaks),
                  expand = c(0.02, 0.02)) +
    labs(y = "Coefficient", x = NULL)
}
