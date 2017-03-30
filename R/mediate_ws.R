#' Calculate indirect effects of X on Y through a mediator in two-condition within-subjects design
#'
#' @param df a tidy dataframe, each row one observation and with columns for that have the value for the mediating variable at time 1, the mediating variable at time 2, the value of the dependent variable (Y) at time 1, and the value of the dependent variable at time 2, respectively
#' @param M1 the name of the column with the values of the mediating variable at time 1
#' @param M2 the name of the column with the values of the mediating variable at time 2
#' @param Y1 the name of the column with the values of the dependent variable at time 1
#' @param Y2 the name of the column with teh values of the dependent variable at time2
#' @param Reps the number of bootstrap samples
#' @param CONF the confidence interval width. You can have multiple values here.
#' @param BSType the bootstrap CI that is calculated
#' @return A data frame, where each row is a different bootstrap calculation (if you did more than one). There is a column indicating the bootstrap method used, the confidence interval level, the lower bound of the confidence interval, the higher bound of the confidence interval, and the estimate
#'
#' @name mediate_ws
#' @importFrom magrittr %>%
#' @examples
#' mediate_ws(sample_data, M1, M2, Y1, Y2, Reps = 3000, CONF = c(.9, .95, .99), BSType = "bca")

tidy.bootci <- function(x, estimate = FALSE) {
  types <- c("normal", "basic", "student", "percent", "bca")
  types <- dplyr::intersect(types, names(x))

  ret <- plyr::ldply(x[types], function(m) {
    # skip the "index of the order statistic" columns
    if (ncol(m) == 5) {
      m <- m[, c(1, 4, 5), drop = FALSE]
    }
    ret <- as.data.frame(m)
    colnames(ret) <- c("level", "conf.low", "conf.high")
    ret
  }, .id = "method")
  if (estimate) {
    ret$estimate <- x$t0
  }

  ret
}


glance.bootci <- function(x) {
  data.frame(estimate = x$t0, replications = x$R)
}

bs <-  function(formula1, formula2, data, indices) {
  d <- data[indices, ]
  a <- broom::tidy(stats::lm(formula1, data = d), quick = TRUE)$estimate
  b <- broom::tidy(stats::lm(formula2, data = d), quick = TRUE)$estimate[2]
  indirect <- a*b

  indirect
}

Gen_Vars <- function(df, M1, M2, Y1, Y2) {
  df1 <- df %>%
    dplyr::mutate(mdiff = M2 - M1) %>%
    dplyr::mutate(ydiff = Y2 - Y1) %>%
    dplyr::mutate(const = 1) %>%
    dplyr::mutate(Msum_cent = .5*(M1 + M2) - .5*(mean(M2) + mean(M1)))

  df1
}

#' @rdname mediate_ws
#' @export
mediate_ws <- function(df, M1, M2, Y1, Y2, Reps = 5000, CONF = .95, BSType = "basic"){
  New_Data <- Gen_Vars(df, M1, M2, Y1, Y2)
  results <- boot::boot(data = New_Data, statistic = bs, R = Reps, formula1 = mdiff ~ const, formula2 = ydiff ~ mdiff + Msum_cent)
  tidy_ci <- broom::tidy(boot::boot.ci(results, conf = CONF, type = BSType), estimate = TRUE)

  tidy_ci
}
