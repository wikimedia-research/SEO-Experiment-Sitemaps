library(magrittr)
library(bsts) # install.packages("bsts")

fit_bsts <- function(y, x, dyn_reg = FALSE, iters = 1000, ...) {
  # sdy <- sd(y) # sdx <- apply(x, 2, sd)
  # ss <- AddLocalLevel(list(), y, sigma.prior = SdPrior(sdy, 100, upper.limit = 2))
  # ss <- AddSemilocalLinearTrend(list(), y, level.sigma.prior = sdp) # better MAPE
  # ss <- AddLocalLinearTrend(list(), y, level.sigma.prior = sdp) # bad MAPE
  ss <- AddLocalLevel(list(), y)
  ss <- ss %>%
    AddSeasonal(y, nseasons = 7, season.duration = 1) %>% # day of week
    # AddSeasonal(y, nseasons = 52, season.duration = 7) %>% # week of year
    AddAutoAr(y, lags = 5) %>%
    AddRegressionHoliday(y, holiday.list = list(
      NamedHoliday("Christmas"), NamedHoliday("NewYearsDay")
    )) %>%
    AddMonthlyAnnualCycle(y) # yearly seasonality

  if (dyn_reg) {
    ss <- AddDynamicRegression(ss, y ~ x)
    model <- bsts(y, state.specification = ss, family = "gaussian", niter = iters,
                  expected.r2 = 0.8, ...)
  } else {
    model <- bsts(y ~ x, state.specification = ss, family = "gaussian", niter = iters,
                  expected.r2 = 0.8, ...)
  }
  return(model)
}

tidy_predictions <- function(fit, x, exponentiate = FALSE, ...) {
  predictions <- predict.bsts(fit, newdata = x, quantiles = c(0.025, 0.975), ...) %>%
    .[c("median", "interval")]
  if (exponentiate) {
    predictions <- purrr::map(predictions, exp)
  }
  predictions <- dplyr::tibble(
    date = index(x),
    expected = predictions$median,
    lower_bound = predictions$interval["2.5%", ],
    upper_bound = predictions$interval["97.5%", ]
  )
  return(predictions)
}

compress <- function(x) {
  return(paste0(ifelse(sign(x) < 0, "-", ""), polloi::compress(abs(x))))
}
