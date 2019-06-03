source("functions.R")
source("data.R")
library(furrr)
plan(multiprocess)

ndays_to_predict <- 60
k_folds <- 10

fold_holdout_dates <- purrr::map(
  k_folds:1,
  ~ as.Date("2018-11-15") - c(.x * (ndays_to_predict + 1), ((.x - 1) * ndays_to_predict) + .x)
)

if (!all(purrr::map_dbl(fold_holdout_dates, ~ .x[2] - .x[1]) == ndays_to_predict))
  stop("not ndays_to_predict")

get_mape <- function(holdout_dates, y, x) {

  dates <- index(y)
  message("fitting model to data from ", min(dates), " through ", holdout_dates[1] - 1)

  yh <- y[dates < holdout_dates[1]]
  xh <- x[dates < holdout_dates[1], ]

  evaluation_model <- fit_bsts(yh, xh, iters = 2000)

  message("using model to predict for ", holdout_dates[1], " through ", holdout_dates[2])
  xhn <- x[(dates >= holdout_dates[1]) & (dates < holdout_dates[2]), ]
  holdout_predictions <- tidy_predictions(evaluation_model, xhn, burn = 1000)

  mape <- (y[(dates >= holdout_dates[1]) & (dates < holdout_dates[2])]) %>%
    as.data.frame %>%
    {
      .$date <- as.Date(rownames(.))
      rownames(.) <- NULL
      .
    } %>%
    setNames(c("actual", "date")) %>%
    dplyr::left_join(holdout_predictions, by = "date") %>%
    dplyr::transmute(ape = abs(actual - expected) / actual) %>%
    dplyr::pull(ape) %>%
    mean

  return(mape)

}

platforms <- c("mobile", "desktop") %>% set_names(., .)
MAPEs <- furrr::future_map(platforms, function(platform) {
  message("calculating MAPE for ", platform, " traffic")
  yx <- daily_pvs(platform)
  yp <- yx[[1]]
  xp <- yx[[2]]
  return(purrr::map_dbl(fold_holdout_dates, get_mape, y = yp, x = xp))
})

# lapply(MAPEs, summary)
# $mobile
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.02086 0.03886 0.04375 0.04851 0.04703 0.11400
#
# $desktop
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.03812 0.05658 0.07673 0.07563 0.09045 0.11685

# MAPEs <- purrr::map(MAPEs, round, digits = 3)
# dump("MAPEs", "")
MAPEs <- list(
  mobile = c(0.044, 0.039, 0.047, 0.038, 0.021, 0.043, 0.047, 0.04, 0.053, 0.114),
  desktop = c(0.098, 0.117, 0.09, 0.038, 0.084, 0.091, 0.053, 0.07, 0.069, 0.048)
)
