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
# 0.03043 0.03326 0.03898 0.05806 0.05090 0.13578
#
# $desktop
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.04858 0.05568 0.06067 0.09739 0.12071 0.28738

MAPEs <- list(
  mobile = c(0.0505, 0.0395, 0.1358, 0.0304, 0.0318, 0.0376, 0.0315, 0.0385, 0.051, 0.1341),
  desktop = c(0.0486, 0.1313, 0.0601, 0.0562, 0.2874, 0.0613, 0.0889, 0.1343, 0.0555, 0.0503)
)
