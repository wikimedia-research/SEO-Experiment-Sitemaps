source("functions.R")
source("data.R")
library(furrr)
plan(multiprocess)

ndays_to_predict <- 30
k_folds <- 10

fold_holdout_dates <- purrr::map(
  k_folds:1,
  ~ as.Date("2018-11-15") - c(.x * (ndays_to_predict + 1), ((.x - 1) * ndays_to_predict) + .x)
)

if (!all(purrr::map_dbl(fold_holdout_dates, ~ .x[2] - .x[1]) == ndays_to_predict))
  stop("not ndays_to_predict")

daily_pvs <- data_generator()
models <- c("MM1v1", "MM1v2", "MM1v3", "MM3v1", "MM3v2") %>% set_names(., .)

purrr::walk(models[1:3], function(model) {
  platforms <- c("mobile", "desktop") %>% set_names(., .)
  message("calculating MAPE for model '", model, "'")
  per_platform_apes <- furrr::future_map(platforms, function(platform) {
    message("calculating MAPE for ", platform, " traffic")
    yx <- daily_pvs(platform, model)
    yp <- yx[[1]]
    xp <- yx[[2]]
    return(purrr::map_dbl(fold_holdout_dates, get_mape, y = yp, x = xp, model = model))
  })

  mape <- data.frame(
    fold = 1:k_folds,
    mobile = per_platform_apes$mobile,
    desktop = per_platform_apes$desktop
  )
  fs::dir_create(file.path("models", model))
  readr::write_csv(mape, file.path("models", model, "mape.csv"))
})
