source("functions.R")
source("data.R")

library(zeallot)
library(glue)

ndays_to_predict <- 30
k_folds <- 5

fold_holdout_dates <- purrr::map(
  k_folds:1,
  ~ as.Date("2018-11-15") - c(.x * (ndays_to_predict + 1), ((.x - 1) * ndays_to_predict) + .x)
)

c(treated_wikis, daily_pvs) %<-% data_generator(grouped = FALSE)
models <- c("MMPv1", "MMPv2", "MMP3v3") %>% set_names(., .)

models_wikis <- expand.grid(model = models, wiki = treated_wikis, stringsAsFactors = FALSE)

purrr::pwalk(models_wikis, function(model, wiki) {
  platforms <- c("mobile", "desktop") %>% set_names(., .)
  message("calculating MAPE for model '", model, "' of traffic to ", wiki)
  per_platform_apes <- purrr::map(platforms, function(platform) {
    message("calculating MAPE for ", platform, " traffic")
    c(y, x) %<-% daily_pvs(platform, wiki)
    return(purrr::map_dbl(fold_holdout_dates, get_mape, y = y, x = x, model = model))
  })
  mape <- data.frame(
    fold = 1:k_folds,
    mobile = per_platform_apes$mobile,
    desktop = per_platform_apes$desktop
  )
  fs::dir_create(file.path("models", model, wiki))
  readr::write_csv(mape, file.path("models", model, wiki, "mape.csv"))
})
