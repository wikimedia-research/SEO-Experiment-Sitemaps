source("functions.R")
source("data.R") # yields daily_pvs(platform) generator

library(zeallot)
library(ggplot2)
library(patchwork)
library(glue)

c(treated_wikis, daily_pvs) %<-% data_generator(grouped = FALSE)
models <- c("MMPv1", "MMPv2", "MMP3v3") %>% set_names(., .)

analyze_causal_impact <- function(platform, model, wiki) {

  c(y, x) %<-% daily_pvs(platform, wiki)
  yy <- y[index(y) < "2018-11-15"]
  xx <- x[index(x) < "2018-11-15", ]
  impact_model <- fit_bsts(yy, xx, iters = 4000, model = model)

  impact_analysis_dates <- index(y) >= "2018-11-15" & index(y) <= "2018-12-15" # 30 days for impact estimation
  impact_predictions <- tidy_predictions(impact_model, x[impact_analysis_dates, ], burn = 2000)

  fitted_values <- apply(impact_model$state.contributions, c(1, 3), sum) %>%
    apply(2, quantile, probs = c(0.025, 0.5, 0.975)) %>%
    t %>%
    as.data.frame %>%
    set_colnames(c("lower_bound", "expected", "upper_bound")) %>%
    cbind(date = index(yy), actual = as.numeric(yy), period = "pre-intervention")

  impact_estimation <- as.data.frame(y[impact_analysis_dates]) %>%
    {
      .$date <- as.Date(rownames(.))
      rownames(.) <- NULL
      .
    } %>%
    setNames(c("actual", "date")) %>%
    dplyr::left_join(impact_predictions, by = "date") %>%
    dplyr::mutate(period = "post-intervention") %>%
    dplyr::bind_rows(fitted_values, .)

  return(list(
    platform = platform,
    model_name = model,
    model = impact_model,
    estimates = impact_estimation,
    wiki = wiki
  ))
}

models_wikis <- expand.grid(model = models, wiki = treated_wikis, stringsAsFactors = FALSE)

purrr::pwalk(models_wikis, function(model, wiki) {
  message("Performing causal impact analysis of ", wiki, " traffic under the '", model, "' model")
  causal_impact_analysis <- purrr::map(c("mobile", "desktop"), analyze_causal_impact, model = model, wiki = wiki)
  fs::dir_create(file.path("models", model, wiki))
  readr::write_rds(causal_impact_analysis, file.path("models", model, wiki, "causal_impact_analysis.rds"), compress = "gz")
})

purrr::pwalk(models_wikis, function(model, wiki) {
  message("Visualizing causal impact analysis results from the '", model, "' model of ", wiki, "traffic")
  message(" 1. Reading data in")
  causal_impact_analysis <- readr::read_rds(glue("models/{model}/{wiki}/causal_impact_analysis.rds"))
  mape <- readr::read_csv(file.path("models", model, wiki, "mape.csv"), col_types = "idd")
  # mape <- list(mobile = 0.0, desktop = 0.0)
  message(" 2. Generating visualizations of predictions and estimates")
  purrr::walk2(causal_impact_analysis, c(median(mape$mobile), median(mape$desktop)), function(ci_analysis, mape) {
    p <- visualize_causal_impact(ci_analysis, mape)
    fs::dir_create(glue("models/{ci_analysis$model_name}/{wiki}/figures"))
    ggsave(glue("models/{ci_analysis$model_name}/{wiki}/figures/causal_impact-{ci_analysis$platform}.png"), p, width = 16, height = 12, dpi = 300)
  })
  message(" 3. Generating visualizations of state contributions and coefficient posteriors")
  purrr::walk(causal_impact_analysis, function(ci_analysis) {
    c(p1, p2) %<-% visualize_contributions(ci_analysis)
    fs::dir_create(glue("models/{ci_analysis$model_name}/{wiki}/figures"))
    ggsave(glue("models/{ci_analysis$model_name}/{wiki}/figures/state_contributions-{ci_analysis$platform}.png"), p1, width = 24, height = 12, dpi = 300)
    ggsave(glue("models/{ci_analysis$model_name}/{wiki}/figures/coef_posteriors-{ci_analysis$platform}.png"), p2, width = 10, height = 6, dpi = 300)
  })
})
