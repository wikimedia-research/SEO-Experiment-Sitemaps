source("functions.R")
source("data.R") # yields daily_pvs(platform) generator

library(zeallot)
library(ggplot2)
library(patchwork)
library(glue)

analyze_causal_impact <- function(platform) {

  c(y, x) %<-% daily_pvs(platform)
  yy <- y[index(y) < "2018-11-15"]
  # std_y <- CausalImpact:::Standardize(y)
  # yy <- std_y$y

  xx <- x[index(x) < "2018-11-15", ]
  # std_x <- CausalImpact:::StandardizeAllVariables(x)
  # xx <- std_x$data
  impact_model <- fit_bsts(yy, xx, iters = 4000)

  # plot.bsts(impact_model)
  # plot.bsts(impact_model, "components")

  impact_analysis_dates <- index(y) >= "2018-11-15" & index(y) < "2019-01-15" # 60 days for impact estimation
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
    model = impact_model,
    estimates = impact_estimation
  ))
}

my_theme <- function() {
  hrbrthemes::theme_ipsum(
    "Source Sans Pro", 12,
    axis_title_size = 12,
    subtitle_size = 14,
    plot_title_size = 16,
    caption_size = 10
  )
}

visualize_causal_impact <- function(ci_analysis, mape) {

  impact_estimation <- dplyr::mutate(
    ci_analysis$estimates,
    impact = actual - expected,
    impact_lower = actual - lower_bound,
    impact_upper = actual - upper_bound,
    relative = (actual - expected) / expected,
    relative_lower = (actual - lower_bound) / lower_bound,
    relative_upper = (actual - upper_bound) / upper_bound
  )

  avg <- impact_estimation %>%
    dplyr::filter(date >= "2018-11-15") %>%
    dplyr::summarize(
      impact_avg = compress(mean(impact) * 1e6),
      relative_avg = scales::percent(mean(relative), 0.01)
    )

  platform <- ci_analysis$platform
  Platform <- polloi::capitalize_first_letter(platform)
  treatment_wikis <- "Dutch, Indonesian, Korean, Portuguese, and Punjabi Wikipedias"
  control_wikis <- "Bhojpuri, Catalan, Cherokee, French, Kalmyk, Kazakh, and Yoruba Wikipedias"

  p1 <- ggplot(impact_estimation, aes(x = date)) +
    geom_vline(xintercept = as.Date("2018-11-15"), linetype = "dashed") +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#dd3333", alpha = 0.2) +
    geom_line(aes(y = expected), color = "#dd3333", size = 1) +
    geom_line(aes(y = actual), size = 1) +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d\n%Y") +
    scale_y_continuous(labels = . %>% paste0(., "M")) +
    coord_cartesian(xlim = as.Date(c("2018-08-01", "2019-01-15"))) +
    labs(
      x = "Date", y = "Pageviews",
      title = glue("{Platform} search-referred traffic to {treatment_wikis} treated with sitemaps"),
      subtitle = "Actual traffic vs predicted counterfactual traffic (\"what if sitemaps weren't deployed\") with 95% credible intervals",
      caption = sprintf("Median MAPE from 10-fold nested CV of pre-sitemaps %s traffic: %.2f%%", platform, 100 * mape)
    ) +
    my_theme()
  p2 <- ggplot(dplyr::filter(impact_estimation, date >= "2018-11-15"), aes(x = date)) +
    geom_hline(yintercept = 0) +
    geom_ribbon(aes(ymin = impact_lower, ymax = impact_upper), fill = "#3366cc", alpha = 0.2) +
    geom_line(aes(y = impact), color = "#3366cc", size = 1.1) +
    scale_y_continuous(labels = . %>% paste0(., "M"), minor_breaks = NULL) +
    labs(
      x = "Date", y = "Pageviews", title = paste("Estimated absolute impact; average:", avg["impact_avg"]),
      subtitle = glue("Counterfactual model of {platform} search-referred traffic based on {control_wikis} as control set")
    ) +
    my_theme()
  p3 <- ggplot(dplyr::filter(impact_estimation, date >= "2018-11-15"), aes(x = date)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = relative_lower, ymax = relative_upper), fill = "#00af89", alpha = 0.2) +
    geom_line(aes(y = relative), color = "#00af89", size = 1.1) +
    scale_y_continuous(label = scales::percent_format(1), minor_breaks = NULL) +
    coord_cartesian(ylim = c(-0.5, 0.5)) +
    labs(x = "Date", y = "Relative difference",
         title = paste("Estimated relative impact; average:", avg["relative_avg"])) +
    my_theme()

  p <- p1 + (
    p2 + p3 + plot_layout(ncol = 2)
  ) + plot_layout(ncol = 1)

  fs::dir_create("figures")
  ggsave(glue("figures/causal_impact-{platform}.png"), p, width = 16, height = 12, dpi = 300)

}

visualize_contributions <- function(ci_analysis) {
  state_contribs <- ci_analysis$model$state.contributions %>%
    apply(c(2, 3), quantile, probs = c(0.25, 0.5, 0.75))
  states <- 1:(dim(state_contribs)[2]) %>%
    set_names(snakecase::to_snake_case(dimnames(state_contribs)[[2]]))
  state_contribs <- purrr::map_dfr(states, function(state) {
    output <- as.data.frame(t(state_contribs[, state, ]))
    names(output) <- c("lower", "middle", "upper")
    output$date <- ci_analysis$estimates$date[1:nrow(output)]
    return(output)
  }, .id = "state")

  p <- ggplot(state_contribs, aes(x = date)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(aes(y = middle)) +
    facet_wrap(~ state, scales = "free_y", ncol = 1) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y", minor_breaks = NULL) +
    scale_y_continuous(minor_breaks = NULL) +
    coord_cartesian(xlim = as.Date(c("2016-02-01", "2018-12-01"))) +
    labs(
      x = NULL, y = "Pageviews (in millions)",
      title = "Per-state contributions",
      subtitle = "With 50% Credible Intervals"
    ) +
    my_theme()

  fs::dir_create("figures")
  ggsave(glue("figures/state_contributions-{ci_analysis$platform}.png"), p, width = 24, height = 12, dpi = 300)

}

causal_impact_analysis <- purrr::map(c("mobile", "desktop"), analyze_causal_impact)
readr::write_rds(causal_impact_analysis, "causal_impact_analysis.rds", compress = "gz")

causal_impact_analysis <- readr::read_rds("causal_impact_analysis.rds")
# ci_analysis <- causal_impact_analysis[[1]]
purrr::walk2(causal_impact_analysis, c(mobile = 0.04375, desktop = 0.07673), visualize_causal_impact)
purrr::walk(causal_impact_analysis, visualize_contributions)
