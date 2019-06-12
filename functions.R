library(magrittr)
library(bsts) # install.packages("bsts")

fit_bsts <- function(y, x, model, dyn_reg = FALSE, iters = 1000, ...) {

  # Base model:
  ss <- AddLocalLevel(list(), y)

  if (grepl("v[12]", model)) {
    # versions 1 & 2 have a seasonality component (version 3 has seasonality handled by control ts)
    ss <- ss %>%
      AddSeasonal(y, nseasons = 7, season.duration = 1) %>% # day of week
      AddSeasonal(y, nseasons = 52, season.duration = 7) %>% # week of year
      AddRegressionHoliday(y, holiday.list = list(
        NamedHoliday("Christmas", days.before = 2, days.after = 2),
        NamedHoliday("NewYearsDay", days.before = 2, days.after = 2)
      ))
    if (grepl("v1", model)) {
      # version 1 specifically has an AR component that is missing from version 2
      ss <- AddAutoAr(ss, y, lags = 5)
    }
  }

  # Decided against this in favor of AddSeasonal(52, 7):
  # ss <- AddMonthlyAnnualCycle(ss, y) # month of year

  if (dyn_reg) {
    ss <- AddDynamicRegression(ss, y ~ x)
    model <- bsts(y, state.specification = ss, family = "gaussian", niter = iters, expected.r2 = 0.8, ...)
  } else {
    model <- bsts(y ~ x, state.specification = ss, family = "gaussian", niter = iters, expected.r2 = 0.8, ...)
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

my_theme <- function() {
  hrbrthemes::theme_ipsum(
    "Source Sans Pro", 12,
    axis_title_size = 12,
    subtitle_size = 14,
    plot_title_size = 16,
    caption_size = 10
  )
}

get_mape <- function(holdout_dates, y, x, model) {

  dates <- index(y)
  message("fitting model to data from ", min(dates), " through ", holdout_dates[1] - 1)

  yh <- y[dates < holdout_dates[1]]
  xh <- x[dates < holdout_dates[1], ]

  evaluation_model <- fit_bsts(yh, xh, iters = 2000, model = model)

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

visualize_causal_impact <- function(ci_analysis, mape) {

  impact_estimation <- dplyr::mutate(
    ci_analysis$estimates,
    impact = actual - expected,
    impact_lower = actual - lower_bound,
    impact_upper = actual - upper_bound,
    relative = (actual - expected) / expected
  )

  avg <- impact_estimation %>%
    dplyr::filter(date >= "2018-11-15") %>%
    dplyr::summarize(
      impact_avg = compress(mean(impact) * 1e6),
      relative_avg = scales::percent(mean(relative), 0.01)
    )

  platform <- ci_analysis$platform
  Platform <- polloi::capitalize_first_letter(platform)

  if (is.null(ci_analysis$wiki)) {
    treated_wikis <- "Dutch, Indonesian, Korean, Portuguese, and Punjabi Wikipedias"
  } else {
    treated_wikis <- paste(c(
      "idwiki" = "Indonesian",
      "kowiki" = "Korean",
      "nlwiki" = "Dutch",
      "pawiki" = "Punjabi",
      "ptwiki" = "Portuguese"
    )[ci_analysis$wiki], "Wikipedia")
  }
  # control_wikis <- "Bhojpuri, Catalan, Cherokee, French, Kalmyk, Kazakh, and Yoruba Wikipedias"

  p1 <- ggplot(impact_estimation, aes(x = date)) +
    geom_vline(xintercept = as.Date("2018-11-15"), linetype = "dashed") +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound, fill = "predicted"), alpha = 0.7) +
    geom_line(aes(y = expected, color = "predicted"), size = 1) +
    geom_line(aes(y = actual, color = "actual"), size = 1) +
    scale_color_manual("", values = c("predicted" = "#ac6600", "actual" = "black"), labels = c("predicted" = "Counterfactual traffic forecast", "actual" = "Actual traffic")) +
    scale_fill_manual("", values = c("predicted" = "#ffcc33"), labels = c("predicted" = "95% Credible Interval of forecast")) +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d\n%Y") +
    scale_y_continuous(labels = . %>% paste0(., "M")) +
    coord_cartesian(xlim = as.Date(c("2018-08-01", "2018-12-15"))) +
    labs(
      x = "Date", y = "Pageviews",
      title = glue("{Platform} search-referred traffic to ${treated_wikis} treated with sitemaps"),
      subtitle = "Actual traffic vs predicted counterfactual traffic (\"what if sitemaps weren't deployed\") with 95% credible intervals",
      caption = sprintf("Median MAPE from CV of pre-sitemaps %s traffic: %.2f%%", platform, 100 * mape)
    ) +
    my_theme() +
    theme(legend.position = "bottom")
  p2 <- ggplot(dplyr::filter(impact_estimation, date >= "2018-11-15"), aes(x = date)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = impact_lower, ymax = impact_upper), fill = "#3366cc", alpha = 0.2) +
    geom_line(aes(y = impact), color = "#3366cc", size = 1.1) +
    scale_y_continuous(labels = . %>% paste0(., "M"), minor_breaks = NULL) +
    labs(
      x = "Date", y = "Pageviews", title = paste("Estimated absolute impact; average:", avg["impact_avg"])
    ) +
    my_theme()
  p3 <- ggplot(dplyr::filter(impact_estimation, date >= "2018-11-15"), aes(x = date)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line(aes(y = relative), color = "#00af89", size = 1.1) +
    scale_y_continuous(label = scales::percent_format(1), minor_breaks = NULL) +
    coord_cartesian(ylim = c(-0.5, 0.5)) +
    labs(x = "Date", y = "Relative difference",
         title = paste("Estimated relative impact; average:", avg["relative_avg"])) +
    my_theme()

  p <- p1 + (
    p2 + p3 + plot_layout(ncol = 2)
  ) + plot_layout(ncol = 1)

  return(p)
}

visualize_contributions <- function(ci_analysis) {
  platform <- ci_analysis$platform
  Platform <- polloi::capitalize_first_letter(platform)

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

  posterior_CI <- function(x) {
    CI <- t(apply(x, 2, quantile, probs = c(0.1, 0.5, 0.9))) %>%
      as.data.frame() %>%
      set_names(c("lower", "estimate", "upper"))
    CI$term <- rownames(CI)
    rownames(CI) <- NULL
    return(CI)
  }
  coefs <- list(
    regression = ci_analysis$model$coefficients
  )
  if (grepl("MM1", ci_analysis$model_name)) {
    coef_fct_lvls <- c("x")
    coef_fct_lbls <- c("control (search)")
  } else if (grepl("MM3", ci_analysis$model_name)) {
    coef_fct_lvls <- c("xcontrol_search", "xtreatment_none", "xcontrol_none")
    coef_fct_lbls <- c("control (search)", "treatment (direct)", "control (direct)")
  } else if (grepl("MMP", ci_analysis$model_name)) {
    coef_fct_lvls <- setdiff(colnames(ci_analysis$model$coefficients), "(Intercept)")
    coef_fct_lbls <- substr(coef_fct_lvls, 2, 20) # remove the x at the beginning of the coefficient names
  }
  if (grepl("v[12]", ci_analysis$model_name)) {
    # versions 1 & 2 have a seasonality component (version 3 has seasonality handled by control ts)
    holiday_coefs <- function(holiday_name) {
      return(paste0(holiday_name, c(" -2", " -1", "", " +1", " +2")))
    }
    coefs$christmas <- set_colnames(ci_analysis$model$Christmas, holiday_coefs("Christmas Day"))
    coefs$newyear <- set_colnames(ci_analysis$model$NewYearsDay, holiday_coefs("New Year Day"))
    coef_fct_lvls <- c(coef_fct_lvls, holiday_coefs("Christmas Day"), holiday_coefs("New Year Day"))
    coef_fct_lbls <- c(coef_fct_lbls, holiday_coefs("Christmas Day"), holiday_coefs("New Year Day"))
    if (grepl("v1", ci_analysis$model_name)) {
      # version 1 specifically has an AR component that is missing from version 2
      coefs$autoreg <- ci_analysis$model$AR5.coefficients
      coef_fct_lvls <- c(coef_fct_lvls, paste0("lag.", 1:5))
      coef_fct_lbls <- c(coef_fct_lbls, paste("AR", 1:5))
    }
  }
  posterior_CIs <- purrr::map_dfr(coefs, posterior_CI) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::mutate(term = factor(term, coef_fct_lvls, coef_fct_lbls))

  p1 <- ggplot(state_contribs, aes(x = date)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(aes(y = middle)) +
    facet_wrap(~ state, scales = "free_y", ncol = 1) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y", minor_breaks = NULL) +
    scale_y_continuous(minor_breaks = NULL) +
    coord_cartesian(xlim = as.Date(c("2016-03-01", "2018-11-14"))) +
    labs(
      x = NULL, y = "Pageviews (in millions)", subtitle = "With 50% Credible Intervals",
      title = glue("Per-state contributions to {platform} search traffic")
    ) +
    my_theme()
  p2 <- ggplot(posterior_CIs, aes(x = term)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_linerange(aes(ymin = lower, ymax = upper), color = "#b32424", size = 1) +
    geom_point(aes(y = estimate), size = 3) +
    scale_y_continuous(breaks = seq(-3, 3, 0.2)) +
    scale_x_discrete(limits = rev(levels(posterior_CIs$term))) +
    coord_flip() +
    labs(
      x = NULL, y = NULL, subtitle = "80% Credible Intervals",
      title = glue("{Platform} search traffic model coefficient posteriors")
    ) +
    my_theme()

  return(list(state_contributions = p1, coef_posteriors = p2))

}
