source("data.R")

library(dtw) # install.packages("dtw")
library(bsts) # install.packages("bsts")
library(CausalImpact) # install.packages("CausalImpact")
library(MarketMatching) # install.packages("MarketMatching")

# options(mc.cores = parallel::detectCores())
mm <- daily_pageviews$mobile %>%
  dplyr::filter(date <= "2018-11-14") %>% # for checking
  best_matches(
    data = .,
    markets_to_be_matched = "treatment_search",
    id_variable = "traffic",
    date_variable = "date",
    matching_variable = "pvsM",
    parallel = FALSE,
    warping_limit = 1,
    dtw_emphasis = 0.9, # rely mostly on dtw for pre-screening
    matches = 2, # request 3 matches
    start_match_period = "2016-02-05",
    end_match_period = "2018-10-01" #  for checking
    # end_match_period = "2018-11-14"
  )

head(mm$BestMatches)

results <- inference(
  matched_markets = mm,
  analyze_betas = TRUE,
  test_market = "treatment_search",
  end_post_period = "2018-11-14",
  # end_post_period = "2019-05-23", # note: 6 weeks in is 2019-12-27
  prior_level_sd = 0.2,
  nseasons = 7
)

library(ggplot2)

predictions <- results$Predictions
ggplot(predictions, aes(x = Date)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "cornflowerblue", alpha = 0.2) +
  geom_line(aes(y = Predicted), color = "cornflowerblue") +
  geom_line(aes(y = Response), color = "red") +
  coord_cartesian(xlim = as.Date(c("2018-09-01", "2019-05-20"))) +
  hrbrthemes::theme_ipsum()

results$PlotActualVersusExpected +
  scale_color_brewer(palette = "Set1") +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b\n%Y") +
  scale_y_continuous(labels = . %>% paste0(., "M")) +
  coord_cartesian(xlim = as.Date(c("2018-08-01", "2018-11-14"))) +
  # coord_cartesian(xlim = as.Date(c("2018-08-01", "2019-02-28"))) +
  hrbrthemes::theme_ipsum() +
  labs(
    x = "Date", y = "Pageviews", color = "Traffic",
    title = "Sitemaps on mobile Indonesian, Korean, Dutch, Punjabi, Portuguese Wikipedias",
    subtitle = "Market-matching causal impact analysis of search engine-referred traffic",
    caption = "Search traffic to treated Wikipedias compared to search traffic among to Wikipedias and direct traffic to both groups"
  ) +
  theme(legend.position = "bottom")

results$PlotPriorLevelSdAnalysis
