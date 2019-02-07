# vignette("MarketMatching-Vignette", package = "MarketMatching")
# cf. https://multithreaded.stitchfix.com/blog/2016/04/21/forget-arima/
# cf. https://google.github.io/CausalImpact/CausalImpact.html

# helpers:
library(magrittr)
library(glue)
library(zeallot)

# viz:
library(ggplot2)

# modeling:
library(bsts) # install.packages("bsts")
library(MarketMatching) # install.packages("MarketMatching")
library(CausalImpact) # install.packages("CausalImpact")

compress <- function(x, ...) {
  y <- polloi::compress(abs(x), ...)
  z <- paste0(ifelse(x < 0, "-", ""), y)
  return(z)
}

stacked_pageviews <- function(granularity = "monthly") {
  if (granularity == "monthly") {
    monthly_pvs <- readr::read_csv("data/monthly_pageviews.csv")
    treated <- monthly_pvs %>%
      dplyr::filter(project == "idwiki" & referrer == "search")
    controls <- monthly_pvs %>%
      dplyr::filter(test_group == "control" | (project == "idwiki" & referrer != "search"))
    stacked <- rbind(treated, controls) %>%
      dplyr::mutate(
        date = as.Date(paste0(year, "-", month, "-01")),
        project_referrer = paste0(project, "_", referrer)
      ) %>%
      dplyr::select(date, project_referrer, pvs = avg)
  } else {
    daily_pageviews <- purrr::map_dfr(dir("data/fetched_2019-02-06", full.names = TRUE), readr::read_csv) %>%
      # collapse across site_version (desktop/mobile):
      dplyr::group_by(date, test_group, project, referrer) %>%
      dplyr::summarize(pageviews = sum(pageviews)) %>%
      dplyr::ungroup()
    treated <- daily_pageviews %>%
      dplyr::filter(project == "idwiki" & referrer == "search")
    controls <- daily_pageviews %>%
      dplyr::filter(test_group == "control" | (project == "idwiki" & referrer != "search"))
    stacked <- rbind(treated, controls) %>%
      dplyr::mutate(
        project_referrer = paste0(project, "_", referrer)
      ) %>%
      dplyr::select(date, project_referrer, pvs = pageviews)
  }
  return(stacked)
}

stacked <- stacked_pageviews("daily")
# for prototyping, introduce a fake intervention that decreases the average PVs by 50%:
fake_effect <- -0.5 * mean(stacked$pvs[stacked$project_referrer == "idwiki_search"])
stacked$fake_intervention <- stacked$pvs +
  (fake_effect * (stacked$date >= "2016-08-01" & stacked$project_referrer == "idwiki_search"))

matched_markets <- best_matches(
  data = stacked,
  markets_to_be_matched = "idwiki_search",
  id_variable = "project_referrer",
  date_variable = "date",
  matching_variable = "fake_intervention",
  start_match_period = min(stacked$date),
  end_match_period = "2016-07-31",
  dtw_emphasis = 0.5,
  matches = 5
)

head(matched_markets$BestMatches)

results <- inference(
  matched_markets = matched_markets,
  nseasons = 7,
  test_market = "idwiki_search",
  end_post_period = max(stacked$date),
)

results$PlotActualVersusExpected +
  scale_y_continuous(labels = compress)
results$PlotCumulativeEffect +
  scale_y_continuous(labels = compress)
results$PlotPointEffect +
  # geom_hline(yintercept = fake_effect, color = "red") +
  scale_y_continuous(labels = compress)
