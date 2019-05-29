# system("scp -r stat7:/home/bearloga/sitemaps_experiment/data/fetched* data/")
library(dplyr)
library(tidyr)
library(lubridate)

daily_pageviews <- purrr::map_dfr(
  dir("data/fetched", full.names = TRUE),
  readr::read_csv, col_types = "Dcccci"
) %>% mutate(
  year = year(date),
  month = month(date),
  week = isoweek(date),
  period = case_when(
    year < 2018 ~ "before",
    year == 2018 & month < 11 ~ "before",
    year == 2018 & month >= 11 ~ "after",
    year > 2018 ~ "after"
  )
)

daily_pageviews %>%
  distinct(year, month, week) %>%
  View("weeks")
# Basic EDA to make sure we're not missing days:
daily_pageviews %>%
  group_by(year, month) %>%
  summarize(days = length(unique(date))) %>%
  arrange(year, month) %>%
  View("day counts")

monthly_pvs <- daily_pageviews %>%
  mutate_at(vars(year:week), as.character) %>%
  # collapse across day of month & site_version (desktop/mobile):
  group_by(year, month, week, period, test_group, project, referrer) %>%
  summarize(avg = mean(pageviews)) %>% # normalizes pageviews by week length, in millions
  ungroup %>%
  mutate_if(is.character, factor) %>%
  complete(referrer, nesting(test_group, project), fill = list(avg = 0)) %>%
  mutate(
    avg_millions = avg / 1e6,
    avg_log10 = log10(avg + 1),
    intervention = case_when(
      period == "before" ~ 0,
      period == "after" & year == 2018 & month == 11 ~ 0.5,
      period == "after" ~ 1
    )
  ) %>%
  select(year, month, week, period, test_group, project, referrer, everything()) %>%
  arrange(year, month, week, period, test_group, project, referrer)

validate <- function() {
  wiki_counts <- monthly_pvs %>%
    count(year, month, week, period, test_group, referrer) %>%
    spread(test_group, n)
  valid_control <- assertthat::assert_that(all(wiki_counts$control == 9))
  valid_treatment <- assertthat::assert_that(all(wiki_counts$treatment == 5))
  return(valid_control && valid_treatment)
}

if (validate()) {
  readr::write_csv(monthly_pvs, "data/monthly_pageviews.csv")
} else {
  stop("monthly_pvs did not pass validation")
}
