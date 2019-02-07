# system("scp -r stat7:/home/bearloga/sitemaps_experiment/data/fetched* data/")
library(dplyr)
library(tidyr)
library(lubridate)

daily_pageviews <- purrr::map_dfr(dir("data/fetched", full.names = TRUE), readr::read_csv)

monthly_pvs <- daily_pageviews %>%
  mutate(
    year = as.character(year(date)), month = as.character(month(date)),
    period = if_else(year >= 2018 & month >= 11, "after", "before")
  ) %>%
  # collapse across day of month & site_version (desktop/mobile):
  group_by(year, month, period, test_group, project, referrer) %>%
  summarize(avg = mean(pageviews)) %>% # normalizes pageviews by month length, in millions
  ungroup %>%
  mutate_if(is.character, factor) %>%
  complete(year, month, period, referrer, nesting(test_group, project), fill = list(avg = 0)) %>%
  mutate(
    avg_millions = avg / 1e6,
    avg_log10 = log10(avg + 1),
    intervention = case_when(
      period == "before" ~ 0,
      period == "after" & year == 2018 & month == 11 ~ 0.5,
      period == "after" ~ 1
    )
  )

validate <- function() {
  wiki_counts <- monthly_pvs %>%
    count(year, month, period, test_group, referrer) %>%
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
