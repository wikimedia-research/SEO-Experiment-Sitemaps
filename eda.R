library(ggplot2)
library(dplyr)
library(lubridate)

daily_pageviews <- purrr::map_dfr(dir("data/fetched", full.names = TRUE), readr::read_csv, col_types = "Dcccci") %>%
  mutate(period = if_else(date < "2018-11-15", "before", "after"))

wiki_groups <- daily_pageviews %>%
  distinct(test_group, project) %>%
  mutate(wiki = purrr::map_chr(project, ~ polloi::parse_wikiid(.x)$language)) %>%
  group_by(test_group) %>%
  summarize(languages = paste0(wiki, collapse = ", ")) %>%
  tidyr::spread(test_group, languages) %>%
  unlist

search_yoy <- daily_pageviews %>%
  filter(date != "2016-02-29", referrer == "search") %>% # filter out leap day
  group_by(date, site_version, test_group, referrer) %>%
  summarize(pageviews = sum(pageviews)) %>%
  ungroup %>%
  mutate(
    year = factor(year(date)),
    site_version = if_else(site_version == "desktop", "Desktop version", "Mobile (web) version"),
    test_group = if_else(test_group == "control", "Wikipedias without sitemaps", "Wikipedias with sitemaps")
  ) %>%
  arrange(site_version, test_group, referrer, date) %>%
  group_by(site_version, test_group, referrer) %>%
  mutate(smoothed = c(rep(NA, 3), RcppRoll::roll_mean(pageviews, 7), rep(NA, 3))) %>%
  ungroup
year(search_yoy$date) <- 2018
p <- ggplot(search_yoy, aes(x = date, y = smoothed)) +
  geom_line(aes(color = year, y = pageviews), alpha = 0.3) +
  geom_line(aes(color = year)) +
  geom_line(
    data = filter(search_yoy, (year == "2018" & date >= "2018-11-15") | (year == "2019")),
    alpha = 0.5, color = "yellow", size = 3
  ) +
  geom_line(
    aes(color = year),
    data = filter(search_yoy, (year == "2018" & date >= "2018-11-15") | (year == "2019"))
  ) +
  facet_grid(site_version ~ test_group) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month", minor_breaks = NULL) +
  scale_y_continuous(labels = polloi::compress) +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = NULL, y = "Pageviews", color = "Year",
    title = "Year-over-year search engine-referred traffic to Wikipedias in Sitemaps SEO Test",
    subtitle = "Post-deployment traffic highlighted",
    caption = paste0(c(
      paste("Wikipedias with sitemaps:", wiki_groups["treatment"]),
      paste("Wikipedias without sitemaps:", wiki_groups["control"])
    ), collapse = "; ")
  ) +
  hrbrthemes::theme_ipsum("Source Sans Pro") +
  theme(legend.position = "bottom")
p
ggsave("figures/yoy.png", p, width = 16, height = 8, units = "in", dpi = 300)

# check when those wikipedias launched:
inception <- WikidataQueryServiceR::query_wikidata("SELECT ?code ?inception
WHERE
{
  ?wikipedia wdt:P31 wd:Q10876391 .
  ?wikipedia wdt:P571 ?inception .
  ?wikipedia wdt:P424 ?code .
  FILTER(?code IN('bh', 'ca', 'chr', 'fr', 'id', 'kk', 'ko', 'nds-nl', 'nl', 'pa', 'pnb', 'pt', 'xal', 'yo'))
}", format = "smart")
