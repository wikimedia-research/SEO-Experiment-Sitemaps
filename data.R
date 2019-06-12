library(magrittr)

data_generator <- function(grouped = TRUE) {

  daily_pageviews <- purrr::map_dfr(dir("data/fetched", full.names = TRUE), readr::read_csv, col_types = "Dcccci") %>%
    dplyr::filter(date >= "2016-03-01") %>%
    dplyr::mutate(
      test_group = dplyr::case_when(
        project %in% paste0(c("id", "ko", "nl", "nds_nl", "pa", "pnb", "pt"), "wiki") ~ "treatment",
        project %in% paste0(c("bh", "chr", "kk", "ca", "fr", "yo", "xal"), "wiki") ~ "control"
      ),
      project = dplyr::case_when(
        project %in% c("nlwiki", "nds_nlwiki") ~ "nlwiki",
        project %in% c("pawiki", "pnbwiki") ~ "pawiki",
        TRUE ~ project
      ),
      period = dplyr::if_else(date < "2018-11-15", "before", "after"),
      # week = lubridate::isoweek(date),
      traffic = paste0(test_group, "_", referrer)
    )

  if (grouped) {
    daily_pageviews %<>%
      dplyr::group_by(site_version, traffic, date) %>%
      dplyr::summarize(
        pvs = sum(pageviews),
        log10_pvs = log10(pvs),
        log_pvs = log(pvs),
        pvsM = pvs / 1e6
      ) %>%
      # For each day, sum across individual projects:
      # summarize(total_pvs = sum(pageviews)) %>%
      # For each week, take the average and log it:
      # summarize(avg = mean(total_pvs), log_avg = log(avg)) %>%
      dplyr::ungroup() %>%
      {
        split(., .$site_version)
      }
    # Data generator function to be returned:
    data_subset <- function(platform, model) {
      data <- daily_pageviews[[platform]] %>%
        dplyr::filter(traffic %in% c("treatment_search", "control_search", "treatment_none", "control_none")) %>%
        dplyr::select(-c(site_version, log_pvs, log10_pvs, pvsM)) %>%
        tidyr::spread(traffic, pvs) %>%
        dplyr::select(date, `treatment_search`, dplyr::everything()) %>%
        {
          zoo(dplyr::select(., -date), .$date)
        }

      y <- data[, "treatment_search"]
      y <- y / 1e6 # "in millions"
      # y <- log(y)

      if (model %in% c("MM1v1", "MM1v2", "MM1v3")) {
        x <- data[, "control_search", drop = FALSE]
      } else if (model %in% c("MM3v1", "MM3v2")) {
        x <- data[, c("control_search", "treatment_none", "control_none")]
      }
      x <- x / 1e6 # "in millions"
      # x <- log(x)

      return(list(y, x))
    }
    return(data_subset)
  } else {
    # Data generator function to be returned:
    daily_pageviews %<>%
      dplyr::filter(referrer == "search") %>%
      dplyr::group_by(test_group, project, site_version, date) %>%
      dplyr::summarize(
        pvs = sum(pageviews),
        log10_pvs = log10(pvs),
        log_pvs = log(pvs),
        pvsM = pvs / 1e6
      ) %>%
      dplyr::ungroup()
    treated_wikis <- sort(unique(daily_pageviews$project[daily_pageviews$test_group == "treatment"]))
    data_subset <- function(platform, wiki) {
      data <- daily_pageviews[daily_pageviews$site_version == platform & (
        daily_pageviews$project == wiki | daily_pageviews$test_group == "control"
      ), c("project", "date", "pvsM")] %>%
        tidyr::spread(project, pvsM, fill = 0) %>%
        {
          zoo(dplyr::select(., -date), .$date)
        }
      y <- data[, wiki]
      x <- data[, setdiff(colnames(data), wiki)]
      return(list(y, x))
    }
    return(list(treated_wikis, data_subset))
  }
}
