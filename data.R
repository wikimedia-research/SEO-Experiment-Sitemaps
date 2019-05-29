library(magrittr)

data_generator <- function() {

  daily_pageviews <- purrr::map_dfr(dir("data/fetched", full.names = TRUE), readr::read_csv, col_types = "Dcccci") %>%
    dplyr::mutate(
      period = dplyr::if_else(date < "2018-11-15", "before", "after"),
      # week = lubridate::isoweek(date),
      traffic = paste0(test_group, "_", referrer)
    ) %>%
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

  data_subset <- function(platform) {

    data <- daily_pageviews[[platform]] %>%
      # dplyr::filter(date != "2016-02-29") %>% # exclude leap day
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

    x <- data[, c("control_search", "treatment_none", "control_none")]
    x <- x / 1e6 # "in millions"
    # x <- log(x)

    return(list(y, x))

  }

  return(data_subset)

}

daily_pvs <- data_generator()
