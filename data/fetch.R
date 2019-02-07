library(readr)
library(dplyr)
library(glue)
library(zeallot)
library(lubridate)

query <- paste0(read_lines("data/query.hql"), collapse = "\n")

today <- lubridate::today()
base_dir <- "data/fetched"
fs::dir_create(base_dir)
output_filename <- fs::path(base_dir, "{date}.csv")

fetch <- function(date) {
  message("fetching data from ", date)
  c(year, month, day) %<-% wmf::extract_ymd(date)
  query <- glue(query, .open = "${", .close = "}")
  result <- wmf::query_hive(query) %>%
    mutate(
      date = as.Date(date),
      project = sub("-", "_", sub(".wikipedia", "wiki", project, fixed = TRUE), fixed = TRUE)
    )
  output_filename <- glue(output_filename)
  write_csv(result, output_filename)
  return(invisible(NULL))
}

c(start_date, end_date) %<-% as.Date(c("2016-02-05", "2019-02-06"))
dates <- seq(start_date, end_date, by = "day")

purrr::walk(dates, fetch)
