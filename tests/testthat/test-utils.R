test_that("news_excerpt extracts the section between two versions (NEWS.md)", {
  news <- paste(
    "# pkg 1.2.0",
    "",
    "* New feature.",
    "",
    "# pkg 1.1.10",
    "",
    "* Bug fix for 1.1.1 regression.",
    "",
    "# pkg 1.1.1",
    "",
    "* Old stuff.",
    sep = "\n"
  )
  expect_equal(news_excerpt(news, "1.2.0", "1.1.10"), "# pkg 1.2.0\n\n* New feature.")
  # "1.1.1" must not match inside "1.1.10" or inside a bullet point
  expect_match(news_excerpt(news, "1.1.10", "1.1.1"), "^# pkg 1.1.10")
  expect_false(grepl("Old stuff", news_excerpt(news, "1.2.0", "1.1.1")))
  expect_true(is.na(news_excerpt(news, "2.0.0", "1.1.1")))
  expect_true(is.na(news_excerpt(news, "1.1.1", "1.2.0")))
})

test_that("news_excerpt works with plain-text NEWS files", {
  news <- paste(
    "CHANGES IN VERSION 0.3-1",
    "    o fixed 0.2-9 bug",
    "",
    "CHANGES IN VERSION 0.2-9",
    "    o initial",
    sep = "\n"
  )
  expect_equal(news_excerpt(news, "0.3-1", "0.2-9"), "CHANGES IN VERSION 0.3-1\n    o fixed 0.2-9 bug")
})

test_that("escape_regex escapes special characters", {
  expect_true(grepl(escape_regex("1.0-2"), "v1.0-2"))
  expect_false(grepl(escape_regex("1.0"), "1x0"))
})

test_that("split_urls and repo_url parse DESCRIPTION fields", {
  urls <- split_urls("https://github.com/tidyverse/dplyr, https://dplyr.tidyverse.org\n  https://x.org")
  expect_equal(urls, c("https://github.com/tidyverse/dplyr", "https://dplyr.tidyverse.org", "https://x.org"))
  expect_equal(split_urls(NA), character(0))
  expect_equal(split_urls(NULL), character(0))
  expect_equal(repo_url(urls), "https://github.com/tidyverse/dplyr")
  expect_true(is.na(repo_url("https://example.org")))
})

test_that("parse_maintainer splits name and email", {
  m <- parse_maintainer("Hadley Wickham <hadley@posit.co>")
  expect_equal(m$name, "Hadley Wickham")
  expect_equal(m$email, "hadley@posit.co")
  expect_true(is.na(parse_maintainer("Someone")$email))
  expect_true(is.na(parse_maintainer(NA)$name))
})

test_that("worst_check_status and status_color", {
  expect_equal(worst_check_status(c("OK", "NOTE", "OK")), "NOTE")
  expect_equal(worst_check_status(c("OK", "ERROR", "WARN")), "ERROR")
  expect_true(is.na(worst_check_status(character(0))))
  expect_equal(status_color("OK"), "success")
  expect_equal(status_color(NA), "secondary")
  expect_equal(status_color(NULL), "secondary")
})

test_that("format helpers produce human friendly output", {
  expect_equal(format_count(950), "950")
  expect_equal(format_count(12345), "12.3k")
  expect_equal(format_count(2500000), "2.5M")
  expect_equal(format_count(NA), "-")
  expect_equal(format_count(Inf), "-")
  expect_equal(format_count(numeric(0)), "-")
  expect_equal(format_days(10), "10 days")
  expect_equal(format_days(90), "3 months")
  expect_equal(format_days(730.5), "2.0 years")
  expect_equal(format_days(Inf), "-")
})

test_that("NEWS markdown escapes embedded HTML", {
  rendered <- shiny::markdown(htmltools::htmlEscape("<script>alert(1)</script>"))
  expect_false(grepl("<script>", as.character(rendered), fixed = TRUE))
  expect_true(grepl("&lt;script&gt;", as.character(rendered), fixed = TRUE))
})

test_that("cached evaluates once per key and does not cache errors", {
  calls <- 0
  f <- function() {
    calls <<- calls + 1
    calls
  }
  key <- list("test-key", runif(1))
  expect_equal(cached(key, f()), 1)
  expect_equal(cached(key, f()), 1)
  expect_equal(calls, 1)

  err_key <- list("err-key", runif(1))
  expect_error(cached(err_key, stop("boom")), "boom")
  expect_equal(cached(err_key, "ok"), "ok")
})

test_that("release history falls back after a failed CRAN request", {
  history <- data.frame(
    Package = "test.release.fallback",
    Version = c("1.0", "1.1"),
    Released = as.Date(c("2024-01-01", "2024-02-01")),
    Archived = as.Date(c("2024-02-01", NA)),
    LifeDuration = as.difftime(c(31, 60), units = "days")
  )
  sources <- character(0)
  local_mocked_bindings(
    pac_timemachine = function(pac, source) {
      sources <<- c(sources, source)
      if (source == "cran") stop("connection failed")
      history
    },
    .package = "pacs"
  )
  app_cache()$reset()
  expect_warning(result <- fetch_release_history(history$Package[1]), "Using crandb instead")
  expect_equal(result, history)
  expect_equal(sources, c("cran", "crandb"))
  expect_equal(fetch_release_history(history$Package[1]), history)
  expect_length(sources, 2)
})

test_that("invalid release histories are rejected and failures are not cached", {
  expect_false(valid_release_history(NA))
  expect_false(valid_release_history(data.frame(Version = "1.0")))
  calls <- 0L
  local_mocked_bindings(
    pac_timemachine = function(pac, source) {
      calls <<- calls + 1L
      NA
    },
    .package = "pacs"
  )
  app_cache()$reset()
  expect_error(fetch_release_history("test.release.invalid"), "missing or invalid release history")
  expect_error(fetch_release_history("test.release.invalid"), "missing or invalid release history")
  expect_equal(calls, 4L)
})

test_that("aggregate_downloads drops incomplete edge buckets", {
  dates <- seq(as.Date("2024-01-03"), as.Date("2024-01-24"), by = "day") # Wed .. Wed
  data <- data.frame(date = dates, count = 1, package = "a")
  weekly <- aggregate_downloads(data, "week")
  expect_equal(weekly$date, as.Date(c("2024-01-08", "2024-01-15")))
  expect_equal(weekly$count, c(7, 7))
  expect_equal(nrow(aggregate_downloads(data, "day")), length(dates))
  # keeps partial buckets when nothing would be left otherwise
  yearly <- aggregate_downloads(data, "year")
  expect_equal(nrow(yearly), 1)
})

test_that("app_ui builds a page", {
  ui <- app_ui()
  expect_s3_class(ui, "shiny.tag.list")
  html <- as.character(ui)
  expect_match(html, "R Dev Dashboard")
  expect_match(html, 'id="pac"')
})
