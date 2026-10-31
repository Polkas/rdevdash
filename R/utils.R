.rdevdash_env <- new.env(parent = emptyenv())

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

#' Shared application cache
#'
#' A memory cache shared by all sessions of the app, so repeated look-ups of
#' the same package (e.g. by different users) do not hit CRAN again.
#' @noRd
app_cache <- function() {
  if (is.null(.rdevdash_env$cache)) {
    .rdevdash_env$cache <- cachem::cache_mem(
      max_size = 512 * 1024^2,
      max_age = 6 * 60 * 60
    )
  }
  .rdevdash_env$cache
}

#' Evaluate `expr` once per `key` and reuse the result (errors are not cached).
#' @noRd
cached <- function(key, expr) {
  cache <- app_cache()
  key <- rlang::hash(key)
  value <- cache$get(key)
  if (!cachem::is.key_missing(value)) {
    return(value)
  }
  value <- force(expr)
  cache$set(key, value)
  value
}

#' Run `expr` and turn any error into a friendly Shiny validation message.
#' @noRd
safely_validate <- function(expr, message) {
  tryCatch(
    expr,
    error = function(e) {
      if (inherits(e, "shiny.silent.error")) stop(e)
      validate(need(FALSE, message))
    }
  )
}

cran_repo <- function() {
  repos <- getOption("repos")
  repo <- if ("CRAN" %in% names(repos)) repos[["CRAN"]] else NA_character_
  if (is.na(repo) || !nzchar(repo) || repo == "@CRAN@") "https://cloud.r-project.org" else repo
}

#' Names of all packages currently available on CRAN.
#' @noRd
cran_packages <- function() {
  cached(list("available_packages", cran_repo()), {
    sort(rownames(utils::available.packages(repos = cran_repo())))
  })
}

#' Escape a version string so it can be used literally inside a regex.
#' @noRd
escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.-])", "\\\\\\1", x)
}

#' Position of the NEWS heading which belongs to a given version.
#'
#' Markdown headings are tried first, then any non-indented line, which covers
#' the most popular NEWS / NEWS.md conventions.
#' @noRd
news_version_pos <- function(news, version) {
  core <- paste0("(?<![0-9A-Za-z.-])", escape_regex(version), "(?![0-9]|[.-][0-9])")
  patterns <- c(paste0("^#+[^\\n]*", core), paste0("^\\S[^\\n]*", core))
  for (p in patterns) {
    pos <- stringr::str_locate(news, stringr::regex(p, multiline = TRUE))[1, "start"]
    if (!is.na(pos)) {
      return(unname(pos))
    }
  }
  NA_integer_
}

#' Extract the NEWS fragment covering the versions in `(old, new]`.
#' @noRd
news_excerpt <- function(news, new, old) {
  new_pos <- news_version_pos(news, new)
  old_pos <- news_version_pos(news, old)
  if (is.na(new_pos) || is.na(old_pos) || old_pos <= new_pos) {
    return(NA_character_)
  }
  trimws(stringr::str_sub(news, new_pos, old_pos - 1))
}

#' Download the NEWS file of a specific package version from the CRAN GitHub mirror.
#' @return A list with `text` and `file`, or `NULL` when no NEWS file exists.
#' @noRd
fetch_news <- function(pac, version) {
  cached(list("news", pac, version), {
    files <- c("NEWS.md", "NEWS", "inst/NEWS.md", "inst/NEWS", "NEWS.Rmd")
    for (file in files) {
      url <- sprintf("https://raw.githubusercontent.com/cran/%s/%s/%s", pac, version, file)
      resp <- httr2::request(url) %>%
        httr2::req_error(is_error = function(resp) {
          status <- httr2::resp_status(resp)
          status >= 400 && status != 404
        }) %>%
        httr2::req_timeout(20) %>%
        httr2::req_perform()
      if (!is.null(resp) && httr2::resp_status(resp) == 200) {
        return(list(text = httr2::resp_body_string(resp), file = file))
      }
    }
    NULL
  })
}

#' Split a DESCRIPTION URL-like field into individual http(s) links.
#' @noRd
split_urls <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(character(0))
  }
  urls <- trimws(strsplit(x, "[,\\s]+", perl = TRUE)[[1]])
  unique(urls[grepl("^https?://", urls)])
}

#' Split a "Name <email>" maintainer field.
#' @noRd
parse_maintainer <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(list(name = NA_character_, email = NA_character_))
  }
  email <- regmatches(x, regexpr("(?<=<)[^>]+(?=>)", x, perl = TRUE))
  list(
    name = trimws(sub("<.*$", "", x)),
    email = if (length(email)) email else NA_character_
  )
}

#' Best GitHub/GitLab/Codeberg repository link from the URL field.
#' @noRd
repo_url <- function(urls) {
  hit <- urls[grepl("(github\\.com|gitlab\\.com|codeberg\\.org|bitbucket\\.org)/[^/]+/[^/]+", urls)]
  if (length(hit)) hit[[1]] else NA_character_
}

check_levels <- c("OK", "NOTE", "WARN", "ERROR", "FAIL")

#' Overall CRAN check status (the worst status across flavors).
#' @noRd
worst_check_status <- function(status) {
  status <- toupper(trimws(status))
  status <- status[status %in% check_levels]
  if (!length(status)) {
    return(NA_character_)
  }
  check_levels[max(match(status, check_levels))]
}

status_color <- function(status) {
  if (is.null(status) || length(status) == 0 || is.na(status)) {
    return("secondary")
  }
  switch(status,
    OK = "success",
    NOTE = "info",
    WARN = "warning",
    ERROR = "danger",
    FAIL = "danger",
    "secondary"
  )
}

#' Human friendly number formatting (1234567 -> "1.2M").
#' @noRd
format_count <- function(x) {
  if (is.null(x) || length(x) != 1) {
    return("-")
  }
  x <- suppressWarnings(as.numeric(x))
  if (!is.finite(x)) {
    return("-")
  }
  if (abs(x) >= 1e9) {
    sprintf("%.1fB", x / 1e9)
  } else if (abs(x) >= 1e6) {
    sprintf("%.1fM", x / 1e6)
  } else if (abs(x) >= 1e4) {
    sprintf("%.1fk", x / 1e3)
  } else {
    format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
  }
}

#' Human friendly duration in days ("45 days", "3.2 years").
#' @noRd
format_days <- function(days) {
  if (is.null(days) || length(days) != 1) {
    return("-")
  }
  days <- suppressWarnings(as.numeric(days))
  if (!is.finite(days)) {
    return("-")
  }
  if (days >= 365) {
    sprintf("%.1f years", days / 365.25)
  } else if (days >= 60) {
    sprintf("%.0f months", days / 30.44)
  } else {
    sprintf("%.0f days", days)
  }
}

#' Apply transparent backgrounds and light/dark-aware fonts to a plotly chart.
#' @noRd
plotly_theme <- function(p, dark = FALSE) {
  fg <- if (isTRUE(dark)) "#dee2e6" else "#212529"
  grid <- if (isTRUE(dark)) "rgba(255,255,255,0.08)" else "rgba(0,0,0,0.07)"
  p %>%
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = fg, family = "Inter, system-ui, sans-serif"),
      xaxis = list(gridcolor = grid, zerolinecolor = grid),
      yaxis = list(gridcolor = grid, zerolinecolor = grid),
      legend = list(orientation = "h", y = -0.15),
      margin = list(l = 10, r = 10, t = 10, b = 10)
    ) %>%
    plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
}

#' Datatable with consistent, modern defaults.
#' @noRd
dt_table <- function(data, page_length = 15, ...) {
  DT::datatable(
    data,
    rownames = FALSE,
    style = "bootstrap5",
    class = "table table-hover table-sm align-middle",
    escape = FALSE,
    selection = "none",
    options = list(
      pageLength = page_length,
      lengthMenu = c(10, 15, 25, 50, 100),
      scrollX = TRUE,
      autoWidth = FALSE,
      dom = "<'d-flex justify-content-between align-items-center mb-2'fl>t<'d-flex justify-content-between align-items-center mt-2'ip>",
      language = list(search = "", searchPlaceholder = "Filter...", emptyTable = "Nothing to show")
    ),
    ...
  )
}

#' Bootstrap badge HTML (as string, for tables).
#' @noRd
badge_html <- function(text, color) {
  sprintf('<span class="badge rounded-pill text-bg-%s">%s</span>', color, htmltools::htmlEscape(text))
}

#' Link to a CRAN package page (as string, for tables).
#' @noRd
pkg_link_html <- function(pkg) {
  sprintf(
    '<a href="https://CRAN.R-project.org/package=%1$s" target="_blank" rel="noopener">%1$s</a>',
    htmltools::htmlEscape(pkg)
  )
}

empty_state <- function(message, icon = "inbox") {
  div(
    class = "empty-state",
    bsicons::bs_icon(icon, size = "2rem"),
    p(message)
  )
}

series_palette <- c("#2165b6", "#e8590c", "#2f9e44", "#ae3ec9", "#f59f00", "#0c8599", "#e03131", "#5c7cfa", "#868e96")

#' Aggregate daily downloads to `unit` buckets.
#'
#' Incomplete buckets at the edges of the date range (e.g. a partial first week) are
#' dropped, as they would otherwise show artificial dips. They are kept if
#' dropping them would leave less than two buckets.
#' @noRd
aggregate_downloads <- function(data, unit = "day") {
  data$date <- as.Date(data$date)
  if (unit == "day") {
    return(data[, c("package", "date", "count")])
  }
  data$bucket <- as.Date(lubridate::floor_date(data$date, unit, week_start = 1))
  agg <- data %>%
    dplyr::group_by(package, date = bucket) %>%
    dplyr::summarise(count = sum(count), n_days = dplyr::n(), .groups = "drop")
  bucket_end <- as.Date(lubridate::ceiling_date(agg$date, unit, change_on_boundary = TRUE, week_start = 1))
  complete <- agg$n_days >= as.numeric(bucket_end - agg$date)
  if (length(unique(agg$date[complete])) >= 2) agg <- agg[complete, ]
  as.data.frame(agg[, c("package", "date", "count")])
}
