utils::globalVariables(c(".", "bucket", "Archived", "LifeDuration", "NrUniqueDeps", "Released", "Version", "count", "package"))

#' Server logic for the Shiny application
#'
#' This function defines the server-side logic of the Shiny application. It handles user inputs,
#' processes data, and updates outputs accordingly. The server function sets up reactive expressions,
#' observers, and render functions to interact with the UI components.
#'
#' @param input Shiny input object containing input values from the UI.
#' @param output Shiny output object used to send output to the UI.
#' @param session Shiny session object.
#' @return None. This function is called for its side effects of setting up the server logic of the Shiny app.
#' @details
#' The server function performs several key operations:
#' \describe{
#'   \item{\strong{Package Search}}{Server-side autocomplete over all CRAN packages and validation of the selection.}
#'   \item{\strong{Shareable URLs}}{The selected package and page are kept in the URL (\code{?pkg=shiny&tab=downloads}).}
#'   \item{\strong{Caching}}{Network calls (CRAN, GitHub CRAN mirror, cranlogs) are cached across sessions.}
#'   \item{\strong{Version Handling}}{Retrieves package versions and updates version selection inputs in the UI.}
#'   \item{\strong{Versions Tab}}{Displays NEWS, DESCRIPTION dependencies and NAMESPACE differences between versions.}
#'   \item{\strong{Downloads Tab}}{Interactive download statistics of the package over time, with comparisons.}
#'   \item{\strong{Checkpage Tab}}{Displays CRAN check results and package status.}
#'   \item{\strong{Release List Tab}}{Shows the list and timeline of package releases.}
#'   \item{\strong{Dependencies Tab}}{Displays user and developer dependencies of the package.}
#'   \item{\strong{Info Tab}}{Provides package information such as title, description, version, license, etc.}
#' }
#' @export
app_server <- function(input, output, session) {
  default_pac <- "shiny"
  valid_tabs <- c("overview", "downloads", "version_diff", "cran_check", "package_deps", "cran_releases", "about")

  ### Search & URL state -----------------------------------------------------

  query <- parseQueryString(isolate(session$clientData$url_search) %||% "")
  initial_pac <- query[["pkg"]] %||% default_pac

  package_list_error <- NULL
  available <- tryCatch(
    cran_packages(),
    error = function(e) {
      package_list_error <<- conditionMessage(e)
      character(0)
    }
  )
  if (!is.null(package_list_error)) {
    warning("Could not load the CRAN package list: ", package_list_error, call. = FALSE)
    showNotification(
      "Could not load the CRAN package list. Check your internet connection and reload the app.",
      type = "error",
      duration = NULL
    )
  }
  if (length(available) && !initial_pac %in% available) {
    showNotification(sprintf("'%s' is not a CRAN package, showing '%s' instead.", initial_pac, default_pac), type = "warning")
    initial_pac <- default_pac
  }
  updateSelectizeInput(session, "pac", choices = available, selected = initial_pac, server = TRUE)
  updateSelectizeInput(session, "pac_compare", choices = available, server = TRUE)

  if (!is.null(query[["tab"]]) && query[["tab"]] %in% valid_tabs) {
    bslib::nav_select("nav", query[["tab"]], session = session)
  }

  observe({
    req(input$pac)
    tab <- input$nav %||% "overview"
    updateQueryString(sprintf("?pkg=%s&tab=%s", utils::URLencode(input$pac, reserved = TRUE), tab), mode = "replace")
  })

  pac <- reactive({
    req(input$pac)
    pkgs <- cran_packages()
    validate(need(input$pac %in% pkgs, sprintf("'%s' is not a valid CRAN package name.", input$pac)))
    input$pac
  })

  last_version <- reactive({
    p <- pac()
    safely_validate(
      cached(list("pac_last", p), pacs::pac_last(p, repos = cran_repo())),
      "Could not fetch the latest version from CRAN."
    )
  })

  get_timemachine <- reactive({
    p <- pac()
    safely_validate(
      fetch_release_history(p),
      "Could not fetch the release history from CRAN."
    )
  })

  get_description <- reactive({
    p <- pac()
    v <- last_version()
    safely_validate(
      cached(list("description", p, v), pacs::pac_description(p, v, repos = cran_repo())),
      "Could not fetch the DESCRIPTION file."
    )
  })

  desc_field <- function(field) {
    val <- get_description()[[field]]
    if (is.null(val) || length(val) == 0 || all(is.na(val)) || !nzchar(val[1])) NA_character_ else val
  }

  observeEvent(pac(), {
    updateSelectInput(session, "version_old", choices = character(0))
    updateSelectInput(session, "version_new", choices = character(0))
    tags_pac <- rev(get_timemachine()[["Version"]])
    updateSelectInput(session, "version_old", choices = tags_pac, selected = tags_pac[min(2, length(tags_pac))])
    updateSelectInput(session, "version_new", choices = tags_pac, selected = tags_pac[1])
  })

  observeEvent(input$swap_versions, {
    old <- input$version_old
    updateSelectInput(session, "version_old", selected = input$version_new)
    updateSelectInput(session, "version_new", selected = old)
  })

  ### Sidebar summary ----------------------------------------------------------

  checkpage <- reactive({
    p <- pac()
    safely_validate(
      cached(list("checkpage", p), pacs::pac_checkpage(p)),
      "Could not fetch CRAN check results."
    )
  })

  check_overall <- reactive({
    tab <- checkpage()
    if (!is.data.frame(tab) || !nrow(tab)) NA_character_ else worst_check_status(tab$Status)
  })

  output$pkg_summary <- renderUI({
    if (!isTRUE(nzchar(input$pac))) {
      return(empty_state("Search for a CRAN package to get started.", "search"))
    }
    p <- pac()
    urls <- split_urls(desc_field("URL"))
    repo <- repo_url(urls)
    bugs <- split_urls(desc_field("BugReports"))
    status <- tryCatch(check_overall(), error = function(e) NA_character_)

    link_btn <- function(href, icon, label) {
      tags$a(
        class = "btn btn-sm btn-outline-secondary", href = href, target = "_blank", rel = "noopener",
        bsicons::bs_icon(icon), label
      )
    }

    tags$div(
      class = "pkg-summary",
      tags$div(
        class = "d-flex align-items-center gap-2 flex-wrap",
        tags$h3(class = "pkg-name mb-0", p),
        tags$span(class = "badge text-bg-primary", paste0("v", last_version())),
        if (!is.na(status)) tags$span(class = paste0("badge text-bg-", status_color(status)), paste("CRAN", status))
      ),
      tags$p(class = "pkg-subtitle text-body-secondary", desc_field("Title")),
      tags$div(
        class = "d-flex flex-wrap gap-2",
        link_btn(sprintf("https://CRAN.R-project.org/package=%s", p), "box-arrow-up-right", "CRAN"),
        if (!is.na(repo)) link_btn(repo, if (grepl("github", repo)) "github" else "git", "Repo"),
        if (length(bugs)) link_btn(bugs[[1]], "bug", "Issues"),
        link_btn(sprintf("https://cran.r-project.org/web/checks/check_results_%s.html", p), "clipboard2-check", "Checks")
      )
    )
  })

  ### Version Diff Tab ---------------------------------------------------------

  pac_news <- reactive({
    p <- pac()
    news <- safely_validate(
      fetch_news(p, last_version()),
      "Could not fetch package NEWS from the CRAN mirror."
    )
    validate(need(!is.null(news), "This package does not ship a NEWS file."))
    news
  })

  versions_ok <- reactive({
    req(input$version_old, input$version_new)
    validate(
      need(
        isTRUE(utils::compareVersion(input$version_new, input$version_old) == 1),
        "The new version has to be higher than the old version."
      )
    )
    TRUE
  })

  output$news_diff <- renderUI({
    versions_ok()
    news <- pac_news()
    excerpt <- news_excerpt(news$text, input$version_new, input$version_old)
    validate(need(!is.na(excerpt), sprintf("Could not locate versions %s and %s in %s.", input$version_old, input$version_new, news$file)))
    if (grepl("\\.(md|Rmd)$", news$file)) {
      tags$div(class = "news-md", markdown(htmltools::htmlEscape(excerpt)))
    } else {
      tags$pre(class = "news-plain", excerpt)
    }
  })

  output$dep_version_diff <- DT::renderDataTable({
    versions_ok()
    validate(need(length(input$diff_deps_select) > 0, "Select at least one type of dependencies."))
    p <- pac()
    tab <- safely_validate(
      cached(
        list("compare_versions", p, input$version_old, input$version_new, sort(input$diff_deps_select)),
        pacs::pac_compare_versions(p, old = input$version_old, new = input$version_new, fields = input$diff_deps_select, repos = cran_repo())
      ),
      "Could not compare DESCRIPTION files."
    )
    tab <- as.data.frame(tab)
    names(tab) <- c("Package", paste("Required in", input$version_old), paste("Required in", input$version_new), "Change")
    tab[[1]] <- ifelse(tab[[1]] == "R", "R", pkg_link_html(tab[[1]]))
    for (i in 2:3) {
      tab[[i]] <- ifelse(is.na(tab[[i]]) | !nzchar(tab[[i]]), "\u2014", htmltools::htmlEscape(tab[[i]]))
    }
    tab[[4]] <- vapply(as.character(tab[[4]]), function(s) {
      switch(s,
        "1" = badge_html("added / raised", "success"),
        "-1" = badge_html("removed / lowered", "danger"),
        badge_html("unchanged", "secondary")
      )
    }, character(1))
    dt_table(tab, page_length = 25)
  })

  output$namespace_diff <- renderUI({
    versions_ok()
    p <- pac()
    ns <- safely_validate(
      cached(
        list("compare_namespace", p, input$version_old, input$version_new),
        pacs::pac_compare_namespace(p, old = input$version_old, new = input$version_new, repos = cran_repo())
      ),
      "Could not compare NAMESPACE files."
    )
    as_items <- function(x) {
      if (is.null(x) || length(x) == 0) {
        return(character(0))
      }
      if (is.data.frame(x)) {
        if (!nrow(x)) {
          return(character(0))
        }
        return(trimws(apply(x[, 1:2, drop = FALSE], 1, paste, collapse = ".")))
      }
      as.character(unlist(x))
    }
    sections <- Filter(Negate(is.null), lapply(names(ns), function(section) {
      added <- as_items(ns[[section]]$added)
      removed <- as_items(ns[[section]]$removed)
      if (!length(added) && !length(removed)) {
        return(NULL)
      }
      tags$div(
        class = "ns-section",
        tags$h6(section, tags$small(class = "text-body-secondary ms-1", sprintf("+%d / -%d", length(added), length(removed)))),
        tags$div(
          class = "d-flex flex-wrap gap-1",
          lapply(added, function(x) tags$span(class = "badge ns-added", paste("+", x))),
          lapply(removed, function(x) tags$span(class = "badge ns-removed", paste("\u2212", x)))
        )
      )
    }))
    if (!length(sections)) {
      return(empty_state("No NAMESPACE changes between these versions.", "check2-circle"))
    }
    tagList(sections)
  })

  ### Downloads Tab ------------------------------------------------------------

  set_range <- function(days) {
    updateDateRangeInput(session, "date_input", start = Sys.Date() - days, end = Sys.Date() - 1)
  }
  observeEvent(input$range_30, set_range(30))
  observeEvent(input$range_90, set_range(90))
  observeEvent(input$range_365, set_range(365))
  observeEvent(input$range_1825, set_range(5 * 365))

  compare_pacs <- reactive({
    raw <- input$pac_compare
    if (length(raw) == 1 && grepl(",", raw)) raw <- strsplit(raw, ",\\s*")[[1]]
    raw <- trimws(raw[nzchar(trimws(raw))])
    setdiff(unique(raw), pac())
  })

  download_pacs <- reactive({
    req(input$date_input)
    from <- as.Date(input$date_input[1])
    to <- as.Date(input$date_input[2])
    validate(need(!any(is.na(c(from, to))) && from <= to, "Please select a valid date range."))
    pkgs <- c(pac(), compare_pacs())
    safely_validate(
      cached(
        list("downloads", pkgs, from, to),
        cranlogs::cran_downloads(pkgs, from = from, to = to)
      ),
      "Could not fetch download statistics from cranlogs."
    )
  })

  downloads_main <- reactive({
    d <- download_pacs()
    d[d$package == pac(), , drop = FALSE]
  })

  output$downloads_total <- renderText({
    d <- downloads_main()
    if (!nrow(d)) "-" else format_count(sum(d$count))
  })
  output$downloads_avg <- renderText({
    d <- downloads_main()
    if (!nrow(d)) "-" else format_count(mean(d$count))
  })
  output$downloads_peak <- renderText({
    d <- downloads_main()
    if (!nrow(d)) "-" else format_count(max(d$count))
  })
  output$downloads_peak_date <- renderText({
    d <- downloads_main()
    if (!nrow(d)) {
      return("-")
    }
    format(d$date[which.max(d$count)], "%d %b %Y")
  })

  output$downloads_plot <- plotly::renderPlotly({
    unit <- input$date_unit %||% "day"
    data <- aggregate_downloads(download_pacs(), unit)
    pkgs <- c(pac(), compare_pacs())
    pkgs <- pkgs[pkgs %in% data$package]
    colors <- stats::setNames(rep_len(series_palette, length(pkgs)), pkgs)
    log_scale <- isTRUE(input$log_scale)
    if (log_scale) data$count <- pmax(data$count, 1)

    p <- plotly::plot_ly()
    for (pk in pkgs) {
      sub <- data[data$package == pk, ]
      p <- p %>% plotly::add_lines(
        x = sub$date, y = sub$count, name = pk, legendgroup = pk,
        line = list(width = 2.5, color = colors[[pk]], shape = "spline", smoothing = 0.4),
        hovertemplate = "%{y:,}<extra>%{fullData.name}</extra>"
      )
      if (isTRUE(input$add_smooth) && nrow(sub) >= 5) {
        fit <- tryCatch(
          stats::predict(stats::loess(count ~ as.numeric(date), data = sub, span = 0.5)),
          error = function(e) NULL
        )
        if (!is.null(fit)) {
          p <- p %>% plotly::add_lines(
            x = sub$date, y = if (log_scale) pmax(fit, 1) else fit,
            name = paste(pk, "trend"), legendgroup = pk, showlegend = FALSE,
            line = list(dash = "dot", width = 2, color = colors[[pk]]),
            opacity = 0.8,
            hovertemplate = "%{y:,.0f}<extra>%{fullData.name}</extra>"
          )
        }
      }
    }

    p %>%
      plotly_theme(identical(input$dark_mode, "dark")) %>%
      plotly::layout(
        hovermode = "x unified",
        showlegend = length(pkgs) > 1,
        xaxis = list(title = ""),
        yaxis = list(title = sprintf("Downloads per %s", unit), type = if (log_scale) "log" else "linear", tickformat = if (log_scale) NULL else "~s")
      )
  })

  output$downloads_csv <- downloadHandler(
    filename = function() sprintf("%s_downloads_%s_%s.csv", pac(), input$date_input[1], input$date_input[2]),
    content = function(file) utils::write.csv(download_pacs(), file, row.names = FALSE)
  )

  last_month <- reactive({
    p <- pac()
    safely_validate(
      cached(list("downloads_last_month", p, Sys.Date()), cranlogs::cran_downloads(p, when = "last-month")),
      "Download statistics are unavailable."
    )
  })

  output$downloads_month <- renderText({
    d <- last_month()
    if (!nrow(d)) "-" else format_count(sum(d$count))
  })
  output$downloads_month_daily <- renderText({
    d <- last_month()
    if (!nrow(d)) "No data" else sprintf("~%s per day", format_count(mean(d$count)))
  })

  output$downloads_spark <- plotly::renderPlotly({
    d <- last_month()
    plotly::plot_ly(d, x = ~date, y = ~count, hoverinfo = "x+y") %>%
      plotly::add_lines(
        color = I("rgba(255,255,255,0.9)"), fill = "tozeroy",
        fillcolor = "rgba(255,255,255,0.2)", span = I(1)
      ) %>%
      plotly::layout(
        xaxis = list(visible = FALSE, showgrid = FALSE, title = NA),
        yaxis = list(visible = FALSE, showgrid = FALSE, title = NA),
        hovermode = "x", margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = "white"),
        paper_bgcolor = "transparent", plot_bgcolor = "transparent"
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })

  ### Checkpage Tab ------------------------------------------------------------

  output$check_page <- renderUI({
    url <- sprintf("https://cran.r-project.org/web/checks/check_results_%s.html", pac())
    tags$a(
      class = "btn btn-sm btn-outline-primary", href = url, target = "_blank", rel = "noopener",
      bsicons::bs_icon("box-arrow-up-right"), " CRAN check page"
    )
  })

  red_status <- reactive({
    p <- pac()
    safely_validate(
      cached(list("checkred", p), pacs::pac_checkred(p)),
      "Could not fetch CRAN check status."
    )
  })

  output$red_status <- renderText({
    if (isTRUE(red_status())) "Failing" else "Healthy"
  })

  count_status <- function(levels) {
    tab <- checkpage()
    if (!is.data.frame(tab) || !nrow(tab)) {
      return(0L)
    }
    sum(toupper(trimws(tab$Status)) %in% levels)
  }
  output$checks_ok <- renderText(count_status("OK"))
  output$checks_warn <- renderText(count_status(c("NOTE", "WARN")))
  output$checks_error <- renderText(count_status(c("ERROR", "FAIL")))

  output$check_status_box <- renderUI({
    status <- tryCatch(check_overall(), error = function(e) NA_character_)
    tab <- tryCatch(checkpage(), error = function(e) NULL)
    detail <- if (!is.data.frame(tab) || !nrow(tab)) {
      "no results"
    } else {
      sprintf("%d of %d flavors OK", count_status("OK"), nrow(tab))
    }
    vbox(
      "CRAN checks", if (is.na(status)) "N/A" else status,
      if (identical(status, "OK")) "clipboard2-check" else "clipboard2-x",
      p(detail),
      theme = status_color(status)
    )
  })

  output$cran_check_results <- DT::renderDataTable({
    tab <- checkpage()
    validate(need(is.data.frame(tab) && nrow(tab) > 0, "No CRAN check results found."))
    tab[] <- lapply(tab, function(col) htmltools::htmlEscape(as.character(col)))
    tab$Status <- vapply(trimws(tab$Status), function(s) badge_html(s, status_color(toupper(s))), character(1))
    dt_table(tab, page_length = 25)
  })

  ### Release List Tab ---------------------------------------------------------

  order_timemachine <- reactive({
    get_timemachine() %>%
      dplyr::select(Version, Released, Archived, LifeDuration) %>%
      dplyr::arrange(dplyr::desc(Released))
  })

  output$releases_list <- DT::renderDataTable({
    table <- order_timemachine()
    validate(need(nrow(table) > 0, "No releases found."))
    table$LifeDuration <- as.numeric(table$LifeDuration)
    names(table)[names(table) == "LifeDuration"] <- "Life (days)"
    table$Archived <- as.character(table$Archived)
    table$Archived[is.na(table$Archived)] <- "current"
    table[] <- lapply(table, function(col) if (is.character(col)) htmltools::htmlEscape(col) else col)
    dt_table(table, page_length = 15)
  })

  output$releases_plot <- plotly::renderPlotly({
    tm <- order_timemachine()
    validate(need(nrow(tm) > 0, "No releases found."))
    tm$life <- as.numeric(tm$LifeDuration)
    tm$life[is.na(tm$life)] <- as.numeric(Sys.Date() - as.Date(tm$Released[is.na(tm$life)]))
    tm$current <- is.na(tm$Archived)
    # Each bar spans the lifetime of a release, from its release to its archival.
    tm$mid <- as.POSIXct(as.Date(tm$Released)) + tm$life * 86400 / 2
    plotly::plot_ly(tm) %>%
      plotly::add_bars(
        x = ~mid, y = ~life,
        width = tm$life * 86400 * 1000,
        marker = list(
          color = ifelse(tm$current, "#2f9e44", "#2165b6"),
          opacity = 0.85,
          line = list(color = "rgba(255,255,255,0.6)", width = 1)
        ),
        customdata = ~ format(as.Date(Released), "%d %b %Y"),
        text = ~Version, textposition = "none",
        hovertemplate = "<b>v%{text}</b><br>Released %{customdata}<br>Lived %{y} days<extra></extra>"
      ) %>%
      plotly_theme(identical(input$dark_mode, "dark")) %>%
      plotly::layout(
        xaxis = list(title = "", type = "date"),
        yaxis = list(title = "Life duration (days)"),
        bargap = 0
      )
  })

  newest_row <- reactive(utils::head(order_timemachine(), 1))
  oldest_row <- reactive(utils::tail(order_timemachine(), 1))

  output$newest_release <- renderText({
    sprintf("%s (%s)", newest_row()$Version, format(as.Date(newest_row()$Released), "%b %Y"))
  })

  output$oldest_release <- renderText({
    sprintf("%s (%s)", oldest_row()$Version, format(as.Date(oldest_row()$Released), "%b %Y"))
  })

  output$years_on_cran <- renderText({
    sprintf("%.1f", as.numeric(Sys.Date() - as.Date(oldest_row()$Released)) / 365.25)
  })

  output$number_releases <- renderText({
    nrow(get_timemachine())
  })

  output$min_releases <- renderText({
    format_days(min(get_timemachine()$LifeDuration, na.rm = TRUE))
  })

  output$mean_releases <- renderText({
    format_days(mean(as.numeric(get_timemachine()$LifeDuration), na.rm = TRUE))
  })

  output$max_releases <- renderText({
    format_days(max(get_timemachine()$LifeDuration, na.rm = TRUE))
  })

  ### Deps Tab -----------------------------------------------------------------

  user_deps <- reactive({
    p <- pac()
    safely_validate(
      cached(list("deps_user", p), pacs::pac_deps_user(p, local = FALSE, repos = cran_repo())),
      "Could not resolve user dependencies."
    )
  })

  dev_deps <- reactive({
    p <- pac()
    safely_validate(
      cached(list("deps_dev", p), pacs::pac_deps_dev(p, local = FALSE, repos = cran_repo())),
      "Could not resolve developer dependencies."
    )
  })

  heavy_deps <- function(fields) {
    p <- pac()
    tab <- safely_validate(
      cached(list("deps_heavy", p, fields), pacs::pac_deps_heavy(p, fields = fields, repos = cran_repo())),
      "Could not compute heavy dependencies."
    )
    tab <- dplyr::arrange(tab, dplyr::desc(NrUniqueDeps))
    tab$Package <- pkg_link_html(tab$Package)
    tab
  }

  deps_table <- function(tab) {
    tab <- as.data.frame(tab)
    validate(need(nrow(tab) > 0, "No dependencies - nice and lightweight!"))
    tab$Package <- pkg_link_html(tab$Package)
    tab$Version <- ifelse(nzchar(tab$Version), htmltools::htmlEscape(tab$Version), "\u2014")
    names(tab)[names(tab) == "Version"] <- "Min. version"
    dt_table(tab, page_length = 15)
  }

  output$number_user_dependencies <- renderText(nrow(user_deps()))
  output$number_user_dependencies_2 <- renderText(nrow(user_deps()))
  output$number_developer_dependencies <- renderText(nrow(dev_deps()))
  output$user_dependencies <- DT::renderDataTable(deps_table(user_deps()))
  output$developer_dependencies <- DT::renderDataTable(deps_table(dev_deps()))
  output$user_heavy_dependencies <- DT::renderDataTable({
    dt_table(heavy_deps(c("Depends", "Imports", "LinkingTo")))
  })
  output$developer_heavy_dependencies <- DT::renderDataTable({
    dt_table(heavy_deps(c("Depends", "Imports", "LinkingTo", "Suggests")))
  })

  output$description_deps <- renderUI({
    fields <- c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
    rows <- lapply(fields, function(field) {
      val <- desc_field(field)
      deps <- if (is.na(val)) character(0) else trimws(strsplit(gsub("\\s+", " ", val), ",")[[1]])
      deps <- deps[nzchar(deps)]
      tags$tr(
        tags$th(scope = "row", field, tags$span(class = "badge text-bg-light ms-1", length(deps))),
        tags$td(
          if (!length(deps)) {
            tags$span(class = "text-body-secondary", "\u2014")
          } else {
            tags$div(
              class = "d-flex flex-wrap gap-1",
              lapply(deps, function(d) {
                name <- sub("\\s*\\(.*$", "", d)
                ver <- regmatches(d, regexpr("\\(.*\\)", d))
                tags$span(
                  class = "dep-chip",
                  if (name == "R") name else tags$a(href = sprintf("https://CRAN.R-project.org/package=%s", name), target = "_blank", rel = "noopener", name),
                  if (length(ver)) tags$small(class = "text-body-secondary ms-1", ver)
                )
              })
            )
          }
        )
      )
    })
    tags$table(
      class = "table details-table deps-table mb-0",
      tags$tbody(rows)
    )
  })

  ### Info Tab -----------------------------------------------------------------

  output$name <- renderText({
    pac()
  })

  output$title <- renderText({
    desc_field("Title")
  })

  output$description <- renderText({
    gsub("\\s+", " ", desc_field("Description"))
  })

  output$last_version <- renderText({
    last_version()
  })

  output$license <- renderText({
    desc_field("License")
  })

  output$maintainer <- renderUI({
    m <- parse_maintainer(desc_field("Maintainer"))
    if (is.na(m$email)) {
      return(m$name)
    }
    tags$a(href = paste0("mailto:", m$email), m$name)
  })

  link_list <- function(urls) {
    if (!length(urls)) {
      return(tags$span(class = "text-body-secondary", "\u2014"))
    }
    tags$div(
      class = "d-flex flex-column",
      lapply(urls, function(e) tags$a(href = e, target = "_blank", rel = "noopener", class = "text-break", e))
    )
  }

  output$bugreports <- renderUI(link_list(split_urls(desc_field("BugReports"))))
  output$url <- renderUI(link_list(split_urls(desc_field("URL"))))

  publication_date <- reactive({
    res <- desc_field("Date/Publication")
    lubridate::as_date(res)
  })

  output$publication <- renderText({
    d <- publication_date()
    if (is.na(d)) "-" else format(d, "%d %b %Y")
  })

  output$publication_ago <- renderText({
    d <- publication_date()
    if (is.na(d)) {
      return("")
    }
    days <- as.numeric(Sys.Date() - d)
    if (days == 0) "published today" else sprintf("published %s ago", format_days(days))
  })

  output$cran_url <- renderUI({
    url <- sprintf("https://CRAN.R-project.org/package=%s", pac())
    tags$a(href = url, target = "_blank", rel = "noopener", url)
  })

  output$encoding <- renderText({
    val <- desc_field("Encoding")
    if (is.na(val)) "-" else val
  })

  output$compilation <- renderText({
    val <- desc_field("NeedsCompilation")
    if (is.na(val)) "-" else val
  })

  output$requirements <- renderText({
    reqs <- desc_field("SystemRequirements")
    rdep <- stringr::str_extract(desc_field("Depends"), "R \\([^)]*\\)")
    out <- c(if (!is.na(rdep)) rdep, if (!is.na(reqs)) gsub("\\s+", " ", reqs))
    if (length(out)) paste(out, collapse = "; ") else "-"
  })
}
