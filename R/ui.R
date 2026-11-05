#' Theme of the application
#' @noRd
app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    primary = "#2165b6",
    secondary = "#6c757d",
    success = "#198754",
    info = "#0dcaf0",
    "border-radius" = "0.6rem",
    "card-border-radius" = "0.9rem",
    base_font = bslib::font_collection(
      bslib::font_google("Inter", local = FALSE),
      "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"
    ),
    code_font = bslib::font_collection(
      bslib::font_google("JetBrains Mono", local = FALSE),
      "SFMono-Regular", "Menlo", "monospace"
    ),
    heading_font = bslib::font_collection(
      bslib::font_google("Inter", local = FALSE),
      "system-ui", "sans-serif"
    )
  )
}

vbox <- function(title, value, icon, ..., theme = "primary") {
  bslib::value_box(
    title = title,
    value = value,
    showcase = bsicons::bs_icon(icon),
    theme = theme,
    ...
  )
}

info_row <- function(label, output) {
  tags$tr(tags$th(scope = "row", label), tags$td(output))
}

#' User Interface for the Shiny Application
#'
#' This function defines the user interface of the Shiny dashboard application. It is built with
#' \code{bslib} (Bootstrap 5) and consists of a global sidebar with a CRAN package search and a package
#' summary, plus several pages: overview, downloads, version differences, CRAN check results,
#' dependencies, releases and about. A light/dark mode switch is available in the navigation bar.
#'
#' @return A Shiny UI definition created using \code{bslib::page_navbar}.
#' @import shiny
#' @import magrittr
#' @importFrom pkgload load_all
#' @export
app_ui <- function() {
  page_title <- "R Dev Dashboard"

  bslib::page_navbar(
    id = "nav",
    title = tags$span(
      class = "app-brand",
      tags$img(src = "https://cran.r-project.org/Rlogo.svg", alt = "R logo", height = "30"),
      tags$span(page_title)
    ),
    window_title = page_title,
    theme = app_theme(),
    lang = "en",
    fillable = FALSE,
    header = tagList(
      tags$head(
        tags$link(rel = "icon", type = "image/x-icon", href = "images/favicon.ico"),
        tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css"),
        tags$meta(name = "description", content = "Explore any CRAN package: downloads, releases, dependencies, CRAN checks and version differences.")
      ),
      shiny::useBusyIndicators()
    ),
    sidebar = bslib::sidebar(
      id = "sidebar",
      width = 320,
      open = list(desktop = "open", mobile = "always-above"),
      selectizeInput(
        "pac",
        label = tags$span(bsicons::bs_icon("search"), "CRAN package"),
        choices = NULL,
        width = "100%",
        options = list(
          placeholder = "Search CRAN packages...",
          maxOptions = 50,
          openOnFocus = FALSE,
          closeAfterSelect = TRUE
        )
      ),
      uiOutput("pkg_summary"),
      tags$div(
        class = "sidebar-footer small text-body-secondary",
        "Powered by ",
        tags$a(href = "https://CRAN.R-project.org/package=pacs", target = "_blank", rel = "noopener", tags$code("pacs")),
        " and ",
        tags$a(href = "https://github.com/r-hub/cranlogs", target = "_blank", rel = "noopener", tags$code("cranlogs"))
      )
    ),

    # Overview ---------------------------------------------------------------
    bslib::nav_panel(
      title = "Overview",
      value = "overview",
      icon = bsicons::bs_icon("grid-1x2"),
      bslib::layout_column_wrap(
        width = "240px",
        fill = FALSE,
        heights_equal = "row",
        vbox(
          "Latest version", textOutput("last_version", inline = TRUE), "tag",
          p(textOutput("publication_ago", inline = TRUE))
        ),
        bslib::value_box(
          title = "Downloads (last 30 days)",
          value = textOutput("downloads_month", inline = TRUE),
          p(textOutput("downloads_month_daily", inline = TRUE)),
          showcase = plotly::plotlyOutput("downloads_spark", height = "70px"),
          showcase_layout = "bottom",
          theme = "teal"
        ),
        uiOutput("check_status_box", class = "h-100"),
        vbox("Dependencies", textOutput("number_user_dependencies", inline = TRUE), "diagram-3",
          theme = "indigo",
          p("recursive, needed to install")
        )
      ),
      bslib::layout_columns(
        col_widths = c(7, 5),
        bslib::card(
          bslib::card_header(bsicons::bs_icon("journal-text"), "Description"),
          bslib::card_body(
            h4(class = "pkg-title", textOutput("title", inline = TRUE)),
            p(class = "lead-sm", textOutput("description", inline = TRUE))
          )
        ),
        bslib::card(
          bslib::card_header(bsicons::bs_icon("card-list"), "Details"),
          bslib::card_body(
            class = "p-0",
            tags$table(
              class = "table table-sm details-table mb-0",
              tags$tbody(
                info_row("Package", textOutput("name", inline = TRUE)),
                info_row("License", textOutput("license", inline = TRUE)),
                info_row("Maintainer", uiOutput("maintainer", inline = TRUE)),
                info_row("Published", textOutput("publication", inline = TRUE)),
                info_row("CRAN page", uiOutput("cran_url", inline = TRUE)),
                info_row("URL", uiOutput("url", inline = TRUE)),
                info_row("Bug reports", uiOutput("bugreports", inline = TRUE)),
                info_row("Compilation", textOutput("compilation", inline = TRUE)),
                info_row("Requirements", textOutput("requirements", inline = TRUE)),
                info_row("Encoding", textOutput("encoding", inline = TRUE))
              )
            )
          )
        )
      ),
      bslib::accordion(
        id = "github_details",
        open = FALSE,
        bslib::accordion_panel(
          "GitHub issues (open / closed)",
          value = "issues",
          icon = bsicons::bs_icon("github"),
          uiOutput("github_issues")
        )
      )
    ),

    # Downloads --------------------------------------------------------------
    bslib::nav_panel(
      title = "Downloads",
      value = "downloads",
      icon = bsicons::bs_icon("graph-up-arrow"),
      bslib::layout_column_wrap(
        width = "220px",
        fill = FALSE,
        heights_equal = "row",
        vbox("Total downloads", textOutput("downloads_total", inline = TRUE), "cloud-download"),
        vbox("Daily average", textOutput("downloads_avg", inline = TRUE), "calendar-day", theme = "teal"),
        vbox("Best day", textOutput("downloads_peak", inline = TRUE), "trophy",
          theme = "indigo",
          p(textOutput("downloads_peak_date", inline = TRUE))
        )
      ),
      bslib::card(
        full_screen = TRUE,
        min_height = "520px",
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          tags$span(bsicons::bs_icon("graph-up"), "Downloads over time"),
          downloadButton("downloads_csv", "CSV", class = "btn-sm btn-outline-primary", icon = icon("download"))
        ),
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            width = 270,
            title = "Options",
            dateRangeInput(
              "date_input", "Date range",
              start = Sys.Date() - 365, end = Sys.Date() - 1, max = Sys.Date()
            ),
            tags$div(
              class = "btn-group btn-group-sm w-100 mb-3", role = "group",
              actionButton("range_30", "30d", class = "btn-outline-secondary"),
              actionButton("range_90", "90d", class = "btn-outline-secondary"),
              actionButton("range_365", "1y", class = "btn-outline-secondary"),
              actionButton("range_1825", "5y", class = "btn-outline-secondary")
            ),
            selectizeInput(
              "pac_compare", "Compare with",
              choices = NULL, multiple = TRUE,
              options = list(placeholder = "Add packages...", maxOptions = 50, maxItems = 8, openOnFocus = FALSE, closeAfterSelect = TRUE)
            ),
            radioButtons(
              "date_unit", "Aggregate by",
              choices = c(Day = "day", Week = "week", Month = "month", Year = "year"),
              selected = "week", inline = TRUE
            ),
            bslib::input_switch("add_smooth", "Trend line", FALSE),
            bslib::input_switch("log_scale", "Log scale", FALSE),
            tags$small(
              class = "text-body-secondary",
              tags$a(
                href = "https://github.com/gpilgrim2670/ThreeWiseMonkeys?tab=readme-ov-file#whats-the-point",
                target = "_blank", rel = "noopener",
                "ThreeWiseMonkeys"
              ),
              " can be used as a zero-downloads benchmark."
            )
          ),
          plotly::plotlyOutput("downloads_plot", height = "440px")
        )
      )
    ),

    # Versions ---------------------------------------------------------------
    bslib::nav_panel(
      title = "Versions",
      value = "version_diff",
      icon = bsicons::bs_icon("arrow-left-right"),
      bslib::card(
        class = "overflow-visible",
        bslib::card_body(
          class = "overflow-visible",
          bslib::layout_columns(
            col_widths = c(5, 2, 5),
            selectInput("version_old", "Old version", choices = NULL, width = "100%"),
            tags$div(
              class = "d-flex align-items-end justify-content-center h-100 pb-3",
              actionButton("swap_versions", bsicons::bs_icon("arrow-left-right"), class = "btn-outline-primary", title = "Swap versions", `aria-label` = "Swap versions")
            ),
            selectInput("version_new", "New version", choices = NULL, width = "100%")
          )
        )
      ),
      bslib::card(
        full_screen = TRUE,
        max_height = "520px",
        bslib::card_header(bsicons::bs_icon("newspaper"), "NEWS between versions"),
        bslib::card_body(class = "news-content", uiOutput("news_diff"))
      ),
      bslib::layout_columns(
        col_widths = c(7, 5),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(bsicons::bs_icon("box-seam"), "DESCRIPTION dependencies diff"),
          bslib::card_body(
            class = "overflow-visible",
            checkboxGroupInput(
              "diff_deps_select", NULL,
              choices = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"),
              selected = c("Depends", "Imports", "LinkingTo"),
              inline = TRUE
            ),
            DT::dataTableOutput("dep_version_diff", fill = FALSE)
          )
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(bsicons::bs_icon("braces"), "NAMESPACE diff"),
          bslib::card_body(uiOutput("namespace_diff"))
        )
      )
    ),

    # CRAN checks -------------------------------------------------------------
    bslib::nav_panel(
      title = "CRAN Checks",
      value = "cran_check",
      icon = bsicons::bs_icon("clipboard2-check"),
      bslib::layout_column_wrap(
        width = "200px",
        fill = FALSE,
        heights_equal = "row",
        vbox("Overall status", textOutput("red_status", inline = TRUE), "activity",
          theme = "primary",
          p("ERROR or FAIL on any flavor")
        ),
        vbox("OK", textOutput("checks_ok", inline = TRUE), "check-circle", theme = "success"),
        vbox("NOTE / WARN", textOutput("checks_warn", inline = TRUE), "exclamation-triangle", theme = "warning"),
        vbox("ERROR / FAIL", textOutput("checks_error", inline = TRUE), "x-octagon", theme = "danger")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          tags$span(bsicons::bs_icon("table"), "Check results by flavor"),
          uiOutput("check_page", inline = TRUE)
        ),
        DT::dataTableOutput("cran_check_results", fill = FALSE)
      )
    ),

    # Dependencies -----------------------------------------------------------
    bslib::nav_panel(
      title = "Dependencies",
      value = "package_deps",
      icon = bsicons::bs_icon("diagram-3"),
      bslib::layout_column_wrap(
        width = "260px",
        fill = FALSE,
        heights_equal = "row",
        vbox(
          "User dependencies", textOutput("number_user_dependencies_2", inline = TRUE), "person",
          p("Depends, Imports, LinkingTo - recursive")
        ),
        vbox("Developer dependencies", textOutput("number_developer_dependencies", inline = TRUE), "tools",
          theme = "indigo",
          p("+ Suggests - recursive")
        )
      ),
      bslib::card(
        bslib::card_header(bsicons::bs_icon("file-earmark-text"), "DESCRIPTION dependencies"),
        bslib::card_body(class = "p-0", uiOutput("description_deps"))
      ),
      bslib::card(
        bslib::card_header(bsicons::bs_icon("pc-display"), "System requirements"),
        bslib::card_body(textOutput("system_requirements"))
      ),
      bslib::navset_card_underline(
        full_screen = TRUE,
        title = tags$span(bsicons::bs_icon("list-nested"), "Dependency tree"),
        bslib::nav_panel("User", DT::dataTableOutput("user_dependencies", fill = FALSE)),
        bslib::nav_panel("Developer", DT::dataTableOutput("developer_dependencies", fill = FALSE)),
        bslib::nav_panel("Heavy (user)", DT::dataTableOutput("user_heavy_dependencies", fill = FALSE)),
        bslib::nav_panel("Heavy (developer)", DT::dataTableOutput("developer_heavy_dependencies", fill = FALSE))
      )
    ),

    # Releases ---------------------------------------------------------------
    bslib::nav_panel(
      title = "Releases",
      value = "cran_releases",
      icon = bsicons::bs_icon("clock-history"),
      bslib::layout_column_wrap(
        width = "220px",
        fill = FALSE,
        heights_equal = "row",
        vbox(
          "Releases", textOutput("number_releases", inline = TRUE), "tags",
          p("newest ", textOutput("newest_release", inline = TRUE))
        ),
        vbox("Years on CRAN", textOutput("years_on_cran", inline = TRUE), "hourglass-split",
          theme = "teal",
          p("first ", textOutput("oldest_release", inline = TRUE))
        ),
        vbox("Mean release life", textOutput("mean_releases", inline = TRUE), "speedometer2",
          theme = "indigo",
          p("min ", textOutput("min_releases", inline = TRUE), " / max ", textOutput("max_releases", inline = TRUE))
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(bsicons::bs_icon("bar-chart-steps"), "Release timeline"),
        plotly::plotlyOutput("releases_plot", height = "320px")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(bsicons::bs_icon("table"), "All CRAN releases"),
        DT::dataTableOutput("releases_list", fill = FALSE)
      )
    ),

    # About ------------------------------------------------------------------
    bslib::nav_panel(
      title = "About",
      value = "about",
      icon = bsicons::bs_icon("question-circle"),
      tags$div(
        class = "about-page",
        bslib::card(
          bslib::card_body(
            class = "p-4 p-lg-5",
            h2("R Package Developer Dashboard"),
            p(
              class = "lead",
              "A fast, friendly way to inspect any CRAN package - built for R package developers and maintainers."
            ),
            bslib::layout_column_wrap(
              width = "260px",
              fill = FALSE,
              heights_equal = "row",
              about_feature("grid-1x2", "Overview", "Latest version, license, maintainer, links and recent downloads at a glance."),
              about_feature("graph-up-arrow", "Downloads", "Interactive download trends, comparisons with other packages and CSV export."),
              about_feature("arrow-left-right", "Versions", "NEWS, dependency and NAMESPACE differences between any two releases."),
              about_feature("clipboard2-check", "CRAN Checks", "Check results across all CRAN flavors with a quick health summary."),
              about_feature("diagram-3", "Dependencies", "Recursive user and developer dependencies, plus the heaviest ones."),
              about_feature("clock-history", "Releases", "Full release history and timeline of the package on CRAN.")
            ),
            h4(class = "mt-4", "Powered by ", tags$code("pacs")),
            tags$div(
              class = "d-flex flex-column flex-md-row gap-4 align-items-center",
              tags$img(
                src = "https://raw.githubusercontent.com/Polkas/pacs/main/man/figures/pacs_logo.svg",
                alt = "pacs logo", width = "140", height = "140"
              ),
              p(
                tags$code("pacs"),
                " is an R package that provides a suite of functions to interact with CRAN packages, analyze dependencies,",
                " compare package versions, and much more. It serves as the backbone of this dashboard. For more information, visit the ",
                tags$a(href = "https://CRAN.R-project.org/package=pacs", "pacs CRAN page", target = "_blank", rel = "noopener"),
                "."
              )
            ),
            tags$div(
              class = "d-flex flex-wrap gap-2 mt-3",
              tags$a(
                class = "btn btn-primary", href = "https://github.com/Polkas/rdevdash", target = "_blank", rel = "noopener",
                bsicons::bs_icon("github"), " Source code"
              ),
              tags$a(
                class = "btn btn-outline-primary", href = "https://github.com/Polkas/rdevdash/issues", target = "_blank", rel = "noopener",
                bsicons::bs_icon("bug"), " Report an issue"
              )
            )
          )
        )
      )
    ),
    bslib::nav_spacer(),
    bslib::nav_item(
      tags$a(
        class = "nav-link", href = "https://github.com/Polkas/rdevdash", target = "_blank", rel = "noopener",
        title = "Source code on GitHub", `aria-label` = "Source code on GitHub",
        bsicons::bs_icon("github")
      )
    ),
    bslib::nav_item(bslib::input_dark_mode(id = "dark_mode"))
  )
}

about_feature <- function(icon, title, text) {
  tags$div(
    class = "about-feature",
    tags$div(class = "about-feature-icon", bsicons::bs_icon(icon)),
    tags$div(tags$strong(title), tags$p(class = "mb-0 small text-body-secondary", text))
  )
}
