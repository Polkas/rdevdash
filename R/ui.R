#' User Interface for the Shiny Application
#'
#' This function defines the user interface of the Shiny dashboard application. It sets up the dashboard layout,
#' including the header, sidebar, and body with multiple tabs for different functionalities such as package information,
#' downloads, version differences, CRAN check results, dependencies, releases, and about.
#'
#' @return A Shiny UI definition created using \code{shinydashboard::dashboardPage}.
#' @import shiny
#' @import shinydashboard
#' @import magrittr
#' @export
app_ui <- function() {
  av_cran <- "https://cran.r-project.org/web/packages/available_packages_by_name.html"

  dashboardPage(
    skin = "blue",
    title = "R Dev Dashboard",
    dashboardHeader(
      title = tags$span(
        tags$img(
          src = "https://cran.r-project.org/Rlogo.svg",
          width = "40px",
          height = "40px",
          style = "margin-bottom:10px;margin-right:10px;"
        ),
        "R Dev Dashboard"
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = av_cran, "CRAN package:", style = "font-weight:bold;")
      ),
      tags$li(
        class = "dropdown search-input",
        shinyWidgets::searchInput(
          "pac",
          value = "shiny",
          btnSearch = icon("magnifying-glass"),
          btnReset = icon("xmark"),
          placeholder = "Enter a one CRAN package",
          btnClass = "btn-default btn-outline-secondary",
          width = 350
        )
      ),
      titleWidth = 250
    ),

    dashboardSidebar(
      width = 250,
      sidebarMenu(
        menuItem("Package Info", tabName = "package_info", icon = icon("info-circle"), selected = TRUE),
        menuItem("Package Downloads", tabName = "downloads", icon = icon("chart-line")),
        menuItem("Differences between Versions", tabName = "version_diff", icon = icon("exchange-alt")),
        menuItem("CRAN Check Results", tabName = "cran_check", icon = icon("check-circle")),
        menuItem("Package Deps", tabName = "package_deps", icon = icon("project-diagram")),
        menuItem("CRAN Releases", tabName = "cran_releases", icon = icon("history")),
        menuItem("About", tabName = "about", icon = icon("question"))
      )
    ),

    dashboardBody(

      tags$head(tags$link(rel = "icon", type = "image/x-icon", href = "images/favicon.ico")),
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css")),

      tabItems(
        tabItem(
          tabName = "package_info",
          fluidRow(
            box(
              class = "important-text",
              title = "Name",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(textOutput("name"))
            )
          ),
          fluidRow(
            box(
              class = "important-text",
              title = "Title",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(textOutput("title"))
            ),
            box(
              class = "important-text",
              title = "CRAN page",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(uiOutput("cran_url"))
            )
          ),
          fluidRow(
            box(
              class = "important-text",
              title = "Description",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(textOutput("description"))
            ),
          ),
          fluidRow(
            box(
              class = "important-text",
              title = "Package Last Version",
              width = 4,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(textOutput("last_version"))
            ),
            box(
              class = "important-text",
              title = "Package License",
              width = 4,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(textOutput("license"))
            ),
            box(
              class = "important-text",
              title = "Package Maintainer",
              width = 4,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(uiOutput("maintainer"))
            )
          ),
          fluidRow(
            box(
              class = "important-text",
              title = "Publication",
              width = 4,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(textOutput("publication"))
            ),
            box(
              class = "important-text",
              title = "URL",
              width = 4,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(uiOutput("url"))
            ),
            box(
              class = "important-text",
              title = "BugReports",
              width = 4,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(uiOutput("bugreports"))
            )
          ),
          fluidRow(
            box(
              class = "important-text",
              title = "Needs Compilation",
              width = 4,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(textOutput("compilation"))
            ),
            box(
              class = "important-text",
              title = "System Requirements",
              width = 4,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(textOutput("requirements"))
            ),
            box(
              class = "important-text",
              title = "Encoding",
              width = 4,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(textOutput("encoding"))
            )
          )
        ),

        tabItem(
          tabName = "downloads",
          fluidRow(
            box(
              title = "Package Downloads Plot",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              dateRangeInput("date_input", "Date Range", start = Sys.Date() - 100, end = Sys.Date()),
              textInput("pac_compare", "Compare with", placeholder = "Packages to compare with, comma seperated."),
              tags$div(
                style = "margin-bottom:10px;",
                tags$a(
                  href = "https://github.com/gpilgrim2670/ThreeWiseMonkeys?tab=readme-ov-file#whats-the-point",
                  "ThreeWiseMonkeys package can be used as a zero downloads benchmark"
                )
              ),
              selectizeInput("date_unit", label = "Unit", choices = c("day", "month", "year"), selected = "day"),
              checkboxInput("add_smooth", "Add Smooth", FALSE),
              shinycssloaders::withSpinner(plotOutput("downloads_plot", height = "400px"))
            )
          )
        ),

        tabItem(
          tabName = "version_diff",
          fluidRow(
            box(
              title = "Versions to Compare",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              style = "overflow:visible",
              selectInput("version_new", "Version New:", choices = c("")),
              selectInput("version_old", "Version Old:", choices = c("")),
            )
          ),
          fluidRow(
            box(
              id = "news_diff_box_body",
              title = "NEWS",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              height = 350,
              shinycssloaders::withSpinner(verbatimTextOutput("news_diff"))
            )
          ),
          fluidRow(
            box(
              style = "overflow: scroll !important;",
              title = "DESCRIPTION Deps Diff",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              footer = "1 == added;0 == not changed;-1 == removed",
              selectInput(
                "diff_deps_select",
                "Types of Dependencies",
                choices = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"),
                selected = c("Depends", "Imports", "LinkingTo"),
                multiple = TRUE
              ),
              shinycssloaders::withSpinner(DT::dataTableOutput("dep_version_diff"))
            ),
            box(
              title = "Namespace Diff",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(verbatimTextOutput("namespace_diff"))
            )
          )
        ),

        tabItem(
          tabName = "cran_check",
          fluidRow(
            box(
              class = "important-text",
              title = "RED status",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              footer = "Whether any Flavor got Error or Fail status",
              uiOutput("red_status")
            ),
            box(
              class = "important-text",
              title = "CRAN Check Page",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              uiOutput("check_page")
            )
          ),
          fluidRow(
            box(
              style = "overflow: scroll !important;",
              title = "CRAN Check Results",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(DT::dataTableOutput("cran_check_results"))
            )
          )
        ),

        tabItem(
          tabName = "package_deps",
          fluidRow(
            box(
              title = "DESCRIPTION deps",
              solidHeader = TRUE,
              width = 12,
              status = "primary",
              shinycssloaders::withSpinner(uiOutput("description_deps"))
            )
          ),
          fluidRow(
            box(
              class = "important-text",
              title = "Number of USER Dependencies",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              footer = "Depends, Imports, LinkingTo - Recursive",
              shinycssloaders::withSpinner(textOutput("number_user_dependencies"))
            ),
            box(
              class = "important-text",
              title = "Number of DEVELOPER Dependencies",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              footer = "Depends, Imports, LinkingTo, Suggests - Recursive",
              shinycssloaders::withSpinner(textOutput("number_developer_dependencies"))
            )
          ),
          fluidRow(
            box(
              title = "Package USER Dependencies",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(DT::dataTableOutput("user_dependencies")),
              footer = "Depends, Imports, LinkingTo - Recursive"
            ),
            box(
              style = "overflow: scroll !important;",
              title = "USER Heavy Dependencies",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              footer = "Depends, Imports, LinkingTo",
              shinycssloaders::withSpinner(DT::dataTableOutput("user_heavy_dependencies"))
            )
          ),
          fluidRow(
            box(
              title = "Package DEVELOPER Dependencies",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(DT::dataTableOutput("developer_dependencies")),
              footer = "Depends, Imports, LinkingTo, Suggests - Recursive"
            ),
            box(
              style = "overflow: scroll !important;",
              title = "DEVELOPER Heavy Dependencies",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              footer = "Depends, Imports, LinkingTo, Suggests",
              shinycssloaders::withSpinner(DT::dataTableOutput("developer_heavy_dependencies"))
            )
          )
        ),

        tabItem(
          tabName = "cran_releases",
          fluidRow(
            box(
              class = "important-text",
              title = "Newest Release",
              width = 4,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(textOutput("newest_release"))
            ),
            box(
              class = "important-text",
              title = "Oldest Release",
              width = 4,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(textOutput("oldest_release"))
            ),
            box(
              class = "important-text",
              title = "Years on CRAN",
              width = 4,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(textOutput("years_on_cran"))
            )
          ),
          fluidRow(
            box(
              class = "important-text",
              title = "Number of Releases",
              width = 3,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(textOutput("number_releases"))
            ),
            box(
              class = "important-text",
              title = "Minimum Life Duration",
              width = 3,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(textOutput("min_releases"))
            ),
            box(
              class = "important-text",
              title = "Mean Life Duration",
              width = 3,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(textOutput("mean_releases"))
            ),
            box(
              class = "important-text",
              title = "Maximum Life Duration",
              width = 3,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(textOutput("max_releases"))
            )
          ),
          fluidRow(
            box(
              style = "overflow: scroll !important;",
              title = "CRAN Releases",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              shinycssloaders::withSpinner(DT::dataTableOutput("releases_list"))
            )
          )
        ),

        tabItem(
          tabName = "about",
          fluidRow(
            div(
              class = "col-sm-8",
              style = "margin: 0px auto; float: none; padding:20px;",
              a(href = "https://github.com/Polkas/rdevdash/issues", h1("Report Issue")),
              h2("About This Dashboard"),
              p(
                "Welcome to the ",
                strong("R Package Developer Dashboard"),
                ", a comprehensive tool designed to streamline the process of tracking and managing CRAN packages."
              ),
              h3("Key Dashboard Features"),
              tags$ul(
                tags$li(
                  strong("Package Information:"),
                  " Quickly retrieve and display detailed information about any CRAN package, including the latest version, license, and maintainer details."
                ),
                tags$li(
                  strong("Package Downloads:"),
                  " Visualize and compare package download statistics over time."
                ),
                tags$li(
                  strong("Version Differences:"),
                  " Compare different versions of a package to track changes and updates."
                ),
                tags$li(
                  strong("CRAN Check Results:"),
                  " View CRAN check results."
                ),
                tags$li(
                  strong("Package Dependencies:"),
                  " Analyze package dependencies from user and developer perspective."
                ),
                tags$li(
                  strong("CRAN Releases:"),
                  " Stay updated with all CRAN releases for a package."
                )
              ),
              p(
                "This dashboard is primarily powered by the ",
                code("pacs"),
                " package, which is an essential toolkit for R developers looking to gain insights into package development, maintenance, and deployment on CRAN."
              ),
              h3("What is ", code("pacs"), "?"),
              img(src = "https://raw.githubusercontent.com/Polkas/pacs/main/man/figures/pacs_logo.svg", width = "300px", height = "300px"),
              p(
                code("pacs"),
                " is an R package that provides a suite of functions to interact with CRAN packages, analyze dependencies, compare package versions, and much more. It serves as the backbone of this dashboard. For more information, visit the ",
                a(href = "https://CRAN.R-project.org/package=pacs", "pacs CRAN page", target = "_blank"),
                "."
              )
            )
          )
        )
      )
    )
  )
}
