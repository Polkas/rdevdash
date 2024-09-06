library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(ggplot2)
library(scales)
library(pacs)
library(stringr)
library(DT)
library(lubridate)
library(cranlogs)
library(dplyr)
library(teal.reporter)
library(httr2)

options(repos = c(CRAN = "http://cran.rstudio.com/"))

app_ui <- function(request) {
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
              withSpinner(textOutput("name"))
            )
          ),
          fluidRow(
            box(
              class = "important-text",
              title = "Title",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              withSpinner(textOutput("title"))
            ),
            box(
              class = "important-text",
              title = "CRAN page",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              withSpinner(uiOutput("cran_url"))
            )
          ),
          fluidRow(
            box(
              class = "important-text",
              title = "Description",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              withSpinner(textOutput("description"))
            ),
          ),
          fluidRow(
            box(
              class = "important-text",
              title = "Package Last Version",
              width = 4,
              solidHeader = TRUE,
              status = "primary",
              withSpinner(textOutput("last_version"))
            ),
            box(
              class = "important-text",
              title = "Package License",
              width = 4,
              solidHeader = TRUE,
              status = "primary",
              withSpinner(textOutput("license"))
            ),
            box(
              class = "important-text",
              title = "Package Maintainer",
              width = 4,
              solidHeader = TRUE,
              status = "primary",
              withSpinner(uiOutput("maintainer"))
            )
          ),
          fluidRow(
            box(
              class = "important-text",
              title = "Publication",
              width = 4,
              solidHeader = TRUE,
              status = "primary",
              withSpinner(textOutput("publication"))
            ),
            box(
              class = "important-text",
              title = "URL",
              width = 4,
              solidHeader = TRUE,
              status = "primary",
              withSpinner(uiOutput("url"))
            ),
            box(
              class = "important-text",
              title = "BugReports",
              width = 4,
              solidHeader = TRUE,
              status = "primary",
              withSpinner(uiOutput("bugreports"))
            )
          ),
          fluidRow(
            box(
              class = "important-text",
              title = "Needs Compilation",
              width = 4,
              solidHeader = TRUE,
              status = "primary",
              withSpinner(textOutput("compilation"))
            ),
            box(
              class = "important-text",
              title = "System Requirements",
              width = 4,
              solidHeader = TRUE,
              status = "primary",
              withSpinner(textOutput("requirements"))
            ),
            box(
              class = "important-text",
              title = "Encoding",
              width = 4,
              solidHeader = TRUE,
              status = "primary",
              withSpinner(textOutput("encoding"))
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
              withSpinner(plotOutput("downloads_plot", height = "400px"))
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
              withSpinner(verbatimTextOutput("news_diff"))
            )
          ),
          fluidRow(
            box(
              style = "overflow: scroll !important;",
              title = "DESCRIPTION (Depends, Imports, LinkingTo) Deps Diff",
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
              withSpinner(DT::dataTableOutput("dep_version_diff"))
            ),
            box(
              title = "Namespace Diff",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              withSpinner(verbatimTextOutput("namespace_diff"))
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
              withSpinner(DT::dataTableOutput("cran_check_results"))
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
              withSpinner(uiOutput("description_deps"))
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
              withSpinner(textOutput("number_user_dependencies"))
            ),
            box(
              class = "important-text",
              title = "Number of DEVELOPER Dependencies",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              footer = "Depends, Imports, LinkingTo, Suggests - Recursive",
              withSpinner(textOutput("number_developer_dependencies"))
            )
          ),
          fluidRow(
            box(
              title = "Package USER Dependencies",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              withSpinner(DT::dataTableOutput("user_dependencies")),
              footer = "Depends, Imports, LinkingTo - Recursive"
            ),
            box(
              style = "overflow: scroll !important;",
              title = "USER Heavy Dependencies",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              footer = "Depends, Imports, LinkingTo",
              withSpinner(DT::dataTableOutput("user_heavy_dependencies"))
            )
          ),
          fluidRow(
            box(
              title = "Package DEVELOPER Dependencies",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              withSpinner(DT::dataTableOutput("developer_dependencies")),
              footer = "Depends, Imports, LinkingTo, Suggests - Recursive"
            ),
            box(
              style = "overflow: scroll !important;",
              title = "DEVELOPER Heavy Dependencies",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              footer = "Depends, Imports, LinkingTo, Suggests",
              withSpinner(DT::dataTableOutput("developer_heavy_dependencies"))
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
              withSpinner(textOutput("newest_release"))
            ),
            box(
              class = "important-text",
              title = "Oldest Release",
              width = 4,
              solidHeader = TRUE,
              status = "primary",
              withSpinner(textOutput("oldest_release"))
            ),
            box(
              class = "important-text",
              title = "Years on CRAN",
              width = 4,
              solidHeader = TRUE,
              status = "primary",
              withSpinner(textOutput("years_on_cran"))
            )
          ),
          fluidRow(
            box(
              class = "important-text",
              title = "Number of Releases",
              width = 3,
              solidHeader = TRUE,
              status = "primary",
              withSpinner(textOutput("number_releases"))
            ),
            box(
              class = "important-text",
              title = "Minimum Life Duration",
              width = 3,
              solidHeader = TRUE,
              status = "primary",
              withSpinner(textOutput("min_releases"))
            ),
            box(
              class = "important-text",
              title = "Mean Life Duration",
              width = 3,
              solidHeader = TRUE,
              status = "primary",
              withSpinner(textOutput("mean_releases"))
            ),
            box(
              class = "important-text",
              title = "Maximum Life Duration",
              width = 3,
              solidHeader = TRUE,
              status = "primary",
              withSpinner(textOutput("max_releases"))
            )
          ),
          fluidRow(
            box(
              style = "overflow: scroll !important;",
              title = "CRAN Releases",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              withSpinner(DT::dataTableOutput("releases_list"))
            )
          )
        ),

        tabItem(
          tabName = "about",
          fluidRow(
            div(
              class = "col-sm-8",
              style = "margin: 0px auto; float: none; padding:20px;",
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

app_server <- function(input, output, session) {
  pac <- eventReactive(input$pac,
    {
      validate(
        need(input$pac != "", "Please select a package name."),
        need(input$pac %in% available.packages(), sprintf("'%s' is not a valid CRAN package name", input$pac))
      )
      input$pac
    },
    ignoreNULL = TRUE
  )

  last_version <- reactive({
    req(pac())
    pacs::pac_last(pac())
  })

  get_timemachine <- reactive({
    req(pac())
    pacs::pac_timemachine(pac())
  })

  get_description <- reactive({
    pacs::pac_description(pac(), last_version())
  })

  observeEvent(input$pac, {
    req(get_timemachine())
    tags_pac <- get_timemachine()[["Version"]]
    updateSelectInput(session, "version_old", choices = tags_pac, selected = head(tail(tags_pac, 2), 1))
    updateSelectInput(session, "version_new", choices = tags_pac, selected = tail(tags_pac, 1))
  })

  ### Version Diff Tab

  pac_news <- reactive({
    req(pac())

    for (file in c("NEWS.md", "NEWS", "NEWS.Rmd")) {
      d_url <- sprintf("https://raw.githubusercontent.com/cran/%s/%s/%s", pac(), pacs::pac_last(pac()), file)
      response <- request(d_url) %>% req_perform()

      if (resp_status(response) == 200) {
        return(resp_body_string(response))
      }
    }
    validate(need(TRUE, "No NEWS file."))
  })

  output$news_diff <- renderText({
    req(isolate(pac_news()))
    req(input$version_old)
    req(input$version_new)
    validate(
      need(
        isTRUE(utils::compareVersion(input$version_new, input$version_old) == 1),
        "New version has to be higher."
      )
    )

    version_pattern <- function(version) {
      paste0("#.*", version)
    }
    old_version_pos <- str_locate(isolate(pac_news()), version_pattern(input$version_old))[1]
    new_version_pos <- str_locate(isolate(pac_news()), version_pattern(input$version_new))[1]

    validate(need(!(is.na(old_version_pos) || is.na(new_version_pos)), "Failed to get the NEWS diff."))

    news_excerpt <- str_sub(pac_news(), new_version_pos, old_version_pos - 1)
    news_excerpt
  })

  output$dep_version_diff <- DT::renderDataTable({
    req(isolate(pac()))
    req(input$version_old)
    req(input$version_new)
    req(input$diff_deps_select)
    validate(
      need(
        isTRUE(utils::compareVersion(input$version_new, input$version_old) == 1),
        "New version has to be higher."
      )
    )
    DT::datatable(
      pacs::pac_compare_versions(isolate(pac()), old = input$version_old, new = input$version_new, fields = input$diff_deps_select),
      options = list(
        paging = TRUE,
        pageLength = 100,
        columnDefs = list(
          list(
            targets = 2:3,
            render = JS(
             "function(data, type, row, meta) {",
             "return data === null ? 'NA' : data;",
             "}"
            )
          )
        )
      )
    )
  })

  output$namespace_diff <- renderPrint({
    req(isolate(pac()))
    req(input$version_old)
    req(input$version_new)
    validate(
      need(
        isTRUE(utils::compareVersion(input$version_new, input$version_old) == 1),
        "New version has to be higher."
      )
    )
    pacs::pac_compare_namespace(isolate(pac()), old = input$version_old, new = input$version_new)
  })

  ### Downloads Tab

  download_pacs <- reactive({
    req(pac())
    try(
      cranlogs::cran_downloads(
        c(pac(), strsplit(input$pac_compare, ", ?")[[1]]),
        from = input$date_input[1],
        to = input$date_input[2]
      ),
      silent = TRUE
    )
  })

  output$downloads_plot <- renderPlot({
    req(input$date_input)
    req(download_pacs)
    if (!inherits(download_pacs(), "try-error")) {
      gg <- ggplot2::ggplot(
        download_pacs() %>%
          dplyr::group_by(package, date = lubridate::floor_date(date, input$date_unit)) %>%
          dplyr::summarise(count = sum(count), .groups = "drop"),
        ggplot2::aes(x = as.Date(date), y = count, col = package)
      ) +
        ggplot2::geom_line(linewidth = 2) +
        ggplot2::scale_x_date() +
        ggplot2::scale_y_continuous(
          labels = scales::comma_format(
            big.mark = ".",
            decimal.mark = ","
          )
        ) +
        ggplot2::labs(
          title = sprintf("Downloads (%s) %s - %s", input$date_unit, input$date_input[1], input$date_input[2]),
          x = "Date"
        ) +
        ggplot2::theme(
          text = ggplot2::element_text(size = 16)
        )
      if (isTRUE(input$add_smooth)) {
        gg <- suppressWarnings(gg + ggplot2::geom_smooth())
      }
      print(gg)
    } else {
      NULL
    }
  })

  ### Checkpage Tab

  output$check_page <- renderUI({
    url <- sprintf("https://cran.r-project.org/web/checks/check_results_%s.html", pac())
    tags$a(href = url, url)
  })

  output$red_status <- renderUI({
    status <- pacs::pac_checkred(pac())
    tags$span(
      style = if (isTRUE(status)) "color:red;" else "color:green;",
      status
    )
  })

  output$cran_check_results <- DT::renderDataTable({
    req(pac())
    table <- pacs::pac_checkpage(pac())
    if (length(table) > 0) {
      DT::datatable(
        table,
        options = list(
          paging = TRUE,
          pageLength = 20
        )
      )
    } else {
      "No CRAN check results found."
    }
  })

  ### Release List Tab

  order_timemachine <- reactive({
    get_timemachine() %>%
      dplyr::select(Package, Version, Released, Archived, LifeDuration) %>%
      dplyr::arrange(dplyr::desc(Released))
  })

  output$releases_list <- DT::renderDataTable({
    req(order_timemachine())
    table <- order_timemachine()
    if (length(table) > 0) {
      DT::datatable(
        table,
        options = list(
          paging = TRUE,
          pageLength = 100
        )
      )
    } else {
      "No releases found."
    }
  })

  newest_row <- reactive(head(order_timemachine(), 1))
  output$newest_release <- renderText({
    newest_row()$Version
  })

  oldest_row <- reactive(tail(order_timemachine(), 1))
  output$oldest_release <- renderText({
    oldest_row()$Version
  })

  output$years_on_cran <- renderText({
    round((as.Date(newest_row()$Released) - as.Date(oldest_row()$Released)) / 365)
  })

  output$number_releases <- renderText({
    nrow(get_timemachine())
  })

  output$min_releases <- renderText({
    min(get_timemachine()$LifeDuration)
  })

  output$mean_releases <- renderText({
    round(mean(get_timemachine()$LifeDuration))
  })

  output$max_releases <- renderText({
    max(get_timemachine()$LifeDuration)
  })

  ### Deps Tab

  user_deps <- reactive({
    req(pac())
    pacs::pac_deps_user(pac(), local = FALSE)
  })

  output$number_user_dependencies <- renderText({
    nrow(user_deps())
  })

  output$user_dependencies <- DT::renderDataTable({
    DT::datatable(
      user_deps(),
      options = list(
        paging = TRUE,
        pageLength = 100
      )
    )
  })

  dev_deps <- reactive({
    req(pac())
    pacs::pac_deps_dev(pac(), local = FALSE)
  })

  output$number_developer_dependencies <- renderText({
    nrow(dev_deps())
  })

  output$developer_dependencies <- DT::renderDataTable({
    DT::datatable(
      dev_deps(),
      options = list(
        paging = TRUE,
        pageLength = 100
      )
    )
  })

  output$developer_heavy_dependencies <-  DT::renderDataTable({
    tab <-  pacs::pac_deps_heavy(
      pac(),
      fields = c("Depends", "Imports", "LinkingTo", "Suggests")
    ) %>% arrange(desc(NrUniqueDeps))

    DT::datatable(
      tab,
      options = list(
        paging = TRUE,
        pageLength = 100
      )
    )
  })

  output$user_heavy_dependencies <-  DT::renderDataTable({
    tab <- pacs::pac_deps_heavy(
      pac(),
      fields = c("Depends", "Imports", "LinkingTo")
    ) %>%
      arrange(desc(NrUniqueDeps))

    DT::datatable(
      tab,
      options = list(
        paging = TRUE,
        pageLength = 100
      )
    )
  })

  output$description_deps <- renderUI({
    result <- list(
      Depends = get_description()[["Depends"]],
      Imports = get_description()[["Imports"]],
      LinkingTo = get_description()[["LinkingTo"]],
      Suggests = get_description()[["Suggests"]],
      Enhances = get_description()[["Enhances"]]
    )
    tags$table(
      class = "table table-striped",
      tags$thead(
        tags$tr(
          tags$th(""),
          tags$th(paste("Version:", result[["Version"]])),
        )
      ),
      tags$tbody(
        tagList(
          lapply(names(result), function(e) {
            tags$tr(
              tags$th(e),
              tags$td(result[[e]])
            )
          })
        )
      )
    )
  })

  ### Info Tab

  output$name <- renderText({
    pac()
  })

  output$title <- renderText({
    get_description()[["Title"]]
  })

  output$description <- renderText({
    get_description()[["Description"]]
  })

  output$last_version <- renderText({
    last_version()
  })

  output$license <- renderText({
    get_description()[["License"]]
  })

  output$maintainer <- renderUI({
    tags$a(href = paste0("mailto:", get_description()[["Maintainer"]]), get_description()[["Maintainer"]])
  })

  output$bugreports <- renderUI({
    if (length(get_description()[["BugReports"]])) {
      tagList(lapply(strsplit(get_description()[["BugReports"]], ",")[[1]], function(e) tags$a(href = e, e)))
    } else {
      NULL
    }
  })

  output$publication <- renderText({
    res <- get_description()[["Date/Publication"]]
    res_date <- lubridate::as_date(res)
    sprintf("%s (%s days ago)", res, as.numeric(Sys.Date() - res_date))
  })

  output$url <- renderUI({
    if (length(get_description()[["URL"]])) {
      tagList(lapply(strsplit(get_description()[["URL"]], ",")[[1]], function(e) tags$a(href = e, e)))
    } else {
      NULL
    }
  })

  output$cran_url <- renderUI({
    url <- sprintf("https://CRAN.R-project.org/package=%s", pac())
    tags$a(href = url, url)
  })

  output$encoding <- renderText({
    get_description()[["Encoding"]]
  })

  output$compilation <- renderText({
    get_description()[["NeedsCompilation"]]
  })

  output$requirements <- renderText({
    reqs <- get_description()[["SystemRequirements"]]
    rdep <- stringr::str_extract(get_description()[["Depends"]], "R \\(.*\\)")
    if (is.na(rdep)) rdep <- NULL
    paste(rdep, reqs)
  })
}

shiny::shinyApp(app_ui, app_server)
