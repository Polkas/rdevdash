globalVariables(c('.' , 'Archived', 'JS', 'LifeDuration', 'NrUniqueDeps', 'Package', 'Released', 'Version', 'count', 'package'))

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
#'   \item{\strong{Package Validation}}{Validates the selected package name and checks if it is a valid CRAN package.}
#'   \item{\strong{Version Handling}}{Retrieves package versions and updates version selection inputs in the UI.}
#'   \item{\strong{NEWS Diff Tab}}{Fetches the NEWS file for the package and displays the differences between versions.}
#'   \item{\strong{Downloads Tab}}{Plots the download statistics of the package over time.}
#'   \item{\strong{Checkpage Tab}}{Displays CRAN check results and package status.}
#'   \item{\strong{Release List Tab}}{Shows the list of package releases with details.}
#'   \item{\strong{Dependencies Tab}}{Displays user and developer dependencies of the package.}
#'   \item{\strong{Info Tab}}{Provides package information such as title, description, version, license, etc.}
#' }
#' @export
app_server <- function(input, output, session) {
  pac <- eventReactive(input$pac,
                       {
                         validate(
                           need(input$pac != "", "Please select a package name."),
                           need(input$pac %in% utils::available.packages(), sprintf("'%s' is not a valid CRAN package name", input$pac))
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
    updateSelectInput(session, "version_old", choices = tags_pac, selected = utils::head(utils::tail(tags_pac, 2), 1))
    updateSelectInput(session, "version_new", choices = tags_pac, selected = utils::tail(tags_pac, 1))
  })

  ### Version Diff Tab

  pac_news <- reactive({
    req(pac())

    for (file in c("NEWS.md", "NEWS", "NEWS.Rmd")) {
      d_url <- sprintf("https://raw.githubusercontent.com/cran/%s/%s/%s", pac(), pacs::pac_last(pac()), file)
      response <- httr2::request(d_url) %>% httr2::req_perform(req = .)

      if (httr2::resp_status(response) == 200) {
        return(httr2::resp_body_string(response))
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
    old_version_pos <- stringr::str_locate(isolate(pac_news()), version_pattern(input$version_old))[1]
    new_version_pos <- stringr::str_locate(isolate(pac_news()), version_pattern(input$version_new))[1]

    validate(need(!(is.na(old_version_pos) || is.na(new_version_pos)), "Failed to get the NEWS diff."))

    news_excerpt <- stringr::str_sub(pac_news(), new_version_pos, old_version_pos - 1)
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

  newest_row <- reactive(utils::head(order_timemachine(), 1))
  output$newest_release <- renderText({
    newest_row()$Version
  })

  oldest_row <- reactive(utils::tail(order_timemachine(), 1))
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
    ) %>% dplyr::arrange(dplyr::desc(NrUniqueDeps))

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
      dplyr::arrange(dplyr::desc(NrUniqueDeps))

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
    if (length(rdep) == 0 || is.na(rdep)) rdep <- NULL
    paste(rdep, reqs)
  })
}
