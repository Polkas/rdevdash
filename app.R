options(repos = c(CRAN = "https://cloud.r-project.org"))

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

shiny::shinyApp(rdevdash::app_ui, rdevdash::app_server)
