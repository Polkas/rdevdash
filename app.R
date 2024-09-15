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
library(httr2)
library(devtools)

options(repos = c(CRAN = "http://cran.rstudio.com/"))

devtools::load_all()

shiny::shinyApp(app_ui, app_server)
