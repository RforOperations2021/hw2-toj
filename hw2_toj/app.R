
#loading relevant libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)


ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody()
)

server <- function(input, output) { }

shinyApp(ui, server)