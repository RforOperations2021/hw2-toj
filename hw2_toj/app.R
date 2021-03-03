
#loading relevant libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)



#Loading in the relevant datasets ---------------------------------

#Covid Vaccination data by state
covid.vac <- read.csv("covidvaccines.csv", header = TRUE)

#Pfizer Vaccine Allocation data
pf.alloc <- read.csv("pfizervaccineallocation.csv")


header <- dashboardHeader(title = "COVID-19 Vaccine Distribution Tracker"
  
      
    
      
    )
  
sidebar <-   dashboardSidebar(
        sidebarMenu(
        id = "tabs",
  
             # Menu Items ----------------------------------------------
              menuItem("Overview of Vaccine Allocation", icon = icon("viruses"), tabName = "overview"),
              menuItem("Vaccine Delivery", icon = icon("truck"), tabName = "delivery"),
              menuItem("Vaccine Administration ", icon = icon("syringe"), tabName = "admin")
        )
)


body <- dashboardBody(tabItems(
  
  # Plot page ----------------------------------------------
  tabItem("overview",
      
          #For the supplied state, display the % of vaccines delivered that are used
  
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("Percent of Vaccine Used")
            #valueBoxOutput()
          )
        )
  )
)


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) { 
  
  }

shinyApp(ui, server)