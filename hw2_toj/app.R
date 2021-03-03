
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
  
  # Overview Page ----------------------------------------------
  tabItem("overview",
      
          #Selects the state to find information about (to be used to filter the data)
          selectInput(inputId = "state",
                      label = "Select a state",
                      choices = covid.vac$State.Territory.Federal.Entity),
          
          
          #For the supplied state, display the % of vaccines delivered that are used
  
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("allocation")
            #valueBoxOutput()
          )
        ),
  
  # Vaccine Delivery Page ----------------------------------------------
  tabItem("delivery",
          
          #Selects the delivery metrics the user would want to see
          radioButtons(inputId = "del.met",
                       label = "Pick a Delivery Metric:",
                       choices = c("Total Number Delivered", "Doses Delivered by 100k")
          )
          
    
  )
  
  
  
  
  # Vaccine Administration Page ----------------------------------------------

  
  )
)


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) { 
  
  
   output$allocation <-
    renderInfoBox({
      infoBox("Amount of Vaccine Allocated", value = 9000)
    })
  }

shinyApp(ui, server)