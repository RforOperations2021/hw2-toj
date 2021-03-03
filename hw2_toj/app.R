
#loading relevant libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)



#Loading in the relevant datasets ---------------------------------

#Covid Vaccination data by state
covid.vac <- read.csv("covidvaccines.csv", header = TRUE)

#Pfizer Vaccine Allocation data
pf.alloc <- read.csv("pfizervaccineallocation.csv") 

#Editing the column names of the pf.alloc dataset
colnames(pf.alloc) <- c("Jurisdiction", "Week.of.Allocations", "First.Dose.Allocations", "Second.Dose.Allocations")


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
                      choices = pf.alloc$Jurisdiction),
          
          
          #For the supplied state, display the % of vaccines delivered that are used
  
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("allocation")
            #valueBoxOutput()
           ),
          
        dataTableOutput(outputId = "state_table")
          
          
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
  
  #Create a subset of the data filtering for state ---------------------------
  state_alloc <- reactive({
    req(input$state)
    filter(pf.alloc, Jurisdiction %in% input$state)
  })
  
   output$allocation <-
    renderInfoBox({
      
      infoBox("Amount of Vaccine Allocated", value = nrow(state_alloc()))
    })
   
   #Display data table of vaccines allocation for the selected state-------------------------------------------
   output$state_table <- DT::renderDataTable({
     DT::datatable(data = state_alloc(),
                   rownames = FALSE)
   })
  }

shinyApp(ui, server)