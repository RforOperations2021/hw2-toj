
#loading relevant libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(scales) #helps to format the numbers in the value/info boxes




#Loading in the relevant datasets ---------------------------------

#Covid Vaccination data by state
covid.vac <- read.csv("covidvaccines.csv", header = TRUE)

#Pfizer Vaccine Allocation data
pf.alloc <- read.csv("pfizervaccineallocation.csv") 

#Editing the column names of the pf.alloc dataset
colnames(pf.alloc) <- c("Jurisdiction", "Week.of.Allocations", "First.Dose.Allocations", "Second.Dose.Allocations")

#using lubridate to change the date values for the week of allocation
pf.alloc <- pf.alloc %>% 
  mutate(Week.of.Allocations = lubridate::mdy(Week.of.Allocations))


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
           ),
          
          #creating a tabset panel to display the data table and the corresponding time series chart
          tabsetPanel(type = "tabs",
                      tabPanel("Data Table", dataTableOutput(outputId = "state_table")),
                      
                      #timeseries plot of vaccination allocation over time
                      tabPanel("Vaccine Allocation Over Time",  plotlyOutput(outputId = "time.series"))
                      ),
          
        ),
  
  # Vaccine Delivery Page ----------------------------------------------
  tabItem("delivery",
          
          #Selects the delivery metrics the user would want to see
          radioButtons(inputId = "del.met",
                       label = "Pick a Delivery Metric:",
                       choices = c("Total Number Delivered", "Doses Delivered by 100k")
          ),
          
    infoBoxOutput("del"),
    infoBoxOutput("alloc_del")
  ),
  
  
  
  
  # Vaccine Administration Page ----------------------------------------------
  tabItem("admin",
          
     infoBoxOutput("per_admin")
    
  )
  
  )
)


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) { 
  
  #Create a subset of the data filtering for state ---------------------------
  state_alloc <- reactive({
    req(input$state)
    filter(pf.alloc, Jurisdiction %in% input$state)
  })
  
  #Creates a subset of the delivery and administraion data by state----------
  state_del_admin <- reactive({
    req(input$state)
    filter(covid.vac, State.Territory.Federal.Entity %in% input$state)
  })
  
   output$allocation <-
    renderInfoBox({
      
      #sums the total number of vaccines received to-date by a jurisdiction
      alloc.sum <- sum(state_alloc()$First.Dose.Allocations)

      infoBox("Total Amount of Vaccine Allocated To-Date", value = alloc.sum)
    })
   
   #Display data table of vaccines allocation for the selected state-------------------------------------------
   output$state_table <- DT::renderDataTable({
     DT::datatable(data = state_alloc(),
                   rownames = FALSE)
   })
   
   output$time.series <- renderPlotly({
     
     
     #create the time series plot
     ggplot(state_alloc(), aes(x=Week.of.Allocations, y=First.Dose.Allocations)) +
      geom_point(color = "blue") +
      geom_line(group = 1) +
      xlab("Month of Allocation") +
      ylab("Number of First Dose Allocations")
     
   })
   
   output$del <- renderInfoBox({
     infoBox("Number of Vaccines Delivered To-Date")
   })
   
   output$alloc_del <- renderInfoBox({
     infoBox("Percent Allocation Delivered")
   })
   
   output$per_admin <- renderInfoBox({
     num_del <- sum(state_del_admin()$Total.Doses.Delivered)
     num_admin <- sum(state_del_admin()$Total.Doses.Administered.by.State.where.Administered)
     per_admin <- round(num_admin/num_del * 100, digits = 2)
     infoBox("Percent of Delivered Vaccines Administered", value = per_admin)
   })
}


shinyApp(ui, server)