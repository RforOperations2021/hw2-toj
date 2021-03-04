
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


header <- dashboardHeader(title = "COVID-19 Vaccine Distribution Tracker",
                          titleWidth = 350

    
      
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
                      label = "Select a state:",
                      choices = pf.alloc$Jurisdiction),
          
         
          #only displays time series data for the date range requested
          dateRangeInput("dates_alloc", label = "Select a Date Range for Time Series Plot:",
                         start = "2020-12-14",
                         end = "2021-03-01",
                         format = "yyyy-mm-dd",
                         separator = "-"),
          
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
      
          selectInput("state_select",
                      "Pick 2 States:",
                      choices = covid.vac$State.Territory.Federal.Entity,
                      multiple = TRUE,
                      selectize = TRUE,
                      selected = c("Maryland", "Virginia")),
          
          plotlyOutput("admin_dist"),
          
     #infoBoxOutput("per_admin"),
     
     dataTableOutput("state_compare")
     
    
     
    
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
  
  #Creates a subset of the delivery and administration data by state----------
  state_del_admin <- reactive({
    req(input$state)
    filter(covid.vac, State.Territory.Federal.Entity %in% input$state)
  })
  
   output$allocation <-
    renderInfoBox({
      
      #sums the total number of vaccines received to-date by a jurisdiction
      alloc.sum <- sum(state_alloc()$First.Dose.Allocations)

      infoBox("Total Amount of Vaccine Allocated To-Date", value = alloc.sum, color = )
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
      ylab("Number of First Dose Allocations") +
      xlim(input$dates_alloc[1], input$dates_alloc[2]) +
      ylim(0, max(state_alloc()$First.Dose.Allocations))
     
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
     infoBox("Percent of Delivered Vaccines Administered", value = per_admin, color = "orange")
   })
   
   
   #Used to create a separate dataset that summarizes data for 2 states
   state_compare <- reactive({
     filter(covid.vac, State.Territory.Federal.Entity %in% input$state_select)
   })
     
   
   output$state_compare<- DT::renderDataTable({
     
     #editing the column names
     
     DT::datatable(data = state_compare() %>% 
                          select(State.Territory.Federal.Entity,
                                Total.Doses.Administered.by.State.where.Administered,
                                 Doses.Administered.per.100k.by.State.where.Administered,
                                 X18..Doses.Administered.by.State.where.Administered,
                                 People.with.1..Doses.by.State.of.Residence,
                                 People.with.2.Doses.by.State.of.Residence),
                   rownames = FALSE)
   })
   
   output$admin_dist <- renderPlotly({

     #filters for total administration data for the first state selected
     num_admin_state_1 <- state_compare() %>%
       filter(State.Territory.Federal.Entity == input$state_select[1]) %>%
       select(Total.Doses.Administered.by.State.where.Administered)

     num_admin_state_2 <- state_compare() %>%
       filter(State.Territory.Federal.Entity == input$state_select[2]) %>%
       select(Total.Doses.Administered.by.State.where.Administered)

     num_admin_compare <- c(num_admin_state_1, num_admin_state_2)

     ggplot(state_compare(), aes(x = State.Territory.Federal.Entity,
                                 y = num_admin_compare, 
                                 fill = State.Territory.Federal.Entity)) +
     geom_bar(stat = "identity")  +
     xlab("States") +
     ylab("Number of Vaccines Administered") +
       
    #removes the legend from the plot
     theme(legend.position = "none")
       


   })
}


shinyApp(ui, server)