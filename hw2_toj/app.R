
#loading relevant libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(scales) #helps to format the numbers in the value/info boxes
#getting rid of scientific notation
options(scipen = 10L)



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
      
          #Adds some introductory text to the overview page
          htmlOutput("welcome"),
          
          #creating some space between the welcome message and the select input
          br(), br(),
          
          
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
          
         
          #Selects the state to find information about (to be used to filter the data)
          selectInput(inputId = "state_del",
                      label = "Select a state or territory:",
                      choices = covid.vac$State.Territory.Federal.Entity),
          
          infoBoxOutput("tot_del"),
          
          
          #plotlyOutput("deliveryPlot")
          
          # dataTableOutput("del_table")
         
  ),
  
  
  
  
  # Vaccine Administration Page ----------------------------------------------
  tabItem("admin",
      
          infoBoxOutput("per_admin_state_1"),
          
          infoBoxOutput("per_admin_state_2"),
          
          selectInput("state_select",
                      "Pick 2 States:",
                      choices = covid.vac$State.Territory.Federal.Entity,
                      multiple = TRUE,
                      selectize = TRUE,
                      selected = c("Maryland", "Virginia")),
          
          plotlyOutput("admin_dist"),
          
         
     
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
    req(input$state_del)
    filter(covid.vac, State.Territory.Federal.Entity %in% input$state_del)
  })
  
  
  #Used to create a separate dataset that summarizes data for 2 states
  state_compare <- reactive({
    
    #changing some of the column names for the covid.vac dataset
    covid.vac <- covid.vac %>% 
      rename("State.Territory" = "State.Territory.Federal.Entity",
             "Total.Doses" = "Total.Doses.Administered.by.State.where.Administered",
             "Num.First.Dose" = "People.with.1..Doses.by.State.of.Residence",
             "Num.Second.Dose" = "People.with.2.Doses.by.State.of.Residence")
    
    filter(covid.vac, State.Territory%in% input$state_select)
    
    
  })
  
  

  output$welcome <- renderUI ({ HTML("Welcome to the COVID-19 Vaccine Distribution Tracker! 
                                     <br/> <br/> There are three different tabs that you can look at:
                                     Vaccine Allocation data (for the Pfizer Vaccine), Vaccine Delivery data, and
                                      Vaccine Administration data.
                                      <br/> <br/> Click on any tab to get started!")
                                      
                                      
                                })
  
  
   output$allocation <-
    renderInfoBox({
      
      #sums the total number of vaccines received to-date by a jurisdiction
      alloc.sum <- sum(state_alloc()$First.Dose.Allocations)
      
      #changing the formatting of the value in the info box
      alloc.sum <- prettyNum(alloc.sum, big.mark = ",")

      infoBox("Total Amount of Vaccine Allocated To-Date", value = alloc.sum, color = )
    })
   
   #Display data table of vaccines allocation for the selected state-------------------------------------------
   output$state_table <- DT::renderDataTable({
     DT::datatable(data = state_alloc(),
                   rownames = FALSE)
   })
   
   output$time.series <- renderPlotly({
     

     #create the time series plot
     ggplotly(
      ggplot(state_alloc(), aes(x=Week.of.Allocations, y=First.Dose.Allocations)) +
        geom_point(color = "blue") +
        geom_line(group = 1) +
        xlab("Month of Allocation") +
        ylab("Number of First Dose Allocations") +
        xlim(input$dates_alloc[1], input$dates_alloc[2]) +
        ylim(0, max(state_alloc()$First.Dose.Allocations))
      )
     
   })
   
   
   output$deliveryPlot <- renderPlotly({
    
     #mean number of total deliveries
     del.mean <- mean(covid.vac$Total.Doses.Delivered)
     
     ggplotly(
       ggplot(covid.vac, aes(x = Total.Doses.Delivered, 
                             fill = State.Territory.Federal.Entity)) +
                              
         geom_histogram()
       
     )
   })
   
   output$del_table<- DT::renderDataTable({
     
     DT::datatable(data = state_del_admin() %>% 
                     select(State.Territory.Federal.Entity,
                            Total.Doses.Delivered,
                            Doses.Delivered.per.100K),
                   rownames = FALSE) 
   })
    
    #info box that delays the total number of vaccines delivered for the state selected
     output$tot_del <- renderInfoBox({
       
       tot_del <- state_del_admin()$Total.Doses.Delivered
       tot_del <- prettyNum(tot_del, big.mark = ",")
       infoBox("Total Number of Vaccines Delivered To-Date", value = tot_del)
      })
 
   
   output$per_admin_state_1 <- renderInfoBox({
     
     num_del <- sum(state_compare()[1,]$Total.Doses.Delivered)
     num_admin <- sum(state_compare()[1,]$Total.Doses)
     per_admin <- round(num_admin/num_del, digits = 2)
     per_admin <- scales::percent(per_admin) 
     
     infoBox(HTML(paste("Percent of Delivered Vaccines", br(), "Administered in", input$state_select[1])),
             value = per_admin, icon = icon("flag-checkered"), color = "orange")
    
   })
   
   output$per_admin_state_2 <- renderInfoBox({
     
     num_del <- sum(state_compare()[2,]$Total.Doses.Delivered)
     num_admin <- sum(state_compare()[2,]$Total.Doses)
     per_admin <- round(num_admin/num_del, digits = 2)
     per_admin <- scales::percent(per_admin)
     
     infoBox(HTML(paste("Percent of Delivered Vaccines", br(), "Administered in", input$state_select[2])),
             value = per_admin, icon = icon("flag-checkered"), color = "purple")
     
   })
   
   
     
   output$state_compare<- DT::renderDataTable({
    
     DT::datatable(data = state_compare() %>% 
                          select(State.Territory,
                                 Total.Doses,
                                 Num.First.Dose,
                                 Num.Second.Dose),
                   rownames = FALSE)
   })
   
   output$admin_dist <- renderPlotly({

     #filters for total administration data for the first state selected
     num_admin_state_1 <- state_compare() %>%
       filter(State.Territory == input$state_select[1]) %>%
       select(Total.Doses)

     num_admin_state_2 <- state_compare() %>%
       filter(State.Territory == input$state_select[2]) %>%
       select(Total.Doses)

     num_admin_compare <- c(num_admin_state_1, num_admin_state_2)
    
     #creating a factor variable to fix the order states display on the x-axis
     state_order <- factor(state_compare()$State.Territory, 
                           level = c(input$state_select[1], input$state_select[2]))
     
     ggplotly(
       ggplot(state_compare(), aes(x = state_order,
                                 y = num_admin_compare,
                                 fill = State.Territory)) +
     geom_bar(stat = "identity")  +
     xlab("States") +
     ylab("Number of Vaccines Administered") +
       
    #removes the legend from the plot
     theme(legend.position = "none")
            )
       


   })
}


shinyApp(ui, server)