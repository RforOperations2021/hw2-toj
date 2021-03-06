
#loading relevant libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(scales) #helps to format the numbers in the value/info boxes

#gets rid of scientific notation
options(scipen = 10L)


#Data Loading and Pre-Proocessing------------------------------------------

#Covid Vaccination data by state
covid.vac <- read.csv("covidvaccines.csv", header = TRUE)

#Pfizer Vaccine Allocation data
pf.alloc <- read.csv("pfizervaccineallocation.csv") 

#Editing the column names of the pf.alloc dataset
colnames(pf.alloc) <- c("Jurisdiction", "Week.of.Allocations", 
                        "First.Dose.Allocations", "Second.Dose.Allocations")

#using lubridate to change the date values in the pf.alloc dataset
pf.alloc <- pf.alloc %>% 
  mutate(Week.of.Allocations = lubridate::mdy(Week.of.Allocations))


#Steps for creating a normalized delivery amount column

#step 1: create a variable for doses delivered per 100k
num.del <- covid.vac$Doses.Delivered.per.100K

#step 2: calculate the mean of doses delivered per 100k
del.mean <- mean(covid.vac$Doses.Delivered.per.100K, na.rm = TRUE)

#step 3: calculate the standard deviation of doses delivered per 100k
del.sd <- sd(covid.vac$Doses.Delivered.per.100K, na.rm = TRUE)

#step 4: normalize the data based on previous steps
covid.vac$del_norm <- round(((num.del-del.mean)/del.sd), 2)

#step 5: assigning data to categories, based on normalized value
covid.vac$del_scale <- ifelse(covid.vac$del_norm < 0, "Below Average", "Above Average")

#step 6: sorting the data according to the "del_scale" variable
covid.vac <- covid.vac[order(covid.vac$del_scale),]

#step 7: maintain the order of the dataset by creating a new factor variable
covid.vac$State.Territory.Federal.Entity <- factor(covid.vac$State.Territory.Federal.Entity,
                                                   levels = covid.vac$State.Territory.Federal.Entity)




# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "COVID-19 Vaccine Distribution Tracker",
                          titleWidth = 350
    )
  
# Dashboard Sidebar ---------------------------------------------------
sidebar <-   dashboardSidebar(
        sidebarMenu(
        id = "tabs",
  
             # Menu Items ----------------------------------------------
              menuItem("Overview of Vaccine Allocation", icon = icon("viruses"), tabName = "overview"),
              menuItem("Vaccine Delivery", icon = icon("truck"), tabName = "delivery"),
              menuItem("Vaccine Administration ", icon = icon("syringe"), tabName = "admin")
        )
)

# Dashboard body ----------------------------------------------
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
          
          #displays the amount of vaccine allocated to the selected state
          fluidRow(
            infoBoxOutput("allocation")
           ),
          
          #creates a tabset panel to display the data table and the corresponding time series chart
          tabsetPanel(type = "tabs",
                      tabPanel("Data Table", dataTableOutput(outputId = "state_table")),
                      tabPanel("Vaccine Allocation Over Time",  plotlyOutput(outputId = "time.series"))
                      ),
          
        ),
  
  # Vaccine Delivery Page ----------------------------------------------
  tabItem("delivery",
          
          #selects the state to find information about (to be used to filter the data)
          selectInput(inputId = "state_del",
                      label = "Select a state or territory:",
                      choices = covid.vac$State.Territory.Federal.Entity),
          
          #displays the total amount of vaccine delivery for selected state
          infoBoxOutput("tot_del"),
          
          #creates space between the info box and the rest of the vaccine delivery page
          br(), br(), br(), br(), br(),
    
          #allows the user to select multiple states to select for the normalized delivery plot
          selectInput("state_del_compare",
                      "Pick A Few States to Compare:",
                      choices = covid.vac$State.Territory.Federal.Entity,
                      multiple = TRUE,
                      selectize = TRUE,
                      selected = c("Maryland", "Virginia", "Hawaii", "Connecticut", "Montana")),
         
                
          #creates the normalized delivery plot
          plotlyOutput("deliveryPlot")
          

  ),
  

  
  # Vaccine Administration Page ----------------------------------------------
  tabItem("admin",
          
          #creates info box on % of vaccines administered that were delivered for first state selected
          infoBoxOutput("per_admin_state_1"),
          
          #creates info box on % of vaccines administered that were delivered for second state selected
          infoBoxOutput("per_admin_state_2"),
          
          #select two states to compare
          selectInput("state_select",
                      "Pick 2 States:",
                      choices = covid.vac$State.Territory.Federal.Entity,
                      multiple = TRUE,
                      selectize = TRUE,
                      selected = c("Maryland", "Virginia")),
          
          #graph of vaccines administered 
          plotlyOutput("admin_dist"),
          
          
          #compares the total amount of vaccine distributed for states selected
          dataTableOutput("state_compare")
     
    
     
    
  )
  
  )
)


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) { 
  
  #creates a subset of the vaccine allocation data filtered for a state ---------------------------
  state_alloc <- reactive({
    req(input$state)
    filter(pf.alloc, Jurisdiction %in% input$state)
  })
  
  #Creates a subset of the delivery and administration data by state------------
  state_del_admin <- reactive({
    req(input$state_del)
    filter(covid.vac, State.Territory.Federal.Entity %in% input$state_del)
  })
  
  
  #creates data subset for comparing the number of vaccines delivered in several states-----
  state_del_comparison <- reactive({
    req(input$state_del_compare)
  
    filter(covid.vac, State.Territory.Federal.Entity %in% input$state_del_compare)
  })
  
  
  #creates separate dataset that summarizes data for 2 states----------------
  state_compare <- reactive({
    
    #changes some of the column names for the covid.vac dataset
    covid.vac <- covid.vac %>% 
      rename("State.Territory" = "State.Territory.Federal.Entity",
             "Total.Doses" = "Total.Doses.Administered.by.State.where.Administered",
             "Num.First.Dose" = "People.with.1..Doses.by.State.of.Residence",
             "Num.Second.Dose" = "People.with.2.Doses.by.State.of.Residence")
    
    filter(covid.vac, State.Territory%in% input$state_select)
    
    
  })
  
  
  #displays welcome message on the overview page-----------------------------
  output$welcome <- renderUI ({ HTML("Welcome to the COVID-19 Vaccine Distribution Tracker! 
                                     <br/> <br/> There are three different tabs that you can look at:
                                     Vaccine Allocation data (for the Pfizer Vaccine), Vaccine Delivery data, and
                                      Vaccine Administration data.
                                      <br/> <br/> Click on any tab to get started!")
                                      
                                })
  
  #creates infoBox for allocation data------------------------------------------
  output$allocation <- renderInfoBox({
      
      #sums the total number of vaccines received to-date by a jurisdiction
      alloc.sum <- sum(state_alloc()$First.Dose.Allocations)
      
      #changes the formatting of the value in the info box
      alloc.sum <- prettyNum(alloc.sum, big.mark = ",")

      infoBox(HTML(paste("Total Number of Vaccines", br(), "Allocated To-Date for", input$state)),
                      value = alloc.sum, color = "green")
    })
   
   #displays data table of vaccines allocation for the selected state-------------------------------------------
   output$state_table <- DT::renderDataTable({
     DT::datatable(data = state_alloc(),
                   rownames = FALSE)
   })
   
   
   #displays a time series plot for the vaccine allocation over time---------------
   output$time.series <- renderPlotly({
     

     #creates the time series plot----------------------------------
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
   
   #creates a diverging bar chart based on normalized delivery data-------------- 
   output$deliveryPlot <- renderPlotly({
    
      del_plot <- ggplotly(

       ggplot(state_del_comparison(), aes(x = State.Territory.Federal.Entity,
                                          y = del_norm)) +
         geom_bar(stat = "identity", aes(fill=del_scale), width = .5) +
         scale_fill_manual(labels = c("Above Average", "Below Average"),
                          values = c("Above Average"="#00ba38", "Below Average"="#f8766d")) +
         labs(title = "Are the selected states receiving an above or below average amount of vaccine deliveries?") +
         xlab("State/Territory") +
         ylab("Normalized Amount of Deliveries") + 
          coord_flip()
         
        ) 
      
      #changes the name of the legend
      del_plot %>% 
        layout(legend = list(title= list(text= "<b> Delivery Amounts <b>")))

    
   })
   
 
    
    #displays the total number of vaccines delivered for the state selected-----------
     output$tot_del <- renderInfoBox({
       
       tot_del <- state_del_admin()$Total.Doses.Delivered
       tot_del <- prettyNum(tot_del, big.mark = ",")
       infoBox(HTML(paste("Total Number of Vaccines", br(), "Delivered To-Date in", input$state_del)),
                          value = tot_del)
     
      
        })
 
   #displays the % of delivered vaccines that were administered for state 1 --------
   output$per_admin_state_1 <- renderInfoBox({
     
     num_del <- sum(state_compare()[1,]$Total.Doses.Delivered)
     num_admin <- sum(state_compare()[1,]$Total.Doses)
     per_admin <- round(num_admin/num_del, digits = 2)
     per_admin <- scales::percent(per_admin) 
     
     infoBox(HTML(paste("Percent of Delivered Vaccines", br(), "Administered in", input$state_select[1])),
             value = per_admin, icon = icon("flag-checkered"), color = "orange")
    
   })
   
   #displays the % of delivered vaccines that were administered for state 2 --------
   output$per_admin_state_2 <- renderInfoBox({
     
     num_del <- sum(state_compare()[2,]$Total.Doses.Delivered)
     num_admin <- sum(state_compare()[2,]$Total.Doses)
     per_admin <- round(num_admin/num_del, digits = 2)
     per_admin <- scales::percent(per_admin)
     
     infoBox(HTML(paste("Percent of Delivered Vaccines", br(), "Administered in", input$state_select[2])),
             value = per_admin, icon = icon("flag-checkered"), color = "purple")
     
   })
   
   
   #displays a data table comparing two states vaccine administration info-------  
   output$state_compare<- DT::renderDataTable({
    
     DT::datatable(data = state_compare() %>% 
                          select(State.Territory,
                                 Total.Doses,
                                 Num.First.Dose,
                                 Num.Second.Dose),
                   rownames = FALSE)
   })
   
   #creates plot of vaccine administration information for two states-----------
   output$admin_dist <- renderPlotly({

     #filters for total administration data for the first state selected
     num_admin_state_1 <- state_compare() %>%
       filter(State.Territory == input$state_select[1]) %>%
       select(Total.Doses)
      
     #filters for total administration data for the second state selected
     num_admin_state_2 <- state_compare() %>%
       filter(State.Territory == input$state_select[2]) %>%
       select(Total.Doses)

    #creates vector of two states' vaccine administration data
     num_admin_compare <- c(num_admin_state_1, num_admin_state_2)
    
     #creates a factor variable to fix the order states display on the x-axis
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