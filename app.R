library(shiny)
library(ggplot2)
library(plotly)
library(tidyr)

r_files <- list.files("R", full.names = TRUE)
lapply(r_files, source)


ui <- navbarPage("COVID-19 Hospital Impact",
                 
                 tabPanel("SIR Model",
                          
                          sidebarLayout(
                              sidebarPanel(
                                           
                                           fluidRow(
                                               
                                               
                                               column(7,
                                                      numericInput(inputId = "susceptible",
                                                                   label = "Initial Susceptible",
                                                                   value = 3599733, min = 0),
                                                      numericInput(inputId = "infected", 
                                                                   label = "Initial Infected",
                                                                   value = 266, min = 0),
                                                      numericInput(inputId = "recovered", 
                                                                   label = "Initial Recovered",
                                                                   value = 0, min = 0)
                                               ),
                                               
                                               column(5,
                                                      numericInput(inputId = "mean_recovery", 
                                                                   label = "Mean Recovery",
                                                                   value = 14, min = 0),
                                                      numericInput(inputId = "doubling_time", 
                                                                   label = "Doubling Time",
                                                                   value = 4, min = 0)
                                               )
                                               
                                           )
                                           
                              ),
                              
                              mainPanel(
                                  
                                  
                                  fluidRow(
                                      column(3,
                                             dateInput(inputId = "start_date", label = "Start Date",
                                                       "2020-03-05")
                                             ),
                                      column(3,
                                             numericInput(inputId = "total_days", 
                                                          label = "Total Days",
                                                          value = 150, min = 100)
                                             )
                                  ),
                                  
                                  plotlyOutput("simulation_graph"),
                                  
                                  dataTableOutput("simulation_table")
                                  
                              )
                          )
                          
                 ),
                 
                 
                 tabPanel("SIR Model by Age",
                          
                          sidebarLayout(
                              sidebarPanel(h4("Age 0-19"),
                                  
                                  fluidRow(
                                      
                                      
                                      column(7,
                                             numericInput(inputId = "age_0_19_susceptible",
                                                          label = "Initial Susceptible",
                                                          value = 899933, min = 0),
                                             numericInput(inputId = "age_0_19_infected", 
                                                          label = "Initial Infected",
                                                          value = 66, min = 0),
                                             numericInput(inputId = "age_0_19_recovered", 
                                                          label = "Initial Recovered",
                                                          value = 0, min = 0)
                                             ),
                                      
                                      column(5,
                                             numericInput(inputId = "age_0_19_mean_recovery", 
                                                          label = "Mean Recovery",
                                                          value = 14, min = 0),
                                             numericInput(inputId = "age_0_19_doubling_time", 
                                                          label = "Doubling Time",
                                                          value = 4, min = 0)
                                             )
                                      
                                      
                                  ),
                                  
                                  h4("Age 20-64"),
                                  
                                  fluidRow(
                                      
                                      
                                      column(7,
                                             numericInput(inputId = "age_20_64_susceptible",
                                                          label = "Initial Susceptible",
                                                          value = 2159840, min = 0),
                                             numericInput(inputId = "age_20_64_infected", 
                                                          label = "Initial Infected",
                                                          value = 160, min = 0),
                                             numericInput(inputId = "age_20_64_recovered",
                                                          label = "Initial Recovered",
                                                          value = 0, min = 0)
                                      ),
                                      
                                      column(5,
                                             numericInput(inputId = "age_20_64_mean_recovery", 
                                                          label = "Mean Recovery",
                                                          value = 14, min = 0),
                                             numericInput(inputId = "age_20_64_doubling_time", 
                                                          label = "Doubling Time",
                                                          value = 4, min = 0)
                                      )
                                      
                                  ),
                                  
                                  h4("Age 65+"),
                                  
                                  fluidRow(
                                      
                                      
                                      column(7,
                                             numericInput(inputId = "age_65_susceptible",
                                                          label = "Initial Susceptible",
                                                          value = 539960, min = 0),
                                             numericInput(inputId = "age_65_infected", 
                                                          label = "Initial Infected",
                                                          value = 40, min = 0),
                                             numericInput(inputId = "age_65_recovered", 
                                                          label = "Initial Recovered",
                                                          value = 0, min = 0)
                                      ),
                                      
                                      column(5,
                                             numericInput(inputId = "age_65_mean_recovery", 
                                                          label = "Mean Recovery",
                                                          value = 14, min = 0),
                                             numericInput(inputId = "age_65_doubling_time", 
                                                          label = "Doubling Time",
                                                          value = 4, min = 0)
                                      )
                                      
                                      
                                  )
                                  
                                  
                              ),
                              
                              mainPanel(
                                  
                                  fluidRow(
                                      column(3,
                                             dateInput(inputId = "age_start_date", 
                                                       label = "Start Date",
                                                       "2020-03-05")
                                      ),
                                      column(3,
                                             numericInput(inputId = "age_total_days", 
                                                          label = "Total Days",
                                                          value = 150, min = 100)
                                      )
                                  ),
                                  
                                  plotlyOutput("age_simulation_graph"),
                                  
                                  dataTableOutput("age_simulation_table")
                                  
                              )
                          )
                          
                 )
                 
)


server <- function(input, output) {
    
    simulation <- reactive({
    
        print(class(input$start_date))
        simulate(susceptible_origin = input$susceptible,
                 infected_origin = input$infected,
                 recovered_origin = input$recovered,
                 mean_recovery = input$mean_recovery,
                 doubling_time = input$doubling_time,
                 day_total = input$total_days,
                 date_origin = input$start_date) %>%
            dplyr::mutate_if(is.numeric, as.integer)
    })
    
    age_simulation <- reactive({
        
        age_0_19 <- simulate(susceptible_origin = input$age_0_19_susceptible,
                             infected_origin = input$age_0_19_infected,
                             recovered_origin = input$age_0_19_recovered,
                             mean_recovery = input$age_0_19_mean_recovery,
                             doubling_time = input$age_0_19_doubling_time,
                             day_total = input$age_total_days,
                             date_origin = input$age_start_date)
        
        age_20_64 <- simulate(susceptible_origin = input$age_20_64_susceptible,
                              infected_origin = input$age_20_64_infected,
                              recovered_origin = input$age_20_64_recovered,
                              mean_recovery = input$age_20_64_mean_recovery,
                              doubling_time = input$age_20_64_doubling_time,
                              day_total = input$age_total_days,
                              date_origin = input$age_start_date)
        
        age_65 <- simulate(susceptible_origin = input$age_65_susceptible,
                           infected_origin = input$age_65_infected,
                           recovered_origin = input$age_65_recovered,
                           mean_recovery = input$age_65_mean_recovery,
                           doubling_time = input$age_65_doubling_time,
                           day_total = input$age_total_days,
                           date_origin = input$age_start_date)
        
        dplyr::bind_rows(age_0_19, age_20_64, age_65) %>%
            dplyr::group_by(day, date) %>%
            dplyr::summarise_all(sum) %>%
            dplyr::mutate_if(is.numeric, as.integer)
        
    })
    
    output$simulation_table <- renderDataTable({
        
        simulation() %>%
            dplyr::mutate(date = as.character(date))
        
    }, options = list(dom = 't', pageLength = input$total_days))
    
    output$age_simulation_table <- renderDataTable({
        
        age_simulation() %>%
            dplyr::mutate(date = as.character(date))
        
    }, options = list(dom = 't', pageLength = input$age_total_days))
    
    output$simulation_graph <- renderPlotly({
        
        sim <- simulation() %>%
            tidyr::pivot_longer(susceptible:recovered, names_to = "status",
                                values_to = "count")
        
        p <- ggplot(sim, aes(date, count, color = status)) + geom_line() +
            scale_y_continuous(labels = scales::comma)
        
        ggplotly(p)
        
    })
    
    output$age_simulation_graph <- renderPlotly({
        
        sim <- age_simulation() %>%
            tidyr::pivot_longer(susceptible:recovered, names_to = "status",
                                values_to = "count")
        
        p <- ggplot(sim, aes(date, count, color = status)) + geom_line() +
            scale_y_continuous(labels = scales::comma)
        
        ggplotly(p)
        
    })
}


shinyApp(ui = ui, server = server)
