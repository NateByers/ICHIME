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
                                               
                                               
                                               column(6,
                                                      numericInput(inputId = "susceptible",
                                                                   label = "Susceptible",
                                                                   value = 3599733, min = 0),
                                                      numericInput(inputId = "infected", 
                                                                   label = "Infected",
                                                                   value = 266, min = 0),
                                                      numericInput(inputId = "recovered", label = "Recovered",
                                                                   value = 0, min = 0)
                                               ),
                                               
                                               column(6,
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
                                  
                                  plotlyOutput("simulation_graph"),
                                  
                                  tableOutput("simulation_table")
                                  
                              )
                          )
                          
                 ),
                 
                 
                 tabPanel("SIR Model by Age",
                          
                          sidebarLayout(
                              sidebarPanel(h4("Age 0-19"),
                                  
                                  fluidRow(
                                      
                                      
                                      column(6,
                                             numericInput(inputId = "age_0_19_susceptible",
                                                          label = "Susceptible",
                                                          value = 899933, min = 0),
                                             numericInput(inputId = "age_0_19_infected", 
                                                          label = "Infected",
                                                          value = 66, min = 0),
                                             numericInput(inputId = "age_0_19_recovered", label = "Recovered",
                                                          value = 0, min = 0)
                                             ),
                                      
                                      column(6,
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
                                      
                                      
                                      column(6,
                                             numericInput(inputId = "age_20_64_susceptible",
                                                          label = "Susceptible",
                                                          value = 2159840, min = 0),
                                             numericInput(inputId = "age_20_64_infected", 
                                                          label = "Infected",
                                                          value = 160, min = 0),
                                             numericInput(inputId = "age_20_64_recovered", label = "Recovered",
                                                          value = 0, min = 0)
                                      ),
                                      
                                      column(6,
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
                                      
                                      
                                      column(6,
                                             numericInput(inputId = "age_65_susceptible",
                                                          label = "Susceptible",
                                                          value = 539960, min = 0),
                                             numericInput(inputId = "age_65_infected", 
                                                          label = "Infected",
                                                          value = 40, min = 0),
                                             numericInput(inputId = "age_65_recovered", label = "Recovered",
                                                          value = 0, min = 0)
                                      ),
                                      
                                      column(6,
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
                                  
                                  plotlyOutput("age_simulation_graph"),
                                  
                                  tableOutput("age_simulation_table")
                                  
                              )
                          )
                          
                 )
                 
)


server <- function(input, output) {
    
    simulation <- reactive({
        total_days <- 150
        
        simulate(susceptible_origin = input$susceptible,
                 infected_origin = input$infected,
                 recovered_origin = input$recovered,
                 mean_recovery = input$mean_recovery,
                 doubling_time = input$doubling_time,
                 day_total = total_days,
                 date_origin = as.Date("2020-03-15"))
    })
    
    age_simulation <- reactive({
        
        total_days <- 150
        
        age_0_19 <- simulate(susceptible_origin = input$age_0_19_susceptible,
                             infected_origin = input$age_0_19_infected,
                             recovered_origin = input$age_0_19_recovered,
                             mean_recovery = input$age_0_19_mean_recovery,
                             doubling_time = input$age_0_19_doubling_time,
                             day_total = total_days,
                             date_origin = as.Date("2020-03-15"))
        
        age_20_64 <- simulate(susceptible_origin = input$age_20_64_susceptible,
                              infected_origin = input$age_20_64_infected,
                              recovered_origin = input$age_20_64_recovered,
                              mean_recovery = input$age_20_64_mean_recovery,
                              doubling_time = input$age_20_64_doubling_time,
                              day_total = total_days,
                              date_origin = as.Date("2020-03-15"))
        
        age_65 <- simulate(susceptible_origin = input$age_65_susceptible,
                           infected_origin = input$age_65_infected,
                           recovered_origin = input$age_65_recovered,
                           mean_recovery = input$age_65_mean_recovery,
                           doubling_time = input$age_65_doubling_time,
                           day_total = total_days,
                           date_origin = as.Date("2020-03-15"))
        
        dplyr::bind_rows(age_0_19, age_20_64, age_65) %>%
            dplyr::group_by(day, date) %>%
            dplyr::summarise_all(sum)
        
    })
    
    output$simulation_table <- renderTable({
        
        simulation() %>%
            dplyr::mutate(date = as.character(date))
        
    })
    
    output$age_simulation_table <- renderTable({
        
        age_simulation() %>%
            dplyr::mutate(date = as.character(date))
        
    })
    
    output$simulation_graph <- renderPlotly({
        
        sim <- simulation() %>%
            tidyr::pivot_longer(susceptible:recovered, names_to = "status",
                                values_to = "count")
        
        p <- ggplot(sim, aes(date, count, color = status)) + geom_line()
        
        ggplotly(p)
        
    })
    
    output$age_simulation_graph <- renderPlotly({
        
        sim <- age_simulation() %>%
            tidyr::pivot_longer(susceptible:recovered, names_to = "status",
                                values_to = "count")
        
        p <- ggplot(sim, aes(date, count, color = status)) + geom_line()
        
        ggplotly(p)
        
    })
}


shinyApp(ui = ui, server = server)
