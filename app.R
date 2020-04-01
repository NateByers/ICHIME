library(shiny)

r_files <- list.files("R", full.names = TRUE)
lapply(r_files, source)

ui <- navbarPage("COVID-19 Hospital Impact",
                 
                 tabPanel("SIR Model",
                          
                          sidebarLayout(
                              sidebarPanel("Age 0-19",
                                  
                                  numericInput(inputId = "age_0_19_susceptible", label = "Susceptible",
                                               value = 52000, min = 0, width = "50%"),
                                  
                                  numericInput(inputId = "age_0_19_infected", label = "Infected",
                                               value = 401, min = 0),
                                  
                                  numericInput(inputId = "age_0_19_recovered", label = "Recovered",
                                               value = 0, min = 0)
                                  
                              ),
                              
                              
                              mainPanel(
                                  dataTableOutput("simulation_table")
                              )
                          )
                          
                 )
                 
)


server <- function(input, output) {
    
    simulation <- reactive({
        
        simulate(susceptible_origin = input$age_0_19_susceptible,
                 infected_origin = input$age_0_19_infected,
                 recovered_origin = input$age_0_19_recovered,
                 mean_recovery = 14, doubling_time = 7, day_total = 100,
                 date_origin = as.Date("2020-03-15"))
        
    })
    
    output$simulation_table <- renderDataTable({
        
        simulation()
        
    })
}


shinyApp(ui = ui, server = server)
