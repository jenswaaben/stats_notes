library('shiny')
library('tidyverse')


# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(width = 4, 
           sliderInput("prob",
                       "Probability of success:",
                       min = 0.001,
                       max = 1,
                       value = 0.5)), 
    column(width = 4, 
           numericInput("success_stop", 
                        "Number of successes required:",
                        min = 1,
                        value = 10))), 
  mainPanel(plotOutput("pdr"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$pdr <- renderPlot({
    tibble(dens = dnbinom(x = 0:(round(input$success_stop/input$prob*2)),
                          size = input$success_stop,
                          prob = input$prob),
           x = 0:(round(input$success_stop/input$prob*2))) %>% 
      ggplot(mapping = aes(x = x,
                           y = dens)) +
      geom_col(fill = '#528ce1') +
      theme_minimal() + 
      labs(x = str_c('Number of failures before ', as.character(input$success_stop), ' successes'))
  })
}



# Run the application 
shinyApp(ui = ui, server = server)