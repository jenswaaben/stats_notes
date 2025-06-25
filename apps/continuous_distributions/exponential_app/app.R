# Define UI for application that draws a histogram

ui <- fluidPage(
  fluidRow(
    column(width = 4, 
           numericInput("rate1",
                        "rate of exponential increase/decay:",
                        min = 0.001,
                        value = 1, 
                        step = 1)), 
    column(width = 4, 
           numericInput("rate2",
                        "rate of exponential increase/decay:",
                        min = 0.001,
                        value = 1))), 
  mainPanel(plotOutput("pdr"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$pdr <- renderPlot({
    tibble(dens1 = dexp(x = seq(0, qexp(rate = input$rate1, p = 0.999) + 1, length.out = 1000),
                         rate = input$rate1),
           x1 = seq(0, qexp(rate = input$rate1, p = 0.999) + 1, length.out = 1000), 
           dens2 = dexp(x = seq(0, qexp(rate = input$rate2, p = 0.999) + 1, length.out = 1000),
                        rate = input$rate2), 
           x2 = seq(0, qexp(rate = input$rate2, p = 0.999) + 1, length.out = 1000)) %>% 
      ggplot() + 
      geom_line(mapping = aes(x = x1, y = dens1), color = '#528ce1', size = 2) +
      geom_line(mapping = aes(x = x2, y = dens2), color = '#fb3eb1', size = 2) +
      theme_minimal() + 
      labs(x = 'Time between poisson points', 
           y = 'density')})
}



# Run the application 
shinyApp(ui = ui, server = server)