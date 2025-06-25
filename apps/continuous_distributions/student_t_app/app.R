# Define UI for application that draws a histogram
library('chi')

ui <- fluidPage(
  fluidRow(
    column(width = 4, 
           numericInput("df",
                        "Degrees of freedom for the ",
                        min = 0,
                        value = 1))), 
  mainPanel(plotOutput("pdr"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$pdr <- renderPlot({
    tibble(dens = dt(x = seq(from = qt(p = 0.001, 
                                     df = input$df),
                             to = qt(p = 0.999, 
                                     df = input$df),
                             length.out = 1000),
                       df = input$df),
           x = seq(from = qt(p = 0.001, 
                             df = input$df),
                   to = qt(p = 0.999, 
                           df = input$df),
                   length.out = 1000)) %>% 
      ggplot(mapping = aes(x = x, y = dens), input$trials) + #bins = round(log(input$trials/5))) +
      geom_line(color = '#528ce1') +
      theme_minimal()})
}



# Run the application 
shinyApp(ui = ui, server = server)