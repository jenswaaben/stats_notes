# Define UI for application that draws a histogram
library('chi')

ui <- fluidPage(
  fluidRow(
    column(width = 4, 
           numericInput("shape",
                        "Shape parameter of Gamma distribution:",
                        min = 0.001,
                        value = 1)), 
    column(width = 4, 
           numericInput("rate",
                        "Rate parameter of Gamma distribution:",
                        min = 0.001,
                        value = 1))), 
  mainPanel(plotOutput("pdr"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$pdr <- renderPlot({
    tibble(dens = dgamma(x = seq(from = 0,
                                 to = qgamma(shape = input$shape,
                                             rate = input$rate,
                                             p = 0.999),
                                 0.01),
                         shape = input$shape, 
                         rate = input$rate),
           x = seq(from = 0,
                   to = qgamma(shape = input$shape,
                               rate = input$rate,
                               p = 0.999),
                   0.01)) %>% 
      ggplot(mapping = aes(x = x, y = dens)) + #bins = round(log(input$trials/5))) +
      geom_line(color = '#528ce1') +
      theme_minimal()})
}



# Run the application 
shinyApp(ui = ui, server = server)