# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(width = 4, 
           numericInput("location",
                        "Location parameter of the logistic distribution:",
                        value = 1)), 
    column(width = 4, 
           numericInput("scale",
                        "Scale parameter of the logistic distribution:",
                        min = 0.0001,
                        value = 1))), 
  mainPanel(plotOutput("pdr"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$pdr <- renderPlot({
    tibble(dens = dlogis(x = seq(from = qlogis(location = input$location,
                                               scale = input$scale,
                                               p = 0.001),
                                 to = qlogis(location = input$location,
                                             scale = input$scale,
                                             p = 0.999),
                                 length.out = 1000),
                         location = input$location, 
                         scale = input$scale),
           x = seq(from = qlogis(location = input$location,
                                 scale = input$scale,
                                 p = 0.001),
                   to = qlogis(location = input$location,
                               scale = input$scale,
                               p = 0.999),
                   length.out = 1000)) %>% 
      ggplot(mapping = aes(x = x, y = dens)) + #bins = round(log(input$trials/5))) +
      geom_line(color = '#528ce1') +
      theme_minimal()})
}



# Run the application 
shinyApp(ui = ui, server = server)