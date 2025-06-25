# Define UI for application that draws a histogram
library('chi')

ui <- fluidPage(
  fluidRow(
    column(width = 4, 
           numericInput("df",
                        "Degrees of freedom for the chisquared distribution (aka number of standard normal distributions to take the SSRs off):",
                        min = 0,
                        value = 1))), 
  mainPanel(plotOutput("pdr"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$pdr <- renderPlot({
    tibble(dens = dchisq(x = seq(0, qchisq(df = input$df, p = 0.999), 0.01),
                       df = input$df),
           x = seq(0, qchisq(df = input$df, p = 0.999), 0.01)) %>% 
      ggplot(mapping = aes(x = x, y = dens), input$trials) + #bins = round(log(input$trials/5))) +
      geom_line(color = '#528ce1') +
      theme_minimal()})
}



# Run the application 
shinyApp(ui = ui, server = server)