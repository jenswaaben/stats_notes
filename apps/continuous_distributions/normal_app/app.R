library('shiny')
library('tidyverse')


# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(width = 4, 
           numericInput("mu",
                        "Mean of the normal distribution:",
                        value = 0)), 
    column(width = 4, 
           numericInput("sigma", 
                        "Standard deviation of the normal distribution:", 
                        value = 1, 
                        min = 0.001))), 
  mainPanel(plotOutput("pdr"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$pdr <- renderPlot({
    tibble(dens = dnorm(x = seq(input$mu - 5 * input$sigma,
                                input$mu + 5 * input$sigma,
                                length.out = 1000),
                        mean = input$mu, 
                        sd = input$sigma),
           x = seq(input$mu - 5 * input$sigma,
                   input$mu + 5 * input$sigma,
                   length.out = 1000)) %>% 
      ggplot(mapping = aes(x = x, y = dens), input$trials) + 
      geom_line(color = '#528ce1') +
      theme_minimal()})
}



# Run the application 
shinyApp(ui = ui, server = server)