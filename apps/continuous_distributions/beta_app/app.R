library('shiny')
library('tidyverse')


# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(width = 4, 
           numericInput("alpha",
                       "Alpha (the exponent of x)",
                       min = 0,
                       value = 0.5)), 
    column(width = 4, 
           numericInput("beta", 
                        "Beta (the exponent of 1-x)", 
                        value = 0.5, 
                        min = 0))), 
  mainPanel(plotOutput("pdr"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$pdr <- renderPlot({
    tibble(dens = dbeta(x = seq(0, 1, 0.01), shape1 = input$alpha, shape2 = input$beta), x = seq(0, 1, 0.01)) %>% 
      ggplot(mapping = aes(x = x, y = dens), input$trials) + #bins = round(log(input$trials/5))) +
      geom_line(color = '#528ce1') +
      theme_minimal()})
}



# Run the application 
shinyApp(ui = ui, server = server)