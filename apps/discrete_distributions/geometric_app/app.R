library('shiny')
library('tidyverse')


# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(width = 4, 
           sliderInput("prob",
                       "Probability of success:",
                       min = 0,
                       max = 1,
                       value = 0.5))), 
  mainPanel(plotOutput("pdr"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$pdr <- renderPlot({
    tibble(dens = dgeom(c(0:round((1/(input$prob/5)))), prob = input$prob),
           x = c(0:round((1/(input$prob/5))))) %>% 
      ggplot(mapping = aes(x = x, y = dens), input$trials) + #bins = round(log(input$trials/5))) +
      geom_col(fill = '#528ce1') +
      theme_minimal() + 
      labs(x = "Number of failures before one success")})
}



# Run the application 
shinyApp(ui = ui, server = server)