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
                              value = 0.5)), 
           column(width = 4, 
                  numericInput("trials", 
                               "Number of trials", 
                               value = 10))), 
  mainPanel(plotOutput("pdr"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$pdr <- renderPlot({
    tibble(dens = dbinom(c(0:input$trials), input$trials, input$prob), x = c(0:input$trials)) %>% 
      ggplot(mapping = aes(x = x, y = dens), input$trials) + #bins = round(log(input$trials/5))) +
      geom_col(fill = '#528ce1') +
      theme_minimal()})
}



# Run the application 
shinyApp(ui = ui, server = server)