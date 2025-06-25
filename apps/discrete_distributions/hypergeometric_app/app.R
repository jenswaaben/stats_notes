library('shiny')
library('tidyverse')


# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(width = 4, 
           numericInput("m",
                       "Number of successes in population:",
                       value = 10, 
                       step = 1)), 
    column(width = 4, 
           numericInput("n",
                       "Number of failures in population:",
                       value = 10, 
                       step = 1)), 
    column(width = 4, 
           sliderInput("k",
                       "Number of pulls:",
                       min = 0,
                       max = 10,
                       value = 5, 
                       round = TRUE))), 
  mainPanel(textOutput('description'),
            plotOutput("pdr"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observe(updateSliderInput(inputId = "k", max = input$n+input$m))
  
  output$description <- renderText(expr = str_c('In a sample of size: ',
                                                as.character(input$m + input$n),
                                                ' with ', 
                                                as.character(input$m), 
                                                ' successes and ', 
                                                as.character(input$n), 
                                                ' failures:'))
  
  
  output$pdr <- renderPlot({
    tibble(dens = dhyper(x = 0:input$k, m = input$m, n = input$n, k = input$k), x = 0:input$k) %>% 
      ggplot(mapping = aes(x = x, y = dens)) + #bins = round(log(input$trials/5))) +
      geom_col(fill = '#528ce1') +
      labs(x = str_c('Number of succeses out of ', as.character(input$k), ' attempts')) +
      theme_minimal()})
}



# Run the application 
shinyApp(ui = ui, server = server)