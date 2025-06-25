library('shiny')
library('tidyverse')
library('extraDistr')

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(width = 4, 
           numericInput("size",
                        "Number of trials - i.e. how many times a bernoulli trial is run:",
                        min = 1,
                        value = 10, 
                        step = 1)), 
    column(width = 4, 
           numericInput("alpha",
                        "Shape parameter alpha of the beta distribution",
                        min = 0,
                        value = 1)), 
    column(width = 4, 
           numericInput("beta",
                       "Shape parameter beta of the beta distribution",
                       min = 0,
                       value = 1))), 
  mainPanel(textOutput('description'),
            plotOutput("pdr"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #observe(updateSliderInput(inputId = "k", max = input$n+input$m))
  
  output$description <- renderText(expr = str_c('In a sample of size: ',
                                                as.character(input$m + input$n),
                                                ' with ', 
                                                as.character(input$m), 
                                                ' successes and ', 
                                                as.character(input$n), 
                                                ' failures:'))
  
  
  output$pdr <- renderPlot({
    tibble(dens = dbbinom(x = 0:input$size, size = input$size, alpha = input$alpha, beta = input$beta), x = 0:input$size) %>% 
      ggplot(mapping = aes(x = x, y = dens)) + #bins = round(log(input$trials/5))) +
      geom_col(fill = '#528ce1') +
      labs(x = str_c('Number of succeses out of ', as.character(input$k), ' attempts')) +
      theme_minimal()})
}



# Run the application 
shinyApp(ui = ui, server = server)