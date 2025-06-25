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
  
  output$description <- renderText(expr = str_c('How many failures in independent bernoulli trials are needed to get ',
                                                as.character(input$size),
                                                ' succcesses?'))
  
  
  output$pdr <- renderPlot({
    tibble(dens = dbnbinom(x = 0:(input$size*max(c(input$alpha/input$beta,
                                                   input$beta/input$alpha,
                                                   5,
                                                   input$size/(input$alpha+input$beta),
                                                   input$alpha+input$beta-input$size+2))),
                                  size = input$size,
                                  alpha = input$alpha,
                                  beta = input$beta),
           x =  0:(input$size*max(c(input$alpha/input$beta,
                                    input$beta/input$alpha,
                                    5,
                                    input$size/(input$alpha+input$beta),
                                    input$alpha+input$beta-input$size+2)))) %>% 
      mutate(cum_dens = cumsum(dens)) %>% 
      filter(cum_dens < 0.975) %>% 
      ggplot(mapping = aes(x = x, y = dens)) + 
      geom_col(fill = '#528ce1') +
      labs(x = str_c('Number of failures to get ', as.character(input$size), ' successes')) +
      theme_minimal()})
}



# Run the application 
shinyApp(ui = ui, server = server)