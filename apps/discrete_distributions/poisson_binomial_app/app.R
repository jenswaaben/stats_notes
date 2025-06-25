library('shiny')
library('tidyverse')
library('extraDistr')
library('PoissonBinomial')

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(width = 4, 
           numericInput("trials",
                        "Number of trials - i.e. how many times a bernoulli trial is run:",
                        min = 1,
                        value = 10, 
                        step = 1)), 
    column(width = 4, 
           numericInput("top",
                        "Top: ",
                        min = 0,
                        max = 1,
                        value = 0.5, 
                        step = 0.05)), 
    column(width = 4, 
           numericInput("bottom",
                        "Bottom",
                        min = 0,
                        max = 1,
                        value = 0.3, 
                        step = 0.05))), 
  mainPanel(textOutput('description'),
            plotOutput("pdr"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observe(updateNumericInput(inputId = "top", min = input$bottom, max = 1))
  
  observe(updateNumericInput(inputId = "bottom", max = input$top-0.001, min = 0))
  
  output$description <- renderText(expr = str_c('Probabilities are uniformly distributed between top and bottom (selected in UI). Number of succeses out of ', as.character(input$trials)))
  
  
  output$pdr <- renderPlot({
    tibble(dens = dpbinom(x = 0:input$trials, probs = runif(input$trials, input$bottom, input$top)), x = 0:input$trials) %>% 
      ggplot(mapping = aes(x = x, y = dens)) + #bins = round(log(input$trials/5))) +
      geom_col(fill = '#528ce1') +
      labs(x = str_c('Number of succeses out of ', as.character(input$trials), ' attempts')) +
      theme_minimal()})
}



# Run the application 
shinyApp(ui = ui, server = server)