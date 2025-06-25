library('shiny')
library('tidyverse')
library('extraDistr')
library('PoissonBinomial')

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(width = 4, 
           numericInput("lambda",
                        "Lambda - the means of the poisson distribution:",
                        min = 0,
                        value = 1))),
  mainPanel(textOutput('description'),
            plotOutput("pdr"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observe(updateNumericInput(inputId = "top", min = input$bottom, max = 1))
  
  
  output$pdr <- renderPlot({
    tibble(dens = dpois(x = 0:round(abs(2*(input$lambda+10))),
                        lambda = input$lambda),
           x = 0:round(abs(2*(input$lambda+10)))) %>% 
      ggplot(mapping = aes(x = x, y = dens)) + #bins = round(log(input$trials/5))) +
      geom_line(color = '#528ce1') +
      geom_point(color = '#528ce1', shape = 21, size = 3, fill = 'white') +
      labs(x = str_c('Number of succeses out of ', as.character(input$trials), ' attempts')) +
      theme_minimal()})
}



# Run the application 
shinyApp(ui = ui, server = server)