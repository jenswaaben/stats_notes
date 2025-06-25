library('shiny')
library('tidyverse')
library('greybox')


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
    tibble(dens = dlogitnorm(q = seq(0,
                                1,
                                length.out = 1000),
                        mu = input$mu, 
                        sigma = input$sigma),
           x = seq(0,
                   1,
                   length.out = 1000)) %>% 
      ggplot(mapping = aes(x = x, y = dens)) + 
      geom_line(color = '#528ce1') +
      theme_minimal()})
}



# Run the application 
shinyApp(ui = ui, server = server)