library('shiny')
library('tidyverse')


# Define UI for application that draws a histogram
ui <- fluidPage(
  sliderInput("prob",
              "Probability of success:",
              min = 0,
              max = 1,
              value = 0.5),
  mainPanel(plotOutput("pdr"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$pdr <- renderPlot({
    tibble(dens = dbinom(c(0,1), 1, input$prob), x = c('Failure', "Success")) %>% 
      ggplot(mapping = aes(x = x, y = dens, fill = x)) +
        geom_col() +
        ylim(0,1) +
        theme_minimal()})
}



# Run the application 
shinyApp(ui = ui, server = server)
