# Define UI for application that draws a histogram
library('chi')

ui <- fluidPage(
  fluidRow(
    column(width = 4, 
           numericInput("df1",
                        "Degrees of freedom for the first chisquared distribution (aka number of standard normal distributions to take the SSRs off):",
                        min = 1,
                        value = 1)), 
    column(width = 4, 
           numericInput("df2",
                        "Degrees of freedom for the second chisquared distribution:",
                        min = 1,
                        value = 1))), 
  mainPanel(plotOutput("pdr"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$pdr <- renderPlot({
    tibble(dens = df(x = seq(from = 0, to = qf(df1 = input$df1, df2 = input$df2, p = 0.99), length.out = 1000),
                     df1 = input$df1, 
                     df2 = input$df2),
           x = seq(from = 0, to = qf(df1 = input$df1, df2 = input$df2, p = 0.99), length.out = 1000)) %>% 
      ggplot(mapping = aes(x = x, y = dens)) + 
      geom_line(color = '#528ce1') +
      theme_minimal()})
}



# Run the application 
shinyApp(ui = ui, server = server)