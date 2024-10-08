library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "Hello Shiny!",
  sidebar = sidebar(
    sliderInput(
      inputId = "bins",
      label = "Number of bins:",
      min = 1,
      max = 50,
      value = 30
    ),
    selectInput(
      "colors",
      "Choose Color",
      list("Blue"="blue","Green"="green")
    )
  ),
  plotOutput(outputId = "distPlot")
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = input$colors, border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}
shinyApp(ui = ui, server = server)
