install.packages("plotly")
library(plotly)


data(iris)
# Create an interactive scatter plot using plotly
scatter_plot <- plot_ly(data = iris, x = ~Sepal.Length, y = ~Sepal.Width, 
                        color = ~Species, text = ~paste("Species: ", Species),
                        mode = "markers", type = "scatter")

# Add labels and title
scatter_plot <- scatter_plot %>% 
  layout(title = "Interactive Scatter Plot of Sepal Length vs. Sepal Width",
         xaxis = list(title = "Sepal Length"),
         yaxis = list(title = "Sepal Width"))

scatter_plot
# Add hover labels, marker symbols, and legend
scatter_plot <- scatter_plot %>% 
  add_trace(marker = list(size = 10),
            hoverinfo = "text",
            text = ~paste("Species: ", Species, "<br>Sepal Length: ", Sepal.Length, 
                          "<br>Sepal Width: ", Sepal.Width))

# Customize legend
scatter_plot <- scatter_plot %>% 
  layout(legend = list(title = "Species"))

scatter_plot
# Load required packages
library(shiny)
library(plotly)

# Define UI for the dashboard
ui <- fluidPage(
  titlePanel("Interactive Iris Data Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("species", "Select Species:", choices = unique(iris$Species))
    ),
    mainPanel(
      plotlyOutput("scatter_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$scatter_plot <- renderPlotly({
    filtered_data <- iris[iris$Species == input$species, ]
    
    plot_ly(data = filtered_data, x = ~Sepal.Length, y = ~Sepal.Width, 
            color = ~Species, text = ~paste("Species: ", Species),
            mode = "markers", type = "scatter") %>%
      layout(title = paste("Interactive Scatter Plot of Sepal Length vs. Sepal Width for", input$species),
             xaxis = list(title = "Sepal Length"),
             yaxis = list(title = "Sepal Width"))
  })
}
options(viewer=NULL)
# Run the app
shinyApp(ui, server)



install.packages("shiny")
library(shiny)

# 1.0 Hello World App
ui <- fluidPage(
  "Hello!"
)
server <- function(input, output, session) {
}
shinyApp(ui, server)

# 1.2 Add Some UI Components
ui <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  tableOutput("table")
)
server <- function(input, output, session) {
}
shinyApp(ui, server)

# 1.3 Add Reactivity
ui <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  tableOutput("table")
)
server <- function(input, output, session) {
  output$summary <- renderPrint({
    dataset <- get(input$dataset, "package:datasets")
    summary(dataset)
  })
  output$table <- renderTable({
    dataset <- get(input$dataset, "package:datasets")
    dataset
  })
}
shinyApp(ui, server)

# 1.5 Separate Reactive Expression
ui <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  tableOutput("table")
)
server <- function(input, output, session) {
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })
  output$summary <- renderPrint({
    summary(dataset())
  })
  output$table <- renderTable({
    dataset()
  })
}
shinyApp(ui, server)

# 4 Case study: ER injuries
library(shiny)
library(vroom)
library(tidyverse)

#
ui <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  tableOutput("table"),
)
server <- function(input, output, session) {
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })
  output$summary <- renderPrint({
    summary(dataset())
  })
  output$table <- renderTable({
    dataset()
  })
}
shinyApp(ui, server)


