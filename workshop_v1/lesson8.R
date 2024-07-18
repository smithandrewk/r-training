# September 28th
install.packages("vroom")
library(vroom)
library(dplyr)
setwd("/Users/andrew/R_training")
products <- vroom("products.tsv")
population <- vroom("population.tsv")
injuries <- vroom("injuries.tsv.gz")
basketball <- injuries[injuries$prod_code == 1205,]

basketball %>% count(body_part)
basketball %>% count(location)
basketball %>% count(sex)
summary <- basketball %>% count(age,sex)

plot(summary$age,summary$n)
library(ggplot2)
ggplot(summary,aes(age,n,color=sex)) + geom_line()

# Shiny Recap
library(shiny)
# v1
# 2 components - ui, server 
ui <- fluidPage(
  "Hello World!"
)
server <- function(input, output, session) {}

shinyApp(ui, server)


#v2
ui <- fluidPage(
  fluidRow(
    column(3,selectInput("selector","Choices",choices=c("649","1205","1333"))),
  ),
  fluidRow(
    column(3,plotOutput("plot_0")),
    column(9,plotOutput("plot_1")),
  )
  )


server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$selector))
  
  output$plot_0 <- renderPlot(
    selected() %>% ggplot(aes(age,n,color=sex)) + geom_line()
  )
  output$plot_1 <- renderPlot(
    selected() %>% ggplot(aes(age,n,color=sex)) + geom_line()
  )
}

shinyApp(ui, server)
prod_codes <- setNames(products$prod_code, products$title)

ui <- fluidPage(
  fluidRow(
    column(6,
           selectInput("code", "Product", choices = prod_codes)
    )
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  )
)

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  output$diag <- renderTable(
    selected() %>% count(diag, wt = weight, sort = TRUE)
  )
  output$body_part <- renderTable(
    selected() %>% count(body_part, wt = weight, sort = TRUE)
  )
  output$location <- renderTable(
    selected() %>% count(location, wt = weight, sort = TRUE)
  )
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    summary() %>%
      ggplot(aes(age, n, colour = sex)) +
      geom_line() +
      labs(y = "Estimated number of injuries")
  }, res = 96)
}
shinyApp(ui,server)






















