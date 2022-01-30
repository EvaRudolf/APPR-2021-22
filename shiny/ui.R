library(shiny)

shinyUI(fluidPage(
  titlePanel(""),
  sidebarLayout(
    position = "left",
    sidebarPanel(
      selectInput(
        "stevilo",
        label = "Število najboljših:",
        choices = c(1, 3, 5, 10, 15, 20, 30),
        selected = 5
      )
    )
    ,
    mainPanel(plotOutput("graf"))
  ),
  uiOutput("izborTabPanel")
))