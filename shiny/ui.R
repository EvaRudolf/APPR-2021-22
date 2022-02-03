library(shiny)

shinyUI(fluidPage(
  titlePanel(""),
  sidebarLayout(
    position = "left",
    sidebarPanel(
      sliderInput(
        "stevilo",
        label = "Število najboljših:",
        min = 5, max = 30, step = 5,
        value = 10
      )
    )
    ,
    mainPanel(plotOutput("graf"))
  ),
  uiOutput("izborTabPanel")
))