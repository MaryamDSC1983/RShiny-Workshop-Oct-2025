#EXERCISE SOLN. 2
ui <- fluidPage(
  titlePanel("Hello Shiny!"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs",
                  "Number of observations:",
                  min = 0,
                  max = 1000,
                  value = 500)
    ),
    mainPanel(
      plotOutput("distPlot")
    ),
    position = "right"  # moves sidebar to the right
  )
)


# Server logic
server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs))
  })
}

# Complete app with UI and server components
shinyApp(ui, server)

-----------------------------------------------------------------------------------------------------------------

#EXERCISE SOLN. 3
ui <- fluidPage(
  
  # Top row: two plots side by side
  fluidRow(
    column(6, plotOutput("plot1")),
    column(6, plotOutput("plot2"))
  ),
  
  # Bottom row: controls
  fluidRow(
    column(12,sliderInput("obs", "Number of points:", min = 10, max = 1000, value = 500, width = '100%'))
  )
)


server <- function(input, output, session) {
  output$plot1 <- renderPlot({
    hist(rnorm(input$obs))
  })
  
  output$plot2 <- renderPlot({
    plot(rnorm(input$obs), rnorm(input$obs))
  })
}

shinyApp(ui, server)