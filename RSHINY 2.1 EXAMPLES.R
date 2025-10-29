library(shiny)
library(ggplot2)

# Example 1
ui <- fluidPage(
  titlePanel("Central limit theorem"),
  sidebarLayout(
    sidebarPanel(
      numericInput("m", "Number of samples:", 2, min = 1, max = 100)
    ),
    mainPanel(
      plotOutput("hist")
    )
  )
)
server <- function(input, output, session) {
  output$hist <- renderPlot({
    means <- replicate(1e4, mean(runif(input$m)))
    hist(means, breaks = 20)
  }, res = 96)
}
shinyApp(ui, server)
----------------------------------------------------------------------------------------------------------
#EXAMPLE 2 
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Import data", 
             fileInput("file", "Data", buttonLabel = "Upload..."),
             textInput("delim", "Delimiter (leave blank to guess)", ""),
             numericInput("skip", "Rows to skip", 0, min = 0),
             numericInput("rows", "Rows to preview", 10, min = 1)
    ),
    tabPanel("Set parameters"),
    tabPanel("Visualise results")
  )
)
-----------------------------------------------------------------------------------------------------------
#EXAMPLE 3 
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        textOutput("panel")
      ),
      mainPanel(
        tabsetPanel(
          id = "tabset",
          tabPanel("panel 1", "one"),
          tabPanel("panel 2", "two"),
          tabPanel("panel 3", "three")
        )
      )
    )
  )
server <- function(input, output, session) {
  output$panel <- renderText({
    paste("Current panel: ", input$tabset)
  })
}
----------------------------------------------------------------------------------------------------------
#EXAMPLE 4
  ui <- fluidPage(
    navlistPanel(
      id = "tabset",
      "Heading 1",
      tabPanel("panel 1", "Panel one contents"),
      "Heading 2",
      tabPanel("panel 2", "Panel two contents"),
      tabPanel("panel 3", "Panel three contents")
    )
  )
---------------------------------------------------------------------------------------------------------
#EXAMPLE 5
  ui <- navbarPage(
    "Page title",   
    tabPanel("panel 1", "one"),
    tabPanel("panel 2", "two"),
    tabPanel("panel 3", "three"),
    navbarMenu("subpanels", 
               tabPanel("panel 4a", "four-a"),
               tabPanel("panel 4b", "four-b"),
               tabPanel("panel 4c", "four-c")
    )
  )
--------------------------------------------------------------------------------------------------------
#EXAMPLE 6
  ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "darkly"), #try sandstone, united, flatly
    sidebarLayout(
      sidebarPanel(
        textInput("txt", "Text input:", "text here"),
        sliderInput("slider", "Slider input:", 1, 100, 30)
      ),
      mainPanel(
        h1(paste0("Theme: darkly")),
        h2("Header 2"),
        p("Some text")
      )
    )
  )
--------------------------------------------------------------------------------------------------------
  #EXAMPLE 7
  theme <- bslib::bs_theme(
    bg = "#0b3", 
    fg = "pink", 
    base_font = "Source Sans Pro"
  )
ui <- fluidPage(
  theme = theme,
  sidebarLayout(
    sidebarPanel(
      textInput("txt", "Text input:", "text here"),
      sliderInput("slider", "Slider input:", 1, 100, 30)
    ),
    mainPanel(
      h1(paste0("YOUR OWN THEME:")),
      h2("Header 2"),
      p("Some text")
    )
  )
)
server <- function(input, output, session){
}
shinyApp(ui, server)
 
#EXAMPLE 8

library(shiny)
library(ggplot2)
ui <- fluidPage(
    plotOutput("plot", click = "plot_click"),
    verbatimTextOutput("info")
  )

server <- function(input, output) {
  output$plot <- renderPlot({
    plot(mtcars$wt, mtcars$mpg)
  }, res = 96)
  
  output$info <- renderPrint({
    req(input$plot_click)  # Ensure code runs only after a click
    x <- round(input$plot_click$x, 2)
    y <- round(input$plot_click$y, 2)
    cat("[", x, ", ", y, "]", sep = "")
  })
}
shinyApp(ui, server)

#------------------------------------------------------------------
  

#EXAMPLE 9
ui <- fluidPage(
    plotOutput("plot", click = "plot_click"),
    tableOutput("data")
  )
server <- function(input, output, session) {
  output$plot <- renderPlot({
    plot(mtcars$wt, mtcars$mpg)
  }, res = 96)
  
  output$data <- renderTable({
    nearPoints(mtcars, input$plot_click, xvar = "wt", yvar = "mpg")
  })
}
shinyApp(ui, server)
#---------------------------------

#example 10
ui <- fluidPage(
  plotOutput("plot", brush = "plot_brush"),
  tableOutput("data")
)
server <- function(input, output, session) {
  output$plot <- renderPlot({
    ggplot(mtcars, aes(wt, mpg)) + geom_point()
  }, res = 96)
  
  output$data <- renderTable({
    brushedPoints(mtcars, input$plot_brush)
  })
}
#-------------------------------------------------------
#example 11
set.seed(1014)
df <- data.frame(x = rnorm(100), y = rnorm(100))

ui <- fluidPage(
  plotOutput("plot", click = "plot_click", )
)
server <- function(input, output, session) {
  dist <- reactiveVal(rep(1, nrow(df)))
  observeEvent(input$plot_click,
               dist(nearPoints(df, input$plot_click, allRows = TRUE, addDist = TRUE)$dist_)  
  )
  
  output$plot <- renderPlot({
    df$dist <- dist()
    ggplot(df, aes(x, y, size = dist)) + 
      geom_point() + 
      scale_size_area(limits = c(0, 1000), max_size = 10, guide = NULL)
  }, res = 96)
}
shinyApp(ui, server)
#----------------------------------------------------------
#Example 12
ui <- fluidPage(
  sliderInput("height", "height", min = 100, max = 500, value = 250),
  sliderInput("width", "width", min = 100, max = 500, value = 250),
  plotOutput("plot", width = 250, height = 250)
)
server <- function(input, output, session) {
  output$plot <- renderPlot(
    width = function() input$width,
    height = function() input$height,
    res = 96,
    {
      plot(rnorm(20), rnorm(20))
    }
  )
}
shinyApp(ui, server)
#----------------------------------------------------------