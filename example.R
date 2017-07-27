library(shiny)

makeHistObj <- function(n){
  x <- rnorm(n)
  plot(x)
  h <- hist(x, plot = FALSE)
  return(h)
}

ui <- fluidPage(
  headerPanel(title = "Produce 'histogram class' and plot"),
  
  sidebarPanel(
    submitButton("Recalculate"),
    sliderInput(
      inputId = "n",
      label = "n",
      min = 5, max = 1000, value = 100, step = 5
    )
  ),
  
  mainPanel(
    tags$h4("Internal xyplot from FUN():"),
    plotOutput("internal_plot"),
    tags$h4("External histogram plot using result of FUN():"),
    plotOutput("external_plot")
  )
    
)

server <- function(input, output) {
  h <- reactive({makeHistObj(n=input$n)})
  output$internal_plot <- renderPlot({
    par(mar=c(5,5,1,1))
    h()
  })
  output$external_plot <- renderPlot({
    par(mar=c(5,5,1,1))
    plot(h())
  })
}

shinyApp(ui = ui, server = server)