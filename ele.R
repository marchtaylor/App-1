library(shiny)
library(TropFishR)

data("alba")
dat <- alba
ui <- fluidPage(
  headerPanel(title = "ELEFAN_GA"),
  
  sidebarPanel(
    submitButton("Start Genetic Algorithm"),
    numericInput(
      inputId = "seasonalized",
      label = "Seasonalized VBGF (TRUE=1, FALSE=0)",
      min = 0, max = 1, value = 0, step = 1
    ),
    
    sliderInput(
      inputId = "MA",
      label = "Moving average (MA) span",
      value = 5, min = 5, max = length(dat$midLengths)/4, round = TRUE,
      step = 2
    ),
    
    sliderInput(
      inputId = "Linf",
      label = "Linf range",
      dragRange = TRUE,
      value = range(dat$midLengths), min = 0, max = max(dat$midLengths)*2,
      step = 0.25
    ),
    
    sliderInput(
      inputId = "K",
      label = "K range",
      dragRange = TRUE,
      value = range(0.001, 3), min = 0.01, max = 3,
      step=0.01
    ),
    
    sliderInput(
      inputId = "t_anchor",
      label = "t_anchor range",
      dragRange = TRUE,
      value = range(0, 1), min = 0, max = 1,
      step=0.1
    ),
    
    sliderInput(
      inputId = "C",
      label = "C range",
      dragRange = TRUE,
      value = range(0, 1), min = 0, max = 1.2,
      step=0.1
    ),
    
    sliderInput(
      inputId = "ts",
      label = "ts range",
      dragRange = TRUE,
      value = range(0, 1), min = 0, max = 1,
      step = 0.1
    ),

    sliderInput(
      inputId = "popSize",
      label = "Population size",
      value = 10, min = 10, max = 100, step = 1, round = TRUE
    ),
    
    sliderInput(
      inputId = "maxiter",
      label = "Maximum number of generations",
      value = 10, min = 5, max = 200, step = 1, round = TRUE
    ),
    
    sliderInput(
      inputId = "run",
      label = "Stopping criteria (generations without improvement",
      value = 10, min = 5, max = 200, step = 1, round = TRUE
    )
  ),
  
  mainPanel(
    tags$h4("LFQ plot"),
    plotOutput("ELEFAN_GA_lfq_plot"),
    tags$h4("Fitted parameters:"),
    tableOutput("pars")
  )
  
)

server <- function(input, output) {
  ELEFAN_GA_res <- reactive({
    ELEFAN_GA(
      x=dat, seasonalised = input$seasonalized, parallel = FALSE,
      low_par = list(Linf=input$Linf[1], K=input$K[1], t_anchor=input$t_anchor[1], C=input$C[1], ts=input$ts[1]),
      up_par = list(Linf=input$Linf[2], K=input$K[2], t_anchor=input$t_anchor[2], C=input$C[2], ts=input$ts[2]),
      popSize=input$popSize, maxiter=input$maxiter, run = input$run, plot.score = TRUE
    )
  }) 

  PARS <- reactive({
    tmp <- as.data.frame(c(ELEFAN_GA_res()$par, ELEFAN_GA_res()$Rn_max))
    names(tmp) <- c(names(tmp)[-length(names(tmp))], "Rn" )
    names(tmp) <- replace(names(tmp), names(tmp)=="phiL", "phi'")
    tmp
  })
  
  output$pars <- renderTable({PARS()})
  
  
  # output$ELEFAN_GA_score_plot <- renderPlot({ # should work but doesn't
  #   # par(mar=c(5,5,2,1))
  #   ELEFAN_GA_res()
  # })
  
  output$ELEFAN_GA_lfq_plot <- renderPlot({
    par(mar=c(5,5,2,1))
    plot(ELEFAN_GA_res())
  })
  
}




shinyApp(ui = ui, server = server)

