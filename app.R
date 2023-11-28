# by A. Marcia Barbosa (https://modtools.wordpress.com/barbosa/)

# RStudio: Session -> Set Working Directory -> To Source File Location


# load required packages:
library(terra)
library(modEvA)
library(shiny)


# user interface:
ui <- pageWithSidebar(
  headerPanel('Model evaluation with threshold-based metrics'),

  sidebarPanel(
    width = 3,
    sliderInput(inputId = "slider",
                label = "Choose threshold:",
                min = 0,
                max = 1,
                value = 0.5,
                step = 0.01),
    p("Circles on maps show species presences")
  ),

  mainPanel(
    fluidRow(
      column(width = 5,
             h4("Original predictions"),
             plotOutput('map_pred_cont')
      ),

      column(width = 5,
             h4("Binarized predictions"),
             plotOutput('map_pred_01')
      )#,

      # column(width = 3,
      #        plotOutput('confumap')
      # )
    ),

    fluidRow(
      column(width = 5,
             fluidRow(
               column(width = 12,
                      h4("Confusion map"),
                      plotOutput('confumap')
               )
             ),

             fluidRow(
               column(width = 12,
                      h4("Confusion matrix"),
                      tableOutput('confumat')
               )
             )
      ),

      column(width = 5,
             h4("Threshold-based metrics"),
             plotOutput('threshmeas')
      )#,

      # column(3,
      #        plotOutput('similarity')
      # )
    )
  )
)


# server actions:
server <- function(input, output) {

  ib <- terra::vect("data/ib.gpkg")
  occ <- read.csv("data/occ.csv")
  pred <- terra::rast("data/pred.tif")
  pred_vals <- as.character(round(values(pred), 2))
  pred_vals[pred_vals == "NaN"] <- ""

  thr <- reactive({
    input$slider
  })

  output$map_pred_cont <- renderPlot({
    plot(pred, axes = FALSE, mar = c(0, 0, 0, 3))
    plot(ib, border = "brown", add = TRUE)
    points(occ, pch = 1, cex = 4, col = "grey30", lwd = 2)
    text(pred, pred_vals, cex = 0.7, halo = TRUE)
  })

  output$map_pred_01 <- renderPlot({
    pred_thr <- modEvA::applyThreshold(obs = occ, pred = pred, thresh = thr(), verbosity = 0)
    plot(pred_thr, axes = FALSE, mar = c(0, 0, 0, 3))
    points(occ, pch = 1, cex = 4, col = "grey30", lwd = 2)
    plot(ib, border = "brown", add = TRUE)
  })

  output$confumap <- renderPlot({
    confumap <- modEvA::confusionLabel(obs = occ, pred = pred, thresh = thr(), verbosity = 0)
    plot(confumap, col = c("orange", "lightblue", "red", "blue"), mar = c(0, 0, 0, 7), legend = FALSE)
    legend(x = 4, y = 44, legend = c("TruePos", "FalsePos", "TrueNeg", "FalseNeg"), fil = c("blue", "lightblue", "red", "orange"), bty = "n", xpd = NA)
    plot(ib, border = "brown", lwd = 2, add = TRUE)
  })

  output$confumat <- renderTable({
    confumat <- modEvA::confusionMatrix(obs = occ, pred = pred, thresh = thr())
  },
  rownames = TRUE
  )

  output$threshmeas <- renderPlot({
    par(mar = c(7, 3, 2, 1))
    modEvA::threshMeasures(obs = occ, pred = pred, thresh = thr(), ylim = c(-1, 1))
  })

  # output$similarity <- renderPlot({
  #   modEvA::similarity(obs = occ, pred = pred, thresh = thr())
  # })

}


# run app:
shinyApp(ui = ui, server = server)
