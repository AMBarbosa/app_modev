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
    p("Basemaps (study area and variables) obtained with the 'geodata' R package and plotted with the 'terra' R package. Predictions obtained with the 'stats::glm' function. Model evaluation metrics computed and plotted with the 'modEvA' R package.")
  ),

  mainPanel(
    tabsetPanel(type = "tabs",

                tabPanel("Prediction maps",
                         column(width = 6,
                                h4("Original predictions"),
                                plotOutput('map_pred_cont'),
                                p("Circles mark observed species presences. Pixel colours represent model predictions.")
                         ),

                         column(width = 6,
                                h4("Binarized predictions"),
                                plotOutput('map_pred_01')
                         )
                ),

                tabPanel("Confusion plots",
                         column(width = 5,
                                h4("Confusion matrix"),
                                tableOutput('confumat')
                         ),

                         column(width = 7,
                                h4("Confusion map"),
                                plotOutput('confumap')
                         )
                ),

                tabPanel("Threshold-based metrics",
                         column(width = 8,
                                h4("Metrics based on the confusion matrix"),
                                plotOutput('threshmeas')
                         ),

                         column(width = 4,
                                h4("Similarity indices"),
                                plotOutput('simil')
                         )
                ),

                tabPanel("AUC",
                         column(width = 6,
                                h4("ROC curve"),
                                plotOutput('ROC_curve')
                         ),

                         column(width = 6,
                                h4("Precision-Recall curve"),
                                plotOutput('PR_curve')
                         )
                )
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
    points(occ, pch = 1, cex = 5, col = "grey30", lwd = 2)
    text(pred, pred_vals, cex = 0.7, halo = TRUE, clip = FALSE)
  })

  output$map_pred_01 <- renderPlot({
    pred_thr <- modEvA::applyThreshold(obs = occ, pred = pred, thresh = thr(), verbosity = 0)
    plot(pred_thr, axes = FALSE, mar = c(0, 0, 0, 3), legend = "bottomright", clip = FALSE)
    points(occ, pch = 1, cex = 5, col = "grey30", lwd = 2)
    plot(ib, border = "brown", add = TRUE)
  })

  output$confumap <- renderPlot({
    confumap <- modEvA::confusionLabel(obs = occ, pred = pred, thresh = thr(), verbosity = 0)
    plot(confumap, col = c("orange", "lightblue", "red", "blue"), mar = c(0, 0, 0, 7), legend = FALSE, axes = FALSE, clip = FALSE)
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

  output$simil <- renderPlot({
    par(mar = c(7, 3, 2, 1))
    modEvA::similarity(obs = occ, pred = pred, thresh = thr(), ylim = c(-1, 1))
  })

  output$ROC_curve <- renderPlot({
    par(mar = c(3, 3, 2, 1))
    auc <- modEvA::AUC(obs = occ, pred = pred)
    points(auc$thresholds[auc$thresholds$thresholds == thr(), c("false.pos.rate", "sensitivity")], pch = 20, cex = 3, col = "royalblue")
  })

  output$PR_curve <- renderPlot({
    par(mar = c(3, 3, 2, 1))
    pr <- modEvA::AUC(obs = occ, pred = pred, curve = "PR")
    points(pr$thresholds[pr$thresholds$thresholds == thr(), c("sensitivity", "precision")], pch = 20, cex = 3, col = "royalblue")
  })

}


# run app:
shinyApp(ui = ui, server = server)
