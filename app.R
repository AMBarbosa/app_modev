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
    radioButtons(inputId = "type", label = "", choices = c("basic", "full"), selected = "basic"),
    hr(),
    p("Basemaps (study area and variables) obtained with the 'geodata' R package and plotted with the 'terra' R package. Predictions obtained with the 'stats::glm' function. Model evaluation metrics computed and plotted with the 'modEvA' R package."),
    p("The R code behind this app is available on ", a(href = "https://github.com/AMBarbosa/modevapp", "GitHub"), ".")
  ),

  mainPanel(
    tabsetPanel(type = "tabs",

                tabPanel("Prediction maps",
                         column(width = 6,
                                # h4("Original predictions"),
                                plotOutput('map_pred_cont'),
                                p("Circles mark observed species presences. Pixel colours and values show model predictions.")
                         ),

                         column(width = 6,
                                # h4("Threshold-converted predictions"),
                                plotOutput('map_pred_01')
                         )
                ),

                tabPanel("Confusion plots",
                         column(width = 5,
                                # h4("Confusion matrix"),
                                tableOutput('confumat')
                         ),

                         column(width = 7,
                                # h4("Confusion map"),
                                plotOutput('confumap'),
                                p("ATTENTION! Section under construction, map categories not yet right!")
                         )
                ),

                tabPanel("Threshold-dependent metrics",
                         column(width = 8,
                                # h4("Metrics based on the confusion matrix"),
                                plotOutput('threshmeas')
                         ),

                         column(width = 4,
                                # h4("Similarity indices"),
                                plotOutput('simil')
                         )
                ),

                tabPanel("AUC",
                         column(width = 6,
                                # h4("ROC curve"),
                                plotOutput('ROC_curve')
                         ),

                         column(width = 6,
                                # h4("Precision-Recall curve"),
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
    terra::plot(pred, axes = FALSE, mar = c(0, 0, 2, 3), main = "Original predictions")
    terra::plot(ib, border = "brown", add = TRUE)
    points(occ, pch = 1, cex = 5, col = "grey30", lwd = 2)
    text(pred, pred_vals, cex = 0.7, halo = TRUE)
  })

  output$map_pred_01 <- renderPlot({
    pred_thr <- modEvA::applyThreshold(obs = occ, pred = pred, thresh = thr())
    terra::plot(pred_thr, axes = FALSE, mar = c(0, 0, 2, 3), legend = "bottomright", main = "Threshold-converted predictions")
    points(occ, pch = 1, cex = 5, col = "grey30", lwd = 2)
    terra::plot(ib, border = "brown", add = TRUE)
  })

  output$confumap <- renderPlot({
    confumap <- modEvA::confusionLabel(obs = occ, pred = pred, thresh = thr())
    terra::plot(confumap, col = c("orange", "lightblue", "red", "blue"), mar = c(0, 0, 2, 7), legend = FALSE, axes = FALSE, main = "Confusion map")
    # levels(confumap) <- data.frame(value = 1:4, type = c("TruePos", "FalsePos", "TrueNeg", "FalseNeg"), clr = c("orange", "lightblue", "red", "blue"))
    # # levels(confumap)
    # terra::plot(confumap, col = c("blue", "lightblue", "red", "orange"), mar = c(0, 0, 0, 7), legend = FALSE, axes = FALSE, clip = FALSE)
    legend(x = 4, y = 44, legend = c("TruePos", "FalsePos", "TrueNeg", "FalseNeg"), fil = c("blue", "lightblue", "red", "orange"), bty = "n", xpd = NA)
    plot(ib, border = "brown", lwd = 2, add = TRUE)
  })

  output$confumat <- renderTable({
    confumat <- modEvA::confusionMatrix(obs = occ, pred = pred, thresh = thr())
  },
  rownames = TRUE
  )

  output$threshmeas <- renderPlot({
    par(mar = c(7, 3, 3, 1))
    if (input$type == "basic") metrics <- c("Sensitivity", "Specificity", "CCR", "TSS", "kappa")
    if (input$type == "full") metrics <- modEvA::modEvAmethods("threshMeasures")[-grep("OddsRatio", modEvAmethods("threshMeasures"))]
    modEvA::threshMeasures(obs = occ, pred = pred, thresh = thr(), ylim = c(-1, 1), measures = metrics, standardize = input$type == "full", main = "Metrics based on\nthe confusion matrix")
  })

  output$simil <- renderPlot({
    par(mar = c(7, 3, 3, 1))
    if (input$type == "full") modEvA::similarity(obs = occ, pred = pred, thresh = thr(), ylim = c(-1, 1), main = "Similarity indices")
  })

  output$ROC_curve <- renderPlot({
    par(mar = c(5, 4.5, 2, 1))
    auc <- modEvA::AUC(obs = occ, pred = pred, main = "ROC curve")
    points(auc$thresholds[auc$thresholds$thresholds == thr(), c("false.pos.rate", "sensitivity")], pch = 20, cex = 3, col = "royalblue")
  })

  output$PR_curve <- renderPlot({
    if (input$type == "full") {
      par(mar = c(5, 4.5, 2, 1))
      pr <- modEvA::AUC(obs = occ, pred = pred, curve = "PR", main = "Precision-Recall curve")
      points(pr$thresholds[pr$thresholds$thresholds == thr(), c("sensitivity", "precision")], pch = 20, cex = 3, col = "royalblue")
    }
  })

}


# run app:
shinyApp(ui = ui, server = server)
