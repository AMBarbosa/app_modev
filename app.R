# by A. Marcia Barbosa
# https://modtools.wordpress.com
# licence: Creative Commons Attribution-ShareAlike (CC BY-SA)
# updated Jan 17, 2024

# RStudio: Session -> Set Working Directory -> To Source File Location
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# load required packages ####

library(terra)
library(modEvA)
library(shiny)


# user interface ####

ui <- pageWithSidebar(
  headerPanel('Model evaluation with threshold-based metrics'),

  sidebarPanel(
    width = 3,

    sliderInput(inputId = "threshold",
                label = "Threshold:",
                min = 0,
                max = 1,
                value = 0.5,
                step = 0.1),

    radioButtons(inputId = "metrics", label = "Metrics set:", choices = c("basic", "ample"), selected = "basic"),

    hr(),  # (barely visible) horizontal line
    # p(""),  # space between lines

    span("Basemaps (study region and variables) obtained with the"),
    strong("geodata"),
    span("R package and processed with the"),
    strong("terra"),
    span("R package. Model evaluation metrics computed and plotted with the"),
    strong("modEvA"),
    span("R package."),

    p(""),  # space between lines
    p("By A. Márcia Barbosa (", a(href = "https://modtools.wordpress.com", "https://modtools.wordpress.com", .noWS = "outside"), "). R code available on ", a(href = "https://github.com/AMBarbosa/modevapp", "GitHub"), " under a Creative Commons Attribution-ShareAlike (CC BY-SA) licence.")
  ),

  mainPanel(
    tabsetPanel(type = "tabs",

                tabPanel("Prediction maps",
                         fluidRow(
                           column(width = 6,
                                  # h4("Original predictions"),
                                  plotOutput('map_pred_cont')
                           ),

                           column(width = 6,
                                  # h4("Threshold-converted predictions"),
                                  plotOutput('map_pred_01')
                           )
                         ),

                         fluidRow(
                           p("Circles show species observation records. Colours and values show model predictions.", style = "text-align: center;")
                         )
                ),

                tabPanel("Confusion plots",
                         column(width = 6,
                                HTML("<br><br><br><br>"),
                                # h4("Confusion matrix"),
                                # tableOutput('confumat')
                                plotOutput('confumat')
                         ),

                         column(width = 6,
                                # h4("Confusion map"),
                                plotOutput('confumap'),
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
                         fluidRow(
                           column(width = 6,
                                  # h4("ROC curve"),
                                  plotOutput('ROC_curve')
                           ),

                           column(width = 6,
                                  # h4("Precision-Recall curve"),
                                  plotOutput('PR_curve')
                           )
                         ),

                         fluidRow(
                           h4("Curve values"),
                           div(tableOutput("AUC_table"), style = "font-size:80%")
                         )
                )
    )
  )
)


# server actions ####

server <- function(input, output) {

  ib <- terra::vect("data/ib.gpkg")
  occ <- read.csv("data/occ.csv")
  pred <- terra::rast("data/pred.tif")
  grid <- terra::as.polygons(pred, aggregate = FALSE)
  pred <- round(pred, 3)
  pred_vals <- as.character(terra::values(pred))
  pred_vals[pred_vals == "NaN"] <- ""

  thr <- reactive({
    input$threshold
  })

  output$map_pred_cont <- renderPlot({
    terra::plot(pred, axes = FALSE, mar = c(0, 0, 2, 3), main = "Original predictions", font.main = 1, cex.main = 1.5)
    terra::plot(grid, lwd = 0.1, add = TRUE)
    terra::plot(ib, border = "brown", add = TRUE)
    points(occ, pch = 1, cex = 5, lwd = 1.5)
    text(pred, pred_vals, cex = 0.7, halo = TRUE)
  })

  output$map_pred_01 <- renderPlot({
    pred_thr <- modEvA::applyThreshold(obs = occ, pred = pred, thresh = thr())
    terra::plot(pred_thr, legend = FALSE, axes = FALSE, mar = c(0, 0, 2, 3), main = "Threshold-converted predictions", font.main = 1, cex.main = 1.4)
    points(occ, pch = 1, cex = 5, lwd = 1.5)
    terra::plot(grid, lwd = 0.1, add = TRUE)
    terra::plot(ib, border = "brown", add = TRUE)
  })


  # output$confumat <- renderTable({
  #   confumat <- confusionMatrix(obs = occ, pred = pred, thresh = thr())
  # },
  # rownames = TRUE, striped = TRUE, hover = TRUE, bordered = TRUE
  # )

  output$confumat <- renderPlot({
    modEvA::confusionMatrix(obs = occ, pred = pred, thresh = thr(), plot = TRUE, classes = TRUE, main = "Confusion matrix", font.main = 1, cex.main = 1.45)
  },
  width = 300, height = 300
  )

  output$confumap <- renderPlot({

    # modEvA::confusionLabel(obs = occ, pred = pred, thresh = thr(), plot = TRUE, mar = c(0, 0, 2, 2), axes = FALSE, legend = "bottomright", main = "Confusion map", font.main = 1, cex.main = 1.4)

    # set colours of raster categories (though this is supposedly already done in the 'confusionLabel' function...):
    confumap <- modEvA::confusionLabel(obs = occ, pred = pred, thresh = thr(), plot = FALSE)
    colr_table <- data.frame(lev = as.factor(c("TruePos", "FalsePos", "TrueNeg", "FalseNeg")), col = c("blue", "lightblue", "red", "orange"))
    existing_levs <- colr_table$lev %in% levels(confumap)[[1]]$cat
    terra::coltab(confumap) <- data.frame(
      values = droplevels(colr_table$lev[existing_levs]),
      cols = colr_table$col[existing_levs])
    terra::plot(confumap, mar = c(0, 0, 2, 2), axes = FALSE, legend = "bottomright", main = "Confusion map", font.main = 1, cex.main = 1.4)  # this is now done by the function, with new arguments 'plot' and '...'

    terra::plot(grid, lwd = 0.1, add = TRUE)
    terra::plot(ib, border = "brown", add = TRUE)
  })


  output$threshmeas <- renderPlot({
    par(mar = c(7, 3, 3, 1))
    if (input$metrics == "basic") metrics <- c("Sensitivity", "Specificity", "CCR", "TSS", "kappa")
    if (input$metrics == "ample") metrics <- modEvA::modEvAmethods("threshMeasures")[-grep("OddsRatio", modEvA::modEvAmethods("threshMeasures"))]
    modEvA::threshMeasures(obs = occ, pred = pred, thresh = thr(), ylim = c(-1, 1), measures = metrics, standardize = input$metrics == "ample", main = "Metrics based on the confusion matrix", font.main = 1, cex.main = 1.4)
  })

  output$simil <- renderPlot({
    par(mar = c(7, 3, 3, 1))
    if (input$metrics == "ample") modEvA::similarity(obs = occ, pred = pred, thresh = thr(), ylim = c(-1, 1), main = "Similarity indices", font.main = 1, cex.main = 1.4)
  })


  output$ROC_curve <- renderPlot({
    par(mar = c(5, 4.5, 2, 1))
    auc <- modEvA::AUC(obs = occ, pred = pred, interval = 0.1, main = "ROC curve", font.main = 1, cex.main = 1.4, grid = TRUE, grid.lty = 3)  # , ticks = input$metrics == "ample", plot.preds = input$metrics == "ample"
    points(auc$thresholds[auc$thresholds$thresholds == thr(), c("false.pos.rate", "sensitivity")], pch = 20, cex = 3, col = "darkturquoise")
  })

  output$AUC_table <- renderTable({
    auc <- modEvA::AUC(obs = occ, pred = pred, interval = 0.1, plot = FALSE)  # apparently 'auc' needs to be created again here, otherwise error
    if (input$metrics == "basic") data.frame(threshold = round(auc$thresholds$thresholds, 1), N_preds = as.integer(auc$thresholds$n.preds), auc$thresholds[ , c("sensitivity", "specificity")])
    else if (input$metrics == "ample") data.frame(threshold = round(auc$thresholds$thresholds, 1), N_preds = as.integer(auc$thresholds$n.preds), auc$thresholds[ , c("sensitivity", "specificity", "precision")])
  },
  rownames = FALSE, striped = TRUE, hover = TRUE, bordered = TRUE
  )

  output$PR_curve <- renderPlot({
    if (input$metrics == "ample") {
      par(mar = c(5, 4.5, 2, 1))
      pr <- modEvA::AUC(obs = occ, pred = pred, interval = 0.1, curve = "PR", main = "Precision-Recall curve", font.main = 1, cex.main = 1.4, grid = TRUE, grid.lty = 3)  # , ticks = input$metrics == "ample", plot.preds = input$metrics == "ample"
      points(pr$thresholds[pr$thresholds$thresholds == thr(), c("sensitivity", "precision")], pch = 20, cex = 3, col = "darkturquoise")
    }
  })

}


# run app ####

shinyApp(ui = ui, server = server)
