

# CFA Shiny App - Estructura inicial inspirada en EFAShiny
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(psych)
library(ggplot2)
library(readxl)
library(ggstats)
library(lavaan)
library(semPlot)
library(semTools)

ui <- navbarPage(
  title = "CFA Shiny",
  theme = shinytheme("flatly"),
  
  # 1. Introducción
  tabPanel("Introduction",
           fluidPage(
             h2("Welcome to CFAshiny !"),
             p("CFAshiny implements confirmatory factor analysis using the lavaan package."),
             tags$ul(
               tags$li("Support for unidimensional and bifactor models."),
               tags$li("Customizable model syntax and estimation method."),
               tags$li("Visual diagnostics via semPlot."),
               tags$li("Reliability analysis with omega."),
               tags$li("Optional validation with external variable.")
             )
           )
  ),
  
  # 2. Data Input
  tabPanel("Data Input",
           sidebarLayout(
             sidebarPanel(
               fileInput("file", "Upload .csv or .xlsx file"),
               checkboxInput("ordered", "Treat items as ordered (categorical)?", TRUE),
               actionButton("load", "Load Data")
             ),
             mainPanel(
               DTOutput("dataPreview")
             )
           )
  ),
  
  # 3. Item Analysis
  tabPanel("Item Analysis",
           sidebarLayout(
             sidebarPanel(
               h4("Settings for Likert Plot"),
               numericInput("widthLikert",  "Width (inches):",  8),
               numericInput("heightLikert", "Height (inches):", 6),
               numericInput("dpiLikert",    "Resolution (dpi):", 300)
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Descriptives", DTOutput("descTable")),
                 tabPanel("Likert Plot", plotOutput("likertPlot"))
               )
             )
           )
  ),
  
  # 4. Model Fit
  tabPanel("Model Fit",
           sidebarLayout(
             sidebarPanel(
               selectInput("estimator", "Estimator", choices = c("WLSMV", "ML", "MLR", "ULS"), selected = "WLSMV"),
               textAreaInput("modelText", "Lavaan Model Syntax", rows = 10),
               actionButton("runCFA", "Run CFA")
             ),
             mainPanel(
               verbatimTextOutput("summaryOut"),
               tableOutput("fitMeasures")
             )
           )
  ),
  
  # 5. Plot CFA
  tabPanel("Plot CFA",
           sidebarLayout(
             sidebarPanel(
               h4("Plot Options"),
               numericInput("sizeLat", "Latent size", 10),
               numericInput("sizeMan", "Manifest size", 8),
               numericInput("labelSize", "Label size", 0.7),
               textInput("bifactorName", "Bifactor name (if any)", ""),
               actionButton("plotCFA", "Generate Plot")
             ),
             mainPanel(
               plotOutput("cfaPlot")
             )
           )
  ),
  
  # 6. Reliability
  tabPanel("Reliability",
           mainPanel(
             tableOutput("omegaTable")
           )
  ),
  
  # 7. Validity
  tabPanel("Validity",
           sidebarLayout(
             sidebarPanel(
               selectInput("externalVar", "External variable for validation", choices = NULL),
               actionButton("runValid", "Run Validation")
             ),
             mainPanel(
               plotOutput("validityPlot")
             )
           )
  )
)

server <- function(input, output, session) {
  
  # Reactive para leer la base cargada
  datasetInput <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$datapath)
    if (tolower(ext) == "csv") {
      read.csv(input$file$datapath, header = TRUE)
    } else {
      read_excel(input$file$datapath)
    }
  })
  
  # Reactive para ajustar el modelo CFA
  cfaModel <- eventReactive(input$runCFA, {
    req(input$modelText)
    tryCatch({
      cfa(input$modelText,
          data = datasetInput(),
          estimator = input$estimator,
          ordered = if (input$ordered) TRUE else NULL)
    }, error = function(e) {
      showNotification(paste("Error in model:", e$message), type = "error")
      NULL
    })
  })
  
  # Vista previa de datos
  output$dataPreview <- DT::renderDT({
    req(datasetInput())
    DT::datatable(datasetInput(), options = list(scrollX = TRUE))
  })
  
  # Tabla descriptiva
  output$descTable <- DT::renderDT({
    req(datasetInput())
    descr <- psych::describe(datasetInput())
    DT::datatable(round(descr, 2), options = list(pageLength = 10))
  })
  
  # Plot Likert
  output$likertPlot <- renderPlot({
    req(datasetInput())
    ggstats::gglikert(datasetInput()) +
      labs(title = "Likert Response Distribution",
           x = "Percentage of responses",
           y = "Items") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })
  
  # Descargar gráfico likert
  output$download_likert <- downloadHandler(
    filename = function() { "likert_plot.png" },
    content = function(file) {
      g <- ggstats::gglikert(datasetInput()) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "bottom")
      ggsave(file, plot = g, width = input$likertWidth, height = input$likertHeight, dpi = input$likertDPI)
    }
  )
  
  # Summary del modelo
  output$summaryOut <- renderPrint({
    fit <- cfaModel()
    req(fit)
    summary(fit, fit.measures = TRUE, standardized = TRUE)
  })
  
  # Fit measures
  output$fitMeasures <- renderTable({
    fit <- cfaModel()
    req(fit)
    fm <- fitMeasures(fit, c("chisq", "df", "rmsea.scaled", "srmr", "cfi.scaled", "tli.scaled"))
    data.frame(Measure = names(fm), Value = round(as.numeric(fm), 3))
  })
  
  # Plot semPaths personalizado
  output$cfaPlot <- renderPlot({
    fit <- cfaModel()
    req(fit)
    semPlot::semPaths(
      fit,
      what = "std",
      weighted = FALSE,
      curve = 1.5,
      curvature = 0.6,
      rotation = 2,
      sizeMan = 8,
      sizeMan2 = 3,
      sizeLat = 10,
      residuals = FALSE,
      edge.label.cex = 0.85,
      edge.color = "#474747",
      intercepts = FALSE,
      thresholds = FALSE,
      label.cex = 0.7,
      mar = c(2, 3, 2, 3),
      style = "lisrel",
      label.prop = 1
    )
  })
  
  # Fiabilidad (omega)
  output$omegaTable <- renderTable({
    fit <- cfaModel()
    req(fit)
    semTools::reliability(fit, what = "omega")
  })
  
  # Validez convergente (correlación con otra variable)
  output$validityPlot <- renderPlot({
    req(datasetInput(), input$externalVar)
    fit <- cfaModel()
    req(fit)
    scores <- lavPredict(fit)
    df <- datasetInput()
    df$factorScore <- scores[, 1]  # simplificado para primer factor
    ggplot(df, aes_string(x = "factorScore", y = input$externalVar)) +
      geom_point() +
      geom_smooth(method = "lm") +
      labs(title = paste("Validity Evidence: Factor vs", input$externalVar),
           x = "Factor Score", y = input$externalVar)
  })
  
} 

shinyApp(ui, server)



