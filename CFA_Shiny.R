

# CFA Shiny App 
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(psych)
library(ggplot2)
library(readxl)
library(openxlsx)
library(ggstats)
library(lavaan)
library(semPlot)
library(semTools)
library(BifactorIndicesCalculator)
library(PsyMetricTools)
library(ThesiStats)
library(MVN)
library(dplyr)
library(ggpubr)
library(labelled)
library(wesanderson)
library(purrr)
library(reshape2)
library(gridExtra)
library(gtable)
library(tidyr)

ui <- navbarPage(
  title = "CFA Shiny",
  theme = shinytheme("flatly"),
  
  # 1. Introducción
  tabPanel("Introducción",
           fluidPage(
             h2(HTML("👋 <strong>¡Bienvenido a CFA-Shiny!</strong>")),
             
             p(HTML('<strong>CFA-Shiny</strong> es una aplicación Shiny diseñada para realizar <strong>Análisis Factorial Confirmatorio (CFA)</strong> utilizando el paquete <code>lavaan</code>. Esta herramienta permite especificar, visualizar y validar modelos de medida de manera flexible.')),
             
             # Funcionalidades
             fluidRow(
               box(
                 title = "¿Qué puedes hacer con CFA-Shiny?", width = 12, status = "primary", solidHeader = TRUE,
                 HTML('
          <ul>
            <li><strong>Importación de datos:</strong> Importa archivos en formato <code>.csv</code> o <code>.xlsx</code> fácilmente.</li>
            <li><strong>Análisis de ítems:</strong> Obtén estadísticas descriptivas, visualiza la distribución de respuestas con gráficos tipo Likert y analiza la normalidad.</li>
            <li><strong>Especificación de modelos:</strong> Define tu modelo CFA usando sintaxis de <code>lavaan</code>.</li>
            <li><strong>Ajuste del modelo:</strong> Obtén estimaciones estandarizadas, índices de ajuste e índices de modificación.</li>
            <li><strong>Visualización del modelo:</strong> Visualiza tu modelo CFA con <code>semPlot</code>.</li>
            <li><strong>Fiabilidad:</strong> Calcula la confiabilidad por consistencia interna con el coeficiente <strong>Omega</strong>. Para modelos bifactor, se calculan índices específicos.</li>
            <li><strong>Evaluación bootstrap:</strong> Evalúa la estabilidad de los índices de ajuste mediante remuestreo.</li>
            <li><strong>Validez convergente:</strong> Analiza relaciones con variables externas.</li>
          </ul>
        ')
               )
             ),
             
             # Recomendaciones
             fluidRow(
               box(
                 title = "Recomendaciones importantes", width = 12, status = "warning", solidHeader = TRUE,
                 HTML('
          <ul>
            <li style="color:#a94442; font-weight:bold;">
              ⚠️ Para analizar modelos con ítems ordinales (por ejemplo, escalas tipo Likert), debes especificar que los ítems son "ordenados" y seleccionar un estimador adecuado como <code>WLSMV</code>.
            </li>
            <li><strong>Asegúrate de indicar el prefijo común de los ítems de la escala que vas a analizar</strong> (por ejemplo, <code>BFI</code>). Este prefijo permite identificar correctamente las variables de interés.</li>
            <li><strong>La base de datos que vas a incorporar debe contener solo ítems</strong>, tanto de la escala que vas a analizar como de la variable externa para evaluar la validez convergente.</li>
          </ul>
        ')
               )
             )
           )
  ),
  
  
  # 2. Data Input
  tabPanel("Data Input",
           sidebarLayout(
             sidebarPanel(
               fileInput("file", "Upload .csv or .xlsx file"),
               checkboxInput("ordered", "¿Los ítems son ordinales?", TRUE),
               textInput("prefixItems", "Prefijo del ítem", ""),
               actionButton("load", "Load Data")
             ),
             mainPanel(
               DTOutput("dataPreview")
             )
           )
  ),
  
  # 3. Item Analysis
  tabPanel("Item Analysis",
           tabsetPanel(
             
             tabPanel("Descriptives", 
                      box(title = "Descriptive Statistics", width = 12, status = "info", solidHeader = TRUE,
                          DT::dataTableOutput("descTable"),
                          br(),
                          downloadBttn("download_descriptivos", "Download Descriptive Table", style = "jelly", color = "primary")
                      )
             ),
             
             tabPanel("Likert Plot", 
                      fluidRow(
                        column(
                          width = 8,
                          box(title = "Likert Response Plot", width = 12, status = "primary", solidHeader = TRUE,
                              plotOutput("likertPlot", height = "600px")
                          )
                        ),
                        column(
                          width = 4,
                          box(title = "Opciones de descarga", width = 12, status = "info", solidHeader = TRUE,
                              h5("Download Settings"),
                              numericInput("likertWidth",  "Width (inches):",  8),
                              numericInput("likertHeight", "Height (inches):", 6),
                              numericInput("likertDPI",    "Resolution (dpi):", 300),
                              downloadBttn("download_likert", "Download Likert Plot", style = "jelly", color = "success")
                          )
                        )
                      )
             ),
             
             tabPanel("Multivariate Normality", 
                      fluidRow(
                        column(
                          width = 4,
                          box(title = "Mardia Multivariate Test", width = 12, status = "warning", solidHeader = TRUE,
                              verbatimTextOutput("mardiaTest"),
                              numericInput("xmin", "X min:", 30),
                              numericInput("xmax", "X max:", 40),
                              numericInput("ymin", "Y min:", 5),
                              numericInput("ymax", "Y max:", 10)
                          )
                        ),
                        column(
                          width = 8,
                          box(title = "Mardia Q-Q Plot", width = 12, status = "primary", solidHeader = TRUE,
                              plotOutput("mardiaPlot", height = "500px")
                          )
                        )
                      )
             ),
             
             tabPanel("Univariate Normality", 
                      fluidRow(
                        column(
                          width = 6,
                          box(title = "QQ Plot por ítem", width = 12, status = "primary", solidHeader = TRUE,
                              plotOutput("qqPlot", height = "800px")
                          )
                        ),
                        column(
                          width = 6,
                          box(title = "Histograma por ítem", width = 12, status = "success", solidHeader = TRUE,
                              plotOutput("histPlot", height = "800px")
                          )
                        )
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
               tabsetPanel(
                 tabPanel("Summary", verbatimTextOutput("summaryOut")),
                 tabPanel("Fit Measures",
                          tableOutput("fitMeasures"),
                          downloadButton("download_fitmeasures", "Download Fit Table")
                 ),
                 tabPanel("Modification Indices", DTOutput("modIndices")),
                 tabPanel("Bifactor Indices",
                          fluidPage(
                            helpText("Se muestran los índices del modelo bifactor si cumple con los criterios: cada ítem carga en un factor general y uno específico."),
                            textInput("bifactorName", "Nombre del factor general", value = "FG"),
                            br(),
                            tabsetPanel(
                              tabPanel("Índices Globales",
                                       tableOutput("bifactorIndices")
                              ),
                              tabPanel("Nivel de Factores",
                                       tableOutput("bifactorFactors")
                              ),
                              tabPanel("Nivel de Ítems",
                                       tableOutput("bifactorItems")
                              ),
                              tabPanel("Guía de interpretación",
                                       uiOutput("bifactorPuntosCorte")  # NUEVO
                              )
                            )
                          )
                 )
               )
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
               numericInput("sizeMan2", "Manifest height", 3),
               numericInput("labelSize", "Label size", 0.7),
               numericInput("curve", "Curvature (curve)", value = 2, min = 0, max = 10, step = 0.1),
               textInput("bifactorName", "Bifactor name (if any)", ""),
               actionButton("plotButton", "Generate Plot")
             ),
             mainPanel(
               plotOutput("cfaPlot"),
               hr(),
               h4("Download Settings"),
               numericInput("plot_width", "Plot Width (inches):", 10),
               numericInput("plot_height", "Plot Height (inches):", 6),
               numericInput("plot_dpi", "Resolution (dpi):", 300),
               downloadBttn("download_cfa_plot", "Download Plot", style = "jelly", color = "success")
             )
           )
  ),
  
  # 6. Reliability
  tabPanel("Reliability",
           tabsetPanel(
             tabPanel("Omega", 
                      tableOutput("omegaTable")
             )
             
           )
  ),
  
  # 7. Bootstrapp
  tabPanel("Bootstrap Fit",
           sidebarLayout(
             sidebarPanel(
               numericInput("n_boot", "Number of Bootstrap Replications", 10),
               selectInput("boot_estimator", "Estimator", choices = c("WLSMV", "ML", "MLR", "ULS"), selected = "WLSMV"),
               numericInput("omega_ymin", "Omega CI Min", 0.87),
               numericInput("omega_ymax", "Omega CI Max", 0.92),
               numericInput("comp_ymin", "Composite CI Min", 0.98),
               numericInput("comp_ymax", "Composite CI Max", 1),
               numericInput("abs_ymin", "Abs Fit CI Min", 0.00),
               numericInput("abs_ymax", "Abs Fit CI Max", 0.02),
               actionButton("run_boot", "Run Bootstrap")
             ),
             mainPanel(
               plotOutput("bootstrapPlot"),
               hr(),
               h4("Download Settings"),
               numericInput("boot_plot_width", "Plot Width (inches)", 10),
               numericInput("boot_plot_height", "Plot Height (inches)", 6),
               numericInput("boot_plot_dpi", "Plot Resolution (dpi)", 300),
               downloadBttn("download_boot_plot", "Download Plot", style = "jelly", color = "primary")
             )
           )
  ),
  
  
  # 8. Validez basada en la relación con otra variable
  tabPanel("Convergent Validity",
           sidebarLayout(
             sidebarPanel(
               textAreaInput("validityModelText", "Lavaan Model for Validation", rows = 8),
               numericInput("valid_sizeLat", "Latent size", 10),
               numericInput("valid_sizeMan", "Manifest size", 8),
               numericInput("valid_sizeMan2", "Manifest height", 3),
               numericInput("valid_labelSize", "Label size", 0.7),
               numericInput("curve", "Curvature (curve)", value = 2, min = 0, max = 10, step = 0.1),
               textInput("valid_bifactorName", "Bifactor name (if any)", ""),
               actionButton("runValid", "Run Validation")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot", 
                          plotOutput("validityPlot"),
                          hr(),
                          h4("Download Settings"),
                          numericInput("valid_plot_width", "Plot Width (inches):", 10),
                          numericInput("valid_plot_height", "Plot Height (inches):", 6),
                          numericInput("valid_plot_dpi", "Resolution (dpi):", 300),
                          downloadBttn("download_valid_plot", "Download Plot", style = "jelly", color = "success")),
                 tabPanel("Fit Measures", tableOutput("validityFit")),
                 tabPanel("Modification Indices", DT::DTOutput("validityMI"))
               )
             )
           )
  ),
  
  #References
  tabPanel("References",
           fluidPage(
             fluidRow(
               column(
                 width = 12,
                 box(
                   title = "How to Cite CFA-Shiny", width = 12, status = "success", solidHeader = TRUE,
                   HTML('
            <ul>
              <li>Lino-Cruz, C. & Ventura-León, J. (2025). <em>CFA-Shiny: Aplicación interactiva en R para Análisis Factorial Confirmatorio</em> [Software]. GitHub. 
      <a href="https://github.com/CristopherLino/CFA-Shiny" target="_blank">https://github.com/CristopherLino/CFA-Shiny</a></li>
            </ul>
            
          ')
                 )
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 box(
                   title = "Recommended References", width = 12, status = "info", solidHeader = TRUE,
                   HTML('
            <ul>
              <li>Abad, F. J., Olea, J., Ponsoda, V., & García, C. (2011). <em>Medición en ciencias sociales y de la salud</em>. Madrid: Síntesis.</li>
              <li>Brown, T. A. (2015). <em>Confirmatory factor analysis for applied research</em> (2nd ed.). The Guilford Press.</li>
              <li>Hu, L.-t., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis: Conventional criteria versus new alternatives. 
                    <em>Structural Equation Modeling, 6</em>(1), 1–55. 
                    <a href="https://doi.org/10.1080/10705519909540118" target="_blank">https://doi.org/10.1080/10705519909540118</a></li>
                <li>Dominguez-Lara, S., & Rodriguez, A. (2017). Índices estadísticos de modelos bifactor. 
                    <em>Interacciones, 3</em>(2), 59–65. 
                    <a href="https://doi.org/10.24016/2017.v3n2.51" target="_blank">https://doi.org/10.24016/2017.v3n2.51</a></li>
                <li>Rodriguez, A., Reise, S. P., & Haviland, M. G. (2016). Evaluating bifactor models: Calculating and interpreting statistical indices. 
                    <em>Psychological Methods, 21</em>(2), 137–150. 
                    <a href="https://doi.org/10.1037/met0000045" target="_blank">https://doi.org/10.1037/met0000045</a></li>
                <li>Reise, S. P. (2012). The rediscovery of bifactor measurement models. 
                    <em>Multivariate Behavioral Research, 47</em>(5), 667–696. 
                    <a href="https://doi.org/10.1080/00273171.2012.715555" target="_blank">https://doi.org/10.1080/00273171.2012.715555</a></li>
                <li>Shi, D., Maydeu-Olivares, A., & Rosseel, Y. (2019). Assessing fit in ordinal factor analysis models: SRMR vs. RMSEA. 
                    <em>Structural Equation Modeling, 26</em>(3), 431–439. 
                    <a href="https://doi.org/10.1080/10705511.2019.1611434" target="_blank">https://doi.org/10.1080/10705511.2019.1611434</a></li>

            </ul>
          ')
                 )
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 box(
                   title = "R Packages Used", width = 12, status = "warning", solidHeader = TRUE,
                   HTML('
            <ul>
              <li>Epskamp, S. (2022). semPlot: Path Diagrams and Visual Analysis of Various SEM Packages Output. 
                    <a href="https://cran.r-project.org/package=semPlot" target="_blank">CRAN</a></li>
                <li>Jorgensen, T. D., Pornprasertmanit, S., Schoemann, A. M., & Rosseel, Y. (2025). semTools: Useful tools for structural equation modeling. 
                    <a href="https://cran.r-project.org/package=semTools" target="_blank">CRAN</a></li>
                <li>Chang, W., Cheng, J., Allaire, J. J., Xie, Y., & McPherson, J. (2023). shiny: Web Application Framework for R. RStudio. 
                    <a href="https://cran.r-project.org/package=shiny" target="_blank">CRAN</a></li>
                <li>Ventura-León, J. (2024). <em>PsyMetricTools</em> [Software]. GitHub. 
                    <a href="https://github.com/jventural/PsyMetricTools" target="_blank">https://github.com/jventural/PsyMetricTools</a></li>
                <li>Ventura-León, J. (2024). <em>ThesiStats</em> [Software]. GitHub. 
                    <a href="https://github.com/jventural/ThesiStats" target="_blank">https://github.com/jventural/ThesiStats</a></li>
                    <li>Rosseel, Y. (2012). lavaan: Latent Variable Analysis. <em>Journal of Statistical Software, 48</em>(2), 1–36. 
                    <a href="https://doi.org/10.18637/jss.v048.i02" target="_blank">https://doi.org/10.18637/jss.v048.i02</a></li>
            </ul>
          ')
                 )
               )
             )
           )
  )
  
)

server <- function(input, output, session) {
  
  # Leer la base cargada
  full_data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$datapath)
    if (tolower(ext) == "csv") {
      read.csv(input$file$datapath, header = TRUE)
    } else {
      read_excel(input$file$datapath)
    }
  })
  
  # Variables externas para validez convergente
  observe({
    req(full_data(), input$prefixItems)
    
    all_vars <- names(full_data())
    main_items <- grep(paste0("^", input$prefixItems), all_vars, value = TRUE)
    external_vars <- setdiff(all_vars, main_items)
    
    updateSelectInput(session, "externalVar",
                      choices = external_vars,
                      selected = if (length(external_vars) > 0) external_vars[1] else NULL)
  })
  
  # Filtrar solo los ítems de la escala principal
  filtered_data <- reactive({
    df <- full_data()
    prefix <- input$prefixItems
    df[, grep(paste0("^", prefix), names(df))]
  })
  
  # Datos para validez convergente (los que no son del prefijo)
  validation_data <- reactive({
    df <- full_data()
    prefix <- input$prefixItems
    df[, -grep(paste0("^", prefix), names(df))]
  })
  
  # Tabla descriptiva
  output$descTable <- renderDT({
    req(filtered_data())
    descr <- describe(filtered_data())
    datatable(round(descr, 2), options = list(pageLength = 10))
  })
  
  # Descargar tabla despriptiva
  output$download_descriptivos <- downloadHandler(
    filename = function() { "descriptive_stats.xlsx" },
    content = function(file) {
      descr <- describe(filtered_data())
      openxlsx::write.xlsx(round(descr, 2), file)
    }
  )
  
  # Plot Likert
  output$likertPlot <- renderPlot({
    req(filtered_data())
    gglikert(filtered_data()) +
      labs(title = "",
           x = "Percentage of responses",
           y = "Items") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })
  
  # Descargar gráfico likert
  output$download_likert <- downloadHandler(
    filename = function() { "likert_plot.jpg" },
    content = function(file) {
      g <- ggstats::gglikert(filtered_data()) +  # <- corrección aquí
        theme_minimal(base_size = 12) +
        labs(title = "", x = "Percentage", y = "Items") +
        theme(legend.position = "bottom")
      
      ggsave(file, plot = g, width = input$likertWidth, height = input$likertHeight, dpi = input$likertDPI)
    }
  )
  
  # Normalidad multivariada (ThesiStats)
  output$mardiaTest <- renderPrint({
    req(filtered_data())
    df <- filtered_data() %>%
      dplyr::mutate(across(everything(), ~ as.numeric(as.character(.)))) %>%
      na.omit()
    ThesiStats::mardia_test(df)
  })
  
  output$mardiaPlot <- renderPlot({
    req(filtered_data(), input$xmin, input$xmax, input$ymin, input$ymax)
    df <- filtered_data() %>%
      dplyr::mutate(across(everything(), ~ as.numeric(as.character(.)))) %>%
      na.omit()
    print(ThesiStats::Multivariate_plot(
      df,
      xmin = input$xmin,
      xmax = input$xmax,
      ymin = input$ymin,
      ymax = input$ymax
    ))
  })
  
  
  # Normalidad univariada (MVN)
  output$qqPlot <- renderPlot({
    req(filtered_data())
    df <- filtered_data() %>%
      dplyr::mutate(across(everything(), ~ as.numeric(as.character(.)))) %>%
      na.omit()
    result <- MVN::mvn(data = df, mvnTest = "mardia", univariatePlot = "qqplot")
    print(result$univariatePlots)
  })
  
  output$histPlot <- renderPlot({
    req(filtered_data())
    df <- filtered_data() %>%
      dplyr::mutate(across(everything(), ~ as.numeric(as.character(.)))) %>%
      na.omit()
    result <- MVN::mvn(data = df, mvnTest = "mardia", univariatePlot = "histogram")
    print(result$univariatePlots)
  })
  
  # Ajustar modelo CFA con datos filtrados
  cfaModel <- eventReactive(input$runCFA, {
    req(input$modelText)
    tryCatch({
      cfa(input$modelText,
          data = filtered_data(),
          estimator = input$estimator,
          ordered = if (input$ordered) TRUE else NULL)
    }, error = function(e) {
      showNotification(paste("Error in model:", e$message), type = "error")
      NULL
    })
  })
  
  # Vista previa de datos
  output$dataPreview <- DT::renderDT({
    req(full_data())
    datatable(full_data(), options = list(scrollX = TRUE))
  })
  
  
  # Summary del modelo
  output$summaryOut <- renderPrint({
    fit <- cfaModel()
    req(fit)
    summary(fit, fit.measures = TRUE, standardized = TRUE)
  })
  
  # Fit measures según estimador
  output$fitMeasures <- renderTable({
    fit <- cfaModel()
    req(fit)
    
    indices <- switch(input$estimator,
                      "ML"    = c("chisq", "df", "rmsea", "srmr", "cfi", "tli"),
                      "MLR"   = c("chisq.scaled", "df.scaled", "rmsea.scaled", "srmr", "cfi.scaled", "tli.scaled"),
                      "WLSMV" = c("chisq.scaled", "df.scaled", "rmsea.scaled", "srmr", "cfi.scaled", "tli.scaled"),
                      "ULS"   = c("chisq", "df", "rmsea", "srmr", "cfi", "tli"))
    
    pretty_names <- c(
      "chisq" = "\u03C7\u00B2",
      "chisq.scaled" = "\u03C7\u00B2",
      "df" = "gl",
      "df.scaled" = "gl",
      "rmsea" = "RMSEA",
      "rmsea.scaled" = "RMSEA",
      "srmr" = "SRMR",
      "cfi" = "CFI",
      "cfi.scaled" = "CFI",
      "tli" = "TLI",
      "tli.scaled" = "TLI"
    )
    
    fm <- fitMeasures(fit, indices)
    labels <- pretty_names[indices]
    labels[is.na(labels)] <- indices[is.na(labels)]
    
    data.frame(
      `Fit Index` = labels,
      `Value` = round(as.numeric(fm), 3)
    )
  })
  
  
  # Descargar tabla
  output$download_fitmeasures <- downloadHandler(
    filename = function() {
      paste0("fit_measures_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      fit <- cfaModel()
      req(fit)
      
      indices <- switch(input$estimator,
                        "ML"    = c("chisq", "df", "rmsea", "srmr", "cfi", "tli"),
                        "MLR"   = c("chisq.scaled", "df.scaled", "rmsea.scaled", "srmr", "cfi.scaled", "tli.scaled"),
                        "WLSMV" = c("chisq.scaled", "df.scaled", "rmsea.scaled", "srmr", "cfi.scaled", "tli.scaled"),
                        "ULS"   = c("chisq", "df", "rmsea", "srmr", "cfi", "tli"))
      
      pretty_names <- c(
        "chisq" = "\u03C7\u00B2",
        "chisq.scaled" = "\u03C7\u00B2",
        "df" = "gl",
        "df.scaled" = "gl",
        "rmsea" = "RMSEA",
        "rmsea.scaled" = "RMSEA",
        "srmr" = "SRMR",
        "cfi" = "CFI",
        "cfi.scaled" = "CFI",
        "tli" = "TLI",
        "tli.scaled" = "TLI"
      )
      
      fm <- fitMeasures(fit, indices)
      labels <- pretty_names[indices]
      labels[is.na(labels)] <- indices[is.na(labels)]
      
      tabla <- data.frame(
        `Fit Index` = labels,
        `Value` = round(as.numeric(fm), 3)
      )
      
      openxlsx::write.xlsx(tabla, file)
    }
  )
  
  
  # Índices de modificación
  output$modIndices <- DT::renderDT({
    fit <- cfaModel()
    req(fit)
    modindices(fit, minimum.value = 10, sort = TRUE)
  })
  
  
  # Plot CFA
  plotCFA <- eventReactive(input$plotButton, {
    fit <- cfaModel()
    req(fit)
    bifactor <- input$bifactorName
    layout_type <- ifelse(bifactor != "", "tree3", "tree2")
    
    semPaths(
      fit,
      what = "std",
      weighted = FALSE,
      layout = layout_type,
      bifactor = if (bifactor != "") bifactor else NULL,
      rotation = 2,
      sizeLat = input$sizeLat,
      sizeMan = input$sizeMan,
      sizeMan2 = input$sizeMan2,
      curve = input$curve,
      residuals = FALSE,
      edge.label.cex = input$labelSize,
      edge.color = "#474747",
      intercepts = FALSE,
      thresholds = FALSE,
      label.cex = input$labelSize,
      mar = c(2, 7, 2, 7),
      style = "lisrel",
      label.prop = 1
    )
  })
  
  output$cfaPlot <- renderPlot({
    req(plotCFA())
    plotCFA()
  })
  
  output$download_cfa_plot <- downloadHandler(
    filename = function() { "cfa_plot.jpg" },
    content = function(file) {
      req(cfaModel())
      
      jpeg(filename = file, 
           width = input$plot_width,
           height = input$plot_height,
           units = "in",
           res = input$plot_dpi)
      
      semPlot::semPaths(
        cfaModel(),
        what = "std",
        weighted = FALSE,
        layout = ifelse(input$bifactorName != "", "tree3", "tree2"),
        bifactor = if (input$bifactorName != "") input$bifactorName else NULL,
        rotation = 2,
        sizeLat = input$sizeLat,
        sizeMan = input$sizeMan,
        sizeMan2 = input$sizeMan2,
        curve = input$curve,
        residuals = FALSE,
        edge.label.cex = input$labelSize,
        edge.color = "#474747",
        intercepts = FALSE,
        thresholds = FALSE,
        label.cex = input$labelSize,
        mar = c(2, 7, 2, 7),
        style = "lisrel",
        label.prop = 1
      )
      
      dev.off()
    }
  )
  
  # Fiabilidad
  output$omegaTable <- renderTable({
    fit <- cfaModel()
    req(fit)
    reliability(fit, what = "omega")
  })
  
  # Índices si el modelo es bifactor
  output$bifactorIndices <- renderTable({
    fit <- cfaModel()
    req(fit)
    
    # Nombre del factor general desde input
    general_factor <- input$bifactorName
    
    # Nombres de factores latentes
    factors <- lavNames(fit, type = "lv")
    
    # Obtener la matriz de cargas estandarizadas
    loadings <- inspect(fit, "std")$lambda
    
    # ✅ Ver cuántos factores carga cada ítem con un umbral (evita ruido)
    load_count <- rowSums(abs(loadings) > 0.20)
    
    # Criterios mínimos para bifactor
    is_candidate <- length(factors) > 1 &&
      general_factor %in% colnames(loadings) &&
      all(load_count == 2)
    
    if (is_candidate) {
      tryCatch({
        bif_indices <- BifactorIndicesCalculator::bifactorIndices(fit)
        round(bif_indices$ModelLevelIndices, 3)  # puedes cambiar aquí el nivel
      }, error = function(e) {
        data.frame(Error = paste("Error en cálculo:", e$message))
      })
    } else {
      data.frame(
        Mensaje = "Este modelo no cumple con los criterios estructurales para un modelo bifactor.",
        Criterios = "Cada ítem debe cargar en un factor general y uno específico con carga > .20."
      )
    }
  })
  
  # Bifactor output
  output$bifactorItems <- renderTable({
    req(cfaModel())
    tryCatch({
      round(BifactorIndicesCalculator::bifactorIndices(cfaModel())$ItemLevelIndices, 3)
    }, error = function(e) {
      data.frame(Error = e$message)
    })
  })
  
  output$bifactorFactors <- renderTable({
    req(cfaModel())
    tryCatch({
      tabla <- round(BifactorIndicesCalculator::bifactorIndices(cfaModel())$FactorLevelIndices, 3)
      data.frame(
        Factor = rownames(tabla),  # convierte nombres de fila en columna
        tabla,
        row.names = NULL,
        check.names = FALSE
      )
    }, error = function(e) {
      data.frame(Error = e$message)
    })
  })
  
  # Puntos de corte bifactor
  output$bifactorPuntosCorte <- renderUI({
    tagList(
      h4("Guía de interpretación de índices bifactor"),
      tags$table(class = "table table-striped",
                 tags$thead(
                   tags$tr(
                     tags$th("Nivel"), tags$th("Índice"), tags$th("Descripción"), tags$th("Punto de corte")
                   )
                 ),
                 tags$tbody(
                   # Nivel del modelo
                   tags$tr(tags$td("Modelo"), tags$td("ECV (FG)"), tags$td("Varianza común explicada por el factor general (ECV_SS)"), tags$td("≥ .70 – .80")),
                   tags$tr(tags$td("Modelo"), tags$td("OmegaH (FG)"), tags$td("Confiabilidad jerárquica del factor general"), tags$td("≥ .75 – .80")),
                   
                   # Nivel del factor
                   tags$tr(tags$td("Factor"), tags$td("OmegaH"), tags$td("Confiabilidad jerárquica por factor específico"), tags$td("≥ .30")),
                   tags$tr(tags$td("Factor"), tags$td("H"), tags$td("Replicabilidad de las puntuaciones del factor"), tags$td("≥ .70")),
                   
                   # Nivel del ítem
                   tags$tr(tags$td("Ítem"), tags$td("IECV"), tags$td("Proporción de varianza del ítem explicada por el factor general"), tags$td("≥ .85"))
                 )
      ),
      br(),
      tags$em("Para una mejor comprensión, se recomienda consultar los trabajos de Rodriguez et al. (2016) y Domínguez-Lara y Rodríguez (2016). Los puntos de corte deben considerarse orientativos y no absolutos.")
    )
  })
  
  
  # Bootstrapping
  boot_results <- eventReactive(input$run_boot, {
    tryCatch({
      PsyMetricTools::boot_cfa(
        new_df = filtered_data(),  
        model_string = input$modelText,
        item_prefix = input$prefixItems,
        seed = 2025,
        n_replications = input$n_boot,
        ordered = input$ordered,
        estimator = input$boot_estimator
      )
    }, error = function(e) {
      showNotification(paste("Bootstrap error:", e$message), type = "error")
      NULL
    })
  })
  
  output$bootstrapPlot <- renderPlot({
    req(boot_results())
    PsyMetricTools::boot_cfa_plot(
      boot_results(),
      save = FALSE,
      omega_ymin_annot = input$omega_ymin,
      omega_ymax_annot = input$omega_ymax,
      comp_ymin_annot  = input$comp_ymin,
      comp_ymax_annot  = input$comp_ymax,
      abs_ymin_annot   = input$abs_ymin,
      abs_ymax_annot   = input$abs_ymax
    )
  })
  
  # Plot en bootstrap
  output$download_boot_plot <- downloadHandler(
    filename = function() {"bootstrap_plot.jpg"},
    content = function(file) {
      p <- PsyMetricTools::boot_cfa_plot(
        boot_results(),
        save = FALSE,
        omega_ymin_annot = input$omega_ymin,
        omega_ymax_annot = input$omega_ymax,
        comp_ymin_annot  = input$comp_ymin,
        comp_ymax_annot  = input$comp_ymax,
        abs_ymin_annot   = input$abs_ymin,
        abs_ymax_annot   = input$abs_ymax
      )
      ggsave(
        filename = file,
        plot = p,
        width = input$boot_plot_width,
        height = input$boot_plot_height,
        dpi = input$boot_plot_dpi
      )
    }
  )
  
  
  # Validez convergente (nuevo model fit con full_data)
  validityModel <- eventReactive(input$runValid, {
    req(input$validityModelText)
    tryCatch({
      cfa(input$validityModelText,
          data = full_data(),
          estimator = input$estimator,
          ordered = if (input$ordered) TRUE else NULL)
    }, error = function(e) {
      showNotification(paste("Error in validity model:", e$message), type = "error")
      NULL
    })
  })
  
  output$validityPlot <- renderPlot({
    fit <- validityModel()
    req(fit)
    
    layout_type <- ifelse(input$valid_bifactorName != "", "tree3", "tree2")
    
    semPaths(
      fit,
      what = "std",
      weighted = FALSE,
      layout = layout_type,
      bifactor = if (input$valid_bifactorName != "") input$valid_bifactorName else NULL,
      rotation = 2,
      sizeLat = input$valid_sizeLat,
      sizeMan = input$valid_sizeMan,
      sizeMan2 = input$valid_sizeMan2,
      curve = input$curve,
      residuals = FALSE,
      edge.label.cex = input$valid_labelSize,
      edge.color = "#474747",
      intercepts = FALSE,
      thresholds = FALSE,
      label.cex = input$valid_labelSize,
      mar = c(2, 7, 2, 7),
      style = "lisrel",
      label.prop = 1
    )
  })
  
  output$validityFit <- renderTable({
    fit <- validityModel()
    req(fit)
    
    estimator <- input$estimator
    
    indices <- switch(estimator,
                      "ML"    = c("chisq", "df", "rmsea", "srmr", "cfi", "tli"),
                      "MLR"   = c("chisq.scaled", "df.scaled", "rmsea.scaled", "srmr", "cfi.scaled", "tli.scaled"),
                      "WLSMV" = c("chisq.scaled", "df.scaled", "rmsea.scaled", "srmr", "cfi.scaled", "tli.scaled"),
                      "ULS"   = c("chisq", "df", "rmsea", "srmr", "cfi", "tli"))
    
    pretty_names <- c(
      "chisq" = "\u03C7\u00B2", "chisq.scaled" = "\u03C7\u00B2",
      "df" = "gl",              "df.scaled" = "gl",
      "rmsea" = "RMSEA",        "rmsea.scaled" = "RMSEA",
      "srmr" = "SRMR",
      "cfi" = "CFI",            "cfi.scaled" = "CFI",
      "tli" = "TLI",            "tli.scaled" = "TLI"
    )
    
    fm <- fitMeasures(fit, indices)
    labels <- pretty_names[indices]
    labels[is.na(labels)] <- indices[is.na(labels)]
    
    data.frame(
      `Fit Index` = labels,
      `Value` = round(as.numeric(fm), 3)
    )
  })
  
  output$validityMI <- DT::renderDT({
    fit <- validityModel()
    req(fit)
    modindices(fit, minimum.value = 10, sort = TRUE)
  })
  
  output$download_validity_fit <- downloadHandler(
    filename = function() {
      paste0("validity_fit_measures_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      fit <- validityModel()
      req(fit)
      
      estimator <- input$estimator
      
      indices <- switch(estimator,
                        "ML"    = c("chisq", "df", "rmsea", "srmr", "cfi", "tli"),
                        "MLR"   = c("chisq.scaled", "df.scaled", "rmsea.scaled", "srmr", "cfi.scaled", "tli.scaled"),
                        "WLSMV" = c("chisq.scaled", "df.scaled", "rmsea.scaled", "srmr", "cfi.scaled", "tli.scaled"),
                        "ULS"   = c("chisq", "df", "rmsea", "srmr", "cfi", "tli"))
      
      pretty_names <- c(
        "chisq" = "\u03C7\u00B2", "chisq.scaled" = "\u03C7\u00B2",
        "df" = "gl",              "df.scaled" = "gl",
        "rmsea" = "RMSEA",        "rmsea.scaled" = "RMSEA",
        "srmr" = "SRMR",
        "cfi" = "CFI",            "cfi.scaled" = "CFI",
        "tli" = "TLI",            "tli.scaled" = "TLI"
      )
      
      fm <- fitMeasures(fit, indices)
      labels <- pretty_names[indices]
      labels[is.na(labels)] <- indices[is.na(labels)]
      
      tabla <- data.frame(
        `Fit Index` = labels,
        `Value` = round(as.numeric(fm), 3)
      )
      
      # Estilo profesional
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Fit Measures")
      openxlsx::writeData(wb, "Fit Measures", tabla, startRow = 1, startCol = 1, headerStyle = openxlsx::createStyle(textDecoration = "bold"))
      openxlsx::setColWidths(wb, "Fit Measures", cols = 1:2, widths = "auto")
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  
  output$download_valid_plot <- downloadHandler(
    filename = function() {
      paste0("validity_plot_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      fit <- validityModel()
      req(fit)
      
      layout_type <- ifelse(input$valid_bifactorName != "", "tree3", "tree2")
      
      jpeg(file, width = 10, height = 8, units = "in", res = 300)
      semPaths(
        fit,
        what = "std",
        weighted = FALSE,
        layout = layout_type,
        bifactor = if (input$valid_bifactorName != "") input$valid_bifactorName else NULL,
        rotation = 2,
        sizeLat = input$valid_sizeLat,
        sizeMan = input$valid_sizeMan,
        sizeMan2 = input$valid_sizeMan2,
        curve = input$curve,
        residuals = FALSE,
        edge.label.cex = input$valid_labelSize,
        edge.color = "#474747",
        intercepts = FALSE,
        thresholds = FALSE,
        label.cex = input$valid_labelSize,
        mar = c(2, 7, 2, 7),
        style = "lisrel",
        label.prop = 1
      )
      dev.off()
    }
  )
}  



