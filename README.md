# CFA-Shiny: Aplicación Shiny para Análisis Factorial Confirmatorio
¡Bienvenido a **CFA-Shiny**! Esta es una aplicación interactiva desarrollada en R con Shiny que permite realizar Análisis Factorial Confirmatorio (CFA) utilizando el paquete `lavaan`. Está pensada para estudiantes, investigadores y docentes que trabajan con escalas psicológicas y desean evaluar modelos de medida de manera accesible y visual.

## Requisitos
Asegúrate de tener instalados los siguientes paquetes de R:

```r
packages <- c(
  "shiny", "shinyWidgets", "shinythemes", "shinydashboard", "shinydashboardPlus",
  "DT", "psych", "ggplot2", "readxl", "openxlsx", "ggstats", 
  "lavaan", "semPlot", "semTools", "BifactorIndicesCalculator", "PsyMetricTools"
)
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}
```
## ¿Cómo usar la app?
Ejecuta desde R directamente este comando:
```r
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
shiny::runGitHub("CFA-Shiny", "CristopherLino")
```

## Accede a la app

Puedes usar la aplicación directamente desde tu navegador, sin necesidad de instalar nada, en el siguiente enlace:

👉 [https://linocruz-cfa-app.shinyapps.io/cfa_shiny/](https://linocruz-cfa-app.shinyapps.io/cfa_shiny/)
