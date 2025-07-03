#!/usr/bin/env Rscript

# Configurar repositorio CRAN
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Función para instalar paquetes con manejo de errores
install_package_safe <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Instalando", pkg, "...\n")
    tryCatch({
      install.packages(pkg, dependencies = TRUE)
      cat("✓", pkg, "instalado exitosamente\n")
    }, error = function(e) {
      cat("✗ Error instalando", pkg, ":", e$message, "\n")
    })
  } else {
    cat("✓", pkg, "ya está instalado\n")
  }
}

# Lista de paquetes necesarios para R-exams ICFES
packages <- c(
  # Paquetes principales
  "exams",
  "knitr", 
  "rmarkdown",
  
  # Manipulación de datos
  "dplyr",
  "tidyr", 
  "data.table",
  "readxl",
  "jsonlite",
  
  # Visualización
  "ggplot2",
  "scales",
  "gridExtra",
  "RColorBrewer",
  
  # Estadística
  "MASS",
  "car",
  "psych",
  
  # LaTeX y documentos
  "tinytex",
  "pdftools",
  "qpdf",
  
  # Integración Python
  "reticulate",
  
  # Herramientas adicionales
  "devtools",
  "testthat",
  "digest",
  "magick",
  "webshot",
  "htmltools",
  "base64enc"
)

# Instalar paquetes
cat("Instalando paquetes R necesarios para R-exams ICFES...\n")
cat("=====================================================\n")

for (pkg in packages) {
  install_package_safe(pkg)
}

cat("\n🎯 Instalación de paquetes R completada\n")
