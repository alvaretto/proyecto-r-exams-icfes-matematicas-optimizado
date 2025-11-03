#!/usr/bin/env Rscript

################################################################################
# Script de Instalación de Paquetes R para Proyecto ICFES R-Exams
# Proyecto: RepositorioMatematicasICFES_R_Exams
# Descripción: Instala todos los paquetes R necesarios para el proyecto
################################################################################

cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║  INSTALACIÓN DE PAQUETES R - PROYECTO ICFES R-EXAMS           ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

# Configurar biblioteca personal
personal_lib <- "~/R/library"
if (!dir.exists(personal_lib)) {
  dir.create(personal_lib, recursive = TRUE)
  cat("📁 Creado directorio de biblioteca personal:", personal_lib, "\n")
}

.libPaths(c(personal_lib, .libPaths()))

# Configurar repositorio CRAN
options(repos = c(CRAN = "https://cloud.r-project.org"))
options(timeout = 300)  # Aumentar timeout para descargas

cat("📚 Biblioteca R:", .libPaths()[1], "\n")
cat("🌐 Repositorio CRAN:", getOption("repos")["CRAN"], "\n\n")

# Función para instalar paquetes con manejo de errores
install_package_safe <- function(pkg, description = "") {
  desc_text <- if (description != "") paste0(" (", description, ")") else ""
  
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    version <- packageVersion(pkg)
    cat(sprintf("  ✓ %-20s %s %s\n", pkg, version, desc_text))
    return(TRUE)
  } else {
    cat(sprintf("  📦 Instalando %-20s %s\n", pkg, desc_text))
    tryCatch({
      install.packages(pkg, dependencies = TRUE, quiet = FALSE)
      
      # Verificar instalación
      if (require(pkg, character.only = TRUE, quietly = TRUE)) {
        version <- packageVersion(pkg)
        cat(sprintf("  ✅ %-20s %s instalado\n", pkg, version))
        return(TRUE)
      } else {
        cat(sprintf("  ❌ %-20s ERROR: No se pudo cargar después de instalar\n", pkg))
        return(FALSE)
      }
    }, error = function(e) {
      cat(sprintf("  ❌ %-20s ERROR: %s\n", pkg, e$message))
      return(FALSE)
    })
  }
}

# Contador de éxitos y fallos
total_packages <- 0
installed_packages <- 0
failed_packages <- c()

cat("═══════════════════════════════════════════════════════════════\n")
cat("FASE 1: PAQUETES PRINCIPALES R-EXAMS\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

main_packages <- list(
  list(name = "exams", desc = "Framework R-exams"),
  list(name = "knitr", desc = "Procesamiento de documentos"),
  list(name = "rmarkdown", desc = "R Markdown")
)

for (pkg_info in main_packages) {
  total_packages <- total_packages + 1
  if (install_package_safe(pkg_info$name, pkg_info$desc)) {
    installed_packages <- installed_packages + 1
  } else {
    failed_packages <- c(failed_packages, pkg_info$name)
  }
}

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("FASE 2: MANIPULACIÓN DE DATOS\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

data_packages <- list(
  list(name = "tidyverse", desc = "Colección de paquetes tidyverse"),
  list(name = "dplyr", desc = "Manipulación de datos"),
  list(name = "tidyr", desc = "Limpieza de datos"),
  list(name = "data.table", desc = "Manipulación eficiente"),
  list(name = "readxl", desc = "Lectura de archivos Excel"),
  list(name = "datasets", desc = "Conjuntos de datos")
)

for (pkg_info in data_packages) {
  total_packages <- total_packages + 1
  if (install_package_safe(pkg_info$name, pkg_info$desc)) {
    installed_packages <- installed_packages + 1
  } else {
    failed_packages <- c(failed_packages, pkg_info$name)
  }
}

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("FASE 3: VISUALIZACIÓN Y GRÁFICOS\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

viz_packages <- list(
  list(name = "ggplot2", desc = "Gráficos avanzados"),
  list(name = "scales", desc = "Escalas para gráficos"),
  list(name = "gridExtra", desc = "Layouts de gráficos"),
  list(name = "RColorBrewer", desc = "Paletas de colores")
)

for (pkg_info in viz_packages) {
  total_packages <- total_packages + 1
  if (install_package_safe(pkg_info$name, pkg_info$desc)) {
    installed_packages <- installed_packages + 1
  } else {
    failed_packages <- c(failed_packages, pkg_info$name)
  }
}

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("FASE 4: ESTADÍSTICA Y ANÁLISIS\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

stats_packages <- list(
  list(name = "MASS", desc = "Funciones estadísticas"),
  list(name = "car", desc = "Análisis de regresión"),
  list(name = "psych", desc = "Análisis psicométrico")
)

for (pkg_info in stats_packages) {
  total_packages <- total_packages + 1
  if (install_package_safe(pkg_info$name, pkg_info$desc)) {
    installed_packages <- installed_packages + 1
  } else {
    failed_packages <- c(failed_packages, pkg_info$name)
  }
}

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("FASE 5: LATEX Y DOCUMENTOS\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

latex_packages <- list(
  list(name = "tinytex", desc = "Distribución LaTeX ligera"),
  list(name = "pdftools", desc = "Manipulación de PDF"),
  list(name = "qpdf", desc = "Procesamiento de PDF")
)

for (pkg_info in latex_packages) {
  total_packages <- total_packages + 1
  if (install_package_safe(pkg_info$name, pkg_info$desc)) {
    installed_packages <- installed_packages + 1
  } else {
    failed_packages <- c(failed_packages, pkg_info$name)
  }
}

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("FASE 6: INTEGRACIÓN PYTHON (CRÍTICO)\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

python_packages <- list(
  list(name = "reticulate", desc = "Integración Python-R")
)

for (pkg_info in python_packages) {
  total_packages <- total_packages + 1
  if (install_package_safe(pkg_info$name, pkg_info$desc)) {
    installed_packages <- installed_packages + 1
  } else {
    failed_packages <- c(failed_packages, pkg_info$name)
  }
}

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("FASE 7: HERRAMIENTAS ADICIONALES\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

tools_packages <- list(
  list(name = "devtools", desc = "Herramientas de desarrollo"),
  list(name = "testthat", desc = "Testing unitario"),
  list(name = "digest", desc = "Hashing para diversidad"),
  list(name = "magick", desc = "Procesamiento de imágenes"),
  list(name = "webshot", desc = "Capturas de pantalla web"),
  list(name = "htmltools", desc = "Herramientas HTML"),
  list(name = "base64enc", desc = "Codificación Base64")
)

for (pkg_info in tools_packages) {
  total_packages <- total_packages + 1
  if (install_package_safe(pkg_info$name, pkg_info$desc)) {
    installed_packages <- installed_packages + 1
  } else {
    failed_packages <- c(failed_packages, pkg_info$name)
  }
}

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("FASE 8: CONFIGURACIÓN DE TINYTEX\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

if (require("tinytex", quietly = TRUE)) {
  if (!tinytex::is_tinytex()) {
    cat("📄 Instalando TinyTeX...\n")
    tryCatch({
      tinytex::install_tinytex(force = TRUE)
      Sys.sleep(5)  # Esperar a que se complete
      cat("✅ TinyTeX instalado correctamente\n")
    }, error = function(e) {
      cat("⚠️ Error instalando TinyTeX:", e$message, "\n")
      cat("   Puedes usar TeX Live del sistema en su lugar\n")
    })
  } else {
    cat("✅ TinyTeX ya está instalado\n")
  }
  
  # Instalar paquetes LaTeX críticos
  if (tinytex::is_tinytex()) {
    cat("\n📦 Instalando paquetes LaTeX críticos...\n")
    
    latex_pkgs <- c(
      "amsmath", "amsfonts", "amssymb", "mathtools",
      "babel", "babel-spanish",
      "booktabs", "colortbl", "array", "multirow",
      "tikz", "pgf", "pgfplots", "xcolor",
      "geometry", "fancyhdr", "enumitem", "float",
      "fontspec", "xunicode", "unicode-math",
      "hyperref", "url", "graphicx", "listings"
    )
    
    tryCatch({
      tinytex::tlmgr_install(latex_pkgs)
      cat("✅ Paquetes LaTeX instalados\n")
    }, error = function(e) {
      cat("⚠️ Algunos paquetes LaTeX pueden no haberse instalado\n")
    })
  }
} else {
  cat("❌ Paquete tinytex no disponible\n")
}

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("FASE 9: CONFIGURACIÓN DE RETICULATE\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

if (require("reticulate", quietly = TRUE)) {
  python3_path <- Sys.which("python3")
  
  if (python3_path != "") {
    cat("🐍 Configurando Python para reticulate...\n")
    tryCatch({
      reticulate::use_python(python3_path, required = FALSE)
      py_config_info <- reticulate::py_config()
      
      cat("✅ Python configurado:\n")
      cat("   Ruta:", py_config_info$python, "\n")
      cat("   Versión:", py_config_info$version, "\n")
      
      # Probar funcionalidad
      reticulate::py_run_string("print('✅ Python-R integración funcional')")
      
    }, error = function(e) {
      cat("⚠️ Error configurando Python:", e$message, "\n")
    })
  } else {
    cat("⚠️ Python 3 no encontrado en el sistema\n")
  }
} else {
  cat("❌ Paquete reticulate no disponible\n")
}

cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║                    RESUMEN DE INSTALACIÓN                      ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat(sprintf("Total de paquetes: %d\n", total_packages))
cat(sprintf("✅ Instalados exitosamente: %d\n", installed_packages))
cat(sprintf("❌ Fallidos: %d\n", length(failed_packages)))

if (length(failed_packages) > 0) {
  cat("\n⚠️ Paquetes que fallaron:\n")
  for (pkg in failed_packages) {
    cat("   •", pkg, "\n")
  }
  cat("\nPuedes intentar instalarlos manualmente con:\n")
  cat("install.packages(c(\"", paste(failed_packages, collapse = "\", \""), "\"))\n", sep = "")
}

cat("\n")
if (length(failed_packages) == 0) {
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║  🎉 TODOS LOS PAQUETES INSTALADOS EXITOSAMENTE                ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n")
} else {
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║  ⚠️ INSTALACIÓN COMPLETADA CON ALGUNOS ERRORES                ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n")
}

cat("\n💡 Próximos pasos:\n")
cat("   1. Reiniciar RStudio si está abierto\n")
cat("   2. Verificar instalación con: source('verify_installation.R')\n")
cat("   3. Abrir proyecto: RepositorioMatematicasICFES_R_Exams.Rproj\n\n")

