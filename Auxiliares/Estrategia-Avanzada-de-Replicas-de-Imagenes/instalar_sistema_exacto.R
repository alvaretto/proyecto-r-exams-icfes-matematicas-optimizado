# ============================================================================
# SCRIPT DE INSTALACIÓN Y CONFIGURACIÓN
# Sistema de Réplica Exacta de Imágenes Matemáticas
# ============================================================================

cat("🚀 INSTALANDO SISTEMA DE RÉPLICA EXACTA\n")
cat("======================================\n\n")

# Función para instalar paquetes si no están disponibles
instalar_si_necesario <- function(paquetes) {
  for (paquete in paquetes) {
    if (!require(paquete, character.only = TRUE, quietly = TRUE)) {
      cat(sprintf("📦 Instalando %s...\n", paquete))
      install.packages(paquete, dependencies = TRUE)
      library(paquete, character.only = TRUE)
      cat(sprintf("✅ %s instalado exitosamente\n", paquete))
    } else {
      cat(sprintf("✅ %s ya está disponible\n", paquete))
    }
  }
}

# Lista de paquetes necesarios
paquetes_requeridos <- c(
  # Paquetes básicos del proyecto
  "exams",
  "digest", 
  "testthat",
  "knitr",
  "rmarkdown",
  
  # Paquetes para análisis de imágenes
  "magick",
  "imager",
  
  # Paquetes para OCR (opcional)
  "tesseract",
  
  # Paquetes de utilidad
  "grDevices",
  "stats",
  "utils"
)

cat("📦 INSTALANDO DEPENDENCIAS\n")
cat("==========================\n")

# Instalar paquetes requeridos
instalar_si_necesario(paquetes_requeridos)

cat("\n🔧 CONFIGURANDO SISTEMA\n")
cat("======================\n")

# Configurar opciones globales para el sistema
options(
  # Configuración TikZ
  tikzLatex = "pdflatex",
  tikzXelatex = FALSE,
  tikzLatexPackages = c(
    "\\usepackage{tikz}",
    "\\usepackage{pgfplots}",
    "\\usepackage{xcolor}",
    "\\usepackage{colortbl}",
    "\\usepackage{amsmath}",
    "\\usepackage{array}"
  ),
  
  # Configuración numérica
  OutDec = ".",
  scipen = 999,
  
  # Configuración de imágenes
  device = c("png", "pdf"),
  dpi = 150
)

# Configurar locale para consistencia numérica
Sys.setlocale(category = "LC_NUMERIC", locale = "C")

cat("✅ Configuración global aplicada\n")

# Verificar disponibilidad de herramientas externas
cat("\n🔍 VERIFICANDO HERRAMIENTAS EXTERNAS\n")
cat("===================================\n")

# Verificar LaTeX
latex_disponible <- system("pdflatex --version", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
if (latex_disponible) {
  cat("✅ LaTeX (pdflatex) disponible\n")
} else {
  cat("⚠️ LaTeX no encontrado. Instalar TeX Live o MiKTeX para funcionalidad completa\n")
}

# Verificar Tesseract OCR
tesseract_disponible <- tryCatch({
  tesseract::tesseract_info()
  TRUE
}, error = function(e) FALSE)

if (tesseract_disponible) {
  cat("✅ Tesseract OCR disponible\n")
} else {
  cat("⚠️ Tesseract OCR no encontrado. OCR será limitado\n")
}

# Verificar ImageMagick
imagemagick_disponible <- tryCatch({
  magick::magick_config()
  TRUE
}, error = function(e) FALSE)

if (imagemagick_disponible) {
  cat("✅ ImageMagick disponible\n")
} else {
  cat("❌ ImageMagick no encontrado. Instalar ImageMagick para funcionalidad completa\n")
}

cat("\n📁 VERIFICANDO ESTRUCTURA DE DIRECTORIOS\n")
cat("========================================\n")

# Crear directorios necesarios si no existen
directorios_necesarios <- c(
  "Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes",
  "Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/reportes",
  "Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/temp",
  "Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/ejemplos"
)

for (directorio in directorios_necesarios) {
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
    cat(sprintf("📁 Creado: %s\n", directorio))
  } else {
    cat(sprintf("✅ Existe: %s\n", directorio))
  }
}

cat("\n🧪 EJECUTANDO PRUEBAS DEL SISTEMA\n")
cat("=================================\n")

# Función de prueba básica
probar_sistema_basico <- function() {
  tryCatch({
    # Crear imagen de prueba
    img_prueba <- magick::image_blank(200, 150, "white")
    img_prueba <- magick::image_annotate(img_prueba, "Prueba", size = 20, location = "+50+75")
    
    # Guardar imagen temporal
    temp_path <- tempfile(fileext = ".png")
    magick::image_write(img_prueba, temp_path)
    
    # Probar análisis básico
    if (file.exists("Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/modulo_analisis_automatico_exacto.R")) {
      source("Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/modulo_analisis_automatico_exacto.R")
      cat("✅ Módulo de análisis cargado correctamente\n")
    }
    
    # Limpiar
    unlink(temp_path)
    
    return(TRUE)
  }, error = function(e) {
    cat(sprintf("❌ Error en prueba: %s\n", e$message))
    return(FALSE)
  })
}

# Ejecutar prueba
prueba_exitosa <- probar_sistema_basico()

cat("\n📋 CARGANDO MÓDULOS DEL SISTEMA\n")
cat("==============================\n")

# Cargar módulos si están disponibles
modulos_sistema <- c(
  "Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/modulo_analisis_automatico_exacto.R",
  "Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/modulo_validacion_cuantitativa.R",
  "Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/agente_graficador_exacto.R",
  "Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/generador_tikz_qtikz_compatible.R",
  "Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/validador_qtikz_compatible.R",
  "Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/sistema_replica_exacta.R",
  "Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/ejemplo_qtikz_compatible.R"
)

modulos_cargados <- 0
for (modulo in modulos_sistema) {
  if (file.exists(modulo)) {
    tryCatch({
      source(modulo)
      cat(sprintf("✅ Cargado: %s\n", basename(modulo)))
      modulos_cargados <- modulos_cargados + 1
    }, error = function(e) {
      cat(sprintf("⚠️ Error cargando %s: %s\n", basename(modulo), e$message))
    })
  } else {
    cat(sprintf("❌ No encontrado: %s\n", basename(modulo)))
  }
}

cat("\n🎯 RESUMEN DE INSTALACIÓN\n")
cat("========================\n")

cat(sprintf("📦 Paquetes instalados: %d/%d\n", length(paquetes_requeridos), length(paquetes_requeridos)))
cat(sprintf("🔧 Herramientas externas: %d/3 disponibles\n", 
            sum(latex_disponible, tesseract_disponible, imagemagick_disponible)))
cat(sprintf("📁 Directorios: %d/%d creados\n", length(directorios_necesarios), length(directorios_necesarios)))
cat(sprintf("📋 Módulos cargados: %d/%d\n", modulos_cargados, length(modulos_sistema)))
cat(sprintf("🧪 Prueba básica: %s\n", if(prueba_exitosa) "✅ EXITOSA" else "❌ FALLÓ"))

# Determinar estado general
if (modulos_cargados == length(modulos_sistema) && prueba_exitosa) {
  estado_general <- "✅ SISTEMA COMPLETAMENTE OPERATIVO"
} else if (modulos_cargados >= 2) {
  estado_general <- "⚠️ SISTEMA PARCIALMENTE OPERATIVO"
} else {
  estado_general <- "❌ SISTEMA REQUIERE CONFIGURACIÓN ADICIONAL"
}

cat(sprintf("\n🎯 ESTADO GENERAL: %s\n", estado_general))

cat("\n📖 PRÓXIMOS PASOS\n")
cat("=================\n")

if (modulos_cargados == length(modulos_sistema)) {
  cat("🚀 El sistema está listo para usar. Comandos disponibles:\n")
  cat("   - replica_exacta('imagen.png')\n")
  cat("   - sistema_replica_exacta('imagen.png')\n")
  cat("   - analizar_imagen_matematica_exacta('imagen.png')\n")
  cat("   - validar_compatibilidad_qtikz(codigo_tikz)\n")
  cat("   - demo_compatibilidad_qtikz('tabla')\n")
  cat("   - ayuda_qtikz()\n")
} else {
  cat("🔧 Para completar la instalación:\n")
  cat("   1. Verificar que todos los módulos estén en el directorio correcto\n")
  cat("   2. Instalar herramientas externas faltantes\n")
  cat("   3. Ejecutar nuevamente este script\n")
}

cat("\n📚 DOCUMENTACIÓN\n")
cat("================\n")
cat("📄 Guía completa: Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/README_Estrategia_Robusta.md\n")
cat("📊 Ejemplos de uso: Ver sección 'GUÍA DE USO RÁPIDO' en README\n")

cat("\n✅ INSTALACIÓN COMPLETADA\n")
cat("=========================\n")

# Función de ayuda rápida
mostrar_ayuda_rapida <- function() {
  cat("\n🆘 AYUDA RÁPIDA - SISTEMA DE RÉPLICA EXACTA\n")
  cat("==========================================\n")
  cat("📖 Comandos principales:\n")
  cat("   replica_exacta('imagen.png')                    # Uso básico\n")
  cat("   sistema_replica_exacta('imagen.png')           # Uso avanzado\n")
  cat("   analizar_imagen_matematica_exacta('imagen.png') # Solo análisis\n")
  cat("\n📊 Verificar estado:\n")
  cat("   source('Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/instalar_sistema_exacto.R')\n")
  cat("\n📚 Documentación completa:\n")
  cat("   file.show('Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/README_Estrategia_Robusta.md')\n")
}

# Hacer función de ayuda disponible globalmente
assign("ayuda_replica_exacta", mostrar_ayuda_rapida, envir = .GlobalEnv)

cat("\n💡 Tip: Ejecuta ayuda_replica_exacta() para ver comandos disponibles\n")

# Retornar estado de instalación
invisible(list(
  paquetes_instalados = length(paquetes_requeridos),
  herramientas_disponibles = sum(latex_disponible, tesseract_disponible, imagemagick_disponible),
  modulos_cargados = modulos_cargados,
  sistema_operativo = (modulos_cargados == length(modulos_sistema) && prueba_exitosa)
))
