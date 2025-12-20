#!/bin/bash
# Ejemplo Avanzado 07: Skill - Validador de Renderizado R-exams
# Nivel: Avanzado
# Propósito: Ejecutar renderizado R-exams y capturar errores para análisis posterior

# Configuración
RMD_FILE="$1"
OUTPUT_DIR="${2:-/tmp/rexams_validation}"
LOG_FILE="${OUTPUT_DIR}/render_log.txt"
ERROR_LOG="${OUTPUT_DIR}/render_errors.txt"
SUCCESS_LOG="${OUTPUT_DIR}/render_success.txt"

# Crear directorio de salida
mkdir -p "$OUTPUT_DIR"

# Colores
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo "🔧 Ejecutando validación de renderizado para: $RMD_FILE"
echo "📁 Directorio de salida: $OUTPUT_DIR"

# 1. Validar que el archivo existe
if [ ! -f "$RMD_FILE" ]; then
    echo -e "${RED}❌ Error: Archivo no encontrado: $RMD_FILE${NC}" >&2
    exit 1
fi

# 2. Validar que R está disponible
if ! command -v Rscript &> /dev/null; then
    echo -e "${RED}❌ Error: Rscript no encontrado. Instala R primero.${NC}" >&2
    exit 1
fi

# 3. Crear script R temporal para validar renderizado
R_VALIDATION_SCRIPT=$(mktemp /tmp/rexams_validate_XXXXXX.R)

cat > "$R_VALIDATION_SCRIPT" << 'EOFSCRIPT'
#!/usr/bin/env Rscript

# Script de validación de renderizado R-exams
args <- commandArgs(trailingOnly = TRUE)
rmd_file <- args[1]
output_dir <- args[2]
log_file <- file.path(output_dir, "render_log.txt")
error_log <- file.path(output_dir, "render_errors.txt")
success_log <- file.path(output_dir, "render_success.txt")

# Redirigir output a archivos
sink(log_file, append = FALSE)
sink(error_log, append = FALSE, type = "message")

cat("=== INICIO VALIDACIÓN RENDERIZADO ===\n")
cat("Archivo:", rmd_file, "\n")
cat("Fecha:", Sys.time(), "\n\n")

# Intentar cargar librerías necesarias
tryCatch({
  if (!require("exams", quietly = TRUE)) {
    stop("Paquete 'exams' no está instalado. Instala con: install.packages('exams')")
  }
  if (!require("rmarkdown", quietly = TRUE)) {
    stop("Paquete 'rmarkdown' no está instalado")
  }
  cat("✓ Librerías cargadas correctamente\n")
}, error = function(e) {
  cat("❌ ERROR cargando librerías:", conditionMessage(e), "\n")
  quit(status = 1)
})

# Intentar renderizar con exams2html (más rápido que PDF)
tryCatch({
  cat("🔄 Intentando renderizar con exams2html...\n")
  
  # Configurar opciones para capturar errores
  options(warn = 1)
  options(error = quote({
    cat("❌ ERROR durante renderizado:\n")
    print(sys.calls())
    traceback()
    quit(status = 1)
  }))
  
  # Renderizar a HTML (más rápido para validación)
  output_html <- exams2html(
    file = rmd_file,
    dir = output_dir,
    name = "validation_output",
    quiet = FALSE,
    converter = "pandoc-mathjax"
  )
  
  cat("✓ Renderizado exitoso\n")
  cat("Archivo generado:", output_html, "\n")
  
  # Escribir éxito
  writeLines(paste("SUCCESS", Sys.time(), rmd_file, sep = "|"), success_log)
  
  sink()
  sink(type = "message")
  
  quit(status = 0)
  
}, error = function(e) {
  cat("❌ ERROR durante renderizado:\n")
  cat(conditionMessage(e), "\n")
  cat("\n=== TRACEBACK ===\n")
  print(traceback())
  
  # Capturar error detallado
  error_details <- paste(
    "ERROR:", conditionMessage(e),
    "\nTRACEBACK:\n",
    paste(capture.output(traceback()), collapse = "\n"),
    sep = "\n"
  )
  writeLines(error_details, error_log)
  
  sink()
  sink(type = "message")
  
  quit(status = 1)
})
EOFSCRIPT

# 4. Ejecutar script R de validación
echo "🔄 Ejecutando renderizado..."

Rscript "$R_VALIDATION_SCRIPT" "$RMD_FILE" "$OUTPUT_DIR" 2>&1 | tee -a "$LOG_FILE"
RENDER_STATUS=$?

# 5. Analizar resultados
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if [ $RENDER_STATUS -eq 0 ]; then
    echo -e "${GREEN}✅ Renderizado exitoso${NC}"
    echo "📄 Archivos generados en: $OUTPUT_DIR"
    
    # Listar archivos generados
    if [ -f "$SUCCESS_LOG" ]; then
        echo -e "${BLUE}✓ Log de éxito: $SUCCESS_LOG${NC}"
    fi
    
    # Limpiar script temporal
    rm -f "$R_VALIDATION_SCRIPT"
    
    exit 0
else
    echo -e "${RED}❌ Renderizado falló${NC}"
    echo "📋 Revisar logs en: $OUTPUT_DIR"
    
    # Mostrar extracto de errores
    if [ -f "$ERROR_LOG" ]; then
        echo ""
        echo -e "${YELLOW}=== EXTRACTO DE ERRORES ===${NC}"
        head -50 "$ERROR_LOG" | tail -30
        echo ""
        echo -e "${BLUE}📄 Log completo: $ERROR_LOG${NC}"
    fi
    
    if [ -f "$LOG_FILE" ]; then
        echo -e "${BLUE}📄 Log completo: $LOG_FILE${NC}"
    fi
    
    # Limpiar script temporal
    rm -f "$R_VALIDATION_SCRIPT"
    
    # Retornar código de error y contenido de error_log para análisis
    echo "$ERROR_LOG"
    exit 1
fi
