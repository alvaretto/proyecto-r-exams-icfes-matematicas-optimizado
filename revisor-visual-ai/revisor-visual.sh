#!/bin/bash

# ============================================================================
# Sistema Revisor-Rmd 2.0 con Análisis Visual IA
# ============================================================================
# 
# Sistema avanzado que combina análisis textual con análisis visual real
# de gráficas generadas por código R/Python
#
# ============================================================================

set -euo pipefail

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
NC='\033[0m'

# Variables globales
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
VENV_PATH="$SCRIPT_DIR/venv"
LOG_FILE="$SCRIPT_DIR/output/revisor-visual.log"

# Función de logging
log() {
    local level="$1"
    local message="$2"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    
    echo "[$timestamp] $level $message" >> "$LOG_FILE"
    
    case "$level" in
        "ERROR")
            echo -e "${RED}❌ $message${NC}" >&2
            ;;
        "WARNING")
            echo -e "${YELLOW}⚠️  $message${NC}"
            ;;
        "INFO")
            echo -e "${BLUE}ℹ️  $message${NC}"
            ;;
        "SUCCESS")
            echo -e "${GREEN}✅ $message${NC}"
            ;;
        "VISUAL")
            echo -e "${PURPLE}👁️  $message${NC}"
            ;;
        *)
            echo "$message"
            ;;
    esac
}

# Banner del sistema
show_banner() {
    echo -e "${PURPLE}"
    echo "╔══════════════════════════════════════════════════════════════════════════════╗"
    echo "║                    REVISOR-RMD 2.0 CON ANÁLISIS VISUAL IA                   ║"
    echo "║              Análisis textual + visual real de gráficas generadas           ║"
    echo "║                    RepositorioMatematicasICFES_R_Exams                       ║"
    echo "╚══════════════════════════════════════════════════════════════════════════════╝"
    echo -e "${NC}"
}

# Verificar dependencias
check_dependencies() {
    log "INFO" "Verificando dependencias del sistema..."
    
    # Verificar R
    if ! command -v R &> /dev/null; then
        log "ERROR" "R no está instalado"
        exit 1
    fi
    
    # Verificar Python
    if ! command -v python3 &> /dev/null; then
        log "ERROR" "Python3 no está instalado"
        exit 1
    fi
    
    # Verificar tesseract
    if ! command -v tesseract &> /dev/null; then
        log "ERROR" "Tesseract OCR no está instalado"
        exit 1
    fi
    
    # Verificar entorno virtual Python
    if [ ! -d "$VENV_PATH" ]; then
        log "ERROR" "Entorno virtual Python no encontrado en: $VENV_PATH"
        log "INFO" "Ejecutar: python3 -m venv $VENV_PATH && source $VENV_PATH/bin/activate && pip install opencv-python pillow pytesseract numpy matplotlib"
        exit 1
    fi
    
    log "SUCCESS" "Todas las dependencias están disponibles"
}

# Configurar entorno
setup_environment() {
    # Crear directorio de logs
    mkdir -p "$(dirname "$LOG_FILE")"
    
    # Activar entorno virtual Python
    source "$VENV_PATH/bin/activate"
    
    # Configurar variables de entorno
    export PYTHONPATH="$SCRIPT_DIR/core:$PYTHONPATH"
    export TESSERACT_CMD="$(which tesseract)"
    
    log "SUCCESS" "Entorno configurado correctamente"
}

# Función de ayuda
show_help() {
    cat << EOF
Uso: $0 [ARCHIVO.Rmd] [OPCIONES]

OPCIONES:
    -h, --help              Mostrar esta ayuda
    -v, --verbose           Modo verbose
    -o, --output DIR        Directorio de salida (default: output/)
    -f, --format FORMAT     Formato de reporte (console, html, json)
    --visual-only           Solo análisis visual (sin análisis textual)
    --no-visual             Solo análisis textual (sin análisis visual)
    --test                  Ejecutar con archivo de prueba

EJEMPLOS:
    $0 ejercicio.Rmd                           # Análisis completo
    $0 ejercicio.Rmd --format html             # Reporte HTML
    $0 ejercicio.Rmd --visual-only             # Solo análisis visual
    $0 --test                                  # Prueba con archivo ejemplo

EOF
}

# Parsear argumentos
parse_arguments() {
    TARGET_FILE=""
    OUTPUT_DIR="$SCRIPT_DIR/output"
    FORMAT="console"
    VERBOSE=false
    VISUAL_ONLY=false
    NO_VISUAL=false
    TEST_MODE=false
    
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                show_help
                exit 0
                ;;
            -v|--verbose)
                VERBOSE=true
                shift
                ;;
            -o|--output)
                OUTPUT_DIR="$2"
                shift 2
                ;;
            -f|--format)
                FORMAT="$2"
                shift 2
                ;;
            --visual-only)
                VISUAL_ONLY=true
                shift
                ;;
            --no-visual)
                NO_VISUAL=true
                shift
                ;;
            --test)
                TEST_MODE=true
                shift
                ;;
            -*)
                log "ERROR" "Opción desconocida: $1"
                show_help
                exit 1
                ;;
            *)
                if [ -z "$TARGET_FILE" ]; then
                    TARGET_FILE="$1"
                else
                    log "ERROR" "Múltiples archivos especificados"
                    exit 1
                fi
                shift
                ;;
        esac
    done
    
    # Validar argumentos
    if [ "$TEST_MODE" = true ]; then
        TARGET_FILE="/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/Auxiliares/Ejemplos-Funcionales-Rmd/Plantillas/tex/Graficos_Estadisticos_Adopcion_Mascotas/I_1796473-Opc-A2v2_optimizado.Rmd"
    fi
    
    if [ -z "$TARGET_FILE" ]; then
        log "ERROR" "Debe especificar un archivo .Rmd"
        show_help
        exit 1
    fi
    
    if [ ! -f "$TARGET_FILE" ]; then
        log "ERROR" "Archivo no encontrado: $TARGET_FILE"
        exit 1
    fi
    
    # Crear directorio de salida
    mkdir -p "$OUTPUT_DIR"
}

# Ejecutar análisis visual
run_visual_analysis() {
    local rmd_file="$1"
    local output_file="$2"
    
    log "VISUAL" "Iniciando análisis visual con IA..."
    log "INFO" "Archivo: $(basename "$rmd_file")"
    
    # Ejecutar análisis visual usando R bridge
    local r_script="$SCRIPT_DIR/core/visual_bridge.R"
    
    if [ ! -f "$r_script" ]; then
        log "ERROR" "Script R bridge no encontrado: $r_script"
        return 1
    fi
    
    # Ejecutar análisis
    local start_time=$(date +%s)
    
    if Rscript "$r_script" "$rmd_file" > "$output_file" 2>> "$LOG_FILE"; then
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))
        
        log "SUCCESS" "Análisis visual completado en ${duration}s"
        return 0
    else
        log "ERROR" "Error en análisis visual"
        return 1
    fi
}

# Generar reporte
generate_report() {
    local results_file="$1"
    local format="$2"
    local output_dir="$3"
    
    log "INFO" "Generando reporte en formato: $format"
    
    case "$format" in
        "console")
            generate_console_report "$results_file"
            ;;
        "html")
            generate_html_report "$results_file" "$output_dir"
            ;;
        "json")
            cat "$results_file"
            ;;
        *)
            log "ERROR" "Formato de reporte no soportado: $format"
            return 1
            ;;
    esac
}

# Generar reporte de consola
generate_console_report() {
    local results_file="$1"
    
    echo
    echo -e "${PURPLE}╔══════════════════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${PURPLE}║                          REPORTE DE ANÁLISIS VISUAL                         ║${NC}"
    echo -e "${PURPLE}╚══════════════════════════════════════════════════════════════════════════════╝${NC}"
    echo
    
    # Procesar resultados JSON
    if command -v jq &> /dev/null; then
        # Usar jq si está disponible
        local error_status=$(jq -r '.error' "$results_file" 2>/dev/null || echo "true")
        
        if [ "$error_status" = "true" ]; then
            local error_msg=$(jq -r '.message' "$results_file" 2>/dev/null || echo "Error desconocido")
            log "ERROR" "$error_msg"
        else
            local graphics_count=$(jq -r '.visual_analysis.summary.graphics_count' "$results_file" 2>/dev/null || echo "0")
            local visual_errors=$(jq -r '.visual_analysis.summary.visual_errors_count' "$results_file" 2>/dev/null || echo "0")
            local math_errors=$(jq -r '.visual_analysis.summary.math_errors_count' "$results_file" 2>/dev/null || echo "0")
            local status=$(jq -r '.visual_analysis.summary.status' "$results_file" 2>/dev/null || echo "unknown")
            
            echo -e "${BLUE}📊 RESUMEN:${NC}"
            echo "   Gráficas analizadas: $graphics_count"
            echo "   Errores visuales: $visual_errors"
            echo "   Errores matemáticos: $math_errors"
            echo "   Estado: $status"
            echo
            
            # Mostrar hallazgos
            local findings_count=$(jq -r '.visual_analysis.findings | length' "$results_file" 2>/dev/null || echo "0")
            
            if [ "$findings_count" -gt 0 ]; then
                echo -e "${YELLOW}🔍 HALLAZGOS:${NC}"
                jq -r '.visual_analysis.findings[] | "   • [\(.severity | ascii_upcase)] \(.message)"' "$results_file" 2>/dev/null || echo "   Error procesando hallazgos"
            else
                log "SUCCESS" "No se encontraron problemas visuales"
            fi
        fi
    else
        # Fallback sin jq
        log "INFO" "Resultados guardados en: $results_file"
        echo "Instalar 'jq' para ver reporte formateado en consola"
    fi
    
    echo
}

# Generar reporte HTML
generate_html_report() {
    local results_file="$1"
    local output_dir="$2"
    local timestamp=$(date '+%Y%m%d_%H%M%S')
    local html_file="$output_dir/reporte_visual_${timestamp}.html"
    
    # TODO: Implementar generador HTML
    log "INFO" "Generador HTML en desarrollo"
    log "INFO" "Resultados JSON disponibles en: $results_file"
    
    # Por ahora, copiar JSON
    cp "$results_file" "$output_dir/resultados_${timestamp}.json"
    log "SUCCESS" "Resultados guardados en: $output_dir/resultados_${timestamp}.json"
}

# Función principal
main() {
    # Limpiar log anterior
    > "$LOG_FILE"
    
    show_banner
    
    # Verificar dependencias
    check_dependencies
    
    # Configurar entorno
    setup_environment
    
    # Parsear argumentos
    parse_arguments "$@"
    
    log "INFO" "Iniciando Revisor-Rmd 2.0 con Análisis Visual IA"
    log "INFO" "Archivo objetivo: $TARGET_FILE"
    log "INFO" "Directorio de salida: $OUTPUT_DIR"
    
    # Ejecutar análisis
    local results_file="$OUTPUT_DIR/visual_results_$(date +%s).json"
    
    if run_visual_analysis "$TARGET_FILE" "$results_file"; then
        # Generar reporte
        generate_report "$results_file" "$FORMAT" "$OUTPUT_DIR"
        
        log "SUCCESS" "Análisis completado exitosamente"
        exit 0
    else
        log "ERROR" "Error en el análisis"
        exit 1
    fi
}

# Ejecutar función principal
main "$@"
