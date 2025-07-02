#!/bin/bash

# =============================================================================
# SCRIPT: estructura-actual-repositorio.sh
# DESCRIPCIÓN: Automatiza la generación de documentación de estructura del repositorio
# AUTOR: Proyecto ICFES R-Exams
# FECHA: $(date +"%Y-%m-%d")
# VERSIÓN: 1.0
# =============================================================================

# Configuración de colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Configuración de archivos
ESTRUCTURA_FILE="Estructura_Repositorio.md"
TEMP_TREE_FILE="temp_tree_output.txt"
BACKUP_DIR="backups"

# =============================================================================
# FUNCIONES AUXILIARES
# =============================================================================

print_header() {
    echo -e "${CYAN}=============================================================================${NC}"
    echo -e "${CYAN}  GENERADOR AUTOMÁTICO DE ESTRUCTURA DEL REPOSITORIO${NC}"
    echo -e "${CYAN}  Proyecto: ICFES R-Exams Matemáticas${NC}"
    echo -e "${CYAN}  Fecha: $(date '+%Y-%m-%d %H:%M:%S')${NC}"
    echo -e "${CYAN}=============================================================================${NC}"
}

print_step() {
    echo -e "${BLUE}[PASO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[ÉXITO]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[ADVERTENCIA]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Verificar dependencias
check_dependencies() {
    print_step "Verificando dependencias..."
    
    local missing_deps=()
    
    if ! command -v tree &> /dev/null; then
        missing_deps+=("tree")
    fi
    
    if ! command -v find &> /dev/null; then
        missing_deps+=("find")
    fi
    
    if ! command -v wc &> /dev/null; then
        missing_deps+=("wc")
    fi
    
    if [ ${#missing_deps[@]} -ne 0 ]; then
        print_error "Dependencias faltantes: ${missing_deps[*]}"
        echo "Instalar con: sudo apt-get install ${missing_deps[*]}"
        exit 1
    fi
    
    print_success "Todas las dependencias están disponibles"
}

# Crear backup del archivo existente
create_backup() {
    if [ -f "$ESTRUCTURA_FILE" ]; then
        print_step "Creando backup del archivo existente..."
        
        # Crear directorio de backup si no existe
        mkdir -p "$BACKUP_DIR"
        
        # Crear backup con timestamp
        local timestamp=$(date +"%Y%m%d_%H%M%S")
        local backup_file="${BACKUP_DIR}/${ESTRUCTURA_FILE%.md}_${timestamp}.md"
        
        cp "$ESTRUCTURA_FILE" "$backup_file"
        print_success "Backup creado: $backup_file"
    fi
}

# Generar estructura con tree
generate_tree_structure() {
    print_step "Generando estructura de directorios..."
    
    # Generar tree excluyendo archivos comunes no deseados
    tree -d -I '__pycache__|*.pyc|.git|.DS_Store|Thumbs.db|*.tmp|*.temp' > "$TEMP_TREE_FILE"
    
    if [ $? -eq 0 ]; then
        print_success "Estructura de directorios generada"
    else
        print_error "Error al generar estructura con tree"
        exit 1
    fi
}

# Contar estadísticas del repositorio
count_statistics() {
    print_step "Calculando estadísticas del repositorio..."
    
    # Contar directorios
    local total_dirs=$(find . -type d | wc -l)
    total_dirs=$((total_dirs - 1)) # Excluir directorio actual
    
    # Contar archivos .Rmd
    local rmd_files=$(find . -name "*.Rmd" -type f | wc -l)
    
    # Contar archivos .R
    local r_files=$(find . -name "*.R" -type f | wc -l)
    
    # Contar directorios de salida
    local output_dirs=$(find . -name "salida" -type d | wc -l)
    
    # Contar directorios de ejercicios
    local exercise_dirs=$(find . -name "ejercicios" -type d | wc -l)
    
    # Exportar variables para uso en la plantilla
    export TOTAL_DIRS="$total_dirs"
    export RMD_FILES="$rmd_files"
    export R_FILES="$r_files"
    export OUTPUT_DIRS="$output_dirs"
    export EXERCISE_DIRS="$exercise_dirs"
    
    print_success "Estadísticas calculadas: $total_dirs directorios, $rmd_files archivos .Rmd"
}

# Detectar competencias implementadas
detect_competencias() {
    print_step "Detectando competencias implementadas..."
    
    local competencias=()
    
    # Buscar directorios principales
    if [ -d "01-Numeros-Reales" ]; then
        competencias+=("Pensamiento Numérico")
    fi
    
    if [ -d "02-Funciones" ]; then
        competencias+=("Pensamiento Variacional")
    fi
    
    if [ -d "03-Razones-Trigonometricas" ] || [ -d "04-Funciones_Identidades-Trigonometricas" ]; then
        competencias+=("Pensamiento Espacial y Métrico")
    fi
    
    if [ -d "05-Geometria-Analitica" ]; then
        competencias+=("Pensamiento Espacial")
    fi
    
    if [ -d "06-Estadística-Y-Probabilidad" ]; then
        competencias+=("Pensamiento Aleatorio")
    fi
    
    export COMPETENCIAS_DETECTADAS="${competencias[*]}"
    export NUM_COMPETENCIAS="${#competencias[@]}"
    
    print_success "Competencias detectadas: ${#competencias[@]}"
}

# Generar archivo Markdown
generate_markdown() {
    print_step "Generando archivo Markdown..."
    
    local current_date=$(date +"%Y-%m-%d")
    local current_time=$(date +"%H:%M:%S")
    
    cat > "$ESTRUCTURA_FILE" << EOF
# 📁 Estructura del Repositorio - Matemáticas ICFES R-Exams

## 🎯 Descripción General

Este repositorio contiene ejercicios matemáticos organizados por competencias del ICFES, implementados usando el framework R-exams. La estructura sigue una organización temática que abarca los principales componentes evaluados en las pruebas ICFES de matemáticas.

**Generado automáticamente el**: $current_date a las $current_time  
**Total de directorios**: $TOTAL_DIRS

---

## 📊 Estadísticas del Repositorio

| Métrica | Cantidad |
|---------|----------|
| 📁 Directorios totales | $TOTAL_DIRS |
| 📄 Archivos .Rmd | $RMD_FILES |
| 🔧 Archivos .R | $R_FILES |
| 📤 Directorios de salida | $OUTPUT_DIRS |
| 🎯 Directorios de ejercicios | $EXERCISE_DIRS |
| 🧠 Competencias detectadas | $NUM_COMPETENCIAS |

---

## 📊 Estructura Completa del Repositorio

\`\`\`
EOF

    # Agregar contenido del tree
    cat "$TEMP_TREE_FILE" >> "$ESTRUCTURA_FILE"
    
    cat >> "$ESTRUCTURA_FILE" << EOF
\`\`\`

---

## 🎯 Competencias ICFES Detectadas

EOF

    # Agregar competencias detectadas
    if [ -n "$COMPETENCIAS_DETECTADAS" ]; then
        IFS=' ' read -ra COMP_ARRAY <<< "$COMPETENCIAS_DETECTADAS"
        for comp in "${COMP_ARRAY[@]}"; do
            echo "- ✅ **$comp**" >> "$ESTRUCTURA_FILE"
        done
    else
        echo "- ⚠️ No se detectaron competencias implementadas" >> "$ESTRUCTURA_FILE"
    fi
    
    cat >> "$ESTRUCTURA_FILE" << EOF

---

## 📁 Convenciones de Estructura

### **Directorios Estándar por Ejercicio**
- \`docus/\` - Documentación del ejercicio
- \`ejercicios/\` - Archivos .Rmd del ejercicio
- \`salida/\` - Archivos generados (HTML, PDF, XML)
- \`images/\` - Recursos gráficos
- \`tikz_temp/\` - Archivos temporales de TikZ
- \`_snaps/\` - Snapshots de pruebas

### **Tipos de Ejercicios R-exams**
- \`schoice/\` - Selección única (single choice)
- \`mchoice/\` - Selección múltiple (multiple choice)
- \`cloze/\` - Ejercicios de completar
- \`num/\` - Respuesta numérica
- \`string/\` - Respuesta de texto

---

## 🔧 Información de Generación

- **Script**: estructura-actual-repositorio.sh
- **Fecha de generación**: $current_date
- **Hora de generación**: $current_time
- **Directorio de trabajo**: \$(pwd)
- **Usuario**: \$(whoami)

---

*Generado automáticamente por el script estructura-actual-repositorio.sh*
EOF

    print_success "Archivo Markdown generado: $ESTRUCTURA_FILE"
}

# Limpiar archivos temporales
cleanup() {
    print_step "Limpiando archivos temporales..."
    
    if [ -f "$TEMP_TREE_FILE" ]; then
        rm "$TEMP_TREE_FILE"
    fi
    
    print_success "Limpieza completada"
}

# Mostrar resumen final
show_summary() {
    echo
    echo -e "${PURPLE}=============================================================================${NC}"
    echo -e "${PURPLE}  RESUMEN DE GENERACIÓN${NC}"
    echo -e "${PURPLE}=============================================================================${NC}"
    echo -e "${GREEN}✅ Archivo generado:${NC} $ESTRUCTURA_FILE"
    echo -e "${GREEN}✅ Directorios analizados:${NC} $TOTAL_DIRS"
    echo -e "${GREEN}✅ Archivos .Rmd encontrados:${NC} $RMD_FILES"
    echo -e "${GREEN}✅ Competencias detectadas:${NC} $NUM_COMPETENCIAS"
    
    if [ -d "$BACKUP_DIR" ]; then
        echo -e "${YELLOW}📁 Backups disponibles en:${NC} $BACKUP_DIR/"
    fi
    
    echo -e "${PURPLE}=============================================================================${NC}"
}

# =============================================================================
# FUNCIÓN PRINCIPAL
# =============================================================================

main() {
    print_header
    
    # Verificar que estamos en el directorio correcto
    if [ ! -d "01-Numeros-Reales" ] && [ ! -d "02-Funciones" ] && [ ! -d "06-Estadística-Y-Probabilidad" ]; then
        print_warning "No se detectaron directorios principales del repositorio ICFES"
        print_warning "¿Está ejecutando el script desde el directorio raíz del repositorio?"
        read -p "¿Continuar de todas formas? (y/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            print_error "Operación cancelada por el usuario"
            exit 1
        fi
    fi
    
    # Ejecutar pasos principales
    check_dependencies
    create_backup
    generate_tree_structure
    count_statistics
    detect_competencias
    generate_markdown
    cleanup
    show_summary
    
    print_success "¡Proceso completado exitosamente!"
}

# =============================================================================
# MANEJO DE ARGUMENTOS Y EJECUCIÓN
# =============================================================================

# Mostrar ayuda
if [ "$1" = "--help" ] || [ "$1" = "-h" ]; then
    echo "Uso: $0 [opciones]"
    echo
    echo "Opciones:"
    echo "  -h, --help     Mostrar esta ayuda"
    echo "  --no-backup    No crear backup del archivo existente"
    echo
    echo "Descripción:"
    echo "  Genera automáticamente la documentación de estructura del repositorio"
    echo "  ICFES R-Exams en formato Markdown."
    echo
    exit 0
fi

# Verificar opción no-backup
if [ "$1" = "--no-backup" ]; then
    create_backup() {
        print_step "Saltando creación de backup (--no-backup especificado)"
    }
fi

# Ejecutar función principal
main

exit 0
