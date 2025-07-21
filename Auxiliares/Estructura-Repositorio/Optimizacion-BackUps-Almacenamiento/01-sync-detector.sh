#!/bin/bash

# ============================================================================
# DETECTOR DE ARCHIVOS DUPLICADOS PARA SINCRONIZACIÓN
# ============================================================================
# Descripción: Escanea el proyecto buscando archivos con nombres idénticos
#              y crea grupos de sincronización automática
# Autor: Sistema de Sincronización Automática
# Fecha: $(date +%Y-%m-%d)
# ============================================================================

# Configuración
PROJECT_ROOT="/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_FILE="$SCRIPT_DIR/04-sync-config.conf"
DATABASE_FILE="$SCRIPT_DIR/05-sync-database.txt"
LOG_FILE="$SCRIPT_DIR/sync-detector.log"

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Función de logging
log_message() {
    local level="$1"
    local message="$2"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "[$timestamp] [$level] $message" >> "$LOG_FILE"
    
    case $level in
        "ERROR")   echo -e "${RED}[ERROR]${NC} $message" ;;
        "WARNING") echo -e "${YELLOW}[WARNING]${NC} $message" ;;
        "INFO")    echo -e "${GREEN}[INFO]${NC} $message" ;;
        "DEBUG")   echo -e "${BLUE}[DEBUG]${NC} $message" ;;
        *)         echo "$message" ;;
    esac
}

# Cargar configuración
load_config() {
    if [[ -f "$CONFIG_FILE" ]]; then
        source "$CONFIG_FILE"
        log_message "INFO" "Configuración cargada desde $CONFIG_FILE"
    else
        log_message "WARNING" "Archivo de configuración no encontrado, usando valores por defecto"
        # Valores por defecto
        INCLUDE_PATTERNS=("*.Rmd" "*.R" "*.md" "*.txt" "*.sh" "*.py")
        EXCLUDE_DIRS=(".git" "node_modules" ".Rproj.user" "rsconnect" ".vscode")
        MIN_FILE_SIZE=1
        MAX_FILE_SIZE=104857600  # 100MB
    fi
}

# Verificar dependencias
check_dependencies() {
    local deps=("find" "sort" "uniq" "wc" "stat")
    local missing=()
    
    for dep in "${deps[@]}"; do
        if ! command -v "$dep" &> /dev/null; then
            missing+=("$dep")
        fi
    done
    
    if [[ ${#missing[@]} -gt 0 ]]; then
        log_message "ERROR" "Dependencias faltantes: ${missing[*]}"
        exit 1
    fi
}

# Construir comando find con exclusiones
build_find_command() {
    local find_cmd="find \"$PROJECT_ROOT\" -type f"
    
    # Agregar exclusiones de directorios
    for exclude_dir in "${EXCLUDE_DIRS[@]}"; do
        find_cmd+=" -not -path \"*/$exclude_dir/*\""
    done
    
    # Agregar patrones de inclusión
    if [[ ${#INCLUDE_PATTERNS[@]} -gt 0 ]]; then
        find_cmd+=" \\("
        for i in "${!INCLUDE_PATTERNS[@]}"; do
            if [[ $i -gt 0 ]]; then
                find_cmd+=" -o"
            fi
            find_cmd+=" -name \"${INCLUDE_PATTERNS[$i]}\""
        done
        find_cmd+=" \\)"
    fi
    
    # Filtros de tamaño
    find_cmd+=" -size +${MIN_FILE_SIZE}c -size -${MAX_FILE_SIZE}c"
    
    echo "$find_cmd"
}

# Encontrar archivos duplicados por nombre
find_duplicate_files() {
    log_message "INFO" "Iniciando búsqueda de archivos duplicados..."
    
    local find_cmd=$(build_find_command)
    local temp_file=$(mktemp)
    local duplicates_file=$(mktemp)
    
    # Ejecutar búsqueda y procesar resultados
    eval "$find_cmd" | while read -r filepath; do
        if [[ -f "$filepath" ]]; then
            local filename=$(basename "$filepath")
            local filesize=$(stat -f%z "$filepath" 2>/dev/null || stat -c%s "$filepath" 2>/dev/null || echo "0")
            local modified=$(stat -f%m "$filepath" 2>/dev/null || stat -c%Y "$filepath" 2>/dev/null || echo "0")
            
            echo "$filename|$filepath|$filesize|$modified" >> "$temp_file"
        fi
    done
    
    # Encontrar nombres duplicados
    cut -d'|' -f1 "$temp_file" | sort | uniq -d > "$duplicates_file"
    
    local duplicate_count=$(wc -l < "$duplicates_file")
    log_message "INFO" "Encontrados $duplicate_count nombres de archivo duplicados"
    
    # Procesar cada grupo de duplicados
    local group_id=1
    while read -r duplicate_name; do
        if [[ -n "$duplicate_name" ]]; then
            process_duplicate_group "$duplicate_name" "$temp_file" "$group_id"
            ((group_id++))
        fi
    done < "$duplicates_file"
    
    # Limpiar archivos temporales
    rm -f "$temp_file" "$duplicates_file"
    
    log_message "INFO" "Procesamiento de duplicados completado"
}

# Procesar un grupo de archivos duplicados
process_duplicate_group() {
    local filename="$1"
    local temp_file="$2"
    local group_id="$3"
    
    log_message "DEBUG" "Procesando grupo $group_id: $filename"
    
    # Extraer todos los archivos con este nombre
    local group_files=$(grep "^$filename|" "$temp_file")
    local file_count=$(echo "$group_files" | wc -l)
    
    if [[ $file_count -gt 1 ]]; then
        # Crear entrada en la base de datos
        local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
        
        echo "# Grupo de sincronización $group_id" >> "$DATABASE_FILE"
        echo "GROUP_ID=$group_id" >> "$DATABASE_FILE"
        echo "FILENAME=$filename" >> "$DATABASE_FILE"
        echo "FILE_COUNT=$file_count" >> "$DATABASE_FILE"
        echo "CREATED=$timestamp" >> "$DATABASE_FILE"
        echo "STATUS=ACTIVE" >> "$DATABASE_FILE"
        
        # Agregar cada archivo del grupo
        local file_index=1
        echo "$group_files" | while IFS='|' read -r fname fpath fsize fmodified; do
            echo "FILE_${file_index}_PATH=$fpath" >> "$DATABASE_FILE"
            echo "FILE_${file_index}_SIZE=$fsize" >> "$DATABASE_FILE"
            echo "FILE_${file_index}_MODIFIED=$fmodified" >> "$DATABASE_FILE"
            ((file_index++))
        done
        
        echo "" >> "$DATABASE_FILE"
        
        log_message "INFO" "Grupo $group_id creado: $filename ($file_count archivos)"
    fi
}

# Validar integridad de la base de datos
validate_database() {
    if [[ ! -f "$DATABASE_FILE" ]]; then
        log_message "WARNING" "Base de datos no existe, se creará una nueva"
        return 0
    fi
    
    log_message "INFO" "Validando integridad de la base de datos..."
    
    local groups=$(grep -c "^GROUP_ID=" "$DATABASE_FILE")
    local files_found=0
    local files_missing=0
    
    # Verificar que todos los archivos en la base de datos existan
    grep "^FILE_.*_PATH=" "$DATABASE_FILE" | while IFS='=' read -r key path; do
        if [[ -f "$path" ]]; then
            ((files_found++))
        else
            ((files_missing++))
            log_message "WARNING" "Archivo faltante: $path"
        fi
    done
    
    log_message "INFO" "Validación completada: $groups grupos, $files_found archivos encontrados, $files_missing faltantes"
}

# Mostrar estadísticas
show_statistics() {
    if [[ ! -f "$DATABASE_FILE" ]]; then
        log_message "INFO" "No hay base de datos disponible"
        return
    fi
    
    local total_groups=$(grep -c "^GROUP_ID=" "$DATABASE_FILE")
    local active_groups=$(grep -c "STATUS=ACTIVE" "$DATABASE_FILE")
    local total_files=$(grep -c "^FILE_.*_PATH=" "$DATABASE_FILE")
    
    echo
    echo "=== ESTADÍSTICAS DE SINCRONIZACIÓN ==="
    echo "Grupos totales: $total_groups"
    echo "Grupos activos: $active_groups"
    echo "Archivos monitoreados: $total_files"
    echo "Base de datos: $DATABASE_FILE"
    echo "Log: $LOG_FILE"
    echo
}

# Función principal
main() {
    local action="${1:-scan}"
    
    log_message "INFO" "Iniciando detector de archivos duplicados - Acción: $action"
    
    # Verificar que estamos en el directorio correcto
    if [[ ! -d "$PROJECT_ROOT" ]]; then
        log_message "ERROR" "Directorio del proyecto no encontrado: $PROJECT_ROOT"
        exit 1
    fi
    
    check_dependencies
    load_config
    
    case "$action" in
        "scan"|"--scan")
            # Crear nueva base de datos
            echo "# Base de datos de sincronización de archivos" > "$DATABASE_FILE"
            echo "# Generada el $(date)" >> "$DATABASE_FILE"
            echo "" >> "$DATABASE_FILE"
            
            find_duplicate_files
            validate_database
            show_statistics
            ;;
        "validate"|"--validate")
            validate_database
            ;;
        "status"|"--status")
            show_statistics
            ;;
        "rescan"|"--rescan")
            log_message "INFO" "Realizando re-escaneo completo..."
            rm -f "$DATABASE_FILE"
            main "scan"
            ;;
        "help"|"--help")
            echo "Uso: $0 [scan|validate|status|rescan|help]"
            echo "  scan     - Escanear y crear base de datos (por defecto)"
            echo "  validate - Validar integridad de base de datos existente"
            echo "  status   - Mostrar estadísticas actuales"
            echo "  rescan   - Eliminar base de datos y re-escanear"
            echo "  help     - Mostrar esta ayuda"
            ;;
        *)
            log_message "ERROR" "Acción desconocida: $action"
            main "help"
            exit 1
            ;;
    esac
    
    log_message "INFO" "Detector completado exitosamente"
}

# Ejecutar función principal con todos los argumentos
main "$@"
