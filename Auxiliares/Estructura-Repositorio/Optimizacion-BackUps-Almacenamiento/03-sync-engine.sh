#!/bin/bash

# ============================================================================
# MOTOR DE SINCRONIZACIÓN AUTOMÁTICA
# ============================================================================
# Descripción: Ejecuta el renombrado sincronizado de archivos duplicados
#              con manejo de conflictos y sistema de locks
# Autor: Sistema de Sincronización Automática
# Fecha: $(date +%Y-%m-%d)
# ============================================================================

# Configuración
PROJECT_ROOT="/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_FILE="$SCRIPT_DIR/04-sync-config.conf"
DATABASE_FILE="$SCRIPT_DIR/05-sync-database.txt"
LOG_FILE="$SCRIPT_DIR/sync-operations.log"
ERROR_LOG="$SCRIPT_DIR/sync-errors.log"
LOCK_DIR="$SCRIPT_DIR/locks"
BACKUP_DIR="$SCRIPT_DIR/backups"

# Variables de control
DRY_RUN=false
FORCE_SYNC=false
MAX_RETRIES=3
LOCK_TIMEOUT=30

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
    
    # Log principal
    echo "[$timestamp] [$level] $message" >> "$LOG_FILE"
    
    # Log de errores separado
    if [[ "$level" == "ERROR" ]]; then
        echo "[$timestamp] $message" >> "$ERROR_LOG"
    fi
    
    # Output a consola
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
        BACKUP_BEFORE_SYNC=true
        VERIFY_AFTER_SYNC=true
        MAX_RETRIES=3
        LOCK_TIMEOUT=30
    fi
}

# Inicializar directorios necesarios
init_directories() {
    mkdir -p "$LOCK_DIR" "$BACKUP_DIR"
    
    if [[ ! -d "$LOCK_DIR" ]] || [[ ! -d "$BACKUP_DIR" ]]; then
        log_message "ERROR" "No se pudieron crear directorios necesarios"
        exit 1
    fi
}

# Sistema de locks para evitar conflictos
acquire_lock() {
    local lock_name="$1"
    local lock_file="$LOCK_DIR/$lock_name.lock"
    local timeout="$LOCK_TIMEOUT"
    
    log_message "DEBUG" "Intentando adquirir lock: $lock_name"
    
    local count=0
    while [[ $count -lt $timeout ]]; do
        if (set -C; echo $$ > "$lock_file") 2>/dev/null; then
            log_message "DEBUG" "Lock adquirido: $lock_name"
            return 0
        fi
        
        # Verificar si el proceso que tiene el lock sigue corriendo
        if [[ -f "$lock_file" ]]; then
            local lock_pid=$(cat "$lock_file" 2>/dev/null)
            if [[ -n "$lock_pid" ]] && ! kill -0 "$lock_pid" 2>/dev/null; then
                log_message "WARNING" "Lock obsoleto detectado, eliminando: $lock_name"
                rm -f "$lock_file"
                continue
            fi
        fi
        
        sleep 1
        ((count++))
    done
    
    log_message "ERROR" "No se pudo adquirir lock después de $timeout segundos: $lock_name"
    return 1
}

# Liberar lock
release_lock() {
    local lock_name="$1"
    local lock_file="$LOCK_DIR/$lock_name.lock"
    
    if [[ -f "$lock_file" ]]; then
        rm -f "$lock_file"
        log_message "DEBUG" "Lock liberado: $lock_name"
    fi
}

# Crear backup de archivo
create_backup() {
    local filepath="$1"
    local backup_timestamp=$(date '+%Y%m%d_%H%M%S')
    local filename=$(basename "$filepath")
    local backup_path="$BACKUP_DIR/${filename}_${backup_timestamp}"
    
    if [[ "$BACKUP_BEFORE_SYNC" == "true" ]] && [[ -f "$filepath" ]]; then
        if cp "$filepath" "$backup_path"; then
            log_message "INFO" "Backup creado: $backup_path"
            return 0
        else
            log_message "ERROR" "No se pudo crear backup de: $filepath"
            return 1
        fi
    fi
    
    return 0
}

# Encontrar grupo de sincronización para un archivo
find_sync_group() {
    local filepath="$1"
    local group_id=""
    local current_group=""
    
    while IFS='=' read -r key value; do
        if [[ "$key" == "GROUP_ID" ]]; then
            current_group="$value"
        elif [[ "$key" =~ ^FILE_.*_PATH$ ]] && [[ "$value" == "$filepath" ]]; then
            group_id="$current_group"
            break
        fi
    done < "$DATABASE_FILE"
    
    echo "$group_id"
}

# Obtener archivos de un grupo
get_group_files() {
    local group_id="$1"
    local files=()
    local in_group=false
    
    while IFS='=' read -r key value; do
        if [[ "$key" == "GROUP_ID" ]] && [[ "$value" == "$group_id" ]]; then
            in_group=true
        elif [[ "$key" == "GROUP_ID" ]] && [[ "$value" != "$group_id" ]]; then
            in_group=false
        elif [[ "$in_group" == "true" ]] && [[ "$key" =~ ^FILE_.*_PATH$ ]]; then
            files+=("$value")
        fi
    done < "$DATABASE_FILE"
    
    printf '%s\n' "${files[@]}"
}

# Validar nuevo nombre de archivo
validate_new_name() {
    local new_path="$1"
    local group_files=("${@:2}")
    
    # Verificar que el nuevo nombre no cause conflictos
    for file in "${group_files[@]}"; do
        local dir=$(dirname "$file")
        local potential_conflict="$dir/$(basename "$new_path")"
        
        if [[ "$potential_conflict" != "$new_path" ]] && [[ -f "$potential_conflict" ]]; then
            log_message "ERROR" "Conflicto de nombres: $potential_conflict ya existe"
            return 1
        fi
    done
    
    # Verificar caracteres válidos
    local new_filename=$(basename "$new_path")
    if [[ "$new_filename" =~ [[:cntrl:]] ]] || [[ "$new_filename" =~ [/\\:*?\"<>|] ]]; then
        log_message "ERROR" "Nombre de archivo contiene caracteres inválidos: $new_filename"
        return 1
    fi
    
    return 0
}

# Ejecutar sincronización de renombrado
execute_sync_rename() {
    local old_path="$1"
    local new_path="$2"
    local group_id=$(find_sync_group "$old_path")
    
    if [[ -z "$group_id" ]]; then
        log_message "ERROR" "Archivo no encontrado en base de datos: $old_path"
        return 1
    fi
    
    log_message "INFO" "Iniciando sincronización para grupo $group_id"
    
    # Adquirir lock para el grupo
    local lock_name="group_${group_id}"
    if ! acquire_lock "$lock_name"; then
        return 1
    fi
    
    # Obtener todos los archivos del grupo
    local group_files=($(get_group_files "$group_id"))
    local old_filename=$(basename "$old_path")
    local new_filename=$(basename "$new_path")
    
    log_message "INFO" "Sincronizando ${#group_files[@]} archivos: $old_filename -> $new_filename"
    
    # Validar nuevo nombre
    if ! validate_new_name "$new_path" "${group_files[@]}"; then
        release_lock "$lock_name"
        return 1
    fi
    
    # Crear backups si está habilitado
    local backup_success=true
    if [[ "$BACKUP_BEFORE_SYNC" == "true" ]]; then
        for file in "${group_files[@]}"; do
            if [[ -f "$file" ]] && ! create_backup "$file"; then
                backup_success=false
                break
            fi
        done
        
        if [[ "$backup_success" == "false" ]]; then
            log_message "ERROR" "Falló la creación de backups, abortando sincronización"
            release_lock "$lock_name"
            return 1
        fi
    fi
    
    # Ejecutar renombrados
    local success_count=0
    local error_count=0
    local renamed_files=()
    
    for file in "${group_files[@]}"; do
        if [[ -f "$file" ]]; then
            local dir=$(dirname "$file")
            local new_file_path="$dir/$new_filename"
            
            if [[ "$file" == "$old_path" ]]; then
                # Este archivo ya fue renombrado por el usuario
                renamed_files+=("$new_file_path")
                ((success_count++))
                continue
            fi
            
            log_message "INFO" "Renombrando: $file -> $new_file_path"
            
            if [[ "$DRY_RUN" == "true" ]]; then
                log_message "INFO" "[DRY RUN] mv \"$file\" \"$new_file_path\""
                renamed_files+=("$new_file_path")
                ((success_count++))
            else
                if mv "$file" "$new_file_path"; then
                    renamed_files+=("$new_file_path")
                    ((success_count++))
                    log_message "INFO" "Renombrado exitoso: $new_file_path"
                else
                    ((error_count++))
                    log_message "ERROR" "Falló renombrado: $file -> $new_file_path"
                fi
            fi
        else
            log_message "WARNING" "Archivo no encontrado: $file"
            ((error_count++))
        fi
    done
    
    # Actualizar base de datos
    if [[ $success_count -gt 0 ]] && [[ "$DRY_RUN" == "false" ]]; then
        update_database_after_rename "$group_id" "$old_filename" "$new_filename" "${renamed_files[@]}"
    fi
    
    # Liberar lock
    release_lock "$lock_name"
    
    # Reporte final
    log_message "INFO" "Sincronización completada: $success_count éxitos, $error_count errores"
    
    if [[ $error_count -eq 0 ]]; then
        return 0
    else
        return 1
    fi
}

# Actualizar base de datos después del renombrado
update_database_after_rename() {
    local group_id="$1"
    local old_filename="$2"
    local new_filename="$3"
    local renamed_files=("${@:4}")
    
    log_message "INFO" "Actualizando base de datos para grupo $group_id"
    
    # Crear archivo temporal para la nueva base de datos
    local temp_db=$(mktemp)
    local in_group=false
    local file_index=1
    
    while IFS='=' read -r key value; do
        if [[ "$key" == "GROUP_ID" ]] && [[ "$value" == "$group_id" ]]; then
            in_group=true
            echo "$key=$value" >> "$temp_db"
        elif [[ "$key" == "GROUP_ID" ]] && [[ "$value" != "$group_id" ]]; then
            in_group=false
            echo "$key=$value" >> "$temp_db"
            file_index=1
        elif [[ "$in_group" == "true" ]]; then
            if [[ "$key" == "FILENAME" ]]; then
                echo "FILENAME=$new_filename" >> "$temp_db"
            elif [[ "$key" =~ ^FILE_.*_PATH$ ]]; then
                if [[ $file_index -le ${#renamed_files[@]} ]]; then
                    echo "$key=${renamed_files[$((file_index-1))]}" >> "$temp_db"
                    ((file_index++))
                fi
            else
                echo "$key=$value" >> "$temp_db"
            fi
        else
            echo "$key=$value" >> "$temp_db"
        fi
    done < "$DATABASE_FILE"
    
    # Reemplazar base de datos original
    if mv "$temp_db" "$DATABASE_FILE"; then
        log_message "INFO" "Base de datos actualizada exitosamente"
    else
        log_message "ERROR" "No se pudo actualizar la base de datos"
        rm -f "$temp_db"
        return 1
    fi
}

# Verificar integridad después de sincronización
verify_sync_integrity() {
    local group_id="$1"
    local group_files=($(get_group_files "$group_id"))
    local missing_files=()
    
    for file in "${group_files[@]}"; do
        if [[ ! -f "$file" ]]; then
            missing_files+=("$file")
        fi
    done
    
    if [[ ${#missing_files[@]} -gt 0 ]]; then
        log_message "ERROR" "Archivos faltantes después de sincronización:"
        for missing in "${missing_files[@]}"; do
            log_message "ERROR" "  - $missing"
        done
        return 1
    fi
    
    log_message "INFO" "Verificación de integridad exitosa para grupo $group_id"
    return 0
}

# Función principal
main() {
    local action="$1"
    
    load_config
    init_directories
    
    case "$action" in
        "--sync-rename")
            local old_path="$2"
            local new_path="$3"
            
            if [[ -z "$old_path" ]] || [[ -z "$new_path" ]]; then
                log_message "ERROR" "Uso: $0 --sync-rename <ruta_antigua> <ruta_nueva>"
                exit 1
            fi
            
            log_message "INFO" "Iniciando sincronización de renombrado"
            execute_sync_rename "$old_path" "$new_path"
            ;;
        "--dry-run")
            DRY_RUN=true
            log_message "INFO" "Modo DRY RUN activado"
            main "${@:2}"
            ;;
        "--force")
            FORCE_SYNC=true
            log_message "INFO" "Modo FORCE activado"
            main "${@:2}"
            ;;
        "help"|"--help")
            echo "Uso: $0 [opciones] <acción> [argumentos]"
            echo ""
            echo "Acciones:"
            echo "  --sync-rename <old> <new>  Sincronizar renombrado de archivo"
            echo ""
            echo "Opciones:"
            echo "  --dry-run                  Mostrar qué se haría sin ejecutar"
            echo "  --force                    Forzar operación ignorando advertencias"
            echo "  --help                     Mostrar esta ayuda"
            ;;
        *)
            log_message "ERROR" "Acción desconocida: $action"
            main "help"
            exit 1
            ;;
    esac
}

# Ejecutar función principal
main "$@"
