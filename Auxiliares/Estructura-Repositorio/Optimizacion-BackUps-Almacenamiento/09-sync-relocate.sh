#!/bin/bash

# ============================================================================
# DETECTOR Y ACTUALIZADOR DE ARCHIVOS REUBICADOS
# ============================================================================
# Descripción: Detecta archivos que han cambiado de ubicación y actualiza
#              la base de datos de sincronización para mantener los grupos
# Autor: Sistema de Sincronización Automática
# Fecha: $(date +%Y-%m-%d)
# ============================================================================

# Configuración
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_FILE="$SCRIPT_DIR/04-sync-config.conf"
DATABASE_FILE="$SCRIPT_DIR/05-sync-database.txt"
LOG_FILE="$SCRIPT_DIR/sync-relocate.log"
TEMP_DIR="$SCRIPT_DIR/temp-relocate"

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
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
        "SUCCESS") echo -e "${GREEN}[SUCCESS]${NC} $message" ;;
        *)         echo "$message" ;;
    esac
}

# Cargar configuración
load_config() {
    if [[ -f "$CONFIG_FILE" ]]; then
        source "$CONFIG_FILE"
    fi
}

# Inicializar directorio temporal
init_temp_dir() {
    mkdir -p "$TEMP_DIR"
}

# Encontrar archivos faltantes en la base de datos
find_missing_files() {
    local missing_files=()
    local missing_count=0
    
    echo -e "${CYAN}=== DETECTANDO ARCHIVOS FALTANTES ===${NC}"
    echo
    
    while IFS='=' read -r key path; do
        if [[ "$key" =~ ^FILE_.*_PATH$ ]] && [[ ! -f "$path" ]]; then
            missing_files+=("$path")
            ((missing_count++))
            echo -e "${RED}✗${NC} Faltante: $path"
        fi
    done < "$DATABASE_FILE"
    
    echo
    log_message "INFO" "Encontrados $missing_count archivos faltantes"
    
    # Guardar lista de archivos faltantes
    printf '%s\n' "${missing_files[@]}" > "$TEMP_DIR/missing_files.txt"
    
    return $missing_count
}

# Buscar archivos por nombre en el proyecto
search_files_by_name() {
    local filename="$1"
    local search_results=()
    
    # Construir comando find similar al detector principal
    local find_cmd="find \"$PROJECT_ROOT\" -type f -name \"$filename\""
    
    # Agregar exclusiones de directorios
    for exclude_dir in "${EXCLUDE_DIRS[@]}"; do
        find_cmd+=" -not -path \"*/$exclude_dir/*\""
    done
    
    # Ejecutar búsqueda
    while IFS= read -r -d '' file; do
        search_results+=("$file")
    done < <(eval "$find_cmd -print0" 2>/dev/null)
    
    printf '%s\n' "${search_results[@]}"
}

# Detectar posibles reubicaciones
detect_relocations() {
    local relocations_found=0
    
    echo -e "${CYAN}=== DETECTANDO POSIBLES REUBICACIONES ===${NC}"
    echo
    
    if [[ ! -f "$TEMP_DIR/missing_files.txt" ]]; then
        log_message "ERROR" "Lista de archivos faltantes no encontrada"
        return 1
    fi
    
    while IFS= read -r missing_path; do
        if [[ -n "$missing_path" ]]; then
            local filename=$(basename "$missing_path")
            local old_dir=$(dirname "$missing_path")
            
            echo -e "${BLUE}Buscando:${NC} $filename (antes en $old_dir)"
            
            # Buscar archivos con el mismo nombre
            local candidates=($(search_files_by_name "$filename"))
            local found_candidates=0
            
            for candidate in "${candidates[@]}"; do
                local candidate_dir=$(dirname "$candidate")
                
                # Verificar que no sea la ubicación original
                if [[ "$candidate" != "$missing_path" ]]; then
                    # Verificar que no esté ya en la base de datos
                    if ! grep -q "FILE_.*_PATH=$candidate" "$DATABASE_FILE"; then
                        echo -e "${GREEN}  ✓ Posible nueva ubicación:${NC} $candidate"
                        
                        # Guardar información de reubicación
                        echo "$missing_path|$candidate" >> "$TEMP_DIR/relocations.txt"
                        ((found_candidates++))
                        ((relocations_found++))
                    else
                        echo -e "${YELLOW}  ⚠ Ya en base de datos:${NC} $candidate"
                    fi
                fi
            done
            
            if [[ $found_candidates -eq 0 ]]; then
                echo -e "${RED}  ✗ No se encontraron candidatos${NC}"
            fi
            
            echo
        fi
    done < "$TEMP_DIR/missing_files.txt"
    
    log_message "INFO" "Detectadas $relocations_found posibles reubicaciones"
    return $relocations_found
}

# Mostrar reubicaciones detectadas
show_relocations() {
    if [[ ! -f "$TEMP_DIR/relocations.txt" ]]; then
        echo -e "${YELLOW}No se detectaron reubicaciones${NC}"
        return 1
    fi
    
    echo -e "${CYAN}=== REUBICACIONES DETECTADAS ===${NC}"
    echo
    
    local count=1
    while IFS='|' read -r old_path new_path; do
        echo -e "${BLUE}[$count]${NC} Reubicación detectada:"
        echo -e "    ${RED}Anterior:${NC} $old_path"
        echo -e "    ${GREEN}Nueva:${NC} $new_path"
        echo
        ((count++))
    done < "$TEMP_DIR/relocations.txt"
}

# Confirmar reubicaciones interactivamente
confirm_relocations() {
    if [[ ! -f "$TEMP_DIR/relocations.txt" ]]; then
        return 1
    fi
    
    echo -e "${CYAN}=== CONFIRMACIÓN DE REUBICACIONES ===${NC}"
    echo
    
    local confirmed_relocations=()
    local count=1
    
    while IFS='|' read -r old_path new_path; do
        echo -e "${BLUE}[$count]${NC} ¿Confirmar reubicación?"
        echo -e "    ${RED}De:${NC} $old_path"
        echo -e "    ${GREEN}A:${NC} $new_path"
        
        read -p "¿Es correcta esta reubicación? (s/N): " confirm
        
        if [[ "$confirm" =~ ^[Ss]$ ]]; then
            confirmed_relocations+=("$old_path|$new_path")
            echo -e "${GREEN}✓ Confirmada${NC}"
        else
            echo -e "${YELLOW}✗ Omitida${NC}"
        fi
        
        echo
        ((count++))
    done < "$TEMP_DIR/relocations.txt"
    
    # Guardar reubicaciones confirmadas
    printf '%s\n' "${confirmed_relocations[@]}" > "$TEMP_DIR/confirmed_relocations.txt"
    
    local confirmed_count=${#confirmed_relocations[@]}
    log_message "INFO" "Confirmadas $confirmed_count reubicaciones"
    
    return $confirmed_count
}

# Actualizar base de datos con reubicaciones
update_database() {
    if [[ ! -f "$TEMP_DIR/confirmed_relocations.txt" ]]; then
        log_message "ERROR" "No hay reubicaciones confirmadas para actualizar"
        return 1
    fi
    
    echo -e "${CYAN}=== ACTUALIZANDO BASE DE DATOS ===${NC}"
    echo
    
    # Crear backup de la base de datos
    local backup_file="$SCRIPT_DIR/backups/database-backup-$(date +%Y%m%d_%H%M%S).txt"
    mkdir -p "$SCRIPT_DIR/backups"
    cp "$DATABASE_FILE" "$backup_file"
    log_message "INFO" "Backup creado: $backup_file"
    
    # Crear nueva base de datos
    local temp_db=$(mktemp)
    local updates_made=0
    
    while IFS='|' read -r old_path new_path; do
        if [[ -n "$old_path" ]] && [[ -n "$new_path" ]]; then
            # Reemplazar ruta antigua con nueva
            sed "s|FILE_.*_PATH=$old_path|FILE_${RANDOM}_PATH=$new_path|g" "$DATABASE_FILE" > "$temp_db"
            cp "$temp_db" "$DATABASE_FILE"
            
            echo -e "${GREEN}✓ Actualizado:${NC} $(basename "$old_path")"
            echo -e "    ${BLUE}Nueva ubicación:${NC} $new_path"
            
            ((updates_made++))
            log_message "INFO" "Actualizada ruta: $old_path -> $new_path"
        fi
    done < "$TEMP_DIR/confirmed_relocations.txt"
    
    rm -f "$temp_db"
    
    echo
    log_message "SUCCESS" "Base de datos actualizada: $updates_made cambios realizados"
    
    return $updates_made
}

# Verificar integridad después de actualización
verify_integrity() {
    echo -e "${CYAN}=== VERIFICANDO INTEGRIDAD ===${NC}"
    echo
    
    local total_files=0
    local existing_files=0
    local missing_files=0
    
    while IFS='=' read -r key path; do
        if [[ "$key" =~ ^FILE_.*_PATH$ ]]; then
            ((total_files++))
            if [[ -f "$path" ]]; then
                ((existing_files++))
            else
                ((missing_files++))
                echo -e "${RED}✗ Aún faltante:${NC} $path"
            fi
        fi
    done < "$DATABASE_FILE"
    
    echo
    echo -e "${BLUE}Resumen de integridad:${NC}"
    echo "  Total de archivos: $total_files"
    echo "  Archivos existentes: $existing_files"
    echo "  Archivos faltantes: $missing_files"
    
    if [[ $missing_files -eq 0 ]]; then
        log_message "SUCCESS" "Verificación de integridad exitosa"
        return 0
    else
        log_message "WARNING" "Quedan $missing_files archivos faltantes"
        return 1
    fi
}

# Función principal
main() {
    local action="${1:-interactive}"
    
    load_config
    init_temp_dir
    
    case "$action" in
        "detect"|"--detect")
            find_missing_files
            if [[ $? -gt 0 ]]; then
                detect_relocations
                show_relocations
            else
                echo -e "${GREEN}✓ No hay archivos faltantes${NC}"
            fi
            ;;
        "update"|"--update")
            find_missing_files
            if [[ $? -gt 0 ]]; then
                detect_relocations
                if [[ $? -gt 0 ]]; then
                    show_relocations
                    confirm_relocations
                    if [[ $? -gt 0 ]]; then
                        update_database
                        verify_integrity
                    fi
                fi
            else
                echo -e "${GREEN}✓ No hay archivos faltantes${NC}"
            fi
            ;;
        "interactive"|"--interactive")
            echo -e "${CYAN}=== DETECTOR DE ARCHIVOS REUBICADOS ===${NC}"
            echo
            
            find_missing_files
            if [[ $? -gt 0 ]]; then
                echo
                read -p "¿Buscar posibles reubicaciones? (S/n): " search_confirm
                
                if [[ ! "$search_confirm" =~ ^[Nn]$ ]]; then
                    detect_relocations
                    if [[ $? -gt 0 ]]; then
                        show_relocations
                        echo
                        read -p "¿Actualizar base de datos con estas reubicaciones? (s/N): " update_confirm
                        
                        if [[ "$update_confirm" =~ ^[Ss]$ ]]; then
                            confirm_relocations
                            if [[ $? -gt 0 ]]; then
                                update_database
                                verify_integrity
                            fi
                        fi
                    else
                        echo -e "${YELLOW}No se detectaron reubicaciones automáticamente${NC}"
                    fi
                fi
            else
                echo -e "${GREEN}✓ Todos los archivos están en sus ubicaciones registradas${NC}"
            fi
            ;;
        "help"|"--help")
            echo "Detector y Actualizador de Archivos Reubicados"
            echo
            echo "Uso: $0 [comando]"
            echo
            echo "Comandos:"
            echo "  detect      - Solo detectar archivos faltantes y posibles reubicaciones"
            echo "  update      - Detectar y actualizar base de datos automáticamente"
            echo "  interactive - Modo interactivo con confirmaciones (por defecto)"
            echo "  help        - Mostrar esta ayuda"
            echo
            echo "Ejemplos:"
            echo "  $0                    # Modo interactivo"
            echo "  $0 detect             # Solo detectar"
            echo "  $0 update             # Detectar y actualizar"
            ;;
        *)
            log_message "ERROR" "Comando desconocido: $action"
            main "help"
            exit 1
            ;;
    esac
}

# Limpiar al salir
cleanup() {
    rm -rf "$TEMP_DIR"
}

trap cleanup EXIT

# Ejecutar función principal
main "$@"
