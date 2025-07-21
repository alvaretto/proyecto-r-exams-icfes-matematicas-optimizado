#!/bin/bash

# ============================================================================
# SINCRONIZACIÓN MANUAL Y SELECCIÓN DE ARCHIVOS MAESTROS
# ============================================================================
# Descripción: Permite seleccionar manualmente qué archivos están más actualizados
#              y sincronizar específicamente antes de activar el sistema automático
# Autor: Sistema de Sincronización Automática
# Fecha: $(date +%Y-%m-%d)
# ============================================================================

# Configuración
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_FILE="$SCRIPT_DIR/04-sync-config.conf"
DATABASE_FILE="$SCRIPT_DIR/05-sync-database.txt"
LOG_FILE="$SCRIPT_DIR/sync-manual.log"
TEMP_DIR="$SCRIPT_DIR/temp-manual"

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
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

# Crear directorio temporal
init_temp_dir() {
    mkdir -p "$TEMP_DIR"
}

# Obtener información detallada de un archivo
get_file_info() {
    local filepath="$1"
    
    if [[ ! -f "$filepath" ]]; then
        echo "MISSING|0|0|0"
        return
    fi
    
    local size=$(stat -f%z "$filepath" 2>/dev/null || stat -c%s "$filepath" 2>/dev/null || echo "0")
    local modified=$(stat -f%m "$filepath" 2>/dev/null || stat -c%Y "$filepath" 2>/dev/null || echo "0")
    local modified_human=$(stat -f%Sm -t "%Y-%m-%d %H:%M:%S" "$filepath" 2>/dev/null || stat -c%y "$filepath" 2>/dev/null | cut -d'.' -f1 || echo "unknown")
    
    echo "EXISTS|$size|$modified|$modified_human"
}

# Comparar archivos de un grupo
compare_group_files() {
    local group_id="$1"
    local group_files=()
    local in_group=false
    local filename=""
    
    # Extraer archivos del grupo
    while IFS='=' read -r key value; do
        if [[ "$key" == "GROUP_ID" ]] && [[ "$value" == "$group_id" ]]; then
            in_group=true
        elif [[ "$key" == "GROUP_ID" ]] && [[ "$value" != "$group_id" ]]; then
            in_group=false
        elif [[ "$in_group" == "true" ]]; then
            if [[ "$key" == "FILENAME" ]]; then
                filename="$value"
            elif [[ "$key" =~ ^FILE_.*_PATH$ ]]; then
                group_files+=("$value")
            fi
        fi
    done < "$DATABASE_FILE"
    
    if [[ ${#group_files[@]} -eq 0 ]]; then
        log_message "ERROR" "No se encontraron archivos para el grupo $group_id"
        return 1
    fi
    
    echo -e "${CYAN}=== GRUPO $group_id: $filename ===${NC}"
    echo
    
    # Mostrar información de cada archivo
    local max_modified=0
    local newest_file=""
    local file_index=1
    
    for file in "${group_files[@]}"; do
        local info=$(get_file_info "$file")
        IFS='|' read -r status size modified modified_human <<< "$info"
        
        local dir=$(dirname "$file")
        local short_path=".../${dir##*/}/$(basename "$file")"
        
        if [[ "$status" == "EXISTS" ]]; then
            local size_kb=$((size / 1024))
            echo -e "${BLUE}[$file_index]${NC} $short_path"
            echo "    📅 Modificado: $modified_human"
            echo "    📏 Tamaño: ${size_kb}KB"
            
            if [[ $modified -gt $max_modified ]]; then
                max_modified=$modified
                newest_file="$file"
            fi
        else
            echo -e "${RED}[$file_index]${NC} $short_path (FALTANTE)"
        fi
        
        echo
        ((file_index++))
    done
    
    if [[ -n "$newest_file" ]]; then
        echo -e "${GREEN}🏆 Archivo más reciente:${NC} $(basename "$newest_file")"
        echo -e "    📍 Ubicación: $newest_file"
        echo
    fi
    
    # Retornar información para uso posterior
    echo "$group_id|$filename|${group_files[*]}|$newest_file" > "$TEMP_DIR/group_${group_id}_info.txt"
}

# Mostrar diferencias entre archivos
show_file_differences() {
    local group_id="$1"
    local info_file="$TEMP_DIR/group_${group_id}_info.txt"
    
    if [[ ! -f "$info_file" ]]; then
        log_message "ERROR" "Información del grupo no encontrada. Ejecutar primero 'compare'"
        return 1
    fi
    
    IFS='|' read -r gid filename files_str newest_file < "$info_file"
    IFS=' ' read -ra files <<< "$files_str"
    
    echo -e "${CYAN}=== DIFERENCIAS EN GRUPO $group_id ===${NC}"
    echo
    
    # Comparar cada archivo con el más reciente
    for file in "${files[@]}"; do
        if [[ "$file" != "$newest_file" ]] && [[ -f "$file" ]] && [[ -f "$newest_file" ]]; then
            echo -e "${BLUE}Comparando:${NC}"
            echo "  📄 Base: $(basename "$newest_file")"
            echo "  📄 Con:  $(basename "$file")"
            echo
            
            if command -v diff &> /dev/null; then
                local diff_output=$(diff -u "$newest_file" "$file" 2>/dev/null)
                if [[ -n "$diff_output" ]]; then
                    echo -e "${YELLOW}Diferencias encontradas:${NC}"
                    echo "$diff_output" | head -20
                    if [[ $(echo "$diff_output" | wc -l) -gt 20 ]]; then
                        echo "... (diferencias truncadas, usar diff manualmente para ver completo)"
                    fi
                else
                    echo -e "${GREEN}✓ Archivos idénticos${NC}"
                fi
            else
                # Comparación básica por tamaño y fecha
                local size1=$(stat -f%z "$newest_file" 2>/dev/null || stat -c%s "$newest_file" 2>/dev/null)
                local size2=$(stat -f%z "$file" 2>/dev/null || stat -c%s "$file" 2>/dev/null)
                
                if [[ "$size1" == "$size2" ]]; then
                    echo -e "${GREEN}✓ Mismo tamaño${NC}"
                else
                    echo -e "${YELLOW}⚠ Tamaños diferentes: $size1 vs $size2 bytes${NC}"
                fi
            fi
            echo
        fi
    done
}

# Sincronizar grupo manualmente
sync_group_manual() {
    local group_id="$1"
    local master_file="$2"
    local info_file="$TEMP_DIR/group_${group_id}_info.txt"
    
    if [[ ! -f "$info_file" ]]; then
        log_message "ERROR" "Información del grupo no encontrada. Ejecutar primero 'compare'"
        return 1
    fi
    
    IFS='|' read -r gid filename files_str newest_file < "$info_file"
    IFS=' ' read -ra files <<< "$files_str"
    
    # Usar archivo maestro especificado o el más reciente
    local source_file="${master_file:-$newest_file}"
    
    if [[ ! -f "$source_file" ]]; then
        log_message "ERROR" "Archivo maestro no encontrado: $source_file"
        return 1
    fi
    
    echo -e "${CYAN}=== SINCRONIZACIÓN MANUAL GRUPO $group_id ===${NC}"
    echo -e "${GREEN}📄 Archivo maestro:${NC} $source_file"
    echo
    
    # Crear backups antes de sincronizar
    local backup_dir="$SCRIPT_DIR/backups/manual-sync-$(date +%Y%m%d_%H%M%S)"
    mkdir -p "$backup_dir"
    
    local synced=0
    local errors=0
    
    for file in "${files[@]}"; do
        if [[ "$file" != "$source_file" ]]; then
            if [[ -f "$file" ]]; then
                # Crear backup
                cp "$file" "$backup_dir/$(basename "$file").backup"
                
                # Sincronizar
                echo -e "${BLUE}Sincronizando:${NC} $(basename "$file")"
                if cp "$source_file" "$file"; then
                    ((synced++))
                    log_message "INFO" "Sincronizado: $file"
                else
                    ((errors++))
                    log_message "ERROR" "Error sincronizando: $file"
                fi
            else
                # Crear archivo faltante
                echo -e "${YELLOW}Creando archivo faltante:${NC} $(basename "$file")"
                local target_dir=$(dirname "$file")
                mkdir -p "$target_dir"
                if cp "$source_file" "$file"; then
                    ((synced++))
                    log_message "INFO" "Creado: $file"
                else
                    ((errors++))
                    log_message "ERROR" "Error creando: $file"
                fi
            fi
        fi
    done
    
    echo
    if [[ $errors -eq 0 ]]; then
        log_message "SUCCESS" "Sincronización completada: $synced archivos sincronizados"
        echo -e "${GREEN}📁 Backups en:${NC} $backup_dir"
    else
        log_message "WARNING" "Sincronización con errores: $synced éxitos, $errors errores"
    fi
    echo
}

# Listar todos los grupos disponibles
list_all_groups() {
    if [[ ! -f "$DATABASE_FILE" ]]; then
        log_message "ERROR" "Base de datos no encontrada. Ejecutar: ./01-sync-detector.sh"
        return 1
    fi
    
    echo -e "${CYAN}=== GRUPOS DE SINCRONIZACIÓN DISPONIBLES ===${NC}"
    echo
    
    local group_count=0
    while IFS='=' read -r key value; do
        if [[ "$key" == "GROUP_ID" ]]; then
            local group_id="$value"
            local filename=""
            local file_count=""
            
            # Leer información del grupo
            while IFS='=' read -r k v; do
                case "$k" in
                    "FILENAME") filename="$v" ;;
                    "FILE_COUNT") file_count="$v" ;;
                    "GROUP_ID") break ;;
                esac
            done
            
            echo -e "${BLUE}Grupo $group_id:${NC} $filename ($file_count archivos)"
            ((group_count++))
        fi
    done < "$DATABASE_FILE"
    
    echo
    echo -e "${GREEN}Total: $group_count grupos encontrados${NC}"
    echo
}

# Menú interactivo
interactive_menu() {
    while true; do
        echo -e "${CYAN}=== SINCRONIZACIÓN MANUAL - MENÚ INTERACTIVO ===${NC}"
        echo
        echo "1. Listar todos los grupos"
        echo "2. Comparar archivos de un grupo"
        echo "3. Ver diferencias entre archivos"
        echo "4. Sincronizar grupo manualmente"
        echo "5. Sincronizar TODOS los grupos (usar más reciente)"
        echo "6. Salir"
        echo
        read -p "Selecciona una opción (1-6): " choice
        
        case $choice in
            1)
                list_all_groups
                ;;
            2)
                read -p "ID del grupo a comparar: " group_id
                if [[ -n "$group_id" ]]; then
                    compare_group_files "$group_id"
                fi
                ;;
            3)
                read -p "ID del grupo para ver diferencias: " group_id
                if [[ -n "$group_id" ]]; then
                    show_file_differences "$group_id"
                fi
                ;;
            4)
                read -p "ID del grupo a sincronizar: " group_id
                read -p "Archivo maestro (Enter para usar el más reciente): " master_file
                if [[ -n "$group_id" ]]; then
                    sync_group_manual "$group_id" "$master_file"
                fi
                ;;
            5)
                echo -e "${YELLOW}⚠ ADVERTENCIA: Esto sincronizará TODOS los grupos usando el archivo más reciente${NC}"
                read -p "¿Continuar? (s/N): " confirm
                if [[ "$confirm" =~ ^[Ss]$ ]]; then
                    sync_all_groups_auto
                fi
                ;;
            6)
                echo "Saliendo..."
                break
                ;;
            *)
                echo -e "${RED}Opción inválida${NC}"
                ;;
        esac
        
        echo
        read -p "Presiona Enter para continuar..."
        clear
    done
}

# Sincronizar todos los grupos automáticamente
sync_all_groups_auto() {
    local groups=$(grep "^GROUP_ID=" "$DATABASE_FILE" | cut -d'=' -f2)
    local total_groups=$(echo "$groups" | wc -l)
    local current=0
    
    echo -e "${CYAN}=== SINCRONIZACIÓN AUTOMÁTICA DE TODOS LOS GRUPOS ===${NC}"
    echo -e "${BLUE}Total de grupos:${NC} $total_groups"
    echo
    
    for group_id in $groups; do
        ((current++))
        echo -e "${BLUE}[$current/$total_groups] Procesando grupo $group_id...${NC}"
        
        compare_group_files "$group_id" > /dev/null
        sync_group_manual "$group_id"
        
        echo "---"
    done
    
    log_message "SUCCESS" "Sincronización automática completada para $total_groups grupos"
}

# Función principal
main() {
    local action="${1:-interactive}"
    
    load_config
    init_temp_dir
    
    case "$action" in
        "list"|"--list")
            list_all_groups
            ;;
        "compare"|"--compare")
            local group_id="$2"
            if [[ -z "$group_id" ]]; then
                log_message "ERROR" "Uso: $0 compare <group_id>"
                exit 1
            fi
            compare_group_files "$group_id"
            ;;
        "diff"|"--diff")
            local group_id="$2"
            if [[ -z "$group_id" ]]; then
                log_message "ERROR" "Uso: $0 diff <group_id>"
                exit 1
            fi
            show_file_differences "$group_id"
            ;;
        "sync"|"--sync")
            local group_id="$2"
            local master_file="$3"
            if [[ -z "$group_id" ]]; then
                log_message "ERROR" "Uso: $0 sync <group_id> [master_file]"
                exit 1
            fi
            compare_group_files "$group_id" > /dev/null
            sync_group_manual "$group_id" "$master_file"
            ;;
        "sync-all"|"--sync-all")
            sync_all_groups_auto
            ;;
        "interactive"|"--interactive")
            interactive_menu
            ;;
        "help"|"--help")
            echo "Sincronización Manual de Archivos"
            echo
            echo "Uso: $0 [comando] [opciones]"
            echo
            echo "Comandos:"
            echo "  list                    - Listar todos los grupos"
            echo "  compare <group_id>      - Comparar archivos de un grupo"
            echo "  diff <group_id>         - Ver diferencias entre archivos"
            echo "  sync <group_id> [file]  - Sincronizar grupo manualmente"
            echo "  sync-all                - Sincronizar todos los grupos"
            echo "  interactive             - Menú interactivo (por defecto)"
            echo "  help                    - Mostrar esta ayuda"
            echo
            echo "Ejemplos:"
            echo "  $0 list                 # Listar grupos"
            echo "  $0 compare 1            # Comparar grupo 1"
            echo "  $0 sync 1               # Sincronizar grupo 1 (usar más reciente)"
            echo "  $0 sync 1 /ruta/master  # Sincronizar grupo 1 desde archivo específico"
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
