#!/bin/bash

# ============================================================================
# MONITOR DE CAMBIOS PARA SINCRONIZACIÓN AUTOMÁTICA
# ============================================================================
# Descripción: Monitorea cambios de nombres de archivos en tiempo real
#              y ejecuta sincronización automática usando inotify
# Autor: Sistema de Sincronización Automática
# Fecha: $(date +%Y-%m-%d)
# ============================================================================

# Configuración
PROJECT_ROOT="/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_FILE="$SCRIPT_DIR/04-sync-config.conf"
DATABASE_FILE="$SCRIPT_DIR/05-sync-database.txt"
LOG_FILE="$SCRIPT_DIR/sync-monitor.log"
PID_FILE="$SCRIPT_DIR/sync-monitor.pid"
SYNC_ENGINE="$SCRIPT_DIR/03-sync-engine.sh"

# Variables de control
MONITOR_ACTIVE=true
LAST_EVENT_TIME=0
DEBOUNCE_INTERVAL=2  # segundos para evitar eventos duplicados

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
    
    if [[ "$VERBOSE" == "true" ]]; then
        case $level in
            "ERROR")   echo -e "${RED}[ERROR]${NC} $message" ;;
            "WARNING") echo -e "${YELLOW}[WARNING]${NC} $message" ;;
            "INFO")    echo -e "${GREEN}[INFO]${NC} $message" ;;
            "DEBUG")   echo -e "${BLUE}[DEBUG]${NC} $message" ;;
            *)         echo "$message" ;;
        esac
    fi
}

# Cargar configuración
load_config() {
    if [[ -f "$CONFIG_FILE" ]]; then
        source "$CONFIG_FILE"
        log_message "INFO" "Configuración cargada desde $CONFIG_FILE"
    else
        log_message "WARNING" "Archivo de configuración no encontrado, usando valores por defecto"
        VERBOSE=false
        DEBOUNCE_INTERVAL=2
        MONITOR_SUBDIRS=true
    fi
}

# Verificar dependencias
check_dependencies() {
    local deps=("inotifywait" "pgrep" "pkill")
    local missing=()
    
    for dep in "${deps[@]}"; do
        if ! command -v "$dep" &> /dev/null; then
            missing+=("$dep")
        fi
    done
    
    if [[ ${#missing[@]} -gt 0 ]]; then
        log_message "ERROR" "Dependencias faltantes: ${missing[*]}"
        log_message "ERROR" "Instalar con: sudo apt-get install inotify-tools"
        exit 1
    fi
}

# Verificar si ya hay un monitor corriendo
check_existing_monitor() {
    if [[ -f "$PID_FILE" ]]; then
        local existing_pid=$(cat "$PID_FILE")
        if kill -0 "$existing_pid" 2>/dev/null; then
            log_message "ERROR" "Monitor ya está corriendo con PID $existing_pid"
            exit 1
        else
            log_message "WARNING" "Archivo PID obsoleto encontrado, eliminando..."
            rm -f "$PID_FILE"
        fi
    fi
}

# Crear archivo PID
create_pid_file() {
    echo $$ > "$PID_FILE"
    log_message "INFO" "Monitor iniciado con PID $$"
}

# Limpiar al salir
cleanup() {
    log_message "INFO" "Deteniendo monitor..."
    MONITOR_ACTIVE=false
    rm -f "$PID_FILE"
    exit 0
}

# Configurar traps para limpieza
setup_traps() {
    trap cleanup SIGTERM SIGINT SIGQUIT
}

# Verificar que la base de datos existe
check_database() {
    if [[ ! -f "$DATABASE_FILE" ]]; then
        log_message "ERROR" "Base de datos no encontrada: $DATABASE_FILE"
        log_message "ERROR" "Ejecutar primero: ./01-sync-detector.sh"
        exit 1
    fi
    
    local groups=$(grep -c "^GROUP_ID=" "$DATABASE_FILE" 2>/dev/null || echo "0")
    log_message "INFO" "Base de datos cargada: $groups grupos de sincronización"
}

# Obtener directorios a monitorear
get_monitor_directories() {
    local dirs=()
    
    # Extraer directorios únicos de la base de datos
    while IFS='=' read -r key path; do
        if [[ "$key" =~ ^FILE_.*_PATH$ ]] && [[ -f "$path" ]]; then
            local dir=$(dirname "$path")
            if [[ ! " ${dirs[@]} " =~ " ${dir} " ]]; then
                dirs+=("$dir")
            fi
        fi
    done < "$DATABASE_FILE"
    
    printf '%s\n' "${dirs[@]}"
}

# Procesar evento de inotify
process_event() {
    local event_type="$1"
    local directory="$2"
    local filename="$3"
    local new_filename="$4"
    
    local current_time=$(date +%s)
    
    # Debounce: ignorar eventos muy frecuentes
    if [[ $((current_time - LAST_EVENT_TIME)) -lt $DEBOUNCE_INTERVAL ]]; then
        log_message "DEBUG" "Evento ignorado por debounce: $event_type $filename"
        return
    fi
    
    LAST_EVENT_TIME=$current_time
    
    log_message "INFO" "Evento detectado: $event_type en $directory"
    log_message "DEBUG" "Archivo: $filename -> $new_filename"
    
    case "$event_type" in
        "MOVED_FROM"|"MOVED_TO")
            handle_move_event "$directory" "$filename" "$new_filename"
            ;;
        "DELETE")
            handle_delete_event "$directory" "$filename"
            ;;
        "CREATE")
            handle_create_event "$directory" "$filename"
            ;;
        *)
            log_message "DEBUG" "Evento no procesado: $event_type"
            ;;
    esac
}

# Manejar evento de movimiento/renombrado
handle_move_event() {
    local directory="$1"
    local old_filename="$2"
    local new_filename="$3"
    
    if [[ -z "$new_filename" ]]; then
        log_message "DEBUG" "Movimiento incompleto, esperando evento complementario"
        return
    fi
    
    local old_path="$directory/$old_filename"
    local new_path="$directory/$new_filename"
    
    log_message "INFO" "Renombrado detectado: $old_filename -> $new_filename"
    
    # Verificar si el archivo está en la base de datos
    if grep -q "FILE_.*_PATH=$old_path" "$DATABASE_FILE"; then
        log_message "INFO" "Archivo encontrado en base de datos, iniciando sincronización"
        
        # Ejecutar motor de sincronización
        if [[ -x "$SYNC_ENGINE" ]]; then
            "$SYNC_ENGINE" --sync-rename "$old_path" "$new_path" &
            local sync_pid=$!
            log_message "INFO" "Sincronización iniciada con PID $sync_pid"
        else
            log_message "ERROR" "Motor de sincronización no encontrado o no ejecutable: $SYNC_ENGINE"
        fi
    else
        log_message "DEBUG" "Archivo no está en base de datos de sincronización"
    fi
}

# Manejar evento de eliminación
handle_delete_event() {
    local directory="$1"
    local filename="$2"
    local filepath="$directory/$filename"
    
    log_message "INFO" "Eliminación detectada: $filename"
    
    if grep -q "FILE_.*_PATH=$filepath" "$DATABASE_FILE"; then
        log_message "WARNING" "Archivo sincronizado eliminado: $filepath"
        # Aquí podrías implementar lógica para manejar eliminaciones
        # Por ejemplo, eliminar de otros grupos o marcar como inactivo
    fi
}

# Manejar evento de creación
handle_create_event() {
    local directory="$1"
    local filename="$2"
    
    log_message "DEBUG" "Creación detectada: $filename"
    
    # Verificar si hay otros archivos con el mismo nombre
    local existing_count=$(grep -c "FILENAME=$filename$" "$DATABASE_FILE" 2>/dev/null || echo "0")
    
    if [[ $existing_count -gt 0 ]]; then
        log_message "INFO" "Nuevo archivo con nombre existente en sincronización: $filename"
        log_message "INFO" "Ejecutar re-escaneo para incluir en sincronización"
    fi
}

# Iniciar monitoreo con inotify
start_monitoring() {
    local directories=($(get_monitor_directories))
    
    if [[ ${#directories[@]} -eq 0 ]]; then
        log_message "ERROR" "No hay directorios para monitorear"
        exit 1
    fi
    
    log_message "INFO" "Iniciando monitoreo de ${#directories[@]} directorios"
    
    # Construir comando inotifywait
    local inotify_cmd="inotifywait -m -r --format '%e %w %f' -e move,delete,create"
    
    for dir in "${directories[@]}"; do
        if [[ -d "$dir" ]]; then
            inotify_cmd+=" \"$dir\""
            log_message "DEBUG" "Monitoreando: $dir"
        fi
    done
    
    log_message "INFO" "Monitor activo, presiona Ctrl+C para detener"
    
    # Ejecutar inotifywait y procesar eventos
    eval "$inotify_cmd" | while read -r event_line; do
        if [[ "$MONITOR_ACTIVE" == "false" ]]; then
            break
        fi
        
        # Parsear línea de evento
        local event_parts=($event_line)
        local event_type="${event_parts[0]}"
        local directory="${event_parts[1]}"
        local filename="${event_parts[2]}"
        local new_filename="${event_parts[3]:-}"
        
        # Filtrar eventos irrelevantes
        if [[ "$filename" =~ ^\. ]] || [[ "$filename" =~ ~$ ]] || [[ "$filename" =~ \.tmp$ ]]; then
            continue
        fi
        
        process_event "$event_type" "$directory" "$filename" "$new_filename"
    done
}

# Mostrar estado del monitor
show_status() {
    if [[ -f "$PID_FILE" ]]; then
        local pid=$(cat "$PID_FILE")
        if kill -0 "$pid" 2>/dev/null; then
            echo "Monitor está corriendo con PID $pid"
            local directories=($(get_monitor_directories))
            echo "Monitoreando ${#directories[@]} directorios"
            return 0
        else
            echo "Archivo PID existe pero proceso no está corriendo"
            return 1
        fi
    else
        echo "Monitor no está corriendo"
        return 1
    fi
}

# Detener monitor
stop_monitor() {
    if [[ -f "$PID_FILE" ]]; then
        local pid=$(cat "$PID_FILE")
        if kill -0 "$pid" 2>/dev/null; then
            log_message "INFO" "Deteniendo monitor con PID $pid"
            kill "$pid"
            rm -f "$PID_FILE"
            echo "Monitor detenido"
        else
            echo "Proceso no está corriendo"
            rm -f "$PID_FILE"
        fi
    else
        echo "Monitor no está corriendo"
    fi
}

# Función principal
main() {
    local action="${1:-start}"
    
    case "$action" in
        "start"|"--start")
            check_dependencies
            load_config
            check_existing_monitor
            check_database
            create_pid_file
            setup_traps
            start_monitoring
            ;;
        "stop"|"--stop")
            stop_monitor
            ;;
        "status"|"--status")
            show_status
            ;;
        "restart"|"--restart")
            stop_monitor
            sleep 2
            main "start"
            ;;
        "help"|"--help")
            echo "Uso: $0 [start|stop|status|restart|help]"
            echo "  start   - Iniciar monitor (por defecto)"
            echo "  stop    - Detener monitor"
            echo "  status  - Mostrar estado del monitor"
            echo "  restart - Reiniciar monitor"
            echo "  help    - Mostrar esta ayuda"
            ;;
        *)
            echo "Acción desconocida: $action"
            main "help"
            exit 1
            ;;
    esac
}

# Ejecutar función principal
main "$@"
