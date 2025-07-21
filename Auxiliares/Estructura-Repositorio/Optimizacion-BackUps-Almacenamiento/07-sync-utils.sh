#!/bin/bash

# ============================================================================
# UTILIDADES DEL SISTEMA DE SINCRONIZACIÓN
# ============================================================================
# Descripción: Herramientas auxiliares para mantenimiento y diagnóstico
#              del sistema de sincronización automática
# Autor: Sistema de Sincronización Automática
# Fecha: $(date +%Y-%m-%d)
# ============================================================================

# Configuración
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_FILE="$SCRIPT_DIR/04-sync-config.conf"
DATABASE_FILE="$SCRIPT_DIR/05-sync-database.txt"
LOG_DIR="$SCRIPT_DIR"

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

# Mostrar información del sistema
show_system_info() {
    echo -e "${CYAN}=== INFORMACIÓN DEL SISTEMA DE SINCRONIZACIÓN ===${NC}"
    echo
    
    # Información básica
    echo -e "${BLUE}Directorio del sistema:${NC} $SCRIPT_DIR"
    echo -e "${BLUE}Base de datos:${NC} $DATABASE_FILE"
    echo -e "${BLUE}Configuración:${NC} $CONFIG_FILE"
    echo
    
    # Estado de archivos principales
    echo -e "${BLUE}Estado de archivos:${NC}"
    local files=("01-sync-detector.sh" "02-sync-monitor.sh" "03-sync-engine.sh" "04-sync-config.conf" "05-sync-database.txt")
    
    for file in "${files[@]}"; do
        local filepath="$SCRIPT_DIR/$file"
        if [[ -f "$filepath" ]]; then
            local size=$(stat -f%z "$filepath" 2>/dev/null || stat -c%s "$filepath" 2>/dev/null || echo "?")
            local modified=$(stat -f%Sm -t "%Y-%m-%d %H:%M" "$filepath" 2>/dev/null || stat -c%y "$filepath" 2>/dev/null | cut -d' ' -f1-2 || echo "?")
            echo "  ✓ $file ($size bytes, modificado: $modified)"
        else
            echo "  ✗ $file (no encontrado)"
        fi
    done
    echo
    
    # Dependencias del sistema
    echo -e "${BLUE}Dependencias del sistema:${NC}"
    local deps=("inotifywait" "find" "grep" "awk" "sed" "sort" "uniq")
    for dep in "${deps[@]}"; do
        if command -v "$dep" &> /dev/null; then
            local version=$(command -v "$dep" --version 2>/dev/null | head -1 || echo "disponible")
            echo "  ✓ $dep ($version)"
        else
            echo "  ✗ $dep (no encontrado)"
        fi
    done
    echo
}

# Diagnóstico completo del sistema
run_diagnostics() {
    echo -e "${CYAN}=== DIAGNÓSTICO DEL SISTEMA ===${NC}"
    echo
    
    local issues=0
    
    # Verificar archivos principales
    echo -e "${BLUE}1. Verificando archivos principales...${NC}"
    local required_files=("01-sync-detector.sh" "02-sync-monitor.sh" "03-sync-engine.sh")
    for file in "${required_files[@]}"; do
        local filepath="$SCRIPT_DIR/$file"
        if [[ -f "$filepath" ]]; then
            if [[ -x "$filepath" ]]; then
                echo "  ✓ $file (existe y es ejecutable)"
            else
                echo "  ⚠ $file (existe pero no es ejecutable)"
                ((issues++))
            fi
        else
            echo "  ✗ $file (no encontrado)"
            ((issues++))
        fi
    done
    echo
    
    # Verificar configuración
    echo -e "${BLUE}2. Verificando configuración...${NC}"
    if [[ -f "$CONFIG_FILE" ]]; then
        if source "$CONFIG_FILE" 2>/dev/null; then
            echo "  ✓ Archivo de configuración válido"
            
            # Verificar variables críticas
            local critical_vars=("PROJECT_ROOT" "INCLUDE_PATTERNS" "EXCLUDE_DIRS")
            for var in "${critical_vars[@]}"; do
                if [[ -n "${!var}" ]]; then
                    echo "  ✓ Variable $var definida"
                else
                    echo "  ⚠ Variable $var no definida"
                    ((issues++))
                fi
            done
        else
            echo "  ✗ Error de sintaxis en configuración"
            ((issues++))
        fi
    else
        echo "  ⚠ Archivo de configuración no encontrado"
        ((issues++))
    fi
    echo
    
    # Verificar base de datos
    echo -e "${BLUE}3. Verificando base de datos...${NC}"
    if [[ -f "$DATABASE_FILE" ]]; then
        local groups=$(grep -c "^GROUP_ID=" "$DATABASE_FILE" 2>/dev/null || echo "0")
        local files_db=$(grep -c "^FILE_.*_PATH=" "$DATABASE_FILE" 2>/dev/null || echo "0")
        echo "  ✓ Base de datos existe ($groups grupos, $files_db archivos)"
        
        # Verificar integridad básica
        local missing_files=0
        while IFS='=' read -r key path; do
            if [[ "$key" =~ ^FILE_.*_PATH$ ]] && [[ ! -f "$path" ]]; then
                ((missing_files++))
            fi
        done < "$DATABASE_FILE"
        
        if [[ $missing_files -eq 0 ]]; then
            echo "  ✓ Todos los archivos registrados existen"
        else
            echo "  ⚠ $missing_files archivos registrados no encontrados"
            ((issues++))
        fi
    else
        echo "  ⚠ Base de datos no existe (ejecutar 01-sync-detector.sh)"
        ((issues++))
    fi
    echo
    
    # Verificar dependencias
    echo -e "${BLUE}4. Verificando dependencias...${NC}"
    local deps=("inotifywait" "find" "grep" "awk" "sed")
    local missing_deps=0
    for dep in "${deps[@]}"; do
        if command -v "$dep" &> /dev/null; then
            echo "  ✓ $dep disponible"
        else
            echo "  ✗ $dep no encontrado"
            ((missing_deps++))
            ((issues++))
        fi
    done
    
    if [[ $missing_deps -gt 0 ]]; then
        echo "  💡 Instalar dependencias: sudo apt-get install inotify-tools findutils"
    fi
    echo
    
    # Verificar permisos
    echo -e "${BLUE}5. Verificando permisos...${NC}"
    if [[ -w "$SCRIPT_DIR" ]]; then
        echo "  ✓ Directorio escribible"
    else
        echo "  ✗ Sin permisos de escritura en directorio"
        ((issues++))
    fi
    
    # Verificar espacio en disco
    local available_space=$(df "$SCRIPT_DIR" | awk 'NR==2 {print $4}')
    if [[ $available_space -gt 1048576 ]]; then  # 1GB en KB
        echo "  ✓ Espacio en disco suficiente"
    else
        echo "  ⚠ Poco espacio en disco disponible"
        ((issues++))
    fi
    echo
    
    # Resumen
    echo -e "${BLUE}=== RESUMEN DEL DIAGNÓSTICO ===${NC}"
    if [[ $issues -eq 0 ]]; then
        log_message "SUCCESS" "Sistema en perfecto estado ✓"
    elif [[ $issues -le 3 ]]; then
        log_message "WARNING" "Sistema funcional con $issues advertencias ⚠"
    else
        log_message "ERROR" "Sistema con $issues problemas que requieren atención ✗"
    fi
    echo
}

# Limpiar archivos temporales y logs antiguos
cleanup_system() {
    echo -e "${CYAN}=== LIMPIEZA DEL SISTEMA ===${NC}"
    echo
    
    local cleaned=0
    
    # Limpiar locks obsoletos
    echo -e "${BLUE}1. Limpiando locks obsoletos...${NC}"
    local lock_dir="$SCRIPT_DIR/locks"
    if [[ -d "$lock_dir" ]]; then
        local old_locks=0
        for lock_file in "$lock_dir"/*.lock; do
            if [[ -f "$lock_file" ]]; then
                local lock_pid=$(cat "$lock_file" 2>/dev/null)
                if [[ -n "$lock_pid" ]] && ! kill -0 "$lock_pid" 2>/dev/null; then
                    rm -f "$lock_file"
                    ((old_locks++))
                    ((cleaned++))
                fi
            fi
        done
        echo "  ✓ $old_locks locks obsoletos eliminados"
    else
        echo "  ✓ No hay directorio de locks"
    fi
    
    # Limpiar logs antiguos
    echo -e "${BLUE}2. Limpiando logs antiguos...${NC}"
    local log_files=("sync-detector.log" "sync-monitor.log" "sync-operations.log" "sync-errors.log")
    local max_size=10485760  # 10MB
    
    for log_file in "${log_files[@]}"; do
        local log_path="$SCRIPT_DIR/$log_file"
        if [[ -f "$log_path" ]]; then
            local size=$(stat -f%z "$log_path" 2>/dev/null || stat -c%s "$log_path" 2>/dev/null || echo "0")
            if [[ $size -gt $max_size ]]; then
                # Mantener solo las últimas 1000 líneas
                tail -1000 "$log_path" > "$log_path.tmp" && mv "$log_path.tmp" "$log_path"
                echo "  ✓ $log_file truncado (era $(($size/1024))KB)"
                ((cleaned++))
            else
                echo "  ✓ $log_file OK ($(($size/1024))KB)"
            fi
        fi
    done
    
    # Limpiar backups antiguos
    echo -e "${BLUE}3. Limpiando backups antiguos...${NC}"
    local backup_dir="$SCRIPT_DIR/backups"
    if [[ -d "$backup_dir" ]]; then
        local old_backups=$(find "$backup_dir" -type f -mtime +30 2>/dev/null | wc -l)
        if [[ $old_backups -gt 0 ]]; then
            find "$backup_dir" -type f -mtime +30 -delete 2>/dev/null
            echo "  ✓ $old_backups backups antiguos eliminados"
            ((cleaned++))
        else
            echo "  ✓ No hay backups antiguos"
        fi
    else
        echo "  ✓ No hay directorio de backups"
    fi
    
    echo
    if [[ $cleaned -gt 0 ]]; then
        log_message "SUCCESS" "Limpieza completada: $cleaned elementos procesados"
    else
        log_message "INFO" "Sistema ya estaba limpio"
    fi
    echo
}

# Crear backup completo del sistema
create_backup() {
    local backup_name="${1:-sync-system-$(date +%Y%m%d_%H%M%S)}"
    local backup_dir="$SCRIPT_DIR/system-backups"
    local backup_path="$backup_dir/$backup_name"
    
    echo -e "${CYAN}=== CREANDO BACKUP DEL SISTEMA ===${NC}"
    echo
    
    # Crear directorio de backup
    mkdir -p "$backup_dir"
    mkdir -p "$backup_path"
    
    # Copiar archivos principales
    echo -e "${BLUE}Copiando archivos del sistema...${NC}"
    local files_to_backup=("*.sh" "*.conf" "*.txt" "*.md")
    local copied=0
    
    for pattern in "${files_to_backup[@]}"; do
        for file in $SCRIPT_DIR/$pattern; do
            if [[ -f "$file" ]]; then
                cp "$file" "$backup_path/"
                ((copied++))
            fi
        done
    done
    
    # Copiar directorios importantes
    local dirs_to_backup=("backups" "logs" "restore_points")
    for dir in "${dirs_to_backup[@]}"; do
        if [[ -d "$SCRIPT_DIR/$dir" ]]; then
            cp -r "$SCRIPT_DIR/$dir" "$backup_path/"
            echo "  ✓ Directorio $dir copiado"
        fi
    done
    
    # Crear archivo de información
    cat > "$backup_path/backup-info.txt" << EOF
Backup del Sistema de Sincronización Automática
Creado: $(date)
Usuario: $(whoami)
Hostname: $(hostname)
Directorio original: $SCRIPT_DIR
Archivos copiados: $copied
EOF
    
    # Comprimir backup
    echo -e "${BLUE}Comprimiendo backup...${NC}"
    cd "$backup_dir"
    tar -czf "${backup_name}.tar.gz" "$backup_name"
    rm -rf "$backup_name"
    
    local backup_size=$(stat -f%z "${backup_name}.tar.gz" 2>/dev/null || stat -c%s "${backup_name}.tar.gz" 2>/dev/null || echo "0")
    
    echo
    log_message "SUCCESS" "Backup creado: $backup_dir/${backup_name}.tar.gz ($(($backup_size/1024))KB)"
    echo
}

# Restaurar desde backup
restore_backup() {
    local backup_file="$1"
    
    if [[ -z "$backup_file" ]]; then
        echo -e "${BLUE}Backups disponibles:${NC}"
        ls -la "$SCRIPT_DIR/system-backups"/*.tar.gz 2>/dev/null || echo "No hay backups disponibles"
        return 1
    fi
    
    if [[ ! -f "$backup_file" ]]; then
        log_message "ERROR" "Archivo de backup no encontrado: $backup_file"
        return 1
    fi
    
    echo -e "${CYAN}=== RESTAURANDO DESDE BACKUP ===${NC}"
    echo
    
    # Crear backup del estado actual antes de restaurar
    log_message "INFO" "Creando backup del estado actual..."
    create_backup "pre-restore-$(date +%Y%m%d_%H%M%S)"
    
    # Extraer backup
    local temp_dir=$(mktemp -d)
    cd "$temp_dir"
    tar -xzf "$backup_file"
    
    # Restaurar archivos
    local restored=0
    for file in *; do
        if [[ -f "$file" ]] && [[ "$file" != "backup-info.txt" ]]; then
            cp "$file" "$SCRIPT_DIR/"
            ((restored++))
        fi
    done
    
    # Limpiar
    rm -rf "$temp_dir"
    
    log_message "SUCCESS" "Restauración completada: $restored archivos restaurados"
    log_message "INFO" "Reiniciar el sistema para aplicar cambios"
    echo
}

# Función principal
main() {
    local action="${1:-help}"
    
    load_config
    
    case "$action" in
        "info"|"--info")
            show_system_info
            ;;
        "diagnose"|"--diagnose")
            run_diagnostics
            ;;
        "cleanup"|"--cleanup")
            cleanup_system
            ;;
        "backup"|"--backup")
            local backup_name="$2"
            create_backup "$backup_name"
            ;;
        "restore"|"--restore")
            local backup_file="$2"
            restore_backup "$backup_file"
            ;;
        "help"|"--help")
            echo -e "${CYAN}Utilidades del Sistema de Sincronización Automática${NC}"
            echo
            echo "Uso: $0 <comando> [opciones]"
            echo
            echo "Comandos disponibles:"
            echo "  info      - Mostrar información del sistema"
            echo "  diagnose  - Ejecutar diagnóstico completo"
            echo "  cleanup   - Limpiar archivos temporales y logs antiguos"
            echo "  backup    - Crear backup completo del sistema"
            echo "  restore   - Restaurar desde backup"
            echo "  help      - Mostrar esta ayuda"
            echo
            echo "Ejemplos:"
            echo "  $0 info                    # Información del sistema"
            echo "  $0 diagnose                # Diagnóstico completo"
            echo "  $0 cleanup                 # Limpiar sistema"
            echo "  $0 backup mi-backup        # Crear backup con nombre"
            echo "  $0 restore backup.tar.gz   # Restaurar desde archivo"
            ;;
        *)
            log_message "ERROR" "Comando desconocido: $action"
            main "help"
            exit 1
            ;;
    esac
}

# Ejecutar función principal
main "$@"
