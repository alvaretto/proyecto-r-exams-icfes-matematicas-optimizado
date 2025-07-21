#!/bin/bash

# ============================================================================
# SERVICIO DE SINCRONIZACIÓN AUTOMÁTICA
# ============================================================================
# Descripción: Script para gestionar el sistema de sincronización como servicio
#              Incluye instalación, desinstalación y gestión de systemd
# Autor: Sistema de Sincronización Automática
# Fecha: $(date +%Y-%m-%d)
# ============================================================================

# Configuración
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SERVICE_NAME="sync-monitor"
SERVICE_FILE="/etc/systemd/system/${SERVICE_NAME}.service"
USER=$(whoami)
GROUP=$(id -gn)

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
    
    case $level in
        "ERROR")   echo -e "${RED}[ERROR]${NC} $message" ;;
        "WARNING") echo -e "${YELLOW}[WARNING]${NC} $message" ;;
        "INFO")    echo -e "${GREEN}[INFO]${NC} $message" ;;
        "DEBUG")   echo -e "${BLUE}[DEBUG]${NC} $message" ;;
        *)         echo "$message" ;;
    esac
}

# Verificar permisos de root
check_root() {
    if [[ $EUID -ne 0 ]]; then
        log_message "ERROR" "Este script requiere permisos de root para gestionar servicios systemd"
        log_message "INFO" "Ejecutar con: sudo $0 $*"
        exit 1
    fi
}

# Crear archivo de servicio systemd
create_service_file() {
    log_message "INFO" "Creando archivo de servicio systemd..."
    
    cat > "$SERVICE_FILE" << EOF
[Unit]
Description=Sistema de Sincronización Automática de Archivos
Documentation=file://$SCRIPT_DIR/00-backup-sincro-tutorial.md
After=network.target
Wants=network.target

[Service]
Type=simple
User=$USER
Group=$GROUP
WorkingDirectory=$SCRIPT_DIR
ExecStart=$SCRIPT_DIR/02-sync-monitor.sh start
ExecStop=$SCRIPT_DIR/02-sync-monitor.sh stop
ExecReload=/bin/kill -HUP \$MAINPID
Restart=always
RestartSec=10
StandardOutput=journal
StandardError=journal
SyslogIdentifier=sync-monitor

# Configuración de seguridad
NoNewPrivileges=true
PrivateTmp=true
ProtectSystem=strict
ProtectHome=false
ReadWritePaths=$SCRIPT_DIR

# Variables de entorno
Environment=PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
Environment=HOME=/home/$USER

[Install]
WantedBy=multi-user.target
EOF

    if [[ -f "$SERVICE_FILE" ]]; then
        log_message "INFO" "Archivo de servicio creado: $SERVICE_FILE"
        return 0
    else
        log_message "ERROR" "No se pudo crear el archivo de servicio"
        return 1
    fi
}

# Instalar servicio
install_service() {
    log_message "INFO" "Instalando servicio de sincronización..."
    
    # Verificar que los scripts existan
    local required_scripts=("01-sync-detector.sh" "02-sync-monitor.sh" "03-sync-engine.sh")
    for script in "${required_scripts[@]}"; do
        if [[ ! -x "$SCRIPT_DIR/$script" ]]; then
            log_message "ERROR" "Script requerido no encontrado o no ejecutable: $script"
            return 1
        fi
    done
    
    # Crear archivo de servicio
    if ! create_service_file; then
        return 1
    fi
    
    # Recargar systemd
    log_message "INFO" "Recargando configuración de systemd..."
    systemctl daemon-reload
    
    # Habilitar servicio
    log_message "INFO" "Habilitando servicio para inicio automático..."
    systemctl enable "$SERVICE_NAME"
    
    log_message "INFO" "Servicio instalado exitosamente"
    log_message "INFO" "Para iniciar: sudo systemctl start $SERVICE_NAME"
    log_message "INFO" "Para ver estado: sudo systemctl status $SERVICE_NAME"
    log_message "INFO" "Para ver logs: sudo journalctl -u $SERVICE_NAME -f"
}

# Desinstalar servicio
uninstall_service() {
    log_message "INFO" "Desinstalando servicio de sincronización..."
    
    # Detener servicio si está corriendo
    if systemctl is-active --quiet "$SERVICE_NAME"; then
        log_message "INFO" "Deteniendo servicio..."
        systemctl stop "$SERVICE_NAME"
    fi
    
    # Deshabilitar servicio
    if systemctl is-enabled --quiet "$SERVICE_NAME"; then
        log_message "INFO" "Deshabilitando servicio..."
        systemctl disable "$SERVICE_NAME"
    fi
    
    # Eliminar archivo de servicio
    if [[ -f "$SERVICE_FILE" ]]; then
        log_message "INFO" "Eliminando archivo de servicio..."
        rm -f "$SERVICE_FILE"
    fi
    
    # Recargar systemd
    log_message "INFO" "Recargando configuración de systemd..."
    systemctl daemon-reload
    
    log_message "INFO" "Servicio desinstalado exitosamente"
}

# Mostrar estado del servicio
show_status() {
    log_message "INFO" "Estado del servicio de sincronización:"
    echo
    
    if [[ -f "$SERVICE_FILE" ]]; then
        echo "✓ Archivo de servicio existe: $SERVICE_FILE"
        
        if systemctl is-enabled --quiet "$SERVICE_NAME"; then
            echo "✓ Servicio habilitado para inicio automático"
        else
            echo "✗ Servicio NO habilitado para inicio automático"
        fi
        
        if systemctl is-active --quiet "$SERVICE_NAME"; then
            echo "✓ Servicio está corriendo"
        else
            echo "✗ Servicio NO está corriendo"
        fi
        
        echo
        echo "Estado detallado:"
        systemctl status "$SERVICE_NAME" --no-pager
        
    else
        echo "✗ Servicio no está instalado"
        echo "  Para instalar: sudo $0 install"
    fi
}

# Mostrar logs del servicio
show_logs() {
    local lines="${1:-50}"
    
    if systemctl list-units --full -all | grep -Fq "$SERVICE_NAME.service"; then
        log_message "INFO" "Mostrando últimas $lines líneas del log del servicio:"
        journalctl -u "$SERVICE_NAME" -n "$lines" --no-pager
    else
        log_message "ERROR" "Servicio no está instalado"
    fi
}

# Seguir logs en tiempo real
follow_logs() {
    if systemctl list-units --full -all | grep -Fq "$SERVICE_NAME.service"; then
        log_message "INFO" "Siguiendo logs del servicio (Ctrl+C para salir):"
        journalctl -u "$SERVICE_NAME" -f
    else
        log_message "ERROR" "Servicio no está instalado"
    fi
}

# Reiniciar servicio
restart_service() {
    if systemctl list-units --full -all | grep -Fq "$SERVICE_NAME.service"; then
        log_message "INFO" "Reiniciando servicio..."
        systemctl restart "$SERVICE_NAME"
        sleep 2
        show_status
    else
        log_message "ERROR" "Servicio no está instalado"
    fi
}

# Iniciar servicio
start_service() {
    if systemctl list-units --full -all | grep -Fq "$SERVICE_NAME.service"; then
        log_message "INFO" "Iniciando servicio..."
        systemctl start "$SERVICE_NAME"
        sleep 2
        show_status
    else
        log_message "ERROR" "Servicio no está instalado"
    fi
}

# Detener servicio
stop_service() {
    if systemctl list-units --full -all | grep -Fq "$SERVICE_NAME.service"; then
        log_message "INFO" "Deteniendo servicio..."
        systemctl stop "$SERVICE_NAME"
        sleep 2
        show_status
    else
        log_message "ERROR" "Servicio no está instalado"
    fi
}

# Verificar dependencias del sistema
check_dependencies() {
    local deps=("systemctl" "journalctl")
    local missing=()
    
    for dep in "${deps[@]}"; do
        if ! command -v "$dep" &> /dev/null; then
            missing+=("$dep")
        fi
    done
    
    if [[ ${#missing[@]} -gt 0 ]]; then
        log_message "ERROR" "Dependencias faltantes: ${missing[*]}"
        log_message "ERROR" "Este sistema requiere systemd"
        return 1
    fi
    
    return 0
}

# Función principal
main() {
    local action="${1:-help}"
    
    if ! check_dependencies; then
        exit 1
    fi
    
    case "$action" in
        "install")
            check_root
            install_service
            ;;
        "uninstall")
            check_root
            uninstall_service
            ;;
        "start")
            check_root
            start_service
            ;;
        "stop")
            check_root
            stop_service
            ;;
        "restart")
            check_root
            restart_service
            ;;
        "status")
            show_status
            ;;
        "logs")
            local lines="${2:-50}"
            show_logs "$lines"
            ;;
        "follow")
            follow_logs
            ;;
        "help"|"--help")
            echo "Gestión del Servicio de Sincronización Automática"
            echo
            echo "Uso: $0 <comando> [opciones]"
            echo
            echo "Comandos de gestión (requieren sudo):"
            echo "  install   - Instalar servicio systemd"
            echo "  uninstall - Desinstalar servicio systemd"
            echo "  start     - Iniciar servicio"
            echo "  stop      - Detener servicio"
            echo "  restart   - Reiniciar servicio"
            echo
            echo "Comandos de información:"
            echo "  status    - Mostrar estado del servicio"
            echo "  logs [N]  - Mostrar últimas N líneas del log (default: 50)"
            echo "  follow    - Seguir logs en tiempo real"
            echo "  help      - Mostrar esta ayuda"
            echo
            echo "Ejemplos:"
            echo "  sudo $0 install          # Instalar servicio"
            echo "  sudo $0 start            # Iniciar servicio"
            echo "  $0 status                # Ver estado"
            echo "  $0 logs 100              # Ver últimas 100 líneas"
            echo "  $0 follow                # Seguir logs en tiempo real"
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
