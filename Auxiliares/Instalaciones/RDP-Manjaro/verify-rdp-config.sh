#!/bin/bash

# Script para verificar que la configuración persistente de RDP esté funcionando correctamente
# Ejecuta este script después de un reinicio para confirmar que todo está configurado

echo "=== Verificación de Configuración Persistente de RDP ==="
echo "Verificando que todos los componentes estén funcionando correctamente..."
echo

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Función para mostrar estado con colores
show_status() {
    local status=$1
    local message=$2
    
    if [ "$status" = "OK" ]; then
        echo -e "${GREEN}✅ $message${NC}"
    elif [ "$status" = "WARNING" ]; then
        echo -e "${YELLOW}⚠️  $message${NC}"
    elif [ "$status" = "ERROR" ]; then
        echo -e "${RED}❌ $message${NC}"
    else
        echo -e "${BLUE}ℹ️  $message${NC}"
    fi
}

# Función para verificar si un paquete está instalado
check_package() {
    local package=$1
    if pacman -Qi "$package" &> /dev/null; then
        return 0
    else
        return 1
    fi
}

# Función para verificar servicios systemd
check_service() {
    local service=$1
    local enabled_status=$(systemctl is-enabled "$service" 2>/dev/null)
    local active_status=$(systemctl is-active "$service" 2>/dev/null)
    
    echo "🔍 Verificando servicio: $service"
    
    if [ "$enabled_status" = "enabled" ]; then
        show_status "OK" "Servicio $service está habilitado para inicio automático"
    else
        show_status "ERROR" "Servicio $service NO está habilitado para inicio automático"
        return 1
    fi
    
    if [ "$active_status" = "active" ]; then
        show_status "OK" "Servicio $service está actualmente activo"
    else
        show_status "WARNING" "Servicio $service no está activo actualmente"
        echo "   Estado: $active_status"
    fi
    
    echo
}

# Función para verificar archivos de configuración
check_config_file() {
    local file=$1
    local description=$2
    
    if [ -f "$file" ]; then
        show_status "OK" "$description existe: $file"
        return 0
    else
        show_status "ERROR" "$description NO existe: $file"
        return 1
    fi
}

# Función para verificar permisos de archivos
check_file_permissions() {
    local file=$1
    local expected_perms=$2
    local description=$3
    
    if [ -f "$file" ]; then
        local actual_perms=$(stat -c "%a" "$file")
        if [ "$actual_perms" = "$expected_perms" ]; then
            show_status "OK" "$description tiene permisos correctos ($expected_perms)"
        else
            show_status "WARNING" "$description tiene permisos $actual_perms (esperado: $expected_perms)"
        fi
    fi
}

echo "1. 📦 VERIFICANDO PAQUETES INSTALADOS"
echo "=================================="

packages=("tailscale" "remmina" "freerdp" "libvncserver" "spice-gtk" "telepathy-glib")
all_packages_ok=true

for package in "${packages[@]}"; do
    if check_package "$package"; then
        show_status "OK" "Paquete $package está instalado"
    else
        show_status "ERROR" "Paquete $package NO está instalado"
        all_packages_ok=false
    fi
done

echo

echo "2. 🔧 VERIFICANDO SERVICIOS SYSTEMD"
echo "================================="

check_service "tailscaled"
check_service "tailscale-autoconnect"

echo "3. 📁 VERIFICANDO ARCHIVOS DE CONFIGURACIÓN"
echo "========================================="

# Verificar archivos de configuración del sistema
check_config_file "/etc/systemd/system/tailscaled.service.d/override.conf" "Configuración override de Tailscale"
check_config_file "/etc/systemd/system/tailscale-autoconnect.service" "Servicio de auto-conexión Tailscale"
check_config_file "/usr/local/bin/tailscale-autoconnect.sh" "Script de auto-conexión Tailscale"

# Verificar archivos de configuración del usuario
check_config_file "$HOME/.config/remmina/remmina.pref" "Configuración de Remmina"
check_config_file "$HOME/rdp-connect.sh" "Script de conexión RDP"

echo

echo "4. 🔐 VERIFICANDO PERMISOS DE ARCHIVOS"
echo "===================================="

check_file_permissions "$HOME/rdp-connect.sh" "755" "Script de conexión RDP"
check_file_permissions "/usr/local/bin/tailscale-autoconnect.sh" "755" "Script de auto-conexión Tailscale"

echo

echo "5. 🌐 VERIFICANDO CONECTIVIDAD TAILSCALE"
echo "======================================"

if command -v tailscale &> /dev/null; then
    show_status "OK" "Comando tailscale disponible"
    
    # Verificar estado de Tailscale
    if tailscale status &> /dev/null; then
        local ts_status=$(tailscale status --json 2>/dev/null | jq -r '.BackendState' 2>/dev/null || echo "unknown")
        if [ "$ts_status" = "Running" ]; then
            show_status "OK" "Tailscale está conectado y funcionando"
            
            # Mostrar IP de Tailscale si está disponible
            local ts_ip=$(tailscale ip -4 2>/dev/null)
            if [ -n "$ts_ip" ]; then
                show_status "INFO" "IP de Tailscale: $ts_ip"
            fi
        else
            show_status "WARNING" "Tailscale no está conectado (Estado: $ts_status)"
            show_status "INFO" "Ejecuta 'sudo tailscale up' para conectar"
        fi
    else
        show_status "WARNING" "Tailscale no está configurado o conectado"
        show_status "INFO" "Ejecuta 'sudo tailscale up' para la configuración inicial"
    fi
else
    show_status "ERROR" "Comando tailscale no está disponible"
fi

echo

echo "6. 🖥️  VERIFICANDO CONFIGURACIÓN DE REMMINA"
echo "========================================"

if command -v remmina &> /dev/null; then
    show_status "OK" "Remmina está instalado y disponible"
    
    # Verificar directorio de datos de Remmina
    if [ -d "$HOME/.local/share/remmina" ]; then
        show_status "OK" "Directorio de datos de Remmina existe"
    else
        show_status "WARNING" "Directorio de datos de Remmina no existe (se creará al usar Remmina)"
    fi
    
    # Verificar plugins de Remmina
    if [ -f "/usr/lib/remmina/plugins/remmina-plugin-rdp.so" ] || [ -f "/usr/lib64/remmina/plugins/remmina-plugin-rdp.so" ]; then
        show_status "OK" "Plugin RDP de Remmina está disponible"
    else
        show_status "WARNING" "Plugin RDP de Remmina podría no estar disponible"
    fi
else
    show_status "ERROR" "Remmina no está instalado"
fi

echo

echo "7. 🔥 VERIFICANDO CONFIGURACIÓN DE FIREWALL"
echo "========================================"

if command -v ufw &> /dev/null; then
    local ufw_status=$(sudo ufw status 2>/dev/null | head -1)
    if echo "$ufw_status" | grep -q "Status: active"; then
        show_status "OK" "UFW está activo"
        
        # Verificar reglas para Tailscale
        if sudo ufw status | grep -q "tailscale0"; then
            show_status "OK" "Reglas de firewall para Tailscale configuradas"
        else
            show_status "WARNING" "No se encontraron reglas específicas para Tailscale"
            show_status "INFO" "Ejecuta: sudo ufw allow in on tailscale0 && sudo ufw allow out on tailscale0"
        fi
    else
        show_status "INFO" "UFW no está activo (no es necesario configurar reglas)"
    fi
else
    show_status "INFO" "UFW no está instalado (firewall no configurado)"
fi

echo

echo "8. 📊 RESUMEN DE VERIFICACIÓN"
echo "=========================="

# Contar problemas críticos
critical_issues=0
warnings=0

# Verificar componentes críticos
if ! check_package "tailscale"; then ((critical_issues++)); fi
if ! check_package "remmina"; then ((critical_issues++)); fi
if ! systemctl is-enabled tailscaled &>/dev/null; then ((critical_issues++)); fi

# Mostrar resumen
if [ $critical_issues -eq 0 ]; then
    show_status "OK" "✨ Configuración persistente de RDP está funcionando correctamente"
    echo
    echo "🚀 PRÓXIMOS PASOS:"
    echo "1. Si Tailscale no está conectado, ejecuta: sudo tailscale up"
    echo "2. Para conectarte a RDP, usa: ~/rdp-connect.sh <IP_TAILSCALE>"
    echo "3. O abre Remmina manualmente desde el menú de aplicaciones"
else
    show_status "ERROR" "Se encontraron $critical_issues problemas críticos"
    echo
    echo "🔧 ACCIONES RECOMENDADAS:"
    echo "1. Ejecuta nuevamente el script de configuración: ./setup-persistent-rdp.sh"
    echo "2. Verifica los logs del sistema: journalctl -u tailscaled -f"
    echo "3. Reinicia el sistema y ejecuta esta verificación nuevamente"
fi

echo
echo "📝 LOGS ÚTILES PARA DIAGNÓSTICO:"
echo "• Tailscale: journalctl -u tailscaled -f"
echo "• Auto-conexión: journalctl -u tailscale-autoconnect -f"
echo "• Sistema: dmesg | tail -20"
echo
echo "=== Verificación completada ==="
