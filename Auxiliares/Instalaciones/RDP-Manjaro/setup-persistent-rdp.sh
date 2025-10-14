#!/bin/bash

# Script para configurar RDP persistente en Manjaro Linux
# Este script hace que todas las configuraciones sean permanentes después de reinicios

set -e  # Salir si hay algún error

echo "=== Configuración Persistente de RDP en Manjaro Linux ==="
echo "Este script configurará RDP de forma permanente en tu sistema"
echo

# Función para verificar si el usuario es root
check_root() {
    if [[ $EUID -eq 0 ]]; then
        echo "❌ No ejecutes este script como root. Usa tu usuario normal."
        echo "El script pedirá permisos sudo cuando sea necesario."
        exit 1
    fi
}

# Función para verificar conectividad a internet
check_internet() {
    echo "🔍 Verificando conectividad a internet..."
    if ! ping -c 1 google.com &> /dev/null; then
        echo "❌ No hay conexión a internet. Verifica tu conexión y vuelve a intentar."
        exit 1
    fi
    echo "✅ Conexión a internet verificada"
}

# Función para actualizar el sistema
update_system() {
    echo "🔄 Actualizando el sistema..."
    sudo pacman -Syu --noconfirm
    echo "✅ Sistema actualizado"
}

# Función para instalar paquetes necesarios
install_packages() {
    echo "📦 Instalando paquetes necesarios..."
    
    # Lista de paquetes requeridos
    packages=(
        "tailscale"
        "remmina"
        "freerdp"
        "libvncserver"
        "spice-gtk"
        "telepathy-glib"
    )
    
    for package in "${packages[@]}"; do
        if pacman -Qi "$package" &> /dev/null; then
            echo "✅ $package ya está instalado"
        else
            echo "📥 Instalando $package..."
            sudo pacman -S --noconfirm "$package"
        fi
    done
    
    echo "✅ Todos los paquetes instalados correctamente"
}

# Función para configurar Tailscale de forma persistente
configure_tailscale() {
    echo "🔧 Configurando Tailscale de forma persistente..."
    
    # Habilitar y iniciar el servicio tailscaled
    echo "🚀 Habilitando servicio tailscaled..."
    sudo systemctl enable tailscaled
    sudo systemctl start tailscaled
    
    # Verificar que el servicio esté activo
    if systemctl is-active --quiet tailscaled; then
        echo "✅ Servicio tailscaled activo y habilitado para inicio automático"
    else
        echo "❌ Error: No se pudo iniciar el servicio tailscaled"
        exit 1
    fi
    
    # Crear archivo de configuración para auto-reconexión
    echo "📝 Creando configuración de auto-reconexión..."
    sudo mkdir -p /etc/systemd/system/tailscaled.service.d/
    
    cat << 'EOF' | sudo tee /etc/systemd/system/tailscaled.service.d/override.conf > /dev/null
[Service]
# Reiniciar automáticamente si el servicio falla
Restart=always
RestartSec=5

# Variables de entorno para configuración persistente
Environment="TS_ACCEPT_DNS=true"
Environment="TS_EXTRA_ARGS=--reset"
EOF
    
    # Recargar configuración de systemd
    sudo systemctl daemon-reload
    sudo systemctl restart tailscaled
    
    echo "✅ Tailscale configurado para inicio automático y reconexión"
}

# Función para configurar Remmina
configure_remmina() {
    echo "🔧 Configurando Remmina..."
    
    # Crear directorio de configuración de Remmina si no existe
    mkdir -p ~/.local/share/remmina
    mkdir -p ~/.config/remmina
    
    # Configuración básica de Remmina
    cat << 'EOF' > ~/.config/remmina/remmina.pref
[remmina]
datadir_path=/home/$USER/.local/share/remmina
remmina_file_name=%G_%P_%N_%h.remmina
screenshot_path=/home/$USER
screenshot_name=remmina_%p_%h_%Y%m%d-%H%M%S.png
console_font=
terminal_font=
scrollback_lines=512
resolutions=640x480,800x600,1024x768,1152x864,1280x960,1400x1050,1440x900,1680x1050,1600x1200,1920x1080,1920x1200,2560x1440
keystrokes=
log_level=INFO
scale_quality=3
hide_connection_toolbar=false
hide_toolbar=false
small_toolbutton=false
view_file_mode=0
confirm_close=true
use_master_password=false
unlock_timeout=300
floating_toolbar_placement=0
toolbar_placement=0
grab_color=#00FF00
EOF
    
    echo "✅ Remmina configurado con ajustes optimizados"
}

# Función para crear script de conexión automática
create_connection_script() {
    echo "📝 Creando script de conexión automática..."
    
    cat << 'EOF' > ~/rdp-connect.sh
#!/bin/bash

# Script para conectar automáticamente a RDP a través de Tailscale
# Uso: ./rdp-connect.sh [IP_TAILSCALE]

RDP_IP="$1"
RDP_USER="RDP"

if [ -z "$RDP_IP" ]; then
    echo "❌ Error: Debes proporcionar la IP de Tailscale"
    echo "Uso: $0 <IP_TAILSCALE>"
    echo "Ejemplo: $0 100.64.0.1"
    exit 1
fi

echo "🔗 Conectando a RDP..."
echo "IP: $RDP_IP"
echo "Usuario: $RDP_USER"
echo

# Verificar que Tailscale esté conectado
if ! tailscale status &> /dev/null; then
    echo "⚠️  Tailscale no está conectado. Ejecutando 'tailscale up'..."
    sudo tailscale up
fi

# Abrir Remmina con la configuración
remmina -c rdp://$RDP_USER@$RDP_IP:3389

EOF
    
    chmod +x ~/rdp-connect.sh
    echo "✅ Script de conexión creado en ~/rdp-connect.sh"
}

# Función para configurar firewall (si está habilitado)
configure_firewall() {
    echo "🔥 Configurando firewall..."
    
    # Verificar si ufw está instalado y activo
    if command -v ufw &> /dev/null && sudo ufw status | grep -q "Status: active"; then
        echo "🔧 UFW detectado y activo, configurando reglas..."
        
        # Permitir tráfico de Tailscale
        sudo ufw allow in on tailscale0
        sudo ufw allow out on tailscale0
        
        echo "✅ Reglas de firewall configuradas para Tailscale"
    else
        echo "ℹ️  UFW no está activo o no está instalado, omitiendo configuración de firewall"
    fi
}

# Función para crear servicio de auto-conexión Tailscale
create_tailscale_autoconnect() {
    echo "🔧 Creando servicio de auto-conexión Tailscale..."
    
    # Crear script de auto-conexión
    cat << 'EOF' | sudo tee /usr/local/bin/tailscale-autoconnect.sh > /dev/null
#!/bin/bash

# Script para auto-conectar Tailscale al inicio
# Se ejecuta después de que el servicio tailscaled esté listo

sleep 10  # Esperar a que tailscaled esté completamente listo

# Verificar si ya está conectado
if tailscale status --json | jq -r '.BackendState' | grep -q "Running"; then
    echo "Tailscale ya está conectado"
    exit 0
fi

# Intentar conectar (requiere que ya se haya hecho 'tailscale up' manualmente al menos una vez)
if [ -f /var/lib/tailscale/tailscaled.state ]; then
    tailscale up
    echo "Tailscale auto-conectado"
else
    echo "Tailscale no ha sido configurado inicialmente. Ejecuta 'sudo tailscale up' manualmente."
fi
EOF
    
    sudo chmod +x /usr/local/bin/tailscale-autoconnect.sh
    
    # Crear servicio systemd para auto-conexión
    cat << 'EOF' | sudo tee /etc/systemd/system/tailscale-autoconnect.service > /dev/null
[Unit]
Description=Tailscale Auto-Connect
After=tailscaled.service
Wants=tailscaled.service
Requires=network-online.target
After=network-online.target

[Service]
Type=oneshot
ExecStart=/usr/local/bin/tailscale-autoconnect.sh
User=root
RemainAfterExit=yes

[Install]
WantedBy=multi-user.target
EOF
    
    # Habilitar el servicio
    sudo systemctl daemon-reload
    sudo systemctl enable tailscale-autoconnect.service
    
    echo "✅ Servicio de auto-conexión Tailscale creado y habilitado"
}

# Función para mostrar información post-instalación
show_post_install_info() {
    echo
    echo "🎉 ¡Configuración completada exitosamente!"
    echo
    echo "=== INFORMACIÓN IMPORTANTE ==="
    echo
    echo "1. 📋 SERVICIOS HABILITADOS:"
    echo "   • tailscaled.service - Servicio principal de Tailscale"
    echo "   • tailscale-autoconnect.service - Auto-conexión al inicio"
    echo
    echo "2. 🔧 CONFIGURACIÓN INICIAL REQUERIDA:"
    echo "   Para la primera conexión, ejecuta:"
    echo "   sudo tailscale up"
    echo "   (Sigue el enlace para autenticar tu dispositivo)"
    echo
    echo "3. 🚀 CONEXIÓN A RDP:"
    echo "   Usa el script: ~/rdp-connect.sh <IP_TAILSCALE>"
    echo "   O abre Remmina manualmente"
    echo
    echo "4. 🔄 PERSISTENCIA:"
    echo "   • Tailscale se iniciará automáticamente en cada reinicio"
    echo "   • Remmina está configurado con ajustes optimizados"
    echo "   • Las configuraciones sobrevivirán a reinicios del sistema"
    echo
    echo "5. 📝 ARCHIVOS CREADOS:"
    echo "   • ~/rdp-connect.sh - Script de conexión rápida"
    echo "   • ~/.config/remmina/remmina.pref - Configuración de Remmina"
    echo "   • /etc/systemd/system/tailscaled.service.d/override.conf - Config Tailscale"
    echo "   • /etc/systemd/system/tailscale-autoconnect.service - Auto-conexión"
    echo
    echo "=== PRÓXIMOS PASOS ==="
    echo "1. Reinicia tu sistema para verificar que todo funcione"
    echo "2. Ejecuta 'sudo tailscale up' para la configuración inicial"
    echo "3. Usa ~/rdp-connect.sh para conectarte a tus servidores RDP"
    echo
}

# Función principal
main() {
    echo "Iniciando configuración persistente de RDP en Manjaro..."
    echo
    
    check_root
    check_internet
    update_system
    install_packages
    configure_tailscale
    configure_remmina
    create_connection_script
    configure_firewall
    create_tailscale_autoconnect
    show_post_install_info
    
    echo "✅ ¡Configuración completada! El sistema está listo para RDP persistente."
}

# Ejecutar función principal
main "$@"
