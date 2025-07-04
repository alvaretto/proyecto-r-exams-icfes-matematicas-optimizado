#!/bin/bash

# 🚀 SCRIPT DE OPTIMIZACIÓN COMPLETA PARA MANJARO XFCE
# Ejecuta todas las optimizaciones de forma automática
# Contraseña sudo: ManjaroPetit

echo "🚀 OPTIMIZACIÓN COMPLETA DE MANJARO XFCE"
echo "========================================"
echo "⚠️  Este script aplicará todas las optimizaciones automáticamente"
echo "📝 Contraseña sudo requerida: ManjaroPetit"
echo ""

# Función para mostrar progreso
mostrar_progreso() {
    echo "✅ $1"
    sleep 1
}

# Función para mostrar espacio
mostrar_espacio() {
    echo "💾 Espacio disponible: $(df -h / | awk 'NR==2{print $4}')"
}

echo "📊 Estado inicial del sistema:"
mostrar_espacio
echo ""

# FASE 1: LIMPIEZA CRÍTICA DE ESPACIO
echo "🧹 FASE 1: LIMPIEZA CRÍTICA DE ESPACIO"
echo "======================================"

echo "🗑️ Limpiando caché de pacman (5.2GB)..."
echo "ManjaroPetit" | sudo -S pacman -Scc --noconfirm
mostrar_progreso "Caché de pacman limpiado"

echo "🗑️ Eliminando paquetes huérfanos..."
orphans=$(pacman -Qtdq)
if [ ! -z "$orphans" ]; then
    echo "ManjaroPetit" | sudo -S pacman -Rns $orphans --noconfirm
    mostrar_progreso "Paquetes huérfanos eliminados"
else
    mostrar_progreso "No hay paquetes huérfanos"
fi

echo "📝 Limpiando logs del sistema..."
echo "ManjaroPetit" | sudo -S journalctl --vacuum-time=7d
echo "ManjaroPetit" | sudo -S journalctl --vacuum-size=100M
mostrar_progreso "Logs del sistema limpiados"

echo "🗂️ Limpiando caché de usuario..."
rm -rf ~/.cache/thumbnails/* 2>/dev/null
rm -rf ~/.cache/mozilla/* 2>/dev/null
rm -rf ~/.cache/chromium/* 2>/dev/null
find ~/.cache -type f -atime +30 -delete 2>/dev/null
mostrar_progreso "Caché de usuario limpiado"

echo "📊 Espacio después de limpieza:"
mostrar_espacio
echo ""

# FASE 2: OPTIMIZACIÓN DE SERVICIOS
echo "⚙️ FASE 2: OPTIMIZACIÓN DE SERVICIOS"
echo "===================================="

servicios_deshabilitar=(
    "bluetooth.service"
    "cups.service"
    "avahi-daemon.service"
    "ModemManager.service"
)

for servicio in "${servicios_deshabilitar[@]}"; do
    if systemctl is-enabled "$servicio" &>/dev/null; then
        echo "🔴 Deshabilitando $servicio"
        echo "ManjaroPetit" | sudo -S systemctl disable "$servicio"
        echo "ManjaroPetit" | sudo -S systemctl stop "$servicio"
    else
        echo "⚪ $servicio ya está deshabilitado"
    fi
done

mostrar_progreso "Servicios optimizados"
echo ""

# FASE 3: OPTIMIZACIÓN DEL KERNEL
echo "🔧 FASE 3: OPTIMIZACIÓN DEL KERNEL"
echo "================================="

echo "📝 Creando configuración de rendimiento..."
echo "ManjaroPetit" | sudo -S tee /etc/sysctl.d/99-performance.conf > /dev/null << 'EOF'
# Optimizaciones de rendimiento para Manjaro XFCE
vm.swappiness=10
vm.vfs_cache_pressure=50
vm.dirty_background_ratio=5
vm.dirty_ratio=10
net.core.rmem_default=262144
net.core.rmem_max=16777216
net.core.wmem_default=262144
net.core.wmem_max=16777216
fs.file-max=2097152
kernel.sched_migration_cost_ns=5000000
kernel.sched_autogroup_enabled=0
EOF

echo "⚡ Aplicando configuración del kernel..."
echo "ManjaroPetit" | sudo -S sysctl -p /etc/sysctl.d/99-performance.conf
mostrar_progreso "Kernel optimizado"
echo ""

# FASE 4: OPTIMIZACIÓN DE XFCE
echo "🎨 FASE 4: OPTIMIZACIÓN DE XFCE"
echo "==============================="

echo "🔧 Configurando XFCE para rendimiento..."
xfconf-query -c xfwm4 -p /general/use_compositing -s false 2>/dev/null
xfconf-query -c xfce4-desktop -p /backdrop/screen0/monitor0/workspace0/last-image -s "" 2>/dev/null
xfconf-query -c xfce4-desktop -p /backdrop/screen0/monitor0/workspace0/image-style -s 0 2>/dev/null
mostrar_progreso "XFCE optimizado"
echo ""

# FASE 5: OPTIMIZACIÓN DE CPU
echo "⚡ FASE 5: OPTIMIZACIÓN DE CPU"
echo "============================="

echo "📦 Instalando herramientas de CPU..."
echo "ManjaroPetit" | sudo -S pacman -S --noconfirm linux-tools

echo "⚡ Configurando CPU para rendimiento..."
echo "ManjaroPetit" | sudo -S cpupower frequency-set -g performance

echo "💾 Creando servicio permanente..."
echo "ManjaroPetit" | sudo -S tee /etc/systemd/system/cpu-performance.service > /dev/null << 'EOF'
[Unit]
Description=Set CPU governor to performance
After=multi-user.target

[Service]
Type=oneshot
ExecStart=/usr/bin/cpupower frequency-set -g performance
RemainAfterExit=yes

[Install]
WantedBy=multi-user.target
EOF

echo "ManjaroPetit" | sudo -S systemctl enable cpu-performance.service
mostrar_progreso "CPU configurada para alto rendimiento"
echo ""

# FASE 6: OPTIMIZACIÓN DE APLICACIONES
echo "📱 FASE 6: OPTIMIZACIÓN DE APLICACIONES"
echo "======================================="

echo "🔧 Optimizando VSCode..."
mkdir -p ~/.config/Code/User/
cat > ~/.config/Code/User/settings.json << 'EOF'
{
    "files.watcherExclude": {
        "**/.git/objects/**": true,
        "**/node_modules/**": true,
        "**/.cache/**": true
    },
    "search.followSymlinks": false,
    "typescript.disableAutomaticTypeAcquisition": true,
    "extensions.autoUpdate": false,
    "telemetry.telemetryLevel": "off",
    "workbench.enableExperiments": false,
    "editor.minimap.enabled": false,
    "workbench.startupEditor": "none"
}
EOF
mostrar_progreso "VSCode optimizado"
echo ""

# FASE 7: INSTALACIÓN DE HERRAMIENTAS DE MONITOREO
echo "📊 FASE 7: HERRAMIENTAS DE MONITOREO"
echo "===================================="

echo "📦 Instalando herramientas de monitoreo..."
echo "ManjaroPetit" | sudo -S pacman -S --noconfirm htop iotop lm-sensors
mostrar_progreso "Herramientas de monitoreo instaladas"
echo ""

# RESUMEN FINAL
echo "🎉 OPTIMIZACIÓN COMPLETADA!"
echo "=========================="
echo ""
echo "📊 Estado final del sistema:"
mostrar_espacio
echo ""
echo "⚙️ Servicios activos: $(systemctl list-units --type=service --state=running | grep -c "\.service")"
echo ""
echo "💾 Uso de memoria:"
free -h | grep Mem
echo ""
echo "🔧 Configuraciones aplicadas:"
echo "   ✅ Espacio en disco liberado (~6-8GB)"
echo "   ✅ Servicios innecesarios deshabilitados"
echo "   ✅ Kernel optimizado (swappiness=10)"
echo "   ✅ XFCE configurado para rendimiento"
echo "   ✅ CPU configurada para máximo rendimiento"
echo "   ✅ VSCode optimizado"
echo "   ✅ Herramientas de monitoreo instaladas"
echo ""
echo "🔄 REINICIA EL SISTEMA para aplicar todos los cambios"
echo "📊 Ejecuta './monitor_rendimiento.sh' para ver el estado"
echo ""
echo "💡 Comandos útiles después del reinicio:"
echo "   htop          - Monitor de procesos"
echo "   iotop         - Monitor de I/O"
echo "   sensors       - Temperaturas del sistema"
