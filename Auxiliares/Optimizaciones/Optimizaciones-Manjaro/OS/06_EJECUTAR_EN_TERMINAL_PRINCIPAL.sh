#!/bin/bash

# 🚀 SCRIPT PARA EJECUTAR EN TU TERMINAL PRINCIPAL
# Este script debe ejecutarse FUERA del entorno actual, en tu terminal de Manjaro

echo "🚀 OPTIMIZACIÓN MANJARO XFCE - TERMINAL PRINCIPAL"
echo "================================================="
echo "⚠️  EJECUTA ESTE SCRIPT EN TU TERMINAL PRINCIPAL DE MANJARO"
echo "📍 Ubicación: /home/pequeniomanjaro/Documentos/proyecto-r-exams-icfes-matematicas-optimizado/Auxiliares/Rexams-Lubuntu/Optimizaciones-Lubuntu/OS/"
echo ""

# Verificar si estamos en el entorno correcto
if [[ "$SUDO_USER" == "" ]] && [[ -f "/etc/manjaro-release" ]]; then
    echo "✅ Ejecutándose en Manjaro nativo"
else
    echo "⚠️  Este script debe ejecutarse en tu terminal principal de Manjaro"
    echo "📋 Copia y pega estos comandos en tu terminal principal:"
    echo ""
    echo "cd /home/pequeniomanjaro/Documentos/proyecto-r-exams-icfes-matematicas-optimizado/Auxiliares/Rexams-Lubuntu/Optimizaciones-Lubuntu/OS/"
    echo "bash 06_EJECUTAR_EN_TERMINAL_PRINCIPAL.sh"
    echo ""
    exit 1
fi

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
sudo pacman -Scc --noconfirm
mostrar_progreso "Caché de pacman limpiado"

echo "🗑️ Eliminando paquetes huérfanos..."
orphans=$(pacman -Qtdq 2>/dev/null)
if [ ! -z "$orphans" ]; then
    sudo pacman -Rns $orphans --noconfirm
    mostrar_progreso "Paquetes huérfanos eliminados"
else
    mostrar_progreso "No hay paquetes huérfanos"
fi

echo "📝 Limpiando logs del sistema..."
sudo journalctl --vacuum-time=7d
sudo journalctl --vacuum-size=100M
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
        sudo systemctl disable "$servicio"
        sudo systemctl stop "$servicio"
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
sudo tee /etc/sysctl.d/99-performance.conf > /dev/null << 'EOF'
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
sudo sysctl -p /etc/sysctl.d/99-performance.conf
mostrar_progreso "Kernel optimizado"
echo ""

# FASE 4: OPTIMIZACIÓN DE CPU
echo "⚡ FASE 4: OPTIMIZACIÓN DE CPU"
echo "============================="

echo "📦 Instalando herramientas de CPU..."
sudo pacman -S --noconfirm linux-tools

echo "⚡ Configurando CPU para rendimiento..."
sudo cpupower frequency-set -g performance

echo "💾 Creando servicio permanente..."
sudo tee /etc/systemd/system/cpu-performance.service > /dev/null << 'EOF'
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

sudo systemctl enable cpu-performance.service
mostrar_progreso "CPU configurada para alto rendimiento"
echo ""

# FASE 5: HERRAMIENTAS DE MONITOREO
echo "📊 FASE 5: HERRAMIENTAS DE MONITOREO"
echo "===================================="

echo "📦 Instalando herramientas de monitoreo..."
sudo pacman -S --noconfirm htop iotop lm-sensors
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
echo "   ✅ CPU configurada para máximo rendimiento"
echo "   ✅ Herramientas de monitoreo instaladas"
echo ""
echo "🔄 REINICIA EL SISTEMA para aplicar todos los cambios"
echo "📊 Ejecuta './02_monitor_rendimiento.sh' para ver el estado"
echo ""
echo "💡 Comandos útiles después del reinicio:"
echo "   htop          - Monitor de procesos"
echo "   iotop         - Monitor de I/O"
echo "   sensors       - Temperaturas del sistema"
