#!/bin/bash

# 🚀 SCRIPT DE OPTIMIZACIÓN COMPLETA PARA MANJARO PLASMA KDE
# Ejecuta todas las optimizaciones de forma automática
# Contraseña sudo: (configura la tuya)

echo "🚀 OPTIMIZACIÓN COMPLETA DE MANJARO PLASMA KDE"
echo "=============================================="
echo "⚠️  Este script aplicará todas las optimizaciones automáticamente"
echo "🔑 Se solicitará contraseña sudo cuando sea necesario"
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

# Función para verificar si un comando existe
comando_existe() {
    command -v "$1" >/dev/null 2>&1
}

echo "📊 Estado inicial del sistema:"
mostrar_espacio
echo ""

# FASE 1: LIMPIEZA CRÍTICA DE ESPACIO
echo "🧹 FASE 1: LIMPIEZA CRÍTICA DE ESPACIO"
echo "======================================"

echo "🗑️ Limpiando caché de pacman..."
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

echo "🎨 Limpiando caché específico de Plasma..."
rm -rf ~/.cache/plasma* 2>/dev/null
rm -rf ~/.cache/kio* 2>/dev/null
rm -rf ~/.cache/baloo* 2>/dev/null
rm -rf ~/.cache/ksvg-elements* 2>/dev/null
rm -rf ~/.cache/plasma-svgelements* 2>/dev/null
rm -rf ~/.cache/plasma_theme* 2>/dev/null
mostrar_progreso "Caché de Plasma limpiado"

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

# Servicios de usuario KDE (opcional)
echo "🎨 Optimizando servicios de usuario KDE..."
if systemctl --user is-enabled kdeconnectd.service &>/dev/null; then
    systemctl --user disable kdeconnectd.service
    systemctl --user stop kdeconnectd.service
    echo "🔴 KDE Connect deshabilitado"
else
    echo "⚪ KDE Connect ya está deshabilitado"
fi

mostrar_progreso "Servicios optimizados"
echo ""

# FASE 3: OPTIMIZACIÓN ESPECÍFICA DE PLASMA
echo "🎨 FASE 3: OPTIMIZACIÓN ESPECÍFICA DE PLASMA"
echo "============================================"

echo "🔍 Configurando Baloo (indexador)..."
if comando_existe balooctl; then
    balooctl disable
    balooctl purge
    balooctl enable
    
    # Configurar carpetas a indexar
    kwriteconfig5 --file baloofilerc --group "General" --key "folders[\$e]" "\$HOME/Documents/,\$HOME/Pictures/"
    kwriteconfig5 --file baloofilerc --group "General" --key "exclude filters" "*.tmp,*.temp,*.o,*.obj,*.so,*.a,*.log,*.cache"
    
    mostrar_progreso "Baloo configurado para mejor rendimiento"
else
    echo "⚠️ Baloo no disponible"
fi

echo "✨ Optimizando efectos de escritorio..."
if comando_existe kwriteconfig5; then
    # Deshabilitar efectos pesados
    kwriteconfig5 --file kwinrc --group Plugins --key blurEnabled false
    kwriteconfig5 --file kwinrc --group Plugins --key slideEnabled false
    kwriteconfig5 --file kwinrc --group Plugins --key fadeEnabled false
    kwriteconfig5 --file kwinrc --group Plugins --key minimizeanimationEnabled false
    
    # Optimizar compositor
    kwriteconfig5 --file kwinrc --group Compositing --key Backend OpenGL
    kwriteconfig5 --file kwinrc --group Compositing --key GLCore true
    kwriteconfig5 --file kwinrc --group Compositing --key HiddenPreviews 5
    kwriteconfig5 --file kwinrc --group Compositing --key MaxFPS 60
    
    # Reducir animaciones
    kwriteconfig5 --file plasmarc --group Units --key duration 1
    kwriteconfig5 --file plasmarc --group Units --key longDuration 2
    
    # Optimizar panel
    kwriteconfig5 --file plasmashellrc --group PlasmaViews --group Panel --key floating 0
    
    mostrar_progreso "Efectos de Plasma optimizados"
else
    echo "⚠️ kwriteconfig5 no disponible"
fi

echo ""

# FASE 4: OPTIMIZACIÓN DEL KERNEL
echo "🔧 FASE 4: OPTIMIZACIÓN DEL KERNEL"
echo "================================="

echo "📝 Creando configuración de rendimiento..."
sudo tee /etc/sysctl.d/99-performance.conf > /dev/null << 'EOF'
# Optimizaciones de rendimiento para Manjaro Plasma
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
# Específico para KDE/Qt
fs.inotify.max_user_watches=524288
EOF

sudo sysctl --system
mostrar_progreso "Configuración del kernel aplicada"
echo ""

# FASE 5: OPTIMIZACIÓN DE CPU
echo "⚡ FASE 5: OPTIMIZACIÓN DE CPU"
echo "============================="

echo "🔧 Configurando governor de CPU..."
if ! comando_existe cpupower; then
    echo "📦 Instalando cpupower..."
    sudo pacman -S cpupower --needed --noconfirm
fi

if [ -f /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor ]; then
    sudo cpupower frequency-set -g performance 2>/dev/null
    sudo systemctl enable cpupower.service
    echo 'governor="performance"' | sudo tee /etc/default/cpupower > /dev/null
    mostrar_progreso "CPU configurada en modo performance"
else
    echo "⚠️ Governor de CPU no disponible en este sistema"
fi

echo ""

# FASE 6: OPTIMIZACIÓN DE APLICACIONES
echo "🚀 FASE 6: OPTIMIZACIÓN DE APLICACIONES"
echo "======================================="

echo "🔧 Configurando variables de entorno Qt..."
if ! grep -q "QT_QPA_PLATFORMTHEME=kde" ~/.bashrc 2>/dev/null; then
    echo 'export QT_QPA_PLATFORMTHEME=kde
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export QT_SCALE_FACTOR=1
export QT_FONT_DPI=96' >> ~/.bashrc
    mostrar_progreso "Variables Qt configuradas"
else
    mostrar_progreso "Variables Qt ya configuradas"
fi

echo "🦊 Configurando Firefox para KDE..."
if [ -d ~/.mozilla ]; then
    mkdir -p ~/.mozilla/firefox/
    echo 'user_pref("layers.acceleration.force-enabled", true);
user_pref("gfx.webrender.all", true);
user_pref("media.ffmpeg.vaapi.enabled", true);
user_pref("widget.use-xdg-desktop-portal.file-picker", 1);' > ~/.mozilla/firefox/user.js
    mostrar_progreso "Firefox optimizado para KDE"
else
    echo "⚪ Firefox no instalado"
fi

echo ""

# FASE 7: APLICAR CAMBIOS
echo "🔄 FASE 7: APLICAR CAMBIOS"
echo "========================="

echo "🎨 Reiniciando Plasma Shell..."
if comando_existe kquitapp5 && comando_existe kstart5; then
    kquitapp5 plasmashell 2>/dev/null
    sleep 2
    kstart5 plasmashell &
    mostrar_progreso "Plasma Shell reiniciado"
else
    echo "⚠️ Comandos de Plasma no disponibles"
fi

echo "🖼️ Reiniciando KWin..."
if comando_existe kwin_x11; then
    kwin_x11 --replace &
    mostrar_progreso "KWin reiniciado"
else
    echo "⚠️ KWin no disponible"
fi

echo "🔍 Reiniciando Baloo..."
if comando_existe balooctl; then
    balooctl restart
    mostrar_progreso "Baloo reiniciado"
fi

echo ""

# RESUMEN FINAL
echo "📊 RESUMEN DE OPTIMIZACIÓN COMPLETADA"
echo "===================================="
echo ""
echo "✅ Limpieza de espacio completada"
echo "✅ Servicios optimizados"
echo "✅ Plasma configurado para rendimiento"
echo "✅ Kernel optimizado"
echo "✅ CPU configurada"
echo "✅ Aplicaciones optimizadas"
echo ""

echo "📊 Estado final del sistema:"
mostrar_espacio
echo "⚙️ Servicios activos: $(systemctl list-units --type=service --state=running | wc -l)"
echo "🔧 Swappiness: $(cat /proc/sys/vm/swappiness)"

if [ -f /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor ]; then
    echo "⚡ CPU Governor: $(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor)"
fi

echo ""
echo "🎉 ¡OPTIMIZACIÓN COMPLETADA!"
echo "=========================="
echo ""
echo "📋 PRÓXIMOS PASOS:"
echo "1. 🔄 Reinicia el sistema: sudo reboot"
echo "2. 📊 Ejecuta el monitor: ./02_monitor_rendimiento.sh"
echo "3. 🎯 Disfruta de tu Manjaro Plasma optimizado"
echo ""
echo "💡 CONSEJOS:"
echo "- Los efectos se pueden reactivar desde Configuración del Sistema"
echo "- Baloo se puede reconfigurar desde Configuración del Sistema > Búsqueda"
echo "- Ejecuta este script mensualmente para mantenimiento"
echo ""
