#!/bin/bash

# 🚀 SCRIPT DE OPTIMIZACIÓN PARA TERMINAL PRINCIPAL - MANJARO PLASMA KDE
# Ejecuta todas las optimizaciones con permisos sudo completos
# Diseñado para ejecutarse en el terminal nativo de Manjaro Plasma

echo "🚀 OPTIMIZACIÓN MANJARO PLASMA - TERMINAL PRINCIPAL"
echo "=================================================="
echo "⚡ Script optimizado para terminal nativo de Manjaro Plasma"
echo "🔑 Se solicitará contraseña sudo cuando sea necesario"
echo "🎨 Incluye optimizaciones específicas para KDE Plasma"
echo ""

# Verificar que estamos en un entorno KDE
if [ "$XDG_CURRENT_DESKTOP" != "KDE" ] && [ -z "$KDE_SESSION_VERSION" ]; then
    echo "⚠️ Advertencia: No se detectó entorno KDE Plasma"
    echo "Este script está optimizado para Manjaro Plasma KDE"
    read -p "¿Continuar de todos modos? (s/N): " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Ss]$ ]]; then
        echo "❌ Operación cancelada"
        exit 1
    fi
fi

# Función para mostrar progreso con colores
mostrar_progreso() {
    echo -e "\e[32m✅ $1\e[0m"
    sleep 1
}

mostrar_info() {
    echo -e "\e[34mℹ️  $1\e[0m"
}

mostrar_advertencia() {
    echo -e "\e[33m⚠️  $1\e[0m"
}

mostrar_error() {
    echo -e "\e[31m❌ $1\e[0m"
}

# Función para mostrar espacio
mostrar_espacio() {
    local espacio=$(df -h / | awk 'NR==2{print $4}')
    echo -e "\e[36m💾 Espacio disponible: $espacio\e[0m"
}

# Función para verificar si un comando existe
comando_existe() {
    command -v "$1" >/dev/null 2>&1
}

# Verificar permisos sudo
echo "🔐 Verificando permisos sudo..."
if ! sudo -n true 2>/dev/null; then
    echo "🔑 Se requiere contraseña sudo para continuar"
    sudo -v
    if [ $? -ne 0 ]; then
        mostrar_error "No se pudieron obtener permisos sudo"
        exit 1
    fi
fi

mostrar_progreso "Permisos sudo verificados"
echo ""

echo "📊 Estado inicial del sistema:"
mostrar_espacio
mostrar_info "Servicios activos: $(systemctl list-units --type=service --state=running | wc -l)"
mostrar_info "Desktop: $XDG_CURRENT_DESKTOP"
echo ""

# FASE 1: LIMPIEZA CRÍTICA DE ESPACIO
echo -e "\e[35m🧹 FASE 1: LIMPIEZA CRÍTICA DE ESPACIO\e[0m"
echo "======================================"

mostrar_info "Limpiando caché de pacman..."
sudo pacman -Scc --noconfirm
mostrar_progreso "Caché de pacman limpiado"

mostrar_info "Eliminando paquetes huérfanos..."
orphans=$(pacman -Qtdq 2>/dev/null)
if [ ! -z "$orphans" ]; then
    sudo pacman -Rns $orphans --noconfirm
    mostrar_progreso "Paquetes huérfanos eliminados"
else
    mostrar_progreso "No hay paquetes huérfanos"
fi

mostrar_info "Limpiando logs del sistema..."
sudo journalctl --vacuum-time=7d --quiet
sudo journalctl --vacuum-size=100M --quiet
mostrar_progreso "Logs del sistema limpiados"

mostrar_info "Limpiando caché de usuario..."
rm -rf ~/.cache/thumbnails/* 2>/dev/null
rm -rf ~/.cache/mozilla/* 2>/dev/null
rm -rf ~/.cache/chromium/* 2>/dev/null
find ~/.cache -type f -atime +30 -delete 2>/dev/null
mostrar_progreso "Caché de usuario limpiado"

mostrar_info "Limpiando caché específico de Plasma..."
rm -rf ~/.cache/plasma* 2>/dev/null
rm -rf ~/.cache/kio* 2>/dev/null
rm -rf ~/.cache/baloo* 2>/dev/null
rm -rf ~/.cache/ksvg-elements* 2>/dev/null
rm -rf ~/.cache/plasma-svgelements* 2>/dev/null
rm -rf ~/.cache/plasma_theme* 2>/dev/null
mostrar_progreso "Caché de Plasma limpiado"

echo ""
echo "📊 Espacio después de limpieza:"
mostrar_espacio
echo ""

# FASE 2: OPTIMIZACIÓN DE SERVICIOS
echo -e "\e[35m⚙️ FASE 2: OPTIMIZACIÓN DE SERVICIOS\e[0m"
echo "===================================="

servicios_sistema=(
    "bluetooth.service:Bluetooth"
    "cups.service:Impresoras"
    "avahi-daemon.service:Descubrimiento de red"
    "ModemManager.service:Módem"
)

for item in "${servicios_sistema[@]}"; do
    servicio="${item%%:*}"
    descripcion="${item##*:}"
    
    if systemctl is-enabled "$servicio" &>/dev/null; then
        mostrar_info "Deshabilitando $descripcion ($servicio)"
        sudo systemctl disable "$servicio" --quiet
        sudo systemctl stop "$servicio" --quiet
        mostrar_progreso "$descripcion deshabilitado"
    else
        mostrar_progreso "$descripcion ya está deshabilitado"
    fi
done

# Servicios de usuario KDE
mostrar_info "Optimizando servicios de usuario KDE..."
if systemctl --user is-enabled kdeconnectd.service &>/dev/null; then
    systemctl --user disable kdeconnectd.service --quiet
    systemctl --user stop kdeconnectd.service --quiet
    mostrar_progreso "KDE Connect deshabilitado"
else
    mostrar_progreso "KDE Connect ya está deshabilitado"
fi

echo ""

# FASE 3: OPTIMIZACIÓN ESPECÍFICA DE PLASMA
echo -e "\e[35m🎨 FASE 3: OPTIMIZACIÓN ESPECÍFICA DE PLASMA\e[0m"
echo "============================================"

if comando_existe balooctl; then
    mostrar_info "Configurando Baloo (indexador de archivos)..."
    balooctl disable 2>/dev/null
    balooctl purge 2>/dev/null
    balooctl enable 2>/dev/null
    
    # Configurar carpetas a indexar (solo documentos e imágenes)
    kwriteconfig5 --file baloofilerc --group "General" --key "folders[\$e]" "\$HOME/Documents/,\$HOME/Pictures/" 2>/dev/null
    kwriteconfig5 --file baloofilerc --group "General" --key "exclude filters" "*.tmp,*.temp,*.o,*.obj,*.so,*.a,*.log,*.cache" 2>/dev/null
    
    mostrar_progreso "Baloo configurado para mejor rendimiento"
else
    mostrar_advertencia "Baloo no disponible"
fi

if comando_existe kwriteconfig5; then
    mostrar_info "Optimizando efectos de escritorio..."
    
    # Deshabilitar efectos pesados
    kwriteconfig5 --file kwinrc --group Plugins --key blurEnabled false 2>/dev/null
    kwriteconfig5 --file kwinrc --group Plugins --key slideEnabled false 2>/dev/null
    kwriteconfig5 --file kwinrc --group Plugins --key fadeEnabled false 2>/dev/null
    kwriteconfig5 --file kwinrc --group Plugins --key minimizeanimationEnabled false 2>/dev/null
    
    # Optimizar compositor
    kwriteconfig5 --file kwinrc --group Compositing --key Backend OpenGL 2>/dev/null
    kwriteconfig5 --file kwinrc --group Compositing --key GLCore true 2>/dev/null
    kwriteconfig5 --file kwinrc --group Compositing --key HiddenPreviews 5 2>/dev/null
    kwriteconfig5 --file kwinrc --group Compositing --key MaxFPS 60 2>/dev/null
    
    # Reducir animaciones
    kwriteconfig5 --file plasmarc --group Units --key duration 1 2>/dev/null
    kwriteconfig5 --file plasmarc --group Units --key longDuration 2 2>/dev/null
    
    # Optimizar panel
    kwriteconfig5 --file plasmashellrc --group PlasmaViews --group Panel --key floating 0 2>/dev/null
    
    mostrar_progreso "Efectos de Plasma optimizados para rendimiento"
else
    mostrar_advertencia "kwriteconfig5 no disponible"
fi

echo ""

# FASE 4: OPTIMIZACIÓN DEL KERNEL
echo -e "\e[35m🔧 FASE 4: OPTIMIZACIÓN DEL KERNEL\e[0m"
echo "================================="

mostrar_info "Aplicando configuración de rendimiento del kernel..."
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

sudo sysctl --system --quiet
mostrar_progreso "Configuración del kernel aplicada"
echo ""

# FASE 5: OPTIMIZACIÓN DE CPU
echo -e "\e[35m⚡ FASE 5: OPTIMIZACIÓN DE CPU\e[0m"
echo "============================="

if ! comando_existe cpupower; then
    mostrar_info "Instalando cpupower..."
    sudo pacman -S cpupower --needed --noconfirm --quiet
fi

if [ -f /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor ]; then
    mostrar_info "Configurando CPU en modo performance..."
    sudo cpupower frequency-set -g performance &>/dev/null
    sudo systemctl enable cpupower.service --quiet
    echo 'governor="performance"' | sudo tee /etc/default/cpupower > /dev/null
    mostrar_progreso "CPU configurada en modo performance"
else
    mostrar_advertencia "Governor de CPU no disponible en este sistema"
fi

echo ""

# FASE 6: OPTIMIZACIÓN DE APLICACIONES
echo -e "\e[35m🚀 FASE 6: OPTIMIZACIÓN DE APLICACIONES\e[0m"
echo "======================================="

mostrar_info "Configurando variables de entorno Qt..."
if ! grep -q "QT_QPA_PLATFORMTHEME=kde" ~/.bashrc 2>/dev/null; then
    echo '# Optimizaciones Qt para KDE Plasma
export QT_QPA_PLATFORMTHEME=kde
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export QT_SCALE_FACTOR=1
export QT_FONT_DPI=96' >> ~/.bashrc
    mostrar_progreso "Variables Qt configuradas"
else
    mostrar_progreso "Variables Qt ya configuradas"
fi

mostrar_info "Configurando Firefox para KDE..."
if [ -d ~/.mozilla ]; then
    mkdir -p ~/.mozilla/firefox/
    echo 'user_pref("layers.acceleration.force-enabled", true);
user_pref("gfx.webrender.all", true);
user_pref("media.ffmpeg.vaapi.enabled", true);
user_pref("widget.use-xdg-desktop-portal.file-picker", 1);' > ~/.mozilla/firefox/user.js
    mostrar_progreso "Firefox optimizado para KDE"
else
    mostrar_progreso "Firefox no instalado"
fi

echo ""

# FASE 7: APLICAR CAMBIOS
echo -e "\e[35m🔄 FASE 7: APLICAR CAMBIOS\e[0m"
echo "========================="

mostrar_info "Reiniciando componentes de Plasma..."

# Reiniciar Plasma Shell
if comando_existe kquitapp5 && comando_existe kstart5; then
    kquitapp5 plasmashell 2>/dev/null
    sleep 2
    kstart5 plasmashell &
    mostrar_progreso "Plasma Shell reiniciado"
else
    mostrar_advertencia "Comandos de Plasma no disponibles"
fi

# Reiniciar KWin
if comando_existe kwin_x11; then
    kwin_x11 --replace &
    sleep 1
    mostrar_progreso "KWin reiniciado"
else
    mostrar_advertencia "KWin no disponible"
fi

# Reiniciar Baloo
if comando_existe balooctl; then
    balooctl restart 2>/dev/null
    mostrar_progreso "Baloo reiniciado"
fi

echo ""

# RESUMEN FINAL
echo -e "\e[32m📊 RESUMEN DE OPTIMIZACIÓN COMPLETADA\e[0m"
echo "===================================="
echo ""
echo -e "\e[32m✅ Limpieza de espacio completada\e[0m"
echo -e "\e[32m✅ Servicios optimizados\e[0m"
echo -e "\e[32m✅ Plasma configurado para rendimiento\e[0m"
echo -e "\e[32m✅ Kernel optimizado\e[0m"
echo -e "\e[32m✅ CPU configurada\e[0m"
echo -e "\e[32m✅ Aplicaciones optimizadas\e[0m"
echo ""

echo "📊 Estado final del sistema:"
mostrar_espacio
mostrar_info "Servicios activos: $(systemctl list-units --type=service --state=running | wc -l)"
mostrar_info "Swappiness: $(cat /proc/sys/vm/swappiness)"

if [ -f /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor ]; then
    mostrar_info "CPU Governor: $(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor)"
fi

echo ""
echo -e "\e[33m🎉 ¡OPTIMIZACIÓN COMPLETADA EXITOSAMENTE!\e[0m"
echo "========================================"
echo ""
echo -e "\e[36m📋 PRÓXIMOS PASOS:\e[0m"
echo "1. 🔄 Reinicia el sistema para aplicar todos los cambios: sudo reboot"
echo "2. 📊 Después del reinicio, ejecuta: ./02_monitor_rendimiento.sh"
echo "3. 🎯 Disfruta de tu Manjaro Plasma optimizado"
echo ""
echo -e "\e[36m💡 CONSEJOS:\e[0m"
echo "- Los efectos visuales se pueden reactivar desde Configuración del Sistema"
echo "- Baloo se puede reconfigurar desde Configuración del Sistema > Búsqueda"
echo "- Ejecuta este script mensualmente para mantenimiento"
echo "- Si experimentas problemas, revisa el archivo 03_COMANDOS_OPTIMIZACION_INMEDIATA.md"
echo ""
echo -e "\e[36m🔧 REVERSIÓN:\e[0m"
echo "- Para revertir servicios: sudo systemctl enable [nombre_servicio]"
echo "- Para revertir efectos: Configuración del Sistema > Efectos de Escritorio"
echo "- Para revertir CPU: sudo cpupower frequency-set -g ondemand"
echo ""

# Preguntar si quiere reiniciar ahora
echo -e "\e[33m🔄 ¿Deseas reiniciar el sistema ahora para aplicar todos los cambios?\e[0m"
read -p "Reiniciar ahora (s/N): " -n 1 -r
echo
if [[ $REPLY =~ ^[Ss]$ ]]; then
    echo -e "\e[32m🔄 Reiniciando sistema en 5 segundos...\e[0m"
    sleep 5
    sudo reboot
else
    echo -e "\e[36mℹ️  Recuerda reiniciar manualmente más tarde: sudo reboot\e[0m"
fi
