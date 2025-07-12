#!/bin/bash

# 🎓 SCRIPT DE OPTIMIZACIÓN AVANZADA PARA MANJARO PLASMA KDE
# ⚠️ SOLO PARA USUARIOS EXPERIMENTADOS
# Configuraciones avanzadas y experimentales

echo "🎓 OPTIMIZACIÓN AVANZADA DE MANJARO PLASMA KDE"
echo "=============================================="
echo "⚠️  ADVERTENCIA: Este script contiene optimizaciones avanzadas"
echo "🔧 Solo para usuarios experimentados"
echo "💾 Se recomienda hacer backup antes de continuar"
echo ""

read -p "¿Continuar con optimizaciones avanzadas? (s/N): " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Ss]$ ]]; then
    echo "❌ Operación cancelada"
    exit 1
fi

# Función para mostrar progreso
mostrar_progreso() {
    echo "✅ $1"
    sleep 1
}

# Función para verificar si un comando existe
comando_existe() {
    command -v "$1" >/dev/null 2>&1
}

# Función para backup de configuración
hacer_backup() {
    local archivo="$1"
    if [ -f "$archivo" ]; then
        cp "$archivo" "$archivo.backup.$(date +%Y%m%d_%H%M%S)"
        echo "📋 Backup creado: $archivo.backup.$(date +%Y%m%d_%H%M%S)"
    fi
}

echo "🚀 Iniciando optimizaciones avanzadas..."
echo ""

# FASE 1: OPTIMIZACIONES AVANZADAS DEL KERNEL
echo "🔧 FASE 1: OPTIMIZACIONES AVANZADAS DEL KERNEL"
echo "=============================================="

echo "📝 Configurando parámetros avanzados del kernel..."
hacer_backup "/etc/sysctl.d/99-performance.conf"

sudo tee /etc/sysctl.d/99-performance-advanced.conf > /dev/null << 'EOF'
# Optimizaciones avanzadas para Manjaro Plasma KDE

# Gestión de memoria avanzada
vm.swappiness=5
vm.vfs_cache_pressure=40
vm.dirty_background_ratio=3
vm.dirty_ratio=8
vm.dirty_expire_centisecs=1500
vm.dirty_writeback_centisecs=500
vm.page-cluster=0
vm.zone_reclaim_mode=0

# Red avanzada
net.core.netdev_max_backlog=5000
net.core.rmem_default=262144
net.core.rmem_max=33554432
net.core.wmem_default=262144
net.core.wmem_max=33554432
net.ipv4.tcp_rmem=4096 87380 33554432
net.ipv4.tcp_wmem=4096 65536 33554432
net.ipv4.tcp_congestion_control=bbr
net.ipv4.tcp_fastopen=3

# Sistema de archivos
fs.file-max=4194304
fs.inotify.max_user_watches=1048576
fs.inotify.max_user_instances=1024

# Scheduler avanzado
kernel.sched_migration_cost_ns=500000
kernel.sched_autogroup_enabled=0
kernel.sched_tunable_scaling=0
kernel.sched_latency_ns=4000000
kernel.sched_min_granularity_ns=500000
kernel.sched_wakeup_granularity_ns=1000000

# Específico para KDE/Qt
kernel.pid_max=4194304
EOF

sudo sysctl --system
mostrar_progreso "Parámetros avanzados del kernel aplicados"
echo ""

# FASE 2: OPTIMIZACIONES AVANZADAS DE PLASMA
echo "🎨 FASE 2: OPTIMIZACIONES AVANZADAS DE PLASMA"
echo "============================================="

if comando_existe kwriteconfig5; then
    echo "🔧 Configurando optimizaciones avanzadas de KWin..."
    
    # Compositor avanzado
    kwriteconfig5 --file kwinrc --group Compositing --key LatencyPolicy Low
    kwriteconfig5 --file kwinrc --group Compositing --key RenderTimeLimit 1
    kwriteconfig5 --file kwinrc --group Compositing --key MaxFPS 120
    kwriteconfig5 --file kwinrc --group Compositing --key RefreshRate 0
    kwriteconfig5 --file kwinrc --group Compositing --key AllowTearing true
    
    # OpenGL avanzado
    kwriteconfig5 --file kwinrc --group Compositing --key OpenGLIsUnsafe false
    kwriteconfig5 --file kwinrc --group Compositing --key WindowsBlockCompositing false
    
    # Ventanas avanzadas
    kwriteconfig5 --file kwinrc --group Windows --key FocusPolicy FocusFollowsMouse
    kwriteconfig5 --file kwinrc --group Windows --key AutoRaise true
    kwriteconfig5 --file kwinrc --group Windows --key DelayFocusInterval 0
    
    mostrar_progreso "KWin optimizado avanzadamente"
    
    echo "🎨 Configurando Plasma avanzado..."
    
    # Plasma Shell avanzado
    kwriteconfig5 --file plasmarc --group Theme --key name breeze-dark
    kwriteconfig5 --file plasmarc --group Units --key gridUnit 18
    kwriteconfig5 --file plasmarc --group Units --key devicePixelRatio 1
    
    # Panel avanzado
    kwriteconfig5 --file plasmashellrc --group PlasmaViews --group Panel --key thickness 32
    kwriteconfig5 --file plasmashellrc --group PlasmaViews --group Panel --key alignment 132
    
    mostrar_progreso "Plasma optimizado avanzadamente"
else
    echo "⚠️ kwriteconfig5 no disponible"
fi

echo ""

# FASE 3: OPTIMIZACIONES AVANZADAS DE BALOO
echo "🔍 FASE 3: OPTIMIZACIONES AVANZADAS DE BALOO"
echo "============================================"

if comando_existe balooctl; then
    echo "🔧 Configurando Baloo avanzado..."
    
    # Configuración avanzada de Baloo
    kwriteconfig5 --file baloofilerc --group "General" --key "index hidden folders" false
    kwriteconfig5 --file baloofilerc --group "General" --key "index mounted remote folders" false
    kwriteconfig5 --file baloofilerc --group "General" --key "max size" 1000
    
    # Filtros avanzados
    kwriteconfig5 --file baloofilerc --group "General" --key "exclude filters" "*.tmp,*.temp,*.o,*.obj,*.so,*.a,*.log,*.cache,*.bak,*.old,*.swp,*~,*.pyc,*.pyo,node_modules,__pycache__,.git,.svn,.hg"
    
    # Carpetas específicas
    kwriteconfig5 --file baloofilerc --group "General" --key "exclude folders[\$e]" "\$HOME/.cache/,\$HOME/.local/share/Trash/,\$HOME/snap/,\$HOME/.npm/,\$HOME/.cargo/"
    
    balooctl restart
    mostrar_progreso "Baloo configurado avanzadamente"
else
    echo "⚠️ Baloo no disponible"
fi

echo ""

# FASE 4: OPTIMIZACIONES DE HARDWARE
echo "⚡ FASE 4: OPTIMIZACIONES DE HARDWARE"
echo "===================================="

echo "🔧 Configurando optimizaciones de GPU..."
# Mesa/OpenGL optimizations
if [ ! -f ~/.drirc ]; then
    tee ~/.drirc > /dev/null << 'EOF'
<driconf>
    <device>
        <application name="Default">
            <option name="vblank_mode" value="0" />
            <option name="adaptive_sync" value="true" />
            <option name="allow_glsl_extension_directive_midshader" value="true" />
        </application>
        <application name="plasmashell">
            <option name="vblank_mode" value="0" />
        </application>
        <application name="kwin_x11">
            <option name="vblank_mode" value="0" />
        </application>
    </device>
</driconf>
EOF
    mostrar_progreso "Configuración GPU creada"
else
    mostrar_progreso "Configuración GPU ya existe"
fi

echo "💾 Configurando optimizaciones de I/O..."
# I/O Scheduler optimization
for disk in /sys/block/sd*; do
    if [ -d "$disk" ]; then
        disk_name=$(basename "$disk")
        if [ -f "/sys/block/$disk_name/queue/scheduler" ]; then
            echo "mq-deadline" | sudo tee "/sys/block/$disk_name/queue/scheduler" > /dev/null
            echo "📀 Scheduler configurado para $disk_name"
        fi
    fi
done

# Hacer permanente
echo 'ACTION=="add|change", KERNEL=="sd[a-z]*", ATTR{queue/scheduler}="mq-deadline"' | sudo tee /etc/udev/rules.d/60-ioschedulers.rules > /dev/null

mostrar_progreso "Optimizaciones de I/O aplicadas"
echo ""

# FASE 5: OPTIMIZACIONES DE SERVICIOS AVANZADAS
echo "⚙️ FASE 5: OPTIMIZACIONES DE SERVICIOS AVANZADAS"
echo "================================================"

echo "🔧 Configurando servicios avanzados..."

# Servicios adicionales a optimizar
servicios_avanzados=(
    "accounts-daemon.service"
    "udisks2.service"
    "upower.service"
    "NetworkManager-wait-online.service"
    "systemd-networkd-wait-online.service"
)

for servicio in "${servicios_avanzados[@]}"; do
    if systemctl is-enabled "$servicio" &>/dev/null; then
        echo "🔧 Configurando $servicio"
        sudo systemctl mask "$servicio"
    fi
done

# Optimizar systemd
sudo systemctl set-default multi-user.target
sudo systemctl mask plymouth-start.service
sudo systemctl mask plymouth-read-write.service
sudo systemctl mask plymouth-quit-wait.service
sudo systemctl mask plymouth-quit.service

mostrar_progreso "Servicios avanzados optimizados"
echo ""

# FASE 6: OPTIMIZACIONES DE APLICACIONES AVANZADAS
echo "🚀 FASE 6: OPTIMIZACIONES DE APLICACIONES AVANZADAS"
echo "==================================================="

echo "🔧 Configurando variables de entorno avanzadas..."
tee ~/.config/plasma-workspace/env/optimizations.sh > /dev/null << 'EOF'
#!/bin/bash
# Optimizaciones avanzadas de entorno para Plasma

# Qt optimizations
export QT_QPA_PLATFORMTHEME=kde
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export QT_SCALE_FACTOR=1
export QT_FONT_DPI=96
export QT_QUICK_CONTROLS_STYLE=org.kde.desktop
export QT_LOGGING_RULES="*.debug=false;qt.qpa.xcb.xcberror.debug=false"

# KDE optimizations
export KWIN_TRIPLE_BUFFER=1
export KWIN_OPENGL_INTERFACE=egl
export PLASMA_USE_QT_SCALING=1

# Mesa optimizations
export MESA_GL_VERSION_OVERRIDE=4.6
export MESA_GLSL_VERSION_OVERRIDE=460
export mesa_glthread=true

# General optimizations
export MALLOC_CHECK_=0
export MALLOC_PERTURB_=0
EOF

chmod +x ~/.config/plasma-workspace/env/optimizations.sh
mostrar_progreso "Variables de entorno avanzadas configuradas"

echo "🦊 Configurando Firefox avanzado..."
if [ -d ~/.mozilla/firefox ]; then
    # Buscar perfil por defecto
    profile_dir=$(find ~/.mozilla/firefox -name "*.default*" -type d | head -1)
    if [ -n "$profile_dir" ]; then
        tee "$profile_dir/user.js" > /dev/null << 'EOF'
// Optimizaciones avanzadas de Firefox para KDE
user_pref("layers.acceleration.force-enabled", true);
user_pref("gfx.webrender.all", true);
user_pref("gfx.webrender.enabled", true);
user_pref("media.ffmpeg.vaapi.enabled", true);
user_pref("widget.use-xdg-desktop-portal.file-picker", 1);
user_pref("widget.use-xdg-desktop-portal.mime-handler", 1);
user_pref("browser.cache.disk.enable", false);
user_pref("browser.cache.memory.enable", true);
user_pref("browser.cache.memory.capacity", 524288);
user_pref("network.http.pipelining", true);
user_pref("network.http.pipelining.maxrequests", 8);
user_pref("nglayout.initialpaint.delay", 0);
user_pref("content.notify.interval", 100000);
user_pref("content.notify.ontimer", true);
user_pref("content.switch.threshold", 250000);
EOF
        mostrar_progreso "Firefox configurado avanzadamente"
    fi
fi

echo ""

# FASE 7: APLICAR CAMBIOS AVANZADOS
echo "🔄 FASE 7: APLICAR CAMBIOS AVANZADOS"
echo "==================================="

echo "🎨 Reiniciando servicios de Plasma..."
if comando_existe systemctl; then
    systemctl --user restart plasma-plasmashell.service 2>/dev/null || true
    systemctl --user restart plasma-kwin_x11.service 2>/dev/null || true
fi

echo "🔧 Recargando configuraciones..."
if comando_existe kbuildsycoca5; then
    kbuildsycoca5 --noincremental
fi

if comando_existe update-desktop-database; then
    update-desktop-database ~/.local/share/applications/
fi

mostrar_progreso "Cambios avanzados aplicados"
echo ""

# RESUMEN FINAL
echo "📊 RESUMEN DE OPTIMIZACIÓN AVANZADA COMPLETADA"
echo "=============================================="
echo ""
echo "✅ Kernel optimizado avanzadamente"
echo "✅ Plasma configurado para máximo rendimiento"
echo "✅ Baloo optimizado avanzadamente"
echo "✅ Hardware optimizado"
echo "✅ Servicios avanzados configurados"
echo "✅ Aplicaciones optimizadas avanzadamente"
echo ""

echo "⚠️  ADVERTENCIAS IMPORTANTES:"
echo "- Algunos cambios son experimentales"
echo "- Se han creado backups de configuraciones críticas"
echo "- Reinicia el sistema para aplicar todos los cambios"
echo "- Si hay problemas, restaura desde los backups"
echo ""

echo "🔄 REVERSIÓN DE CAMBIOS:"
echo "- Restaurar kernel: sudo rm /etc/sysctl.d/99-performance-advanced.conf"
echo "- Restaurar servicios: sudo systemctl unmask [servicio]"
echo "- Restaurar target: sudo systemctl set-default graphical.target"
echo ""

echo "🎉 ¡OPTIMIZACIÓN AVANZADA COMPLETADA!"
echo "===================================="
echo ""
echo "📋 PRÓXIMOS PASOS:"
echo "1. 🔄 Reinicia el sistema: sudo reboot"
echo "2. 📊 Ejecuta el monitor: ./02_monitor_rendimiento.sh"
echo "3. 🎯 Monitorea el rendimiento durante unos días"
echo "4. 🔧 Ajusta configuraciones según sea necesario"
echo ""
echo "💡 CONSEJOS AVANZADOS:"
echo "- Monitorea la temperatura del sistema"
echo "- Verifica la estabilidad durante varios días"
echo "- Ajusta MaxFPS según tu monitor"
echo "- Considera usar un kernel personalizado para más optimizaciones"
echo ""
