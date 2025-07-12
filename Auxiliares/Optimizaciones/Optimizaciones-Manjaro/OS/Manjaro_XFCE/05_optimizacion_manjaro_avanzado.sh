#!/bin/bash

# 🚀 SCRIPT DE OPTIMIZACIÓN PROFUNDA PARA MANJARO XFCE
# Autor: Asistente de Optimización
# Fecha: $(date)

echo "🚀 INICIANDO OPTIMIZACIÓN PROFUNDA DE MANJARO XFCE..."
echo "=================================================="

# Función para mostrar espacio liberado
mostrar_espacio() {
    echo "💾 Espacio disponible: $(df -h / | awk 'NR==2{print $4}')"
}

echo "📊 Estado inicial:"
mostrar_espacio

# 1. LIMPIEZA CRÍTICA DE ALMACENAMIENTO
echo ""
echo "🧹 FASE 1: LIMPIEZA CRÍTICA DE ALMACENAMIENTO"
echo "============================================="

# Limpiar caché de pacman (5.2GB)
echo "🗑️  Limpiando caché de pacman..."
sudo pacman -Scc --noconfirm
echo "✅ Caché de pacman limpiado"

# Limpiar paquetes huérfanos
echo "🗑️  Eliminando paquetes huérfanos..."
sudo pacman -Rns $(pacman -Qtdq) --noconfirm 2>/dev/null || echo "No hay paquetes huérfanos"
echo "✅ Paquetes huérfanos eliminados"

# Limpiar logs antiguos
echo "🗑️  Limpiando logs del sistema..."
sudo journalctl --vacuum-time=7d
sudo journalctl --vacuum-size=100M
echo "✅ Logs del sistema limpiados"

# Limpiar caché de usuario
echo "🗑️  Limpiando caché de usuario..."
rm -rf ~/.cache/thumbnails/*
rm -rf ~/.cache/mozilla/*
rm -rf ~/.cache/chromium/*
find ~/.cache -type f -atime +30 -delete 2>/dev/null
echo "✅ Caché de usuario limpiado"

# Limpiar archivos temporales
echo "🗑️  Limpiando archivos temporales..."
sudo rm -rf /tmp/*
sudo rm -rf /var/tmp/*
echo "✅ Archivos temporales limpiados"

echo "📊 Espacio después de limpieza:"
mostrar_espacio

# 2. OPTIMIZACIÓN DE SERVICIOS
echo ""
echo "⚙️  FASE 2: OPTIMIZACIÓN DE SERVICIOS"
echo "===================================="

# Deshabilitar servicios innecesarios
echo "🔧 Deshabilitando servicios innecesarios..."

# Lista de servicios comúnmente innecesarios en desktop
servicios_deshabilitar=(
    "bluetooth.service"
    "cups.service"
    "avahi-daemon.service"
    "ModemManager.service"
    "accounts-daemon.service"
)

for servicio in "${servicios_deshabilitar[@]}"; do
    if systemctl is-enabled "$servicio" &>/dev/null; then
        echo "  🔴 Deshabilitando $servicio"
        sudo systemctl disable "$servicio"
        sudo systemctl stop "$servicio"
    else
        echo "  ⚪ $servicio ya está deshabilitado"
    fi
done

echo "✅ Servicios optimizados"

# 3. OPTIMIZACIÓN DEL KERNEL
echo ""
echo "🔧 FASE 3: OPTIMIZACIÓN DEL KERNEL"
echo "================================="

# Crear archivo de configuración del kernel
echo "📝 Configurando parámetros del kernel..."
sudo tee /etc/sysctl.d/99-performance.conf > /dev/null << 'EOF'
# Optimizaciones de rendimiento para Manjaro XFCE

# Gestión de memoria
vm.swappiness=10
vm.vfs_cache_pressure=50
vm.dirty_background_ratio=5
vm.dirty_ratio=10

# Red
net.core.rmem_default=262144
net.core.rmem_max=16777216
net.core.wmem_default=262144
net.core.wmem_max=16777216

# Sistema de archivos
fs.file-max=2097152

# Kernel
kernel.sched_migration_cost_ns=5000000
kernel.sched_autogroup_enabled=0
EOF

echo "✅ Parámetros del kernel configurados"

# 4. OPTIMIZACIÓN DE XFCE
echo ""
echo "🎨 FASE 4: OPTIMIZACIÓN DE XFCE"
echo "==============================="

echo "🔧 Configurando XFCE para alto rendimiento..."

# Deshabilitar compositor si está activo
xfconf-query -c xfwm4 -p /general/use_compositing -s false 2>/dev/null || echo "Compositor ya deshabilitado"

# Optimizar configuraciones del escritorio
xfconf-query -c xfce4-desktop -p /backdrop/screen0/monitor0/workspace0/last-image -s "" 2>/dev/null
xfconf-query -c xfce4-desktop -p /backdrop/screen0/monitor0/workspace0/image-style -s 0 2>/dev/null

# Configurar panel para mejor rendimiento
xfconf-query -c xfce4-panel -p /panels/panel-1/autohide-behavior -s 0 2>/dev/null

echo "✅ XFCE optimizado"

# 5. CONFIGURACIÓN DE CPU GOVERNOR
echo ""
echo "⚡ FASE 5: CONFIGURACIÓN DE RENDIMIENTO DE CPU"
echo "=============================================="

echo "🔧 Configurando CPU governor para rendimiento..."

# Instalar cpupower si no está instalado
if ! command -v cpupower &> /dev/null; then
    echo "📦 Instalando cpupower..."
    sudo pacman -S --noconfirm linux-tools
fi

# Configurar governor a performance
sudo cpupower frequency-set -g performance 2>/dev/null || echo "Governor ya configurado"

# Crear servicio para mantener configuración
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

echo "✅ CPU configurada para alto rendimiento"

echo ""
echo "🎉 OPTIMIZACIÓN COMPLETADA!"
echo "=========================="
echo "📊 Estado final:"
mostrar_espacio

echo ""
echo "🔄 REINICIA EL SISTEMA para aplicar todos los cambios"
echo "💡 Mejoras aplicadas:"
echo "   ✅ Espacio en disco liberado"
echo "   ✅ Servicios innecesarios deshabilitados"
echo "   ✅ Kernel optimizado"
echo "   ✅ XFCE configurado para rendimiento"
echo "   ✅ CPU configurada para máximo rendimiento"
