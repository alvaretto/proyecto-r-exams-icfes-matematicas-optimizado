#!/bin/bash

# 📊 MONITOR DE RENDIMIENTO MANJARO PLASMA KDE
# Muestra el estado del sistema antes y después de optimizaciones

echo "📊 MONITOR DE RENDIMIENTO DEL SISTEMA PLASMA"
echo "============================================="
echo "Fecha: $(date)"
echo ""

# Función para mostrar separador
separador() {
    echo "-------------------------------------"
}

# 1. INFORMACIÓN DEL SISTEMA
echo "🖥️  INFORMACIÓN DEL SISTEMA"
separador
echo "OS: $(cat /etc/os-release | grep PRETTY_NAME | cut -d'"' -f2)"
echo "Kernel: $(uname -r)"
echo "Uptime: $(uptime -p)"
echo "Desktop: $(echo $XDG_CURRENT_DESKTOP)"
echo "Sesión: $(echo $XDG_SESSION_TYPE)"
echo ""

# 2. USO DE DISCO
echo "💾 USO DE DISCO"
separador
df -h / | awk 'NR==1{print $0} NR==2{
    used_percent = $5
    gsub(/%/, "", used_percent)
    if (used_percent >= 90) color = "🔴"
    else if (used_percent >= 80) color = "🟡"
    else color = "🟢"
    print color " " $0
}'
echo ""

# 3. USO DE MEMORIA
echo "🧠 USO DE MEMORIA"
separador
free -h | awk '
NR==1{print $0}
NR==2{
    total = $2
    used = $3
    available = $7
    used_percent = (used / total) * 100
    if (used_percent >= 90) color = "🔴"
    else if (used_percent >= 70) color = "🟡"
    else color = "🟢"
    printf "%s Mem: %s usado de %s (%.1f%%) - %s disponible\n", color, used, total, used_percent, available
}'
echo ""

# 4. PROCESOS QUE MÁS CONSUMEN CPU
echo "⚡ TOP 5 PROCESOS POR CPU"
separador
ps aux --sort=-%cpu | head -6 | awk '
NR==1{print $0}
NR>1{
    cpu = $3
    if (cpu >= 10) color = "🔴"
    else if (cpu >= 5) color = "🟡"
    else color = "🟢"
    printf "%s %s\n", color, $0
}'
echo ""

# 5. PROCESOS QUE MÁS CONSUMEN MEMORIA
echo "🧠 TOP 5 PROCESOS POR MEMORIA"
separador
ps aux --sort=-%mem | head -6 | awk '
NR==1{print $0}
NR>1{
    mem = $4
    if (mem >= 10) color = "🔴"
    else if (mem >= 5) color = "🟡"
    else color = "🟢"
    printf "%s %s\n", color, $0
}'
echo ""

# 6. PROCESOS ESPECÍFICOS DE KDE/PLASMA
echo "🎨 PROCESOS DE PLASMA/KDE"
separador
echo "Procesos KDE activos:"
ps aux | grep -E "(plasma|kwin|baloo|kded|krunner)" | grep -v grep | awk '{
    cpu = $3
    mem = $4
    if (cpu >= 5 || mem >= 5) color = "🟡"
    else color = "🟢"
    printf "%s %s %s %s %s\n", color, $11, $3"%", $4"%", $2
}' | sort -k3 -nr
echo ""

# 7. SERVICIOS DEL SISTEMA
echo "⚙️  SERVICIOS ACTIVOS"
separador
total_services=$(systemctl list-units --type=service --state=running | grep -c "\.service")
if [ $total_services -ge 35 ]; then
    color="🔴"
elif [ $total_services -ge 25 ]; then
    color="🟡"
else
    color="🟢"
fi
echo "$color Total de servicios activos: $total_services"

echo ""
echo "Servicios KDE específicos:"
systemctl --user list-units --type=service --state=running | grep -E "(kde|plasma|baloo)" | awk '{print "🎨 " $1 " - " $4}'
echo ""

# 8. CONFIGURACIÓN DEL SISTEMA
echo "🔧 CONFIGURACIÓN DEL SISTEMA"
separador

# Swappiness
swappiness=$(cat /proc/sys/vm/swappiness)
if [ $swappiness -le 10 ]; then
    color="🟢"
elif [ $swappiness -le 30 ]; then
    color="🟡"
else
    color="🔴"
fi
echo "$color Swappiness: $swappiness"

# CPU Governor
if [ -f /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor ]; then
    governor=$(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor)
    if [ "$governor" = "performance" ]; then
        color="🟢"
    elif [ "$governor" = "ondemand" ]; then
        color="🟡"
    else
        color="🔴"
    fi
    echo "$color CPU Governor: $governor"
else
    echo "🟡 CPU Governor: No disponible"
fi

# Estado de Baloo
if command -v balooctl &> /dev/null; then
    baloo_status=$(balooctl status | grep "Baloo File Indexer is" | awk '{print $5}')
    if [ "$baloo_status" = "running" ]; then
        color="🟡"
        echo "$color Baloo (Indexador): Activo"
    else
        color="🟢"
        echo "$color Baloo (Indexador): Inactivo/Optimizado"
    fi
else
    echo "🟡 Baloo: No instalado"
fi

echo ""

# 9. ESPACIO EN CACHÉ
echo "🗂️  ESPACIO EN CACHÉ"
separador

# Caché de pacman
if [ -d /var/cache/pacman/pkg ]; then
    pacman_cache=$(du -sh /var/cache/pacman/pkg 2>/dev/null | cut -f1)
    echo "📦 Caché de Pacman: $pacman_cache"
fi

# Caché de usuario
if [ -d ~/.cache ]; then
    user_cache=$(du -sh ~/.cache 2>/dev/null | cut -f1)
    echo "👤 Caché de Usuario: $user_cache"
fi

# Caché específico de Plasma
plasma_cache_size=0
for dir in ~/.cache/plasma* ~/.cache/kio* ~/.cache/baloo* ~/.cache/ksvg*; do
    if [ -d "$dir" ]; then
        size=$(du -s "$dir" 2>/dev/null | cut -f1)
        plasma_cache_size=$((plasma_cache_size + size))
    fi
done
if [ $plasma_cache_size -gt 0 ]; then
    plasma_cache_human=$(echo $plasma_cache_size | awk '{printf "%.1fMB", $1/1024}')
    echo "🎨 Caché de Plasma: $plasma_cache_human"
fi

echo ""

# 10. TEMPERATURA (si está disponible)
echo "🌡️  TEMPERATURA"
separador
if command -v sensors &> /dev/null; then
    sensors | grep -E "(Core|temp)" | head -3
else
    echo "⚠️  Comando 'sensors' no disponible (instalar: sudo pacman -S lm_sensors)"
fi
echo ""

# 11. EFECTOS DE ESCRITORIO
echo "✨ EFECTOS DE ESCRITORIO"
separador
if command -v kreadconfig5 &> /dev/null; then
    compositing=$(kreadconfig5 --file kwinrc --group Compositing --key Enabled --default true)
    if [ "$compositing" = "true" ]; then
        echo "🎨 Compositor: Activo"
        
        # Verificar efectos específicos
        blur=$(kreadconfig5 --file kwinrc --group Plugins --key blurEnabled --default true)
        slide=$(kreadconfig5 --file kwinrc --group Plugins --key slideEnabled --default true)
        fade=$(kreadconfig5 --file kwinrc --group Plugins --key fadeEnabled --default true)
        
        effects_count=0
        [ "$blur" = "true" ] && effects_count=$((effects_count + 1)) && echo "  🔹 Blur: Activo"
        [ "$slide" = "true" ] && effects_count=$((effects_count + 1)) && echo "  🔹 Slide: Activo"
        [ "$fade" = "true" ] && effects_count=$((effects_count + 1)) && echo "  🔹 Fade: Activo"
        
        if [ $effects_count -gt 2 ]; then
            echo "🟡 Muchos efectos activos (considerar optimizar)"
        elif [ $effects_count -eq 0 ]; then
            echo "🟢 Efectos optimizados para rendimiento"
        else
            echo "🟡 Efectos moderados"
        fi
    else
        echo "🔴 Compositor: Desactivado"
    fi
else
    echo "⚠️  No se puede verificar estado de efectos"
fi

echo ""

# 12. RESUMEN FINAL
echo "📋 RESUMEN DE OPTIMIZACIÓN"
separador

score=0
total_checks=6

# Disco
disk_usage=$(df / | awk 'NR==2{print $5}' | sed 's/%//')
if [ $disk_usage -lt 80 ]; then score=$((score + 1)); fi

# Servicios
if [ $total_services -lt 30 ]; then score=$((score + 1)); fi

# Swappiness
if [ $swappiness -le 15 ]; then score=$((score + 1)); fi

# CPU Governor
if [ "$governor" = "performance" ]; then score=$((score + 1)); fi

# Baloo
if [ "$baloo_status" != "running" ]; then score=$((score + 1)); fi

# Memoria
mem_usage=$(free | awk 'NR==2{printf "%.0f", $3*100/$2}')
if [ $mem_usage -lt 70 ]; then score=$((score + 1)); fi

percentage=$((score * 100 / total_checks))

if [ $percentage -ge 80 ]; then
    echo "🟢 Sistema OPTIMIZADO ($score/$total_checks checks) - $percentage%"
elif [ $percentage -ge 60 ]; then
    echo "🟡 Sistema PARCIALMENTE OPTIMIZADO ($score/$total_checks checks) - $percentage%"
else
    echo "🔴 Sistema NECESITA OPTIMIZACIÓN ($score/$total_checks checks) - $percentage%"
fi

echo ""
echo "💡 Ejecuta los scripts de optimización para mejorar el rendimiento"
echo "📊 Vuelve a ejecutar este monitor después de optimizar para ver mejoras"
