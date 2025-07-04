#!/bin/bash

# 📊 MONITOR DE RENDIMIENTO MANJARO XFCE
# Muestra el estado del sistema antes y después de optimizaciones

echo "📊 MONITOR DE RENDIMIENTO DEL SISTEMA"
echo "====================================="
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

# 6. SERVICIOS ACTIVOS
echo "⚙️  SERVICIOS DEL SISTEMA"
separador
services_count=$(systemctl list-units --type=service --state=running | grep -c "\.service")
if [ $services_count -gt 30 ]; then
    color="🔴"
elif [ $services_count -gt 25 ]; then
    color="🟡"
else
    color="🟢"
fi
echo "$color Servicios activos: $services_count"
echo ""

# 7. TEMPERATURA (si está disponible)
echo "🌡️  TEMPERATURA"
separador
if command -v sensors &> /dev/null; then
    sensors | grep -E "(Core|temp)" | head -3
else
    echo "⚠️  Instala lm-sensors para ver temperaturas: sudo pacman -S lm-sensors"
fi
echo ""

# 8. CARGA DEL SISTEMA
echo "📈 CARGA DEL SISTEMA"
separador
load_avg=$(uptime | awk -F'load average:' '{print $2}' | sed 's/^ *//')
echo "Load Average: $load_avg"
echo ""

# 9. ESPACIO EN DIRECTORIOS CRÍTICOS
echo "📁 ESPACIO EN DIRECTORIOS CRÍTICOS"
separador
echo "Caché de pacman: $(du -sh /var/cache/pacman/pkg/ 2>/dev/null | cut -f1)"
echo "Logs del sistema: $(sudo du -sh /var/log/ 2>/dev/null | cut -f1)"
echo "Caché de usuario: $(du -sh ~/.cache/ 2>/dev/null | cut -f1)"
echo ""

# 10. CONFIGURACIONES DE RENDIMIENTO
echo "🔧 CONFIGURACIONES DE RENDIMIENTO"
separador

# Verificar swappiness
swappiness=$(cat /proc/sys/vm/swappiness 2>/dev/null)
if [ "$swappiness" -le 10 ]; then
    echo "🟢 Swappiness: $swappiness (Optimizado)"
else
    echo "🟡 Swappiness: $swappiness (Recomendado: ≤10)"
fi

# Verificar CPU governor
if command -v cpupower &> /dev/null; then
    governor=$(cpupower frequency-info -p 2>/dev/null | grep "governor" | awk '{print $4}' | tr -d '"')
    if [ "$governor" = "performance" ]; then
        echo "🟢 CPU Governor: $governor (Optimizado)"
    else
        echo "🟡 CPU Governor: $governor (Recomendado: performance)"
    fi
else
    echo "⚠️  Instala linux-tools para ver CPU governor"
fi

# Verificar compositor XFCE
compositor=$(xfconf-query -c xfwm4 -p /general/use_compositing 2>/dev/null)
if [ "$compositor" = "false" ]; then
    echo "🟢 Compositor XFCE: Deshabilitado (Optimizado)"
else
    echo "🟡 Compositor XFCE: Habilitado (Recomendado: deshabilitar)"
fi

echo ""

# 11. RECOMENDACIONES
echo "💡 RECOMENDACIONES"
separador

# Verificar uso de disco
disk_usage=$(df / | awk 'NR==2{print $5}' | sed 's/%//')
if [ $disk_usage -gt 85 ]; then
    echo "🔴 Disco muy lleno ($disk_usage%). Ejecuta limpieza urgente."
fi

# Verificar memoria
mem_usage=$(free | awk 'NR==2{printf "%.0f", $3/$2*100}')
if [ $mem_usage -gt 80 ]; then
    echo "🔴 Memoria alta ($mem_usage%). Cierra aplicaciones innecesarias."
fi

# Verificar servicios
if [ $services_count -gt 25 ]; then
    echo "🟡 Muchos servicios activos ($services_count). Considera deshabilitar algunos."
fi

echo ""
echo "🔄 Para actualizar: bash $0"
echo "📊 Para monitoreo continuo: watch -n 5 bash $0"
