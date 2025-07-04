# 🚀 COMANDOS DE OPTIMIZACIÓN INMEDIATA PARA MANJARO XFCE

## ⚠️ INSTRUCCIONES ACTUALIZADAS
**OPCIÓN 1 (RECOMENDADA): Ejecuta el script automático**
```bash
cd /home/pequeniomanjaro/Documentos/proyecto-r-exams-icfes-matematicas-optimizado/Auxiliares/Rexams-Lubuntu/Optimizaciones-Lubuntu/OS/
./06_EJECUTAR_EN_TERMINAL_PRINCIPAL.sh
```

**OPCIÓN 2: Copia y pega estos comandos UNO POR UNO en tu terminal principal (fuera de este entorno)**

---

## 🔥 FASE 1: LIMPIEZA CRÍTICA DE ESPACIO (EJECUTAR PRIMERO)

### 1.1 Verificar Estado Inicial
```bash
echo "📊 Estado inicial del disco:"
df -h /
echo ""
echo "📦 Tamaño del caché de pacman:"
du -sh /var/cache/pacman/pkg/
```

### 1.2 Limpiar Caché de Pacman (Liberará ~5.2GB)
```bash
echo "🧹 Limpiando caché de pacman..."
sudo pacman -Scc
```
**⚠️ Confirma con 'Y' cuando pregunte**

### 1.3 Eliminar Paquetes Huérfanos
```bash
echo "🗑️ Eliminando paquetes huérfanos..."
sudo pacman -Rns $(pacman -Qtdq) 2>/dev/null || echo "No hay paquetes huérfanos"
```

### 1.4 Limpiar Logs del Sistema
```bash
echo "📝 Limpiando logs del sistema..."
sudo journalctl --vacuum-time=7d
sudo journalctl --vacuum-size=100M
```

### 1.5 Limpiar Caché de Usuario
```bash
echo "🗂️ Limpiando caché de usuario..."
rm -rf ~/.cache/thumbnails/*
rm -rf ~/.cache/mozilla/*
rm -rf ~/.cache/chromium/*
find ~/.cache -type f -atime +30 -delete 2>/dev/null
echo "✅ Caché de usuario limpiado"
```

### 1.6 Verificar Espacio Liberado
```bash
echo "📊 Estado después de limpieza:"
df -h /
```

---

## ⚙️ FASE 2: OPTIMIZACIÓN DE SERVICIOS

### 2.1 Ver Servicios Activos Actuales
```bash
echo "📋 Servicios activos (antes):"
systemctl list-units --type=service --state=running | wc -l
```

### 2.2 Deshabilitar Servicios Innecesarios
```bash
# Bluetooth (si no lo usas)
echo "🔴 Deshabilitando Bluetooth..."
sudo systemctl disable bluetooth.service
sudo systemctl stop bluetooth.service

# Impresoras (si no las usas)
echo "🔴 Deshabilitando servicio de impresoras..."
sudo systemctl disable cups.service
sudo systemctl stop cups.service

# Avahi (descubrimiento de red local)
echo "🔴 Deshabilitando Avahi..."
sudo systemctl disable avahi-daemon.service
sudo systemctl stop avahi-daemon.service

# ModemManager (si no usas módem)
echo "🔴 Deshabilitando ModemManager..."
sudo systemctl disable ModemManager.service
sudo systemctl stop ModemManager.service
```

### 2.3 Verificar Servicios Después
```bash
echo "📋 Servicios activos (después):"
systemctl list-units --type=service --state=running | wc -l
```

---

## 🔧 FASE 3: OPTIMIZACIÓN DEL KERNEL

### 3.1 Crear Configuración de Rendimiento
```bash
echo "📝 Creando configuración de kernel optimizada..."
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
```

### 3.2 Aplicar Configuración
```bash
echo "⚡ Aplicando configuración del kernel..."
sudo sysctl -p /etc/sysctl.d/99-performance.conf
```

---

## 🎨 FASE 4: OPTIMIZACIÓN DE XFCE

### 4.1 Deshabilitar Compositor (Mejora rendimiento)
```bash
echo "🎨 Optimizando XFCE..."
xfconf-query -c xfwm4 -p /general/use_compositing -s false
echo "✅ Compositor deshabilitado"
```

### 4.2 Optimizar Configuraciones del Escritorio
```bash
# Quitar fondo de pantalla para ahorrar memoria
xfconf-query -c xfce4-desktop -p /backdrop/screen0/monitor0/workspace0/last-image -s ""
xfconf-query -c xfce4-desktop -p /backdrop/screen0/monitor0/workspace0/image-style -s 0
echo "✅ Escritorio optimizado"
```

---

## ⚡ FASE 5: OPTIMIZACIÓN DE CPU

### 5.1 Instalar Herramientas de CPU
```bash
echo "📦 Instalando herramientas de CPU..."
sudo pacman -S --noconfirm linux-tools
```

### 5.2 Configurar CPU para Máximo Rendimiento
```bash
echo "⚡ Configurando CPU para rendimiento..."
sudo cpupower frequency-set -g performance
```

### 5.3 Hacer Configuración Permanente
```bash
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
echo "✅ CPU configurada para alto rendimiento"
```

---

## 📱 FASE 6: OPTIMIZACIÓN DE APLICACIONES

### 6.1 Optimizar VSCode (Reducir Consumo de Recursos)
```bash
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
    "workbench.enableExperiments": false
}
EOF
echo "✅ VSCode optimizado"
```

---

## 🔍 FASE 7: VERIFICACIÓN FINAL

### 7.1 Instalar Herramientas de Monitoreo
```bash
echo "📊 Instalando herramientas de monitoreo..."
sudo pacman -S --noconfirm htop iotop
```

### 7.2 Verificar Resultados
```bash
echo "🎉 OPTIMIZACIÓN COMPLETADA!"
echo "=========================="
echo ""
echo "📊 Estado final del sistema:"
df -h /
echo ""
echo "⚙️ Servicios activos:"
systemctl list-units --type=service --state=running | wc -l
echo ""
echo "💾 Uso de memoria:"
free -h
echo ""
echo "🔄 REINICIA EL SISTEMA para aplicar todos los cambios"
```

---

## 🎯 RESULTADOS ESPERADOS

- **Espacio liberado**: 6-8GB
- **Servicios reducidos**: De 29 a ~15-20
- **Mejor responsividad**: Notoria mejora
- **Menor uso de CPU**: VSCode optimizado
- **Arranque más rápido**: 20-30% mejora

---

## 📊 VERIFICACIÓN POST-OPTIMIZACIÓN

### Verificar Estado del Sistema
```bash
# Ejecutar monitor de rendimiento
./02_monitor_rendimiento.sh

# Verificar configuraciones aplicadas
cat ~/.config/Code/User/settings.json
xfconf-query -c xfwm4 -p /general/use_compositing
```

### Verificar Mejoras Esperadas
- **Disco**: De 83% a ~65% de uso
- **Swappiness**: De 60 a 10
- **CPU Governor**: Configurado a "performance"
- **Servicios**: Reducidos de ~22 a ~15-18

---

## ⚠️ NOTAS IMPORTANTES ACTUALIZADAS

1. **RECOMENDADO**: Usa `./06_EJECUTAR_EN_TERMINAL_PRINCIPAL.sh` para optimización automática
2. **Ejecuta los comandos UNO POR UNO** en tu terminal principal si usas opción manual
3. **Reinicia** después de completar todas las fases
4. **Confirma** con 'Y' cuando pacman pregunte
5. **Guarda** este archivo para futuras referencias
6. **Verifica** mejoras con `./02_monitor_rendimiento.sh`

---

## 🆕 ARCHIVOS NUEVOS DISPONIBLES

- **`06_EJECUTAR_EN_TERMINAL_PRINCIPAL.sh`** - Script optimizado para terminal nativo
- **`02_monitor_rendimiento.sh`** - Monitor mejorado del sistema
- **`00_INDICE_OPTIMIZACION_MANJARO.md`** - Índice completo actualizado
