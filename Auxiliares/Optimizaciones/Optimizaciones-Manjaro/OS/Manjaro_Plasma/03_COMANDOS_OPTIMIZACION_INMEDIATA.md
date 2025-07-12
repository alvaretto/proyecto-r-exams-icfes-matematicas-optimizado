# 🔧 COMANDOS DE OPTIMIZACIÓN INMEDIATA - MANJARO PLASMA KDE

## 📋 INSTRUCCIONES
- Copia y pega cada comando en tu terminal
- Ejecuta los comandos en el orden indicado
- Espera a que cada comando termine antes de ejecutar el siguiente
- Algunos comandos requieren confirmación (presiona 'Y' cuando se solicite)

---

## 🧹 FASE 1: LIMPIEZA CRÍTICA DE ESPACIO

### 1.1 Limpiar Caché de Pacman (Liberará ~5GB)
```bash
sudo pacman -Scc
```
**⚠️ Confirma con 'Y' cuando pregunte**

### 1.2 Eliminar Paquetes Huérfanos
```bash
sudo pacman -Rns $(pacman -Qtdq)
```

### 1.3 Limpiar Logs del Sistema
```bash
sudo journalctl --vacuum-time=7d
sudo journalctl --vacuum-size=100M
```

### 1.4 Limpiar Caché de Usuario General
```bash
rm -rf ~/.cache/thumbnails/*
rm -rf ~/.cache/mozilla/*
rm -rf ~/.cache/chromium/*
find ~/.cache -type f -atime +30 -delete
```

### 1.5 Limpiar Caché Específico de Plasma
```bash
rm -rf ~/.cache/plasma*
rm -rf ~/.cache/kio*
rm -rf ~/.cache/baloo*
rm -rf ~/.cache/ksvg-elements*
rm -rf ~/.cache/plasma-svgelements*
rm -rf ~/.cache/plasma_theme*
```

### 1.6 Verificar Espacio Liberado
```bash
df -h /
```

---

## ⚙️ FASE 2: OPTIMIZACIÓN DE SERVICIOS

### 2.1 Ver Servicios Activos Actuales
```bash
systemctl list-units --type=service --state=running | wc -l
```

### 2.2 Deshabilitar Bluetooth (si no lo usas)
```bash
sudo systemctl disable bluetooth.service
sudo systemctl stop bluetooth.service
```

### 2.3 Deshabilitar Impresoras (si no las usas)
```bash
sudo systemctl disable cups.service
sudo systemctl stop cups.service
```

### 2.4 Deshabilitar Avahi (descubrimiento de red)
```bash
sudo systemctl disable avahi-daemon.service
sudo systemctl stop avahi-daemon.service
```

### 2.5 Deshabilitar ModemManager (si no usas módem)
```bash
sudo systemctl disable ModemManager.service
sudo systemctl stop ModemManager.service
```

### 2.6 Optimizar Servicios de Usuario KDE (opcional)
```bash
systemctl --user disable kdeconnectd.service
systemctl --user stop kdeconnectd.service
```

---

## 🎨 FASE 3: OPTIMIZACIÓN ESPECÍFICA DE PLASMA

### 3.1 Configurar Baloo (Indexador) para Mejor Rendimiento
```bash
# Deshabilitar temporalmente
balooctl disable
balooctl purge

# Reactivar con configuración optimizada
balooctl enable
```

### 3.2 Configurar Carpetas a Indexar (Solo Documentos e Imágenes)
```bash
kwriteconfig5 --file baloofilerc --group "General" --key "folders[\$e]" "\$HOME/Documents/,\$HOME/Pictures/"
```

### 3.3 Configurar Filtros de Exclusión para Baloo
```bash
kwriteconfig5 --file baloofilerc --group "General" --key "exclude filters" "*.tmp,*.temp,*.o,*.obj,*.so,*.a,*.log,*.cache"
```

### 3.4 Deshabilitar Efectos Pesados de KWin
```bash
kwriteconfig5 --file kwinrc --group Plugins --key blurEnabled false
kwriteconfig5 --file kwinrc --group Plugins --key slideEnabled false
kwriteconfig5 --file kwinrc --group Plugins --key fadeEnabled false
kwriteconfig5 --file kwinrc --group Plugins --key minimizeanimationEnabled false
```

### 3.5 Optimizar Compositor para Rendimiento
```bash
kwriteconfig5 --file kwinrc --group Compositing --key Backend OpenGL
kwriteconfig5 --file kwinrc --group Compositing --key GLCore true
kwriteconfig5 --file kwinrc --group Compositing --key HiddenPreviews 5
kwriteconfig5 --file kwinrc --group Compositing --key MaxFPS 60
```

### 3.6 Reducir Animaciones de Plasma
```bash
kwriteconfig5 --file plasmarc --group Units --key duration 1
kwriteconfig5 --file plasmarc --group Units --key longDuration 2
```

### 3.7 Optimizar Panel de Plasma
```bash
kwriteconfig5 --file plasmashellrc --group PlasmaViews --group Panel --key floating 0
```

---

## 🔧 FASE 4: OPTIMIZACIÓN DEL KERNEL

### 4.1 Crear Archivo de Configuración de Rendimiento
```bash
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
```

### 4.2 Aplicar Configuración del Kernel
```bash
sudo sysctl --system
```

---

## ⚡ FASE 5: OPTIMIZACIÓN DE CPU

### 5.1 Instalar cpupower (si no está instalado)
```bash
sudo pacman -S cpupower --needed
```

### 5.2 Ver Governors Disponibles
```bash
cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_available_governors
```

### 5.3 Configurar Governor de Performance
```bash
sudo cpupower frequency-set -g performance
```

### 5.4 Hacer Configuración Permanente
```bash
sudo systemctl enable cpupower.service
echo 'governor="performance"' | sudo tee /etc/default/cpupower
```

---

## 🚀 FASE 6: OPTIMIZACIÓN DE APLICACIONES

### 6.1 Configurar Variables de Entorno Qt
```bash
echo 'export QT_QPA_PLATFORMTHEME=kde
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export QT_SCALE_FACTOR=1
export QT_FONT_DPI=96' >> ~/.bashrc
```

### 6.2 Optimizar Firefox para KDE (si lo usas)
```bash
mkdir -p ~/.mozilla/firefox/
echo 'user_pref("layers.acceleration.force-enabled", true);
user_pref("gfx.webrender.all", true);
user_pref("media.ffmpeg.vaapi.enabled", true);
user_pref("widget.use-xdg-desktop-portal.file-picker", 1);' > ~/.mozilla/firefox/user.js
```

---

## 🔄 FASE 7: APLICAR CAMBIOS

### 7.1 Reiniciar Plasma Shell (sin reiniciar sistema)
```bash
kquitapp5 plasmashell && kstart5 plasmashell &
```

### 7.2 Reiniciar KWin (Compositor)
```bash
kwin_x11 --replace &
```

### 7.3 Recargar Configuración de Baloo
```bash
balooctl restart
```

---

## 📊 FASE 8: VERIFICACIÓN

### 8.1 Verificar Servicios Activos
```bash
echo "Servicios activos: $(systemctl list-units --type=service --state=running | wc -l)"
```

### 8.2 Verificar Swappiness
```bash
echo "Swappiness: $(cat /proc/sys/vm/swappiness)"
```

### 8.3 Verificar CPU Governor
```bash
echo "CPU Governor: $(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor)"
```

### 8.4 Verificar Espacio en Disco
```bash
df -h /
```

### 8.5 Verificar Estado de Baloo
```bash
balooctl status
```

---

## 🎯 REINICIO FINAL

### 8.6 Reiniciar Sistema (Recomendado)
```bash
sudo reboot
```

---

## 📋 COMANDOS DE VERIFICACIÓN POST-REINICIO

### Ejecutar después del reinicio para verificar optimizaciones:

```bash
# Ver estado general
./02_monitor_rendimiento.sh

# Verificar servicios
systemctl list-units --type=service --state=running | wc -l

# Verificar configuraciones
cat /proc/sys/vm/swappiness
cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor
balooctl status
```

---

## ⚠️ NOTAS IMPORTANTES

### **REVERSIÓN DE CAMBIOS:**
- **Servicios**: `sudo systemctl enable [servicio]`
- **Efectos**: Configuración del Sistema > Efectos de Escritorio
- **Baloo**: `balooctl enable` y reconfigurar en Configuración del Sistema
- **CPU Governor**: `sudo cpupower frequency-set -g ondemand`

### **PROBLEMAS COMUNES:**
- Si Plasma no responde: `Alt+F2` → `kquitapp5 plasmashell && kstart5 plasmashell`
- Si KWin falla: `Alt+F2` → `kwin_x11 --replace`
- Si hay problemas con Qt: Reiniciar sesión

### **MANTENIMIENTO:**
- Ejecutar limpieza de caché mensualmente
- Revisar servicios cada 3 meses
- Actualizar sistema: `sudo pacman -Syu`

---

**🎉 ¡Optimización completada! Tu Manjaro Plasma debería funcionar más rápido y eficientemente.**
