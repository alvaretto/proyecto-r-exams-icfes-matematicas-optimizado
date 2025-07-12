# 🚀 GUÍA COMPLETA DE OPTIMIZACIÓN MANJARO PLASMA KDE

## 📊 ANÁLISIS INICIAL
- **Disco usado**: 83% (CRÍTICO)
- **RAM**: 19GB (Excelente)
- **Problemas detectados**:
  - Caché de pacman: 5.2GB
  - Múltiples procesos KDE/Plasma
  - Servicios KDE innecesarios activos
  - Baloo (indexador) sin límites
  - Efectos visuales sin optimizar

---

## 🔥 FASE 1: LIMPIEZA CRÍTICA DE ESPACIO (EJECUTAR INMEDIATAMENTE)

### 1.1 Limpiar Caché de Pacman (Liberará ~5.2GB)
```bash
sudo pacman -Scc
```
**Confirma con 'Y' cuando pregunte**

### 1.2 Eliminar Paquetes Huérfanos
```bash
sudo pacman -Rns $(pacman -Qtdq)
```

### 1.3 Limpiar Logs del Sistema
```bash
sudo journalctl --vacuum-time=7d
sudo journalctl --vacuum-size=100M
```

### 1.4 Limpiar Caché de Usuario y KDE
```bash
rm -rf ~/.cache/thumbnails/*
rm -rf ~/.cache/mozilla/*
rm -rf ~/.cache/chromium/*
rm -rf ~/.cache/plasma*
rm -rf ~/.cache/kio*
rm -rf ~/.cache/baloo*
find ~/.cache -type f -atime +30 -delete
```

### 1.5 Limpiar Caché de Plasma
```bash
rm -rf ~/.cache/plasma-svgelements*
rm -rf ~/.cache/plasma_theme*
rm -rf ~/.cache/ksvg-elements*
```

### 1.6 Verificar Espacio Liberado
```bash
df -h /
```

---

## ⚙️ FASE 2: OPTIMIZACIÓN DE SERVICIOS KDE

### 2.1 Ver Servicios Activos
```bash
systemctl list-units --type=service --state=running
```

### 2.2 Deshabilitar Servicios Innecesarios
```bash
# Bluetooth (si no lo usas)
sudo systemctl disable bluetooth.service
sudo systemctl stop bluetooth.service

# Impresoras (si no las usas)
sudo systemctl disable cups.service
sudo systemctl stop cups.service

# Avahi (descubrimiento de red)
sudo systemctl disable avahi-daemon.service
sudo systemctl stop avahi-daemon.service

# ModemManager (si no usas módem)
sudo systemctl disable ModemManager.service
sudo systemctl stop ModemManager.service

# KDE Connect (si no lo usas)
systemctl --user disable kdeconnectd.service
systemctl --user stop kdeconnectd.service
```

---

## 🎨 FASE 3: OPTIMIZACIÓN ESPECÍFICA DE PLASMA

### 3.1 Configurar Baloo (Indexador de KDE)
```bash
# Configurar Baloo para mejor rendimiento
balooctl disable
balooctl purge
balooctl enable

# Configurar archivos a indexar
kwriteconfig5 --file baloofilerc --group "General" --key "folders[$e]" "$HOME/Documents/,$HOME/Pictures/"
kwriteconfig5 --file baloofilerc --group "General" --key "exclude filters" "*.tmp,*.temp,*.o,*.obj,*.so,*.a"
```

### 3.2 Optimizar Efectos de Escritorio
```bash
# Deshabilitar efectos pesados
kwriteconfig5 --file kwinrc --group Plugins --key blurEnabled false
kwriteconfig5 --file kwinrc --group Plugins --key slideEnabled false
kwriteconfig5 --file kwinrc --group Plugins --key fadeEnabled false
kwriteconfig5 --file kwinrc --group Plugins --key minimizeanimationEnabled false

# Configurar compositor para rendimiento
kwriteconfig5 --file kwinrc --group Compositing --key Backend OpenGL
kwriteconfig5 --file kwinrc --group Compositing --key GLCore true
kwriteconfig5 --file kwinrc --group Compositing --key HiddenPreviews 5
kwriteconfig5 --file kwinrc --group Compositing --key MaxFPS 60
```

### 3.3 Optimizar Configuración de Plasma
```bash
# Reducir animaciones
kwriteconfig5 --file plasmarc --group Units --key duration 1
kwriteconfig5 --file plasmarc --group Units --key longDuration 2

# Optimizar panel
kwriteconfig5 --file plasmashellrc --group PlasmaViews --group Panel --key floating 0
```

---

## 🔧 FASE 4: OPTIMIZACIÓN DEL KERNEL

### 4.1 Crear Archivo de Configuración
```bash
sudo nano /etc/sysctl.d/99-performance.conf
```

**Agregar este contenido:**
```
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
```

### 4.2 Aplicar Configuración
```bash
sudo sysctl --system
```

---

## ⚡ FASE 5: OPTIMIZACIÓN DE CPU

### 5.1 Instalar cpupower (si no está instalado)
```bash
sudo pacman -S cpupower
```

### 5.2 Configurar Governor de CPU
```bash
# Ver governors disponibles
cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_available_governors

# Configurar performance
sudo cpupower frequency-set -g performance

# Hacer permanente
sudo systemctl enable cpupower.service
echo 'governor="performance"' | sudo tee /etc/default/cpupower
```

---

## 🚀 FASE 6: OPTIMIZACIÓN DE APLICACIONES

### 6.1 Optimizar Firefox para KDE
```bash
# Crear perfil de rendimiento para Firefox
mkdir -p ~/.mozilla/firefox/profiles
echo 'user_pref("layers.acceleration.force-enabled", true);
user_pref("gfx.webrender.all", true);
user_pref("media.ffmpeg.vaapi.enabled", true);' > ~/.mozilla/firefox/user.js
```

### 6.2 Optimizar Configuración Qt
```bash
# Variables de entorno para Qt
echo 'export QT_QPA_PLATFORMTHEME=kde
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export QT_SCALE_FACTOR=1
export QT_FONT_DPI=96' >> ~/.bashrc
```

---

## 📊 FASE 7: MONITOREO Y VERIFICACIÓN

### 7.1 Verificar Servicios Activos
```bash
systemctl list-units --type=service --state=running | wc -l
```

### 7.2 Verificar Uso de Memoria
```bash
free -h
```

### 7.3 Verificar Espacio en Disco
```bash
df -h /
```

### 7.4 Verificar Governor de CPU
```bash
cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor
```

---

## 🔄 FASE 8: REINICIO Y CONFIGURACIÓN FINAL

### 8.1 Reiniciar Plasma (sin reiniciar sistema)
```bash
kquitapp5 plasmashell && kstart5 plasmashell
```

### 8.2 Reiniciar KWin
```bash
kwin_x11 --replace &
```

### 8.3 Reinicio Completo (Recomendado)
```bash
sudo reboot
```

---

## 📈 RESULTADOS ESPERADOS

### **ANTES:**
- 🔴 Servicios: 35+ activos
- 🔴 Baloo: Indexando todo sin límites
- 🔴 Efectos: Todos activos
- 🔴 Swappiness: 60
- 🔴 CPU Governor: ondemand

### **DESPUÉS:**
- 🟢 Servicios: ~20-25 activos
- 🟢 Baloo: Configurado eficientemente
- 🟢 Efectos: Optimizados para rendimiento
- 🟢 Swappiness: 10
- 🟢 CPU Governor: performance

---

## ⚠️ NOTAS IMPORTANTES

### **ESPECÍFICO PARA PLASMA:**
- Los efectos visuales se pueden reactivar individualmente desde Configuración del Sistema
- Baloo se puede reconfigurar desde Configuración del Sistema > Búsqueda
- Los cambios en KWin requieren reinicio del compositor

### **REVERSIÓN:**
- Para revertir efectos: Configuración del Sistema > Efectos de Escritorio
- Para revertir Baloo: `balooctl enable` y reconfigurar
- Para revertir servicios: `sudo systemctl enable [servicio]`

### **MANTENIMIENTO:**
- Ejecutar limpieza de caché mensualmente
- Revisar servicios activos trimestralmente
- Actualizar sistema regularmente: `sudo pacman -Syu`

---

**🎉 ¡Disfruta de tu Manjaro Plasma optimizado!**
