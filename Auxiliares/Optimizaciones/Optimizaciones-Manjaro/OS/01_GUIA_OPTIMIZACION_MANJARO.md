# 🚀 GUÍA COMPLETA DE OPTIMIZACIÓN MANJARO XFCE

## 📊 ANÁLISIS INICIAL
- **Disco usado**: 83% (CRÍTICO)
- **RAM**: 19GB (Excelente)
- **Problemas detectados**:
  - Caché de pacman: 5.2GB
  - Múltiples procesos VSCode
  - 9 snaps instalados
  - 29 servicios activos

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

### 1.4 Limpiar Caché de Usuario
```bash
rm -rf ~/.cache/thumbnails/*
rm -rf ~/.cache/mozilla/*
rm -rf ~/.cache/chromium/*
find ~/.cache -type f -atime +30 -delete
```

### 1.5 Verificar Espacio Liberado
```bash
df -h /
```

---

## ⚙️ FASE 2: OPTIMIZACIÓN DE SERVICIOS

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
```

---

## 🔧 FASE 3: OPTIMIZACIÓN DEL KERNEL

### 3.1 Crear Archivo de Configuración
```bash
sudo nano /etc/sysctl.d/99-performance.conf
```

**Agregar este contenido:**
```
# Optimizaciones de rendimiento
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
```

### 3.2 Aplicar Configuración
```bash
sudo sysctl -p /etc/sysctl.d/99-performance.conf
```

---

## 🎨 FASE 4: OPTIMIZACIÓN DE XFCE

### 4.1 Deshabilitar Compositor (Mejora rendimiento)
```bash
xfconf-query -c xfwm4 -p /general/use_compositing -s false
```

### 4.2 Optimizar Escritorio
```bash
# Quitar fondo de pantalla
xfconf-query -c xfce4-desktop -p /backdrop/screen0/monitor0/workspace0/last-image -s ""

# Configurar panel
xfconf-query -c xfce4-panel -p /panels/panel-1/autohide-behavior -s 0
```

---

## ⚡ FASE 5: OPTIMIZACIÓN DE CPU

### 5.1 Instalar Herramientas de CPU
```bash
sudo pacman -S linux-tools
```

### 5.2 Configurar Governor a Performance
```bash
sudo cpupower frequency-set -g performance
```

### 5.3 Hacer Permanente la Configuración
```bash
sudo nano /etc/systemd/system/cpu-performance.service
```

**Contenido del archivo:**
```
[Unit]
Description=Set CPU governor to performance
After=multi-user.target

[Service]
Type=oneshot
ExecStart=/usr/bin/cpupower frequency-set -g performance
RemainAfterExit=yes

[Install]
WantedBy=multi-user.target
```

**Habilitar el servicio:**
```bash
sudo systemctl enable cpu-performance.service
```

---

## 📱 FASE 6: OPTIMIZACIÓN DE APLICACIONES

### 6.1 Gestionar Snaps (Opcional)
```bash
# Ver snaps instalados
snap list

# Eliminar snaps innecesarios (ejemplo)
sudo snap remove gnome-3-28-1804
sudo snap remove gtk-common-themes
```

### 6.2 Optimizar VSCode
Agregar a `~/.config/Code/User/settings.json`:
```json
{
    "files.watcherExclude": {
        "**/.git/objects/**": true,
        "**/node_modules/**": true
    },
    "search.followSymlinks": false,
    "typescript.disableAutomaticTypeAcquisition": true
}
```

---

## 🔍 FASE 7: MONITOREO Y VERIFICACIÓN

### 7.1 Instalar Herramientas de Monitoreo
```bash
sudo pacman -S htop iotop nethogs
```

### 7.2 Verificar Mejoras
```bash
# Uso de CPU
htop

# Uso de disco
iotop

# Uso de red
nethogs

# Temperatura
sensors
```

---

## 🎯 RESULTADOS ESPERADOS

Después de aplicar todas las optimizaciones:

- **Espacio liberado**: 6-8GB
- **Servicios reducidos**: De 29 a ~15-20
- **Tiempo de arranque**: Reducido 20-30%
- **Responsividad**: Mejora notable
- **Uso de RAM**: Optimizado

---

## ⚠️ NOTAS IMPORTANTES

1. **Reinicia** después de cada fase para aplicar cambios
2. **Haz backup** antes de modificar archivos del sistema
3. **Prueba** cada optimización gradualmente
4. **Revierte** cambios si algo no funciona

---

## 🚀 EJECUCIÓN AUTOMÁTICA RECOMENDADA

### **OPCIÓN 1: Script Automático (RECOMENDADO)**
```bash
# Ejecutar en terminal principal de Manjaro
cd /home/pequeniomanjaro/Documentos/proyecto-r-exams-icfes-matematicas-optimizado/Auxiliares/Rexams-Lubuntu/Optimizaciones-Lubuntu/OS/
./06_EJECUTAR_EN_TERMINAL_PRINCIPAL.sh
```

### **OPCIÓN 2: Script Básico (si hay problemas con sudo)**
```bash
./04_EJECUTAR_OPTIMIZACION_COMPLETA.sh
# Luego ejecutar comandos manuales del archivo 03_COMANDOS_OPTIMIZACION_INMEDIATA.md
```

---

## 🔄 MANTENIMIENTO REGULAR

### Semanal:
```bash
sudo pacman -Syu
sudo pacman -Sc
```

### Mensual:
```bash
sudo journalctl --vacuum-time=30d
find ~/.cache -type f -atime +30 -delete
```

---

## ⚠️ NOTAS IMPORTANTES ACTUALIZADAS

### **NUEVOS ARCHIVOS DISPONIBLES:**
- **`06_EJECUTAR_EN_TERMINAL_PRINCIPAL.sh`** - Script optimizado para terminal nativo
- **`02_monitor_rendimiento.sh`** - Monitor mejorado del sistema

### **PROBLEMAS CONOCIDOS:**
- Algunos entornos pueden tener restricciones sudo
- Usar `06_EJECUTAR_EN_TERMINAL_PRINCIPAL.sh` para máxima compatibilidad
- El script `04_EJECUTAR_OPTIMIZACION_COMPLETA.sh` aplica optimizaciones básicas

### **VERIFICACIÓN POST-OPTIMIZACIÓN:**
```bash
# Verificar estado después de optimizar
./02_monitor_rendimiento.sh

# Verificar configuraciones aplicadas
cat ~/.config/Code/User/settings.json
xfconf-query -c xfwm4 -p /general/use_compositing
```
