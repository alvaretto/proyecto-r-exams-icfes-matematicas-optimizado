# Configuración Persistente de RDP en Manjaro Linux

Este documento explica cómo hacer que la configuración del cliente RDP en Manjaro Linux sea completamente persistente y sobreviva a los reinicios del sistema.

## 🎯 Objetivo

Hacer que todos los componentes necesarios para conectarse a servidores RDP remotos (especialmente a través de Tailscale) se inicien automáticamente y mantengan su configuración después de cada reinicio del sistema.

## 📋 Componentes que Necesitan ser Persistentes

### 1. **Servicio Tailscale (`tailscaled`)**
- **Problema**: Sin configuración persistente, Tailscale no se inicia automáticamente
- **Solución**: Habilitar el servicio con `systemctl enable`
- **Archivos modificados**:
  - `/etc/systemd/system/tailscaled.service.d/override.conf`
  - `/etc/systemd/system/tailscale-autoconnect.service`

### 2. **Cliente RDP (Remmina + FreeRDP)**
- **Problema**: Configuraciones por defecto no optimizadas
- **Solución**: Crear archivos de configuración personalizados
- **Archivos creados**:
  - `~/.config/remmina/remmina.pref`

### 3. **Configuración de Red y Firewall**
- **Problema**: Reglas de firewall pueden bloquear conexiones Tailscale
- **Solución**: Configurar reglas específicas para Tailscale
- **Comandos**: `ufw allow in/out on tailscale0`

## 🔧 Soluciones Implementadas

### 1. Configuración Persistente de Tailscale

#### A. Habilitación del Servicio Principal
```bash
sudo systemctl enable tailscaled
sudo systemctl start tailscaled
```
**Por qué funciona**: `systemctl enable` crea enlaces simbólicos en `/etc/systemd/system/multi-user.target.wants/` que hacen que el servicio se inicie automáticamente en cada boot.

#### B. Configuración de Reinicio Automático
**Archivo**: `/etc/systemd/system/tailscaled.service.d/override.conf`
```ini
[Service]
Restart=always
RestartSec=5
Environment="TS_ACCEPT_DNS=true"
Environment="TS_EXTRA_ARGS=--reset"
```
**Por qué funciona**: 

- `Restart=always`: Reinicia el servicio si falla
- `RestartSec=5`: Espera 5 segundos antes de reiniciar
- Variables de entorno persistentes para configuración

#### C. Servicio de Auto-Conexión
**Archivo**: `/etc/systemd/system/tailscale-autoconnect.service`
```ini
[Unit]
Description=Tailscale Auto-Connect
After=tailscaled.service
Wants=tailscaled.service

[Service]
Type=oneshot
ExecStart=/usr/local/bin/tailscale-autoconnect.sh
RemainAfterExit=yes

[Install]
WantedBy=multi-user.target
```
**Por qué funciona**: 

- Se ejecuta después de que `tailscaled` esté listo
- `WantedBy=multi-user.target` asegura que se ejecute en cada boot
- `RemainAfterExit=yes` mantiene el servicio como "activo" después de ejecutarse

### 2. Configuración Persistente de Remmina

#### Archivo de Configuración Optimizado
**Archivo**: `~/.config/remmina/remmina.pref`
```ini
[remmina]
datadir_path=/home/$USER/.local/share/remmina
screenshot_path=/home/$USER
console_font=
terminal_font=
scrollback_lines=512
resolutions=640x480,800x600,1024x768,1920x1080,2560x1440
scale_quality=3
confirm_close=true
```
**Por qué funciona**: 

- Configuración almacenada en directorio del usuario
- Se carga automáticamente cada vez que se abre Remmina
- Incluye resoluciones optimizadas y configuraciones de calidad

### 3. Script de Conexión Automatizada

#### Script de Conexión Rápida
**Archivo**: `~/rdp-connect.sh`
```bash
#!/bin/bash
RDP_IP="$1"
RDP_USER="RDP"

# Verificar conexión Tailscale
if ! tailscale status &> /dev/null; then
    sudo tailscale up
fi

# Conectar con Remmina
remmina -c rdp://$RDP_USER@$RDP_IP:3389
```
**Por qué funciona**:

- Verifica automáticamente el estado de Tailscale
- Se reconecta si es necesario
- Lanza Remmina con parámetros correctos

## 🚀 Proceso de Instalación

### Paso 1: Ejecutar el Script de Configuración
```bash
chmod +x setup-persistent-rdp.sh
./setup-persistent-rdp.sh
```

### Paso 2: Configuración Inicial de Tailscale (Solo una vez)
```bash
sudo tailscale up
# Seguir el enlace para autenticar el dispositivo
```

### Paso 3: Verificar Configuración
```bash
# Verificar servicios habilitados
systemctl is-enabled tailscaled
systemctl is-enabled tailscale-autoconnect

# Verificar estado de servicios
systemctl status tailscaled
systemctl status tailscale-autoconnect
```

## 🔄 Verificación de Persistencia

### Después del Reinicio, Verificar:

1. **Tailscale se inicia automáticamente**:
   ```bash
   systemctl status tailscaled
   tailscale status
   ```

2. **Configuración de Remmina se mantiene**:
   ```bash
   ls -la ~/.config/remmina/
   cat ~/.config/remmina/remmina.pref
   ```

3. **Script de conexión disponible**:
   ```bash
   ls -la ~/rdp-connect.sh
   ```

## 📁 Archivos y Servicios Creados/Modificados

### Servicios Systemd
- `tailscaled.service` - Habilitado para inicio automático
- `tailscale-autoconnect.service` - Creado y habilitado

### Archivos de Configuración
- `/etc/systemd/system/tailscaled.service.d/override.conf` - Configuración de reinicio
- `/etc/systemd/system/tailscale-autoconnect.service` - Servicio de auto-conexión
- `/usr/local/bin/tailscale-autoconnect.sh` - Script de auto-conexión
- `~/.config/remmina/remmina.pref` - Configuración de Remmina
- `~/rdp-connect.sh` - Script de conexión rápida

### Paquetes Instalados
- `tailscale` - Cliente VPN
- `remmina` - Cliente RDP
- `freerdp` - Protocolo RDP
- `libvncserver` - Soporte VNC
- `spice-gtk` - Soporte SPICE
- `telepathy-glib` - Comunicaciones

## 🛠️ Solución de Problemas

### Si Tailscale no se conecta automáticamente:
```bash
# Verificar estado del servicio
sudo systemctl status tailscaled
sudo systemctl status tailscale-autoconnect

# Reiniciar servicios
sudo systemctl restart tailscaled
sudo systemctl restart tailscale-autoconnect

# Verificar logs
journalctl -u tailscaled -f
journalctl -u tailscale-autoconnect -f
```

### Si Remmina no mantiene la configuración:
```bash
# Verificar archivos de configuración
ls -la ~/.config/remmina/
cat ~/.config/remmina/remmina.pref

# Recrear configuración
rm ~/.config/remmina/remmina.pref
# Ejecutar nuevamente el script de configuración
```

## ✅ Garantías de Persistencia

1. **Servicios Systemd**: Los servicios habilitados con `systemctl enable` se inician automáticamente en cada boot
2. **Archivos de Configuración**: Almacenados en ubicaciones estándar del sistema que persisten entre reinicios
3. **Scripts Ejecutables**: Colocados en directorios del usuario con permisos correctos
4. **Configuración de Red**: Reglas de firewall persistentes (si UFW está habilitado)
5. **Auto-Reconexión**: Servicios configurados para reiniciarse automáticamente si fallan

Esta configuración garantiza que después de cualquier reinicio del sistema, todos los componentes necesarios para RDP estarán disponibles y funcionando automáticamente.
