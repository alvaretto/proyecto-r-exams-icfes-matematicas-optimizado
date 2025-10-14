# 🚀 Configuración Persistente de RDP en Manjaro Linux

## 📋 Resumen

Este conjunto de scripts automatiza completamente la configuración de RDP en Manjaro Linux, haciendo que **todos los componentes sean persistentes** y sobrevivan a los reinicios del sistema.

## 🎯 Problema Resuelto

**Antes**: Después de cada reinicio necesitabas:
- Iniciar manualmente Tailscale
- Reconfigurar Remmina
- Recordar comandos y configuraciones

**Después**: Todo se inicia automáticamente y funciona sin intervención manual.

## 📁 Archivos Incluidos

| Archivo | Descripción |
|---------|-------------|
| `setup-persistent-rdp.sh` | 🔧 Script principal de configuración automática |
| `verify-rdp-config.sh` | ✅ Script de verificación post-instalación |
| `CONFIGURACION-PERSISTENTE.md` | 📖 Documentación técnica detallada |
| `RDP-Manjaro.md` | 📝 Tutorial original actualizado |

## ⚡ Instalación Rápida (3 pasos)

### Paso 1: Ejecutar Configuración Automática
```bash
./setup-persistent-rdp.sh
```
**Qué hace**: Instala paquetes, configura servicios, crea archivos de configuración

### Paso 2: Configuración Inicial de Tailscale (Solo una vez)
```bash
sudo tailscale up
```
**Qué hace**: Autentica tu dispositivo con Tailscale (sigue el enlace que aparece)

### Paso 3: Verificar Configuración
```bash
./verify-rdp-config.sh
```
**Qué hace**: Confirma que todo esté funcionando correctamente

## 🔄 Uso Diario

### Conexión Rápida a RDP
```bash
# Método 1: Script automatizado (recomendado)
~/rdp-connect.sh <IP_TAILSCALE>

# Método 2: Remmina manual (ya configurado)
remmina
```

### Obtener IP de Tailscale
```bash
tailscale ip -4
```

## ✅ Configuraciones que se Hacen Persistentes

### 1. 🌐 Tailscale (VPN)
- ✅ Servicio `tailscaled` habilitado para inicio automático
- ✅ Auto-reconexión si el servicio falla
- ✅ Servicio personalizado de auto-conexión
- ✅ Configuración de variables de entorno persistentes

### 2. 🖥️ Remmina (Cliente RDP)
- ✅ Configuración optimizada guardada en `~/.config/remmina/`
- ✅ Resoluciones de pantalla preconfiguradas
- ✅ Ajustes de calidad y rendimiento optimizados
- ✅ Directorio de datos configurado

### 3. 🔧 Scripts y Herramientas
- ✅ Script de conexión rápida (`~/rdp-connect.sh`)
- ✅ Verificación automática de estado de Tailscale
- ✅ Reconexión automática si es necesario

### 4. 🔥 Firewall (si está habilitado)
- ✅ Reglas para permitir tráfico de Tailscale
- ✅ Configuración persistente de UFW

## 🛠️ Servicios Systemd Creados

| Servicio | Estado | Función |
|----------|--------|---------|
| `tailscaled.service` | Habilitado | Servicio principal de Tailscale |
| `tailscale-autoconnect.service` | Habilitado | Auto-conexión al inicio del sistema |

## 📊 Verificación Post-Reinicio

Después de reiniciar tu sistema, ejecuta:
```bash
./verify-rdp-config.sh
```

**Salida esperada**:
- ✅ Todos los paquetes instalados
- ✅ Servicios habilitados y activos
- ✅ Archivos de configuración presentes
- ✅ Tailscale conectado automáticamente

## 🔍 Solución de Problemas

### Si algo no funciona después del reinicio:

1. **Verificar servicios**:
   ```bash
   systemctl status tailscaled
   systemctl status tailscale-autoconnect
   ```

2. **Ver logs**:
   ```bash
   journalctl -u tailscaled -f
   journalctl -u tailscale-autoconnect -f
   ```

3. **Reconectar Tailscale manualmente**:
   ```bash
   sudo tailscale up
   ```

4. **Re-ejecutar configuración**:
   ```bash
   ./setup-persistent-rdp.sh
   ```

## 🎉 Beneficios de la Configuración Persistente

### ⏰ Ahorro de Tiempo
- No más configuración manual después de reinicios
- Conexión automática a Tailscale
- Scripts de conexión rápida

### 🔒 Confiabilidad
- Servicios configurados para reiniciarse automáticamente
- Configuraciones almacenadas en ubicaciones estándar del sistema
- Verificación automática de estado

### 🚀 Facilidad de Uso
- Un solo comando para conectarse: `~/rdp-connect.sh <IP>`
- Configuración de Remmina optimizada
- Scripts de verificación incluidos

## 📝 Archivos de Configuración Creados

### Sistema (requieren sudo)
- `/etc/systemd/system/tailscaled.service.d/override.conf`
- `/etc/systemd/system/tailscale-autoconnect.service`
- `/usr/local/bin/tailscale-autoconnect.sh`

### Usuario (en tu directorio home)
- `~/.config/remmina/remmina.pref`
- `~/rdp-connect.sh`

## 🔄 Mantenimiento

### Actualizar Configuración
Si necesitas modificar la configuración, simplemente ejecuta nuevamente:
```bash
./setup-persistent-rdp.sh
```

### Desinstalar (si es necesario)
```bash
# Deshabilitar servicios
sudo systemctl disable tailscale-autoconnect
sudo systemctl disable tailscaled

# Remover archivos de configuración
sudo rm -f /etc/systemd/system/tailscale-autoconnect.service
sudo rm -f /usr/local/bin/tailscale-autoconnect.sh
sudo rm -rf /etc/systemd/system/tailscaled.service.d/

# Remover configuración de usuario
rm -f ~/.config/remmina/remmina.pref
rm -f ~/rdp-connect.sh
```

## 🎯 Resultado Final

Después de ejecutar esta configuración:

1. **Reinicia tu sistema** 
2. **Tailscale se conecta automáticamente**
3. **Remmina está listo para usar**
4. **Conecta a RDP con**: `~/rdp-connect.sh <IP_TAILSCALE>`

**¡Tu configuración RDP es ahora completamente persistente y automática!** 🎉
