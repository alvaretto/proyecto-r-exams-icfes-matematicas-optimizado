# Tutorial: Exportar VM de GNOME Boxes a VMware Workstation en Manjaro

## Requisitos previos
- Manjaro Linux como sistema anfitrión
- GNOME Boxes (Flatpak) con VM existente
- VMware Workstation instalado
- Espacio libre en disco (al menos el doble del tamaño de la VM)

## Paso 1: Preparar el sistema

### 1.1 Cerrar completamente GNOME Boxes
```bash
# Cerrar Boxes (Flatpak)
flatpak kill org.gnome.Boxes

# Verificar que no hay procesos ejecutándose
ps aux | grep -i boxes
ps aux | grep qemu
```

### 1.2 Instalar herramientas necesarias
```bash
# Instalar qemu-img (herramienta de conversión)
sudo pacman -S qemu-img

# Verificar instalación
qemu-img --version
```

## Paso 2: Localizar la VM en GNOME Boxes

### 2.1 Encontrar los archivos de la VM
```bash
# Listar VMs disponibles (Flatpak)
ls -la ~/.var/app/org.gnome.Boxes/data/gnome-boxes/images/

# Ver información del archivo
file ~/.var/app/org.gnome.Boxes/data/gnome-boxes/images/*
```

### 2.2 Verificar el formato y tamaño
```bash
# Obtener información detallada de la VM
qemu-img info ~/.var/app/org.gnome.Boxes/data/gnome-boxes/images/[nombre-vm]
```

## Paso 3: Conversión del disco virtual

### 3.1 Crear directorio de trabajo
```bash
# Crear directorio para VMware
mkdir -p ~/VMware-VMs/[nombre-vm]

# Verificar creación
ls -la ~/VMware-VMs/
```

### 3.2 Convertir QCOW2 a VMDK
```bash
# Conversión principal (puede tomar 30-60 minutos)
qemu-img convert -f qcow2 -O vmdk -U \
  ~/.var/app/org.gnome.Boxes/data/gnome-boxes/images/[nombre-vm] \
  ~/VMware-VMs/[nombre-vm]/[nombre-vm].vmdk

# Verificar la conversión exitosa
qemu-img info ~/VMware-VMs/[nombre-vm]/[nombre-vm].vmdk
ls -lah ~/VMware-VMs/[nombre-vm]/
```

### 3.3 Alternativa si hay problemas de bloqueo
```bash
# Si el archivo está en uso, hacer copia primero
cp ~/.var/app/org.gnome.Boxes/data/gnome-boxes/images/[nombre-vm] \
   ~/VMware-VMs/[nombre-vm]-backup.qcow2

# Convertir la copia
qemu-img convert -f qcow2 -O vmdk \
  ~/VMware-VMs/[nombre-vm]-backup.qcow2 \
  ~/VMware-VMs/[nombre-vm]/[nombre-vm].vmdk
```

## Paso 4: Crear VM en VMware Workstation

### 4.1 Configuración inicial
1. **Abrir VMware Workstation**
2. **File → New Virtual Machine**
3. **Seleccionar "Custom (advanced)"**
4. **Hardware compatibility**: Workstation 17.x
5. **Guest operating system**: Linux → Other Linux 5.x kernel 64-bit

### 4.2 Configuración de la VM
- **Virtual machine name**: [nombre-descriptivo]
- **Location**: `/home/[usuario]/VMware-VMs/[nombre-vm]/`
- **Processors**: 2-4 cores (según hardware disponible)
- **Memory**: 4-8GB (recomendado mínimo 4GB)
- **Network connection**: NAT (recomendado)
- **I/O controller**: LSI Logic (recomendado)
- **Virtual disk type**: SCSI (recomendado)

### 4.3 Configurar disco existente
1. **Select a disk**: "Use an existing virtual disk"
2. **Browse** → Navegar a `~/VMware-VMs/[nombre-vm]/[nombre-vm].vmdk`
3. **Keep existing format** (recomendado)

## Paso 5: Primer arranque y configuración

### 5.1 Arrancar la VM
- Iniciar la VM en VMware Workstation
- Esperar a que arranque el sistema operativo

### 5.2 Instalar VMware Tools (dentro de la VM)
```bash
# Actualizar sistema
sudo pacman -Syu

# Instalar VMware Tools para Linux
sudo pacman -S open-vm-tools

# Habilitar servicios
sudo systemctl enable vmtoolsd
sudo systemctl start vmtoolsd
```

### 5.3 Optimizaciones gráficas
```bash
# Driver gráfico optimizado para VMware
sudo pacman -S xf86-video-vmware

# Para XFCE específicamente
sudo pacman -S xf86-video-vmware mesa

# Reiniciar para aplicar cambios
sudo reboot
```

## Paso 6: Configuraciones adicionales

### 6.1 Servicios de integración
```bash
# Habilitar carpetas compartidas (opcional)
sudo systemctl enable vmware-vmblock-fuse
sudo systemctl start vmware-vmblock-fuse

# Para mejor rendimiento de red
sudo systemctl enable vmware-networks
```

### 6.2 Verificar funcionamiento
```bash
# Verificar que VMware Tools funciona
systemctl status vmtoolsd

# Verificar driver gráfico
lspci | grep -i vga
lsmod | grep vmw
```

### 6.3 Configurar carpetas compartidas (opcional)
1. **En VMware**: VM → Settings → Options → Shared Folders
2. **Habilitar**: "Always enabled"
3. **Agregar carpeta**: Seleccionar carpeta del host
4. **En la VM**:
```bash
# Crear punto de montaje
sudo mkdir /mnt/hgfs

# Montar carpetas compartidas
sudo mount -t fuse.vmhgfs-fuse .host:/ /mnt/hgfs -o allow_other
```

## Paso 7: Optimizaciones finales

### 7.1 Configuración de rendimiento
- **VM Settings → Hardware → Display**: Habilitar "Accelerate 3D graphics"
- **VM Settings → Hardware → Memory**: Ajustar según necesidades
- **VM Settings → Hardware → Processors**: Habilitar "Virtualize Intel VT-x/EPT or AMD-V/RVI"

### 7.2 Configuración de red
- **NAT**: Para acceso a internet básico
- **Bridged**: Para que la VM aparezca como dispositivo independiente en la red
- **Host-only**: Para comunicación solo con el host

## Solución de problemas comunes

### Error: "Failed to get shared write lock"
```bash
# Asegurar que Boxes está cerrado
flatpak kill org.gnome.Boxes
pkill -f qemu

# Usar flag -U para forzar acceso
qemu-img convert -f qcow2 -O vmdk -U [origen] [destino]
```

### VM no arranca en VMware
- Verificar que el archivo VMDK no está corrupto
- Cambiar tipo de controlador SCSI a IDE
- Reducir memoria asignada temporalmente

### Problemas gráficos
```bash
# Reinstalar drivers
sudo pacman -S xf86-video-vmware mesa
sudo systemctl restart display-manager
```

### Rendimiento lento
- Aumentar memoria RAM asignada
- Habilitar aceleración 3D
- Verificar que VMware Tools está instalado correctamente

## Notas importantes

1. **Backup**: Siempre hacer respaldo de la VM original antes de la conversión
2. **Espacio**: La conversión requiere espacio adicional (temporalmente el doble)
3. **Tiempo**: La conversión puede tomar 30-60 minutos según el tamaño
4. **Compatibilidad**: Algunas configuraciones específicas pueden requerir ajustes manuales
5. **Licencias**: Verificar que las licencias de software en la VM permiten la migración

## Comandos de referencia rápida

```bash
# Localizar VMs de Boxes
find ~/.var/app/org.gnome.Boxes/ -name "*.qcow2"

# Conversión básica
qemu-img convert -f qcow2 -O vmdk [origen] [destino]

# Información de disco
qemu-img info [archivo]

# Verificar VMware Tools
systemctl status vmtoolsd
```

Este tutorial proporciona una guía completa para migrar VMs de GNOME Boxes a VMware Workstation en sistemas Manjaro Linux.
