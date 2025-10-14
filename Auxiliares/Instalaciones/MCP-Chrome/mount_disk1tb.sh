#!/bin/bash

# Script para montar el disco de 1TB con permisos totales
# Autor: Asistente IA
# Fecha: $(date)

DEVICE="/dev/sdb1"
MOUNT_POINT="/media/disco1tb"
USER="proyectos"

echo "=== Script de montaje del disco de 1TB ==="

# Verificar si el dispositivo existe
if [ ! -b "$DEVICE" ]; then
    echo "Error: El dispositivo $DEVICE no existe"
    exit 1
fi

# Crear el punto de montaje si no existe
if [ ! -d "$MOUNT_POINT" ]; then
    echo "Creando punto de montaje: $MOUNT_POINT"
    sudo mkdir -p "$MOUNT_POINT"
fi

# Verificar si ya está montado
if mount | grep -q "$DEVICE"; then
    echo "El dispositivo ya está montado:"
    mount | grep "$DEVICE"
    
    # Intentar cambiar permisos del punto de montaje actual
    CURRENT_MOUNT=$(mount | grep "$DEVICE" | awk '{print $3}')
    echo "Cambiando permisos de: $CURRENT_MOUNT"
    
    # Cambiar propietario del punto de montaje
    sudo chown $USER:$USER "$CURRENT_MOUNT" 2>/dev/null || echo "No se pudo cambiar el propietario del punto de montaje"
    
    # Cambiar permisos
    sudo chmod 755 "$CURRENT_MOUNT" 2>/dev/null || echo "No se pudo cambiar permisos del punto de montaje"
    
    echo "Permisos actuales:"
    ls -la "$CURRENT_MOUNT"
    
else
    echo "Intentando montar $DEVICE en $MOUNT_POINT..."
    
    # Intentar montar con diferentes opciones
    if sudo mount -o rw,user_subvol_rm_allowed "$DEVICE" "$MOUNT_POINT" 2>/dev/null; then
        echo "Montado exitosamente con opciones estándar"
    elif sudo mount -o ro "$DEVICE" "$MOUNT_POINT" 2>/dev/null; then
        echo "Montado en modo solo lectura (el sistema de archivos tiene errores)"
    else
        echo "Error: No se pudo montar el dispositivo"
        echo "Verificando errores del sistema:"
        sudo dmesg | tail -5
        exit 1
    fi
    
    # Cambiar permisos después del montaje
    echo "Configurando permisos..."
    sudo chown $USER:$USER "$MOUNT_POINT" 2>/dev/null || echo "Advertencia: No se pudo cambiar el propietario"
    sudo chmod 755 "$MOUNT_POINT" 2>/dev/null || echo "Advertencia: No se pudo cambiar permisos"
fi

echo ""
echo "=== Estado final ==="
echo "Dispositivo: $DEVICE"
echo "Punto de montaje: $(mount | grep "$DEVICE" | awk '{print $3}')"
echo "Opciones de montaje: $(mount | grep "$DEVICE" | sed 's/.*(\(.*\)).*/\1/')"
echo ""
echo "Contenido del disco:"
ACTUAL_MOUNT=$(mount | grep "$DEVICE" | awk '{print $3}')
if [ -n "$ACTUAL_MOUNT" ]; then
    ls -la "$ACTUAL_MOUNT"
    echo ""
    echo "Espacio disponible:"
    df -h "$ACTUAL_MOUNT"
fi

echo ""
echo "=== Acceso rápido ==="
echo "Puedes acceder al disco usando:"
echo "  cd ~/disco1tb"
echo "  o directamente: cd $(mount | grep "$DEVICE" | awk '{print $3}')"
