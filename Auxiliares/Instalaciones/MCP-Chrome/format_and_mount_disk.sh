#!/bin/bash

# Script para formatear y montar el disco de 1TB con permisos totales
# Este script debe ejecutarse después de reiniciar el sistema
# Autor: Asistente IA

DEVICE="/dev/sdb1"
MOUNT_POINT="/media/disco1tb"
USER="proyectos"
LABEL="Disco1TB"

echo "=== Script de formateo y montaje del disco de 1TB ==="
echo "⚠️  ADVERTENCIA: Este script BORRARÁ todos los datos del disco $DEVICE"
echo ""

# Función para preguntar confirmación
ask_confirmation() {
    echo "¿Estás seguro de que quieres continuar? (escriba 'SI' para confirmar)"
    read -r response
    if [ "$response" != "SI" ]; then
        echo "Operación cancelada."
        exit 1
    fi
}

# Verificar si el dispositivo existe
if [ ! -b "$DEVICE" ]; then
    echo "Error: El dispositivo $DEVICE no existe"
    exit 1
fi

# Mostrar información del disco
echo "Información del disco:"
sudo fdisk -l "$DEVICE" 2>/dev/null || echo "No se pudo obtener información del disco"
echo ""

# Pedir confirmación
ask_confirmation

echo "Iniciando proceso de formateo..."

# Paso 1: Desmontar si está montado
echo "1. Desmontando dispositivo si está montado..."
sudo umount "$DEVICE" 2>/dev/null || true

# Paso 2: Limpiar firmas del sistema de archivos
echo "2. Limpiando firmas del sistema de archivos..."
sudo wipefs -a "$DEVICE" 2>/dev/null || echo "Advertencia: No se pudieron limpiar todas las firmas"

# Paso 3: Sobrescribir el inicio de la partición
echo "3. Limpiando inicio de la partición..."
sudo dd if=/dev/zero of="$DEVICE" bs=1M count=10 2>/dev/null || echo "Advertencia: No se pudo limpiar completamente"

# Paso 4: Formatear con ext4
echo "4. Formateando con ext4..."
if sudo mkfs.ext4 -F -L "$LABEL" "$DEVICE"; then
    echo "✅ Formateo completado exitosamente"
else
    echo "❌ Error en el formateo. Intentando método alternativo..."
    # Método alternativo más agresivo
    sudo dd if=/dev/zero of="$DEVICE" bs=1M count=100
    sleep 2
    sudo mkfs.ext4 -F -L "$LABEL" "$DEVICE" || {
        echo "❌ Error crítico: No se pudo formatear el disco"
        exit 1
    }
fi

# Paso 5: Crear punto de montaje
echo "5. Creando punto de montaje..."
sudo mkdir -p "$MOUNT_POINT"

# Paso 6: Montar el disco
echo "6. Montando el disco..."
if sudo mount "$DEVICE" "$MOUNT_POINT"; then
    echo "✅ Disco montado exitosamente"
else
    echo "❌ Error al montar el disco"
    exit 1
fi

# Paso 7: Configurar permisos
echo "7. Configurando permisos..."
sudo chown "$USER:$USER" "$MOUNT_POINT"
sudo chmod 755 "$MOUNT_POINT"

# Paso 8: Crear directorio de trabajo
echo "8. Creando estructura de directorios..."
sudo -u "$USER" mkdir -p "$MOUNT_POINT"/{Documentos,Descargas,Proyectos,Backup,Temporal}

# Paso 9: Configurar montaje automático permanente
echo "9. Configurando montaje automático..."

# Obtener UUID del disco
UUID=$(sudo blkid -s UUID -o value "$DEVICE")
if [ -n "$UUID" ]; then
    # Crear entrada en fstab
    FSTAB_ENTRY="UUID=$UUID $MOUNT_POINT ext4 defaults,user,rw,exec 0 2"
    
    # Verificar si ya existe una entrada para este UUID
    if ! grep -q "$UUID" /etc/fstab; then
        echo "Agregando entrada a /etc/fstab..."
        echo "$FSTAB_ENTRY" | sudo tee -a /etc/fstab
        echo "✅ Montaje automático configurado"
    else
        echo "⚠️  Ya existe una entrada en /etc/fstab para este disco"
    fi
else
    echo "⚠️  No se pudo obtener el UUID del disco"
fi

# Paso 10: Crear enlace simbólico
echo "10. Creando acceso rápido..."
ln -sf "$MOUNT_POINT" "/home/$USER/disco1tb"

echo ""
echo "=== Configuración completada ==="
echo "Disco: $DEVICE"
echo "Punto de montaje: $MOUNT_POINT"
echo "Sistema de archivos: ext4"
echo "Etiqueta: $LABEL"
echo "UUID: $UUID"
echo ""
echo "Información del disco:"
df -h "$MOUNT_POINT"
echo ""
echo "Permisos:"
ls -la "$MOUNT_POINT"
echo ""
echo "Contenido inicial:"
ls -la "$MOUNT_POINT"
echo ""
echo "=== Acceso al disco ==="
echo "Puedes acceder al disco usando:"
echo "  cd ~/disco1tb"
echo "  o directamente: cd $MOUNT_POINT"
echo ""
echo "El disco se montará automáticamente al reiniciar el sistema."
