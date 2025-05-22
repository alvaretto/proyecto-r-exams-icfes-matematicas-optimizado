#!/bin/bash

# Directorio temporal para descargar la extensión
TEMP_DIR="/tmp/augment-extension"
mkdir -p $TEMP_DIR

# URL de la extensión Augment (podría necesitar ser actualizada)
EXTENSION_URL="https://marketplace.visualstudio.com/_apis/public/gallery/publishers/augment/vsextensions/vscode-augment/latest/vspackage"

echo "Descargando la extensión Augment..."
curl -L -o "$TEMP_DIR/augment.vsix" "$EXTENSION_URL"

echo "Instalando la extensión Augment..."
code-server --install-extension "$TEMP_DIR/augment.vsix"

echo "Limpiando archivos temporales..."
rm -rf $TEMP_DIR

echo "¡Instalación completada!"
