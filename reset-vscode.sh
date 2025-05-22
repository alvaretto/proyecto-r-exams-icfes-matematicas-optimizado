#!/bin/bash

# Detener el contenedor
echo "Deteniendo el contenedor VSCode..."
docker stop vscode-server

# Eliminar las extensiones
echo "Eliminando las extensiones..."
docker exec -it vscode-server rm -rf /home/coder/.local/share/code-server/extensions/*

# Reiniciar el contenedor
echo "Reiniciando el contenedor VSCode..."
docker start vscode-server

echo "¡Listo! El contenedor VSCode ha sido reiniciado sin extensiones."
echo "Puedes acceder a VSCode en http://localhost:8080"
echo "Recuerda que necesitarás la contraseña para iniciar sesión."
