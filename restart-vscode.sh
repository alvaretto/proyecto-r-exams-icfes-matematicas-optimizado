#!/bin/bash

# Detener y eliminar el contenedor actual
echo "Deteniendo y eliminando el contenedor VSCode..."
docker stop vscode-server
docker rm vscode-server

# Crear un nuevo contenedor con un volumen temporal para las extensiones
echo "Creando un nuevo contenedor VSCode sin extensiones..."
docker run -d -p 8080:8080 \
  -v "$(pwd):/workspaces/project" \
  --tmpfs /home/coder/.local/share/code-server/extensions \
  --name vscode-server \
  codercom/code-server:latest

# Mostrar la contraseña para acceder
echo "Obteniendo la contraseña para acceder..."
sleep 2
PASSWORD=$(docker exec vscode-server cat /home/coder/.config/code-server/config.yaml | grep password | awk '{print $2}')

echo ""
echo "¡Listo! El contenedor VSCode ha sido reiniciado sin extensiones."
echo "Puedes acceder a VSCode en http://localhost:8080"
echo "Contraseña: $PASSWORD"
