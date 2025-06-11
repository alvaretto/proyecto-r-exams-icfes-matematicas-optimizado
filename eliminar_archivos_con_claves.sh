#!/bin/bash
# Script para eliminar todos los archivos con claves API

# Eliminar archivos que contienen claves API
rm -f .env
rm -f ~/.mcp.json
rm -f ~/start-brave-search-mcp.sh
rm -f ~/install_plandex.sh
rm -f ~/Desktop/Plandex.desktop
rm -f ~/.Renviron

echo "Archivos con claves API eliminados"