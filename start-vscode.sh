#!/bin/bash

# Limpiar el directorio de extensiones al inicio
rm -rf /home/coder/.local/share/code-server/extensions/*

# Iniciar code-server con los argumentos predeterminados
exec /usr/bin/entrypoint.sh "$@"
