#!/bin/bash

# Cargar NVM
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"

# Cargar variables de entorno desde .env si existe en el directorio actual
if [ -f .env ]; then
  export $(grep -v '^#' .env | xargs)
  echo "Variables de entorno cargadas desde .env"
else
  echo "Archivo .env no encontrado en el directorio actual"
  echo "Asegúrate de tener configurada la variable OPENROUTER_API_KEY"
fi

# Ejecutar Codex con Open Router como proveedor
codex --provider openrouter "$@"
