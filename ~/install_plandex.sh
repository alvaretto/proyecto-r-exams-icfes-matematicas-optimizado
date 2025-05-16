#!/bin/bash

# Script para instalar Plandex en Manjaro Linux

echo "Iniciando instalación de Plandex..."

# Verificar si ya existe el directorio bin en el home
if [ ! -d "$HOME/bin" ]; then
    echo "Creando directorio ~/bin..."
    mkdir -p "$HOME/bin"
fi

# Copiar el ejecutable de Plandex
echo "Copiando Plandex a ~/bin..."
cp "$HOME/Downloads/plandex" "$HOME/bin/"
chmod +x "$HOME/bin/plandex"

# Añadir ~/bin al PATH si no está ya
if [[ ":$PATH:" != *":$HOME/bin:"* ]]; then
    echo "Añadiendo ~/bin al PATH..."
    echo 'export PATH="$HOME/bin:$PATH"' >> "$HOME/.bashrc"
    export PATH="$HOME/bin:$PATH"
fi

# Crear un enlace simbólico en ~/.local/bin (alternativa común en distribuciones basadas en Arch)
if [ ! -d "$HOME/.local/bin" ]; then
    echo "Creando directorio ~/.local/bin..."
    mkdir -p "$HOME/.local/bin"
fi

echo "Creando enlace simbólico en ~/.local/bin..."
ln -sf "$HOME/bin/plandex" "$HOME/.local/bin/plandex"

# Añadir ~/.local/bin al PATH si no está ya
if [[ ":$PATH:" != *":$HOME/.local/bin:"* ]]; then
    echo "Añadiendo ~/.local/bin al PATH..."
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> "$HOME/.bashrc"
    export PATH="$HOME/.local/bin:$PATH"
fi

echo "Verificando la instalación..."
echo "Plandex se ha instalado en: $HOME/bin/plandex"
echo "También está disponible en: $HOME/.local/bin/plandex"

echo "Para usar Plandex, reinicia tu terminal o ejecuta: source ~/.bashrc"
echo "Luego puedes ejecutar 'plandex' desde cualquier ubicación."

echo "Instalación completada."
