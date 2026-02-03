#!/bin/bash

# =============================================================================
# CONFIGURACIÓN COMPLETA DE GIT PARA EL REPOSITORIO
# =============================================================================

echo "🔧 Configurando Git para el repositorio..."

# 1. CONFIGURAR IDENTIDAD DE GIT (GLOBAL)
echo "📝 Configurando identidad de usuario..."
git config --global user.email "alvaroangelm@gmail.com"
git config --global user.name "Alvaro Angel"

# 2. VERIFICAR CONFIGURACIÓN
echo "✅ Verificando configuración:"
git config --global user.email
git config --global user.name

# 3. CONFIGURAR AUTENTICACIÓN CON TOKEN (RECOMENDADO)
echo "🔐 Configurando autenticación con token..."

# Opción A: Configurar credential helper para almacenar token
git config --global credential.helper store

# 4. AGREGAR CAMBIOS AL STAGING
echo "📦 Agregando cambios al staging..."
git add .

# 5. HACER COMMIT
echo "💾 Haciendo commit..."
git commit -m "Actualizando Rexams Lubuntu - Solución definitiva matplotlib y reticulate"

# 6. CONFIGURAR REMOTE CON TOKEN (si es necesario)
echo "🌐 Verificando configuración del remote..."
git remote -v

# 7. PUSH CON FORCE-WITH-LEASE
echo "🚀 Haciendo push al repositorio..."
git push --force-with-lease origin experimentos-seguros

echo "✅ ¡Proceso completado!"