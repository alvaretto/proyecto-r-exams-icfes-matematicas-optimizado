#!/bin/bash

# ============================================================================
# SCRIPT DE LIMPIEZA COMPLETA DE CACHÉ - R Y RSTUDIO
# ============================================================================

echo ""
echo "============================================================================"
echo "LIMPIEZA COMPLETA DE CACHÉ - R Y RSTUDIO"
echo "============================================================================"
echo ""

# Directorio actual
DIR_ACTUAL=$(pwd)
echo "📂 Directorio actual: $DIR_ACTUAL"
echo ""

# ============================================================================
# 1. LIMPIAR CACHÉ DE KNITR (en directorio actual)
# ============================================================================

echo "🧹 PASO 1: Limpiando caché de knitr..."

if [ -d "${DIR_ACTUAL}/*_cache" ]; then
    rm -rf ${DIR_ACTUAL}/*_cache
    echo "   ✅ Eliminado: *_cache/"
else
    echo "   ℹ️  No hay carpetas *_cache en este directorio"
fi

if [ -d "${DIR_ACTUAL}/*_files" ]; then
    rm -rf ${DIR_ACTUAL}/*_files
    echo "   ✅ Eliminado: *_files/"
else
    echo "   ℹ️  No hay carpetas *_files en este directorio"
fi

# ============================================================================
# 2. LIMPIAR ARCHIVOS TEMPORALES DE R
# ============================================================================

echo ""
echo "🧹 PASO 2: Limpiando archivos temporales de R..."

# .Rhistory
if [ -f "${DIR_ACTUAL}/.Rhistory" ]; then
    rm -f ${DIR_ACTUAL}/.Rhistory
    echo "   ✅ Eliminado: .Rhistory"
fi

# .RData
if [ -f "${DIR_ACTUAL}/.RData" ]; then
    rm -f ${DIR_ACTUAL}/.RData
    echo "   ✅ Eliminado: .RData"
fi

# .Rproj.user (si existe)
if [ -d "${DIR_ACTUAL}/.Rproj.user" ]; then
    rm -rf ${DIR_ACTUAL}/.Rproj.user
    echo "   ✅ Eliminado: .Rproj.user/"
fi

echo "   ✅ Archivos temporales de R limpiados"

# ============================================================================
# 3. LIMPIAR CACHÉ GLOBAL DE R
# ============================================================================

echo ""
echo "🧹 PASO 3: Limpiando caché global de R..."

# Caché de R en home
if [ -d "$HOME/.cache/R" ]; then
    rm -rf $HOME/.cache/R/*
    echo "   ✅ Eliminado: ~/.cache/R/*"
fi

# Caché de knitr global
if [ -d "$HOME/.cache/knitr" ]; then
    rm -rf $HOME/.cache/knitr/*
    echo "   ✅ Eliminado: ~/.cache/knitr/*"
fi

echo "   ✅ Caché global de R limpiado"

# ============================================================================
# 4. LIMPIAR CACHÉ DE RSTUDIO
# ============================================================================

echo ""
echo "🧹 PASO 4: Limpiando caché de RStudio..."

# Caché de RStudio
if [ -d "$HOME/.cache/rstudio" ]; then
    rm -rf $HOME/.cache/rstudio/*
    echo "   ✅ Eliminado: ~/.cache/rstudio/*"
fi

# Configuración de RStudio (estado de sesión)
if [ -d "$HOME/.rstudio" ]; then
    # Solo limpiar archivos de sesión, no configuración
    find $HOME/.rstudio -name "*.rdb" -delete 2>/dev/null
    find $HOME/.rstudio -name "*.rdx" -delete 2>/dev/null
    find $HOME/.rstudio -name "*-lock" -delete 2>/dev/null
    echo "   ✅ Eliminados archivos de sesión de RStudio"
fi

# Caché local de RStudio
if [ -d "$HOME/.local/share/rstudio" ]; then
    rm -rf $HOME/.local/share/rstudio/sessions/*
    echo "   ✅ Eliminado: ~/.local/share/rstudio/sessions/*"
fi

echo "   ✅ Caché de RStudio limpiado"

# ============================================================================
# 5. LIMPIAR CACHÉ DE RETICULATE (Python)
# ============================================================================

echo ""
echo "🧹 PASO 5: Limpiando caché de reticulate..."

# Caché de reticulate
if [ -d "$HOME/.cache/reticulate" ]; then
    rm -rf $HOME/.cache/reticulate/*
    echo "   ✅ Eliminado: ~/.cache/reticulate/*"
fi

# __pycache__ en directorio actual
if [ -d "${DIR_ACTUAL}/__pycache__" ]; then
    rm -rf ${DIR_ACTUAL}/__pycache__
    echo "   ✅ Eliminado: __pycache__/"
fi

echo "   ✅ Caché de reticulate limpiado"

# ============================================================================
# 6. LIMPIAR ARCHIVOS GENERADOS POR PRUEBAS
# ============================================================================

echo ""
echo "🧹 PASO 6: Limpiando archivos de pruebas anteriores..."

# Carpetas de prueba
for dir in test_output test_verificacion compilacion_directa salida_manual; do
    if [ -d "${DIR_ACTUAL}/${dir}" ]; then
        rm -rf ${DIR_ACTUAL}/${dir}
        echo "   ✅ Eliminado: ${dir}/"
    fi
done

# Archivos .md temporales generados por knitr
find ${DIR_ACTUAL} -maxdepth 1 -name "*.md" -newer ${DIR_ACTUAL}/INSTRUCCIONES_COMPILACION.md -delete 2>/dev/null

echo "   ✅ Archivos de pruebas limpiados"

# ============================================================================
# RESUMEN
# ============================================================================

echo ""
echo "============================================================================"
echo "RESUMEN DE LIMPIEZA"
echo "============================================================================"
echo ""
echo "✅ Caché de knitr limpiado"
echo "✅ Archivos temporales de R eliminados"
echo "✅ Caché global de R limpiado"
echo "✅ Caché de RStudio limpiado"
echo "✅ Caché de reticulate limpiado"
echo "✅ Archivos de pruebas eliminados"
echo ""
echo "============================================================================"
echo "PRÓXIMOS PASOS"
echo "============================================================================"
echo ""
echo "1. Si RStudio está abierto, CIÉRRALO COMPLETAMENTE"
echo "2. Vuelve a abrir RStudio"
echo "3. Abre el archivo .Rmd"
echo "4. Compila usando COMPILAR_AQUI.R o desde la consola"
echo ""
echo "============================================================================"
echo ""

