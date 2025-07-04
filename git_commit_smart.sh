#!/bin/bash

# Script de commit inteligente para proyecto R-exams ICFES
# Autor: Optimización de flujo de trabajo Git
# Fecha: Enero 2025

echo "🚀 COMMIT INTELIGENTE - Proyecto R-exams ICFES"
echo "=============================================="

# Verificar que estamos en un repositorio Git
if ! git rev-parse --git-dir > /dev/null 2>&1; then
    echo "❌ Error: No estás en un repositorio Git"
    exit 1
fi

# Verificar estado
echo "📊 Estado actual del repositorio:"
git status --short

# Verificar si hay cambios para commitear
if git diff --cached --quiet && git diff --quiet; then
    echo "ℹ️  No hay cambios para commitear"
    exit 0
fi

# Verificar archivos grandes
echo ""
echo "📦 Verificando archivos grandes (>50MB)..."
archivos_grandes=$(find . -type f -size +50M -not -path "./.git/*" 2>/dev/null | head -5)
if [ -n "$archivos_grandes" ]; then
    echo "⚠️  Archivos grandes encontrados:"
    echo "$archivos_grandes"
    echo "💡 Considera usar Git LFS para estos archivos"
    echo ""
fi

# Verificar archivos .Rmd modificados
rmd_files=$(git diff --name-only --diff-filter=ACM | grep '\.Rmd$' | head -5)
if [ -n "$rmd_files" ]; then
    echo "📝 Archivos .Rmd modificados:"
    echo "$rmd_files"
    echo ""
fi

# Preguntar tipo de commit
echo "🎯 ¿Qué tipo de cambios estás committeando?"
echo "1) feat - Nueva funcionalidad (nuevos ejercicios, metodologías)"
echo "2) fix - Corrección de errores (TikZ, compilación, datos)"
echo "3) docs - Documentación (README, guías, metodologías)"
echo "4) refactor - Refactoring (optimización de código)"
echo "5) test - Pruebas (validación, testing)"
echo "6) chore - Mantenimiento (configuración, limpieza)"

read -p "Selecciona (1-6): " tipo_num

case $tipo_num in
    1) tipo="feat" ;;
    2) tipo="fix" ;;
    3) tipo="docs" ;;
    4) tipo="refactor" ;;
    5) tipo="test" ;;
    6) tipo="chore" ;;
    *) 
        echo "⚠️  Selección inválida, usando 'chore'"
        tipo="chore" 
        ;;
esac

# Preguntar alcance
echo ""
echo "🎯 Alcance del cambio:"
echo "- ejercicios (nuevos ejercicios, modificaciones)"
echo "- tikz (gráficos, visualizaciones)"
echo "- metodologia (metodologías TikZ, corrección errores)"
echo "- docs (documentación, guías)"
echo "- config (configuración, scripts)"
echo "- general (cambios múltiples)"

read -p "Alcance: " alcance

# Validar alcance
if [ -z "$alcance" ]; then
    alcance="general"
fi

# Preguntar descripción
echo ""
read -p "📝 Descripción breve (obligatoria): " descripcion

# Validar descripción
if [ -z "$descripcion" ]; then
    echo "❌ Error: La descripción es obligatoria"
    exit 1
fi

# Preguntar descripción detallada
echo ""
echo "📋 Descripción detallada (opcional):"
echo "Escribe detalles línea por línea. Presiona Enter en línea vacía para terminar."
descripcion_detallada=""
while IFS= read -r line; do
    [[ -z $line ]] && break
    if [ -z "$descripcion_detallada" ]; then
        descripcion_detallada="- $line"
    else
        descripcion_detallada+=$'\n'"- $line"
    fi
done

# Construir mensaje de commit
mensaje="$tipo($alcance): $descripcion"
if [[ -n $descripcion_detallada ]]; then
    mensaje+=$'\n\n'"$descripcion_detallada"
fi

# Mostrar mensaje de commit
echo ""
echo "📝 Mensaje de commit generado:"
echo "=============================="
echo "$mensaje"
echo "=============================="
echo ""

# Mostrar archivos que se van a commitear
echo "📁 Archivos que se incluirán en el commit:"
git add . 2>/dev/null
git diff --cached --name-only | head -10
if [ $(git diff --cached --name-only | wc -l) -gt 10 ]; then
    echo "... y $(( $(git diff --cached --name-only | wc -l) - 10 )) archivos más"
fi
echo ""

# Confirmar
read -p "¿Proceder con el commit? (y/N): " confirmar

if [[ $confirmar =~ ^[Yy]$ ]]; then
    # Realizar commit
    if git commit -m "$mensaje"; then
        echo "✅ Commit realizado exitosamente"
        
        # Mostrar información del commit
        echo ""
        echo "📊 Información del commit:"
        git log -1 --oneline
        
        # Preguntar si hacer push
        echo ""
        read -p "🚀 ¿Hacer push a experimentos-seguros? (y/N): " push_confirmar
        
        if [[ $push_confirmar =~ ^[Yy]$ ]]; then
            echo "🔄 Realizando push seguro..."
            
            # Verificar rama actual
            rama_actual=$(git branch --show-current)
            if [ "$rama_actual" != "experimentos-seguros" ]; then
                echo "⚠️  Estás en la rama '$rama_actual', no en 'experimentos-seguros'"
                read -p "¿Cambiar a experimentos-seguros? (y/N): " cambiar_rama
                
                if [[ $cambiar_rama =~ ^[Yy]$ ]]; then
                    git checkout experimentos-seguros
                    git merge "$rama_actual"
                fi
            fi
            
            # Realizar push
            if git push --force-with-lease origin experimentos-seguros; then
                echo "🎉 Push completado exitosamente"
                echo ""
                echo "📊 Estado final:"
                git log -1 --oneline
            else
                echo "❌ Error en el push. Verifica la configuración de Git"
                echo "💡 Puedes intentar: git push --force-with-lease origin experimentos-seguros"
            fi
        else
            echo "ℹ️  Commit guardado localmente. Puedes hacer push más tarde con:"
            echo "   git push --force-with-lease origin experimentos-seguros"
        fi
    else
        echo "❌ Error al realizar el commit"
        exit 1
    fi
else
    echo "❌ Commit cancelado"
    # Deshacer git add
    git reset HEAD . 2>/dev/null
    echo "ℹ️  Cambios no agregados al staging area"
fi

echo ""
echo "🎯 Script completado"