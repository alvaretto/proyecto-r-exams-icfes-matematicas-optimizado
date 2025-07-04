#!/bin/bash

# Script para configurar Git LFS en el proyecto R-exams ICFES
# Soluciona el problema de archivos grandes (>50MB)
# Autor: Optimización de flujo de trabajo Git
# Fecha: Enero 2025

echo "📦 CONFIGURACIÓN GIT LFS - Proyecto R-exams ICFES"
echo "================================================="

# Verificar que estamos en un repositorio Git
if ! git rev-parse --git-dir > /dev/null 2>&1; then
    echo "❌ Error: No estás en un repositorio Git"
    exit 1
fi

# Verificar si Git LFS está instalado
if ! command -v git-lfs &> /dev/null; then
    echo "⚠️  Git LFS no está instalado"
    echo "🔧 Instalando Git LFS..."
    
    # Detectar sistema operativo e instalar
    if command -v pacman &> /dev/null; then
        # Manjaro/Arch Linux
        sudo pacman -S git-lfs
    elif command -v apt &> /dev/null; then
        # Ubuntu/Debian
        sudo apt update && sudo apt install git-lfs
    elif command -v yum &> /dev/null; then
        # CentOS/RHEL
        sudo yum install git-lfs
    else
        echo "❌ No se pudo detectar el gestor de paquetes"
        echo "💡 Instala Git LFS manualmente desde: https://git-lfs.github.io/"
        exit 1
    fi
    
    if [ $? -eq 0 ]; then
        echo "✅ Git LFS instalado exitosamente"
    else
        echo "❌ Error al instalar Git LFS"
        exit 1
    fi
fi

# Verificar archivos grandes actuales
echo ""
echo "🔍 Buscando archivos grandes (>50MB)..."
archivos_grandes=$(find . -type f -size +50M -not -path "./.git/*" 2>/dev/null)

if [ -n "$archivos_grandes" ]; then
    echo "📊 Archivos grandes encontrados:"
    echo "$archivos_grandes" | while read archivo; do
        tamaño=$(du -h "$archivo" | cut -f1)
        echo "  - $archivo ($tamaño)"
    done
    echo ""
else
    echo "✅ No se encontraron archivos >50MB"
fi

# Inicializar Git LFS
echo "🚀 Inicializando Git LFS..."
git lfs install

if [ $? -eq 0 ]; then
    echo "✅ Git LFS inicializado"
else
    echo "❌ Error al inicializar Git LFS"
    exit 1
fi

# Configurar tracking de tipos de archivos
echo ""
echo "📋 Configurando tracking de archivos para Git LFS..."

# Archivos de documentos y datos
git lfs track "*.pdf"
git lfs track "*.docx"
git lfs track "*.pptx"
git lfs track "*.xlsx"

# Archivos de imágenes
git lfs track "*.png"
git lfs track "*.jpg"
git lfs track "*.jpeg"
git lfs track "*.gif"
git lfs track "*.bmp"
git lfs track "*.tiff"
git lfs track "*.svg"

# Archivos de datos R
git lfs track "*.rds"
git lfs track "*.RData"
git lfs track "*.rda"

# Archivos comprimidos
git lfs track "*.zip"
git lfs track "*.tar.gz"
git lfs track "*.rar"
git lfs track "*.7z"

# Archivos de video (si los hay)
git lfs track "*.mp4"
git lfs track "*.avi"
git lfs track "*.mov"

# Archivos específicos del proyecto que puedan ser grandes
git lfs track "*.log"
git lfs track "test_*.pdf"
git lfs track "*_output.html"

echo "✅ Configuración de tracking completada"

# Mostrar configuración actual
echo ""
echo "📋 Tipos de archivos configurados para Git LFS:"
cat .gitattributes

# Agregar .gitattributes al repositorio
echo ""
echo "📝 Agregando configuración al repositorio..."
git add .gitattributes

# Verificar si hay archivos grandes que necesiten ser migrados
echo ""
echo "🔄 Verificando archivos que necesitan migración a LFS..."

# Buscar archivos que coincidan con los patrones pero no estén en LFS
archivos_migrar=""
for patron in "*.pdf" "*.png" "*.jpg" "*.jpeg" "*.rds" "*.RData" "*.zip"; do
    archivos=$(find . -name "$patron" -type f -not -path "./.git/*" 2>/dev/null)
    if [ -n "$archivos" ]; then
        archivos_migrar+="$archivos"$'\n'
    fi
done

if [ -n "$archivos_migrar" ]; then
    echo "📦 Archivos que se migrarán a LFS:"
    echo "$archivos_migrar" | head -10
    
    if [ $(echo "$archivos_migrar" | wc -l) -gt 10 ]; then
        echo "... y más archivos"
    fi
    
    echo ""
    read -p "¿Migrar archivos existentes a Git LFS? (y/N): " migrar
    
    if [[ $migrar =~ ^[Yy]$ ]]; then
        echo "🔄 Migrando archivos a Git LFS..."
        
        # Migrar archivos por tipo
        for patron in "*.pdf" "*.png" "*.jpg" "*.jpeg" "*.rds" "*.RData" "*.zip"; do
            if find . -name "$patron" -type f -not -path "./.git/*" 2>/dev/null | grep -q .; then
                echo "  Migrando archivos $patron..."
                git lfs migrate import --include="$patron" --everything
            fi
        done
        
        echo "✅ Migración completada"
    else
        echo "ℹ️  Migración omitida. Los archivos nuevos usarán LFS automáticamente."
    fi
fi

# Verificar configuración final
echo ""
echo "🔍 Verificando configuración final..."
git lfs ls-files | head -5
if [ $(git lfs ls-files | wc -l) -gt 5 ]; then
    echo "... y $(( $(git lfs ls-files | wc -l) - 5 )) archivos más en LFS"
fi

# Crear commit con la configuración
echo ""
read -p "¿Crear commit con la configuración de Git LFS? (y/N): " commit_config

if [[ $commit_config =~ ^[Yy]$ ]]; then
    git add .gitattributes
    git commit -m "chore(config): configurar Git LFS para archivos grandes

- Configurado tracking para PDF, imágenes, datos R y archivos comprimidos
- Soluciona advertencias de GitHub sobre archivos >50MB
- Mejora rendimiento del repositorio"

    echo "✅ Configuración de Git LFS commiteada"
    
    # Preguntar si hacer push
    read -p "🚀 ¿Hacer push de la configuración? (y/N): " push_config
    
    if [[ $push_config =~ ^[Yy]$ ]]; then
        git push --force-with-lease origin experimentos-seguros
        echo "🎉 Configuración enviada al repositorio remoto"
    fi
fi

echo ""
echo "🎯 CONFIGURACIÓN COMPLETADA"
echo "=========================="
echo "✅ Git LFS configurado y funcionando"
echo "✅ Archivos grandes serán manejados automáticamente"
echo "✅ GitHub no mostrará más advertencias sobre archivos grandes"
echo ""
echo "💡 Próximos pasos:"
echo "   - Los nuevos archivos grandes se manejarán automáticamente"
echo "   - Usa './git_commit_smart.sh' para commits optimizados"
echo "   - El repositorio será más eficiente y rápido"
echo ""
echo "📚 Más información: https://git-lfs.github.io/"