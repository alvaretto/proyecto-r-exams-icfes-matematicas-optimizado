#!/bin/bash
# ============================================================================
# SCRIPT: Copiar demos y recursos a carpeta docs/ para GitHub Pages
# FECHA: Diciembre 2025
# ============================================================================

echo "╔════════════════════════════════════════════════════════╗"
echo "║  COPIA DE ARCHIVOS A DOCS/ PARA GITHUB PAGES          ║"
echo "╚════════════════════════════════════════════════════════╝"
echo ""

# Directorio base
BASE_DIR="/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
EJERCICIO_DIR="$BASE_DIR/A-Produccion/En-Desarrollo/consumo_telefonico_adicional"

# Verificar que estamos en el directorio correcto
if [ ! -d "$BASE_DIR" ]; then
    echo "❌ ERROR: Directorio base no encontrado: $BASE_DIR"
    exit 1
fi

cd "$BASE_DIR"

# Crear estructura de directorios en docs/
echo "📁 Creando estructura de directorios..."
mkdir -p docs/demos
mkdir -p docs/recursos
mkdir -p docs/assets/css
mkdir -p docs/assets/js
mkdir -p docs/assets/img

echo "✓ Directorios creados"
echo ""

# Copiar demos HTML
echo "📊 Copiando demos HTML..."
if [ -d "$EJERCICIO_DIR/Estrategia-LinkedIn/demos-html" ]; then
    cp -v "$EJERCICIO_DIR/Estrategia-LinkedIn/demos-html"/*.html docs/demos/
    echo "✓ Demos HTML copiados"
else
    echo "⚠️  ADVERTENCIA: No se encontró directorio de demos HTML"
fi
echo ""

# Copiar recursos descargables
echo "📥 Copiando recursos descargables..."
if [ -d "$EJERCICIO_DIR/Estrategia-LinkedIn/recursos-descargables" ]; then
    # Copiar PDFs
    if ls "$EJERCICIO_DIR/Estrategia-LinkedIn/recursos-descargables"/*.pdf 1> /dev/null 2>&1; then
        cp -v "$EJERCICIO_DIR/Estrategia-LinkedIn/recursos-descargables"/*.pdf docs/recursos/
        echo "✓ PDFs copiados"
    fi
    
    # Copiar XMLs
    if ls "$EJERCICIO_DIR/Estrategia-LinkedIn/recursos-descargables"/*.xml 1> /dev/null 2>&1; then
        cp -v "$EJERCICIO_DIR/Estrategia-LinkedIn/recursos-descargables"/*.xml docs/recursos/
        echo "✓ XMLs copiados"
    fi
else
    echo "⚠️  ADVERTENCIA: No se encontró directorio de recursos"
fi
echo ""

# Copiar código fuente .Rmd
echo "💻 Copiando código fuente..."
if [ -f "$EJERCICIO_DIR/consumo_telefonico_adicional_n2_v1.Rmd" ]; then
    cp -v "$EJERCICIO_DIR/consumo_telefonico_adicional_n2_v1.Rmd" docs/recursos/
    echo "✓ Código fuente copiado"
else
    echo "⚠️  ADVERTENCIA: No se encontró archivo .Rmd"
fi
echo ""

# Verificar archivos copiados
echo "╔════════════════════════════════════════════════════════╗"
echo "║  VERIFICACIÓN DE ARCHIVOS COPIADOS                    ║"
echo "╚════════════════════════════════════════════════════════╝"
echo ""

echo "📊 Demos HTML en docs/demos/:"
ls -lh docs/demos/*.html 2>/dev/null | awk '{print "  " $9 " (" $5 ")"}'
echo ""

echo "📥 Recursos en docs/recursos/:"
ls -lh docs/recursos/* 2>/dev/null | awk '{print "  " $9 " (" $5 ")"}'
echo ""

# Resumen
DEMOS_COUNT=$(ls docs/demos/*.html 2>/dev/null | wc -l)
RECURSOS_COUNT=$(ls docs/recursos/* 2>/dev/null | wc -l)

echo "╔════════════════════════════════════════════════════════╗"
echo "║  RESUMEN                                               ║"
echo "╚════════════════════════════════════════════════════════╝"
echo ""
echo "  ✅ Demos HTML copiados: $DEMOS_COUNT"
echo "  ✅ Recursos copiados: $RECURSOS_COUNT"
echo ""

if [ $DEMOS_COUNT -eq 5 ]; then
    echo "✨ ¡Archivos listos para GitHub Pages!"
    echo ""
    echo "🎯 PRÓXIMOS PASOS:"
    echo "1. Revisar que docs/index.html existe"
    echo "2. Commit y push a rama gh-pages:"
    echo "   git checkout gh-pages"
    echo "   git add docs/"
    echo "   git commit -m '🚀 Demos interactivos - Consumo Telefónico'"
    echo "   git push origin gh-pages"
    echo "3. Configurar GitHub Pages en Settings → Pages"
    echo "4. Esperar deployment (2-3 minutos)"
    echo "5. Verificar URL: https://alvaretto.github.io/proyecto-r-exams-icfes-matematicas-optimizado/"
    echo ""
else
    echo "⚠️  ADVERTENCIA: Se esperaban 5 demos HTML, se copiaron $DEMOS_COUNT"
    echo "Revisar generación de demos"
    echo ""
fi

