#!/bin/bash

echo "=== DIAGNÓSTICO R - VSCODE ==="
echo ""

echo "1. Verificando instalación de R..."
if command -v R &> /dev/null; then
    echo "✓ R está instalado: $(R --version | head -n1)"
else
    echo "✗ R no está instalado"
    exit 1
fi

echo ""
echo "2. Verificando Rscript..."
if command -v Rscript &> /dev/null; then
    echo "✓ Rscript está disponible: $(which Rscript)"
else
    echo "✗ Rscript no está disponible"
fi

echo ""
echo "3. Verificando librerías R básicas..."
Rscript -e "
tryCatch({
    cat('✓ R base funciona correctamente\n')
    cat('✓ Versión R:', R.version.string, '\n')
    cat('✓ Directorio de librerías:', .libPaths()[1], '\n')
}, error = function(e) {
    cat('✗ Error en R base:', e$message, '\n')
})
"

echo ""
echo "4. Verificando paquetes esenciales para VSCode..."
Rscript -e "
required_packages <- c('jsonlite', 'httpgd', 'languageserver')
installed_packages <- installed.packages()[,'Package']

for(pkg in required_packages) {
    if(pkg %in% installed_packages) {
        cat('✓', pkg, 'está instalado\n')
    } else {
        cat('✗', pkg, 'NO está instalado\n')
    }
}
"

echo ""
echo "5. Verificando configuración R para VSCode..."
if [ -f ~/.Rprofile ]; then
    echo "✓ Archivo .Rprofile encontrado"
    echo "Contenido relevante:"
    grep -E "(httpgd|languageserver|VSCode)" ~/.Rprofile 2>/dev/null || echo "  No hay configuración específica de VSCode"
else
    echo "⚠ No se encontró archivo .Rprofile"
fi

echo ""
echo "=== SOLUCIONES RECOMENDADAS ==="
echo ""
echo "Para solucionar 'R: (not attached)' en VSCode:"
echo ""
echo "1. Instalar paquetes necesarios:"
echo "   install.packages(c('httpgd', 'languageserver', 'jsonlite'))"
echo ""
echo "2. En VSCode, verificar configuración:"
echo "   - Abrir Command Palette (Ctrl+Shift+P)"
echo "   - Buscar 'R: Create R Terminal'"
echo "   - O usar 'R: Attach Active Terminal'"
echo ""
echo "3. Verificar extensión de R en VSCode:"
echo "   - Extensión 'R' debe estar instalada y habilitada"
echo "   - Reiniciar VSCode después de instalar"
echo ""
echo "4. Configurar ruta de R en VSCode (si es necesario):"
echo "   - Settings > Extensions > R > Rpath > Linux: $(which R)"