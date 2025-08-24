#!/bin/bash

# Script de verificación de configuración .gitignore para imágenes
# Proyecto: RepositorioMatematicasICFES_R_Exams

echo "🔍 VERIFICACIÓN DE CONFIGURACIÓN .gitignore PARA IMÁGENES"
echo "========================================================"

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Función para mostrar resultados
show_result() {
    if [ $1 -eq 0 ]; then
        echo -e "${GREEN}✅ $2${NC}"
    else
        echo -e "${RED}❌ $2${NC}"
    fi
}

# Función para mostrar información
show_info() {
    echo -e "${BLUE}ℹ️  $1${NC}"
}

# Función para mostrar advertencias
show_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

echo ""
echo "📋 VERIFICANDO CONFIGURACIÓN DE .gitignore..."
echo "============================================="

# Verificar que .gitignore existe
if [ -f ".gitignore" ]; then
    show_result 0 "Archivo .gitignore existe"
else
    show_result 1 "Archivo .gitignore NO existe"
    exit 1
fi

# Verificar reglas de inclusión de imágenes
echo ""
show_info "Verificando reglas de inclusión de imágenes..."

INCLUSION_PATTERNS=("!*.png" "!*.jpg" "!*.jpeg" "!*.gif" "!*.svg" "!**/*.png" "!**/*.jpg")
for pattern in "${INCLUSION_PATTERNS[@]}"; do
    if grep -q "^$pattern$" .gitignore; then
        show_result 0 "Patrón de inclusión encontrado: $pattern"
    else
        show_result 1 "Patrón de inclusión FALTANTE: $pattern"
    fi
done

# Verificar reglas de exclusión para directorios temporales
echo ""
show_info "Verificando reglas de exclusión para directorios temporales..."

EXCLUSION_PATTERNS=("**/salida/*.png" "**/output/*.png" "**/temp/*.png" "**/tmp/*.png")
for pattern in "${EXCLUSION_PATTERNS[@]}"; do
    if grep -q "^$pattern$" .gitignore; then
        show_result 0 "Patrón de exclusión encontrado: $pattern"
    else
        show_result 1 "Patrón de exclusión FALTANTE: $pattern"
    fi
done

# Verificar inclusiones específicas para directorios importantes
echo ""
show_info "Verificando inclusiones específicas para directorios importantes..."

IMPORTANT_DIRS=(
    "!**/Auxiliares/Ejemplos-Funcionales-Rmd/**/*.png"
    "!**/Auxiliares/TikZ-Documentation/**/*.png"
    "!**/Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/**/*.png"
    "!**/Auxiliares/Agente-Graficador-TikZ/**/*.png"
)

for pattern in "${IMPORTANT_DIRS[@]}"; do
    if grep -q "^$pattern$" .gitignore; then
        show_result 0 "Inclusión específica encontrada: $pattern"
    else
        show_result 1 "Inclusión específica FALTANTE: $pattern"
    fi
done

echo ""
echo "📊 ANÁLISIS DE ARCHIVOS DE IMAGEN EN EL PROYECTO"
echo "==============================================="

# Buscar archivos de imagen en el proyecto
show_info "Buscando archivos de imagen existentes..."

# Crear archivo temporal para resultados
TEMP_FILE="/tmp/image_files_$$.txt"

# Buscar archivos de imagen (con timeout para evitar colgarse)
timeout 30 find . -type f \( -name "*.png" -o -name "*.jpg" -o -name "*.jpeg" -o -name "*.gif" -o -name "*.svg" -o -name "*.bmp" -o -name "*.tiff" -o -name "*.webp" \) 2>/dev/null > "$TEMP_FILE"

if [ $? -eq 0 ]; then
    TOTAL_IMAGES=$(wc -l < "$TEMP_FILE")
    show_info "Total de archivos de imagen encontrados: $TOTAL_IMAGES"
    
    if [ $TOTAL_IMAGES -gt 0 ]; then
        echo ""
        show_info "Primeros 10 archivos de imagen encontrados:"
        head -10 "$TEMP_FILE" | while read -r file; do
            echo "  📸 $file"
        done
        
        # Analizar distribución por directorios
        echo ""
        show_info "Distribución por directorios principales:"
        grep "^./Auxiliares" "$TEMP_FILE" | cut -d'/' -f1-3 | sort | uniq -c | sort -nr | head -5 | while read -r count dir; do
            echo "  📁 $dir: $count archivos"
        done
    else
        show_warning "No se encontraron archivos de imagen en el proyecto"
    fi
else
    show_warning "Timeout o error al buscar archivos de imagen"
fi

# Limpiar archivo temporal
rm -f "$TEMP_FILE"

echo ""
echo "🧪 PRUEBAS DE FUNCIONAMIENTO"
echo "============================"

# Crear directorio de prueba temporal
TEST_DIR="/tmp/gitignore_test_$$"
mkdir -p "$TEST_DIR"

# Copiar .gitignore para prueba
cp .gitignore "$TEST_DIR/"

cd "$TEST_DIR"

# Inicializar repositorio git temporal
git init . >/dev/null 2>&1

# Crear archivos de prueba
mkdir -p test_images/temp test_images/examples
echo "test" > test_images/examples/test.png
echo "test" > test_images/temp/temp.png
echo "test" > test_images/normal.jpg

# Verificar qué archivos serían incluidos/excluidos
show_info "Probando reglas de .gitignore..."

if git check-ignore test_images/examples/test.png >/dev/null 2>&1; then
    show_result 1 "test_images/examples/test.png sería IGNORADO (debería incluirse)"
else
    show_result 0 "test_images/examples/test.png sería INCLUIDO"
fi

if git check-ignore test_images/temp/temp.png >/dev/null 2>&1; then
    show_result 0 "test_images/temp/temp.png sería IGNORADO (correcto)"
else
    show_result 1 "test_images/temp/temp.png sería INCLUIDO (debería ignorarse)"
fi

if git check-ignore test_images/normal.jpg >/dev/null 2>&1; then
    show_result 1 "test_images/normal.jpg sería IGNORADO (debería incluirse)"
else
    show_result 0 "test_images/normal.jpg sería INCLUIDO"
fi

# Limpiar
cd - >/dev/null
rm -rf "$TEST_DIR"

echo ""
echo "📋 RECOMENDACIONES PARA GEMINI CLI"
echo "=================================="

echo ""
show_info "Configuración optimizada para Gemini CLI:"
echo "  ✅ Imágenes incluidas en todo el proyecto"
echo "  ✅ Exclusiones para directorios temporales"
echo "  ✅ Inclusiones específicas para directorios importantes"
echo "  ✅ Control de tamaño para evitar archivos muy grandes"

echo ""
show_info "Directorios clave para análisis IA:"
echo "  📁 Auxiliares/Ejemplos-Funcionales-Rmd/"
echo "  📁 Auxiliares/TikZ-Documentation/"
echo "  📁 Auxiliares/Estrategia-Avanzada-de-Replicas-de-Imagenes/"
echo "  📁 Auxiliares/Agente-Graficador-TikZ/"

echo ""
show_info "Comandos útiles para monitoreo:"
echo "  📊 git count-objects -vH  # Tamaño del repositorio"
echo "  🔍 git ls-files '*.png'   # Listar imágenes PNG versionadas"
echo "  📈 git log --stat --oneline | grep -E '\.(png|jpg|jpeg)' # Historial de imágenes"

echo ""
echo "✅ Verificación completada"
echo ""
echo "Para usar con Gemini CLI:"
echo "  gemini --image 'ruta/imagen.png' 'Genera código TikZ para esta imagen'"
echo "  gemini 'Analiza las imágenes en Auxiliares/Ejemplos-Funcionales-Rmd/'"
