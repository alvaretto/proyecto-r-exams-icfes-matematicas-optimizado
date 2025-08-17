#!/bin/bash

# Script para optimizar el contexto de Gemini CLI en el proyecto ICFES R-exams
# Ubicación: Auxiliares/Instalaciones/Ais/Gemini_CLI/
# Autor: Configuración automatizada
# Fecha: $(date)

echo "🎯 OPTIMIZACIÓN DE CONTEXTO GEMINI CLI - PROYECTO ICFES R-EXAMS"
echo "=============================================================="
echo ""

# Colores para output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Función para mostrar resultados
show_result() {
    if [ $1 -eq 0 ]; then
        echo -e "${GREEN}✅ $2${NC}"
    else
        echo -e "${RED}❌ $2${NC}"
    fi
}

# Obtener directorio del script y proyecto
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

echo -e "${BLUE}📁 VERIFICANDO ESTRUCTURA DEL PROYECTO...${NC}"
echo "-------------------------------------------"

# Verificar que estamos en el proyecto correcto
echo -n "Verificando directorio del proyecto: "
if [[ -f "$PROJECT_ROOT/.gitignore" ]] && [[ -d "$PROJECT_ROOT/Auxiliares" ]]; then
    echo -e "${GREEN}✅ $PROJECT_ROOT${NC}"
else
    echo -e "${RED}❌ Directorio de proyecto no válido${NC}"
    exit 1
fi

# Verificar estructura de Auxiliares
echo -n "Verificando estructura Auxiliares: "
if [[ -d "$PROJECT_ROOT/Auxiliares/Ejemplos-Funcionales-Rmd" ]] && [[ -d "$PROJECT_ROOT/Auxiliares/TikZ-Documentation" ]]; then
    echo -e "${GREEN}✅ Estructura correcta${NC}"
else
    echo -e "${YELLOW}⚠️  Estructura incompleta pero continuando${NC}"
fi

echo ""
echo -e "${BLUE}🔧 CREANDO CONFIGURACIÓN OPTIMIZADA...${NC}"
echo "---------------------------------------"

# Crear archivo .geminiignore si no existe
echo -n "Verificando .geminiignore: "
if [[ -f "$PROJECT_ROOT/.geminiignore" ]]; then
    echo -e "${GREEN}✅ Ya existe${NC}"
else
    echo -e "${YELLOW}⚠️  Creando archivo .geminiignore${NC}"
    
    # Crear .geminiignore optimizado
    cat > "$PROJECT_ROOT/.geminiignore" << 'EOF'
# Gemini CLI - Archivos a ignorar para optimizar contexto
# Proyecto: RepositorioMatematicasICFES_R_Exams

# Directorios de sistema y temporales
.git/
.vscode/settings.json
.Rproj.user/
.Rhistory
.RData
.Ruserdata
node_modules/
__pycache__/
*.tmp
*.temp
*.log

# Archivos de salida generados
*.html
*.pdf
*.docx
*.xml
salida/
output/
temp/
temporal/

# Archivos de imagen grandes (mantener solo ejemplos)
*.png
*.jpg
*.jpeg
*.gif
!Auxiliares/Ejemplos-Funcionales-Rmd/**/*.png
!Auxiliares/TikZ-Documentation/**/*.png

# Archivos de datos grandes
*.csv
*.xlsx
*.rds
*.RData

# Archivos de backup
*~
*.bak
*.backup
*.old

# Archivos de LaTeX temporales
*.aux
*.log
*.out
*.synctex.gz

# Archivos de sistema
.DS_Store
Thumbs.db

# INCLUIR archivos importantes
!*.md
!*.Rmd
!*.R
!*.py
!*.sh
!*.json
!Auxiliares/
!Lab/
EOF
    
    show_result $? "Archivo .geminiignore creado"
fi

# Crear directorio de configuración Gemini específico del proyecto
GEMINI_PROJECT_CONFIG="$PROJECT_ROOT/.gemini"
echo -n "Creando configuración específica del proyecto: "
mkdir -p "$GEMINI_PROJECT_CONFIG"

cat > "$GEMINI_PROJECT_CONFIG/config.json" << EOF
{
  "project_name": "RepositorioMatematicasICFES_R_Exams",
  "project_type": "r_exams_mathematics",
  "context_optimization": {
    "max_files": 100,
    "priority_directories": [
      "Auxiliares/Ejemplos-Funcionales-Rmd",
      "Auxiliares/TikZ-Documentation", 
      "Auxiliares/Python-Documentation",
      "Auxiliares/Instalaciones/Ais/Gemini_CLI"
    ],
    "exclude_patterns": [
      "*.html",
      "*.pdf", 
      "*.png",
      "*.jpg",
      ".git/*",
      "node_modules/*",
      "*.log"
    ]
  },
  "default_context_files": [
    "Auxiliares/Instalaciones/Ais/Gemini_CLI/GEMINI.md",
    "Auxiliares/rules_full/rules_full_v1.md"
  ]
}
EOF

show_result $? "Configuración del proyecto creada"

# Crear script de inicio optimizado
echo -n "Actualizando script de inicio: "
cat > "$SCRIPT_DIR/gemini-optimizado.sh" << 'EOF'
#!/bin/bash

# Script de inicio optimizado para Gemini CLI
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

echo "🤖 GEMINI CLI OPTIMIZADO - PROYECTO ICFES R-EXAMS"
echo "================================================="
echo ""

# Verificar directorio del proyecto
if [[ ! -f "$PROJECT_DIR/.geminiignore" ]]; then
    echo "❌ Error: .geminiignore no encontrado. Ejecuta optimizar-contexto-gemini.sh primero"
    exit 1
fi

# Mostrar información del contexto
echo "📁 Directorio del proyecto: $PROJECT_DIR"
echo "🎯 Contexto optimizado con .geminiignore"
echo "📋 Archivos de configuración cargados:"
echo "   • rules-gemini.md"
echo "   • task-list-gemini.md"
echo "   • GEMINI.md"
echo ""

# Cambiar al directorio del proyecto
cd "$PROJECT_DIR"

# Verificar que no estamos en root
if [[ "$PWD" == "/" ]]; then
    echo "⚠️  Error: No se puede ejecutar desde directorio raíz"
    exit 1
fi

echo "📍 Directorio actual: $(pwd)"
echo "🚀 Iniciando Gemini CLI con contexto optimizado..."
echo ""
echo "💡 Para cargar contexto completo, usa:"
echo "   @Auxiliares/Instalaciones/Ais/Gemini_CLI/GEMINI.md"
echo ""

# Iniciar Gemini CLI
gemini
EOF

chmod +x "$SCRIPT_DIR/gemini-optimizado.sh"
show_result $? "Script optimizado creado"

# Actualizar comando global
echo -n "Actualizando comando global: "
if [[ -f "$HOME/.local/bin/gemini-icfes" ]]; then
    ln -sf "$SCRIPT_DIR/gemini-optimizado.sh" "$HOME/.local/bin/gemini-icfes-optimizado"
    show_result $? "Comando gemini-icfes-optimizado disponible"
else
    ln -sf "$SCRIPT_DIR/gemini-optimizado.sh" "$HOME/.local/bin/gemini-icfes-optimizado"
    show_result $? "Comando gemini-icfes-optimizado creado"
fi

echo ""
echo -e "${BLUE}📊 ANÁLISIS DE OPTIMIZACIÓN...${NC}"
echo "------------------------------"

# Contar archivos que serán incluidos/excluidos
cd "$PROJECT_ROOT"

TOTAL_FILES=$(find . -type f | wc -l)
EXCLUDED_FILES=$(find . -name "*.html" -o -name "*.pdf" -o -name "*.png" -o -name "*.jpg" -o -path "./.git/*" | wc -l)
INCLUDED_FILES=$((TOTAL_FILES - EXCLUDED_FILES))

echo "📈 Estadísticas de contexto:"
echo "   • Total de archivos: $TOTAL_FILES"
echo "   • Archivos excluidos: $EXCLUDED_FILES"
echo "   • Archivos incluidos: $INCLUDED_FILES"
echo "   • Reducción de contexto: $(( (EXCLUDED_FILES * 100) / TOTAL_FILES ))%"

echo ""
echo -e "${BLUE}✅ OPTIMIZACIÓN COMPLETADA${NC}"
echo "=========================="
echo ""
echo -e "${GREEN}🎉 CONTEXTO GEMINI CLI OPTIMIZADO EXITOSAMENTE${NC}"
echo ""
echo -e "${CYAN}🚀 Para usar la versión optimizada:${NC}"
echo "  1. Comando optimizado: gemini-icfes-optimizado"
echo "  2. O ejecuta: bash $SCRIPT_DIR/gemini-optimizado.sh"
echo "  3. Carga contexto: @Auxiliares/Instalaciones/Ais/Gemini_CLI/GEMINI.md"
echo ""
echo -e "${CYAN}📋 Beneficios de la optimización:${NC}"
echo "  • ✅ Contexto reducido y enfocado"
echo "  • ✅ Carga más rápida de Gemini CLI"
echo "  • ✅ Mejor rendimiento en análisis"
echo "  • ✅ Eliminación de archivos irrelevantes"
echo ""
echo -e "${YELLOW}⚠️  Nota: Usa 'gemini-icfes-optimizado' en lugar de 'gemini-icfes'${NC}"
echo -e "${GREEN}¡Listo para usar Gemini CLI de manera óptima!${NC}"
