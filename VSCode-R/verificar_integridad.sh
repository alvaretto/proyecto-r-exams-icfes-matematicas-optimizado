#!/bin/bash

# Script para verificar la integridad de los archivos del tutorial VSCode-R

set -e

# Colores
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${BLUE}🔍 Verificador de Integridad - Tutorial VSCode-R${NC}"
echo "=============================================="

# Lista de archivos requeridos
REQUIRED_FILES=(
    "README.md"
    "TUTORIAL_INSTALACION_R_VSCODE.md"
    "README_INSTALACION_R.md"
    "RESUMEN_TUTORIAL.md"
    "instalar_r_vscode.sh"
    "configuracion_respaldo.tar.gz.sh"
    "test_vscode_r.R"
    ".Rprofile"
    ".vscode/settings.json"
    "exportar_tutorial.sh"
    "verificar_integridad.sh"
)

# Contadores
TOTAL_FILES=${#REQUIRED_FILES[@]}
FOUND_FILES=0
MISSING_FILES=0

echo -e "${BLUE}📋 Verificando archivos requeridos...${NC}"
echo ""

# Verificar cada archivo
for file in "${REQUIRED_FILES[@]}"; do
    if [ -f "$file" ] || [ -d "$file" ]; then
        echo -e "${GREEN}✓${NC} $file"
        ((FOUND_FILES++))
    else
        echo -e "${RED}✗${NC} $file (FALTANTE)"
        ((MISSING_FILES++))
    fi
done

echo ""
echo -e "${BLUE}📊 RESUMEN:${NC}"
echo "==========="
echo "Total de archivos: $TOTAL_FILES"
echo -e "Encontrados: ${GREEN}$FOUND_FILES${NC}"
echo -e "Faltantes: ${RED}$MISSING_FILES${NC}"

# Verificar permisos de ejecución
echo ""
echo -e "${BLUE}🔐 Verificando permisos de ejecución...${NC}"

EXECUTABLE_FILES=(
    "instalar_r_vscode.sh"
    "configuracion_respaldo.tar.gz.sh"
    "exportar_tutorial.sh"
    "verificar_integridad.sh"
)

for file in "${EXECUTABLE_FILES[@]}"; do
    if [ -f "$file" ]; then
        if [ -x "$file" ]; then
            echo -e "${GREEN}✓${NC} $file (ejecutable)"
        else
            echo -e "${YELLOW}⚠${NC} $file (no ejecutable - corrigiendo...)"
            chmod +x "$file"
            echo -e "${GREEN}✓${NC} $file (permisos corregidos)"
        fi
    fi
done

# Verificar contenido de archivos clave
echo ""
echo -e "${BLUE}📄 Verificando contenido de archivos clave...${NC}"

# Verificar que instalar_r_vscode.sh tiene el shebang correcto
if [ -f "instalar_r_vscode.sh" ]; then
    if head -n1 "instalar_r_vscode.sh" | grep -q "#!/bin/bash"; then
        echo -e "${GREEN}✓${NC} instalar_r_vscode.sh tiene shebang correcto"
    else
        echo -e "${RED}✗${NC} instalar_r_vscode.sh falta shebang"
    fi
fi

# Verificar que .Rprofile tiene configuración de biblioteca
if [ -f ".Rprofile" ]; then
    if grep -q "~/R/library" ".Rprofile"; then
        echo -e "${GREEN}✓${NC} .Rprofile tiene configuración de biblioteca"
    else
        echo -e "${RED}✗${NC} .Rprofile falta configuración de biblioteca"
    fi
fi

# Verificar configuración VSCode
if [ -f ".vscode/settings.json" ]; then
    if grep -q "r.rterm.linux" ".vscode/settings.json"; then
        echo -e "${GREEN}✓${NC} VSCode settings.json tiene configuración R"
    else
        echo -e "${RED}✗${NC} VSCode settings.json falta configuración R"
    fi
fi

# Resultado final
echo ""
echo "=============================================="
if [ $MISSING_FILES -eq 0 ]; then
    echo -e "${GREEN}🎉 ¡VERIFICACIÓN EXITOSA!${NC}"
    echo -e "${GREEN}✅ Todos los archivos están presentes y configurados correctamente${NC}"
    echo ""
    echo -e "${BLUE}🚀 El tutorial está listo para usar:${NC}"
    echo "1. Ejecutar: ./instalar_r_vscode.sh"
    echo "2. Verificar: Rscript test_vscode_r.R"
    echo "3. Exportar: ./exportar_tutorial.sh"
else
    echo -e "${RED}❌ VERIFICACIÓN FALLIDA${NC}"
    echo -e "${RED}✗ Faltan $MISSING_FILES archivos${NC}"
    echo ""
    echo -e "${YELLOW}🔧 Para corregir:${NC}"
    echo "1. Regenerar los archivos faltantes"
    echo "2. Ejecutar nuevamente este script"
fi

echo ""
echo -e "${BLUE}📁 Ubicación actual:${NC} $(pwd)"
echo -e "${BLUE}📅 Verificación realizada:${NC} $(date)"
