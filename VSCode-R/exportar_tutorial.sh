#!/bin/bash

# Script para exportar el tutorial VSCode-R a diferentes formatos
# Útil para transferir a otras máquinas o crear respaldos

set -e

# Colores para output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${BLUE}📦 Exportador de Tutorial VSCode-R${NC}"
echo "=================================="

# Obtener directorio actual
CURRENT_DIR=$(pwd)
PARENT_DIR=$(dirname "$CURRENT_DIR")
FOLDER_NAME=$(basename "$CURRENT_DIR")

# Verificar que estamos en la carpeta correcta
if [ "$FOLDER_NAME" != "VSCode-R" ]; then
    echo -e "${YELLOW}⚠ Ejecuta este script desde dentro de la carpeta VSCode-R${NC}"
    exit 1
fi

# Crear nombre con timestamp
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
EXPORT_NAME="VSCode-R_Tutorial_${TIMESTAMP}"

echo -e "${BLUE}[1/4]${NC} Creando archivo comprimido..."

# Ir al directorio padre para comprimir la carpeta completa
cd "$PARENT_DIR"

# Crear archivo tar.gz
tar -czf "${EXPORT_NAME}.tar.gz" VSCode-R/

echo -e "${GREEN}✓${NC} Archivo creado: ${EXPORT_NAME}.tar.gz"

# Crear archivo zip como alternativa
if command -v zip >/dev/null 2>&1; then
    echo -e "${BLUE}[2/4]${NC} Creando archivo ZIP..."
    zip -r "${EXPORT_NAME}.zip" VSCode-R/ >/dev/null
    echo -e "${GREEN}✓${NC} Archivo creado: ${EXPORT_NAME}.zip"
else
    echo -e "${YELLOW}⚠${NC} zip no disponible, solo se creó .tar.gz"
fi

echo -e "${BLUE}[3/4]${NC} Generando instrucciones de uso..."

# Crear archivo de instrucciones
cat > "${EXPORT_NAME}_INSTRUCCIONES.txt" << EOF
📋 INSTRUCCIONES DE USO - Tutorial VSCode-R
==========================================

🎯 CONTENIDO:
- Tutorial completo de instalación R + VSCode
- Scripts de instalación automática
- Archivos de configuración optimizados
- Scripts de verificación y pruebas

🚀 INSTALACIÓN EN NUEVA MÁQUINA:

Opción 1: Desde archivo .tar.gz
-------------------------------
1. Copiar ${EXPORT_NAME}.tar.gz a la nueva máquina
2. Extraer: tar -xzf ${EXPORT_NAME}.tar.gz
3. Entrar: cd VSCode-R
4. Ejecutar: ./instalar_r_vscode.sh
5. Verificar: Rscript test_vscode_r.R

Opción 2: Desde archivo .zip
----------------------------
1. Copiar ${EXPORT_NAME}.zip a la nueva máquina
2. Extraer: unzip ${EXPORT_NAME}.zip
3. Entrar: cd VSCode-R
4. Ejecutar: ./instalar_r_vscode.sh
5. Verificar: Rscript test_vscode_r.R

📚 DOCUMENTACIÓN:
- README.md: Índice principal
- TUTORIAL_INSTALACION_R_VSCODE.md: Tutorial completo
- README_INSTALACION_R.md: Instrucciones rápidas

🔧 DISTRIBUCIONES SOPORTADAS:
- Manjaro/Arch Linux (principal)
- Ubuntu/Debian
- Fedora/CentOS/RHEL
- openSUSE

⚠ REQUISITOS:
- Sistema Linux
- Conexión a internet
- Permisos sudo

🎯 OPTIMIZADO PARA:
- Proyectos r-exams ICFES
- Análisis de datos con R
- Integración Python-R
- Documentos académicos

Generado: $(date)
EOF

echo -e "${GREEN}✓${NC} Instrucciones creadas: ${EXPORT_NAME}_INSTRUCCIONES.txt"

echo -e "${BLUE}[4/4]${NC} Generando resumen de archivos..."

# Mostrar información de los archivos creados
echo ""
echo -e "${GREEN}📦 ARCHIVOS EXPORTADOS:${NC}"
echo "========================"

if [ -f "${EXPORT_NAME}.tar.gz" ]; then
    SIZE_TAR=$(du -h "${EXPORT_NAME}.tar.gz" | cut -f1)
    echo "📁 ${EXPORT_NAME}.tar.gz (${SIZE_TAR})"
fi

if [ -f "${EXPORT_NAME}.zip" ]; then
    SIZE_ZIP=$(du -h "${EXPORT_NAME}.zip" | cut -f1)
    echo "📁 ${EXPORT_NAME}.zip (${SIZE_ZIP})"
fi

echo "📄 ${EXPORT_NAME}_INSTRUCCIONES.txt"

echo ""
echo -e "${GREEN}✅ EXPORTACIÓN COMPLETADA${NC}"
echo "=========================="
echo ""
echo -e "${BLUE}📋 PARA USAR EN OTRA MÁQUINA:${NC}"
echo "1. Copiar los archivos generados"
echo "2. Extraer el archivo comprimido"
echo "3. Seguir las instrucciones del archivo .txt"
echo ""
echo -e "${BLUE}📍 UBICACIÓN:${NC} $(pwd)"

# Volver al directorio original
cd "$CURRENT_DIR"
