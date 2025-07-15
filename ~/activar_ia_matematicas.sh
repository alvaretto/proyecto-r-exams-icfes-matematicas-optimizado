#!/bin/bash

# 🧮 ACTIVADOR DE HERRAMIENTAS IA PARA MATEMÁTICAS
# ===============================================
# 
# Script para activar entorno con herramientas de IA matemáticas
# Optimizado para GTX 1050 (2GB VRAM)

# Colores para output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${BLUE}🧮 Activando herramientas IA para matemáticas...${NC}"

# Verificar si conda está disponible
if command -v conda &> /dev/null; then
    # Intentar activar entorno conda si existe
    if conda env list | grep -q "pix2text\|math\|ia"; then
        # Buscar entorno apropiado
        ENV_NAME=$(conda env list | grep -E "pix2text|math|ia" | head -1 | awk '{print $1}')
        echo "🔧 Activando entorno conda: $ENV_NAME"
        eval "$(conda shell.bash hook)"
        conda activate "$ENV_NAME" 2>/dev/null || echo "⚠️  Entorno conda no disponible, continuando..."
    fi
fi

# Verificar si python está disponible
if command -v python3 &> /dev/null; then
    PYTHON_CMD="python3"
elif command -v python &> /dev/null; then
    PYTHON_CMD="python"
else
    echo "❌ Python no encontrado"
    exit 1
fi

echo -e "${GREEN}✅ Entorno activado!${NC}"
echo
echo -e "${BLUE}📚 Herramientas disponibles:${NC}"
echo "  • pix2text - OCR matemático superior"
echo "  • python - Con todas las librerías IA"
echo
echo -e "${YELLOW}💡 Uso recomendado para GTX 1050:${NC}"
echo "  1. Para fórmulas: pix2text (principal)"
echo "  2. Modo CPU activado (evita problemas VRAM)"
echo
echo "🔧 Para desactivar: deactivate"
echo -e "${YELLOW}⚠️  Configurado para CPU (GTX 1050 = 2GB VRAM)${NC}"

# Configurar variables de entorno para modo CPU
export CUDA_VISIBLE_DEVICES=""
export PYTORCH_CUDA_ALLOC_CONF="max_split_size_mb:512"

# Exportar comando python
export PYTHON_CMD="$PYTHON_CMD"
