#!/bin/bash

# ============================================================================
# Script de Instalación y Configuración de AnythingLLM para ICFES R-Exams
# ============================================================================
# Uso: ./03-setup-anythingllm.sh
# Descripción: Instala y configura AnythingLLM con el workspace ICFES R-Exams
# ============================================================================

set -e  # Salir si hay errores

# Colores
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Variables
PROYECTO_ROOT="/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
ANYTHINGLLM_DATA="$HOME/anythingllm-data"
CONTAINER_NAME="anythingllm-icfes"
PORT=3001

echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║  INSTALACIÓN DE ANYTHINGLLM PARA ICFES R-EXAMS            ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
echo ""

# ============================================================================
# 1. VERIFICAR DOCKER
# ============================================================================
echo -e "${BLUE}[1/6]${NC} Verificando Docker..."

if ! command -v docker &> /dev/null; then
    echo -e "${RED}❌ Docker no está instalado${NC}"
    echo ""
    echo "Instalando Docker..."
    
    # Instalar Docker en Manjaro
    sudo pacman -S docker docker-compose --noconfirm
    sudo systemctl start docker
    sudo systemctl enable docker
    sudo usermod -aG docker $USER
    
    echo -e "${GREEN}✅ Docker instalado${NC}"
    echo -e "${YELLOW}⚠️  Necesitas cerrar sesión y volver a entrar para que los cambios surtan efecto${NC}"
    echo ""
    read -p "Presiona Enter para continuar después de reiniciar sesión..."
else
    echo -e "${GREEN}✅ Docker está instalado${NC}"
fi

# Verificar que Docker esté corriendo
if ! docker info &> /dev/null; then
    echo -e "${YELLOW}⚠️  Docker no está corriendo. Iniciando...${NC}"
    sudo systemctl start docker
    sleep 2
fi

echo ""

# ============================================================================
# 2. CREAR DIRECTORIO DE DATOS
# ============================================================================
echo -e "${BLUE}[2/6]${NC} Creando directorio de datos..."

if [ -d "$ANYTHINGLLM_DATA" ]; then
    echo -e "${YELLOW}⚠️  Directorio ya existe: $ANYTHINGLLM_DATA${NC}"
else
    mkdir -p "$ANYTHINGLLM_DATA"
    echo -e "${GREEN}✅ Directorio creado: $ANYTHINGLLM_DATA${NC}"
fi

echo ""

# ============================================================================
# 3. DETENER CONTENEDOR EXISTENTE (SI EXISTE)
# ============================================================================
echo -e "${BLUE}[3/6]${NC} Verificando contenedor existente..."

if docker ps -a | grep -q "$CONTAINER_NAME"; then
    echo -e "${YELLOW}⚠️  Deteniendo y eliminando contenedor existente...${NC}"
    docker stop "$CONTAINER_NAME" 2>/dev/null || true
    docker rm "$CONTAINER_NAME" 2>/dev/null || true
    echo -e "${GREEN}✅ Contenedor anterior eliminado${NC}"
else
    echo -e "${GREEN}✅ No hay contenedor existente${NC}"
fi

echo ""

# ============================================================================
# 4. DESCARGAR IMAGEN DE ANYTHINGLLM
# ============================================================================
echo -e "${BLUE}[4/6]${NC} Descargando imagen de AnythingLLM..."

docker pull mintplexlabs/anythingllm:latest

echo -e "${GREEN}✅ Imagen descargada${NC}"
echo ""

# ============================================================================
# 5. EJECUTAR CONTENEDOR
# ============================================================================
echo -e "${BLUE}[5/6]${NC} Ejecutando contenedor de AnythingLLM..."

docker run -d \
  --name "$CONTAINER_NAME" \
  -p $PORT:3001 \
  -v "$ANYTHINGLLM_DATA":/app/server/storage \
  -v "$PROYECTO_ROOT":/workspace:ro \
  -e STORAGE_DIR=/app/server/storage \
  --restart unless-stopped \
  mintplexlabs/anythingllm:latest

echo -e "${GREEN}✅ Contenedor ejecutándose${NC}"
echo ""

# Esperar a que el contenedor esté listo
echo -e "${YELLOW}⏳ Esperando a que AnythingLLM esté listo...${NC}"
sleep 10

# ============================================================================
# 6. VERIFICAR INSTALACIÓN
# ============================================================================
echo -e "${BLUE}[6/6]${NC} Verificando instalación..."

if docker ps | grep -q "$CONTAINER_NAME"; then
    echo -e "${GREEN}✅ AnythingLLM está corriendo${NC}"
else
    echo -e "${RED}❌ Error: AnythingLLM no está corriendo${NC}"
    exit 1
fi

echo ""
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo -e "${GREEN}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${GREEN}║  ✅ INSTALACIÓN COMPLETADA EXITOSAMENTE                    ║${NC}"
echo -e "${GREEN}╚════════════════════════════════════════════════════════════╝${NC}"
echo ""
echo -e "${YELLOW}📋 INFORMACIÓN DE ACCESO:${NC}"
echo ""
echo -e "  🌐 URL: ${GREEN}http://localhost:$PORT${NC}"
echo -e "  📁 Datos: ${GREEN}$ANYTHINGLLM_DATA${NC}"
echo -e "  📂 Proyecto: ${GREEN}$PROYECTO_ROOT${NC}"
echo ""
echo -e "${YELLOW}📝 PRÓXIMOS PASOS:${NC}"
echo ""
echo "  1. Abrir navegador en http://localhost:$PORT"
echo "  2. Crear cuenta de administrador"
echo "  3. Crear workspace 'ICFES R-Exams'"
echo "  4. Cargar documentación del proyecto"
echo "  5. Configurar agentes especializados"
echo ""
echo -e "${YELLOW}📚 DOCUMENTACIÓN:${NC}"
echo ""
echo "  - Guía completa: AnythingLLM-Config/01-README_AnythingLLM_ICFES.md"
echo "  - Configuración: AnythingLLM-Config/02-workspace-config.json"
echo ""
echo -e "${YELLOW}🔧 COMANDOS ÚTILES:${NC}"
echo ""
echo "  Ver logs:      docker logs -f $CONTAINER_NAME"
echo "  Detener:       docker stop $CONTAINER_NAME"
echo "  Iniciar:       docker start $CONTAINER_NAME"
echo "  Reiniciar:     docker restart $CONTAINER_NAME"
echo "  Eliminar:      docker stop $CONTAINER_NAME && docker rm $CONTAINER_NAME"
echo ""
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""

