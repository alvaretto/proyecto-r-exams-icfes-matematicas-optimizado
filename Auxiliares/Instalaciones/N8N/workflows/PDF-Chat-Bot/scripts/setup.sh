#!/bin/bash

# ========================================
# PDF-Chat-Bot - Script de Instalación
# ========================================

set -e  # Salir si hay errores

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Variables del proyecto
PROJECT_NAME="PDF-Chat-Bot"
PROJECT_DIR="/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/Auxiliares/Instalaciones/N8N/workflows/PDF-Chat-Bot"

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}  $PROJECT_NAME - Instalación Inicial${NC}"
echo -e "${BLUE}========================================${NC}"

# Función para mostrar mensajes
log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Verificar si estamos en el directorio correcto
if [ ! -f "$PROJECT_DIR/README.md" ]; then
    log_error "No se encontró el archivo README.md en $PROJECT_DIR"
    log_error "Asegúrate de ejecutar este script desde el directorio del proyecto"
    exit 1
fi

cd "$PROJECT_DIR"

# 1. Verificar dependencias del sistema
log_info "Verificando dependencias del sistema..."

# Verificar Docker
if ! command -v docker &> /dev/null; then
    log_warn "Docker no está instalado. Se recomienda instalarlo para Qdrant"
    echo "Instalar Docker: https://docs.docker.com/get-docker/"
fi

# Verificar PostgreSQL
if ! command -v psql &> /dev/null; then
    log_warn "PostgreSQL no está instalado"
    echo "Instalar PostgreSQL: sudo apt-get install postgresql postgresql-contrib"
fi

# Verificar Node.js
if ! command -v node &> /dev/null; then
    log_warn "Node.js no está instalado"
    echo "Instalar Node.js: https://nodejs.org/"
fi

# 2. Crear archivo .env si no existe
log_info "Configurando variables de entorno..."

if [ ! -f "config/.env" ]; then
    cp config/.env.example config/.env
    log_info "Archivo .env creado desde .env.example"
    log_warn "¡IMPORTANTE! Edita config/.env con tus API keys antes de continuar"
else
    log_info "Archivo .env ya existe"
fi

# 3. Crear directorios necesarios
log_info "Creando directorios del proyecto..."

mkdir -p data/documents/{matematicas,examenes}
mkdir -p data/processed
mkdir -p data/backups
mkdir -p logs
mkdir -p web/assets

log_info "Directorios creados exitosamente"

# 4. Configurar permisos
log_info "Configurando permisos..."

chmod +x scripts/*.sh
chmod 755 data/
chmod 755 logs/

# 5. Instalar Qdrant con Docker (opcional)
read -p "¿Deseas instalar Qdrant con Docker? (y/n): " install_qdrant

if [ "$install_qdrant" = "y" ] || [ "$install_qdrant" = "Y" ]; then
    log_info "Instalando Qdrant con Docker..."
    
    # Crear directorio para datos de Qdrant
    mkdir -p data/qdrant_storage
    
    # Ejecutar Qdrant
    docker run -d \
        --name pdf-chatbot-qdrant \
        -p 6333:6333 \
        -p 6334:6334 \
        -v "$PROJECT_DIR/data/qdrant_storage:/qdrant/storage" \
        -v "$PROJECT_DIR/config/qdrant-config.yaml:/qdrant/config/production.yaml" \
        qdrant/qdrant:latest
    
    log_info "Qdrant instalado y ejecutándose en puerto 6333"
else
    log_info "Saltando instalación de Qdrant"
fi

# 6. Configurar PostgreSQL (opcional)
read -p "¿Deseas configurar la base de datos PostgreSQL? (y/n): " setup_db

if [ "$setup_db" = "y" ] || [ "$setup_db" = "Y" ]; then
    log_info "Configurando base de datos PostgreSQL..."
    
    # Verificar si PostgreSQL está ejecutándose
    if ! systemctl is-active --quiet postgresql; then
        log_warn "PostgreSQL no está ejecutándose. Iniciando..."
        sudo systemctl start postgresql
    fi
    
    # Crear base de datos y usuario
    sudo -u postgres psql << EOF
CREATE DATABASE pdf_chatbot;
CREATE USER pdf_chatbot_user WITH PASSWORD 'secure_password_123';
GRANT ALL PRIVILEGES ON DATABASE pdf_chatbot TO pdf_chatbot_user;
\q
EOF
    
    # Ejecutar schema
    sudo -u postgres psql -d pdf_chatbot -f config/database.sql
    
    log_info "Base de datos configurada exitosamente"
    log_warn "¡IMPORTANTE! Cambia la contraseña en config/.env"
else
    log_info "Saltando configuración de base de datos"
fi

# 7. Crear archivos de log iniciales
log_info "Creando archivos de log..."

touch logs/system.log
touch logs/chatbot.log
touch logs/file-processor.log
touch logs/error.log

# 8. Verificar instalación de N8N
if ! command -v n8n &> /dev/null; then
    log_warn "N8N no está instalado globalmente"
    echo "Instalar N8N: npm install -g n8n"
fi

# 9. Crear script de inicio rápido
cat > start-quick.sh << 'EOF'
#!/bin/bash
# Script de inicio rápido para PDF-Chat-Bot

echo "Iniciando servicios de PDF-Chat-Bot..."

# Cargar variables de entorno
source config/.env

# Iniciar Qdrant si no está ejecutándose
if ! docker ps | grep -q pdf-chatbot-qdrant; then
    echo "Iniciando Qdrant..."
    docker start pdf-chatbot-qdrant || docker run -d \
        --name pdf-chatbot-qdrant \
        -p 6333:6333 \
        -p 6334:6334 \
        -v "$(pwd)/data/qdrant_storage:/qdrant/storage" \
        qdrant/qdrant:latest
fi

# Verificar PostgreSQL
if ! systemctl is-active --quiet postgresql; then
    echo "Iniciando PostgreSQL..."
    sudo systemctl start postgresql
fi

echo "Servicios iniciados. Puedes iniciar N8N con: n8n start"
echo "Interface web: http://localhost:5678"
EOF

chmod +x start-quick.sh

# 10. Mostrar resumen de instalación
echo -e "\n${GREEN}========================================${NC}"
echo -e "${GREEN}  Instalación Completada${NC}"
echo -e "${GREEN}========================================${NC}"

log_info "Proyecto configurado en: $PROJECT_DIR"
log_info "Archivos de configuración creados"
log_info "Directorios del proyecto creados"

echo -e "\n${YELLOW}Próximos pasos:${NC}"
echo "1. Editar config/.env con tus API keys"
echo "2. Importar workflows/*.json en N8N"
echo "3. Ejecutar: ./start-quick.sh"
echo "4. Iniciar N8N: n8n start"
echo "5. Acceder a: http://localhost:5678"

echo -e "\n${YELLOW}Documentación:${NC}"
echo "- README.md: Información general"
echo "- docs/installation.md: Guía detallada"
echo "- docs/configuration.md: Configuración avanzada"

echo -e "\n${BLUE}¡Instalación completada exitosamente!${NC}"
