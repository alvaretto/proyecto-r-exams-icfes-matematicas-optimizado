#!/bin/bash

# ========================================
# PDF-Chat-Bot - Script de Inicio de Servicios
# ========================================

set -e

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Variables del proyecto
PROJECT_DIR="/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/Auxiliares/Instalaciones/N8N/workflows/PDF-Chat-Bot"

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}  PDF-Chat-Bot - Iniciando Servicios${NC}"
echo -e "${BLUE}========================================${NC}"

# Funciones de logging
log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Verificar directorio del proyecto
if [ ! -f "$PROJECT_DIR/config/.env" ]; then
    log_error "No se encontró el archivo .env en $PROJECT_DIR/config/"
    log_error "Ejecuta primero: ./setup.sh"
    exit 1
fi

cd "$PROJECT_DIR"

# Cargar variables de entorno
source config/.env

log_info "Cargando configuración del proyecto..."

# 1. Verificar y crear directorios necesarios
log_info "Verificando directorios..."

mkdir -p logs
mkdir -p data/qdrant_storage
mkdir -p data/documents/{matematicas,examenes}
mkdir -p data/processed
mkdir -p data/backups

# 2. Iniciar PostgreSQL
log_info "Verificando PostgreSQL..."

if ! systemctl is-active --quiet postgresql; then
    log_warn "PostgreSQL no está ejecutándose. Intentando iniciar..."
    if sudo systemctl start postgresql; then
        log_info "PostgreSQL iniciado exitosamente"
    else
        log_error "No se pudo iniciar PostgreSQL"
        exit 1
    fi
else
    log_info "PostgreSQL ya está ejecutándose"
fi

# Verificar conexión a la base de datos
if ! PGPASSWORD="$DB_PASSWORD" psql -h "$DB_HOST" -U "$DB_USER" -d "$DB_NAME" -c "SELECT 1;" > /dev/null 2>&1; then
    log_error "No se puede conectar a la base de datos"
    log_error "Verifica las credenciales en config/.env"
    exit 1
fi

log_info "Conexión a PostgreSQL verificada"

# 3. Iniciar Qdrant
log_info "Verificando Qdrant..."

# Verificar si el contenedor existe
if docker ps -a | grep -q pdf-chatbot-qdrant; then
    # El contenedor existe, verificar si está ejecutándose
    if docker ps | grep -q pdf-chatbot-qdrant; then
        log_info "Qdrant ya está ejecutándose"
    else
        log_info "Iniciando contenedor Qdrant existente..."
        docker start pdf-chatbot-qdrant
    fi
else
    # Crear y ejecutar nuevo contenedor
    log_info "Creando y ejecutando nuevo contenedor Qdrant..."
    docker run -d \
        --name pdf-chatbot-qdrant \
        -p 6333:6333 \
        -p 6334:6334 \
        -v "$PROJECT_DIR/data/qdrant_storage:/qdrant/storage" \
        -v "$PROJECT_DIR/config/qdrant-config.yaml:/qdrant/config/production.yaml" \
        qdrant/qdrant:latest
fi

# Esperar a que Qdrant esté listo
log_info "Esperando a que Qdrant esté listo..."
for i in {1..30}; do
    if curl -s http://localhost:6333/health > /dev/null 2>&1; then
        log_info "Qdrant está listo"
        break
    fi
    if [ $i -eq 30 ]; then
        log_error "Timeout esperando a Qdrant"
        exit 1
    fi
    sleep 2
done

# 4. Crear colección en Qdrant si no existe
log_info "Configurando colección de documentos en Qdrant..."

# Verificar si la colección existe
if ! curl -s "http://localhost:6333/collections/documents" | grep -q "documents"; then
    log_info "Creando colección 'documents' en Qdrant..."
    
    curl -X PUT "http://localhost:6333/collections/documents" \
        -H "Content-Type: application/json" \
        -d '{
            "vectors": {
                "size": 1536,
                "distance": "Cosine"
            },
            "optimizers_config": {
                "default_segment_number": 0,
                "max_segment_size_kb": 200000,
                "memmap_threshold_kb": 200000,
                "indexing_threshold_kb": 20000,
                "flush_interval_sec": 5,
                "max_optimization_threads": 1
            },
            "hnsw_config": {
                "m": 16,
                "ef_construct": 100,
                "full_scan_threshold": 10000
            }
        }' > /dev/null 2>&1
    
    if [ $? -eq 0 ]; then
        log_info "Colección 'documents' creada exitosamente"
    else
        log_error "Error creando colección en Qdrant"
        exit 1
    fi
else
    log_info "Colección 'documents' ya existe en Qdrant"
fi

# 5. Verificar APIs externas
log_info "Verificando APIs externas..."

# Verificar API de Anthropic
if [ -n "$ANTHROPIC_API_KEY" ] && [ "$ANTHROPIC_API_KEY" != "your_anthropic_api_key_here" ]; then
    log_info "API key de Anthropic configurada"
else
    log_warn "API key de Anthropic no configurada en .env"
fi

# Verificar API de OpenAI
if [ -n "$OPENAI_API_KEY" ] && [ "$OPENAI_API_KEY" != "your_openai_api_key_here" ]; then
    log_info "API key de OpenAI configurada"
else
    log_warn "API key de OpenAI no configurada en .env"
fi

# 6. Crear archivos de log si no existen
log_info "Configurando archivos de log..."

touch logs/system.log
touch logs/chatbot.log
touch logs/file-processor.log
touch logs/error.log

# Configurar rotación de logs
cat > logs/logrotate.conf << EOF
$PROJECT_DIR/logs/*.log {
    daily
    missingok
    rotate 7
    compress
    delaycompress
    notifempty
    create 644 $USER $USER
}
EOF

# 7. Verificar N8N
log_info "Verificando N8N..."

if command -v n8n &> /dev/null; then
    log_info "N8N está instalado"
    
    # Verificar si N8N está ejecutándose
    if pgrep -f "n8n" > /dev/null; then
        log_info "N8N ya está ejecutándose"
    else
        log_warn "N8N no está ejecutándose"
        echo "Para iniciar N8N: n8n start"
    fi
else
    log_warn "N8N no está instalado"
    echo "Instalar con: npm install -g n8n"
fi

# 8. Mostrar estado de servicios
echo -e "\n${GREEN}========================================${NC}"
echo -e "${GREEN}  Estado de Servicios${NC}"
echo -e "${GREEN}========================================${NC}"

# PostgreSQL
if systemctl is-active --quiet postgresql; then
    echo -e "PostgreSQL: ${GREEN}✓ Ejecutándose${NC}"
else
    echo -e "PostgreSQL: ${RED}✗ Detenido${NC}"
fi

# Qdrant
if docker ps | grep -q pdf-chatbot-qdrant; then
    echo -e "Qdrant: ${GREEN}✓ Ejecutándose${NC} (Puerto 6333)"
else
    echo -e "Qdrant: ${RED}✗ Detenido${NC}"
fi

# N8N
if pgrep -f "n8n" > /dev/null; then
    echo -e "N8N: ${GREEN}✓ Ejecutándose${NC} (Puerto 5678)"
else
    echo -e "N8N: ${YELLOW}○ No iniciado${NC}"
fi

# 9. Mostrar URLs importantes
echo -e "\n${BLUE}URLs del Sistema:${NC}"
echo "• N8N Interface: http://localhost:5678"
echo "• Qdrant API: http://localhost:6333"
echo "• Chatbot Web: http://localhost:5678/webhook/web/chatbot"
echo "• API Status: http://localhost:5678/webhook/status"

# 10. Mostrar próximos pasos
echo -e "\n${YELLOW}Próximos pasos:${NC}"
echo "1. Importar workflows desde: workflows/*.json"
echo "2. Iniciar N8N si no está ejecutándose: n8n start"
echo "3. Colocar documentos en: data/documents/"
echo "4. Acceder al chatbot en: http://localhost:5678/webhook/web/chatbot"

echo -e "\n${GREEN}¡Servicios iniciados exitosamente!${NC}"

# Crear archivo de estado
cat > .service_status << EOF
LAST_START=$(date)
POSTGRESQL_STATUS=$(systemctl is-active postgresql)
QDRANT_STATUS=$(docker ps --format "table {{.Names}}\t{{.Status}}" | grep pdf-chatbot-qdrant | awk '{print $2}' || echo "Not running")
N8N_STATUS=$(pgrep -f "n8n" > /dev/null && echo "Running" || echo "Not running")
EOF

log_info "Estado de servicios guardado en .service_status"
