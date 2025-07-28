#!/bin/bash

# ========================================
# PDF-Chat-Bot - Script de Backup
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
BACKUP_DIR="$PROJECT_DIR/data/backups"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}  PDF-Chat-Bot - Backup del Sistema${NC}"
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
    exit 1
fi

cd "$PROJECT_DIR"

# Cargar variables de entorno
source config/.env

# Crear directorio de backup
mkdir -p "$BACKUP_DIR"

log_info "Iniciando backup del sistema..."
log_info "Timestamp: $TIMESTAMP"

# 1. Backup de la base de datos PostgreSQL
log_info "Realizando backup de PostgreSQL..."

DB_BACKUP_FILE="$BACKUP_DIR/database_backup_$TIMESTAMP.sql"

if PGPASSWORD="$DB_PASSWORD" pg_dump -h "$DB_HOST" -U "$DB_USER" -d "$DB_NAME" > "$DB_BACKUP_FILE"; then
    log_info "Backup de PostgreSQL completado: $(basename $DB_BACKUP_FILE)"
    
    # Comprimir backup de BD
    gzip "$DB_BACKUP_FILE"
    log_info "Backup de BD comprimido: $(basename $DB_BACKUP_FILE).gz"
else
    log_error "Error en backup de PostgreSQL"
    exit 1
fi

# 2. Backup de Qdrant
log_info "Realizando backup de Qdrant..."

QDRANT_BACKUP_DIR="$BACKUP_DIR/qdrant_backup_$TIMESTAMP"
mkdir -p "$QDRANT_BACKUP_DIR"

# Crear snapshot en Qdrant
if curl -X POST "http://localhost:6333/collections/documents/snapshots" > /dev/null 2>&1; then
    log_info "Snapshot de Qdrant creado"
    
    # Copiar datos de Qdrant
    if [ -d "data/qdrant_storage" ]; then
        cp -r data/qdrant_storage/* "$QDRANT_BACKUP_DIR/"
        log_info "Datos de Qdrant copiados"
    else
        log_warn "Directorio de datos de Qdrant no encontrado"
    fi
else
    log_warn "No se pudo crear snapshot de Qdrant (servicio no disponible)"
fi

# 3. Backup de documentos procesados
log_info "Realizando backup de documentos..."

DOCS_BACKUP_FILE="$BACKUP_DIR/documents_backup_$TIMESTAMP.tar.gz"

if [ -d "data/documents" ] && [ "$(ls -A data/documents)" ]; then
    tar -czf "$DOCS_BACKUP_FILE" -C data documents/
    log_info "Backup de documentos completado: $(basename $DOCS_BACKUP_FILE)"
else
    log_warn "No hay documentos para respaldar"
fi

# 4. Backup de archivos procesados
log_info "Realizando backup de archivos procesados..."

PROCESSED_BACKUP_FILE="$BACKUP_DIR/processed_backup_$TIMESTAMP.tar.gz"

if [ -d "data/processed" ] && [ "$(ls -A data/processed)" ]; then
    tar -czf "$PROCESSED_BACKUP_FILE" -C data processed/
    log_info "Backup de archivos procesados completado: $(basename $PROCESSED_BACKUP_FILE)"
else
    log_warn "No hay archivos procesados para respaldar"
fi

# 5. Backup de configuración
log_info "Realizando backup de configuración..."

CONFIG_BACKUP_FILE="$BACKUP_DIR/config_backup_$TIMESTAMP.tar.gz"

tar -czf "$CONFIG_BACKUP_FILE" \
    --exclude="config/.env" \
    config/ workflows/ scripts/

log_info "Backup de configuración completado: $(basename $CONFIG_BACKUP_FILE)"

# 6. Backup de logs
log_info "Realizando backup de logs..."

LOGS_BACKUP_FILE="$BACKUP_DIR/logs_backup_$TIMESTAMP.tar.gz"

if [ -d "logs" ] && [ "$(ls -A logs)" ]; then
    tar -czf "$LOGS_BACKUP_FILE" logs/
    log_info "Backup de logs completado: $(basename $LOGS_BACKUP_FILE)"
else
    log_warn "No hay logs para respaldar"
fi

# 7. Crear manifiesto del backup
log_info "Creando manifiesto del backup..."

MANIFEST_FILE="$BACKUP_DIR/backup_manifest_$TIMESTAMP.txt"

cat > "$MANIFEST_FILE" << EOF
========================================
PDF-Chat-Bot - Manifiesto de Backup
========================================

Fecha: $(date)
Timestamp: $TIMESTAMP
Usuario: $(whoami)
Hostname: $(hostname)

Archivos incluidos en este backup:
EOF

# Listar archivos del backup
ls -la "$BACKUP_DIR"/*_$TIMESTAMP* >> "$MANIFEST_FILE"

# Agregar información del sistema
cat >> "$MANIFEST_FILE" << EOF

Información del sistema:
- PostgreSQL: $(systemctl is-active postgresql 2>/dev/null || echo "No disponible")
- Qdrant: $(docker ps --format "{{.Status}}" --filter "name=pdf-chatbot-qdrant" 2>/dev/null || echo "No disponible")
- Espacio en disco: $(df -h "$PROJECT_DIR" | tail -1 | awk '{print $4}' || echo "No disponible")

Configuración:
- Base de datos: $DB_NAME@$DB_HOST:$DB_PORT
- Qdrant: $QDRANT_URL
- Documentos: $(find data/documents -type f 2>/dev/null | wc -l || echo "0") archivos

Checksums MD5:
EOF

# Agregar checksums
find "$BACKUP_DIR" -name "*_$TIMESTAMP*" -type f -exec md5sum {} \; >> "$MANIFEST_FILE"

log_info "Manifiesto creado: $(basename $MANIFEST_FILE)"

# 8. Limpiar backups antiguos (mantener últimos 7 días)
log_info "Limpiando backups antiguos..."

find "$BACKUP_DIR" -name "*backup_*" -type f -mtime +7 -delete 2>/dev/null || true
find "$BACKUP_DIR" -name "backup_manifest_*" -type f -mtime +7 -delete 2>/dev/null || true
find "$BACKUP_DIR" -name "qdrant_backup_*" -type d -mtime +7 -exec rm -rf {} \; 2>/dev/null || true

REMAINING_BACKUPS=$(find "$BACKUP_DIR" -name "*backup_*" -type f | wc -l)
log_info "Backups antiguos limpiados. Backups restantes: $REMAINING_BACKUPS"

# 9. Calcular tamaño total del backup
BACKUP_SIZE=$(du -sh "$BACKUP_DIR"/*_$TIMESTAMP* 2>/dev/null | awk '{total+=$1} END {print total}' || echo "0")
log_info "Tamaño total del backup: $(du -sh "$BACKUP_DIR"/*_$TIMESTAMP* 2>/dev/null | awk '{sum+=$1} END {print sum "B"}' || echo "Desconocido")"

# 10. Verificar integridad del backup
log_info "Verificando integridad del backup..."

BACKUP_FILES=$(find "$BACKUP_DIR" -name "*_$TIMESTAMP*" -type f)
CORRUPTED_FILES=0

for file in $BACKUP_FILES; do
    if [[ "$file" == *.gz ]]; then
        if ! gzip -t "$file" 2>/dev/null; then
            log_error "Archivo corrupto: $(basename $file)"
            ((CORRUPTED_FILES++))
        fi
    elif [[ "$file" == *.tar.gz ]]; then
        if ! tar -tzf "$file" > /dev/null 2>&1; then
            log_error "Archivo corrupto: $(basename $file)"
            ((CORRUPTED_FILES++))
        fi
    fi
done

if [ $CORRUPTED_FILES -eq 0 ]; then
    log_info "Verificación de integridad: ✓ Todos los archivos están íntegros"
else
    log_error "Verificación de integridad: ✗ $CORRUPTED_FILES archivos corruptos"
fi

# 11. Resumen final
echo -e "\n${GREEN}========================================${NC}"
echo -e "${GREEN}  Backup Completado${NC}"
echo -e "${GREEN}========================================${NC}"

echo -e "\n${BLUE}Archivos de backup creados:${NC}"
ls -la "$BACKUP_DIR"/*_$TIMESTAMP* 2>/dev/null || echo "No se crearon archivos de backup"

echo -e "\n${BLUE}Ubicación del backup:${NC} $BACKUP_DIR"
echo -e "${BLUE}Manifiesto:${NC} $(basename $MANIFEST_FILE)"

if [ $CORRUPTED_FILES -eq 0 ]; then
    echo -e "\n${GREEN}✓ Backup completado exitosamente${NC}"
    exit 0
else
    echo -e "\n${RED}✗ Backup completado con errores${NC}"
    exit 1
fi
