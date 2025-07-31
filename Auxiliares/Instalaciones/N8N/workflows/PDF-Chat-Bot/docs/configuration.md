# ⚙️ Guía de Configuración - PDF-Chat-Bot

Esta guía detalla todas las opciones de configuración disponibles para personalizar el comportamiento del sistema.

## 📁 Estructura de Configuración

```
config/
├── .env                 # Variables de entorno principales
├── .env.example        # Plantilla de configuración
├── database.sql        # Schema de base de datos
└── qdrant-config.yaml  # Configuración de Qdrant
```

## 🔧 Variables de Entorno (.env)

### Configuración del Proyecto

```env
# Información básica del proyecto
PROJECT_NAME=PDF-Chat-Bot
PROJECT_VERSION=1.0.0
PROJECT_PATH="/ruta/completa/al/proyecto"
```

### Rutas de Directorios

```env
# Carpetas principales
DOCUMENTS_PATH="${PROJECT_PATH}/data/documents"
PROCESSED_PATH="${PROJECT_PATH}/data/processed"
BACKUPS_PATH="${PROJECT_PATH}/data/backups"
LOGS_PATH="${PROJECT_PATH}/logs"

# Subcarpetas específicas
MATEMATICAS_PATH="${DOCUMENTS_PATH}/matematicas"
EXAMENES_PATH="${DOCUMENTS_PATH}/examenes"
```

### Servicios Locales

#### Ollama (LLM Local)
```env
OLLAMA_HOST=http://localhost:11434
OLLAMA_MODEL=llama3:8b              # o llama3:70b, codellama, etc.
OLLAMA_MAX_TOKENS=2000              # Máximo según modelo
```

**Modelos recomendados:**
- `llama3:8b`: Equilibrio entre velocidad y calidad
- `llama3:70b`: Máxima calidad (requiere más RAM)
- `codellama:7b`: Especializado en código

#### Sentence Transformers (Embeddings Locales)
```env
EMBEDDINGS_MODEL=all-MiniLM-L6-v2   # Modelo local recomendado
EMBEDDINGS_DIMENSIONS=384           # Dimensiones del vector
EMBEDDINGS_DEVICE=cpu               # cpu o cuda
```

**Modelos de embedding disponibles:**
- `all-MiniLM-L6-v2`: Rápido, buena calidad
- `all-mpnet-base-v2`: Mayor precisión
- `paraphrase-multilingual-MiniLM-L12-v2`: Multiidioma

#### Tesseract OCR (Local)
```env
TESSERACT_PATH=/usr/bin/tesseract
OCR_LANGUAGE=spa+eng                # Idiomas: spa, eng, fra, etc.
```

### Base de Datos PostgreSQL

```env
DB_HOST=localhost
DB_PORT=5432
DB_NAME=pdf_chatbot
DB_USER=pdf_chatbot_user
DB_PASSWORD=tu_password_seguro
DB_SSL=false                        # true para conexiones SSL
```

**Configuración avanzada:**
```env
DB_POOL_MIN=2                       # Conexiones mínimas en pool
DB_POOL_MAX=10                      # Conexiones máximas en pool
DB_TIMEOUT=30000                    # Timeout en milisegundos
```

### Qdrant (Base de Datos Vectorial)

```env
QDRANT_HOST=localhost
QDRANT_PORT=6333
QDRANT_COLLECTION=documents
QDRANT_URL="http://${QDRANT_HOST}:${QDRANT_PORT}"
```

**Para Qdrant Cloud:**
```env
QDRANT_CLOUD_URL=https://tu-cluster.qdrant.io
QDRANT_API_KEY=tu_api_key_qdrant_cloud
```

### Procesamiento de Documentos

#### Configuración de Chunking
```env
CHUNK_SIZE=1000                     # Tamaño de chunk en caracteres
CHUNK_OVERLAP=200                   # Solapamiento entre chunks
MAX_CHUNKS_PER_DOCUMENT=100         # Límite de chunks por documento
```

**Recomendaciones por tipo de documento:**
- **Documentos técnicos**: CHUNK_SIZE=800, OVERLAP=150
- **Libros/Textos largos**: CHUNK_SIZE=1200, OVERLAP=250
- **Documentos cortos**: CHUNK_SIZE=600, OVERLAP=100

#### Límites de Archivos
```env
MAX_FILE_SIZE_MB=50                 # Tamaño máximo por archivo
SUPPORTED_EXTENSIONS=pdf,jpg,jpeg,png
```

#### Configuración de Búsqueda
```env
SEARCH_LIMIT=5                      # Número de resultados por búsqueda
SIMILARITY_THRESHOLD=0.7            # Umbral mínimo de similitud (0.0-1.0)
```

### Configuración del Chatbot

```env
# Respuestas
MAX_RESPONSE_LENGTH=2000            # Longitud máxima de respuesta
RESPONSE_LANGUAGE=es                # Idioma de respuestas
INCLUDE_SOURCES=true                # Incluir fuentes en respuestas

# Sesiones
SESSION_TIMEOUT_HOURS=24            # Expiración de sesiones
MAX_CONVERSATIONS_PER_USER=100      # Límite de conversaciones por usuario
```

### Monitoreo y Logs

```env
# Monitoreo de archivos
FILE_MONITOR_INTERVAL=5             # Intervalo en minutos

# Configuración de logs
LOG_LEVEL=info                      # debug, info, warn, error
LOG_MAX_SIZE=10MB                   # Tamaño máximo por archivo de log
LOG_MAX_FILES=5                     # Número de archivos de log a mantener
```

### Seguridad

```env
# Validación de archivos
ENABLE_FILE_VALIDATION=true         # Validar tipos de archivo
SCAN_FOR_MALWARE=false             # Escaneo de malware (requiere ClamAV)

# Rate limiting
RATE_LIMIT_REQUESTS_PER_MINUTE=60   # Límite por minuto
RATE_LIMIT_REQUESTS_PER_HOUR=1000   # Límite por hora
```

### Backup y Mantenimiento

```env
# Configuración de respaldos
BACKUP_ENABLED=true                 # Habilitar backups automáticos
BACKUP_INTERVAL_HOURS=24            # Intervalo entre backups
BACKUP_RETENTION_DAYS=30            # Días a mantener backups
```

### Notificaciones (Opcional)

```env
# Email para notificaciones
NOTIFICATION_EMAIL=admin@example.com
SMTP_HOST=smtp.gmail.com
SMTP_PORT=587
SMTP_USER=tu_email@gmail.com
SMTP_PASSWORD=tu_app_password
```

## 🗄️ Configuración de Base de Datos

### Optimización de PostgreSQL

**Archivo: `/etc/postgresql/13/main/postgresql.conf`**

```conf
# Memoria
shared_buffers = 256MB              # 25% de RAM disponible
effective_cache_size = 1GB          # 75% de RAM disponible
work_mem = 4MB                      # Memoria por operación

# Conexiones
max_connections = 100               # Número máximo de conexiones

# Logging
log_statement = 'mod'               # Log de modificaciones
log_min_duration_statement = 1000   # Log queries > 1 segundo

# Checkpoint
checkpoint_completion_target = 0.9
wal_buffers = 16MB
```

### Índices Personalizados

```sql
-- Índice para búsqueda de texto completo en español
CREATE INDEX idx_document_chunks_spanish_search 
ON document_chunks USING gin(to_tsvector('spanish', content));

-- Índice para búsquedas por fecha
CREATE INDEX idx_messages_created_at_desc 
ON messages (created_at DESC);

-- Índice compuesto para consultas frecuentes
CREATE INDEX idx_conversations_user_session 
ON conversations (user_id, session_id, created_at DESC);
```

## 🔍 Configuración de Qdrant

### Archivo: `config/qdrant-config.yaml`

#### Configuración de Memoria
```yaml
storage:
  # Configuración de memoria
  wal:
    wal_capacity_mb: 64             # Aumentar para mejor rendimiento
    wal_segments_ahead: 2

# Configuración de optimización
optimizer:
  memmap_threshold_kb: 500000       # Umbral para usar memory mapping
  indexing_threshold_kb: 50000      # Umbral para indexación
```

#### Configuración de Colección
```yaml
collections:
  documents:
    vectors:
      size: 1536                    # Debe coincidir con embedding model
      distance: Cosine              # Cosine, Dot, Euclid
    
    # Configuración HNSW para búsqueda rápida
    hnsw_config:
      m: 32                         # Más conexiones = mejor recall
      ef_construct: 200             # Más alto = mejor calidad de índice
      full_scan_threshold: 20000    # Umbral para búsqueda exhaustiva
```

#### Configuración de Performance
```yaml
# Para datasets grandes
collections:
  documents:
    optimizer_config:
      max_segment_size_kb: 500000   # Segmentos más grandes
      memmap_threshold_kb: 500000   # Usar memory mapping
      indexing_threshold_kb: 50000  # Indexar segmentos grandes
      flush_interval_sec: 10        # Flush menos frecuente
```

## 🔄 Configuración de N8N

### Variables de Entorno para N8N

```env
# Configuración básica
N8N_HOST=0.0.0.0
N8N_PORT=5678
N8N_PROTOCOL=http

# Base de datos (opcional, para persistencia)
DB_TYPE=postgresdb
DB_POSTGRESDB_HOST=localhost
DB_POSTGRESDB_PORT=5432
DB_POSTGRESDB_DATABASE=n8n
DB_POSTGRESDB_USER=n8n_user
DB_POSTGRESDB_PASSWORD=n8n_password

# Seguridad
N8N_BASIC_AUTH_ACTIVE=true
N8N_BASIC_AUTH_USER=admin
N8N_BASIC_AUTH_PASSWORD=tu_password_admin

# Logs
N8N_LOG_LEVEL=info
N8N_LOG_OUTPUT=console,file
N8N_LOG_FILE_LOCATION=logs/n8n.log
```

### Configuración de Workflows

#### Trigger de Monitoreo de Archivos
```json
{
  "parameters": {
    "rule": {
      "interval": [{"field": "minutes", "value": 5}]
    }
  }
}
```

**Personalización:**
- Cambiar intervalo según frecuencia de nuevos archivos
- Para alta frecuencia: 1-2 minutos
- Para baja frecuencia: 10-15 minutos

#### Configuración de Chunking
```javascript
// En el nodo de Code para chunking
const chunkSize = parseInt(process.env.CHUNK_SIZE) || 1000;
const overlap = parseInt(process.env.CHUNK_OVERLAP) || 200;
```

## 📊 Configuración de Monitoreo

### Logs Personalizados

**Archivo: `config/logrotate.conf`**
```conf
/ruta/al/proyecto/logs/*.log {
    daily
    missingok
    rotate 14
    compress
    delaycompress
    notifempty
    create 644 usuario grupo
    postrotate
        systemctl reload rsyslog > /dev/null 2>&1 || true
    endscript
}
```

### Métricas y Alertas

```bash
# Script de monitoreo personalizado
#!/bin/bash
# scripts/monitor.sh

# Verificar uso de disco
DISK_USAGE=$(df -h /ruta/al/proyecto | awk 'NR==2 {print $5}' | sed 's/%//')
if [ $DISK_USAGE -gt 80 ]; then
    echo "ALERTA: Uso de disco alto: ${DISK_USAGE}%"
fi

# Verificar memoria de Qdrant
QDRANT_MEMORY=$(docker stats --no-stream pdf-chatbot-qdrant --format "{{.MemUsage}}")
echo "Memoria Qdrant: $QDRANT_MEMORY"

# Verificar conexiones PostgreSQL
PG_CONNECTIONS=$(PGPASSWORD="$DB_PASSWORD" psql -h localhost -U $DB_USER -d $DB_NAME -t -c "SELECT count(*) FROM pg_stat_activity;")
echo "Conexiones PostgreSQL: $PG_CONNECTIONS"
```

## 🔧 Configuración Avanzada

### Configuración Multi-Idioma

```env
# Soporte para múltiples idiomas
SUPPORTED_LANGUAGES=es,en,fr
DEFAULT_LANGUAGE=es
OCR_LANGUAGES=spa,eng,fra
```

### Configuración de Cache

```env
# Redis para cache (opcional)
REDIS_HOST=localhost
REDIS_PORT=6379
REDIS_PASSWORD=tu_redis_password
CACHE_TTL_SECONDS=3600
ENABLE_CACHE=true
```

### Configuración de Clustering

```env
# Para múltiples instancias
CLUSTER_MODE=true
CLUSTER_NODE_ID=node_1
CLUSTER_NODES=node_1,node_2,node_3
```

## 🔄 Aplicar Configuración

### Reiniciar Servicios

```bash
# Reiniciar todos los servicios
./scripts/restart-services.sh

# O reiniciar servicios individuales
docker restart pdf-chatbot-qdrant
sudo systemctl restart postgresql
```

### Validar Configuración

```bash
# Verificar configuración
./scripts/validate-config.sh

# Verificar conectividad
curl http://localhost:6333/health
curl http://localhost:5678/health
```

## 📝 Configuración por Entorno

### Desarrollo
```env
DEBUG_MODE=true
LOG_LEVEL=debug
RATE_LIMIT_REQUESTS_PER_MINUTE=1000
```

### Producción
```env
DEBUG_MODE=false
LOG_LEVEL=warn
ENABLE_SSL=true
BACKUP_ENABLED=true
```

### Testing
```env
DB_NAME=pdf_chatbot_test
QDRANT_COLLECTION=documents_test
LOG_LEVEL=error
```

---

**Configuración completada!** ⚙️

El sistema está ahora personalizado según tus necesidades específicas.
