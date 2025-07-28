# PDF-Chat-Bot 🤖📄

Sistema RAG (Retrieval Augmented Generation) para análisis inteligente de documentos PDF usando N8N, Claude API y búsqueda vectorial.

## 🏗️ Arquitectura del Sistema

```
PDF-Chat-Bot/
├── workflows/              # Workflows de N8N
│   ├── main-workflow.json  # Workflow principal
│   ├── file-processor.json # Procesamiento de archivos
│   └── chatbot-api.json    # API del chatbot
├── config/                 # Configuraciones
│   ├── .env                # Variables de entorno
│   ├── database.sql        # Schema de BD
│   └── qdrant-config.yaml  # Configuración Qdrant
├── scripts/                # Scripts de utilidad
│   ├── setup.sh           # Instalación inicial
│   ├── start-services.sh  # Iniciar servicios
│   └── backup.sh          # Respaldo de datos
├── web/                    # Interface web
│   ├── index.html         # Chatbot interface
│   ├── styles.css         # Estilos
│   └── app.js             # Lógica frontend
├── data/                   # Datos del sistema
│   ├── documents/         # PDFs e imágenes
│   │   ├── matematicas/   # Subcarpeta ejemplo
│   │   └── examenes/      # Subcarpeta ejemplo
│   ├── processed/         # Archivos procesados
│   └── backups/           # Respaldos
├── logs/                   # Logs del sistema
└── docs/                   # Documentación
    ├── installation.md    # Guía de instalación
    ├── configuration.md   # Configuración
    └── api.md             # Documentación API
```

## 🚀 Características Principales

- ✅ **Monitoreo automático** de carpetas y subcarpetas
- ✅ **Procesamiento inteligente** de PDFs e imágenes con OCR
- ✅ **Búsqueda semántica** con embeddings vectoriales
- ✅ **Chatbot RAG** con Claude API para análisis complejo
- ✅ **Historial completo** de conversaciones
- ✅ **Interface web** responsive
- ✅ **Base de datos** PostgreSQL para metadatos
- ✅ **Qdrant** para búsqueda vectorial

## 📋 Requisitos del Sistema

### Software Necesario:
- **N8N** (v1.0+)
- **PostgreSQL** (v13+)
- **Qdrant** (v1.7+)
- **Node.js** (v18+)
- **Docker** (opcional, recomendado)

### APIs Requeridas:
- **Anthropic API** (Claude)
- **OpenAI API** (Embeddings)
- **OCR.space API** (OCR para imágenes)

## 🔧 Instalación Rápida

```bash
# 1. Clonar estructura del proyecto
cd "/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/Auxiliares/Instalaciones/N8N/workflows/PDF-Chat-Bot"

# 2. Ejecutar script de instalación
chmod +x scripts/setup.sh
./scripts/setup.sh

# 3. Configurar variables de entorno
cp config/.env.example config/.env
# Editar config/.env con tus API keys

# 4. Inicializar base de datos
psql -U postgres -d pdf_chatbot -f config/database.sql

# 5. Importar workflows a N8N
# Importar workflows/*.json desde la interfaz de N8N

# 6. Iniciar servicios
./scripts/start-services.sh
```

## 📊 Rutas de Datos

### Carpetas de Documentos:
- **Principal**: `data/documents/`
- **Matemáticas**: `data/documents/matematicas/`
- **Exámenes**: `data/documents/examenes/`

### Formatos Soportados:
- **PDFs**: `.pdf`
- **Imágenes**: `.jpg`, `.jpeg`, `.png`

## 🔗 Endpoints API

- **Chatbot**: `POST /webhook/chatbot`
- **Upload**: `POST /webhook/upload`
- **History**: `GET /webhook/history/{user_id}`
- **Status**: `GET /webhook/status`

## 📱 Acceso Web

Interface del chatbot disponible en:
`http://localhost:5678/webhook/web/chatbot`

## 🛠️ Configuración

Ver documentación detallada en:
- [Instalación](docs/installation.md)
- [Configuración](docs/configuration.md)
- [API](docs/api.md)

## 📈 Monitoreo

- **Logs**: `logs/`
- **Métricas**: Dashboard N8N
- **Estado**: Endpoint `/webhook/status`

## 🔒 Seguridad

- Variables de entorno para API keys
- Validación de tipos de archivo
- Límites de tamaño de archivo
- Sanitización de inputs

## 📞 Soporte

Para problemas o preguntas, revisar:

1. Logs en `logs/`
2. Documentación en `docs/`
3. Estado de servicios con `./scripts/status.sh`

---

**Versión**: 1.0.0  
**Última actualización**: $(date)  
**Autor**: Sistema PDF-Chat-Bot
