# 🚀 Guía de Instalación - PDF-Chat-Bot

Esta guía te llevará paso a paso por la instalación completa del sistema PDF-Chat-Bot.

## 📋 Requisitos Previos

### Software Necesario

1. **Sistema Operativo**: Linux (Ubuntu 20.04+ recomendado)
2. **Node.js**: v18.0 o superior
3. **PostgreSQL**: v13.0 o superior
4. **Docker**: v20.0 o superior (para Qdrant)
5. **N8N**: v1.0 o superior

### APIs Requeridas

1. **Anthropic API** (Claude)
   - Registrarse en: https://console.anthropic.com/
   - Obtener API key

2. **OpenAI API** (Embeddings)
   - Registrarse en: https://platform.openai.com/
   - Obtener API key

3. **OCR.space API** (OCR para imágenes)
   - Registrarse en: https://ocr.space/ocrapi
   - Obtener API key gratuita

## 🔧 Instalación Paso a Paso

### 1. Preparar el Sistema

```bash
# Actualizar sistema
sudo apt update && sudo apt upgrade -y

# Instalar dependencias básicas
sudo apt install -y curl wget git build-essential
```

### 2. Instalar Node.js

```bash
# Instalar Node.js v18
curl -fsSL https://deb.nodesource.com/setup_18.x | sudo -E bash -
sudo apt-get install -y nodejs

# Verificar instalación
node --version
npm --version
```

### 3. Instalar PostgreSQL

```bash
# Instalar PostgreSQL
sudo apt install -y postgresql postgresql-contrib

# Iniciar servicio
sudo systemctl start postgresql
sudo systemctl enable postgresql

# Verificar instalación
sudo systemctl status postgresql
```

### 4. Instalar Docker

```bash
# Instalar Docker
curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh get-docker.sh

# Agregar usuario al grupo docker
sudo usermod -aG docker $USER

# Reiniciar sesión o ejecutar:
newgrp docker

# Verificar instalación
docker --version
```

### 5. Instalar N8N

```bash
# Instalar N8N globalmente
npm install -g n8n

# Verificar instalación
n8n --version
```

### 6. Configurar el Proyecto

```bash
# Navegar al directorio del proyecto
cd "/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/Auxiliares/Instalaciones/N8N/workflows/PDF-Chat-Bot"

# Ejecutar script de instalación
chmod +x scripts/setup.sh
./scripts/setup.sh
```

### 7. Configurar Variables de Entorno

```bash
# Copiar archivo de configuración
cp config/.env.example config/.env

# Editar configuración
nano config/.env
```

**Configurar las siguientes variables:**

```env
# API Keys (REEMPLAZAR CON TUS CLAVES REALES)
ANTHROPIC_API_KEY=tu_clave_anthropic_aqui
OPENAI_API_KEY=tu_clave_openai_aqui
OCR_SPACE_API_KEY=tu_clave_ocr_space_aqui

# Base de datos
DB_PASSWORD=tu_password_seguro_aqui
```

### 8. Configurar PostgreSQL

```bash
# Conectar como usuario postgres
sudo -u postgres psql

# Crear base de datos y usuario
CREATE DATABASE pdf_chatbot;
CREATE USER pdf_chatbot_user WITH PASSWORD 'tu_password_seguro_aqui';
GRANT ALL PRIVILEGES ON DATABASE pdf_chatbot TO pdf_chatbot_user;
\q

# Ejecutar schema de base de datos
sudo -u postgres psql -d pdf_chatbot -f config/database.sql
```

### 9. Configurar Qdrant

```bash
# Crear directorio para datos
mkdir -p data/qdrant_storage

# Ejecutar Qdrant con Docker
docker run -d \
  --name pdf-chatbot-qdrant \
  -p 6333:6333 \
  -p 6334:6334 \
  -v "$(pwd)/data/qdrant_storage:/qdrant/storage" \
  -v "$(pwd)/config/qdrant-config.yaml:/qdrant/config/production.yaml" \
  qdrant/qdrant:latest

# Verificar que esté ejecutándose
docker ps | grep qdrant
```

### 10. Importar Workflows a N8N

```bash
# Iniciar N8N
n8n start &

# Esperar a que N8N esté listo (unos segundos)
sleep 10
```

**En el navegador:**

1. Ir a: http://localhost:5678
2. Crear cuenta de administrador
3. Ir a "Workflows" → "Import from file"
4. Importar los siguientes archivos:
   - `workflows/main-workflow.json`
   - `workflows/chatbot-api.json`

### 11. Configurar Credenciales en N8N

**En la interfaz de N8N:**

1. Ir a "Credentials"
2. Crear las siguientes credenciales:

**Anthropic API:**
- Tipo: "HTTP Header Auth"
- Nombre: "anthropicApi"
- Header Name: "x-api-key"
- Header Value: [tu API key de Anthropic]

**OpenAI API:**
- Tipo: "OpenAI API"
- Nombre: "openAi"
- API Key: [tu API key de OpenAI]

**OCR.space API:**
- Tipo: "HTTP Header Auth"
- Nombre: "ocrSpace"
- Header Name: "apikey"
- Header Value: [tu API key de OCR.space]

**PostgreSQL:**
- Tipo: "Postgres"
- Nombre: "postgres"
- Host: localhost
- Database: pdf_chatbot
- User: pdf_chatbot_user
- Password: [tu password]

### 12. Iniciar Servicios

```bash
# Ejecutar script de inicio
./scripts/start-services.sh
```

### 13. Verificar Instalación

```bash
# Verificar servicios
curl http://localhost:6333/health  # Qdrant
curl http://localhost:5678         # N8N

# Verificar base de datos
PGPASSWORD="tu_password" psql -h localhost -U pdf_chatbot_user -d pdf_chatbot -c "SELECT 1;"
```

## 🎯 Prueba del Sistema

### 1. Subir Documentos de Prueba

```bash
# Copiar algunos PDFs a la carpeta de documentos
cp /ruta/a/tus/pdfs/* data/documents/matematicas/
```

### 2. Acceder al Chatbot

1. Abrir navegador
2. Ir a: http://localhost:5678/webhook/web/chatbot
3. Hacer una pregunta de prueba

### 3. Verificar Procesamiento

```bash
# Ver logs del sistema
tail -f logs/system.log

# Verificar documentos en Qdrant
curl "http://localhost:6333/collections/documents/points/count"
```

## 🔧 Solución de Problemas

### Error: "N8N no puede conectar a PostgreSQL"

```bash
# Verificar que PostgreSQL esté ejecutándose
sudo systemctl status postgresql

# Verificar credenciales
PGPASSWORD="tu_password" psql -h localhost -U pdf_chatbot_user -d pdf_chatbot -c "SELECT version();"
```

### Error: "Qdrant no responde"

```bash
# Verificar contenedor Docker
docker ps | grep qdrant

# Reiniciar Qdrant
docker restart pdf-chatbot-qdrant

# Ver logs de Qdrant
docker logs pdf-chatbot-qdrant
```

### Error: "API keys inválidas"

1. Verificar que las API keys estén correctas en `config/.env`
2. Verificar que las credenciales estén configuradas en N8N
3. Verificar límites de uso de las APIs

### Error: "Archivos no se procesan"

```bash
# Verificar permisos de carpetas
ls -la data/documents/

# Verificar logs de procesamiento
tail -f logs/file-processor.log

# Verificar workflow activo en N8N
```

## 📈 Optimización

### Para Mejor Rendimiento

1. **Aumentar memoria de Qdrant:**
   ```yaml
   # En qdrant-config.yaml
   limits:
     max_request_size_mb: 64
   ```

2. **Optimizar PostgreSQL:**
   ```bash
   # Editar /etc/postgresql/13/main/postgresql.conf
   shared_buffers = 256MB
   effective_cache_size = 1GB
   ```

3. **Configurar SSL (Producción):**
   ```bash
   # Instalar certificados SSL
   sudo apt install certbot
   sudo certbot certonly --standalone -d tu-dominio.com
   ```

## 🔒 Seguridad

### Configuración Básica de Seguridad

1. **Firewall:**
   ```bash
   sudo ufw enable
   sudo ufw allow 22    # SSH
   sudo ufw allow 5678  # N8N (solo si es necesario externamente)
   ```

2. **Backup automático:**
   ```bash
   # Agregar a crontab
   crontab -e
   # Agregar línea:
   0 2 * * * /ruta/al/proyecto/scripts/backup.sh
   ```

3. **Monitoreo:**
   ```bash
   # Instalar herramientas de monitoreo
   sudo apt install htop iotop nethogs
   ```

## ✅ Checklist de Instalación

- [ ] Node.js instalado y funcionando
- [ ] PostgreSQL instalado y configurado
- [ ] Docker instalado y Qdrant ejecutándose
- [ ] N8N instalado y accesible
- [ ] Variables de entorno configuradas
- [ ] API keys válidas y configuradas
- [ ] Base de datos creada y schema aplicado
- [ ] Workflows importados en N8N
- [ ] Credenciales configuradas en N8N
- [ ] Servicios iniciados correctamente
- [ ] Chatbot accesible y respondiendo
- [ ] Documentos de prueba procesados

## 🆘 Soporte

Si encuentras problemas durante la instalación:

1. Revisar logs en `logs/`
2. Verificar estado de servicios con `./scripts/status.sh`
3. Consultar documentación en `docs/`
4. Verificar configuración en `config/.env`

---

**¡Instalación completada!** 🎉

El sistema PDF-Chat-Bot debería estar funcionando correctamente. Puedes acceder al chatbot en: http://localhost:5678/webhook/web/chatbot
