# 🔌 Documentación de API - PDF-Chat-Bot

Esta documentación describe todos los endpoints disponibles en el sistema 
PDF-Chat-Bot.

## 🌐 URL Base

```
http://localhost:5678/webhook
```

## 🔐 Autenticación

Actualmente el sistema no requiere autenticación para las APIs públicas. En producción se recomienda implementar autenticación mediante:
- API Keys
- JWT Tokens
- OAuth 2.0

## 📡 Endpoints Disponibles

### 1. Chatbot API

#### `POST /chatbot`

Envía una pregunta al chatbot y recibe una respuesta basada en los documentos procesados.

**Request:**
```json
{
  "question": "¿Cuáles son los temas principales del documento?",
  "user_id": "user_123",
  "session_id": "session_456",
  "conversation_id": "conv_789"
}
```

**Response:**
```json
{
  "response": "Los temas principales del documento incluyen...",
  "conversation_id": "conv_789",
  "session_id": "session_456",
  "sources": [
    {
      "filename": "matematicas_basicas.pdf",
      "relevance_score": 0.95,
      "chunk_index": 2
    }
  ],
  "metadata": {
    "tokens_used": 150,
    "processing_time_ms": 1250,
    "context_documents": 3,
    "timestamp": "2024-01-01T12:00:00Z"
  }
}
```

**Parámetros:**

| Campo | Tipo | Requerido | Descripción |
|-------|------|-----------|-------------|
| `question` | string | ✅ | Pregunta del usuario (máx. 2000 caracteres) |
| `user_id` | string | ❌ | ID único del usuario (default: "anonymous") |
| `session_id` | string | ❌ | ID de sesión (se genera automáticamente) |
| `conversation_id` | string | ❌ | ID de conversación existente |

**Códigos de Estado:**
- `200`: Respuesta exitosa
- `400`: Pregunta inválida o muy larga
- `500`: Error interno del servidor

---

### 2. Upload API

#### `POST /upload`

Sube documentos PDF o imágenes para ser procesados por el sistema.

**Request (multipart/form-data):**
```
file: [archivo PDF o imagen]
user_id: "user_123"
folder: "matematicas"
```

**Response:**
```json
{
  "success": true,
  "message": "Archivo subido exitosamente",
  "file_info": {
    "filename": "documento.pdf",
    "size": 1024000,
    "type": "application/pdf",
    "hash": "abc123def456",
    "processing_status": "queued"
  },
  "upload_id": "upload_789"
}
```

**Parámetros:**

| Campo | Tipo | Requerido | Descripción |
|-------|------|-----------|-------------|
| `file` | file | ✅ | Archivo PDF, JPG, PNG (máx. 50MB) |
| `user_id` | string | ❌ | ID del usuario |
| `folder` | string | ❌ | Subcarpeta de destino |

**Códigos de Estado:**
- `200`: Upload exitoso
- `400`: Archivo inválido o muy grande
- `413`: Archivo excede límite de tamaño
- `500`: Error en el procesamiento

---

### 3. History API

#### `GET /history/{user_id}`

Obtiene el historial de conversaciones de un usuario.

**Request:**
```
GET /webhook/history/user_123?limit=20&offset=0
```

**Response:**
```json
{
  "conversations": [
    {
      "conversation_id": "conv_789",
      "title": "Preguntas sobre matemáticas básicas",
      "created_at": "2024-01-01T10:00:00Z",
      "updated_at": "2024-01-01T10:30:00Z",
      "message_count": 6,
      "preview": "¿Cuáles son los temas principales..."
    }
  ],
  "total": 15,
  "limit": 20,
  "offset": 0
}
```

**Parámetros de Query:**

| Campo | Tipo | Default | Descripción |
|-------|------|---------|-------------|
| `limit` | integer | 20 | Número de conversaciones a retornar |
| `offset` | integer | 0 | Número de conversaciones a saltar |
| `search` | string | - | Buscar en títulos y contenido |

---

### 4. Conversation API

#### `GET /conversation/{conversation_id}`

Obtiene los mensajes de una conversación específica.

**Request:**
```
GET /webhook/conversation/conv_789
```

**Response:**
```json
{
  "conversation": {
    "id": "conv_789",
    "title": "Preguntas sobre matemáticas",
    "created_at": "2024-01-01T10:00:00Z",
    "user_id": "user_123",
    "session_id": "session_456"
  },
  "messages": [
    {
      "id": "msg_001",
      "role": "user",
      "content": "¿Qué es una función matemática?",
      "created_at": "2024-01-01T10:00:00Z"
    },
    {
      "id": "msg_002",
      "role": "assistant",
      "content": "Una función matemática es...",
      "created_at": "2024-01-01T10:00:30Z",
      "sources": [
        {
          "filename": "funciones.pdf",
          "relevance_score": 0.92
        }
      ],
      "metadata": {
        "tokens_used": 120,
        "model_used": "claude-3-sonnet"
      }
    }
  ]
}
```

---

### 5. Status API

#### `GET /status`

Verifica el estado del sistema y sus componentes.

**Request:**
```
GET /webhook/status
```

**Response:**
```json
{
  "status": "healthy",
  "timestamp": "2024-01-01T12:00:00Z",
  "version": "1.0.0",
  "components": {
    "database": {
      "status": "healthy",
      "response_time_ms": 5,
      "connections": 3
    },
    "qdrant": {
      "status": "healthy",
      "response_time_ms": 12,
      "collections": 1,
      "points_count": 1250
    },
    "anthropic_api": {
      "status": "healthy",
      "response_time_ms": 450
    },
    "openai_api": {
      "status": "healthy",
      "response_time_ms": 200
    }
  },
  "statistics": {
    "total_documents": 45,
    "total_conversations": 123,
    "total_messages": 456,
    "disk_usage": "2.3GB",
    "uptime_seconds": 86400
  }
}
```

---

### 6. Documents API

#### `GET /documents`

Lista los documentos procesados en el sistema.

**Request:**
```
GET /webhook/documents?limit=10&folder=matematicas
```

**Response:**
```json
{
  "documents": [
    {
      "id": "doc_123",
      "filename": "algebra_basica.pdf",
      "filepath": "/data/documents/matematicas/algebra_basica.pdf",
      "file_size": 2048000,
      "file_type": "pdf",
      "processed_at": "2024-01-01T09:00:00Z",
      "chunk_count": 25,
      "status": "completed"
    }
  ],
  "total": 45,
  "limit": 10,
  "offset": 0
}
```

#### `DELETE /documents/{document_id}`

Elimina un documento del sistema.

**Request:**
```
DELETE /webhook/documents/doc_123
```

**Response:**
```json
{
  "success": true,
  "message": "Documento eliminado exitosamente",
  "document_id": "doc_123"
}
```

---

### 7. Search API

#### `POST /search`

Realiza búsqueda semántica en los documentos.

**Request:**
```json
{
  "query": "funciones trigonométricas",
  "limit": 5,
  "threshold": 0.7,
  "filters": {
    "folder": "matematicas",
    "file_type": "pdf"
  }
}
```

**Response:**
```json
{
  "results": [
    {
      "document_id": "doc_123",
      "filename": "trigonometria.pdf",
      "chunk_index": 3,
      "content": "Las funciones trigonométricas son...",
      "relevance_score": 0.95,
      "metadata": {
        "page": 15,
        "section": "Capítulo 3"
      }
    }
  ],
  "query": "funciones trigonométricas",
  "total_results": 12,
  "processing_time_ms": 45
}
```

---

## 🔧 Configuración de Cliente

### JavaScript/Node.js

```javascript
class PDFChatBotClient {
  constructor(baseUrl = 'http://localhost:5678/webhook') {
    this.baseUrl = baseUrl;
  }

  async askQuestion(question, userId = 'anonymous') {
    const response = await fetch(`${this.baseUrl}/chatbot`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        question,
        user_id: userId,
        session_id: this.getSessionId()
      })
    });

    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }

    return await response.json();
  }

  async uploadDocument(file, userId = 'anonymous') {
    const formData = new FormData();
    formData.append('file', file);
    formData.append('user_id', userId);

    const response = await fetch(`${this.baseUrl}/upload`, {
      method: 'POST',
      body: formData
    });

    return await response.json();
  }

  getSessionId() {
    let sessionId = localStorage.getItem('pdf_chatbot_session');
    if (!sessionId) {
      sessionId = 'session_' + Date.now();
      localStorage.setItem('pdf_chatbot_session', sessionId);
    }
    return sessionId;
  }
}

// Uso
const client = new PDFChatBotClient();
const response = await client.askQuestion('¿Qué es una integral?');
console.log(response.response);
```

### Python

```python
import requests
import json

class PDFChatBotClient:
    def __init__(self, base_url='http://localhost:5678/webhook'):
        self.base_url = base_url
        self.session_id = f"session_{int(time.time())}"

    def ask_question(self, question, user_id='anonymous'):
        url = f"{self.base_url}/chatbot"
        payload = {
            'question': question,
            'user_id': user_id,
            'session_id': self.session_id
        }
        
        response = requests.post(url, json=payload)
        response.raise_for_status()
        return response.json()

    def upload_document(self, file_path, user_id='anonymous'):
        url = f"{self.base_url}/upload"
        
        with open(file_path, 'rb') as file:
            files = {'file': file}
            data = {'user_id': user_id}
            
            response = requests.post(url, files=files, data=data)
            response.raise_for_status()
            return response.json()

# Uso
client = PDFChatBotClient()
response = client.ask_question('¿Qué es una derivada?')
print(response['response'])
```

### cURL

```bash
# Hacer pregunta
curl -X POST http://localhost:5678/webhook/chatbot \
  -H "Content-Type: application/json" \
  -d '{
    "question": "¿Qué es un límite matemático?",
    "user_id": "user_123"
  }'

# Subir documento
curl -X POST http://localhost:5678/webhook/upload \
  -F "file=@documento.pdf" \
  -F "user_id=user_123"

# Verificar estado
curl http://localhost:5678/webhook/status
```

## 📊 Rate Limiting

El sistema implementa rate limiting para prevenir abuso:

- **Por minuto**: 60 requests por IP
- **Por hora**: 1000 requests por IP
- **Por día**: 10000 requests por IP

**Headers de respuesta:**
```
X-RateLimit-Limit: 60
X-RateLimit-Remaining: 45
X-RateLimit-Reset: 1640995200
```

## ❌ Códigos de Error

| Código | Descripción | Solución |
|--------|-------------|----------|
| 400 | Bad Request | Verificar formato de datos |
| 401 | Unauthorized | Verificar autenticación |
| 403 | Forbidden | Verificar permisos |
| 404 | Not Found | Verificar URL del endpoint |
| 413 | Payload Too Large | Reducir tamaño del archivo |
| 429 | Too Many Requests | Esperar antes de reintentar |
| 500 | Internal Server Error | Verificar logs del servidor |
| 503 | Service Unavailable | Verificar estado de servicios |

## 🔍 Debugging

### Logs de API

```bash
# Ver logs en tiempo real
tail -f logs/api.log

# Filtrar por endpoint
grep "POST /chatbot" logs/api.log

# Ver errores
grep "ERROR" logs/api.log
```

### Testing de Endpoints

```bash
# Script de prueba
#!/bin/bash
BASE_URL="http://localhost:5678/webhook"

# Test status
echo "Testing status endpoint..."
curl -s "$BASE_URL/status" | jq .

# Test chatbot
echo "Testing chatbot endpoint..."
curl -s -X POST "$BASE_URL/chatbot" \
  -H "Content-Type: application/json" \
  -d '{"question": "Test question"}' | jq .
```

---

**API Documentation completada!** 🔌

Todos los endpoints están documentados y listos para usar.
