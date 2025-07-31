# 🔌 Documentación de Endpoints - PDF-Chat-Bot

Esta documentación describe todos los endpoints disponibles en el sistema 
PDF-Chat-Bot local.

## 🔐 Autenticación

Actualmente el sistema no requiere autenticación para uso local. En producción se recomienda implementar autenticación mediante:
- Tokens locales
- JWT Tokens
- Autenticación básica

## 📋 Endpoints Disponibles

### 1. Chatbot Local

**Endpoint**: `POST /webhook/chatbot`

**Descripción**: Procesa consultas de usuarios y genera respuestas usando el modelo local.

**Headers**:
```
Content-Type: application/json
```

**Body**:
```json
{
  "question": "¿Qué información contiene este documento?",
  "user_id": "user123",
  "session_id": "session456",
  "conversation_id": "conv789",
  "metadata": {
    "source": "web",
    "timestamp": "2024-01-01T12:00:00Z"
  }
}
```

**Respuesta exitosa (200)**:
```json
{
  "response": "El documento contiene información sobre...",
  "conversation_id": "conv789",
  "session_id": "session456",
  "sources": [
    {
      "filename": "documento.pdf",
      "relevance_score": 0.95,
      "chunk_index": 2
    }
  ],
  "metadata": {
    "tokens_processed": 150,
    "processing_time_ms": 1200,
    "context_documents": 3,
    "timestamp": "2024-01-01T12:00:01Z"
  }
}
```

### 2. Upload Local

**Endpoint**: `POST /webhook/upload`

**Descripción**: Procesa archivos PDF e imágenes para extracción de texto y generación de embeddings.

**Headers**:
```
Content-Type: multipart/form-data
```

**Body**:
```
file: [archivo PDF o imagen]
user_id: user123
folder_path: /documents/
```

**Respuesta exitosa (200)**:
```json
{
  "status": "success",
  "file_id": "file_abc123",
  "filename": "documento.pdf",
  "pages_processed": 5,
  "chunks_created": 12,
  "text_extracted": true,
  "embeddings_generated": true,
  "processing_time_ms": 3500,
  "metadata": {
    "file_size": 2048576,
    "mime_type": "application/pdf",
    "upload_timestamp": "2024-01-01T12:00:00Z"
  }
}
```

### 3. History Local

**Endpoint**: `GET /webhook/history/{user_id}`

**Descripción**: Obtiene el historial de conversaciones de un usuario.

**Parámetros de URL**:
- `user_id`: ID del usuario

**Query Parameters**:
- `limit`: Número máximo de conversaciones (default: 50)
- `offset`: Offset para paginación (default: 0)
- `session_id`: Filtrar por sesión específica (opcional)

**Respuesta exitosa (200)**:
```json
{
  "conversations": [
    {
      "conversation_id": "conv789",
      "session_id": "session456",
      "messages": [
        {
          "role": "user",
          "content": "¿Qué información contiene este documento?",
          "timestamp": "2024-01-01T12:00:00Z"
        },
        {
          "role": "assistant",
          "content": "El documento contiene información sobre...",
          "timestamp": "2024-01-01T12:00:01Z",
          "tokens_processed": 120,
          "model_used": "ollama-llama3"
        }
      ],
      "created_at": "2024-01-01T12:00:00Z",
      "updated_at": "2024-01-01T12:00:01Z"
    }
  ],
  "total_conversations": 25,
  "has_more": true
}
```

### 4. Conversation Local

**Endpoint**: `GET /webhook/conversation/{conversation_id}`

**Descripción**: Obtiene una conversación específica con todos sus mensajes.

**Parámetros de URL**:
- `conversation_id`: ID de la conversación

**Respuesta exitosa (200)**:
```json
{
  "conversation_id": "conv789",
  "session_id": "session456",
  "user_id": "user123",
  "messages": [
    {
      "message_id": "msg001",
      "role": "user",
      "content": "Analiza este documento",
      "timestamp": "2024-01-01T12:00:00Z"
    },
    {
      "message_id": "msg002",
      "role": "assistant",
      "content": "He analizado el documento...",
      "timestamp": "2024-01-01T12:00:01Z",
      "tokens_processed": 120,
      "model_used": "ollama-llama3"
    }
  ],
  "referenced_documents": [
    {
      "filename": "documento.pdf",
      "relevance_score": 0.95,
      "chunk_index": 2
    }
  ],
  "created_at": "2024-01-01T12:00:00Z",
  "updated_at": "2024-01-01T12:00:01Z"
}
```

### 5. Status Local

**Endpoint**: `GET /webhook/status`

**Descripción**: Verifica el estado de todos los servicios del sistema.

**Respuesta exitosa (200)**:
```json
{
  "status": "healthy",
  "timestamp": "2024-01-01T12:00:00Z",
  "services": {
    "database": {
      "status": "healthy",
      "response_time_ms": 15
    },
    "vector_store": {
      "status": "healthy",
      "response_time_ms": 25
    },
    "ollama_service": {
      "status": "healthy",
      "response_time_ms": 450
    },
    "embeddings_service": {
      "status": "healthy",
      "response_time_ms": 200
    }
  },
  "system_info": {
    "version": "1.0.0",
    "uptime_seconds": 86400,
    "total_documents": 150,
    "total_conversations": 45
  }
}
```

### 6. Documents Local

**Endpoint**: `GET /webhook/documents`

**Descripción**: Lista todos los documentos procesados en el sistema.

**Query Parameters**:
- `limit`: Número máximo de documentos (default: 50)
- `offset`: Offset para paginación (default: 0)
- `user_id`: Filtrar por usuario (opcional)
- `search`: Buscar en nombres de archivo (opcional)

**Respuesta exitosa (200)**:
```json
{
  "documents": [
    {
      "file_id": "file_abc123",
      "filename": "documento.pdf",
      "user_id": "user123",
      "upload_date": "2024-01-01T12:00:00Z",
      "file_size": 2048576,
      "pages": 5,
      "chunks": 12,
      "status": "processed"
    }
  ],
  "total_documents": 150,
  "has_more": true
}
```

### 7. Search Local

**Endpoint**: `POST /webhook/search`

**Descripción**: Realiza búsqueda semántica en los documentos procesados.

**Body**:
```json
{
  "query": "información sobre contratos",
  "user_id": "user123",
  "limit": 10,
  "threshold": 0.7
}
```

**Respuesta exitosa (200)**:
```json
{
  "results": [
    {
      "document_id": "file_abc123",
      "filename": "contratos.pdf",
      "chunk_index": 3,
      "content": "Los contratos deben incluir...",
      "relevance_score": 0.95,
      "metadata": {
        "page": 2,
        "section": "Términos y Condiciones"
      }
    }
  ],
  "total_results": 25,
  "query_time_ms": 150
}
```

## 🚨 Códigos de Error

### 400 - Bad Request
```json
{
  "error": "invalid_request",
  "message": "El campo 'question' es requerido",
  "details": {
    "field": "question",
    "code": "required"
  }
}
```

### 404 - Not Found
```json
{
  "error": "not_found",
  "message": "Conversación no encontrada",
  "conversation_id": "conv789"
}
```

### 500 - Internal Server Error
```json
{
  "error": "internal_error",
  "message": "Error interno del servidor",
  "timestamp": "2024-01-01T12:00:00Z"
}
```

## 📊 Monitoreo y Logs

### Logs del Sistema

```bash
# Ver logs en tiempo real
tail -f logs/system.log

# Filtrar por endpoint
grep "POST /chatbot" logs/system.log

# Ver errores
grep "ERROR" logs/system.log
```

### Métricas Disponibles

- Tiempo de respuesta por endpoint
- Número de tokens procesados
- Documentos procesados por día
- Conversaciones activas
- Uso de memoria y CPU

## 🔧 Configuración

Para configurar los endpoints, consulta el archivo `configuration.md`.

## 📝 Ejemplos de Uso

Consulta la carpeta `examples/` para ver ejemplos completos de uso de cada endpoint.

---

**Documentación de Endpoints completada!** 🔌

Todos los endpoints locales están documentados y listos para usar.
