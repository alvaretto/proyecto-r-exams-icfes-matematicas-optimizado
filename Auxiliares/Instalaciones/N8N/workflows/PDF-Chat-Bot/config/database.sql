-- ========================================
-- PDF-Chat-Bot - Schema de Base de Datos
-- ========================================

-- Crear base de datos (ejecutar como superusuario)
-- CREATE DATABASE pdf_chatbot;
-- CREATE USER pdf_chatbot_user WITH PASSWORD 'your_secure_password_here';
-- GRANT ALL PRIVILEGES ON DATABASE pdf_chatbot TO pdf_chatbot_user;

-- Conectar a la base de datos pdf_chatbot
\c pdf_chatbot;

-- Habilitar extensiones necesarias
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pg_trgm";

-- ========================================
-- TABLA DE DOCUMENTOS
-- ========================================
CREATE TABLE IF NOT EXISTS documents (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    filename VARCHAR(500) NOT NULL,
    filepath TEXT NOT NULL,
    file_hash VARCHAR(64) UNIQUE NOT NULL,
    file_size BIGINT NOT NULL,
    file_type VARCHAR(10) NOT NULL,
    mime_type VARCHAR(100),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    processed_at TIMESTAMP WITH TIME ZONE,
    status VARCHAR(20) DEFAULT 'pending', -- pending, processing, completed, error
    chunk_count INTEGER DEFAULT 0,
    metadata JSONB DEFAULT '{}',
    INDEX (filename),
    INDEX (file_hash),
    INDEX (status),
    INDEX (created_at)
);

-- ========================================
-- TABLA DE CHUNKS DE DOCUMENTOS
-- ========================================
CREATE TABLE IF NOT EXISTS document_chunks (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    document_id UUID NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
    chunk_index INTEGER NOT NULL,
    content TEXT NOT NULL,
    content_hash VARCHAR(64) NOT NULL,
    token_count INTEGER,
    embedding_id VARCHAR(100), -- ID en Qdrant
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    metadata JSONB DEFAULT '{}',
    UNIQUE(document_id, chunk_index),
    INDEX (document_id),
    INDEX (chunk_index),
    INDEX (content_hash)
);

-- ========================================
-- TABLA DE CONVERSACIONES
-- ========================================
CREATE TABLE IF NOT EXISTS conversations (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    user_id VARCHAR(255) NOT NULL,
    session_id VARCHAR(255) NOT NULL,
    title VARCHAR(500),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    status VARCHAR(20) DEFAULT 'active', -- active, archived, deleted
    message_count INTEGER DEFAULT 0,
    metadata JSONB DEFAULT '{}',
    INDEX (user_id),
    INDEX (session_id),
    INDEX (status),
    INDEX (created_at)
);

-- ========================================
-- TABLA DE MENSAJES
-- ========================================
CREATE TABLE IF NOT EXISTS messages (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    conversation_id UUID NOT NULL REFERENCES conversations(id) ON DELETE CASCADE,
    role VARCHAR(20) NOT NULL, -- 'user' o 'assistant'
    content TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    tokens_used INTEGER,
    processing_time_ms INTEGER,
    model_used VARCHAR(100),
    metadata JSONB DEFAULT '{}',
    INDEX (conversation_id),
    INDEX (role),
    INDEX (created_at)
);

-- ========================================
-- TABLA DE DOCUMENTOS REFERENCIADOS EN MENSAJES
-- ========================================
CREATE TABLE IF NOT EXISTS message_documents (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    message_id UUID NOT NULL REFERENCES messages(id) ON DELETE CASCADE,
    document_id UUID NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
    chunk_id UUID REFERENCES document_chunks(id) ON DELETE CASCADE,
    relevance_score FLOAT,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    INDEX (message_id),
    INDEX (document_id),
    INDEX (relevance_score)
);

-- ========================================
-- TABLA DE LOGS DEL SISTEMA
-- ========================================
CREATE TABLE IF NOT EXISTS system_logs (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    level VARCHAR(10) NOT NULL, -- debug, info, warn, error
    message TEXT NOT NULL,
    component VARCHAR(50), -- file_processor, chatbot, vectorizer, etc.
    user_id VARCHAR(255),
    session_id VARCHAR(255),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    metadata JSONB DEFAULT '{}',
    INDEX (level),
    INDEX (component),
    INDEX (created_at)
);

-- ========================================
-- TABLA DE CONFIGURACIÓN
-- ========================================
CREATE TABLE IF NOT EXISTS system_config (
    key VARCHAR(100) PRIMARY KEY,
    value TEXT NOT NULL,
    description TEXT,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_by VARCHAR(255)
);

-- ========================================
-- FUNCIONES Y TRIGGERS
-- ========================================

-- Función para actualizar updated_at automáticamente
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ language 'plpgsql';

-- Triggers para updated_at
CREATE TRIGGER update_documents_updated_at 
    BEFORE UPDATE ON documents 
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

CREATE TRIGGER update_conversations_updated_at 
    BEFORE UPDATE ON conversations 
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

-- Función para actualizar contador de mensajes
CREATE OR REPLACE FUNCTION update_conversation_message_count()
RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        UPDATE conversations 
        SET message_count = message_count + 1,
            updated_at = NOW()
        WHERE id = NEW.conversation_id;
        RETURN NEW;
    ELSIF TG_OP = 'DELETE' THEN
        UPDATE conversations 
        SET message_count = message_count - 1,
            updated_at = NOW()
        WHERE id = OLD.conversation_id;
        RETURN OLD;
    END IF;
    RETURN NULL;
END;
$$ language 'plpgsql';

-- Trigger para contador de mensajes
CREATE TRIGGER update_message_count_trigger
    AFTER INSERT OR DELETE ON messages
    FOR EACH ROW EXECUTE FUNCTION update_conversation_message_count();

-- ========================================
-- ÍNDICES ADICIONALES PARA PERFORMANCE
-- ========================================

-- Índices para búsqueda de texto completo
CREATE INDEX IF NOT EXISTS idx_documents_content_search 
    ON document_chunks USING gin(to_tsvector('spanish', content));

CREATE INDEX IF NOT EXISTS idx_messages_content_search 
    ON messages USING gin(to_tsvector('spanish', content));

-- Índices compuestos para consultas frecuentes
CREATE INDEX IF NOT EXISTS idx_conversations_user_status 
    ON conversations(user_id, status, created_at DESC);

CREATE INDEX IF NOT EXISTS idx_messages_conversation_created 
    ON messages(conversation_id, created_at DESC);

-- ========================================
-- DATOS INICIALES
-- ========================================

-- Configuración inicial del sistema
INSERT INTO system_config (key, value, description) VALUES
('system_version', '1.0.0', 'Versión del sistema PDF-Chat-Bot'),
('max_file_size_mb', '50', 'Tamaño máximo de archivo en MB'),
('chunk_size', '1000', 'Tamaño de chunk para vectorización'),
('chunk_overlap', '200', 'Solapamiento entre chunks'),
('search_limit', '5', 'Número máximo de resultados de búsqueda'),
('similarity_threshold', '0.7', 'Umbral mínimo de similitud'),
('session_timeout_hours', '24', 'Tiempo de expiración de sesión en horas')
ON CONFLICT (key) DO NOTHING;

-- ========================================
-- PERMISOS
-- ========================================

-- Otorgar permisos al usuario de la aplicación
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO pdf_chatbot_user;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO pdf_chatbot_user;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA public TO pdf_chatbot_user;

-- Permisos para futuras tablas
ALTER DEFAULT PRIVILEGES IN SCHEMA public 
    GRANT ALL PRIVILEGES ON TABLES TO pdf_chatbot_user;
ALTER DEFAULT PRIVILEGES IN SCHEMA public 
    GRANT ALL PRIVILEGES ON SEQUENCES TO pdf_chatbot_user;

-- ========================================
-- COMENTARIOS EN TABLAS
-- ========================================

COMMENT ON TABLE documents IS 'Almacena información de documentos PDF e imágenes procesados';
COMMENT ON TABLE document_chunks IS 'Chunks de texto extraídos de documentos para vectorización';
COMMENT ON TABLE conversations IS 'Conversaciones de usuarios con el chatbot';
COMMENT ON TABLE messages IS 'Mensajes individuales dentro de conversaciones';
COMMENT ON TABLE message_documents IS 'Relación entre mensajes y documentos referenciados';
COMMENT ON TABLE system_logs IS 'Logs del sistema para monitoreo y debugging';
COMMENT ON TABLE system_config IS 'Configuración del sistema';

-- ========================================
-- FINALIZACIÓN
-- ========================================

-- Mostrar resumen de tablas creadas
SELECT 
    schemaname,
    tablename,
    tableowner
FROM pg_tables 
WHERE schemaname = 'public' 
ORDER BY tablename;
