#!/bin/bash

# Configuración de variables de entorno para MCPs
# Ejecutar: source mcp-env-setup.sh

echo "🔑 CONFIGURACIÓN DE VARIABLES DE ENTORNO PARA MCPs"
echo "================================================="

# Context7 (Upstash Redis) - Opcional
if [ -z "$UPSTASH_REDIS_REST_URL" ]; then
    echo "⚠️  UPSTASH_REDIS_REST_URL no configurada"
    echo "   Para usar Context7, configura:"
    echo "   export UPSTASH_REDIS_REST_URL='tu_url_redis'"
    echo "   export UPSTASH_REDIS_REST_TOKEN='tu_token_redis'"
fi

# Brave Search API - Opcional
if [ -z "$BRAVE_API_KEY" ]; then
    echo "⚠️  BRAVE_API_KEY no configurada"
    echo "   Para usar Brave Search, configura:"
    echo "   export BRAVE_API_KEY='tu_api_key_brave'"
fi

# Verificar Gemini API Key
if [ -z "$GEMINI_API_KEY" ]; then
    echo "❌ GEMINI_API_KEY no configurada"
    echo "   Configura tu API Key de Gemini:"
    echo "   export GEMINI_API_KEY='tu_api_key_gemini'"
else
    echo "✅ GEMINI_API_KEY configurada"
fi

echo ""
echo "💡 Para configurar las APIs opcionales:"
echo "   1. Context7: https://upstash.com/ (Redis gratuito)"
echo "   2. Brave Search: https://api.search.brave.com/ (API gratuita)"
echo ""
echo "🚀 Una vez configuradas, reinicia gemini-icfes-optimizado"
