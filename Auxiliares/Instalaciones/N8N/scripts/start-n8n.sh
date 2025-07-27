#!/bin/bash

# 🚀 Script de inicio N8N para ICFES Matemáticas
# Configuración optimizada para el repositorio

echo "🚀 Iniciando N8N Community Edition para ICFES Matemáticas..."
echo "📁 Repositorio: RepositorioMatematicasICFES_R_Exams"
echo "📅 Fecha: $(date)"

# Configurar variables de entorno
export N8N_BASIC_AUTH_ACTIVE=true
export N8N_BASIC_AUTH_USER=admin
export N8N_BASIC_AUTH_PASSWORD=icfes2025
export N8N_HOST=localhost
export N8N_PORT=5678
export N8N_PROTOCOL=http

# Configurar directorios
export N8N_USER_FOLDER=~/.n8n
export N8N_CUSTOM_EXTENSIONS=~/.n8n/custom

# Configuración específica para ICFES
export N8N_DEFAULT_LOCALE=es
export N8N_TIMEZONE=America/Bogota

# Configurar logs
export N8N_LOG_LEVEL=info
export N8N_LOG_OUTPUT=console,file
export N8N_LOG_FILE=~/.n8n/logs/n8n.log

# Crear directorios necesarios
mkdir -p ~/.n8n/logs
mkdir -p ~/.n8n/backups
mkdir -p ~/n8n-workflows/RepositorioMatematicasICFES_R_Exams/{workflows,exports,backups}

echo "✅ Variables de entorno configuradas"
echo "📂 Directorios creados"

# Verificar instalación de N8N
if ! command -v n8n &> /dev/null; then
    echo "❌ N8N no está instalado. Instalando..."
    npm install -g n8n
    echo "✅ N8N instalado"
fi

# Verificar versión
N8N_VERSION=$(n8n --version)
echo "📦 Versión N8N: $N8N_VERSION"

# Crear backup antes de iniciar
echo "💾 Creando backup de seguridad..."
if [ -f ~/.n8n/database.sqlite ]; then
    cp ~/.n8n/database.sqlite ~/.n8n/backups/database_$(date +%Y%m%d_%H%M%S).sqlite
    echo "✅ Backup creado"
fi

# Mostrar información de acceso
echo ""
echo "🌐 INFORMACIÓN DE ACCESO:"
echo "   URL: http://localhost:5678"
echo "   Usuario: admin"
echo "   Contraseña: icfes2025"
echo ""
echo "📊 WORKFLOWS DISPONIBLES:"
echo "   - 🔧 Validador Automático ICFES (cada 30 min)"
echo "   - 📊 Compilador Masivo Multi-formato"
echo "   - 🎨 Replicador TikZ PNG→TikZ"
echo "   - 🚨 Monitor de Errores (cada 15 min)"
echo ""
echo "🔧 AUTO-CORRECTOR:"
echo "   - Versión: 2.0 Mejorado"
echo "   - Ubicación: core/auto_corrector_icfes.py"
echo "   - Uso manual: python3 core/auto_corrector_icfes.py --scan-recent"
echo ""

# Iniciar N8N
echo "🚀 Iniciando N8N..."
echo "   Presiona Ctrl+C para detener"
echo "   Los logs se guardan en ~/.n8n/logs/n8n.log"
echo ""

# Ejecutar N8N
n8n start
