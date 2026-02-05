#!/bin/bash

# Script de prueba para verificar instalación y funcionamiento de Gemini-CLI
# Autor: Evaluación Gemini-CLI vs Augment
# Fecha: $(date +%Y-%m-%d)

set -e

echo "🧪 PRUEBAS DE VERIFICACIÓN GEMINI-CLI"
echo "=" | tr ' ' '=' | head -c 50; echo

# Función para verificar comandos
verificar_comando() {
    local comando=$1
    local descripcion=$2
    
    echo "🔍 Verificando: $descripcion"
    if command -v "$comando" &> /dev/null; then
        echo "✅ $comando disponible"
        return 0
    else
        echo "❌ $comando NO encontrado"
        return 1
    fi
}

# Función para ejecutar prueba con timeout
ejecutar_con_timeout() {
    local timeout_sec=$1
    local comando=$2
    local descripcion=$3
    
    echo "⏱️ Ejecutando: $descripcion (timeout: ${timeout_sec}s)"
    
    if timeout "$timeout_sec" bash -c "$comando" 2>/dev/null; then
        echo "✅ $descripcion - EXITOSO"
        return 0
    else
        echo "❌ $descripcion - FALLÓ o TIMEOUT"
        return 1
    fi
}

# 1. VERIFICAR INSTALACIÓN BÁSICA
echo "📦 VERIFICACIÓN DE INSTALACIÓN"
echo "------------------------------"

verificar_comando "node" "Node.js"
verificar_comando "npm" "NPM"
verificar_comando "gemini" "Gemini-CLI"

echo ""

# 2. VERIFICAR VERSIÓN DE GEMINI-CLI
echo "📋 INFORMACIÓN DE VERSIÓN"
echo "-------------------------"

if command -v gemini &> /dev/null; then
    echo "🔍 Obteniendo versión de Gemini-CLI..."
    version_output=$(gemini --version 2>/dev/null || echo "versión no disponible")
    echo "📊 Versión: $version_output"
else
    echo "❌ Gemini-CLI no está instalado"
    exit 1
fi

echo ""

# 3. VERIFICAR AYUDA Y COMANDOS BÁSICOS
echo "📚 VERIFICACIÓN DE COMANDOS"
echo "---------------------------"

ejecutar_con_timeout 10 "gemini --help" "Comando de ayuda"

echo ""

# 4. VERIFICAR CONFIGURACIÓN
echo "⚙️ VERIFICACIÓN DE CONFIGURACIÓN"
echo "--------------------------------"

# Verificar directorio de configuración
config_dir="$HOME/.gemini"
if [ -d "$config_dir" ]; then
    echo "✅ Directorio de configuración existe: $config_dir"
    
    # Listar archivos de configuración
    if [ "$(ls -A $config_dir 2>/dev/null)" ]; then
        echo "📁 Archivos de configuración encontrados:"
        ls -la "$config_dir" | grep -v "^total" | tail -n +2 | while read line; do
            echo "   $line"
        done
    else
        echo "📁 Directorio de configuración vacío (normal en primera instalación)"
    fi
else
    echo "⚠️ Directorio de configuración no existe (se creará en primer uso)"
fi

echo ""

# 5. VERIFICAR AUTENTICACIÓN
echo "🔐 VERIFICACIÓN DE AUTENTICACIÓN"
echo "--------------------------------"

if [ -n "$GEMINI_API_KEY" ]; then
    echo "✅ Variable GEMINI_API_KEY configurada"
    echo "🔑 API Key: ${GEMINI_API_KEY:0:10}..."
elif [ -n "$GOOGLE_API_KEY" ]; then
    echo "✅ Variable GOOGLE_API_KEY configurada"
    echo "🔑 API Key: ${GOOGLE_API_KEY:0:10}..."
else
    echo "⚠️ No se encontraron variables de API key"
    echo "💡 Configurar con: export GEMINI_API_KEY=your_key"
    echo "💡 O usar OAuth: gemini (seguir flujo de autenticación)"
fi

echo ""

# 6. PRUEBA BÁSICA DE FUNCIONALIDAD (si hay autenticación)
echo "🚀 PRUEBA BÁSICA DE FUNCIONALIDAD"
echo "---------------------------------"

if [ -n "$GEMINI_API_KEY" ] || [ -n "$GOOGLE_API_KEY" ]; then
    echo "🧪 Ejecutando prueba básica..."
    
    # Crear archivo temporal de prueba
    test_file=$(mktemp)
    echo "console.log('Hello, World!');" > "$test_file"
    
    # Intentar análisis básico con timeout
    if ejecutar_con_timeout 30 "echo 'Explain this code briefly' | gemini -p 'Analyze this JavaScript code' --include-files '$test_file'" "Análisis de código básico"; then
        echo "✅ Funcionalidad básica verificada"
    else
        echo "⚠️ Prueba básica falló (puede requerir configuración adicional)"
    fi
    
    # Limpiar archivo temporal
    rm -f "$test_file"
else
    echo "⏭️ Saltando prueba funcional (requiere autenticación)"
    echo "💡 Configurar autenticación para pruebas completas"
fi

echo ""

# 7. VERIFICAR CAPACIDADES MCP
echo "🔌 VERIFICACIÓN DE CAPACIDADES MCP"
echo "----------------------------------"

# Verificar si gemini tiene comandos MCP
if gemini --help 2>/dev/null | grep -q "mcp\|MCP" 2>/dev/null; then
    echo "✅ Soporte MCP detectado en ayuda"
else
    echo "⚠️ Soporte MCP no detectado en ayuda (puede estar integrado)"
fi

# Verificar archivo de configuración MCP
mcp_config="$HOME/.gemini/settings.json"
if [ -f "$mcp_config" ]; then
    echo "✅ Archivo de configuración MCP encontrado: $mcp_config"
else
    echo "📝 Archivo de configuración MCP no existe (se puede crear manualmente)"
fi

echo ""

# 8. RESUMEN FINAL
echo "📊 RESUMEN DE VERIFICACIÓN"
echo "========================="

# Contar verificaciones exitosas
total_checks=0
passed_checks=0

# Verificaciones básicas
for cmd in node npm gemini; do
    total_checks=$((total_checks + 1))
    if command -v "$cmd" &> /dev/null; then
        passed_checks=$((passed_checks + 1))
    fi
done

# Verificación de autenticación
total_checks=$((total_checks + 1))
if [ -n "$GEMINI_API_KEY" ] || [ -n "$GOOGLE_API_KEY" ]; then
    passed_checks=$((passed_checks + 1))
fi

echo "📈 Verificaciones pasadas: $passed_checks/$total_checks"

if [ $passed_checks -eq $total_checks ]; then
    echo "🎉 INSTALACIÓN COMPLETAMENTE VERIFICADA"
    echo "✅ Gemini-CLI está listo para usar"
elif [ $passed_checks -gt $((total_checks / 2)) ]; then
    echo "⚠️ INSTALACIÓN PARCIALMENTE VERIFICADA"
    echo "🔧 Revisar elementos faltantes"
else
    echo "❌ INSTALACIÓN INCOMPLETA"
    echo "🛠️ Revisar instalación y configuración"
fi

echo ""
echo "📋 PRÓXIMOS PASOS RECOMENDADOS:"
echo "1. Si no hay autenticación: gemini (seguir flujo OAuth)"
echo "2. Probar comando básico: gemini --help"
echo "3. Configurar MCP servers si es necesario"
echo "4. Ejecutar pruebas de evaluación específicas"
echo ""
