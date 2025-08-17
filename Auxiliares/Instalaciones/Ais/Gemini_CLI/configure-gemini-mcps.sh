#!/bin/bash

# Script de configuración de MCPs para gemini-icfes-optimizado
# Ubicación: Auxiliares/Instalaciones/Ais/Gemini_CLI/
# Autor: Configuración automatizada para proyecto ICFES R-exams
# Fecha: $(date)

echo "🔧 CONFIGURACIÓN DE MCPs PARA GEMINI-ICFES-OPTIMIZADO"
echo "===================================================="
echo ""

# Colores para output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Función para mostrar resultados
show_result() {
    if [ $1 -eq 0 ]; then
        echo -e "${GREEN}✅ $2${NC}"
    else
        echo -e "${RED}❌ $2${NC}"
    fi
}

# Obtener directorio del script y proyecto
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

echo -e "${BLUE}📋 VERIFICANDO INSTALACIÓN DE MCPs...${NC}"
echo "------------------------------------"

# Verificar que los MCPs están instalados
echo -n "Verificando directorio MCPs: "
if [ -d "$PROJECT_ROOT/.mcps" ]; then
    echo -e "${GREEN}✅ Encontrado${NC}"
else
    echo -e "${RED}❌ No encontrado${NC}"
    echo "Ejecuta primero: bash install-mcps.sh"
    exit 1
fi

echo -n "Verificando configuración MCP: "
if [ -f "$PROJECT_ROOT/.mcp-config.json" ]; then
    echo -e "${GREEN}✅ Encontrada${NC}"
else
    echo -e "${RED}❌ No encontrada${NC}"
    echo "Ejecuta primero: bash install-mcps.sh"
    exit 1
fi

echo ""
echo -e "${BLUE}⚙️ CREANDO CONFIGURACIÓN INTEGRADA...${NC}"
echo "-------------------------------------"

# Crear configuración de Gemini CLI con MCPs
cat > "$PROJECT_ROOT/.gemini-mcp-config.json" << EOF
{
  "version": "1.0",
  "project": "RepositorioMatematicasICFES_R_Exams",
  "gemini": {
    "model": "gemini-2.5-pro",
    "context_window": "1M_tokens",
    "temperature": 0.1,
    "max_tokens": 8192
  },
  "mcps": {
    "auto_enable": true,
    "servers": {
      "context7": {
        "enabled": true,
        "auto_trigger": ["documentación", "librería", "API", "referencia"],
        "description": "Documentación de librerías y APIs"
      },
      "playwright": {
        "enabled": true,
        "auto_trigger": ["web", "navegador", "scraping", "testing", "automatización"],
        "description": "Automatización web y testing"
      },
      "memory": {
        "enabled": true,
        "auto_trigger": ["recordar", "memoria", "guardar", "persistir"],
        "description": "Gestión de memoria persistente"
      },
      "brave-search": {
        "enabled": true,
        "auto_trigger": ["buscar", "investigar", "información", "actualizada"],
        "description": "Búsqueda web privada"
      },
      "filesystem": {
        "enabled": true,
        "auto_trigger": ["archivo", "directorio", "leer", "escribir"],
        "description": "Acceso a archivos locales"
      }
    }
  },
  "icfes_integration": {
    "auto_context": [
      "Auxiliares/Instalaciones/Ais/Gemini_CLI/GEMINI.md",
      "Auxiliares/rules_full/rules_full_v1.md"
    ],
    "priority_directories": [
      "Auxiliares/Ejemplos-Funcionales-Rmd",
      "Auxiliares/TikZ-Documentation",
      "Lab-Manjaro"
    ]
  }
}
EOF

show_result $? "Configuración integrada creada"

# Crear script de inicio con MCPs
cat > "$SCRIPT_DIR/gemini-mcps-optimizado.sh" << 'EOF'
#!/bin/bash

# Script de inicio de Gemini CLI con MCPs optimizado
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

echo "🤖 GEMINI CLI + MCPs OPTIMIZADO - PROYECTO ICFES R-EXAMS"
echo "========================================================"
echo ""

# Verificar configuración
if [[ ! -f "$PROJECT_DIR/.gemini-mcp-config.json" ]]; then
    echo "❌ Error: Configuración MCP no encontrada"
    echo "   Ejecuta: bash configure-gemini-mcps.sh"
    exit 1
fi

# Cargar variables de entorno para MCPs
if [[ -f "$SCRIPT_DIR/mcp-env-setup.sh" ]]; then
    source "$SCRIPT_DIR/mcp-env-setup.sh"
fi

echo "📁 Directorio del proyecto: $PROJECT_DIR"
echo "🔧 MCPs habilitados:"
echo "   • ✅ Context7 - Documentación de librerías"
echo "   • ✅ Playwright - Automatización web"
echo "   • ✅ Memory - Gestión de memoria persistente"
echo "   • ✅ Brave Search - Búsqueda web privada"
echo "   • ✅ Filesystem - Acceso a archivos locales"
echo ""
echo "🎯 Contexto optimizado con .geminiignore"
echo "📋 Configuración ICFES cargada automáticamente"
echo ""

# Cambiar al directorio del proyecto
cd "$PROJECT_DIR"

# Verificar que no estamos en root
if [[ "$PWD" == "/" ]]; then
    echo "⚠️  Error: No se puede ejecutar desde directorio raíz"
    exit 1
fi

echo "📍 Directorio actual: $(pwd)"
echo "🚀 Iniciando Gemini CLI con MCPs integrados..."
echo ""
echo "💡 Comandos MCP disponibles:"
echo "   • 'buscar información sobre [tema]' → Brave Search"
echo "   • 'documentación de [librería]' → Context7"
echo "   • 'automatizar navegación web' → Playwright"
echo "   • 'recordar [información]' → Memory"
echo "   • 'leer archivo [path]' → Filesystem"
echo ""
echo "📖 Para cargar contexto completo:"
echo "   @Auxiliares/Instalaciones/Ais/Gemini_CLI/GEMINI.md"
echo ""

# Configurar variables de entorno para MCPs
export MCP_CONFIG_PATH="$PROJECT_DIR/.mcp-config.json"
export GEMINI_MCP_CONFIG="$PROJECT_DIR/.gemini-mcp-config.json"

# Iniciar Gemini CLI con configuración MCP
if command -v gemini &> /dev/null; then
    gemini --config "$GEMINI_MCP_CONFIG"
else
    echo "❌ Error: Gemini CLI no encontrado"
    echo "   Instala primero: bash install-gemini-cli.sh"
    exit 1
fi
EOF

chmod +x "$SCRIPT_DIR/gemini-mcps-optimizado.sh"
show_result $? "Script de inicio con MCPs creado"

# Crear comandos automáticos para MCPs
cat > "$SCRIPT_DIR/mcp-commands.md" << 'EOF'
# 🤖 COMANDOS MCP AUTOMÁTICOS PARA GEMINI-ICFES-OPTIMIZADO

## 🔍 **Brave Search MCP**
### Activación Automática:
- "buscar información sobre [tema]"
- "investigar [concepto] ICFES 2025"
- "información actualizada sobre [competencia]"

### Ejemplos:
```
buscar información sobre competencias matemáticas ICFES 2025
investigar metodologías de evaluación argumentación matemática
información actualizada sobre estándares MEN matemáticas
```

## 📚 **Context7 MCP**
### Activación Automática:
- "documentación de [librería]"
- "API de [herramienta]"
- "referencia de [función]"

### Ejemplos:
```
documentación de R-exams para ejercicios matemáticos
API de TikZ para gráficas geométricas
referencia de matplotlib para gráficos estadísticos
```

## 🌐 **Playwright MCP**
### Activación Automática:
- "automatizar navegación web"
- "scraping de [sitio]"
- "testing de [aplicación]"

### Ejemplos:
```
automatizar navegación web para buscar ejemplos ICFES
scraping de sitio MEN para obtener documentación oficial
testing de compilación HTML de ejercicios R-exams
```

## 💾 **Memory MCP**
### Activación Automática:
- "recordar [información]"
- "guardar [dato]"
- "persistir [configuración]"

### Ejemplos:
```
recordar que este ejercicio evalúa competencia interpretación
guardar la configuración de tolerancias para ejercicios numéricos
persistir las mejores prácticas identificadas en este proyecto
```

## 📁 **Filesystem MCP**
### Activación Automática:
- "leer archivo [path]"
- "escribir en [directorio]"
- "listar archivos en [ubicación]"

### Ejemplos:
```
leer archivo Auxiliares/Ejemplos-Funcionales-Rmd/ejercicio.Rmd
escribir ejercicio en Lab-Manjaro/01-S1-2024B/
listar archivos en Auxiliares/TikZ-Documentation/
```

## 🎯 **Comandos Combinados (Flujos Inteligentes)**

### Crear Ejercicio desde Investigación:
```
1. "buscar información sobre competencia formulación ICFES 2025"
2. "documentación de R-exams para ejercicios de álgebra"
3. "leer archivo Auxiliares/Ejemplos-Funcionales-Rmd/algebra_ejemplo.Rmd"
4. "recordar las mejores prácticas identificadas"
```

### Optimizar Ejercicio Existente:
```
1. "leer archivo Lab-Manjaro/01-S1-2024B/ejercicio_actual.Rmd"
2. "buscar información sobre errores comunes en ejercicios ICFES"
3. "documentación de TikZ para mejorar gráficas"
4. "recordar las optimizaciones aplicadas"
```

### Validar Estándares ICFES:
```
1. "buscar información actualizada sobre estándares ICFES 2025"
2. "documentación oficial MEN competencias matemáticas"
3. "recordar criterios de validación identificados"
```
EOF

show_result $? "Guía de comandos MCP creada"

# Actualizar comando global
echo -n "Actualizando comando global: "
if [[ -f "$HOME/.local/bin/gemini-icfes-optimizado" ]]; then
    ln -sf "$SCRIPT_DIR/gemini-mcps-optimizado.sh" "$HOME/.local/bin/gemini-icfes-mcps"
    show_result $? "Comando gemini-icfes-mcps disponible"
else
    ln -sf "$SCRIPT_DIR/gemini-mcps-optimizado.sh" "$HOME/.local/bin/gemini-icfes-mcps"
    show_result $? "Comando gemini-icfes-mcps creado"
fi

# Crear archivo de configuración de VSCode para MCPs
cat > "$SCRIPT_DIR/vscode-mcp-tasks.json" << 'EOF'
{
    "version": "2.0.0",
    "tasks": [
        {
            "label": "🤖 Gemini CLI + MCPs",
            "type": "shell",
            "command": "bash",
            "args": ["${workspaceFolder}/Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-mcps-optimizado.sh"],
            "group": "build",
            "presentation": {
                "echo": true,
                "reveal": "always",
                "focus": false,
                "panel": "new"
            },
            "problemMatcher": []
        },
        {
            "label": "🔧 Configurar Variables MCP",
            "type": "shell",
            "command": "source",
            "args": ["${workspaceFolder}/Auxiliares/Instalaciones/Ais/Gemini_CLI/mcp-env-setup.sh"],
            "group": "build",
            "presentation": {
                "echo": true,
                "reveal": "always",
                "focus": false,
                "panel": "new"
            },
            "problemMatcher": []
        }
    ]
}
EOF

show_result $? "Tareas VSCode para MCPs creadas"

echo ""
echo -e "${GREEN}✅ CONFIGURACIÓN DE MCPs COMPLETADA${NC}"
echo "=================================="
echo ""
echo -e "${CYAN}🚀 Comando principal (con MCPs):${NC}"
echo "  gemini-icfes-mcps"
echo ""
echo -e "${CYAN}📋 Archivos creados:${NC}"
echo "  • $PROJECT_ROOT/.gemini-mcp-config.json"
echo "  • $SCRIPT_DIR/gemini-mcps-optimizado.sh"
echo "  • $SCRIPT_DIR/mcp-commands.md"
echo "  • $SCRIPT_DIR/vscode-mcp-tasks.json"
echo ""
echo -e "${CYAN}🔧 MCPs configurados:${NC}"
echo "  • ✅ Context7 - Activación automática con 'documentación'"
echo "  • ✅ Playwright - Activación automática con 'web/automatizar'"
echo "  • ✅ Memory - Activación automática con 'recordar/guardar'"
echo "  • ✅ Brave Search - Activación automática con 'buscar/investigar'"
echo "  • ✅ Filesystem - Activación automática con 'archivo/leer'"
echo ""
echo -e "${YELLOW}⚠️  PRÓXIMOS PASOS:${NC}"
echo "  1. Configurar APIs opcionales: source mcp-env-setup.sh"
echo "  2. Probar funcionamiento: gemini-icfes-mcps"
echo "  3. Revisar comandos: cat mcp-commands.md"
echo ""
echo -e "${GREEN}🎉 ¡MCPs completamente integrados con gemini-icfes-optimizado!${NC}"
