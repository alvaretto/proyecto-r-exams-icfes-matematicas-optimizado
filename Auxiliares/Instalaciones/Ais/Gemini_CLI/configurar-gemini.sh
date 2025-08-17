#!/bin/bash

# Script de configuración para Gemini CLI con archivos personalizados
# Ubicación: Auxiliares/Instalaciones/Ais/Gemini_CLI/
# Autor: Configuración automatizada para proyecto ICFES R-exams
# Fecha: $(date)

echo "🤖 CONFIGURACIÓN GEMINI CLI PARA PROYECTO ICFES R-EXAMS"
echo "======================================================="
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

# Obtener directorio del script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

echo -e "${BLUE}📁 VERIFICANDO ESTRUCTURA DE ARCHIVOS...${NC}"
echo "-------------------------------------------"

# Verificar archivos de configuración Gemini
echo -n "Verificando rules-gemini.md: "
if [ -f "$SCRIPT_DIR/rules-gemini.md" ]; then
    echo -e "${GREEN}✅ Encontrado${NC}"
else
    echo -e "${RED}❌ No encontrado${NC}"
    exit 1
fi

echo -n "Verificando task-list-gemini.md: "
if [ -f "$SCRIPT_DIR/task-list-gemini.md" ]; then
    echo -e "${GREEN}✅ Encontrado${NC}"
else
    echo -e "${RED}❌ No encontrado${NC}"
    exit 1
fi

echo -n "Verificando GEMINI.md: "
if [ -f "$SCRIPT_DIR/GEMINI.md" ]; then
    echo -e "${GREEN}✅ Encontrado${NC}"
else
    echo -e "${RED}❌ No encontrado${NC}"
    exit 1
fi

echo ""
echo -e "${BLUE}🔧 CONFIGURANDO GEMINI CLI...${NC}"
echo "------------------------------"

# Verificar que Gemini CLI está instalado
echo -n "Verificando instalación Gemini CLI: "
if command -v gemini &> /dev/null; then
    GEMINI_VERSION=$(gemini --version 2>/dev/null)
    echo -e "${GREEN}✅ v$GEMINI_VERSION${NC}"
else
    echo -e "${RED}❌ No instalado${NC}"
    echo "Por favor, ejecuta primero: bash $SCRIPT_DIR/install-gemini-cli.sh"
    exit 1
fi

# Verificar API Key
echo -n "Verificando API Key: "
if [ -n "$GEMINI_API_KEY" ]; then
    echo -e "${GREEN}✅ Configurada${NC}"
else
    echo -e "${RED}❌ No configurada${NC}"
    echo "Por favor, configura tu API Key primero"
    exit 1
fi

echo ""
echo -e "${BLUE}📋 CREANDO CONFIGURACIÓN PERSONALIZADA...${NC}"
echo "-------------------------------------------"

# Crear directorio de configuración Gemini si no existe
GEMINI_CONFIG_DIR="$HOME/.config/gemini"
mkdir -p "$GEMINI_CONFIG_DIR"

# Crear archivo de configuración personalizado
cat > "$GEMINI_CONFIG_DIR/icfes-config.json" << EOF
{
  "project_name": "RepositorioMatematicasICFES_R_Exams",
  "project_root": "$PROJECT_ROOT",
  "rules_file": "$SCRIPT_DIR/rules-gemini.md",
  "tasks_file": "$SCRIPT_DIR/task-list-gemini.md",
  "context_file": "$SCRIPT_DIR/GEMINI.md",
  "examples_path": "$PROJECT_ROOT/Auxiliares/Ejemplos-Funcionales-Rmd",
  "tikz_docs": "$PROJECT_ROOT/Auxiliares/TikZ-Documentation",
  "python_docs": "$PROJECT_ROOT/Auxiliares/Python-Documentation",
  "default_model": "gemini-2.5-pro",
  "context_window": "1M_tokens",
  "tools_enabled": {
    "web_search": true,
    "file_operations": true,
    "code_execution": true,
    "image_analysis": true
  },
  "icfes_competencias": [
    "interpretacion_representacion",
    "formulacion_ejecucion", 
    "argumentacion"
  ],
  "niveles_dificultad": [1, 2, 3, 4],
  "formatos_salida": ["html", "pdf", "word", "moodle"]
}
EOF

show_result $? "Configuración personalizada creada"

# Crear script de inicio rápido
cat > "$SCRIPT_DIR/iniciar-gemini-icfes.sh" << 'EOF'
#!/bin/bash

# Script de inicio rápido para Gemini CLI con configuración ICFES
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "🤖 INICIANDO GEMINI CLI PARA PROYECTO ICFES R-EXAMS"
echo "=================================================="
echo ""
echo "📋 Configuración cargada:"
echo "  • Reglas: rules-gemini.md"
echo "  • Tareas: task-list-gemini.md" 
echo "  • Contexto: GEMINI.md"
echo ""
echo "🚀 Comandos rápidos disponibles:"
echo "  • Análisis: 'Analiza este ejercicio: @path/to/file.Rmd'"
echo "  • Creación: 'Crea ejercicio desde esta imagen'"
echo "  • Optimización: 'Optimiza este código manteniendo calidad'"
echo ""
echo "💡 Para cargar contexto completo, usa:"
echo "   '@Auxiliares/Instalaciones/Ais/Gemini_CLI/GEMINI.md'"
echo ""

# Cambiar al directorio del proyecto
cd "$SCRIPT_DIR/../../../.."

# Iniciar Gemini CLI
gemini
EOF

chmod +x "$SCRIPT_DIR/iniciar-gemini-icfes.sh"
show_result $? "Script de inicio rápido creado"

# Crear alias para comandos frecuentes
cat > "$SCRIPT_DIR/comandos-gemini-icfes.md" << 'EOF'
# 🤖 COMANDOS GEMINI CLI - PROYECTO ICFES R-EXAMS

## 🚀 COMANDOS DE INICIO

### Cargar Contexto Completo
```
@Auxiliares/Instalaciones/Ais/Gemini_CLI/GEMINI.md
```

### Cargar Reglas Específicas
```
@Auxiliares/Instalaciones/Ais/Gemini_CLI/rules-gemini.md
```

### Cargar Plan de Tareas
```
@Auxiliares/Instalaciones/Ais/Gemini_CLI/task-list-gemini.md
```

## 🔍 COMANDOS DE ANÁLISIS

### Analizar Ejercicio Existente
```
Analiza este ejercicio: @path/to/ejercicio.Rmd
```

### Identificar Competencia ICFES
```
Identifica qué competencia ICFES evalúa mejor este ejercicio
```

### Revisar Calidad Pedagógica
```
Revisa la calidad pedagógica y técnica de este ejercicio
```

## 🎨 COMANDOS DE CREACIÓN

### Crear desde Imagen
```
Analiza esta imagen y aplica el sistema condicional automático
```

### Generar Ejercicio Completo
```
Crea un ejercicio ICFES completo siguiendo las mejores prácticas
```

### Replicar con TikZ
```
Replica esta imagen con TikZ manteniendo 98% fidelidad
```

## 🔧 COMANDOS DE OPTIMIZACIÓN

### Corregir Errores
```
Identifica y corrige errores siguiendo la metodología del proyecto
```

### Optimizar Código
```
Optimiza este código manteniendo la calidad pedagógica
```

### Validar Estándares
```
Valida que cumple estándares ICFES y genera 300+ versiones
```

## 🌐 COMANDOS DE INVESTIGACIÓN

### Buscar Información ICFES
```
Busca información actualizada sobre [competencia] ICFES 2025
```

### Validar Documentación Oficial
```
Valida que este ejercicio cumple estándares oficiales MEN
```

## 🧪 COMANDOS DE TESTING

### Probar Compilación
```
Compila este ejercicio en HTML, PDF y Word
```

### Validar Diversidad
```
Verifica que genera 300+ versiones únicas
```

### Testing Completo
```
Ejecuta testing completo de calidad y funcionamiento
```
EOF

show_result $? "Guía de comandos creada"

echo ""
echo -e "${BLUE}🎯 CONFIGURANDO ACCESOS DIRECTOS...${NC}"
echo "-----------------------------------"

# Crear enlace simbólico en directorio de usuario para acceso rápido
ln -sf "$SCRIPT_DIR/iniciar-gemini-icfes.sh" "$HOME/.local/bin/gemini-icfes" 2>/dev/null
if [ $? -eq 0 ]; then
    echo -e "${GREEN}✅ Comando 'gemini-icfes' disponible globalmente${NC}"
else
    echo -e "${YELLOW}⚠️  No se pudo crear comando global (opcional)${NC}"
fi

echo ""
echo -e "${BLUE}✅ CONFIGURACIÓN COMPLETADA${NC}"
echo "=========================="
echo ""
echo -e "${GREEN}🎉 GEMINI CLI CONFIGURADO PARA PROYECTO ICFES R-EXAMS${NC}"
echo ""
echo -e "${CYAN}📋 Archivos creados:${NC}"
echo "  • $GEMINI_CONFIG_DIR/icfes-config.json"
echo "  • $SCRIPT_DIR/iniciar-gemini-icfes.sh"
echo "  • $SCRIPT_DIR/comandos-gemini-icfes.md"
echo ""
echo -e "${CYAN}🚀 Para empezar:${NC}"
echo "  1. Ejecuta: bash $SCRIPT_DIR/iniciar-gemini-icfes.sh"
echo "  2. O usa: gemini-icfes (si está en PATH)"
echo "  3. Carga contexto: @Auxiliares/Instalaciones/Ais/Gemini_CLI/GEMINI.md"
echo ""
echo -e "${CYAN}📖 Documentación:${NC}"
echo "  • Reglas: $SCRIPT_DIR/rules-gemini.md"
echo "  • Tareas: $SCRIPT_DIR/task-list-gemini.md"
echo "  • Comandos: $SCRIPT_DIR/comandos-gemini-icfes.md"
echo ""
echo -e "${GREEN}¡Listo para crear ejercicios ICFES de alta calidad con Gemini CLI!${NC}"
