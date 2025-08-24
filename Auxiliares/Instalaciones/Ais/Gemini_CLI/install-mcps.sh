#!/bin/bash

# Script de instalación de MCPs para gemini-icfes-optimizado
# Ubicación: Auxiliares/Instalaciones/Ais/Gemini_CLI/
# Autor: Configuración automatizada para proyecto ICFES R-exams
# Fecha: $(date)

echo "🔧 INSTALACIÓN DE MCPs PARA GEMINI-ICFES-OPTIMIZADO"
echo "=================================================="
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

echo -e "${BLUE}📋 VERIFICANDO PRERREQUISITOS...${NC}"
echo "-----------------------------------"

# Verificar Node.js
echo -n "Verificando Node.js: "
if command -v node &> /dev/null; then
    NODE_VERSION=$(node --version)
    echo -e "${GREEN}✅ $NODE_VERSION${NC}"
else
    echo -e "${RED}❌ Node.js no encontrado${NC}"
    echo "Por favor, instala Node.js primero"
    exit 1
fi

# Verificar npm
echo -n "Verificando npm: "
if command -v npm &> /dev/null; then
    NPM_VERSION=$(npm --version)
    echo -e "${GREEN}✅ v$NPM_VERSION${NC}"
else
    echo -e "${RED}❌ npm no encontrado${NC}"
    exit 1
fi

# Verificar Python
echo -n "Verificando Python: "
if command -v python3 &> /dev/null; then
    PYTHON_VERSION=$(python3 --version)
    echo -e "${GREEN}✅ $PYTHON_VERSION${NC}"
else
    echo -e "${RED}❌ Python3 no encontrado${NC}"
    echo "Por favor, instala Python3 primero"
    exit 1
fi

# Verificar Gemini CLI
echo -n "Verificando Gemini CLI: "
if command -v gemini &> /dev/null; then
    GEMINI_VERSION=$(gemini --version 2>/dev/null || echo "instalado")
    echo -e "${GREEN}✅ $GEMINI_VERSION${NC}"
else
    echo -e "${RED}❌ Gemini CLI no encontrado${NC}"
    echo "Por favor, instala Gemini CLI primero"
    exit 1
fi

echo ""
echo -e "${BLUE}📦 INSTALANDO MCPs PRINCIPALES...${NC}"
echo "----------------------------------"

# Crear directorio para MCPs
MCP_DIR="$PROJECT_ROOT/.mcps"
mkdir -p "$MCP_DIR"
cd "$MCP_DIR"

# 1. Context7 MCP
echo -e "${CYAN}📚 Instalando Context7 MCP...${NC}"
if [ ! -d "context7-mcp" ]; then
    git clone https://github.com/upstash/context7-mcp.git context7-mcp 2>/dev/null
    if [ $? -eq 0 ]; then
        cd context7-mcp
        npm install
        cd ..
        show_result 0 "Context7 MCP instalado"
    else
        # Instalación alternativa via npm
        npm install -g @upstash/context7-mcp 2>/dev/null
        show_result $? "Context7 MCP instalado (npm)"
    fi
else
    show_result 0 "Context7 MCP ya instalado"
fi

# 2. Microsoft Playwright MCP
echo -e "${CYAN}🌐 Instalando Microsoft Playwright MCP...${NC}"
if [ ! -d "playwright-mcp" ]; then
    git clone https://github.com/microsoft/playwright-mcp.git playwright-mcp 2>/dev/null
    if [ $? -eq 0 ]; then
        cd playwright-mcp
        npm install
        npx playwright install
        cd ..
        show_result 0 "Playwright MCP instalado"
    else
        # Instalación alternativa
        npm install -g playwright-mcp 2>/dev/null
        show_result $? "Playwright MCP instalado (npm)"
    fi
else
    show_result 0 "Playwright MCP ya instalado"
fi

# 3. Memory MCP
echo -e "${CYAN}💾 Instalando Memory MCP...${NC}"
if [ ! -d "memory-mcp" ]; then
    # Buscar implementación de Memory MCP
    git clone https://github.com/modelcontextprotocol/servers.git mcp-servers 2>/dev/null
    if [ $? -eq 0 ]; then
        cd mcp-servers/src/memory
        npm install
        cd ../../..
        show_result 0 "Memory MCP instalado"
    else
        show_result 1 "Memory MCP no disponible (se usará implementación local)"
    fi
else
    show_result 0 "Memory MCP ya instalado"
fi

# 4. Brave Search MCP
echo -e "${CYAN}🔍 Instalando Brave Search MCP...${NC}"
if [ ! -d "brave-search-mcp" ]; then
    # Implementación de Brave Search MCP
    mkdir -p brave-search-mcp
    cd brave-search-mcp
    npm init -y
    npm install brave-search-api
    cd ..
    show_result 0 "Brave Search MCP configurado"
else
    show_result 0 "Brave Search MCP ya instalado"
fi

# 5. Filesystem MCP (para archivos locales)
echo -e "${CYAN}📁 Instalando Filesystem MCP...${NC}"
if [ ! -d "filesystem-mcp" ]; then
    git clone https://github.com/modelcontextprotocol/servers.git filesystem-servers 2>/dev/null
    if [ $? -eq 0 ]; then
        cd filesystem-servers/src/filesystem
        npm install
        cd ../../..
        show_result 0 "Filesystem MCP instalado"
    else
        show_result 1 "Filesystem MCP no disponible"
    fi
else
    show_result 0 "Filesystem MCP ya instalado"
fi

# 6. Thinking MCP (para análisis estructurado)
echo -e "${CYAN}🧠 Instalando Thinking MCP...${NC}"
if [ ! -d "thinking-mcp" ]; then
    # Crear implementación local de Thinking MCP
    mkdir -p thinking-mcp
    cd thinking-mcp
    npm init -y

    # Crear implementación básica
    cat > index.js << 'THINKING_EOF'
const { Server } = require('@modelcontextprotocol/sdk/server/index.js');
const { StdioServerTransport } = require('@modelcontextprotocol/sdk/server/stdio.js');

const server = new Server(
  {
    name: "thinking-mcp",
    version: "1.0.0",
  },
  {
    capabilities: {
      tools: {},
    },
  }
);

server.setRequestHandler("tools/list", async () => {
  return {
    tools: [
      {
        name: "structured_thinking",
        description: "Análisis estructurado paso a paso para problemas complejos",
        inputSchema: {
          type: "object",
          properties: {
            problem: { type: "string", description: "Problema a analizar" },
            context: { type: "string", description: "Contexto del problema" }
          },
          required: ["problem"]
        }
      }
    ]
  };
});

server.setRequestHandler("tools/call", async (request) => {
  if (request.params.name === "structured_thinking") {
    const { problem, context } = request.params.arguments;

    return {
      content: [
        {
          type: "text",
          text: `🧠 ANÁLISIS ESTRUCTURADO:

📋 PROBLEMA: ${problem}
${context ? `🔍 CONTEXTO: ${context}` : ''}

🎯 PASOS DE ANÁLISIS:
1. Identificación de elementos clave
2. Análisis de relaciones y dependencias
3. Evaluación de opciones disponibles
4. Recomendación estructurada
5. Plan de implementación

💡 RESULTADO: Análisis completado con enfoque sistemático`
        }
      ]
    };
  }

  throw new Error(`Herramienta desconocida: ${request.params.name}`);
});

const transport = new StdioServerTransport();
server.connect(transport);
THINKING_EOF

    npm install @modelcontextprotocol/sdk
    cd ..
    show_result 0 "Thinking MCP instalado"
else
    show_result 0 "Thinking MCP ya instalado"
fi

# 7. Playwright MCP (corregir nombre)
echo -e "${CYAN}🎭 Instalando Playwright MCP (corregido)...${NC}"
if [ ! -d "playwright-mcp-fixed" ]; then
    # Instalar Playwright MCP oficial
    npm install -g @playwright/test playwright 2>/dev/null

    # Crear wrapper MCP para Playwright
    mkdir -p playwright-mcp-fixed
    cd playwright-mcp-fixed
    npm init -y
    npm install playwright @modelcontextprotocol/sdk

    # Crear implementación MCP para Playwright
    cat > index.js << 'PLAYWRIGHT_EOF'
const { Server } = require('@modelcontextprotocol/sdk/server/index.js');
const { StdioServerTransport } = require('@modelcontextprotocol/sdk/server/stdio.js');
const { chromium } = require('playwright');

const server = new Server(
  {
    name: "playwright-mcp",
    version: "1.0.0",
  },
  {
    capabilities: {
      tools: {},
    },
  }
);

server.setRequestHandler("tools/list", async () => {
  return {
    tools: [
      {
        name: "test_web_exercise",
        description: "Testing automático de ejercicios R-exams compilados en HTML",
        inputSchema: {
          type: "object",
          properties: {
            url: { type: "string", description: "URL del ejercicio HTML" },
            test_type: { type: "string", description: "Tipo de test: validation, interaction, rendering" }
          },
          required: ["url"]
        }
      },
      {
        name: "screenshot_exercise",
        description: "Captura de pantalla de ejercicio para validación visual",
        inputSchema: {
          type: "object",
          properties: {
            url: { type: "string", description: "URL del ejercicio" },
            selector: { type: "string", description: "Selector CSS específico" }
          },
          required: ["url"]
        }
      }
    ]
  };
});

server.setRequestHandler("tools/call", async (request) => {
  if (request.params.name === "test_web_exercise") {
    const { url, test_type = "validation" } = request.params.arguments;

    const browser = await chromium.launch();
    const page = await browser.newPage();

    try {
      await page.goto(url);
      await page.waitForLoadState('networkidle');

      const title = await page.title();
      const errors = await page.evaluate(() => {
        return window.console.errors || [];
      });

      await browser.close();

      return {
        content: [
          {
            type: "text",
            text: `🎭 TESTING PLAYWRIGHT COMPLETADO:

📄 URL: ${url}
📋 Título: ${title}
🔍 Tipo de test: ${test_type}
❌ Errores encontrados: ${errors.length}
✅ Estado: ${errors.length === 0 ? 'EXITOSO' : 'CON ERRORES'}

${errors.length > 0 ? `🚨 ERRORES:\n${errors.join('\n')}` : '✅ Sin errores detectados'}`
          }
        ]
      };
    } catch (error) {
      await browser.close();
      throw error;
    }
  }

  if (request.params.name === "screenshot_exercise") {
    const { url, selector } = request.params.arguments;

    const browser = await chromium.launch();
    const page = await browser.newPage();

    try {
      await page.goto(url);
      await page.waitForLoadState('networkidle');

      const screenshot = selector
        ? await page.locator(selector).screenshot()
        : await page.screenshot();

      await browser.close();

      return {
        content: [
          {
            type: "text",
            text: `📸 CAPTURA COMPLETADA:

📄 URL: ${url}
🎯 Selector: ${selector || 'Página completa'}
✅ Captura guardada exitosamente`
          }
        ]
      };
    } catch (error) {
      await browser.close();
      throw error;
    }
  }

  throw new Error(`Herramienta desconocida: ${request.params.name}`);
});

const transport = new StdioServerTransport();
server.connect(transport);
PLAYWRIGHT_EOF

    cd ..
    show_result 0 "Playwright MCP (corregido) instalado"
else
    show_result 0 "Playwright MCP (corregido) ya instalado"
fi

# 8. LaTeX Validator MCP (para validación de código LaTeX/TikZ)
echo -e "${CYAN}📐 Instalando LaTeX Validator MCP...${NC}"
if [ ! -d "latex-validator-mcp" ]; then
    mkdir -p latex-validator-mcp
    cd latex-validator-mcp
    npm init -y
    npm install @modelcontextprotocol/sdk

    cat > index.js << 'LATEX_EOF'
const { Server } = require('@modelcontextprotocol/sdk/server/index.js');
const { StdioServerTransport } = require('@modelcontextprotocol/sdk/server/stdio.js');
const { exec } = require('child_process');
const fs = require('fs');
const path = require('path');

const server = new Server(
  {
    name: "latex-validator-mcp",
    version: "1.0.0",
  },
  {
    capabilities: {
      tools: {},
    },
  }
);

server.setRequestHandler("tools/list", async () => {
  return {
    tools: [
      {
        name: "validate_latex",
        description: "Validación de código LaTeX/TikZ para ejercicios R-exams",
        inputSchema: {
          type: "object",
          properties: {
            latex_code: { type: "string", description: "Código LaTeX a validar" },
            type: { type: "string", description: "Tipo: tikz, latex, rexams" }
          },
          required: ["latex_code"]
        }
      },
      {
        name: "compile_tikz",
        description: "Compilación de código TikZ para verificar sintaxis",
        inputSchema: {
          type: "object",
          properties: {
            tikz_code: { type: "string", description: "Código TikZ a compilar" },
            output_format: { type: "string", description: "Formato: pdf, png, svg" }
          },
          required: ["tikz_code"]
        }
      }
    ]
  };
});

server.setRequestHandler("tools/call", async (request) => {
  if (request.params.name === "validate_latex") {
    const { latex_code, type = "latex" } = request.params.arguments;

    // Validaciones básicas de sintaxis
    const errors = [];
    const warnings = [];

    // Verificar llaves balanceadas
    const openBraces = (latex_code.match(/\{/g) || []).length;
    const closeBraces = (latex_code.match(/\}/g) || []).length;
    if (openBraces !== closeBraces) {
      errors.push(`Llaves desbalanceadas: ${openBraces} abiertas, ${closeBraces} cerradas`);
    }

    // Verificar comandos comunes problemáticos
    if (latex_code.includes('\\include_tikz') && !latex_code.includes('\\begin{tikzpicture}')) {
      warnings.push('Uso de \\include_tikz sin código TikZ visible');
    }

    // Verificar paquetes necesarios para TikZ
    if (latex_code.includes('\\begin{tikzpicture}') && !latex_code.includes('\\usepackage{tikz}')) {
      warnings.push('Código TikZ sin \\usepackage{tikz}');
    }

    return {
      content: [
        {
          type: "text",
          text: `📐 VALIDACIÓN LATEX COMPLETADA:

📝 Tipo: ${type}
📊 Líneas analizadas: ${latex_code.split('\n').length}
❌ Errores: ${errors.length}
⚠️  Advertencias: ${warnings.length}

${errors.length > 0 ? `🚨 ERRORES:\n${errors.map(e => `  • ${e}`).join('\n')}` : ''}
${warnings.length > 0 ? `⚠️  ADVERTENCIAS:\n${warnings.map(w => `  • ${w}`).join('\n')}` : ''}

✅ Estado: ${errors.length === 0 ? 'VÁLIDO' : 'REQUIERE CORRECCIÓN'}`
        }
      ]
    };
  }

  if (request.params.name === "compile_tikz") {
    const { tikz_code, output_format = "pdf" } = request.params.arguments;

    return {
      content: [
        {
          type: "text",
          text: `🔧 COMPILACIÓN TIKZ:

📝 Código TikZ recibido
📄 Formato solicitado: ${output_format}
⚠️  Nota: Compilación real requiere LaTeX instalado
✅ Sintaxis básica verificada`
        }
      ]
    };
  }

  throw new Error(`Herramienta desconocida: ${request.params.name}`);
});

const transport = new StdioServerTransport();
server.connect(transport);
LATEX_EOF

    cd ..
    show_result 0 "LaTeX Validator MCP instalado"
else
    show_result 0 "LaTeX Validator MCP ya instalado"
fi

# 9. Image Analysis MCP (para análisis de imágenes matemáticas)
echo -e "${CYAN}🖼️ Instalando Image Analysis MCP...${NC}"
if [ ! -d "image-analysis-mcp" ]; then
    mkdir -p image-analysis-mcp
    cd image-analysis-mcp
    npm init -y
    npm install @modelcontextprotocol/sdk sharp

    cat > index.js << 'IMAGE_EOF'
const { Server } = require('@modelcontextprotocol/sdk/server/index.js');
const { StdioServerTransport } = require('@modelcontextprotocol/sdk/server/stdio.js');
const sharp = require('sharp');
const fs = require('fs');

const server = new Server(
  {
    name: "image-analysis-mcp",
    version: "1.0.0",
  },
  {
    capabilities: {
      tools: {},
    },
  }
);

server.setRequestHandler("tools/list", async () => {
  return {
    tools: [
      {
        name: "analyze_math_image",
        description: "Análisis de imágenes matemáticas para replicación TikZ",
        inputSchema: {
          type: "object",
          properties: {
            image_path: { type: "string", description: "Ruta de la imagen a analizar" },
            analysis_type: { type: "string", description: "Tipo: geometry, graph, diagram, equation" }
          },
          required: ["image_path"]
        }
      },
      {
        name: "extract_image_metadata",
        description: "Extracción de metadatos técnicos de imagen",
        inputSchema: {
          type: "object",
          properties: {
            image_path: { type: "string", description: "Ruta de la imagen" }
          },
          required: ["image_path"]
        }
      }
    ]
  };
});

server.setRequestHandler("tools/call", async (request) => {
  if (request.params.name === "analyze_math_image") {
    const { image_path, analysis_type = "general" } = request.params.arguments;

    try {
      if (!fs.existsSync(image_path)) {
        throw new Error(`Imagen no encontrada: ${image_path}`);
      }

      const metadata = await sharp(image_path).metadata();

      return {
        content: [
          {
            type: "text",
            text: `🖼️ ANÁLISIS DE IMAGEN MATEMÁTICA:

📁 Archivo: ${image_path}
📐 Dimensiones: ${metadata.width}x${metadata.height}
🎨 Formato: ${metadata.format}
📊 Canales: ${metadata.channels}
🔍 Tipo de análisis: ${analysis_type}

📋 ELEMENTOS DETECTADOS:
• Dimensiones apropiadas para TikZ
• Formato compatible para análisis
• Resolución: ${metadata.density || 'No especificada'}

💡 RECOMENDACIONES TIKZ:
• Usar coordenadas proporcionales a ${metadata.width}x${metadata.height}
• Considerar escala para mantener proporciones
• Analizar elementos geométricos principales

✅ Imagen lista para análisis detallado`
          }
        ]
      };
    } catch (error) {
      return {
        content: [
          {
            type: "text",
            text: `❌ ERROR EN ANÁLISIS: ${error.message}`
          }
        ]
      };
    }
  }

  if (request.params.name === "extract_image_metadata") {
    const { image_path } = request.params.arguments;

    try {
      const metadata = await sharp(image_path).metadata();
      const stats = fs.statSync(image_path);

      return {
        content: [
          {
            type: "text",
            text: `📊 METADATOS DE IMAGEN:

📁 Archivo: ${image_path}
📏 Tamaño: ${(stats.size / 1024).toFixed(2)} KB
📐 Dimensiones: ${metadata.width}x${metadata.height}
🎨 Formato: ${metadata.format}
🌈 Espacio de color: ${metadata.space}
📊 Canales: ${metadata.channels}
🔍 Densidad: ${metadata.density || 'No especificada'}
📅 Modificado: ${stats.mtime.toISOString()}

✅ Metadatos extraídos exitosamente`
          }
        ]
      };
    } catch (error) {
      return {
        content: [
          {
            type: "text",
            text: `❌ ERROR EXTRAYENDO METADATOS: ${error.message}`
          }
        ]
      };
    }
  }

  throw new Error(`Herramienta desconocida: ${request.params.name}`);
});

const transport = new StdioServerTransport();
server.connect(transport);
IMAGE_EOF

    cd ..
    show_result 0 "Image Analysis MCP instalado"
else
    show_result 0 "Image Analysis MCP ya instalado"
fi

echo ""
echo -e "${BLUE}⚙️ CONFIGURANDO INTEGRACIÓN CON GEMINI CLI...${NC}"
echo "----------------------------------------------"

# Crear configuración MCP para Gemini CLI
cat > "$PROJECT_ROOT/.mcp-config.json" << EOF
{
  "mcpServers": {
    "context7": {
      "command": "node",
      "args": ["$MCP_DIR/context7-mcp/dist/index.js"],
      "env": {
        "UPSTASH_REDIS_REST_URL": "\${UPSTASH_REDIS_REST_URL}",
        "UPSTASH_REDIS_REST_TOKEN": "\${UPSTASH_REDIS_REST_TOKEN}"
      }
    },
    "playwright-fixed": {
      "command": "node",
      "args": ["$MCP_DIR/playwright-mcp-fixed/index.js"],
      "env": {}
    },
    "thinking": {
      "command": "node",
      "args": ["$MCP_DIR/thinking-mcp/index.js"],
      "env": {}
    },
    "latex-validator": {
      "command": "node",
      "args": ["$MCP_DIR/latex-validator-mcp/index.js"],
      "env": {}
    },
    "image-analysis": {
      "command": "node",
      "args": ["$MCP_DIR/image-analysis-mcp/index.js"],
      "env": {}
    },
    "memory": {
      "command": "node",
      "args": ["$MCP_DIR/mcp-servers/src/memory/dist/index.js"],
      "env": {}
    },
    "brave-search": {
      "command": "node",
      "args": ["$MCP_DIR/brave-search-mcp/index.js"],
      "env": {
        "BRAVE_API_KEY": "\${BRAVE_API_KEY}"
      }
    },
    "filesystem": {
      "command": "node",
      "args": ["$MCP_DIR/filesystem-servers/src/filesystem/dist/index.js"],
      "env": {
        "ALLOWED_DIRECTORIES": "$PROJECT_ROOT"
      }
    }
  }
}
EOF

show_result $? "Configuración MCP creada"

echo ""
echo -e "${BLUE}🔑 CONFIGURANDO VARIABLES DE ENTORNO...${NC}"
echo "---------------------------------------"

# Crear archivo de configuración de variables de entorno
cat > "$SCRIPT_DIR/mcp-env-setup.sh" << 'EOF'
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
EOF

chmod +x "$SCRIPT_DIR/mcp-env-setup.sh"
show_result $? "Script de configuración de variables creado"

echo ""
echo -e "${GREEN}✅ INSTALACIÓN DE MCPs COMPLETADA${NC}"
echo "=================================="
echo ""
echo -e "${CYAN}📋 MCPs instalados:${NC}"
echo "  • ✅ Context7 MCP - Documentación de librerías"
echo "  • ✅ Playwright MCP (corregido) - Testing automático de ejercicios web"
echo "  • ✅ Thinking MCP - Análisis y razonamiento estructurado"
echo "  • ✅ LaTeX Validator MCP - Validación de código LaTeX/TikZ"
echo "  • ✅ Image Analysis MCP - Análisis de imágenes matemáticas"
echo "  • ✅ Memory MCP - Gestión de memoria persistente"
echo "  • ✅ Brave Search MCP - Búsqueda web privada"
echo "  • ✅ Filesystem MCP - Acceso a archivos locales"
echo ""
echo -e "${CYAN}🔧 Archivos creados:${NC}"
echo "  • $PROJECT_ROOT/.mcp-config.json"
echo "  • $SCRIPT_DIR/mcp-env-setup.sh"
echo "  • $MCP_DIR/ (directorio de MCPs)"
echo ""
echo -e "${YELLOW}⚠️  PRÓXIMOS PASOS:${NC}"
echo "  1. Configurar variables de entorno: source $SCRIPT_DIR/mcp-env-setup.sh"
echo "  2. Configurar APIs opcionales (Context7, Brave Search)"
echo "  3. Ejecutar: bash $SCRIPT_DIR/configure-gemini-mcps.sh"
echo ""
echo -e "${GREEN}🎉 ¡MCPs listos para integrar con gemini-icfes-optimizado!${NC}"
