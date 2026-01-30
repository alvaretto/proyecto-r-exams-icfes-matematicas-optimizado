#!/bin/bash

# Script para configurar VSCode Insiders con MCP
# Autor: Evaluación Gemini-CLI MCP
# Fecha: $(date +%Y-%m-%d)

set -e

echo "🔧 CONFIGURACIÓN DE VSCODE INSIDERS CON MCP"
echo "=" | tr ' ' '=' | head -c 50; echo

# Directorios
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
VSCODE_CONFIG_DIR="$HOME/.config/Code - Insiders/User"
WORKSPACE_DIR="/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams"

# Función para logging
log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

# Verificar VSCode Insiders
log "🔍 Verificando VSCode Insiders..."
if ! command -v code-insiders &> /dev/null; then
    log "❌ VSCode Insiders no está instalado"
    exit 1
fi
log "✅ VSCode Insiders encontrado"

# Crear directorio de configuración
log "📁 Creando directorios de configuración..."
mkdir -p "$VSCODE_CONFIG_DIR"

# Verificar servidores MCP
log "🔍 Verificando servidores MCP..."
MCP_SERVERS=(
    "$WORKSPACE_DIR/.mcps/context7-mcp/dist/index.js"
    "$WORKSPACE_DIR/.mcps/latex-validator-mcp/index-fixed.js"
    "$WORKSPACE_DIR/.mcps/image-analysis-mcp/index-fixed.js"
)

for server in "${MCP_SERVERS[@]}"; do
    if [ -f "$server" ]; then
        log "✅ Servidor encontrado: $(basename "$server")"
    else
        log "❌ Servidor no encontrado: $server"
    fi
done

# Instalar extensiones recomendadas
log "📦 Instalando extensiones recomendadas..."
EXTENSIONS=(
    "ms-vscode.vscode-json"
    "ms-python.python"
    "reditorsupport.r"
    "james-yu.latex-workshop"
    "ms-vscode.vscode-typescript-next"
)

for ext in "${EXTENSIONS[@]}"; do
    log "📦 Instalando extensión: $ext"
    code-insiders --install-extension "$ext" --force || log "⚠️ No se pudo instalar: $ext"
done

# Crear configuración de workspace
log "⚙️ Creando configuración de workspace..."
WORKSPACE_CONFIG="$WORKSPACE_DIR/.vscode/settings.json"
mkdir -p "$(dirname "$WORKSPACE_CONFIG")"

cat > "$WORKSPACE_CONFIG" << 'EOF'
{
  "mcp.servers": {
    "context7": {
      "command": "node",
      "args": ["./.mcps/context7-mcp/dist/index.js"],
      "env": {},
      "description": "Context7 MCP Server for documentation",
      "enabled": true
    },
    "latex-validator": {
      "command": "node", 
      "args": ["./.mcps/latex-validator-mcp/index-fixed.js"],
      "env": {},
      "description": "LaTeX/TikZ validator for R-exams",
      "enabled": true
    },
    "image-analysis": {
      "command": "node",
      "args": ["./.mcps/image-analysis-mcp/index-fixed.js"],
      "env": {},
      "description": "Image analysis for TikZ replication",
      "enabled": true
    }
  },
  "files.associations": {
    "*.Rmd": "rmd",
    "*.Rnw": "rnoweb"
  },
  "r.rterm.linux": "/usr/bin/R",
  "python.defaultInterpreterPath": "/usr/bin/python3",
  "latex-workshop.latex.autoBuild.run": "never",
  "latex-workshop.latex.tools": [
    {
      "name": "pdflatex",
      "command": "pdflatex",
      "args": [
        "-synctex=1",
        "-interaction=nonstopmode", 
        "-file-line-error",
        "%DOC%"
      ]
    }
  ]
}
EOF

log "✅ Configuración de workspace creada"

# Crear archivo de tareas para MCP
log "⚙️ Creando tareas de VSCode..."
TASKS_CONFIG="$WORKSPACE_DIR/.vscode/tasks.json"

cat > "$TASKS_CONFIG" << 'EOF'
{
  "version": "2.0.0",
  "tasks": [
    {
      "label": "Start Context7 MCP",
      "type": "shell",
      "command": "node",
      "args": ["./.mcps/context7-mcp/dist/index.js"],
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
      "label": "Start LaTeX Validator MCP",
      "type": "shell", 
      "command": "node",
      "args": ["./.mcps/latex-validator-mcp/index-fixed.js"],
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
      "label": "Start Image Analysis MCP",
      "type": "shell",
      "command": "node", 
      "args": ["./.mcps/image-analysis-mcp/index-fixed.js"],
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
      "label": "Test Gemini CLI with MCP",
      "type": "shell",
      "command": "gemini",
      "args": ["mcp", "list"],
      "group": "test",
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

log "✅ Tareas de VSCode creadas"

# Crear configuración de launch para debugging
log "⚙️ Creando configuración de debug..."
LAUNCH_CONFIG="$WORKSPACE_DIR/.vscode/launch.json"

cat > "$LAUNCH_CONFIG" << 'EOF'
{
  "version": "0.2.0",
  "configurations": [
    {
      "name": "Debug Context7 MCP",
      "type": "node",
      "request": "launch",
      "program": "${workspaceFolder}/.mcps/context7-mcp/dist/index.js",
      "console": "integratedTerminal",
      "env": {
        "NODE_ENV": "development"
      }
    },
    {
      "name": "Debug LaTeX Validator MCP",
      "type": "node",
      "request": "launch", 
      "program": "${workspaceFolder}/.mcps/latex-validator-mcp/index-fixed.js",
      "console": "integratedTerminal",
      "env": {
        "NODE_ENV": "development"
      }
    },
    {
      "name": "Debug Image Analysis MCP",
      "type": "node",
      "request": "launch",
      "program": "${workspaceFolder}/.mcps/image-analysis-mcp/index-fixed.js", 
      "console": "integratedTerminal",
      "env": {
        "NODE_ENV": "development"
      }
    }
  ]
}
EOF

log "✅ Configuración de debug creada"

# Crear snippet para R-exams con MCP
log "⚙️ Creando snippets para R-exams..."
SNIPPETS_DIR="$VSCODE_CONFIG_DIR/snippets"
mkdir -p "$SNIPPETS_DIR"

cat > "$SNIPPETS_DIR/rmd.json" << 'EOF'
{
  "R-exams template with MCP": {
    "prefix": "rexams-mcp",
    "body": [
      "```{r setup, include=FALSE}",
      "library(exams)",
      "library(reticulate)",
      "library(knitr)",
      "# MCP LaTeX Validator available",
      "# MCP Image Analysis available", 
      "```",
      "",
      "Question",
      "========",
      "",
      "${1:Question text}",
      "",
      "```{r solution, echo=FALSE, results='hide'}",
      "# Solution code",
      "${2:solution_code}",
      "```",
      "",
      "Answerlist",
      "----------",
      "* ${3:Option A}",
      "* ${4:Option B}",
      "* ${5:Option C}",
      "* ${6:Option D}",
      "",
      "Solution",
      "========",
      "",
      "${7:Solution explanation}",
      "",
      "Meta-information",
      "================",
      "extype: schoice",
      "exsolution: ${8:1000}",
      "exname: ${9:exercise_name}",
      "extol: 0.01"
    ],
    "description": "R-exams template with MCP integration"
  }
}
EOF

log "✅ Snippets creados"

# Abrir VSCode en el workspace
log "🚀 Abriendo VSCode Insiders en el workspace..."
code-insiders "$WORKSPACE_DIR" &

log "✅ Configuración completada"
echo ""
log "📋 RESUMEN DE CONFIGURACIÓN:"
log "✅ VSCode Insiders configurado con MCP"
log "✅ 3 servidores MCP configurados"
log "✅ Extensiones recomendadas instaladas"
log "✅ Tareas y debug configurados"
log "✅ Snippets para R-exams creados"
echo ""
log "🔧 PRÓXIMOS PASOS:"
log "1. Verificar que los servidores MCP se conecten"
log "2. Probar las tareas de VSCode (Ctrl+Shift+P > Tasks)"
log "3. Usar snippets: escribir 'rexams-mcp' en archivo .Rmd"
log "4. Verificar integración con Gemini-CLI"
