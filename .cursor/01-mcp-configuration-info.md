# Configuración de Model Context Protocol (MCP) - Proyecto ICFES R-Exams

## MCPs Instalados y Configurados

### 1. Sequential Thinking MCP 
- **Propósito**: Razonamiento paso a paso para problemas matemáticos complejos
- **Comando**: `npx -y @modelcontextprotocol/server-sequential-thinking`
- **Uso**: Activado automáticamente cuando Cursor necesita razonamiento estructurado

### 2. Memory MCP
- **Propósito**: Persistencia de información entre sesiones
- **Comando**: `npx -y @modelcontextprotocol/server-memory`
- **Uso**: Almacena contexto y decisiones importantes del proyecto

### 3. Playwright MCP
- **Propósito**: Automatización de pruebas y validación de ejercicios generados
- **Comando**: `npx -y @playwright/mcp@latest`
- **Uso**: Validación automática de archivos HTML/PDF generados por R-exams

### 4. Filesystem MCP
- **Propósito**: Acceso estructurado al sistema de archivos del proyecto
- **Comando**: `npx -y @modelcontextprotocol/server-filesystem`
- **Ruta**: `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams`

### 5. GitHub MCP ⚠️
- **Propósito**: Integración con GitHub para gestión de repositorio
- **Comando**: `npx -y @modelcontextprotocol/server-github`
- **Configuración requerida**: Agregar `GITHUB_PERSONAL_ACCESS_TOKEN` en `.cursor/mcp.json`

### 6. Git MCP
- **Propósito**: Operaciones Git locales
- **Comando**: `npx -y @modelcontextprotocol/server-git`
- **Repositorio**: `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams`

### 7. ArXiv LaTeX MCP ✅ (Python)
- **Propósito**: Obtener código LaTeX de papers de arXiv para contenido matemático
- **Comando**: `uv --directory .cursor/mcp-servers-python/arxiv-latex-mcp run server/main.py`
- **Uso**: Acceso a fuentes LaTeX de papers matemáticos para referencia y ejemplos
- **Repositorio**: `takashiishida/arxiv-latex-mcp`
- **Estado**: Instalado y configurado

### 8. Typst MCP ✅ (Python)
- **Propósito**: Conversión LaTeX/Typst, validación de sintaxis y generación de imágenes
- **Comando**: `uv --directory .cursor/mcp-servers-python/typst-mcp run server.py`
- **Uso**: Validación de código LaTeX/TikZ y conversión entre formatos
- **Repositorio**: `johannesbrandenburger/typst-mcp`
- **Dependencias**: Requiere Typst instalado (✅ instalado)
- **Estado**: Instalado y configurado

### 9. Python Executor MCP ✅ (Python + Deno)
- **Propósito**: Ejecutar código Python en sandbox seguro usando Pyodide + Deno
- **Comando**: `uvx mcp-run-python stdio`
- **Uso**: Ejecución segura de código Python aislado del sistema operativo
- **Repositorio**: `pydantic/mcp-run-python`
- **Dependencias**: Requiere Deno instalado (✅ instalado)
- **Estado**: Instalado y configurado



## Cómo Activar los MCPs en Cursor

1. **Reiniciar Cursor IDE** después de modificar `.cursor/mcp.json`
2. Los MCPs se cargan automáticamente al iniciar Cursor
3. Verificar en la barra de estado de Cursor que los MCPs estén activos

## Configuración del Token de GitHub (Opcional)

Para habilitar GitHub MCP:

1. Ir a https://github.com/settings/tokens
2. Generar un nuevo token con permisos: `repo`, `read:org`
3. Editar `.cursor/mcp.json` y agregar el token en `GITHUB_PERSONAL_ACCESS_TOKEN`

## MCPs Relevantes para el Proyecto

### Context 7 MCP
**Estado**: No encontrado en repositorios oficiales MCP
**Alternativa**: Memory MCP proporciona funcionalidad similar de gestión de contexto persistente

## Herramientas Instaladas

### uv (Gestor de paquetes Python moderno)
- **Versión**: 0.9.7
- **Ubicación**: `/home/bootcamp/.local/bin/uv`
- **Propósito**: Gestión rápida de dependencias Python para MCPs

### Deno (Runtime JavaScript/TypeScript)
- **Versión**: 2.5.6
- **Ubicación**: `/home/bootcamp/.deno/bin/deno`
- **Propósito**: Requerido para Python Executor MCP (Pyodide sandbox)

### Typst (Sistema de composición tipográfica)
- **Versión**: 0.13.1
- **Ubicación**: `/usr/bin/typst`
- **Propósito**: Requerido para Typst MCP (conversión LaTeX/Typst)

## MCPs Adicionales Disponibles (No Instalados)

### Image Analysis MCP 🐍
- **Repositorio**: `champierre/image-mcp-server`
- **Propósito**: Análisis de imágenes usando GPT-4 Vision
- **Instalación**: Requiere clonar repositorio de GitHub e instalar con Python
- **Uso**: Procesamiento y análisis de imágenes matemáticas en ejercicios ICFES
- **Nota**: Requiere API key de OpenAI

### Otros MCPs Disponibles
- **FastMCP Framework**: Para construir MCPs personalizados en Python
- **OpenStack MCP**: `openstack-kr/python-openstackmcp-server` - Gestión de infraestructura cloud

## Verificación de Funcionamiento

### MCPs npm (Node.js)

```bash
# Verificar Sequential Thinking
timeout 3 npx -y @modelcontextprotocol/server-sequential-thinking

# Verificar Memory
timeout 3 npx -y @modelcontextprotocol/server-memory

# Verificar Playwright
timeout 3 npx -y @playwright/mcp@latest
```

### MCPs Python

```bash
# Verificar ArXiv LaTeX MCP
cd .cursor/mcp-servers-python/arxiv-latex-mcp
~/.local/bin/uv run server/main.py --help

# Verificar Typst MCP
cd .cursor/mcp-servers-python/typst-mcp
~/.local/bin/uv run server.py --help

# Verificar Python Executor MCP
~/.local/bin/uvx mcp-run-python --help
```

## Ubicación de Archivos de Configuración

- **Configuración MCP**: `.cursor/mcp.json`
- **Información**: `.cursor/01-mcp-configuration-info.md`
- **Reglas Cursor**: `.cursor/rules/`

## Compatibilidad

- ✅ Sistema: Manjaro Plasma KDE (nativo)
- ✅ IDE: Cursor (VSCode fork)
- ✅ Node.js: v24.8.0
- ✅ npm: 11.6.0
- ✅ Proyecto: Compatible con flujo de trabajo actual

