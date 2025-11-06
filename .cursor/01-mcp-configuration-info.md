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

### MCPs Adicionales Recomendados (Requieren Instalación Manual)

#### **Python-Based MCPs** (Requieren Python y pip)

1. **ArXiv LaTeX MCP** 🐍
   - **Repositorio**: `takashiishida/arxiv-latex-mcp`
   - **Propósito**: Obtener código LaTeX de papers de arXiv para contenido matemático
   - **Instalación**: Requiere clonar repositorio de GitHub e instalar con Python
   - **Uso**: Acceso a fuentes LaTeX de papers matemáticos para referencia

2. **Typst MCP** 🐍
   - **Repositorio**: `johannesbrandenburger/typst-mcp`
   - **Propósito**: Conversión LaTeX/Typst, validación de sintaxis
   - **Instalación**: Requiere clonar repositorio de GitHub e instalar con Python
   - **Uso**: Validación de código LaTeX/TikZ y conversión entre formatos

3. **Image Analysis MCP** 🐍
   - **Repositorio**: `champierre/image-mcp-server`
   - **Propósito**: Análisis de imágenes usando GPT-4 Vision
   - **Instalación**: Requiere clonar repositorio de GitHub e instalar con Python
   - **Uso**: Procesamiento y análisis de imágenes matemáticas en ejercicios ICFES
   - **Nota**: Requiere API key de OpenAI

4. **Python Execution MCP** 🐍
   - **Repositorio**: `pydantic/pydantic-ai/mcp-run-python`
   - **Propósito**: Ejecutar código Python en sandbox seguro
   - **Instalación**: Requiere instalación con pip

#### **Otros MCPs Disponibles**

- **FastMCP Framework**: Para construir MCPs personalizados en Python
- **OpenStack MCP**: `openstack-kr/python-openstackmcp-server` - Gestión de infraestructura cloud

### Nota sobre MCPs Python

Los MCPs basados en Python no están disponibles como paquetes npm. Para instalarlos:

1. Clonar el repositorio de GitHub
2. Instalar dependencias con `pip install -r requirements.txt`
3. Configurar en `.cursor/mcp.json` usando `python` como comando en lugar de `npx`

**Ejemplo de configuración para MCP Python:**

```json
{
  "mcpServers": {
    "python-mcp-example": {
      "type": "stdio",
      "command": "python",
      "args": ["/ruta/al/repositorio/server.py"],
      "description": "Descripción del MCP"
    }
  }
}
```

## Verificación de Funcionamiento

Ejecutar en terminal para verificar MCPs instalados:

```bash
# Verificar Sequential Thinking
timeout 3 npx -y @modelcontextprotocol/server-sequential-thinking

# Verificar Memory
timeout 3 npx -y @modelcontextprotocol/server-memory

# Verificar Playwright
timeout 3 npx -y @playwright/mcp@latest
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

