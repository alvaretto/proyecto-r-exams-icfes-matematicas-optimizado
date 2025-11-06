# Configuración de Model Context Protocol (MCP) - Proyecto ICFES R-Exams

## MCPs Instalados y Configurados

### 1. Sequential Thinking MCP ✅
**Propósito**: Razonamiento paso a paso para problemas matemáticos complejos
**Comando**: `npx -y @modelcontextprotocol/server-sequential-thinking`
**Uso**: Activado automáticamente cuando Cursor necesita razonamiento estructurado

### 2. Memory MCP ✅
**Propósito**: Persistencia de información entre sesiones
**Comando**: `npx -y @modelcontextprotocol/server-memory`
**Uso**: Almacena contexto y decisiones importantes del proyecto

### 3. Playwright MCP ✅
**Propósito**: Automatización de pruebas y validación de ejercicios generados
**Comando**: `npx -y @playwright/mcp@latest`
**Uso**: Validación automática de archivos HTML/PDF generados por R-exams

### 4. Filesystem MCP ✅
**Propósito**: Acceso estructurado al sistema de archivos del proyecto
**Comando**: `npx -y @modelcontextprotocol/server-filesystem`
**Ruta**: `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams`

### 5. GitHub MCP ⚠️
**Propósito**: Integración con GitHub para gestión de repositorio
**Comando**: `npx -y @modelcontextprotocol/server-github`
**Configuración requerida**: Agregar `GITHUB_PERSONAL_ACCESS_TOKEN` en `.cursor/mcp.json`

### 6. Git MCP ✅
**Propósito**: Operaciones Git locales
**Comando**: `npx -y @modelcontextprotocol/server-git`
**Repositorio**: `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams`

## Cómo Activar los MCPs en Cursor

1. **Reiniciar Cursor IDE** después de modificar `.cursor/mcp.json`
2. Los MCPs se cargan automáticamente al iniciar Cursor
3. Verificar en la barra de estado de Cursor que los MCPs estén activos

## Configuración del Token de GitHub (Opcional)

Para habilitar GitHub MCP:

1. Ir a https://github.com/settings/tokens
2. Generar un nuevo token con permisos: `repo`, `read:org`
3. Editar `.cursor/mcp.json` y agregar el token en `GITHUB_PERSONAL_ACCESS_TOKEN`

## MCPs Relevantes para el Proyecto (No Instalados)

### Context 7 MCP
**Estado**: No encontrado en repositorios oficiales MCP
**Alternativa**: Memory MCP proporciona funcionalidad similar

### Otros MCPs Recomendados

- **Python MCP**: Para análisis de código Python (reticulate)
- **LaTeX MCP**: Para validación de código LaTeX/TikZ
- **Image Analysis MCP**: Para procesamiento de imágenes matemáticas

## Verificación de Funcionamiento

Ejecutar en terminal:
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

