# 🚀 Inicio Rápido - MCPs Instalados en Cursor IDE

## ✅ Estado de Instalación

**Total MCPs Activos: 5**

### MCPs npm (Node.js) - 4 activos
1. ✅ Sequential Thinking MCP
2. ✅ Memory MCP
3. ✅ Playwright MCP
4. ✅ Filesystem MCP

### MCPs Python - 1 activo
5. ✅ Python Executor MCP

### MCPs Desactivados - 4 desactivados
- ❌ ArXiv LaTeX MCP (configuración incompleta)
- ❌ Typst MCP (requiere compilación Rust)
- ❌ GitHub MCP (no existe en npm)
- ❌ GitHub-Git MCP (no compatible stdio)

**Nota**: Ver `.cursor/03-mcp-python-disabled-info.md` para detalles sobre MCPs desactivados

## 🔄 Activar MCPs en Cursor IDE

### Paso 1: Reiniciar Cursor IDE
```bash
# Cerrar completamente Cursor IDE y volver a abrirlo
# Los MCPs se cargarán automáticamente al iniciar
```

### Paso 2: Verificar MCPs Activos
1. Abrir Cursor IDE
2. Buscar el ícono de herramientas/hammer en la interfaz
3. Verificar que aparezcan los 5 MCPs en la lista de "Available MCP tools"

## 🛠️ Herramientas Instaladas

### uv (Gestor Python)
- **Ubicación**: `~/.local/bin/uv`
- **Versión**: 0.9.7
- **Uso**: Gestión de dependencias Python para MCPs

### Deno (Runtime JS/TS)
- **Ubicación**: `~/.deno/bin/deno`
- **Versión**: 2.5.6
- **Uso**: Requerido para Python Executor MCP

### Typst (Composición Tipográfica)
- **Ubicación**: `/usr/bin/typst`
- **Versión**: 0.13.1
- **Uso**: Requerido para Typst MCP

## 📋 Uso de MCPs

### Python Executor MCP
```
Pregunta a Cursor: "Ejecuta este código Python en sandbox: import numpy as np; print(np.array([1,2,3]))"
```

### Sequential Thinking MCP
```
Pregunta a Cursor: "Resuelve paso a paso este problema matemático: ..."
```

### Memory MCP
```
Pregunta a Cursor: "Recuerda que estamos trabajando en ejercicios ICFES de estadística"
```

## ⚙️ Notas Importantes

### Operaciones Git
- Los MCPs Git no están disponibles actualmente
- Usar comandos Git directamente desde terminal
- Augment IA tiene acceso a Git integrado en VSCode

## 🔍 Verificación Manual

### Verificar MCPs npm
```bash
timeout 3 npx -y @modelcontextprotocol/server-sequential-thinking
timeout 3 npx -y @modelcontextprotocol/server-memory
timeout 3 npx -y @playwright/mcp@latest
```

### Verificar MCPs Python
```bash
# Python Executor
~/.local/bin/uvx mcp-run-python --help
```

## 📚 Documentación Completa

Ver archivo: `.cursor/01-mcp-configuration-info.md`

## 🎯 Próximos Pasos

1. ✅ Reiniciar Cursor IDE
2. ✅ Verificar que los 5 MCPs activos aparezcan sin errores
3. ✅ Probar cada MCP con preguntas de ejemplo
4. 📖 Consultar `.cursor/03-mcp-python-disabled-info.md` para información sobre MCPs desactivados

## 🆘 Solución de Problemas

### MCP no aparece en Cursor
- Verificar que `.cursor/mcp.json` esté correctamente formateado
- Reiniciar Cursor IDE completamente
- Revisar logs de Cursor en la consola de desarrollador

### Error al ejecutar MCP Python
- Verificar que `uv` esté instalado: `~/.local/bin/uv --version`
- Verificar que Deno esté instalado: `~/.deno/bin/deno --version`
- Verificar que Typst esté instalado: `typst --version`

### MCP Python Executor falla
- Verificar que Deno esté en el PATH
- Ejecutar manualmente: `~/.local/bin/uvx mcp-run-python stdio`
- Revisar logs de error en la terminal

## 📞 Soporte

Para más información, consultar:
- Documentación MCP: https://modelcontextprotocol.io/
- Repositorio Cursor: https://github.com/getcursor/cursor
- Awesome MCP Servers: https://github.com/punkpeye/awesome-mcp-servers

