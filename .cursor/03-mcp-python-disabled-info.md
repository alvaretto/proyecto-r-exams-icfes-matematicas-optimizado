# ⚠️ MCPs Python Temporalmente Desactivados

## Estado Actual

Los siguientes MCPs Python han sido **temporalmente desactivados** debido a problemas de configuración:

### 1. ArXiv LaTeX MCP ❌
- **Problema**: Requiere configuración adicional no documentada
- **Estado**: Desactivado temporalmente
- **Ubicación**: `.cursor/mcp-servers-python/arxiv-latex-mcp`

### 2. Typst MCP ❌
- **Problema**: Requiere compilación de documentación con Rust
- **Detalles**: 
  - Necesita clonar repositorio Typst (✅ clonado)
  - Requiere compilar con Cargo (Rust) para generar `typst-docs/main.json`
  - Comando requerido: `cargo run --package typst-docs -- --assets-dir ../typst-mcp/typst-docs --out-file ../typst-mcp/typst-docs/main.json`
  - Rust no está instalado en el sistema
- **Estado**: Desactivado temporalmente
- **Ubicación**: `.cursor/mcp-servers-python/typst-mcp`

## MCPs Activos (6 MCPs)

### MCPs npm (5 activos)
1. ✅ Sequential Thinking MCP
2. ✅ Memory MCP
3. ✅ Playwright MCP
4. ✅ Filesystem MCP
5. ✅ GitHub-Git MCP (29 operaciones Git + 11 workflows, requiere token para GitHub)

### MCPs Python (1 activo)
6. ✅ Python Executor MCP

## Cambios Recientes

### Git MCP Reemplazado
- **Problema**: `@modelcontextprotocol/server-git` no existe en npm
- **Solución**: Reemplazado por `@0xshariq/github-mcp-server`
- **Ventajas**:
  - 29 operaciones Git locales
  - 11 workflows combinados
  - Integración GitHub opcional (con token)

## Solución para Reactivar MCPs Python

### Opción 1: Instalar Rust y Compilar (Recomendado para Typst MCP)

```bash
# Instalar Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source $HOME/.cargo/env

# Compilar documentación de Typst
cd .cursor/mcp-servers-python/typst
cargo run --package typst-docs -- \
  --assets-dir ../typst-mcp/typst-docs \
  --out-file ../typst-mcp/typst-docs/main.json

# Reactivar Typst MCP en .cursor/mcp.json
```

### Opción 2: Buscar Documentación Pre-generada

Buscar en releases de Typst MCP si hay archivos `typst-docs/main.json` pre-generados.

### Opción 3: Usar MCPs Alternativos

Considerar MCPs alternativos que no requieran compilación:
- **Pandoc MCP**: Para conversión LaTeX
- **LaTeX Compiler MCP**: Para validación de LaTeX

## Configuración Actual

El archivo `.cursor/mcp.json` ha sido actualizado para incluir solo los MCPs funcionales.

**MCPs desactivados** están comentados en el archivo de configuración de respaldo.

## Próximos Pasos

1. **Reiniciar Cursor IDE** para cargar configuración actualizada
2. **Verificar** que los 7 MCPs activos funcionen correctamente
3. **Decidir** si instalar Rust para habilitar Typst MCP
4. **Explorar** MCPs alternativos para funcionalidad LaTeX/Typst

## Archivos de Configuración

- `.cursor/mcp.json` - Configuración activa (7 MCPs)
- `.cursor/01-mcp-configuration-info.md` - Documentación completa
- `.cursor/02-mcp-quick-start.md` - Guía de inicio rápido
- `.cursor/03-mcp-python-disabled-info.md` - Este archivo

## Notas Técnicas

### ArXiv LaTeX MCP
- Repositorio clonado correctamente
- Dependencias instaladas con `uv sync`
- Problema: Error al ejecutar `server/main.py`
- Requiere investigación adicional de logs de error

### Typst MCP
- Repositorio clonado correctamente
- Repositorio Typst clonado (132 MB)
- Dependencias instaladas con `uv sync`
- **Bloqueador**: Falta archivo `typst-docs/main.json`
- **Solución**: Compilar con Rust o buscar archivo pre-generado

### Python Executor MCP
- ✅ Funcionando correctamente
- Usa `uvx` para ejecución directa
- No requiere clonación de repositorio
- Dependencias: Deno 2.5.6 (instalado)

## Recomendación

**Para uso inmediato**: Usar los 7 MCPs activos que están funcionando.

**Para funcionalidad completa**: Instalar Rust y compilar documentación de Typst.

**Alternativa**: Explorar MCPs similares que no requieran compilación.

