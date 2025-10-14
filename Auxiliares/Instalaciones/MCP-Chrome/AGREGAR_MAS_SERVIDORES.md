# Cómo Agregar Más Servidores MCP

Esta guía te muestra cómo agregar servidores MCP adicionales a tu configuración de VS Code Insiders.

## 📍 Ubicación del Archivo de Configuración

```
~/.config/Code - Insiders/User/globalStorage/github.copilot-chat/mcp.json
```

## 📝 Estructura Básica

El archivo `mcp.json` tiene esta estructura:

```json
{
  "mcpServers": {
    "nombre-servidor-1": {
      "command": "comando",
      "args": ["arg1", "arg2"]
    },
    "nombre-servidor-2": {
      "command": "comando",
      "args": ["arg1", "arg2"]
    }
  }
}
```

## ➕ Agregar un Nuevo Servidor

### Método 1: Editar el archivo manualmente

1. Abre el archivo de configuración:
```bash
code-insiders ~/.config/Code\ -\ Insiders/User/globalStorage/github.copilot-chat/mcp.json
```

2. Agrega el nuevo servidor dentro de `mcpServers`:
```json
{
  "mcpServers": {
    "chrome-devtools": {
      "command": "npx",
      "args": ["-y", "chrome-devtools-mcp@latest"]
    },
    "nuevo-servidor": {
      "command": "npx",
      "args": ["-y", "nombre-paquete@latest"]
    }
  }
}
```

3. Guarda el archivo y reinicia VS Code Insiders

### Método 2: Usar el CLI de VS Code

```bash
code-insiders --add-mcp '{"name":"nombre-servidor","command":"comando","args":["arg1","arg2"]}'
```

## 🌟 Servidores MCP Populares

### 1. Filesystem MCP
Permite al agente leer y escribir archivos.

```json
{
  "mcpServers": {
    "filesystem": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-filesystem", "/ruta/permitida"]
    }
  }
}
```

### 2. GitHub MCP
Interactúa con repositorios de GitHub.

```json
{
  "mcpServers": {
    "github": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-github"],
      "env": {
        "GITHUB_PERSONAL_ACCESS_TOKEN": "tu-token-aqui"
      }
    }
  }
}
```

### 3. PostgreSQL MCP
Consulta bases de datos PostgreSQL.

```json
{
  "mcpServers": {
    "postgres": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-postgres", "postgresql://user:pass@localhost/db"]
    }
  }
}
```

### 4. Brave Search MCP
Búsquedas web con Brave Search.

```json
{
  "mcpServers": {
    "brave-search": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-brave-search"],
      "env": {
        "BRAVE_API_KEY": "tu-api-key-aqui"
      }
    }
  }
}
```

### 5. Slack MCP
Interactúa con Slack.

```json
{
  "mcpServers": {
    "slack": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-slack"],
      "env": {
        "SLACK_BOT_TOKEN": "xoxb-tu-token",
        "SLACK_TEAM_ID": "T01234567"
      }
    }
  }
}
```

### 6. Google Drive MCP
Accede a archivos en Google Drive.

```json
{
  "mcpServers": {
    "gdrive": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-gdrive"]
    }
  }
}
```

### 7. Memory MCP
Proporciona memoria persistente al agente.

```json
{
  "mcpServers": {
    "memory": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-memory"]
    }
  }
}
```

### 8. Puppeteer MCP
Automatización de navegador (alternativa a chrome-devtools).

```json
{
  "mcpServers": {
    "puppeteer": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-puppeteer"]
    }
  }
}
```

## 📦 Ejemplo de Configuración Completa

```json
{
  "mcpServers": {
    "chrome-devtools": {
      "command": "npx",
      "args": ["-y", "chrome-devtools-mcp@latest"]
    },
    "filesystem": {
      "command": "npx",
      "args": [
        "-y",
        "@modelcontextprotocol/server-filesystem",
        "/home/proyectos"
      ]
    },
    "github": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-github"],
      "env": {
        "GITHUB_PERSONAL_ACCESS_TOKEN": "${GITHUB_TOKEN}"
      }
    },
    "memory": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-memory"]
    }
  }
}
```

## 🔐 Manejo de Variables de Entorno

### Opción 1: Directamente en el JSON
```json
{
  "mcpServers": {
    "github": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-github"],
      "env": {
        "GITHUB_PERSONAL_ACCESS_TOKEN": "ghp_tu_token_aqui"
      }
    }
  }
}
```

### Opción 2: Usar variables del sistema
```json
{
  "mcpServers": {
    "github": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-github"],
      "env": {
        "GITHUB_PERSONAL_ACCESS_TOKEN": "${GITHUB_TOKEN}"
      }
    }
  }
}
```

Luego define la variable en tu shell:
```bash
export GITHUB_TOKEN="ghp_tu_token_aqui"
```

## 🛠️ Crear tu Propio Servidor MCP

### Estructura básica de un servidor MCP

```javascript
#!/usr/bin/env node
import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';

const server = new Server({
  name: 'mi-servidor',
  version: '1.0.0',
});

// Registrar herramientas
server.setRequestHandler('tools/list', async () => ({
  tools: [
    {
      name: 'mi_herramienta',
      description: 'Descripción de la herramienta',
      inputSchema: {
        type: 'object',
        properties: {
          parametro: {
            type: 'string',
            description: 'Descripción del parámetro'
          }
        },
        required: ['parametro']
      }
    }
  ]
}));

// Manejar llamadas a herramientas
server.setRequestHandler('tools/call', async (request) => {
  if (request.params.name === 'mi_herramienta') {
    // Implementar la lógica aquí
    return {
      content: [
        {
          type: 'text',
          text: 'Resultado de la herramienta'
        }
      ]
    };
  }
});

// Iniciar servidor
const transport = new StdioServerTransport();
await server.connect(transport);
```

### Agregar tu servidor personalizado

```json
{
  "mcpServers": {
    "mi-servidor": {
      "command": "node",
      "args": ["/ruta/a/mi-servidor.js"]
    }
  }
}
```

## 🔍 Verificar Servidores Instalados

Puedes verificar qué servidores están configurados:

```bash
cat ~/.config/Code\ -\ Insiders/User/globalStorage/github.copilot-chat/mcp.json | jq '.mcpServers | keys'
```

## 🐛 Debugging de Servidores MCP

### Ver logs de un servidor

Agrega variables de entorno de debug:

```json
{
  "mcpServers": {
    "chrome-devtools": {
      "command": "npx",
      "args": ["-y", "chrome-devtools-mcp@latest"],
      "env": {
        "DEBUG": "*",
        "NODE_ENV": "development"
      }
    }
  }
}
```

### Verificar que un servidor funciona

Prueba ejecutar el comando manualmente:

```bash
npx -y chrome-devtools-mcp@latest --help
```

## 📚 Recursos

- [MCP Registry](https://github.com/modelcontextprotocol/servers) - Lista oficial de servidores
- [MCP SDK](https://github.com/modelcontextprotocol/typescript-sdk) - SDK para crear servidores
- [MCP Specification](https://spec.modelcontextprotocol.io/) - Especificación del protocolo

## ⚠️ Notas Importantes

1. **Reiniciar VS Code**: Siempre reinicia VS Code Insiders después de modificar `mcp.json`
2. **Permisos**: Algunos servidores requieren permisos especiales o tokens de API
3. **Rendimiento**: Demasiados servidores pueden afectar el rendimiento
4. **Seguridad**: Solo instala servidores de fuentes confiables
5. **Versiones**: Usa `@latest` para siempre tener la última versión

## 🔄 Script de Actualización

Crea un script para actualizar todos tus servidores MCP:

```bash
#!/bin/bash
# update_mcp_servers.sh

echo "Actualizando servidores MCP..."

# Limpiar caché de npx
npx clear-npx-cache

# Forzar actualización de paquetes
npm cache clean --force

echo "✅ Servidores MCP actualizados"
echo "🔄 Reinicia VS Code Insiders para aplicar cambios"
```

---

**Última actualización**: 2025-10-02

