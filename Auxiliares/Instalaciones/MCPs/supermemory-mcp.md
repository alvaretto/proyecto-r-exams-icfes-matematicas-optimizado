# 🧠 Configuración de Supermemory MCP en Augment AI

Guía completa para instalar y configurar el Model Context Protocol (MCP) de Supermemory en Augment AI para VSCode.

## 📋 Prerrequisitos

- ✅ Node.js instalado (v22.16.0 o superior)
- ✅ VSCode con extensión Augment AI
- ✅ Conexión a internet

## 🚀 Paso 1: Instalación del Cliente MCP

### Instalar el cliente MCP oficial de Supermemory:

```bash
npx install-mcp --client claude --name supermemory https://mcp.supermemory.ai/YhlFnOH5_-FLrwDBHuvpU/sse
```

**Resultado esperado:**
```
✔ Install MCP server "supermemory" in claude?
Yes

 ╭─────────────────────────────────────────────────────────────╮
 │                                                             │
 │  Successfully installed MCP server "supermemory" in claude  │
 │                                                             │
 ╰─────────────────────────────────────────────────────────────╯
```

## 🔧 Paso 2: Crear Script Bridge para Augment AI

### Crear el archivo `supermemory-mcp-bridge.js`:

```javascript
#!/usr/bin/env node

/**
 * Supermemory MCP Bridge for Augment AI
 * Connects to Supermemory's HTTP/SSE MCP endpoint
 */

const { spawn } = require('child_process');
const https = require('https');
const { URL } = require('url');

const SUPERMEMORY_URL = 'https://mcp.supermemory.ai/YhlFnOH5_-FLrwDBHuvpU/sse';

class SupermemoryMCPBridge {
  constructor() {
    this.connected = false;
  }

  connect() {
    console.log('Connecting to Supermemory MCP...');
    
    const url = new URL(SUPERMEMORY_URL);
    
    const options = {
      hostname: url.hostname,
      port: url.port || 443,
      path: url.pathname + url.search,
      method: 'GET',
      headers: {
        'Accept': 'text/event-stream',
        'Cache-Control': 'no-cache',
        'Connection': 'keep-alive'
      }
    };

    const req = https.request(options, (res) => {
      console.log(`Connected to Supermemory MCP (Status: ${res.statusCode})`);
      this.connected = true;

      res.on('data', (chunk) => {
        // Forward SSE data to stdout for MCP protocol
        process.stdout.write(chunk);
      });

      res.on('end', () => {
        console.log('Connection to Supermemory MCP ended');
        this.connected = false;
      });
    });

    req.on('error', (error) => {
      console.error('Error connecting to Supermemory MCP:', error);
      process.exit(1);
    });

    // Handle stdin for MCP protocol
    process.stdin.on('data', (data) => {
      // Forward MCP requests to Supermemory
      req.write(data);
    });

    process.stdin.on('end', () => {
      req.end();
    });

    req.end();
  }

  start() {
    this.connect();
    
    // Keep the process alive
    process.on('SIGINT', () => {
      console.log('Shutting down Supermemory MCP bridge...');
      process.exit(0);
    });
  }
}

// Start the bridge
const bridge = new SupermemoryMCPBridge();
bridge.start();
```

### Hacer el script ejecutable:

```bash
chmod +x supermemory-mcp-bridge.js
```

## ⚙️ Paso 3: Configuración en Augment AI

### Método 1: Panel de Configuración (Recomendado)

1. **Abrir VSCode** con la extensión Augment AI
2. **Ir al panel de Augment** (barra lateral)
3. **Hacer clic en el ícono de engranaje** ⚙️ (esquina superior derecha)
4. **Buscar la sección "MCP"** en el panel de configuración
5. **Hacer clic en "+"** para agregar nuevo servidor MCP

#### Completar los campos:

- **Name:** `supermemory`
- **Command:** `node`
- **Arguments/Variables:**
  - Si hay campo para argumentos: agregar la ruta completa al script
  - Si solo hay variables de entorno: usar el Método 2

### Método 2: Editar settings.json (Alternativo)

1. **Presionar** `Ctrl+Shift+P` (o `Cmd+Shift+P` en Mac)
2. **Buscar** "Augment: Edit Settings"
3. **Seleccionar** "Edit in settings.json" bajo Advanced
4. **Agregar** la configuración:

```json
"augment.advanced": {
    "mcpServers": [
        {
            "name": "supermemory",
            "command": "node",
            "args": ["/ruta/completa/a/supermemory-mcp-bridge.js"]
        }
    ]
}
```

**Nota:** Reemplazar `/ruta/completa/a/` con la ruta real donde guardaste el script.

## 🔄 Paso 4: Reiniciar y Verificar

### Reiniciar VSCode:
1. **Cerrar VSCode** completamente
2. **Volver a abrir VSCode**
3. **Verificar** que Augment AI reconozca el servidor MCP

### Verificar la instalación:
1. **Abrir el panel de Augment AI**
2. **Iniciar una conversación** con el agente
3. **Preguntar:** "¿Puedes acceder a mi memoria de Supermemory?"
4. **El agente debería** poder conectarse y usar las funcionalidades

## 🔧 Solución de Problemas

### Error de conexión:
- ✅ Verificar URL de Supermemory
- ✅ Confirmar conexión a internet
- ✅ Revisar logs de consola

### El servidor MCP no aparece:
- ✅ Reiniciar VSCode completamente
- ✅ Verificar sintaxis en settings.json
- ✅ Revisar logs de Augment AI

### Permisos de archivo:
```bash
# Verificar permisos
ls -la supermemory-mcp-bridge.js

# Debería mostrar: -rwxr-xr-x
```

### Verificar instalación de Node.js:
```bash
node --version
# Debería mostrar: v22.16.0 o superior
```

## 🎉 Funcionalidades Disponibles

Una vez configurado correctamente, podrás:

- 💾 **Almacenar información** en memoria personal universal
- 🔄 **Recuperar contexto** de conversaciones anteriores  
- 📈 **Mantener continuidad** entre sesiones de AI
- 🔗 **Compartir memoria** entre diferentes herramientas de AI

## 📞 Soporte

Si encuentras problemas:

1. **Revisar logs** de VSCode y Augment AI
2. **Verificar configuración** de MCP en settings.json
3. **Comprobar conectividad** a Supermemory API
4. **Reiniciar** VSCode y servicios relacionados

---

**URL de Supermemory MCP:** `https://mcp.supermemory.ai/YhlFnOH5_-FLrwDBHuvpU/sse`

**Documentación oficial:** [https://docs.supermemory.ai](https://docs.supermemory.ai)
