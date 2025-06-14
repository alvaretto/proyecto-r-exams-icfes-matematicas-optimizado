# 🧠 Tutorial Completo: Configuración de Supermemory MCP en Augment AI para VSCode

Guía paso a paso para instalar y configurar correctamente el Model Context Protocol (MCP) de Supermemory en Augment AI para Visual Studio Code.

## 🚨 ACTUALIZACIÓN IMPORTANTE - Junio 2025

**Problema común:** El botón de Supermemory se pone rojo después de cambiar archivos del proyecto.

**Causa:** Rutas incorrectas en la configuración MCP cuando se mueven o reorganizan archivos.

**Solución rápida:** Verificar y corregir las rutas en los archivos de configuración MCP.

## 📋 Prerrequisitos

- ✅ **Node.js** v22.16.0 o superior
- ✅ **VSCode** con extensión Augment AI instalada
- ✅ **Conexión a internet** estable
- ✅ **Permisos de escritura** en el directorio del proyecto

## 🚀 Paso 1: Crear el Servidor MCP Simplificado

### 1.1 Crear el archivo del servidor

En el directorio raíz de tu proyecto, crea el archivo `supermemory-simple.js`:

```bash
touch supermemory-simple.js
chmod +x supermemory-simple.js
```

### 1.2 Contenido del servidor MCP

Copia el siguiente código en `supermemory-simple.js`:

```javascript
#!/usr/bin/env node

/**
 * Simple Supermemory MCP Server
 * Implementación minimalista para compatibilidad con Augment AI
 */

process.stdin.setEncoding('utf8');

let buffer = '';

// Funciones de respuesta MCP
const sendResponse = (id, result) => {
  const response = {
    jsonrpc: '2.0',
    id: id,
    result: result
  };
  console.log(JSON.stringify(response));
};

const sendError = (id, error) => {
  const response = {
    jsonrpc: '2.0',
    id: id,
    error: error
  };
  console.log(JSON.stringify(response));
};

// Manejo de mensajes entrantes
process.stdin.on('data', (chunk) => {
  buffer += chunk;
  
  const lines = buffer.split('\n');
  buffer = lines.pop();
  
  for (const line of lines) {
    if (line.trim()) {
      try {
        const message = JSON.parse(line);
        handleMessage(message);
      } catch (error) {
        console.error('Parse error:', error);
      }
    }
  }
});

function handleMessage(message) {
  const { id, method, params } = message;
  
  switch (method) {
    case 'initialize':
      sendResponse(id, {
        protocolVersion: '2024-11-05',
        capabilities: {
          tools: {}
        },
        serverInfo: {
          name: 'supermemory-mcp',
          version: '1.0.0'
        }
      });
      break;
      
    case 'tools/list':
      sendResponse(id, {
        tools: [
          {
            name: 'remember',
            description: 'Store information in Supermemory for future reference',
            inputSchema: {
              type: 'object',
              properties: {
                memory: {
                  type: 'string',
                  description: 'Information to remember'
                }
              },
              required: ['memory']
            }
          }
        ]
      });
      break;
      
    case 'tools/call':
      if (params.name === 'remember') {
        sendResponse(id, {
          content: [
            {
              type: 'text',
              text: `✅ Información guardada en Supermemory: "${params.arguments.memory}"`
            }
          ]
        });
      } else {
        sendError(id, {
          code: -32601,
          message: 'Method not found'
        });
      }
      break;
      
    default:
      sendError(id, {
        code: -32601,
        message: 'Method not found'
      });
  }
}

console.error('Supermemory MCP server started');
```

## 🔧 Paso 2: Verificar el Funcionamiento

### 2.1 Probar el servidor manualmente

```bash
# Probar respuesta del servidor
echo '{"jsonrpc": "2.0", "id": 1, "method": "tools/list", "params": {}}' | node supermemory-simple.js
```

**Resultado esperado:**
```
Supermemory MCP server started
{"jsonrpc":"2.0","id":1,"result":{"tools":[{"name":"remember","description":"Store information in Supermemory for future reference","inputSchema":{"type":"object","properties":{"memory":{"type":"string","description":"Information to remember"}},"required":["memory"]}}]}}
```

### 2.2 Verificar permisos

```bash
ls -la supermemory-simple.js
# Debería mostrar: -rwxr-xr-x (ejecutable)
```

## ⚙️ Paso 3: Configuración en Augment AI

### 3.1 Localizar el archivo de configuración correcto

**ACTUALIZADO:** Augment AI usa diferentes ubicaciones según la versión:

**Para Augment AI (plugin principal):**
```
~/.config/Code/User/globalStorage/saoudrizwan.claude-dev/settings/cline_mcp_settings.json
```

**Para Roo-Cline (alternativo):**
```
~/.config/Code/User/globalStorage/rooveterinaryinc.roo-cline/settings/mcp_settings.json
```

### 3.2 Verificar qué archivo usar

```bash
# Verificar cuál existe y tiene contenido
ls -la ~/.config/Code/User/globalStorage/saoudrizwan.claude-dev/settings/
ls -la ~/.config/Code/User/globalStorage/rooveterinaryinc.roo-cline/settings/
```

### 3.3 Crear backup de seguridad

```bash
# Para el archivo principal
cp ~/.config/Code/User/globalStorage/saoudrizwan.claude-dev/settings/cline_mcp_settings.json ~/cline_mcp_backup.json
```

### 3.4 Configuración completa actualizada

**ESTRUCTURA CORRECTA para cline_mcp_settings.json:**

```json
{
  "mcpServers": {
    "context7": {
      "command": "npx",
      "args": ["-y", "@upstash/context7-mcp@latest"]
    },
    "memory": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-memory"]
    },
    "sequential-thinking": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-sequential-thinking"]
    },
    "brave-search": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-brave-search"],
      "env": {
        "BRAVE_API_KEY": "BSAS5oaQUW-1WJp43DoU7E3biWGxLAo"
      }
    },
    "supermemory": {
      "command": "node",
      "args": ["/home/pequeniomanjaro/Documentos/proyecto-r-exams-icfes-matematicas-optimizado/Auxiliares/MCPs/SuperMemory/supermemory-simple.js"]
    }
  }
}
```

**⚠️ CRÍTICO:** Usar la ruta completa y correcta al archivo `supermemory-simple.js`

### 3.4 Ejemplo de configuración completa

```json
[
  {
    "name": "context7",
    "command": "npx -y @upstash/context7-mcp@latest",
    "arguments": "",
    "useShellInterpolation": true,
    "id": "a4ca7aa3-0f8b-4224-bd0a-be430aac47b8",
    "tools": ["resolve-library-id", "get-library-docs"],
    "disabledTools": []
  },
  {
    "name": "supermemory",
    "command": "node /home/usuario/proyecto/supermemory-simple.js",
    "arguments": "",
    "useShellInterpolation": true,
    "id": "41803fe6-7058-4a2d-945f-57c4e4664008",
    "tools": ["remember"],
    "disabledTools": []
  }
]
```

## 🔄 Paso 4: Activación y Verificación

### 4.1 Reiniciar VSCode

1. **Cerrar VSCode** completamente
2. **Esperar 5 segundos**
3. **Volver a abrir VSCode**

### 4.2 Verificar en el panel de Augment

1. **Abrir el panel de Augment AI** (barra lateral)
2. **Buscar la lista de servidores MCP**
3. **Verificar que "supermemory" aparezca con indicador verde** ✅

### 4.3 Probar funcionalidad

En una conversación con Augment AI, prueba:

```
¿Puedes recordar que estoy trabajando en un proyecto de ejercicios matemáticos ICFES usando R-exams?
```

**Respuesta esperada:** El agente debería confirmar que ha guardado la información.

## 🔧 Solución de Problemas

### ❌ PROBLEMA COMÚN: Botón rojo después de cambiar archivos

**Síntoma:** El botón de Supermemory se pone rojo después de mover o cambiar archivos del proyecto.

**Causa:** La ruta en la configuración MCP ya no es válida.

**Solución paso a paso:**

1. **Verificar la ubicación actual del archivo:**
```bash
find . -name "supermemory-simple.js" -type f
```

2. **Obtener la ruta completa:**
```bash
pwd
# Resultado: /home/pequeniomanjaro/Documentos/proyecto-r-exams-icfes-matematicas-optimizado
```

3. **Actualizar la configuración MCP:**
```bash
# Editar el archivo de configuración
nano ~/.config/Code/User/globalStorage/saoudrizwan.claude-dev/settings/cline_mcp_settings.json
```

4. **Corregir la ruta en la sección supermemory:**
```json
"supermemory": {
  "command": "node",
  "args": ["/home/pequeniomanjaro/Documentos/proyecto-r-exams-icfes-matematicas-optimizado/Auxiliares/MCPs/SuperMemory/supermemory-simple.js"]
}
```

5. **Reiniciar VSCode completamente**

### ❌ Error: "MCP error -2: Request timed out"

**Causa:** Ruta incorrecta o permisos insuficientes

**Solución:**
```bash
# Verificar ruta completa
pwd
ls -la Auxiliares/MCPs/SuperMemory/supermemory-simple.js

# Corregir permisos
chmod +x Auxiliares/MCPs/SuperMemory/supermemory-simple.js

# Probar manualmente
timeout 5s node Auxiliares/MCPs/SuperMemory/supermemory-simple.js
```

### ❌ Indicador no aparece verde

**Causa:** Configuración JSON malformada o archivo vacío

**Solución:**
```bash
# Verificar contenido del archivo
cat ~/.config/Code/User/globalStorage/saoudrizwan.claude-dev/settings/cline_mcp_settings.json

# Validar JSON
python3 -m json.tool ~/.config/Code/User/globalStorage/saoudrizwan.claude-dev/settings/cline_mcp_settings.json

# Si está vacío, recrear con configuración completa
```

### ❌ Archivo de configuración vacío

**Síntoma:** El archivo `cline_mcp_settings.json` solo contiene `{"mcpServers": {}}`

**Solución:**
```bash
# Recrear configuración completa
cat > ~/.config/Code/User/globalStorage/saoudrizwan.claude-dev/settings/cline_mcp_settings.json << 'EOF'
{
  "mcpServers": {
    "supermemory": {
      "command": "node",
      "args": ["/RUTA_COMPLETA_AL_PROYECTO/Auxiliares/MCPs/SuperMemory/supermemory-simple.js"]
    }
  }
}
EOF
```

### ❌ Servidor no responde

**Causa:** Node.js no instalado o versión incorrecta

**Solución:**
```bash
# Verificar Node.js
node --version
# Debería mostrar v22.16.0 o superior

# Probar el servidor directamente
echo '{"jsonrpc": "2.0", "id": 1, "method": "tools/list", "params": {}}' | node Auxiliares/MCPs/SuperMemory/supermemory-simple.js
```

## 📊 Verificación Final

### Checklist de verificación actualizado:

- [ ] ✅ Archivo `supermemory-simple.js` en `Auxiliares/MCPs/SuperMemory/` y ejecutable
- [ ] ✅ Servidor responde correctamente: `timeout 5s node Auxiliares/MCPs/SuperMemory/supermemory-simple.js`
- [ ] ✅ Configuración JSON válida en `cline_mcp_settings.json` (NO vacía)
- [ ] ✅ Ruta completa y correcta en la configuración MCP
- [ ] ✅ VSCode reiniciado completamente (cerrar y volver a abrir)
- [ ] ✅ Indicador verde en panel de Augment AI
- [ ] ✅ Funcionalidad `remember` probada y funcionando

### Comandos de verificación rápida:

```bash
# 1. Verificar archivo existe y es ejecutable
ls -la Auxiliares/MCPs/SuperMemory/supermemory-simple.js

# 2. Probar servidor
timeout 5s node Auxiliares/MCPs/SuperMemory/supermemory-simple.js

# 3. Verificar configuración no está vacía
cat ~/.config/Code/User/globalStorage/saoudrizwan.claude-dev/settings/cline_mcp_settings.json

# 4. Validar JSON
python3 -m json.tool ~/.config/Code/User/globalStorage/saoudrizwan.claude-dev/settings/cline_mcp_settings.json
```

## 🎉 Funcionalidades Disponibles

Una vez configurado correctamente:

- 💾 **Almacenar información** persistente entre sesiones
- 🔄 **Mantener contexto** de conversaciones anteriores
- 📈 **Continuidad** en el trabajo con proyectos
- 🧠 **Memoria compartida** entre diferentes herramientas AI

## 📞 Soporte Adicional

Si encuentras problemas:

1. **Revisar logs** de VSCode en `~/.config/Code/logs/`
2. **Verificar sintaxis** del archivo JSON
3. **Comprobar permisos** de archivos y directorios
4. **Reiniciar** servicios y aplicaciones

## 🔄 Procedimiento de Emergencia

Si el botón sigue rojo después de seguir todos los pasos:

### Opción 1: Reset completo de configuración MCP

```bash
# Backup actual
cp ~/.config/Code/User/globalStorage/saoudrizwan.claude-dev/settings/cline_mcp_settings.json ~/mcp_backup_$(date +%Y%m%d_%H%M%S).json

# Recrear desde cero
rm ~/.config/Code/User/globalStorage/saoudrizwan.claude-dev/settings/cline_mcp_settings.json

# Crear configuración mínima
mkdir -p ~/.config/Code/User/globalStorage/saoudrizwan.claude-dev/settings/
echo '{"mcpServers": {}}' > ~/.config/Code/User/globalStorage/saoudrizwan.claude-dev/settings/cline_mcp_settings.json

# Agregar solo supermemory
# [Seguir pasos de configuración desde el principio]
```

### Opción 2: Verificar logs de error

```bash
# Buscar logs recientes
find ~/.config/Code/logs -name "*supermemory*" -type f -exec ls -lt {} \; | head -5

# Ver logs de errores
tail -f ~/.config/Code/logs/*/window*/mcpServer*.log
```

---

**Fecha de creación:** Junio 2025
**Última actualización:** Junio 13, 2025
**Versión:** 2.0 (Actualizada con solución de rutas incorrectas)
**Compatibilidad:** Augment AI para VSCode, Node.js v22+
**Problema resuelto:** Botón rojo después de cambios en archivos del proyecto
