# Instalación de Chrome DevTools MCP en VS Code Insiders

## ✅ Instalación Completada

El servidor MCP de Chrome DevTools ha sido instalado exitosamente en VS Code Insiders.

### Ubicación del archivo de configuración
```
~/.config/Code - Insiders/User/globalStorage/github.copilot-chat/mcp.json
```

### Configuración aplicada
```json
{
  "mcpServers": {
    "chrome-devtools": {
      "command": "npx",
      "args": ["-y", "chrome-devtools-mcp@latest"]
    }
  }
}
```

## 🚀 Cómo usar

### 1. Reiniciar VS Code Insiders
Para que los cambios surtan efecto, cierra y vuelve a abrir VS Code Insiders.

### 2. Verificar la instalación
Una vez reiniciado VS Code Insiders, el servidor MCP debería estar disponible automáticamente.

### 3. Primer prompt de prueba
En el chat de Copilot, prueba con este comando:

```
Verifica el rendimiento de https://developers.chrome.com
```

El servidor MCP debería:
- Abrir automáticamente una instancia de Chrome
- Navegar a la URL especificada
- Grabar un trace de rendimiento
- Analizar y reportar métricas de rendimiento

## 📋 Características principales

### Automatización de entrada (7 herramientas)
- `click` - Hacer clic en elementos
- `drag` - Arrastrar elementos
- `fill` - Rellenar campos
- `fill_form` - Rellenar formularios completos
- `handle_dialog` - Manejar diálogos
- `hover` - Pasar el cursor sobre elementos
- `upload_file` - Subir archivos

### Automatización de navegación (7 herramientas)
- `close_page` - Cerrar páginas
- `list_pages` - Listar páginas abiertas
- `navigate_page` - Navegar a URLs
- `navigate_page_history` - Navegar en el historial
- `new_page` - Abrir nuevas páginas
- `select_page` - Seleccionar páginas
- `wait_for` - Esperar por elementos/eventos

### Emulación (3 herramientas)
- `emulate_cpu` - Emular CPU más lenta
- `emulate_network` - Emular condiciones de red
- `resize_page` - Cambiar tamaño de viewport

### Rendimiento (3 herramientas)
- `performance_analyze_insight` - Analizar insights de rendimiento
- `performance_start_trace` - Iniciar grabación de trace
- `performance_stop_trace` - Detener grabación de trace

### Red (2 herramientas)
- `get_network_request` - Obtener detalles de una petición
- `list_network_requests` - Listar todas las peticiones

### Depuración (4 herramientas)
- `evaluate_script` - Ejecutar JavaScript
- `list_console_messages` - Listar mensajes de consola
- `take_screenshot` - Tomar capturas de pantalla
- `take_snapshot` - Tomar snapshots del DOM

## ⚙️ Opciones de configuración

Puedes personalizar el comportamiento del servidor MCP agregando opciones en el array `args`:

### Modo headless (sin interfaz gráfica)
```json
{
  "mcpServers": {
    "chrome-devtools": {
      "command": "npx",
      "args": [
        "chrome-devtools-mcp@latest",
        "--headless=true"
      ]
    }
  }
}
```

### Usar Chrome Canary
```json
{
  "mcpServers": {
    "chrome-devtools": {
      "command": "npx",
      "args": [
        "chrome-devtools-mcp@latest",
        "--channel=canary"
      ]
    }
  }
}
```

### Modo aislado (perfil temporal)
```json
{
  "mcpServers": {
    "chrome-devtools": {
      "command": "npx",
      "args": [
        "chrome-devtools-mcp@latest",
        "--isolated=true"
      ]
    }
  }
}
```

### Viewport personalizado
```json
{
  "mcpServers": {
    "chrome-devtools": {
      "command": "npx",
      "args": [
        "chrome-devtools-mcp@latest",
        "--viewport=1920x1080"
      ]
    }
  }
}
```

### Combinación de opciones
```json
{
  "mcpServers": {
    "chrome-devtools": {
      "command": "npx",
      "args": [
        "chrome-devtools-mcp@latest",
        "--channel=canary",
        "--headless=true",
        "--isolated=true",
        "--viewport=1280x720"
      ]
    }
  }
}
```

## 📁 Directorio de datos de usuario

Por defecto, Chrome DevTools MCP usa:
- **Linux/MacOS**: `$HOME/.cache/chrome-devtools-mcp/chrome-profile-stable`
- **Windows**: `%HOMEPATH%/.cache/chrome-devtools-mcp/chrome-profile-stable`

Este directorio se comparte entre todas las instancias. Usa `--isolated=true` para usar un directorio temporal que se limpia automáticamente.

## 🔧 Requisitos del sistema

- ✅ Node.js v20.19 o superior (Instalado: v22.17.1)
- ✅ npm (Instalado: 10.9.2)
- ✅ Chrome versión estable o superior
- ✅ VS Code Insiders

## 🐛 Solución de problemas

### El servidor no se inicia
1. Verifica que VS Code Insiders esté completamente cerrado y reiniciado
2. Revisa la consola de salida de Copilot en VS Code
3. Verifica que Chrome esté instalado: `which google-chrome`

### Chrome no se abre automáticamente
El servidor MCP solo inicia Chrome cuando usas una herramienta que lo requiere. Simplemente conectarse al servidor no abre el navegador.

### Errores de permisos
Si Chrome no puede crear sandboxes, puedes:
1. Ejecutar con `--no-sandbox` (no recomendado para producción)
2. Usar `--connect-url` para conectarte a una instancia de Chrome que inicies manualmente

### Ver logs detallados
Agrega la opción `--logFile` para guardar logs:
```json
{
  "mcpServers": {
    "chrome-devtools": {
      "command": "npx",
      "args": [
        "chrome-devtools-mcp@latest",
        "--logFile=/tmp/chrome-devtools-mcp.log"
      ],
      "env": {
        "DEBUG": "*"
      }
    }
  }
}
```

## 📚 Recursos adicionales

- [Repositorio oficial](https://github.com/ChromeDevTools/chrome-devtools-mcp)
- [Documentación de herramientas](https://github.com/ChromeDevTools/chrome-devtools-mcp#tools)
- [Guía de solución de problemas](https://github.com/ChromeDevTools/chrome-devtools-mcp#known-limitations)
- [Changelog](https://github.com/ChromeDevTools/chrome-devtools-mcp/blob/main/CHANGELOG.md)

## 🔄 Actualización

El servidor se actualiza automáticamente a la última versión cada vez que se inicia, gracias al uso de `chrome-devtools-mcp@latest`.

Para forzar una actualización manual:
```bash
npx -y chrome-devtools-mcp@latest --help
```

## 📝 Ejemplos de uso

### Analizar rendimiento de una página
```
Analiza el rendimiento de https://example.com y dame un reporte detallado
```

### Tomar screenshot de una página
```
Toma un screenshot de https://github.com
```

### Ejecutar JavaScript en una página
```
Navega a https://example.com y ejecuta console.log('Hello from MCP!')
```

### Verificar peticiones de red
```
Abre https://example.com y muéstrame todas las peticiones de red que se hicieron
```

### Emular dispositivo móvil
```
Cambia el viewport a 375x667 y navega a https://example.com
```

---

**Fecha de instalación**: 2025-10-02  
**Versión**: chrome-devtools-mcp@latest (v0.6.0)

