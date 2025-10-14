# Chrome DevTools MCP - Instalación Completa

> **Estado**: ✅ Instalado y verificado  
> **Fecha**: 2025-10-02  
> **Versión**: chrome-devtools-mcp@latest (v0.6.0)

## 📋 Resumen de la Instalación

El servidor MCP de Chrome DevTools ha sido instalado exitosamente en **VS Code Insiders**. Este servidor permite que Copilot controle y automatice Chrome/Chromium para:

- 🚀 Análisis de rendimiento web
- 🔍 Debugging avanzado
- 📸 Capturas de pantalla
- 🌐 Automatización de navegación
- 📊 Análisis de red
- 🎯 Testing de aplicaciones web

## 📁 Archivos de Documentación

Este directorio contiene toda la documentación necesaria:

### 1. [README_INSTALACION.md](./README_INSTALACION.md)
**Guía completa de instalación y configuración**
- ✅ Verificación de requisitos
- ⚙️ Opciones de configuración
- 🔧 Solución de problemas
- 📚 Recursos adicionales

### 2. [EJEMPLOS_USO.md](./EJEMPLOS_USO.md)
**25+ ejemplos prácticos de uso**
- 🎯 Ejemplos básicos (análisis, screenshots, red)
- 🔧 Ejemplos avanzados (emulación, automatización)
- 🎨 Ejemplos para desarrollo web
- 🐛 Ejemplos para debugging
- 🔄 Flujos completos de trabajo

### 3. [AGREGAR_MAS_SERVIDORES.md](./AGREGAR_MAS_SERVIDORES.md)
**Guía para expandir tu configuración MCP**
- ➕ Cómo agregar más servidores
- 🌟 Lista de servidores populares
- 🛠️ Crear tus propios servidores
- 🔐 Manejo de variables de entorno

### 4. [USO_CON_REXAMS.md](./USO_CON_REXAMS.md) ⭐ NUEVO
**Guía específica para tu proyecto R/exams**
- 🎯 10+ casos de uso específicos para exámenes ICFES
- 🔧 Scripts de automatización
- 📋 Flujo de trabajo recomendado
- 🐛 Debugging común de exámenes HTML

### 5. [test_mcp.sh](./test_mcp.sh)
**Script de verificación**
- Verifica todos los requisitos
- Valida la configuración
- Muestra el estado del sistema

### 6. [verificar_examen.sh](./verificar_examen.sh) ⭐ NUEVO
**Generador de prompts para verificar exámenes**
- Genera prompts específicos para Copilot
- Verificación básica, completa, responsive
- Validación de MathJax y metadatos ICFES
- Modo interactivo

## 🚀 Inicio Rápido

### 1. Reiniciar VS Code Insiders
```bash
# Cierra todas las ventanas de VS Code Insiders
killall code-insiders

# Abre VS Code Insiders
code-insiders
```

### 2. Verificar la Instalación
```bash
cd Auxiliares/Instalaciones/MCP-Chrome
./test_mcp.sh
```

### 3. Primer Prompt de Prueba
Abre el chat de Copilot en VS Code Insiders y escribe:

```
Verifica el rendimiento de https://developers.chrome.com
```

## 📊 Estado del Sistema

### ✅ Requisitos Verificados

| Componente | Estado | Versión |
|------------|--------|---------|
| Node.js | ✅ Instalado | v22.17.1 |
| npm | ✅ Instalado | v10.9.2 |
| Chromium | ✅ Instalado | 140.0.7339.207 |
| VS Code Insiders | ✅ Instalado | Disponible |
| Paquete MCP | ✅ Disponible | v0.6.0 |

### 📁 Configuración

**Archivo de configuración:**
```
~/.config/Code - Insiders/User/globalStorage/github.copilot-chat/mcp.json
```

**Contenido:**
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

## 🎯 Casos de Uso Principales

### Para Desarrollo Web
- Verificar responsive design
- Analizar tiempos de carga
- Debuggear JavaScript
- Verificar peticiones de red
- Optimizar rendimiento

### Para Testing
- Automatizar pruebas de UI
- Verificar formularios
- Probar flujos de usuario
- Validar accesibilidad
- Comparar versiones

### Para Análisis
- Auditorías de rendimiento
- Análisis de SEO básico
- Verificar recursos cargados
- Analizar fuentes y estilos
- Detectar errores

## 🛠️ Herramientas Disponibles

El servidor MCP proporciona **26 herramientas** organizadas en 6 categorías:

| Categoría | Herramientas | Descripción |
|-----------|--------------|-------------|
| 🖱️ Input | 7 | click, drag, fill, fill_form, handle_dialog, hover, upload_file |
| 🧭 Navegación | 7 | close_page, list_pages, navigate_page, new_page, select_page, wait_for |
| 📱 Emulación | 3 | emulate_cpu, emulate_network, resize_page |
| ⚡ Rendimiento | 3 | performance_analyze_insight, performance_start_trace, performance_stop_trace |
| 🌐 Red | 2 | get_network_request, list_network_requests |
| 🐛 Debug | 4 | evaluate_script, list_console_messages, take_screenshot, take_snapshot |

## 📚 Ejemplos Rápidos

### Análisis de Rendimiento
```
Analiza el rendimiento de https://example.com
```

### Screenshot
```
Toma un screenshot de https://github.com
```

### Inspeccionar Red
```
Abre https://example.com y muéstrame las peticiones de red
```

### Ejecutar JavaScript
```
Navega a https://example.com y ejecuta: document.title
```

### Emular Móvil
```
Cambia el viewport a 375x667 y navega a https://example.com
```

## 🔧 Personalización

### Modo Headless (sin UI)
Edita `mcp.json` y agrega:
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

### Viewport Personalizado
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

## 🐛 Solución de Problemas

### El servidor no se inicia
1. Reinicia VS Code Insiders completamente
2. Verifica la consola de salida de Copilot
3. Ejecuta `./test_mcp.sh` para verificar requisitos

### Chrome no se abre
El servidor solo inicia Chrome cuando usas una herramienta que lo requiere. Conectarse al servidor no abre el navegador automáticamente.

### Ver logs detallados
Agrega `--logFile` a la configuración:
```json
{
  "mcpServers": {
    "chrome-devtools": {
      "command": "npx",
      "args": [
        "chrome-devtools-mcp@latest",
        "--logFile=/tmp/chrome-mcp.log"
      ],
      "env": {
        "DEBUG": "*"
      }
    }
  }
}
```

## 📖 Recursos

- 📘 [Documentación Completa](./README_INSTALACION.md)
- 💡 [Ejemplos de Uso](./EJEMPLOS_USO.md)
- ➕ [Agregar Más Servidores](./AGREGAR_MAS_SERVIDORES.md)
- 🔗 [Repositorio Oficial](https://github.com/ChromeDevTools/chrome-devtools-mcp)
- 📋 [Changelog](https://github.com/ChromeDevTools/chrome-devtools-mcp/blob/main/CHANGELOG.md)

## 🔄 Actualización

El servidor se actualiza automáticamente a la última versión cada vez que se inicia (gracias a `@latest`).

Para forzar una actualización manual:
```bash
npx -y chrome-devtools-mcp@latest --help
```

## 📞 Soporte

Si encuentras problemas:

1. Revisa la [guía de solución de problemas](./README_INSTALACION.md#-solución-de-problemas)
2. Ejecuta el script de verificación: `./test_mcp.sh`
3. Consulta los [issues del repositorio](https://github.com/ChromeDevTools/chrome-devtools-mcp/issues)
4. Revisa la [documentación oficial](https://github.com/ChromeDevTools/chrome-devtools-mcp)

## ✨ Próximos Pasos

1. ✅ Reinicia VS Code Insiders
2. ✅ Prueba el primer prompt de ejemplo
3. 📖 Explora los [ejemplos de uso](./EJEMPLOS_USO.md)
4. 🎨 Personaliza la configuración según tus necesidades
5. ➕ Considera [agregar más servidores MCP](./AGREGAR_MAS_SERVIDORES.md)

---

**¡Disfruta automatizando Chrome con Copilot! 🚀**

