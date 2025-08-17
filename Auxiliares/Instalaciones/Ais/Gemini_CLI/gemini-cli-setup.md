# Configuración de Gemini CLI en Manjaro Plasma con VSCode Insiders

## ✅ Instalación Completada

### Componentes Instalados:
- **Go 1.24.6** - Instalado desde repositorios oficiales de Manjaro
- **Gemini CLI v0.1.21** - Instalado globalmente via npm
- **API Key configurada** - Variable de entorno configurada permanentemente

### Ubicaciones:
- **Ejecutable Gemini CLI**: `/home/proyectos/.npm-global/bin/gemini`
- **API Key**: Configurada en `~/.bashrc` y `~/.zshrc`

## 🚀 Uso Básico

### Comandos Principales:

```bash
# Modo interactivo
gemini

# Prompt directo (no interactivo)
gemini -p "Tu pregunta aquí"

# Prompt interactivo (ejecuta y continúa en modo interactivo)
gemini -i "Tu pregunta inicial"

# Con archivos específicos en contexto
gemini --all-files

# Modo debug
gemini -d

# Ver ayuda completa
gemini --help
```

### Ejemplos de Uso:

```bash
# Análisis de código
gemini -p "Analiza este archivo Python y sugiere mejoras" archivo.py

# Generación de código
gemini -p "Crea una función en Python que calcule números primos"

# Revisión de proyecto
gemini --all-files -p "Revisa la estructura de este proyecto y sugiere mejoras"
```

## 🔧 Configuración para VSCode Insiders

### 1. Terminal Integrado
El terminal integrado de VSCode Insiders ya tiene acceso a `gemini` porque:
- La API key está configurada en `~/.bashrc` y `~/.zshrc`
- El comando `gemini` está en el PATH global

### 2. Tareas Personalizadas (tasks.json)
Crea `.vscode/tasks.json` en tu proyecto:

```json
{
    "version": "2.0.0",
    "tasks": [
        {
            "label": "Gemini: Analizar Archivo Actual",
            "type": "shell",
            "command": "gemini",
            "args": [
                "-p",
                "Analiza este archivo y sugiere mejoras: ${file}"
            ],
            "group": "build",
            "presentation": {
                "echo": true,
                "reveal": "always",
                "focus": false,
                "panel": "new"
            }
        },
        {
            "label": "Gemini: Revisar Proyecto",
            "type": "shell",
            "command": "gemini",
            "args": [
                "--all-files",
                "-p",
                "Revisa la estructura y código de este proyecto"
            ],
            "group": "build",
            "presentation": {
                "echo": true,
                "reveal": "always",
                "focus": false,
                "panel": "new"
            }
        }
    ]
}
```

### 3. Atajos de Teclado (keybindings.json)
Agrega a tu `keybindings.json`:

```json
[
    {
        "key": "ctrl+shift+g",
        "command": "workbench.action.tasks.runTask",
        "args": "Gemini: Analizar Archivo Actual"
    },
    {
        "key": "ctrl+shift+alt+g",
        "command": "workbench.action.tasks.runTask",
        "args": "Gemini: Revisar Proyecto"
    }
]
```

## 🛠️ Funcionalidades Avanzadas

### Herramientas Integradas:
- **Google Search grounding** - Búsquedas web automáticas
- **File operations** - Operaciones de archivos
- **Shell commands** - Ejecución de comandos
- **Web fetching** - Obtención de contenido web

### Modelos Disponibles:
- **Gemini 2.5 Pro** (por defecto) - 1M tokens de contexto
- **Gemini 2.5 Flash** - Más rápido para tareas simples

### Límites del Tier Gratuito:
- **60 requests/min**
- **1,000 requests/day**

## 🔒 Seguridad

### API Key:
- Configurada como variable de entorno
- No expuesta en archivos de configuración del proyecto
- Accesible solo para tu usuario

### Recomendaciones:
- No compartas tu API key
- Revisa regularmente el uso en Google AI Studio
- Considera rotar la key periódicamente

## 📝 Comandos de Verificación

```bash
# Verificar instalación
gemini --version

# Verificar API key
echo $GEMINI_API_KEY

# Verificar ubicación del ejecutable
which gemini

# Prueba básica
gemini -p "Hola, ¿funcionas correctamente?"
```

## 🆘 Solución de Problemas

### Si gemini no se encuentra:
```bash
# Verificar PATH
echo $PATH | grep npm-global

# Reinstalar si es necesario
npm install -g @google/gemini-cli
```

### Si la API key no funciona:
```bash
# Verificar variable de entorno
echo $GEMINI_API_KEY

# Recargar configuración del shell
source ~/.bashrc
```

### Para actualizar:
```bash
npm update -g @google/gemini-cli
```

## 🎯 Próximos Pasos

1. **Experimenta** con diferentes tipos de prompts
2. **Configura tareas personalizadas** en VSCode para tus flujos de trabajo
3. **Explora MCP servers** para integraciones avanzadas
4. **Considera usar** el modo sandbox para ejecución segura de código

---

**¡Gemini CLI está listo para usar en tu Manjaro Plasma con VSCode Insiders!** 🎉
