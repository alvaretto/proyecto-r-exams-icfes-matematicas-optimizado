# ⚙️ INSTALACIÓN Y CONFIGURACIÓN - PROMPT ENHANCER

**Ubicación**: `Auxiliares/Prompt-Enhancer/`

## ✅ ESTADO ACTUAL: CONFIGURADO

Los alias ya están configurados en tu sistema:
- ✅ **~/.bashrc** (Bash)
- ✅ **~/.zshrc** (Zsh)

**Ver detalles completos**: [04-CONFIGURACION_ALIAS.md](04-CONFIGURACION_ALIAS.md)

---

## 🚀 Activación Rápida

### Opción 1: Recargar Configuración (Terminal Actual)

```bash
# Para Bash
source ~/.bashrc

# Para Zsh
source ~/.zshrc
```

### Opción 2: Nueva Terminal (Recomendado)

Simplemente abre una nueva terminal y los alias estarán disponibles automáticamente.

---

## 🔍 Verificar Configuración

### Script de Verificación Automática

```bash
./Auxiliares/Prompt-Enhancer/verificar-alias.sh
```

Este script verifica:
- ✅ Alias en ~/.bashrc
- ✅ Alias en ~/.zshrc
- 📋 Instrucciones de activación
- 📝 Ejemplos de uso

### Verificación Manual

```bash
# Verificar que el script es ejecutable
ls -l Auxiliares/Prompt-Enhancer/prompt-enhancer.sh

# Probar ayuda
./Auxiliares/Prompt-Enhancer/prompt-enhancer.sh --help

# Prueba funcional
./Auxiliares/Prompt-Enhancer/prompt-enhancer.sh "Prueba de funcionamiento"
```

**Salida esperada**: Prompt mejorado con contexto del proyecto incluyendo:
- Reglas de `.augment/rules/`
- Documentación de `.claude/`
- Guía de estilo de `.claudedoc/`

---

---

## ⚙️ Configuración de Alias (YA CONFIGURADO)

### ✅ Estado: Configurado Automáticamente

Los alias ya están configurados en:
- ✅ **~/.bashrc**
- ✅ **~/.zshrc**

### Alias Configurados

```bash
export ICFES_PROJECT_ROOT="/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
alias prompt-enhancer="$ICFES_PROJECT_ROOT/Auxiliares/Prompt-Enhancer/prompt-enhancer.sh"
alias pe="$ICFES_PROJECT_ROOT/Auxiliares/Prompt-Enhancer/prompt-enhancer.sh"
alias pec="$ICFES_PROJECT_ROOT/Auxiliares/Prompt-Enhancer/prompt-enhancer.sh -c"
alias pei="$ICFES_PROJECT_ROOT/Auxiliares/Prompt-Enhancer/prompt-enhancer.sh -i"
```

### Uso de Alias

```bash
# Desde cualquier ubicación
pe "Mi prompt"
pec "Mi prompt con copia"
pei  # Modo interactivo
```

### Reconfigurar Manualmente (Si es Necesario)

Si necesitas reconfigurar los alias manualmente:

#### Bash (~/.bashrc)

```bash
# Añadir al final de ~/.bashrc
export ICFES_PROJECT_ROOT="/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
alias prompt-enhancer="$ICFES_PROJECT_ROOT/Auxiliares/Prompt-Enhancer/prompt-enhancer.sh"
alias pe="$ICFES_PROJECT_ROOT/Auxiliares/Prompt-Enhancer/prompt-enhancer.sh"
alias pec="$ICFES_PROJECT_ROOT/Auxiliares/Prompt-Enhancer/prompt-enhancer.sh -c"
alias pei="$ICFES_PROJECT_ROOT/Auxiliares/Prompt-Enhancer/prompt-enhancer.sh -i"

# Aplicar cambios
source ~/.bashrc
```

#### Zsh (~/.zshrc)

```bash
# Añadir al final de ~/.zshrc
export ICFES_PROJECT_ROOT="/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
alias prompt-enhancer="$ICFES_PROJECT_ROOT/Auxiliares/Prompt-Enhancer/prompt-enhancer.sh"
alias pe="$ICFES_PROJECT_ROOT/Auxiliares/Prompt-Enhancer/prompt-enhancer.sh"
alias pec="$ICFES_PROJECT_ROOT/Auxiliares/Prompt-Enhancer/prompt-enhancer.sh -c"
alias pei="$ICFES_PROJECT_ROOT/Auxiliares/Prompt-Enhancer/prompt-enhancer.sh -i"

# Aplicar cambios
source ~/.zshrc
```

---

## 📋 Dependencias Opcionales

### xclip (Para Portapapeles en Linux)

#### Manjaro/Arch Linux

```bash
sudo pacman -S xclip
```

#### Ubuntu/Debian

```bash
sudo apt install xclip
```

#### Fedora

```bash
sudo dnf install xclip
```

**Verificar instalación**:
```bash
echo "Prueba" | xclip -selection clipboard
xclip -selection clipboard -o
```

### pbcopy (macOS)

Ya viene instalado por defecto en macOS.

**Verificar**:
```bash
echo "Prueba" | pbcopy
pbpaste
```

---

## 🎨 Configuración de VSCode

### Tarea Personalizada

Crea o edita `.vscode/tasks.json`:

```json
{
    "version": "2.0.0",
    "tasks": [
        {
            "label": "Mejorar Prompt",
            "type": "shell",
            "command": "${workspaceFolder}/prompt-enhancer.sh",
            "args": [
                "${input:userPrompt}",
                "-c"
            ],
            "presentation": {
                "reveal": "always",
                "panel": "new"
            },
            "problemMatcher": []
        },
        {
            "label": "Mejorar Prompt (Interactivo)",
            "type": "shell",
            "command": "${workspaceFolder}/prompt-enhancer.sh",
            "args": ["-i"],
            "presentation": {
                "reveal": "always",
                "panel": "new"
            },
            "problemMatcher": []
        }
    ],
    "inputs": [
        {
            "id": "userPrompt",
            "type": "promptString",
            "description": "Ingresa tu prompt:",
            "default": ""
        }
    ]
}
```

**Uso en VSCode**:
1. `Ctrl+Shift+P` → "Tasks: Run Task"
2. Seleccionar "Mejorar Prompt"
3. Ingresar prompt
4. Resultado copiado al portapapeles

### Snippet de VSCode

Crea o edita `.vscode/snippets.code-snippets`:

```json
{
    "Prompt Enhancer": {
        "prefix": "pe",
        "body": [
            "#!/bin/bash",
            "./prompt-enhancer.sh \"$1\" -c"
        ],
        "description": "Ejecutar Prompt Enhancer"
    }
}
```

---

## 🔗 Integración con Herramientas de IA

### Augment (VSCode Extension)

```bash
# Generar prompt mejorado
./prompt-enhancer.sh "Tu solicitud" -o /tmp/prompt.txt

# Copiar contenido
cat /tmp/prompt.txt

# Pegar en Augment Chat
```

### Claude Desktop

```bash
# Generar y copiar
./prompt-enhancer.sh "Tu solicitud" -c

# Pegar en Claude Desktop (Ctrl+V)
```

### API de OpenAI (Automatizado)

Crea un script `prompt-to-openai.sh`:

```bash
#!/bin/bash

PROMPT="$1"
ENHANCED=$(/ruta/completa/prompt-enhancer.sh "$PROMPT")

curl https://api.openai.com/v1/chat/completions \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -d "{
    \"model\": \"gpt-4\",
    \"messages\": [{\"role\": \"user\", \"content\": \"$ENHANCED\"}]
  }"
```

---

## 🛠️ Funciones Bash Avanzadas

### Función con Historial

Añade a `~/.bashrc`:

```bash
function pe_history() {
    local prompt="$1"
    local timestamp=$(date +%Y%m%d_%H%M%S)
    local history_file="$HOME/.prompt_enhancer_history"
    
    # Mejorar prompt
    $ICFES_PROJECT_ROOT/prompt-enhancer.sh "$prompt" -o "/tmp/prompt_$timestamp.txt"
    
    # Guardar en historial
    echo "[$timestamp] $prompt" >> "$history_file"
    
    # Mostrar resultado
    cat "/tmp/prompt_$timestamp.txt"
}
```

**Uso**:
```bash
pe_history "Mi prompt"
```

**Ver historial**:
```bash
cat ~/.prompt_enhancer_history
```

### Función con Selección de IA

```bash
function pe_ia() {
    local prompt="$1"
    local ia="${2:-augment}"  # Por defecto: augment
    
    # Mejorar prompt
    local enhanced=$($ICFES_PROJECT_ROOT/prompt-enhancer.sh "$prompt")
    
    case "$ia" in
        augment)
            echo "$enhanced" | xclip -selection clipboard
            echo "✓ Prompt copiado para Augment"
            ;;
        claude)
            echo "$enhanced" | xclip -selection clipboard
            echo "✓ Prompt copiado para Claude"
            ;;
        openai)
            # Integración con API de OpenAI
            echo "Enviando a OpenAI..."
            ;;
        *)
            echo "IA no reconocida: $ia"
            ;;
    esac
}
```

**Uso**:
```bash
pe_ia "Mi prompt" augment
pe_ia "Mi prompt" claude
```

---

## 📊 Configuración de Logging

### Habilitar Logs

Crea un archivo de configuración `~/.prompt_enhancer.conf`:

```bash
# Configuración de Prompt Enhancer
ENABLE_LOGGING=true
LOG_FILE="$HOME/.prompt_enhancer.log"
LOG_LEVEL="INFO"  # DEBUG, INFO, WARN, ERROR
```

### Modificar Script para Usar Configuración

Añade al inicio de `prompt-enhancer.sh`:

```bash
# Cargar configuración si existe
if [[ -f "$HOME/.prompt_enhancer.conf" ]]; then
    source "$HOME/.prompt_enhancer.conf"
fi
```

---

## 🔍 Verificación de Instalación

### Script de Verificación

Crea `verify-installation.sh`:

```bash
#!/bin/bash

echo "🔍 Verificando instalación de Prompt Enhancer..."

# Verificar script principal
if [[ -x "./prompt-enhancer.sh" ]]; then
    echo "✓ Script principal encontrado y ejecutable"
else
    echo "❌ Script principal no encontrado o no ejecutable"
    exit 1
fi

# Verificar raíz del proyecto
if [[ -d ".augment" ]] && [[ -d "A-Produccion" ]]; then
    echo "✓ Raíz del proyecto detectada correctamente"
else
    echo "❌ No se detectó la raíz del proyecto"
    exit 1
fi

# Verificar xclip (opcional)
if command -v xclip &> /dev/null; then
    echo "✓ xclip instalado (funcionalidad de portapapeles disponible)"
else
    echo "⚠ xclip no instalado (funcionalidad de portapapeles no disponible)"
fi

# Prueba funcional
echo ""
echo "🧪 Ejecutando prueba funcional..."
output=$(./prompt-enhancer.sh "Prueba de instalación" 2>&1)

if [[ $? -eq 0 ]]; then
    echo "✓ Prueba funcional exitosa"
else
    echo "❌ Prueba funcional falló"
    exit 1
fi

echo ""
echo "✅ Instalación verificada correctamente"
```

**Ejecutar**:
```bash
chmod +x verify-installation.sh
./verify-installation.sh
```

---

## 🚨 Solución de Problemas

### Problema: "Permission denied"

**Solución**:
```bash
chmod +x prompt-enhancer.sh
```

### Problema: "No se pudo encontrar la raíz del proyecto"

**Causa**: No estás dentro del repositorio

**Solución**:
```bash
cd /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams
./prompt-enhancer.sh "Tu prompt"
```

### Problema: xclip no funciona

**Verificar instalación**:
```bash
which xclip
```

**Reinstalar**:
```bash
sudo pacman -S xclip  # Manjaro/Arch
```

---

## 📚 Recursos Adicionales

- **Documentación completa**: `01-README_PROMPT_ENHANCER.md`
- **Ejemplos de uso**: `02-EJEMPLOS_USO_PROMPT_ENHANCER.md`
- **Reglas del proyecto**: `.augment/rules/reglas-generales.md`

---

**Última actualización**: 2025-12-20  
**Versión**: 1.0.0

