# 🚀 PROMPT ENHANCER - Optimizador de Prompts para IA Genérica

**Versión**: 2.0.0
**Última actualización**: 2025-12-21
**Estado**: ✅ Estable - Optimizado para IA genérica

## 📋 Descripción

**Prompt Enhancer** es un script bash inteligente que mejora automáticamente los
prompts del usuario añadiendo contexto del proyecto ICFES R-Exams. Genera outputs
optimizados en formato markdown estándar compatibles con **cualquier IA** (ChatGPT,
Claude, Gemini, Copilot, etc.).

**Ubicación**: `Auxiliares/Prompt-Enhancer/`

## 🆕 Última Actualización - v2.0.0 (2025-12-21)

### Optimización para IA Genérica
Esta versión representa un cambio mayor en el enfoque del script:

- ✅ **Enfoque exclusivo en `.claude/`** - Eliminadas referencias a `.augment/` y `.claudedoc/`
- ✅ **Generación automática de archivo .txt** - SIEMPRE genera `prompt_mejorado_YYYYMMDD_HHMMSS.txt`
- ✅ **Formato markdown estándar** - Compatible con cualquier IA
- ✅ **Prioriza workflow documentado** - Incluye `.claude/docs/WORKFLOW_PASO_A_PASO.md`
- ✅ **Incluye hooks de automatización** - Lista hooks disponibles de `.claude/hooks/`
- ✅ **Instrucciones específicas para IA** - Sección de guía para la IA incluida

Ver detalles completos en: [07-CHANGELOG.md](07-CHANGELOG.md)

## 🎯 Características Principales

### ✅ Generación Automática de Archivo
- **SIEMPRE genera archivo .txt** - `prompt_mejorado_YYYYMMDD_HHMMSS.txt`
- **Ubicación**: Directorio actual de trabajo
- **Formato**: Markdown estándar optimizado para IA

### ✅ Detección Automática de Contexto
- **Ubicación actual**: Detecta automáticamente desde qué carpeta se está ejecutando
- **Tipo de contenido**: Identifica si estás en producción, desarrollo, auxiliares, etc.
- **Raíz del proyecto**: Encuentra la raíz del proyecto desde cualquier subcarpeta

### ✅ Enriquecimiento desde `.claude/`
- **Workflow paso a paso**: Prioriza `.claude/docs/WORKFLOW_PASO_A_PASO.md`
- **Skills disponibles**: Lista skills de `.claude/skills/`
- **Comandos disponibles**: Lista comandos de `.claude/commands/`
- **Hooks de automatización**: Lista hooks de `.claude/hooks/`
- **Patrones de error**: Incluye errores conocidos cuando es relevante
- **Ejemplos funcionales**: Lista ejemplos disponibles de `A-Produccion/`

### ✅ Múltiples Modos de Uso
- **Modo interactivo**: Ingresa tu prompt directamente
- **Desde archivo**: Lee prompts desde archivos de texto
- **Salida adicional**: Guarda copia adicional en archivo específico
- **Portapapeles**: Copia también al portapapeles (adicional, no reemplazo)

## 🔧 Instalación y Configuración

### ✅ Estado Actual: CONFIGURADO

Los alias ya están configurados en tu sistema:

- ✅ **~/.bashrc** (Bash)
- ✅ **~/.zshrc** (Zsh)

**Ver detalles de configuración**: [04-CONFIGURACION_ALIAS.md](04-CONFIGURACION_ALIAS.md)

### Activar Alias en Terminal Actual

```bash
# Para Bash
source ~/.bashrc

# Para Zsh
source ~/.zshrc
```

O simplemente **abre una nueva terminal**.

### Verificar Configuración

```bash
# Ejecutar script de verificación
./Auxiliares/Prompt-Enhancer/verificar-alias.sh
```

### Alias Disponibles

| Alias | Descripción |
|-------|-------------|
| `pe` | Versión corta (recomendado) |
| `pec` | Con copia al portapapeles |
| `pei` | Modo interactivo |
| `prompt-enhancer` | Comando completo |

## 📖 Uso

### Modo Interactivo (Recomendado)

```bash
# Con alias (recomendado)
pei

# O con ruta completa
./Auxiliares/Prompt-Enhancer/prompt-enhancer.sh
```

Luego ingresa tu prompt y presiona `Ctrl+D` cuando termines.

### Prompt Directo

```bash
# Con alias (recomendado)
pe "Genera un ejercicio de geometría nivel 2 sobre áreas"

# O con ruta completa
./Auxiliares/Prompt-Enhancer/prompt-enhancer.sh "Genera un ejercicio de geometría nivel 2 sobre áreas"
```

### Desde Archivo

```bash
# Con alias (recomendado)
pe -f mi_prompt.txt -o prompt_mejorado.txt

# O con ruta completa
./Auxiliares/Prompt-Enhancer/prompt-enhancer.sh -f mi_prompt.txt -o prompt_mejorado.txt
```

### Copiar al Portapapeles

```bash
./Auxiliares/Prompt-Enhancer/prompt-enhancer.sh "Corrige errores TikZ en este ejercicio" -c

# O con alias
pec "Corrige errores TikZ en este ejercicio"
```

### Ver Ayuda

```bash
./Auxiliares/Prompt-Enhancer/prompt-enhancer.sh --help

# O con alias
pe --help
```

## 🎨 Ejemplos de Uso

### Ejemplo 1: Desde Carpeta de Producción

```bash
cd A-Produccion/En-Produccion/Ejemplos-Funcionales-Rmd/
../../../Auxiliares/Prompt-Enhancer/prompt-enhancer.sh "Necesito crear un ejercicio similar a este"

# O con alias desde cualquier ubicación
pe "Necesito crear un ejercicio similar a este"
```

**Resultado**:

- Genera archivo `prompt_mejorado_YYYYMMDD_HHMMSS.txt`
- Incluye workflow de `.claude/docs/`
- Skills y comandos disponibles
- Ejemplos funcionales cercanos
- Instrucciones específicas para la IA

### Ejemplo 2: Desde Carpeta de Desarrollo

```bash
cd A-Produccion/En-Desarrollo/
pe "Ayúdame a implementar un gráfico TikZ"
```

**Resultado**:

- Genera archivo automáticamente
- Contexto de desarrollo activo
- Skills de corrección de gráficos
- Patrones de error TikZ conocidos

### Ejemplo 3: Con copia al portapapeles

```bash
pe "Crea un script de validación" -c
```

**Resultado**:

- Genera archivo automáticamente
- ADEMÁS copia al portapapeles
- Listo para pegar en cualquier IA

## 🔍 Contextos Detectados

El script identifica automáticamente estos contextos:

| Ubicación | Contexto | Recomendaciones Incluidas |
|-----------|----------|---------------------------|
| `A-Produccion/En-Produccion/` | Producción | Consultar ejemplos funcionales, mantener compatibilidad |
| `A-Produccion/En-PreDesarrollo/` | Pre-desarrollo | Validar diversidad 300+, preparar promoción |
| `A-Produccion/En-Desarrollo/` | Desarrollo | Seguir templates, documentar cambios |
| `Auxiliares/` | Auxiliares | Mantener compatibilidad, probar en entorno real |
| `A-Produccion/Templates/` | Templates | Estructura estándar, compatibilidad multi-formato |

## 📊 Estructura del Prompt Mejorado

El archivo `.txt` generado incluye las siguientes secciones:

```markdown
# PROMPT MEJORADO - PROYECTO ICFES R-EXAMS

## 📍 CONTEXTO DEL PROYECTO
- Proyecto: Sistema de generación de ejercicios matemáticos ICFES
- Ubicación actual: [ruta relativa]
- Tipo de contexto: [producción/desarrollo/etc]

## 🔄 WORKFLOW DEL PROYECTO
[Extracto de .claude/docs/WORKFLOW_PASO_A_PASO.md]

## 🔧 HERRAMIENTAS Y RECURSOS DISPONIBLES
### Documentación Principal
[Extracto de .claude/docs/README.md]

### Skills Disponibles
[Lista de skills en .claude/skills/]

### Comandos Disponibles
[Lista de comandos en .claude/commands/]

### Hooks de Automatización
[Lista de hooks en .claude/hooks/]

## 🚨 ERRORES CONOCIDOS Y SOLUCIONES
[Si el prompt menciona errores - patrones conocidos]

## 📚 EJEMPLOS FUNCIONALES DISPONIBLES
[Lista de archivos .Rmd de ejemplo]

## 💡 RECOMENDACIONES SEGÚN CONTEXTO
[Recomendaciones específicas según ubicación]

---

## 🎯 SOLICITUD DEL USUARIO
> [Tu prompt original]

## 📋 INSTRUCCIONES PARA LA IA
1. Priorizar uso de skills y comandos de .claude/
2. Consultar ejemplos funcionales antes de generar código
3. Validar compatibilidad con sistema exams2*
4. Seguir workflow documentado paso a paso
5. Documentar errores nuevos si se resuelven
```

## 🛠️ Requisitos

- **Bash**: 4.0 o superior
- **Dependencias requeridas**: grep, sed, cat, find, head, sort (incluidas en sistemas Unix)
- **Opcional**: `xclip`, `pbcopy`, o `wl-copy` para funcionalidad de portapapeles

### Instalar Herramientas de Portapapeles (Linux)

```bash
# Manjaro/Arch (X11)
sudo pacman -S xclip

# Manjaro/Arch (Wayland)
sudo pacman -S wl-clipboard

# Ubuntu/Debian (X11)
sudo apt install xclip

# Ubuntu/Debian (Wayland)
sudo apt install wl-clipboard
```

## 🚀 Casos de Uso Avanzados

### Integración con IA

Puedes usar el prompt mejorado directamente con herramientas de IA:

```bash
# Generar prompt y copiarlo
./prompt-enhancer.sh "Genera ejercicio de estadística" -c

# Luego pegar en tu herramienta de IA favorita (Augment, Claude, etc.)
```

### Automatización con Scripts

```bash
# Crear múltiples prompts mejorados
for prompt in "Ejercicio 1" "Ejercicio 2" "Ejercicio 3"; do
    ./prompt-enhancer.sh "$prompt" -o "prompt_${prompt// /_}.txt"
done
```

### Pipeline de Desarrollo

```bash
# 1. Crear prompt mejorado
./prompt-enhancer.sh -f requisitos.txt -o prompt_mejorado.txt

# 2. Usar con IA para generar código
# [Proceso manual o automatizado]

# 3. Validar resultado
cd A-Produccion/En-Desarrollo/
# [Proceso de validación]
```

## 📝 Notas Importantes

1. **Archivo siempre generado**: SIEMPRE se crea `prompt_mejorado_YYYYMMDD_HHMMSS.txt`
2. **Portabilidad**: El script funciona desde cualquier subcarpeta del proyecto
3. **Compatibilidad IA**: El archivo es compatible con cualquier IA (ChatGPT, Claude, Gemini, etc.)
4. **Sin dependencias pesadas**: Solo requiere bash estándar
5. **Enfoque en .claude/**: Solo incluye información de `.claude/` (workflow, skills, comandos, hooks)

## 🔧 Solución de Problemas

### Error: "No se pudo encontrar la raíz del proyecto"

**Causa**: No estás dentro del repositorio RepositorioMatematicasICFES_R_Exams

**Solución**: Navega a cualquier carpeta dentro del proyecto

### Advertencia: "xclip o pbcopy no están instalados"

**Causa**: Falta la herramienta de portapapeles (solo afecta opción `-c`)

**Solución**: Instala xclip (Linux) o wl-copy (Wayland). El archivo .txt se genera igualmente.

## 📚 Recursos Relacionados

- **Workflow del proyecto**: `.claude/docs/WORKFLOW_PASO_A_PASO.md`
- **Skills disponibles**: `.claude/skills/`
- **Comandos disponibles**: `.claude/commands/`
- **Hooks de automatización**: `.claude/hooks/`
- **Patrones de errores**: `.claude/docs/patrones-errores-conocidos.md`
- **Ejemplos funcionales**: `A-Produccion/`

## 🎯 Mejoras Implementadas en v2.0.0

- ✅ Generación automática de archivo .txt
- ✅ Formato markdown estándar para IA genérica
- ✅ Enfoque exclusivo en `.claude/`
- ✅ Priorización del workflow documentado
- ✅ Inclusión de hooks de automatización
- ✅ Sección de instrucciones para la IA

---

**Versión**: 2.0.0
**Fecha**: 2025-12-21
**Autor**: Sistema ICFES R-Exams
**Licencia**: Uso interno del proyecto

**Changelog**: Ver [07-CHANGELOG.md](07-CHANGELOG.md) para detalles completos de todas las versiones

