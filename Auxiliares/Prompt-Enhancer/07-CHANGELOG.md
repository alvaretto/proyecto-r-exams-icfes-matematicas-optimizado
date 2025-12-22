# 📋 CHANGELOG - PROMPT ENHANCER

## [2.0.0] - 2025-12-21

### 🚀 Optimización para IA Genérica

#### Cambio Mayor de Enfoque
Esta versión representa un cambio fundamental en el enfoque del script, optimizándolo
para generar prompts compatibles con **cualquier IA** (ChatGPT, Claude, Gemini, Copilot, etc.).

#### Cambios Principales
- ✅ **Enfoque exclusivo en `.claude/`** - Eliminadas todas las referencias a `.augment/` y `.claudedoc/`
- ✅ **Generación automática de archivo .txt** - SIEMPRE genera `prompt_mejorado_YYYYMMDD_HHMMSS.txt`
- ✅ **Priorización del workflow** - Incluye contenido de `.claude/docs/WORKFLOW_PASO_A_PASO.md`
- ✅ **Hooks de automatización** - Nueva función para listar hooks de `.claude/hooks/`
- ✅ **Instrucciones para IA** - Sección específica guiando a la IA sobre cómo proceder

#### Nuevas Funciones
- ✅ `read_workflow_documentation()` - Lee y prioriza el workflow paso a paso
- ✅ `list_available_hooks()` - Lista hooks de automatización disponibles
- ✅ `read_project_context()` - Reemplaza y mejora `read_project_rules()`

#### Funciones Modificadas
- ✅ `enhance_prompt()` - Nueva estructura del prompt mejorado:
  1. Contexto del proyecto
  2. Workflow del proyecto (priorizado)
  3. Herramientas disponibles (skills, comandos, hooks)
  4. Patrones de error (si aplica)
  5. Ejemplos funcionales
  6. Solicitud del usuario (destacada)
  7. Instrucciones específicas para la IA (nueva)
- ✅ `main()` - Siempre genera archivo .txt además de otras opciones
- ✅ `show_help()` - Documentación actualizada

#### Estructura del Prompt Mejorado
```markdown
# PROMPT MEJORADO - PROYECTO ICFES R-EXAMS
## 📍 CONTEXTO DEL PROYECTO
## 🔄 WORKFLOW DEL PROYECTO  (nuevo - priorizado)
## 🔧 HERRAMIENTAS Y RECURSOS DISPONIBLES
   - Skills, Comandos, Hooks (nuevo)
## 🚨 ERRORES CONOCIDOS (si aplica)
## 📚 EJEMPLOS FUNCIONALES
## 💡 RECOMENDACIONES
---
## 🎯 SOLICITUD DEL USUARIO (destacada)
## 📋 INSTRUCCIONES PARA LA IA (nuevo)
```

#### Eliminaciones
- ❌ Referencias a `.augment/rules/`
- ❌ Referencias a `.claudedoc/`
- ❌ Función `read_style_guide()` (no relevante para IA genérica)
- ❌ Función `read_project_rules()` (reemplazada por `read_project_context()`)

#### Estadísticas
- **Líneas de código**: 618 → 749 (+21%)
- **Enfoque**: De proyecto-específico a IA-genérica
- **Fuentes de datos**: 3 → 1 (solo `.claude/`)
- **Archivo de salida**: Opcional → SIEMPRE generado

---

## [1.2.0] - 2025-12-21

### 🔧 Refactorización Mayor del Código

#### Bugs Críticos Corregidos
- ✅ **Líneas 91 y 97**: Paths incorrectos en lectura de archivos de reglas
- ✅ **Línea 435**: Manejo incorrecto de argumentos con espacios

#### Mejoras de Arquitectura
- ✅ **Constantes extraídas** para configuración centralizada
- ✅ **Funciones auxiliares nuevas**: `die()`, `warn()`, `success()`, `info()`, `read_file_limited()`
- ✅ **Función `read_project_rules()` refactorizada** en 5 funciones especializadas
- ✅ **Nuevas funciones especializadas** para mejor separación de responsabilidades

#### Estadísticas
- **Líneas de código**: 437 → 618 (+41%)
- **Funciones**: 6 → 17 (+183%)
- **Bugs corregidos**: 2 críticos

---

## [1.1.0] - 2025-12-20

### ✅ Configuración Automática de Alias

#### Añadido
- ✅ Configuración automática de alias en `~/.bashrc`
- ✅ Configuración automática de alias en `~/.zshrc`
- ✅ Script de verificación: `verificar-alias.sh`
- ✅ Documentación de configuración: `04-CONFIGURACION_ALIAS.md`
- ✅ Changelog del proyecto: `07-CHANGELOG.md`

#### Alias Configurados
```bash
export ICFES_PROJECT_ROOT="/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
alias prompt-enhancer="$ICFES_PROJECT_ROOT/Auxiliares/Prompt-Enhancer/prompt-enhancer.sh"
alias pe="$ICFES_PROJECT_ROOT/Auxiliares/Prompt-Enhancer/prompt-enhancer.sh"
alias pec="$ICFES_PROJECT_ROOT/Auxiliares/Prompt-Enhancer/prompt-enhancer.sh -c"
alias pei="$ICFES_PROJECT_ROOT/Auxiliares/Prompt-Enhancer/prompt-enhancer.sh -i"
```

#### Documentación Actualizada
- ✅ `README.md` - Actualizado con información de alias configurados
- ✅ `01-README_PROMPT_ENHANCER.md` - Actualizado con estado de configuración
- ✅ `02-EJEMPLOS_USO_PROMPT_ENHANCER.md` - Ejemplos actualizados con alias
- ✅ `03-INSTALACION_CONFIGURACION_PROMPT_ENHANCER.md` - Instrucciones actualizadas

#### Mejoras
- 📝 Todos los ejemplos ahora muestran primero el uso con alias (recomendado)
- 📝 Instrucciones claras de activación de alias
- 📝 Script de verificación automática de configuración
- 📝 Documentación completa del estado de configuración

---

## [1.0.0] - 2025-12-20

### 🚀 Lanzamiento Inicial

#### Características Principales
- ✅ Detección automática de contexto del proyecto
- ✅ Inclusión de reglas de `.augment/rules/`
- ✅ Inclusión de documentación de `.claude/`
- ✅ Inclusión de guía de estilo de `.claudedoc/`
- ✅ Listado de ejemplos funcionales disponibles
- ✅ Recomendaciones contextuales según ubicación

#### Modos de Uso
- ✅ Modo interactivo
- ✅ Prompt directo
- ✅ Lectura desde archivo
- ✅ Salida a archivo
- ✅ Copia al portapapeles

#### Opciones Disponibles
```bash
-h, --help              Mostrar ayuda
-i, --interactive       Modo interactivo (por defecto)
-f, --file FILE         Leer prompt desde archivo
-o, --output FILE       Guardar prompt mejorado en archivo
-c, --clipboard         Copiar prompt mejorado al portapapeles
```

#### Archivos Incluidos
- `prompt-enhancer.sh` - Script principal
- `README.md` - Documentación principal
- `01-README_PROMPT_ENHANCER.md` - Documentación completa
- `02-EJEMPLOS_USO_PROMPT_ENHANCER.md` - Ejemplos de uso
- `03-INSTALACION_CONFIGURACION_PROMPT_ENHANCER.md` - Instalación y configuración

---

## 📊 Estadísticas del Proyecto

### Archivos
- **Total de archivos**: 7
- **Scripts ejecutables**: 2 (`prompt-enhancer.sh`, `verificar-alias.sh`)
- **Documentación**: 5 archivos Markdown

### Líneas de Código (Actualizado v2.0.0)
- **prompt-enhancer.sh**: ~749 líneas (+21% vs v1.2.0)
- **verificar-alias.sh**: ~140 líneas
- **Total documentación**: ~1600+ líneas

### Funcionalidades
- ✅ Generación automática de archivo .txt (v2.0.0)
- ✅ Compatibilidad con IA genérica (v2.0.0)
- ✅ Inclusión de workflow documentado (v2.0.0)
- ✅ Listado de hooks de automatización (v2.0.0)
- ✅ 4 alias configurados
- ✅ Enfoque exclusivo en `.claude/` (v2.0.0)
- ✅ Detección automática de 6+ tipos de contexto
- ✅ Soporte Wayland clipboard

---

## 🎯 Próximas Mejoras Planificadas

### Versión 2.1.0 (Futuro)
- [ ] Detección automática de tipo de ejercicio (schoice, cloze)
- [ ] Sugerencias automáticas de metadatos ICFES
- [ ] Análisis de archivos .Rmd existentes en la ubicación actual
- [ ] Modo verbose para debugging

### Versión 2.2.0 (Futuro)
- [ ] Generación automática de estructura de ejercicios
- [ ] Validación de prompts antes de enviar
- [ ] Historial de prompts generados
- [ ] Estadísticas de uso

---

## 🔧 Mantenimiento

### Última Actualización
- **Fecha**: 2025-12-21
- **Versión**: 2.0.0
- **Estado**: ✅ Estable - Optimizado para IA genérica
- **Tipo de actualización**: Cambio mayor de enfoque

### Compatibilidad
- ✅ Bash 4.0+
- ✅ Zsh 5.0+
- ✅ Manjaro Plasma KDE
- ✅ Linux en general
- ✅ Soporte Wayland
- ✅ Cualquier IA (ChatGPT, Claude, Gemini, Copilot, etc.)

### Dependencias
- **Requeridas**: bash, grep, sed, cat, find, head, sort
- **Opcionales**: xclip, pbcopy, o wl-copy (para portapapeles)

---

## 📝 Notas de Versión

### v2.0.0
Esta versión representa un cambio fundamental de enfoque: el script ahora genera outputs optimizados para **cualquier IA** (ChatGPT, Claude, Gemini, Copilot, etc.) en lugar de estar limitado a herramientas específicas. Se eliminaron todas las referencias a `.augment/` y `.claudedoc/`, enfocándose exclusivamente en el contenido de `.claude/`. El archivo `.txt` ahora se genera SIEMPRE automáticamente, y se incluye una nueva sección de "Instrucciones para la IA" para guiar el procesamiento del prompt.

### v1.2.0
Refactorización mayor del código base con corrección de 2 bugs críticos. Mejoras en modularidad (17 funciones vs 6), manejo de errores y soporte Wayland.

### v1.1.0
Configuración automática de alias en bash y zsh. Sistema listo para usar sin configuración manual.

### v1.0.0
Primera versión funcional con todas las características principales implementadas.

---

**Mantenido por**: Sistema ICFES R-Exams
**Licencia**: Uso interno del proyecto

