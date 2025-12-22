---
output:
  html_document: default
  word_document: default
---
# 🚀 PROMPT ENHANCER - Optimizador de Prompts para IA Genérica

**Versión**: 2.0.0
**Ubicación**: `Auxiliares/Prompt-Enhancer/`
**Última actualización**: 2025-12-21
**Tipo de actualización**: Optimización para IA genérica

## 📋 Descripción Rápida

Sistema inteligente que mejora automáticamente los prompts del usuario añadiendo
contexto del proyecto ICFES R-Exams. Genera outputs en formato markdown estándar
compatibles con **cualquier IA** (ChatGPT, Claude, Gemini, Copilot, etc.).

- ✅ **SIEMPRE genera archivo** `prompt_mejorado_YYYYMMDD_HHMMSS.txt`
- ✅ Workflow documentado de `.claude/docs/`
- ✅ Skills y comandos de `.claude/`
- ✅ Hooks de automatización disponibles
- ✅ Ejemplos funcionales y recomendaciones contextuales

## 🚀 Inicio Rápido

### ✅ Alias Ya Configurados

Los alias ya están configurados en tu sistema:

- ✅ **~/.bashrc** (Bash)
- ✅ **~/.zshrc** (Zsh)

**Para activarlos en tu terminal actual:**
```bash
source ~/.bashrc  # o source ~/.zshrc si usas zsh
```

**O simplemente abre una nueva terminal.**

### Uso con Alias (Recomendado)

```bash
# Uso básico
pe "Tu prompt aquí"

# Con copia al portapapeles
pec "Tu prompt"

# Modo interactivo
pei
```

### Uso sin Alias (Ruta Completa)

```bash
# Desde la raíz del proyecto
./Auxiliares/Prompt-Enhancer/prompt-enhancer.sh "Tu prompt aquí"

# Modo interactivo
./Auxiliares/Prompt-Enhancer/prompt-enhancer.sh

# Con copia al portapapeles
./Auxiliares/Prompt-Enhancer/prompt-enhancer.sh "Tu prompt" -c
```

## 📚 Documentación Completa (Orden de Lectura)

1. **[01-README_PROMPT_ENHANCER.md](01-README_PROMPT_ENHANCER.md)** - Documentación completa
2. **[02-EJEMPLOS_USO_PROMPT_ENHANCER.md](02-EJEMPLOS_USO_PROMPT_ENHANCER.md)** - Ejemplos prácticos
3. **[03-INSTALACION_CONFIGURACION_PROMPT_ENHANCER.md](03-INSTALACION_CONFIGURACION_PROMPT_ENHANCER.md)** - Instalación y configuración avanzada
4. **[04-CONFIGURACION_ALIAS.md](04-CONFIGURACION_ALIAS.md)** - ✅ Estado de configuración de alias
5. **[05-GUIA_USO_PEI.md](05-GUIA_USO_PEI.md)** - Guía detallada del modo interactivo
6. **[06-ANALISIS_MEJORAS_DETECCION_ERRORES.md](06-ANALISIS_MEJORAS_DETECCION_ERRORES.md)** - Análisis de mejoras y detección de errores
7. **[07-CHANGELOG.md](07-CHANGELOG.md)** - Historial de cambios

## 🎯 Características Principales

### Detección Automática de Contexto

El script detecta automáticamente:

- Ubicación actual en el proyecto
- Tipo de contenido (producción, desarrollo, auxiliares, etc.)
- Reglas aplicables
- Ejemplos funcionales relevantes

### Enriquecimiento desde `.claude/`

El archivo generado incluye:

#### 🔄 Workflow del Proyecto (Priorizado)
- Contenido de `.claude/docs/WORKFLOW_PASO_A_PASO.md`
- Pasos detallados del flujo de trabajo

#### 🔧 Herramientas Disponibles
- Skills de `.claude/skills/`
- Comandos de `.claude/commands/`
- Hooks de `.claude/hooks/`

#### 📚 Recursos Adicionales
- Patrones de errores conocidos (si aplica)
- Ejemplos funcionales de `A-Produccion/`
- Instrucciones específicas para la IA

## 🔧 Opciones Disponibles

```bash
-h, --help              Mostrar ayuda
-i, --interactive       Modo interactivo (por defecto)
-f, --file FILE         Leer prompt desde archivo
-o, --output FILE       Guardar prompt mejorado en archivo
-c, --clipboard         Copiar prompt mejorado al portapapeles
```

## 📊 Ejemplo de Salida (archivo .txt generado)

```markdown
# PROMPT MEJORADO - PROYECTO ICFES R-EXAMS

## 📍 CONTEXTO DEL PROYECTO
- Proyecto: Sistema de generación de ejercicios matemáticos ICFES
- Ubicación actual: A-Produccion/En-Desarrollo
- Tipo de contexto: desarrollo

## 🔄 WORKFLOW DEL PROYECTO
[Contenido de WORKFLOW_PASO_A_PASO.md...]

## 🔧 HERRAMIENTAS Y RECURSOS DISPONIBLES
### Skills Disponibles
[Lista de skills...]

### Comandos Disponibles
[Lista de comandos...]

### Hooks de Automatización
[Lista de hooks...]

## 📚 EJEMPLOS FUNCIONALES DISPONIBLES
[Lista de ejemplos...]

## 💡 RECOMENDACIONES SEGÚN CONTEXTO
[Recomendaciones específicas...]

---

## 🎯 SOLICITUD DEL USUARIO
> [Tu prompt original]

## 📋 INSTRUCCIONES PARA LA IA
1. Priorizar uso de skills y comandos de .claude/
2. Consultar ejemplos funcionales antes de generar código
3. Validar compatibilidad con sistema exams2*
```

## 🎨 Casos de Uso

- ✅ Generar ejercicios ICFES con cualquier IA
- ✅ Corregir errores con skills y hooks disponibles
- ✅ Desarrollar siguiendo workflow documentado
- ✅ Adaptar ejercicios usando ejemplos funcionales
- ✅ Compatible con ChatGPT, Claude, Gemini, Copilot, etc.

## 🔍 Verificación

### Verificar Configuración de Alias

```bash
# Ejecutar script de verificación
./Auxiliares/Prompt-Enhancer/verificar-alias.sh
```

Este script verifica:

- ✅ Alias en ~/.bashrc
- ✅ Alias en ~/.zshrc
- 📋 Instrucciones de activación
- 📝 Ejemplos de uso

### Verificar Funcionamiento

```bash
# Activar alias (si no lo has hecho)
source ~/.bashrc

# Probar con alias
pe "Test"

# O con ruta completa
./Auxiliares/Prompt-Enhancer/prompt-enhancer.sh "Prueba de funcionamiento"
```

## 📝 Notas Importantes

- **Archivo siempre generado**: SIEMPRE crea `prompt_mejorado_YYYYMMDD_HHMMSS.txt`
- **Portabilidad**: Funciona desde cualquier subcarpeta del proyecto
- **Compatibilidad IA**: El archivo es compatible con cualquier IA
- **Enfoque en .claude/**: Solo incluye información de `.claude/`
- **Sin dependencias pesadas**: Solo requiere bash estándar
- **Opcional**: xclip, pbcopy o wl-copy para portapapeles

## 🆕 Novedades v2.0.0 (2025-12-21)

### Cambio Mayor de Enfoque
- ✅ **Enfoque exclusivo en `.claude/`** - Eliminadas referencias a `.augment/` y `.claudedoc/`
- ✅ **Generación automática de archivo .txt** - SIEMPRE genera archivo
- ✅ **Formato markdown estándar** - Compatible con cualquier IA
- ✅ **Prioriza workflow documentado** - Incluye WORKFLOW_PASO_A_PASO.md
- ✅ **Incluye hooks** - Lista hooks de automatización

### Nueva Estructura del Prompt
- ✅ Sección de workflow priorizada
- ✅ Skills, comandos y hooks listados
- ✅ Solicitud del usuario destacada
- ✅ Instrucciones específicas para la IA (nueva sección)

## 🚨 Solución de Problemas

### Error: "command not found: pe"

**Causa:** Los alias no están activados en la terminal actual.

**Solución:**
```bash
source ~/.bashrc  # o source ~/.zshrc si usas zsh
```

O simplemente abre una nueva terminal.

### Error: "Permission denied"

**Solución:**
```bash
chmod +x Auxiliares/Prompt-Enhancer/prompt-enhancer.sh
```

### Error: "No se pudo encontrar la raíz del proyecto"

**Causa:** No estás dentro del repositorio.

**Solución:** Asegúrate de estar dentro del repositorio RepositorioMatematicasICFES_R_Exams

### Los alias no funcionan en scripts

**Explicación:** Los alias solo funcionan en shells interactivos.

**Solución:** En scripts, usa la ruta completa o la variable de entorno:
```bash
$ICFES_PROJECT_ROOT/Auxiliares/Prompt-Enhancer/prompt-enhancer.sh "Tu prompt"
```

---

**Autor**: Sistema ICFES R-Exams  
**Licencia**: Uso interno del proyecto

