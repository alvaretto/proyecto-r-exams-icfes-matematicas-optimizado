# 🚀 PROMPT ENHANCER - Sistema de Mejora de Prompts

**Versión**: 1.0.0  
**Ubicación**: `Auxiliares/Prompt-Enhancer/`  
**Fecha**: 2025-12-20

## 📋 Descripción Rápida

Sistema inteligente que mejora automáticamente los prompts del usuario añadiendo contexto completo del proyecto ICFES R-Exams, incluyendo:

- ✅ Reglas de `.augment/rules/`
- ✅ Documentación técnica de `.claude/`
- ✅ Guía de estilo de `.claudedoc/`
- ✅ Ejemplos funcionales disponibles
- ✅ Recomendaciones contextuales según ubicación

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

## 📚 Documentación Completa

1. **[CONFIGURACION_ALIAS.md](CONFIGURACION_ALIAS.md)** - ✅ Estado de configuración de alias
2. **[01-README_PROMPT_ENHANCER.md](01-README_PROMPT_ENHANCER.md)** - Documentación completa
3. **[02-EJEMPLOS_USO_PROMPT_ENHANCER.md](02-EJEMPLOS_USO_PROMPT_ENHANCER.md)** - Ejemplos prácticos
4. **[03-INSTALACION_CONFIGURACION_PROMPT_ENHANCER.md](03-INSTALACION_CONFIGURACION_PROMPT_ENHANCER.md)** - Instalación y configuración avanzada

## 🎯 Características Principales

### Detección Automática de Contexto

El script detecta automáticamente:
- Ubicación actual en el proyecto
- Tipo de contenido (producción, desarrollo, auxiliares, etc.)
- Reglas aplicables
- Ejemplos funcionales relevantes

### Enriquecimiento Completo

Incluye en el prompt mejorado:

#### 📋 Reglas Generales (.augment/rules/)
- Reglas generales del proyecto
- Reglas siempre aplicables
- Filosofía Matemáticas ICFES 2025

#### 🔧 Documentación Técnica (.claude/)
- Documentación principal del sistema
- Solución de problemas (TROUBLESHOOTING)
- Skills disponibles
- Comandos disponibles

#### 🎨 Guía de Estilo (.claudedoc/)
- Guía de estilo ICFES
- Estructura obligatoria de archivos .Rmd
- Criterios de calidad

## 🔧 Opciones Disponibles

```bash
-h, --help              Mostrar ayuda
-i, --interactive       Modo interactivo (por defecto)
-f, --file FILE         Leer prompt desde archivo
-o, --output FILE       Guardar prompt mejorado en archivo
-c, --clipboard         Copiar prompt mejorado al portapapeles
```

## 📊 Ejemplo de Salida

```markdown
# PROMPT MEJORADO CON CONTEXTO DEL PROYECTO

## CONTEXTO DE UBICACIÓN
- Proyecto: RepositorioMatematicasICFES_R_Exams
- Ubicación actual: A-Produccion/En-Desarrollo
- Tipo de contexto: desarrollo

## 📋 REGLAS GENERALES DEL PROYECTO (.augment/rules/)
[Reglas completas...]

## 🔧 DOCUMENTACIÓN TÉCNICA (.claude/)
[Documentación técnica...]

## 🎨 GUÍA DE ESTILO ICFES (.claudedoc/)
[Guía de estilo...]

## EJEMPLOS FUNCIONALES DISPONIBLES
[Lista de ejemplos...]

## RECOMENDACIONES SEGÚN CONTEXTO
[Recomendaciones específicas...]

## SOLICITUD DEL USUARIO
[Tu prompt original]
```

## 🎨 Casos de Uso

- ✅ Generar ejercicios ICFES con contexto completo
- ✅ Corregir errores con documentación técnica incluida
- ✅ Desarrollar nuevos scripts con reglas del proyecto
- ✅ Adaptar ejercicios existentes con ejemplos funcionales
- ✅ Integración con herramientas de IA (Augment, Claude, ChatGPT)

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

- **Portabilidad**: Funciona desde cualquier subcarpeta del proyecto
- **Detección automática**: No necesitas especificar la raíz del proyecto
- **Contexto completo**: Incluye TODAS las reglas y documentación del proyecto
- **Sin dependencias pesadas**: Solo requiere bash estándar
- **Opcional**: xclip para funcionalidad de portapapeles

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

