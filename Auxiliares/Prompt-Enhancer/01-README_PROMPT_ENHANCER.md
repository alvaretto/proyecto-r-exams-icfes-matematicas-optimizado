# 🚀 PROMPT ENHANCER - Sistema de Mejora de Prompts con Contexto del Proyecto

**Versión**: 1.2.0
**Última actualización**: 2025-12-21
**Estado**: ✅ Estable y refactorizado

## 📋 Descripción

**Prompt Enhancer** es un script bash inteligente que mejora automáticamente los
prompts del usuario añadiendo contexto relevante del proyecto ICFES R-Exams.
Funciona desde cualquier ubicación dentro del repositorio y proporciona información
contextual específica según la carpeta desde donde se ejecuta.

**Ubicación**: `Auxiliares/Prompt-Enhancer/`

## 🆕 Última Actualización - v1.2.0 (2025-12-21)

### Refactorización Mayor
Esta versión incluye una refactorización completa del código con:
- ✅ **2 bugs críticos corregidos** (lectura de archivos y manejo de argumentos)
- ✅ **17 funciones modulares** (vs 6 en v1.1.0)
- ✅ **+41% más líneas de código** bien estructuradas (618 vs 437)
- ✅ **Soporte Wayland** para portapapeles (wl-copy)
- ✅ **Mejor manejo de errores** y validaciones

Ver detalles completos en: [CHANGELOG.md](CHANGELOG.md)

## 🎯 Características Principales

### ✅ Detección Automática de Contexto
- **Ubicación actual**: Detecta automáticamente desde qué carpeta se está ejecutando
- **Tipo de contenido**: Identifica si estás en producción, desarrollo, auxiliares, etc.
- **Raíz del proyecto**: Encuentra la raíz del proyecto desde cualquier subcarpeta

### ✅ Enriquecimiento Inteligente Completo
- **Reglas del proyecto**: Incluye reglas de `.claude/`
- **Documentación técnica**: Incluye documentación de `.claude/`
- **Guía de estilo**: Incluye guía de estilo de `.claudedoc/`
- **Ejemplos funcionales**: Lista ejemplos relevantes disponibles
- **Recomendaciones contextuales**: Sugerencias específicas según la ubicación

### ✅ Múltiples Modos de Uso
- **Modo interactivo**: Ingresa tu prompt directamente
- **Desde archivo**: Lee prompts desde archivos de texto
- **Salida a archivo**: Guarda el prompt mejorado
- **Portapapeles**: Copia automáticamente el resultado

## 🔧 Instalación y Configuración

### ✅ Estado Actual: CONFIGURADO

Los alias ya están configurados en tu sistema:
- ✅ **~/.bashrc** (Bash)
- ✅ **~/.zshrc** (Zsh)

**Ver detalles de configuración**: [CONFIGURACION_ALIAS.md](CONFIGURACION_ALIAS.md)

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

**Resultado**: El prompt mejorado incluirá:

- Contexto de que estás en producción
- Reglas de `.augment/rules/`, `.claude/` y `.claudedoc/`
- Lista de ejemplos funcionales cercanos
- Recomendaciones para validación

### Ejemplo 2: Desde Carpeta de Desarrollo

```bash
cd A-Produccion/En-Desarrollo/
../../Auxiliares/Prompt-Enhancer/prompt-enhancer.sh "Ayúdame a implementar un gráfico TikZ"

# O con alias
pe "Ayúdame a implementar un gráfico TikZ"
```

**Resultado**: El prompt mejorado incluirá:

- Contexto de desarrollo activo
- Reglas de metodología TikZ de `.augment/` y `.claude/`
- Referencias a templates
- Recomendaciones de documentación

### Ejemplo 3: Desde Auxiliares

```bash
cd Auxiliares/Scripts/
../Prompt-Enhancer/prompt-enhancer.sh "Crea un script de validación"

# O con alias
pe "Crea un script de validación"
```

**Resultado**: El prompt mejorado incluirá:

- Contexto de herramientas auxiliares
- Reglas de compatibilidad con scripts existentes
- Documentación técnica de `.claude/`
- Recomendaciones de testing

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

El prompt mejorado incluye las siguientes secciones:

```markdown
# PROMPT MEJORADO CON CONTEXTO DEL PROYECTO

## CONTEXTO DE UBICACIÓN
- Proyecto: RepositorioMatematicasICFES_R_Exams
- Ubicación actual: [ruta relativa]
- Tipo de contexto: [producción/desarrollo/etc]
- Descripción: [descripción del contexto]

## 📋 REGLAS GENERALES DEL PROYECTO (`.claude/`)
### Reglas Generales
[`.claude/`]

### Reglas Siempre Aplicables
[`.claude/`]

## 🔧 DOCUMENTACIÓN TÉCNICA (.claude/)
### Documentación Principal
[Extracto de .claude/docs/README.md]

### Solución de Problemas
[Extracto de .claude/TROUBLESHOOTING.md]

### Skills Disponibles
[Lista de skills en .claude/skills/]

### Comandos Disponibles
[Lista de comandos en .claude/commands/]

## 🎨 GUÍA DE ESTILO ICFES (.claudedoc/)
### Guía de Estilo ICFES
[Extracto de guia_estilo_icfes.md]

## EJEMPLOS FUNCIONALES DISPONIBLES
[Lista de archivos .Rmd de ejemplo]

## RECOMENDACIONES SEGÚN CONTEXTO
[Recomendaciones específicas según ubicación]

## SOLICITUD DEL USUARIO
[Tu prompt original]
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

1. **Portabilidad**: El script funciona desde cualquier subcarpeta del proyecto
2. **Detección automática**: No necesitas especificar la raíz del proyecto
3. **Contexto dinámico**: Las recomendaciones cambian según tu ubicación
4. **Sin dependencias pesadas**: Solo requiere bash estándar

## 🔧 Solución de Problemas

### Error: "No se pudo encontrar la raíz del proyecto"

**Causa**: No estás dentro del repositorio RepositorioMatematicasICFES_R_Exams

**Solución**: Navega a cualquier carpeta dentro del proyecto

### Advertencia: "xclip o pbcopy no están instalados"

**Causa**: Falta la herramienta de portapapeles

**Solución**: Instala xclip (Linux) o usa macOS que incluye pbcopy

## 📚 Recursos Relacionados

- **Documentación completa**: `Auxiliares/Prompt-Enhancer/01-README_PROMPT_ENHANCER.md`
- **Ejemplos de uso**: `Auxiliares/Prompt-Enhancer/02-EJEMPLOS_USO_PROMPT_ENHANCER.md`
- **Instalación y configuración**: `Auxiliares/Prompt-Enhancer/03-INSTALACION_CONFIGURACION_PROMPT_ENHANCER.md`
- **Reglas del proyecto**: `.augment/rules/reglas-generales.md`
- **Documentación técnica**: `.claude/docs/README.md`
- **Guía de estilo**: `.claudedoc/guia_estilo_icfes.md`
- **Ejemplos funcionales**: `A-Produccion/`

## 🎯 Próximas Mejoras

- [ ] Integración directa con APIs de IA
- [ ] Detección de tipo de ejercicio (schoice, cloze)
- [ ] Sugerencias de metadatos ICFES
- [ ] Análisis de archivos .Rmd existentes en la ubicación actual

---

**Versión**: 1.2.0
**Fecha**: 2025-12-21
**Autor**: Sistema ICFES R-Exams
**Licencia**: Uso interno del proyecto

**Changelog**: Ver [CHANGELOG.md](CHANGELOG.md) para detalles completos de todas las versiones

