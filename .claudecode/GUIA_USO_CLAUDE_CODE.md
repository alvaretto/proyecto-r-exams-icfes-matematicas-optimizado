# 🚀 Guía de Uso: Automatización ICFES dentro de Claude Code

Esta guía explica cómo usar los scripts, agentes y workflows creados dentro del **entorno dinámico de Claude Code** (Cursor/Claude Desktop).

---

## 📋 Tabla de Contenidos

1. [Uso Directo de Scripts Bash](#1-uso-directo-de-scripts-bash)
2. [Integración con Comandos de Claude Code](#2-integración-con-comandos-de-claude-code)
3. [Configuración de Agentes Personalizados](#3-configuración-de-agentes-personalizados)
4. [Ejecución de Workflows](#4-ejecución-de-workflows)
5. [Ejemplos Prácticos de Flujos de Trabajo](#5-ejemplos-prácticos-de-flujos-de-trabajo)

---

## 1. Uso Directo de Scripts Bash

### Desde el Chat de Claude Code

Puedes pedirle a Claude Code que ejecute los scripts directamente:

```
Ejecuta la validación YAML del archivo ejercicio.Rmd usando el script 
.claudecode/examples/basico/01-pre-commit-yaml-validator.sh
```

Claude Code ejecutará el comando y te mostrará los resultados.

### Comandos Específicos que Puedes Usar

#### Validación Básica Individual
```
Ejecuta: .claudecode/examples/basico/01-pre-commit-yaml-validator.sh A-Produccion/mi_ejercicio.Rmd
```

```
Ejecuta: .claudecode/examples/basico/02-latex-escape-validator.sh A-Produccion/mi_ejercicio.Rmd
```

```
Ejecuta: .claudecode/examples/basico/03-metadata-icfes-validator.sh A-Produccion/mi_ejercicio.Rmd
```

#### Validación Completa
```
Ejecuta las tres validaciones básicas en secuencia para el archivo A-Produccion/mi_ejercicio.Rmd
```

---

## 2. Integración con Comandos de Claude Code

### Usando `@codebase` para Contexto

Claude Code puede leer y analizar archivos usando `@codebase`:

```
Lee el archivo .claudedoc/guia_estilo_icfes.md y valida que mi archivo 
A-Produccion/mi_ejercicio.Rmd cumpla con todos los estándares mencionados.
```

### Combinando Lectura y Validación

```
1. Lee A-Produccion/mi_ejercicio.Rmd
2. Consulta .claudedoc/guia_estilo_icfes.md
3. Ejecuta .claudecode/examples/basico/01-pre-commit-yaml-validator.sh sobre el archivo
4. Compara los resultados y sugiere correcciones
```

---

## 3. Configuración de Agentes Personalizados

### Agente Validador de Estilo (Ejemplo 04)

**Activar el agente manualmente:**

```
Actúa como el Agente Validador de Estilo ICFES. 

Lee el archivo A-Produccion/mi_ejercicio.Rmd y valídalo contra 
.claudedoc/guia_estilo_icfes.md.

Para cada problema encontrado, proporciona:
- [SEVERIDAD] Descripción del problema
- Ubicación (línea/sección)
- Código actual
- Corrección sugerida
- Referencia a la sección de guia_estilo_icfes.md
```

### Agente Corrector Automático (Ejemplo 05)

**Para correcciones automáticas:**

```
Actúa como el Agente Corrector Automático ICFES.

Lee A-Produccion/mi_ejercicio.Rmd y aplica las siguientes correcciones automáticas:
1. Agregar campos faltantes en YAML
2. Corregir set.seed() fijo a aleatorio
3. Escapar caracteres especiales LaTeX en texto markdown
4. Agregar opciones faltantes (scipen, OutDec) en chunk inicio

Muestra los cambios antes de aplicarlos.
```

**Para aplicar correcciones automáticamente:**

```
Aplica las correcciones automáticas al archivo A-Produccion/mi_ejercicio.Rmd 
según la guía de estilo. Crea una versión corregida mostrando los cambios 
aplicados.
```

### Agente Comparador de Estructura (Ejemplo 06)

```
Compara la estructura de A-Produccion/mi_ejercicio.Rmd contra los ejemplos 
funcionales en A-Produccion/Ejemplos-Funcionales-Rmd/.

Identifica:
- Desviaciones de patrones estándar
- Chunks faltantes o en orden incorrecto
- Configuraciones diferentes a los ejemplos validados
- Recomendaciones específicas para alinear estructura
```

---

## 4. Ejecución de Workflows

### Workflow Completo de Validación (Ejemplo 09)

**Ejecución manual paso a paso:**

```
Ejecuta el workflow completo de validación para A-Produccion/mi_ejercicio.Rmd:

1. Ejecuta: .claudecode/examples/basico/01-pre-commit-yaml-validator.sh
2. Ejecuta: .claudecode/examples/basico/03-metadata-icfes-validator.sh
3. Si hay errores, muestra análisis detallado
4. Ejecuta: .claudecode/examples/avanzado/07-skill-render-validator.sh
5. Si falla renderizado, analiza los errores y sugiere correcciones
6. Proporciona un resumen completo de validación
```

**Ejecución automática del workflow:**

```
Ejecuta el script de workflow completo:
.claudecode/workflows/validation_chain.sh A-Produccion/mi_ejercicio.Rmd

Muestra los resultados paso a paso.
```

---

## 5. Ejemplos Prácticos de Flujos de Trabajo

### Flujo 1: Validación Rápida Pre-Commit

**Prompt para Claude Code:**
```
Estoy a punto de hacer commit del archivo A-Produccion/nuevo_ejercicio.Rmd.

Ejecuta todas las validaciones básicas (YAML, metadatos ICFES, caracteres 
especiales) y dime si está listo para commit o qué correcciones necesita.
```

**Claude Code ejecutará:**
```bash
.claudecode/examples/basico/01-pre-commit-yaml-validator.sh A-Produccion/nuevo_ejercicio.Rmd
.claudecode/examples/basico/02-latex-escape-validator.sh A-Produccion/nuevo_ejercicio.Rmd
.claudecode/examples/basico/03-metadata-icfes-validator.sh A-Produccion/nuevo_ejercicio.Rmd
```

### Flujo 2: Corrección Automática Completa

**Prompt para Claude Code:**
```
Tengo un archivo .Rmd con errores. Necesito que:

1. Valides el archivo contra guia_estilo_icfes.md
2. Identifiques todos los errores
3. Apliques correcciones automáticas posibles
4. Muestres un diff de los cambios aplicados
5. Guardes la versión corregida como archivo_nuevo.Rmd

Archivo: A-Produccion/ejercicio_con_errores.Rmd
```

### Flujo 3: Validación con Renderizado Real

**Prompt para Claude Code:**
```
Valida completamente el archivo A-Produccion/mi_ejercicio.Rmd incluyendo 
renderizado real:

1. Validaciones estáticas (YAML, metadatos)
2. Renderizado de prueba usando exams2html
3. Análisis de errores si el renderizado falla
4. Sugerencias de corrección basadas en errores reales

Muestra resultados detallados de cada paso.
```

**Claude Code ejecutará:**
```bash
# Paso 1: Validaciones básicas
.claudecode/examples/basico/01-pre-commit-yaml-validator.sh A-Produccion/mi_ejercicio.Rmd
.claudecode/examples/basico/03-metadata-icfes-validator.sh A-Produccion/mi_ejercicio.Rmd

# Paso 2: Renderizado
.claudecode/examples/avanzado/07-skill-render-validator.sh A-Produccion/mi_ejercicio.Rmd /tmp/render_test

# Paso 3: Si hay errores, análisis
# (Claude Code analizará los logs de error)
```

### Flujo 4: Aprendizaje de Ejemplos Funcionales

**Prompt para Claude Code:**
```
Quiero crear un nuevo ejercicio .Rmd siguiendo los patrones de los ejemplos 
funcionales.

1. Lee varios ejemplos de A-Produccion/Ejemplos-Funcionales-Rmd/
2. Identifica los patrones comunes de estructura
3. Crea un template nuevo siguiendo esos patrones
4. Valida el template contra guia_estilo_icfes.md
5. Muestra el template listo para usar
```

### Flujo 5: Diagnóstico Completo de Errores

**Prompt para Claude Code:**
```
El archivo A-Produccion/ejercicio_roto.Rmd no renderiza. Haz un diagnóstico 
completo:

1. Ejecuta validaciones estáticas
2. Intenta renderizar y captura el error
3. Analiza el log de error en detalle
4. Identifica el tipo de error (YAML, LaTeX, TikZ, R, etc.)
5. Consulta guia_estilo_icfes.md para la solución
6. Proporciona corrección específica con código antes/después
```

---

## 🎯 Comandos Claude Code Especializados

### Validación en Lote

```
Valida todos los archivos .Rmd en A-Produccion/Ejercicios-Nuevos/ usando 
los validadores básicos. Crea un reporte resumen con errores encontrados.
```

### Corrección Interactiva

```
Lee A-Produccion/mi_ejercicio.Rmd y para cada error encontrado:
1. Muestra el problema
2. Pregunta si quiero corregirlo automáticamente
3. Aplica la corrección solo si confirmo
4. Muestra el resultado final
```

### Integración con Edición de Archivos

```
Edita el archivo A-Produccion/mi_ejercicio.Rmd aplicando estas correcciones:
- Agregar latex_engine: xelatex si falta
- Corregir set.seed(12345) a set.seed(sample(1:100000, 1))
- Escapar caracteres & y % en el texto de la pregunta
- Agregar options(scipen = 999) en el chunk inicio

Muestra el diff antes de guardar.
```

---

## 🔧 Configuración Avanzada

### Crear Alias/Atajos Personalizados

Puedes pedirle a Claude Code que recuerde comandos frecuentes:

```
Cuando diga "validar icfes [archivo]", ejecuta:
1. .claudecode/examples/basico/01-pre-commit-yaml-validator.sh [archivo]
2. .claudecode/examples/basico/03-metadata-icfes-validator.sh [archivo]
3. .claudecode/examples/basico/02-latex-escape-validator.sh [archivo]
4. Muestra resumen consolidado
```

### Integración con Git

```
Antes de hacer commit, valida todos los archivos .Rmd modificados usando 
los scripts de validación. Si hay errores, muestra qué archivos necesitan 
corrección antes del commit.
```

---

## 📝 Ejemplo Completo de Sesión

```
Usuario: Tengo un archivo nuevo ejercicio.Rmd que creé. ¿Está listo para usar?

Claude Code: [Ejecuta validaciones]

Usuario: Hay algunos errores. ¿Puedes corregirlos automáticamente?

Claude Code: [Aplica correcciones automáticas]

Usuario: Ahora intenta renderizarlo para asegurarte de que funciona.

Claude Code: [Ejecuta renderizado y verifica]

Usuario: Perfecto. Ahora compara su estructura con los ejemplos funcionales 
para ver si sigue las mejores prácticas.

Claude Code: [Compara y sugiere mejoras]
```

---

## 💡 Tips y Mejores Prácticas

1. **Usa prompts específicos**: Menciona exactamente qué validación quieres ejecutar
2. **Combina validaciones**: Pide múltiples validaciones en un solo prompt
3. **Pide contexto**: Solicita que Claude Code consulte guia_estilo_icfes.md cuando valide
4. **Itera**: Después de correcciones, vuelve a validar
5. **Usa ejemplos funcionales**: Pide comparación con ejemplos validados cuando creas archivos nuevos

---

## 🚨 Solución de Problemas

### Si los scripts no se ejecutan:

```
Verifica que los scripts tienen permisos de ejecución:
chmod +x .claudecode/examples/basico/*.sh
chmod +x .claudecode/workflows/validation_chain.sh
```

### Si R no se encuentra:

```
Verifica la instalación de R:
which Rscript
Rscript --version
```

### Para debugging:

```
Ejecuta el script con salida detallada y muéstrame todos los mensajes de error:
bash -x .claudecode/examples/basico/01-pre-commit-yaml-validator.sh archivo.Rmd
```

---

**Última actualización**: 2025-01-XX  
**Versión**: 1.0.0
