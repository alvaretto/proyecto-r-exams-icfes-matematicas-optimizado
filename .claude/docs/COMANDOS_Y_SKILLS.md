# Comandos y Skills del Sistema

## 📋 Índice

1. [Comandos Manuales (Slash Commands)](#comandos-manuales-slash-commands)
2. [Skills Automáticos](#skills-automáticos)
3. [Referencia Rápida](#referencia-rápida)

---

## Comandos Manuales (Slash Commands)

**Invocación**: El usuario debe ejecutarlos explícitamente con `/nombre-comando`

### Workflow Principal

#### `/analizar-icfes`
**Propósito**: Iniciar análisis de imagen ICFES para clasificación multidimensional

**Uso**:
```bash
/analizar-icfes
```

**Qué hace**:
1. Solicita imagen del ejercicio ICFES
2. Analiza según las 6 dimensiones ICFES:
   - Competencia (Interpretación/Formulación/Argumentación)
   - Componente (Aleatorio/Cambio/Datos/Espacial/Medida)
   - Afirmación
   - Evidencia
   - Nivel (1-4)
   - Tipo (SCHOICE/CLOZE)
3. Detecta si requiere gráficos (activa Flujo B si aplica)
4. Genera reporte de análisis completo

**Output**: Archivo JSON con clasificación multidimensional

---

#### `/generar-schoice`
**Propósito**: Generar ejercicio de selección única (Single Choice)

**Uso**:
```bash
/generar-schoice
```

**Requisitos previos**:
- Análisis ICFES completado
- Si tiene gráficos: Flujo B completado

**Qué hace**:
1. Verifica requisitos previos
2. Genera archivo .Rmd con estructura SCHOICE
3. Incluye metadatos ICFES completos
4. Aplica plantilla validada
5. Ejecuta Ciclo de Validación automático

**Output**: Archivo `.Rmd` listo para renderizar

**Metadatos generados**:
```yaml
extype: schoice
exsolution: [binario, ej: 01000]
exshuffle: TRUE  # OBLIGATORIO
exname: nombre_ejercicio
exextra[Type]: SCHOICE
exextra[Competencia]: [clasificación]
exextra[Componente]: [clasificación]
# ... resto de metadatos ICFES
```

---

#### `/generar-cloze`
**Propósito**: Generar ejercicio compuesto (pregunta con múltiples partes)

**Uso**:
```bash
/generar-cloze
```

**Requisitos previos**:
- Análisis ICFES completado
- Si tiene gráficos: Flujo B completado

**Qué hace**:
1. Verifica requisitos previos
2. Genera archivo .Rmd con estructura CLOZE
3. Define tipos de subpreguntas (num/schoice/mchoice/string)
4. Configura tolerancias numéricas
5. Ejecuta Ciclo de Validación automático

**Output**: Archivo `.Rmd` listo para renderizar

**Metadatos generados**:
```yaml
extype: cloze
exclozetype: num|schoice|mchoice|string
```

---

#### `/validar-pedagogico` 🆕
**Propósito**: Análisis pedagógico avanzado basado en evidencias científicas y taxonomías cognitivas modernas

**Uso**:
```bash
/validar-pedagogico [ruta-al-ejercicio.Rmd]
```

**Requisitos previos**:
- Archivo .Rmd generado y renderizado
- (Opcional) Ciclo de Validación completado

**Qué hace**:
1. **Módulo 1**: Análisis cognitivo multinivel (Bloom, SOLO, Webb DOK)
2. **Módulo 2**: Validación ICFES (6 dimensiones obligatorias)
3. **Módulo 3**: Análisis avanzado de distractores (6 tipologías)
4. **Módulo 4**: Optimización pedagógica (7 principios científicos)
5. **Módulo 5**: Meta-evaluación (puntuación 0-100)

**Output**: Reporte completo con:
- Clasificación taxonómica triple
- Validación Marco Conceptual ICFES 2026
- Evaluación de distractores por tipología de error
- Aplicación de principios de aprendizaje
- Puntuación compuesta con desglose
- Recomendaciones específicas de mejora

**Modelo**: Claude Opus 4.5 (máxima capacidad cognitiva)

**Documentos consultados automáticamente**:
- `errores-conceptuales-matematicas.md`
- `principios-aprendizaje-evidencias.md`
- `taxonomias-cognitivas-integradas.md`
- `marco-conceptual-icfes-2026.md`
- `diseno-distractores-tipologia.md`

**Diferencia con `/analizar-icfes`**:
- `/analizar-icfes`: Clasificación inicial ANTES de crear .Rmd (input: imagen)
- `/validar-pedagogico`: Análisis pedagógico profundo DESPUÉS de crear .Rmd (input: archivo completo)

---
exsolution: [valores separados por |]
extol: [tolerancias numéricas]
exname: nombre_ejercicio
exextra[Type]: CLOZE
# ... resto de metadatos ICFES
```

---

#### `/promover-ejercicio`
**Propósito**: Mover ejercicio validado de En-Desarrollo a Nuevos-Ejercicios

**Uso**:
```bash
/promover-ejercicio nombre_ejercicio.Rmd
```

**Requisitos previos**:
- Ejercicio en `A-Produccion/En-Desarrollo/`
- Ciclo de Validación completo (FASE 1+2+3 exitosas)
- Tests pasando 100%
- Usuario aprobó explícitamente

**Qué hace**:
1. Verifica estado de validación
2. Ejecuta tests finales
3. Mueve archivo a `A-Produccion/Nuevos-Ejercicios/`
4. Actualiza índice de ejercicios
5. Registra en historial

**Output**: Ejercicio en producción listo para usar

---

### Graficador Experto (Flujo B - SECUENCIAL)

**⚠️ IMPORTANTE**: Estos comandos deben ejecutarse SECUENCIALMENTE, no simultáneamente.

#### `/auto-refinar-grafico tikz`
**Propósito**: Generar y refinar versión TikZ del gráfico (PRIMERO)

**Uso**:
```bash
/auto-refinar-grafico tikz [umbral_similitud]
```

**Ejemplo**:
```bash
/auto-refinar-grafico tikz 95
```

**Qué hace**:
1. Genera código TikZ dinámico desde R
2. Compila a PDF y convierte a PNG
3. Compara con imagen original
4. Itera hasta alcanzar umbral de similitud (≥95%)
5. Verifica las 5 coherencias
6. Solicita aprobación del usuario
7. SOLO continúa si usuario aprueba

**Iteraciones**:
- v1, v2, v3... hasta alcanzar similitud
- Máximo 10 iteraciones por defecto

**Output**: `output_tikz_vN.tex`, `tikz_output_vN.png`

---

#### `/auto-refinar-grafico python`
**Propósito**: Generar y refinar versión Python del gráfico (SEGUNDO)

**Uso**:
```bash
/auto-refinar-grafico python [umbral_similitud]
```

**Requisito previo**: TikZ aprobado por usuario

**Qué hace**:
1. Usa misma lógica matemática que TikZ
2. Genera código Python/matplotlib
3. Compila con reticulate
4. Compara con imagen original
5. Itera hasta alcanzar umbral
6. Verifica las 5 coherencias
7. Solicita aprobación del usuario
8. SOLO continúa si usuario aprueba

**Output**: `output_python_vN.py`, `python_output_vN.png`

---

#### `/auto-refinar-grafico r`
**Propósito**: Generar y refinar versión R del gráfico (TERCERO)

**Uso**:
```bash
/auto-refinar-grafico r [umbral_similitud]
```

**Requisito previo**: Python aprobado por usuario

**Qué hace**:
1. Usa misma lógica matemática que versiones previas
2. Genera código R/ggplot2 nativo
3. Compila a PNG
4. Compara con imagen original
5. Itera hasta alcanzar umbral
6. Verifica las 5 coherencias
7. Solicita aprobación del usuario
8. Pregunta al usuario cuál versión usar en .Rmd

**Output**: `output_r_vN.R`, `r_output_vN.png`

---

#### `/estado-graficador`
**Propósito**: Consultar estado actual del workflow gráfico

**Uso**:
```bash
/estado-graficador
```

**Qué muestra**:
```json
{
  "fase_actual": "tikz_iteracion|python_iteracion|r_iteracion|seleccion_final",
  "tikz": {
    "estado": "pendiente|en_iteracion|verificando|aprobado",
    "version_actual": 3,
    "similitud_actual": 96,
    "coherencias_verificadas": true,
    "usuario_aprobo": true
  },
  "python": {
    "estado": "en_iteracion",
    "version_actual": 2,
    "similitud_actual": 94,
    "coherencias_verificadas": false,
    "usuario_aprobo": false
  },
  "r": {
    "estado": "bloqueado",
    "version_actual": 0
  }
}
```

**Sin argumentos**

---

#### `/exportar-graficos`
**Propósito**: Exportar resultados finales del Flujo B

**Uso**:
```bash
/exportar-graficos
```

**Requisito previo**: Flujo B completado (TikZ/Python/R aprobados)

**Qué hace**:
1. Copia versión seleccionada a directorio final
2. Genera reporte consolidado con:
   - Similitudes alcanzadas
   - Coherencias verificadas
   - Código fuente de cada versión
   - Comparaciones visuales
3. Archiva versiones intermedias

**Output**: Reporte PDF + archivos finales listos para .Rmd

---

## Skills Automáticos

**Invocación**: Claude los ejecuta automáticamente según contexto

### Validación (Ejecución Automática)

#### Validar Renderizado (FASE 1)
**Cuándo se ejecuta**: Después de generar/editar .Rmd

**Qué hace**:
```r
exams2html("archivo.Rmd", n = 1)
exams2pdf("archivo.Rmd", n = 1)
exams2pandoc("archivo.Rmd", n = 1, type = "docx")
exams2nops("archivo.Rmd", n = 1)
```

Captura errores/advertencias de cada formato

---

#### Validar Coherencia (FASE 2)
**Cuándo se ejecuta**: Después de renderizado exitoso (AUTOMÁTICO vía hook)

**FASE 2A - Validación Matemática** (automática):
```bash
Rscript .claude/scripts/validar_coherencia_matematica.R archivo.Rmd
```

**Verifica**:
- Chunks R sin errores
- Metadatos ICFES completos
- exshuffle = TRUE
- SCHOICE: exsolution binario, exactamente 1 correcta
- CLOZE: tipos/soluciones/tolerancias consistentes
- Variables sin NA/NaN/Inf
- Coherencia matemática entre variables

**FASE 2B - Preview Visual** (automática):
```bash
magick -density 150 archivo.pdf -quality 90 preview.png
```

Claude DEBE entonces:
1. `Read("preview.png")`
2. Verificar 5 coherencias VISUALMENTE
3. Documentar hallazgos
4. Solicitar aprobación del usuario

---

#### Diagnosticar Errores (FASE 3)
**Cuándo se ejecuta**: Cuando FASE 1 o 2 detectan problemas

**Qué hace**:
1. Analiza tipo de error
2. Consulta patrones conocidos (@.claude/docs/patrones-errores-conocidos.md)
3. Busca solución en ejemplos funcionales
4. Propone corrección específica
5. Aplica corrección
6. VUELVE A FASE 1 (re-renderiza)

---

### Corrección (Ejecución Automática en Errores)

#### Corregir Gráficos (SUBFASE 3A)
**Cuándo se ejecuta**: Error en renderizado de gráficos

**Qué hace**:
1. Identifica tipo de error:
   - TikZ no compila en HTML
   - Python falla con reticulate
   - R ggplot2 dimensiones incorrectas
2. Consulta ejemplos funcionales:
   ```
   /A-Produccion/Ejemplos-Funcionales-Rmd/
   ```
3. Aplica patrón de solución validado
4. Re-renderiza
5. Vuelve a FASE 2 (validación)

---

#### Corregir Errores Imagen TikZ
**Cuándo se ejecuta**: TikZ no renderiza correctamente

**Patrones comunes**:
- Falta `\usepackage{tikz}`
- Coordenadas fuera de rango
- Sintaxis TikZ incorrecta
- No usa `include_tikz()` correctamente

**Solución**:
```r
# Renderizado condicional
if (knitr::is_latex_output()) {
  include_tikz("grafico.tex", ...)
} else {
  knitr::include_graphics("grafico.png")
}
```

---

### Graficador (Ejecución SECUENCIAL)

#### Analizar Imagen Matemática
**Cuándo se ejecuta**: Al cargar imagen en `/analizar-icfes`

**Qué hace**:
1. Detecta si contiene gráficos matemáticos
2. Identifica tipo: barras/líneas/dispersión/geometría/etc.
3. Extrae valores numéricos visibles
4. Registra en análisis: `requiere_flujo_b: true/false`
5. Si TRUE → BLOQUEA generación .Rmd sin Flujo B

---

#### Generar Código [TikZ|Python|R]
**Cuándo se ejecuta**: Durante `/auto-refinar-grafico [lenguaje]`

**Proceso iterativo**:
```
1. Analizar imagen original
2. Generar código versión N
3. Compilar/renderizar
4. Comparar con original (similitud %)
5. Si < umbral → Ajustar → N+1 → repetir
6. Si ≥ umbral → Verificar coherencias
7. Solicitar aprobación usuario
8. Si rechazado → N+1 → repetir
9. Si aprobado → SIGUIENTE lenguaje
```

**Similitud**:
- Algoritmo: Comparación pixel-by-pixel con SSIM
- Umbral recomendado: ≥95%
- Máximo iteraciones: 10

---

## Referencia Rápida

### Flujo Completo Típico

```bash
# 1. Análisis inicial
/analizar-icfes
# → Detecta: Requiere gráficos

# 2. Flujo B (SECUENCIAL)
/auto-refinar-grafico tikz 95
# → Iterar hasta ≥95% + aprobar

/auto-refinar-grafico python 95
# → Iterar hasta ≥95% + aprobar

/auto-refinar-grafico r 95
# → Iterar hasta ≥95% + seleccionar versión

# 3. Generación de ejercicio
/generar-schoice
# O
/generar-cloze

# 4. Validación automática (skills)
# FASE 1: Renderizado 4 formatos
# FASE 2A: Validación matemática (hook automático)
# FASE 2B: Preview PNG (hook automático)
# Claude: Verifica 5 coherencias + solicita aprobación

# 5. Promoción
/promover-ejercicio nombre_ejercicio.Rmd
```

---

### Tabla de Comandos por Contexto

| Necesito... | Comando |
|-------------|---------|
| Empezar ejercicio nuevo | `/analizar-icfes` |
| Ejercicio sin gráficos | `/generar-schoice` o `/generar-cloze` |
| Ejercicio con gráficos | Flujo B primero → luego generar |
| Gráfico TikZ | `/auto-refinar-grafico tikz 95` |
| Gráfico Python | `/auto-refinar-grafico python 95` |
| Gráfico R | `/auto-refinar-grafico r 95` |
| Ver estado gráficos | `/estado-graficador` |
| Exportar gráficos | `/exportar-graficos` |
| Promover a producción | `/promover-ejercicio archivo.Rmd` |

---

### Skills que NO Requieren Invocación Manual

Estos se ejecutan **automáticamente**:
- Validar renderizado (después de generar .Rmd)
- Validar coherencia (después de renderizar, vía hook)
- Diagnosticar errores (cuando hay fallos)
- Corregir gráficos (cuando detecta errores gráficos)
- Analizar imagen matemática (al cargar imagen)
- **Skill-retroalimentación** (al generar sección Solution) 🆕

**NO ejecutar manualmente**. El sistema los invoca cuando corresponde.

---

### Skill-retroalimentación (OBLIGATORIO, AUTOMÁTICO, PERMANENTE) 🆕

**Cuándo se ejecuta**: Al generar la sección Solution de cualquier .Rmd

**Fuente oficial**: ICFES - Guía de Orientación Matemáticas 11° Cuadernillo 2-2023 (pp. 22-51)

**Qué genera automáticamente**:

1. **Encabezado diagnóstico**: Competencia, Componente, Afirmación, Evidencia, Nivel
2. **¿Qué evalúa?**: Descripción específica de la capacidad evaluada
3. **Justificación de respuesta correcta**: Pasos matemáticos con fórmulas LaTeX
4. **Opciones no válidas**: Para CADA distractor, análisis con patrón:
   > "Es posible que los estudiantes que eligen la opción X [error conceptual]..."
5. **Reflexión metacognitiva**: Estrategias para evitar errores comunes

**Ejemplo de salida**:
```markdown
### Opciones No Válidas

**Opción B:**
Es posible que los estudiantes que eligen la opción B confundan el
porcentaje con la cantidad absoluta, poniendo 30 en lugar de calcular
120 × 30/100 = 36. Este error se presenta cuando el estudiante no
comprende que el porcentaje es una proporción que debe aplicarse al
total. Para evitar este error, el estudiante debe recordar que:
$$\text{Cantidad} = \text{Total} \times \frac{\text{Porcentaje}}{100}$$
```

**Ubicación del skill**: `.claude/skills/skill-retroalimentacion/SKILL.md`

---

## Configuracion de Skills

**Ubicacion**: `.claude/skills/`

Cada skill sigue el patron **Progressive Disclosure** de Anthropic Agent Skills:

```
skill-name/
├── SKILL.md              # Archivo principal (~3-4KB)
│   ├── Frontmatter YAML  # name, description, allowed-tools
│   ├── Decision Tree     # Arbol de decision inicial
│   ├── Proceso paso a paso
│   └── Referencias a docs detallados
└── references/           # Documentacion extraida
    ├── patron-X.md
    └── ejemplos-Y.md
```

**Estructura del frontmatter**:

```yaml
---
name: nombre-skill
description: >
  Descripcion concisa del skill.
license: Proyecto Educativo - IE Pedacito de Cielo
compatibility: Requisitos del skill.
metadata:
  author: alvaretto
  version: "2.1"
  language: es
allowed-tools:
  - Read
  - Write
  - Bash(comando:*)
---
```

**Skills disponibles** (refactorizados v2.1):

| Skill | Tamano | Referencias | Proposito |
|-------|--------|-------------|-----------|
| analizar-icfes | 3.8KB | 3 | Clasificacion 6 dimensiones ICFES |
| analizar-imagen-grafica | 3.1KB | 2 | Extraccion de elementos visuales |
| comparar-similitud-visual | 3.7KB | 3 | Puntuacion 0-100 de similitud |
| corregir-error-imagen | 3.4KB | 1 | ERR_G1: File not found |
| corregir-graficos | 3.3KB | 1 | ERR_G1-G4: Errores graficos |
| diagnosticar-errores | 4.4KB | 2 | FASE 3: Clasificacion de errores |
| generar-cloze | 4.1KB | 2 | Ejercicios tipo CLOZE |
| generar-codigo-python | 3.4KB | 2 | Matplotlib para graficos |
| generar-codigo-r | 3.2KB | 2 | ggplot2 para graficos |
| generar-codigo-tikz | 3.3KB | 2 | TikZ/pgfplots para graficos |
| generar-schoice | 4.1KB | 2 | Ejercicios tipo SCHOICE |
| gestionar-estado-graficador | 3.5KB | 2 | workflow_state.json |
| refinar-codigo-grafico | 3.1KB | 2 | Iteracion hasta >=95% |
| transferir-conocimiento-grafico | 3.6KB | 2 | Lecciones entre lenguajes |
| validar-coherencia | 4.2KB | 2 | FASE 2: 5 coherencias |
| validar-pedagogico | 3.3KB | 1 | Analisis pedagogico Opus 4.5 |
| validar-renderizado | 3.7KB | 2 | FASE 1: 4 formatos |
| skill-retroalimentacion | 4.5KB | 1 | Retroalimentación científica ICFES (OBLIGATORIO) |

**NO modificar skills** sin ejecutar tests de regresion.

---

## 📚 Documentación Relacionada

- **Reglas críticas**: @.claude/docs/REGLAS_CRITICAS.md
- **Flujo B obligatorio**: @.claude/rules/flujo-b-obligatorio.md
- **Proceso secuencial**: @.claude/rules/graficador-secuencial.md
- **Ciclo validación**: @.claude/rules/ciclo-validacion.md
- **Workflow completo**: @.claude/docs/WORKFLOW_PASO_A_PASO.md

---

**Version**: 1.1
**Fecha**: 2026-02-06
**Modulo de**: @.claude/CLAUDE.md (v3.0.0)

### Cambios v1.1 (2026-02-06)

- Documentacion de estructura Progressive Disclosure para skills
- Tabla completa de 17 skills refactorizados (v2.1)
- Frontmatter YAML estandarizado para todos los skills
