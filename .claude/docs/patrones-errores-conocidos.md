# Patrones de Errores Conocidos y Soluciones - R/exams

> **Nota:** Este documento solo registra errores que ya han sido identificados, corregidos y verificados. No se documentan problemas sin solución confirmada.

---

## Índice

1. [Error: Imagen PNG no encontrada en compilación PDF](#error-1-imagen-png-no-encontrada)
2. [Error: Argumento no numérico para función matemática abs()](#error-2-argumento-no-numerico-abs)
3. [Error: Imágenes Python/matplotlib no visibles en exams2pdf](#error-3-imagenes-python-no-visibles-pdf)
4. [Error: Gráficos como opciones mostrados en grid](#error-4-gráficos-como-opciones-mostrados-en-grid-no-individuales)
5. [Error: Gráfico aplastado por escala incompatible](#error-5-gráfico-aplastado-por-escala-incompatible-est-box-01)
6. [Error: Rango insuficiente para sample()](#error-6-rango-insuficiente-para-sample-sin-reemplazo)
7. [Error: Descripción de error conceptual incoherente con paridad de datos](#error-7-descripción-de-error-conceptual-incoherente-con-paridad-de-datos)
8. [Error: Corrupción de RNG por test de diversidad](#error-8-corrupción-de-rng-por-test-de-diversidad)
9. [Error: ##ANSWERi## mal ubicados en ejercicio CLOZE](#error-9-answeri-mal-ubicados-en-ejercicio-cloze)
10. [Error: NA en comparación while con calcula()](#error-10-na-en-comparación-while-con-calcula)

### Categoría: Infraestructura `.claude/` (sesión Ruflo 2026-05-03)
11. [Error: Drift silencioso de hooks tras instalación de plataforma externa](#error-11-drift-silencioso-de-hooks-tras-instalación-de-plataforma-externa)
12. [Error: `CLAUDE.md` raíz sobrescrito por plantilla genérica de plataforma](#error-12-claudemd-raíz-sobrescrito-por-plantilla-genérica-de-plataforma)
13. [Error: MCP registrado pero sin conectar (paquete fantasma)](#error-13-mcp-registrado-pero-sin-conectar-paquete-fantasma)
14. [Error: CLI claude-flow falla con `npm error Invalid Version`](#error-14-cli-claude-flow-falla-con-npm-error-invalid-version)
15. [Error: Auto-memory bridge sin paquete instalado (`Memory package not available`)](#error-15-auto-memory-bridge-sin-paquete-instalado)

### Categoría: Pipeline render PDF + coherencia Solution (sesiones 2026-05-03/14)
16. [Error: `\pandocbounded` undefined en PDF](#error-16-pandocbounded-undefined-al-renderizar-pdf-con-imágenes-markdown-sin-atributo-width)
17. [Error: Inconsistencia Solution↔Answerlist por exshuffle](#error-17-inconsistencia-solutionanswerlist-por-exshuffle-true-con-referencia-explícita-a-letra)
18. [Error: Estudiante identifica opción correcta por formato gráfico sin verificar datos](#error-18-estudiante-puede-identificar-opción-correcta-por-formato-gráfico-sin-verificar-datos)
19. [Error: Solution con letra_correcta rompe coherencia bajo Moodle](#error-19-solution-con-r-letra_correcta-rompe-coherencia-bajo-moodle-re-shuffle)
20. [Error: GRAF-BAR-01 — Barras con categorías correctas pero alturas permutadas](#error-20-graf-bar-01--gráfico-de-barras-con-categorías-correctas-pero-alturas-permutadas)

---

## Error 1: Imagen PNG no encontrada en compilación PDF

> ⚠️ **OBSOLETO (parcial) — 2026-08-15**: la solución "renderizado condicional" con
> `knitr::is_latex_output()` documentada en la sección "Código DESPUÉS (correcto)" de este error
> quedó **RETIRADA**. Medido con fixtures renderizados: `is_latex_output()` es SIEMPRE FALSE bajo
> R/exams (los 5 pipelines tejen a Markdown y delegan en pandoc), así que la rama LaTeX nunca se
> ejecuta y la figura desaparece en el PDF sin error ni warning. El código de esta entrada se
> conserva como referencia histórica del incidente original (2025-12-19); el enfoque VIGENTE es
> `include_tikz(..., markup = "markdown")` en una sola llamada, sin condicional. Ver
> `.claude/rules/codigo-rmd.md` regla #1 y `.claude/rules/markdown-imagenes-pdf.md` Patrón B'.

### ❌ Mensaje de Error
```
Package pdftex.def Error: File `nombre_archivo.png' not found: using draft setting.
Error: LaTeX failed to compile archivo.tex.
See https://yihui.org/tinytex/r/#debugging for debugging tips.
```

### 🔍 Causa Raíz
El uso de `include_tikz()` dentro de chunks de generación de datos crea archivos PNG/PDF en directorios temporales que no son accesibles cuando `exams2pdf()` ejecuta la compilación LaTeX final.

**Flujo del problema:**

1. `include_tikz()` genera `imagen.png` en `/tmp/RtmpXXXX/...`
2. El chunk retorna referencia markdown: `![](imagen.png)`
3. Durante `exams2pdf()`, LaTeX busca `imagen.png` en el directorio de trabajo actual
4. El archivo no existe en esa ruta → Error de compilación

### ✅ Solución Verificada

**Enfoque:** Renderizado condicional según formato de salida

#### Código ANTES (incorrecto):

```r
```{r generar_diagrama_cilindro, echo=FALSE, results="hide"}
options(OutDec = ".")

generar_tikz_cilindro <- function(r, h) {
  tikz_code <- paste0(
    "\\begin{tikzpicture}[scale=0.6]\n",
    # ... código TikZ ...
    "\\end{tikzpicture}"
  )
  return(tikz_code)
}

tikz_cilindro <- generar_tikz_cilindro(radio, altura)

# ❌ PROBLEMA: include_tikz en chunk de generación
include_tikz(tikz_cilindro,
             name = "cilindro_vaso",
             markup = "markdown",
             format = typ,
             packages = c("tikz", "xcolor", "amsmath"),
             width = "8cm")
```

Uso en Question:
```markdown
![](cilindro_vaso.png){width=50%}
```

#### Código DESPUÉS (histórico — el condicional `is_latex_output()` de este bloque quedó RETIRADO el 2026-08-15, ver nota al inicio de este Error 1; sustituto vigente después del bloque):

```r
```{r generar_codigo_tikz, echo=FALSE, results="hide"}
# ✅ Solo generar el código TikZ, NO renderizarlo
options(OutDec = ".")

generar_tikz_cilindro <- function(r, h) {
  tikz_code <- paste0(
    "\\begin{tikzpicture}[scale=0.6]\n",
    "\\def\\radio{", r * 0.4, "}\n",
    "\\def\\altura{", h * 0.4, "}\n",
    "\\draw[thick, brown!70!black] (0, \\altura) ellipse (\\radio cm and 0.4 cm);\n",
    "\\draw[thick, brown!70!black, dashed] (0, 0) ellipse (\\radio cm and 0.4 cm);\n",
    "\\draw[thick, brown!70!black] (-\\radio, 0) -- (-\\radio, \\altura);\n",
    "\\draw[thick, brown!70!black] (\\radio, 0) -- (\\radio, \\altura);\n",
    "\\draw[<->, thick, red] (0, \\altura) -- (\\radio, \\altura) ",
    "node[midway, above] {\\textbf{", r, " cm}};\n",
    "\\draw[<->, thick, blue] (\\radio + 0.8, 0) -- (\\radio + 0.8, \\altura) ",
    "node[midway, right] {\\textbf{", h, " cm}};\n",
    "\\end{tikzpicture}"
  )
  return(tikz_code)
}

tikz_cilindro <- generar_tikz_cilindro(radio, altura)
# NO llamar a include_tikz aquí
```

Uso en Question (con renderizado condicional):
```r
```{r mostrar_cilindro, echo=FALSE, results='asis', fig.align='center'}
# ✅ Renderizado condicional según formato
es_latex <- knitr::is_latex_output()

if (es_latex) {
  # Para PDF/LaTeX: insertar código TikZ directamente
  cat("\\begin{center}\n")
  cat(tikz_cilindro)
  cat("\n\\end{center}\n\n")
} else {
  # Para HTML: usar include_tikz
  include_tikz(tikz_cilindro,
               name = "cilindro_vaso",
               markup = "markdown",
               format = typ,
               packages = c("tikz", "xcolor", "amsmath"),
               width = "8cm")
  cat("\n\n")
}
```

#### Código VIGENTE (2026-08-15) — sustituto sin condicional

```r
```{r mostrar_cilindro, echo=FALSE, results='asis', fig.align='center'}
# ✅ Una sola llamada, sin ramificar por is_latex_output(): markup="markdown" enruta
# correctamente a los 5 destinos (html/pdf/docx/nops/moodle)
include_tikz(tikz_cilindro,
             name = "cilindro_vaso",
             markup = "markdown",
             format = typ,
             packages = c("tikz", "xcolor", "amsmath"),
             width = "8cm")
```

### 🧪 Validación de la Solución

La validación debe hacerse en **TRES NIVELES** según el flujo de trabajo real:

#### **Nivel 1: RStudio (Run > Run all)**
Ejecutar todos los chunks interactivamente en RStudio.

**Criterio de éxito:**

- ✅ Todos los chunks ejecutan sin errores
- ✅ El output configurado en YAML se genera correctamente
- ✅ Los gráficos TikZ se visualizan

**Método:**
```

1. Abrir .Rmd en RStudio
2. Run > Run All
3. Verificar output (HTML/PDF/Word según YAML)
```

#### **Nivel 2: Generación Masiva (SemilleroUnico_v2.R)**
Ejecutar el script de generación completa desde la misma carpeta del .Rmd.

**Criterios de éxito:**

- ✅ `exams2html()` compila sin errores
- ✅ `exams2pdf()` compila sin errores
- ✅ `exams2pandoc()` genera DOCX sin errores
- ✅ `exams2nops()` genera exámenes escaneables sin errores
- ✅ Diagramas TikZ correctos en TODOS los formatos

**Método:**
```bash
# Desde la carpeta del .Rmd
Rscript SemilleroUnico_v2.R
```

**O usar script de prueba completa:**
```bash
Rscript test_todos_formatos.R
```

**Resultado esperado:**
```
Formato    | Resultado
-----------|----------
HTML       | ✅ EXITOSO
PDF        | ✅ EXITOSO
DOCX       | ✅ EXITOSO
NOPS       | ✅ EXITOSO

Tasa de éxito: 4 de 4 formatos (100%)
```

#### **Nivel 3: Terreno (Estudiantes)**
Validación en el aula con estudiantes reales.

**Criterios de validación:**

- ✅ Enunciado claro y sin ambigüedades
- ✅ Solución matemática correcta
- ✅ Distractores plausibles pero incorrectos
- ✅ Contexto apropiado para el nivel
- ✅ Tiempos de resolución razonables
- ✅ Sin errores de cálculo o tipográficos

**Importante:**
> Esta validación detecta errores que no son visibles en pruebas técnicas: ambigüedades en el lenguaje, errores matemáticos sutiles, contextos confusos, etc.

### 📋 Checklist de Corrección (actualizado 2026-08-15)

- [ ] Identificar chunks que usan `include_tikz()`
- [ ] Mover `include_tikz()` fuera del chunk de generación (dejar solo la construcción del código)
- [ ] Crear UN chunk `results='asis'` con `include_tikz(tikz_code, ..., markup = "markdown")` —
      **NO** ramificar con `knitr::is_latex_output()` (RETIRADO: SIEMPRE FALSE bajo R/exams,
      pierde la figura en el PDF, ver `.claude/rules/codigo-rmd.md` regla #1)
- [ ] Verificar compilación a PDF
- [ ] Verificar compilación a HTML
- [ ] Confirmar visualización correcta en ambos formatos (y en Moodle/DOCX si aplica)

### 🎯 Casos Aplicables

Este patrón de solución aplica para:

- ✅ Diagramas geométricos con TikZ (cilindros, prismas, polígonos)
- ✅ Gráficos estadísticos generados con TikZ
- ✅ Diagramas de árbol de probabilidad
- ✅ Cualquier visualización generada con código TikZ en R/exams

### ⚠️ Casos NO Aplicables

Este patrón NO aplica para:

- ❌ Imágenes externas (PNG/JPG ya existentes)
- ❌ Gráficos generados con ggplot2 o base R
- ❌ Diagramas generados con Python/matplotlib

### 🔗 Archivos de Referencia

**Ejemplo corregido verificado:**

- `/A-Produccion/En-Desarrollo/volumen_cilindro_geometrico_metrico_interpretacion_n2_v1.Rmd`

**Skill asociado:**

- `.claude/skills/corregir-error-imagen/skill.md`

### 📅 Historial

| Fecha | Versión | Estado | Validado en | Niveles Validados |
|-------|---------|--------|-------------|-------------------|
| 2025-12-19 22:36 | v1.1 | ✅ Verificado | volumen_cilindro_geometrico_metrico_interpretacion_n2_v1.Rmd | Nivel 2 ✅ (Validación completa) |
| 2025-12-19 | v1.0 | ✅ Verificado | volumen_cilindro_geometrico_metrico_interpretacion_n2_v1.Rmd | Niveles 1 y 2 ✅ |

**Pruebas de validación realizadas (v1.1 - 2025-12-19 22:36):**

**Nivel 1 - RStudio (Run > Run all):**

- ⏭️ Pendiente de validación por usuario

**Nivel 2 - Generación Masiva (validar_sin_gui.R):**

- ✅ exams2html: Exitoso
  - HTML generado sin errores
  - include_tikz() funcionó correctamente para formato HTML
- ✅ exams2pdf: Exitoso (template solpcielo, pdfTeX-1.40.28)
  - Archivo: volumen_cilindro_geometrico_metrico_interpretacion_n2_v1_1.pdf
  - Tamaño: 94K (95,542 bytes)
  - Páginas: 4
  - Código TikZ insertado directamente (no usa archivos PNG externos)
  - Sin errores de "File not found"
- ✅ exams2pandoc: Exitoso (DOCX con pcielo.tex)
  - Archivo: volumen_cilindro_geometrico_metrico_interpretacion_n2_v1_1.docx
  - Tamaño: 23K
  - Imágenes embebidas detectadas:
    - word/media/rId23.png (3.1 KB)
    - word/media/rId32.png (2.4 KB)
    - word/media/rId20.svg (6.6 KB)
    - word/media/rId29.svg (7.6 KB)
- ✅ exams2nops: Exitoso (exámenes escaneables)
  - Archivo: volumen_cilindro_geometrico_metrico_interpretacion_n2_v1_nops_1.pdf
  - Tamaño: 81K
  - Formato escaneable generado correctamente
- ✅ Tasa de éxito: 4 de 4 formatos (100%)
- ✅ Diagrama TikZ del cilindro renderizado correctamente en TODOS los formatos
- ✅ Renderizado condicional funcionando perfectamente:
  - PDF: Código TikZ insertado directamente con cat()
  - HTML: include_tikz() genera PNG en directorio temporal
  - DOCX: Imágenes PNG/SVG embebidas en el archivo
- ✅ Sin errores de "File not found" en ningún formato
- ✅ Solución confirmada y reproducible
- ⚠️ Advertencias menores: Labels LaTeX duplicados (no afectan funcionalidad)

**Nivel 3 - Terreno (Estudiantes):**

- ⏭️ Pendiente de validación en aula

---

## Error 2: Argumento no numérico para función matemática abs()

### ❌ Mensaje de Error
```
Error in `abs(b_formateado)`: Argumento no numérico para una función matemática
Backtrace:

 1. └─global generar_datos()
 2. └─base::paste0("y = ", m_formateado, "x - ", abs(b_formateado))
Error: ! Test failed
```

### 🔍 Causa Raíz
Aplicar funciones matemáticas (como `abs()`, `round()`, `floor()`, etc.) sobre variables que ya han sido formateadas como strings. Las variables formateadas son de tipo `character`, no `numeric`, por lo que no pueden usarse en operaciones matemáticas.

**Flujo del problema:**

1. Se genera un valor numérico: `b <- -2.5`
2. Se formatea como string: `b_formateado <- ifelse(b == as.integer(b), as.character(b), sprintf("%.1f", b))` → `"-2.5"` (string)
3. Se intenta aplicar `abs()` sobre el string: `abs(b_formateado)` → ❌ Error
4. La función `abs()` requiere un argumento numérico, no un string

**Patrón común:**
Este error ocurre frecuentemente cuando se necesita:

- Aplicar valor absoluto a un número negativo para mostrarlo en una ecuación
- Formatear el resultado después de aplicar la función matemática
- Usar el valor formateado en múltiples lugares

### ✅ Solución Verificada

**Enfoque:** Aplicar la función matemática sobre el valor numérico original, luego formatear el resultado.

#### Código ANTES (incorrecto):

```r
# Generar valor numérico
b <- -2.5

# Formatear como string
b_formateado <- ifelse(b == as.integer(b), 
                       as.character(b), 
                       sprintf("%.1f", b))
# b_formateado = "-2.5" (string)

# ❌ ERROR: Intentar aplicar abs() sobre string
if (b < 0) {
  ecuacion <- paste0("y = ", m_formateado, "x - ", abs(b_formateado))
  # Error: abs() no puede trabajar con strings
}
```

#### Código DESPUÉS (correcto):

```r
# Generar valor numérico
b <- -2.5

# Formatear valor original (para casos donde b >= 0)
b_formateado <- ifelse(b == as.integer(b), 
                       as.character(b), 
                       sprintf("%.1f", b))

# Para casos donde b < 0, aplicar abs() sobre el número, luego formatear
if (b < 0) {
  # ✅ Aplicar abs() sobre el valor numérico
  b_abs <- abs(b)  # b_abs = 2.5 (numérico)
  
  # ✅ Formatear el resultado
  b_abs_formateado <- ifelse(b_abs == as.integer(b_abs), 
                             as.character(b_abs), 
                             sprintf("%.1f", b_abs))
  # b_abs_formateado = "2.5" (string)
  
  ecuacion <- paste0("y = ", m_formateado, "x - ", b_abs_formateado)
}
```

**Patrón generalizado:**

```r
# ❌ INCORRECTO: Aplicar función matemática sobre string formateado
resultado <- funcion_matematica(variable_formateada)

# ✅ CORRECTO: Aplicar función matemática sobre número, luego formatear
valor_original <- obtener_valor_numerico()
resultado_numerico <- funcion_matematica(valor_original)
resultado_formateado <- formatear(resultado_numerico)
```

### 🧪 Validación de la Solución

#### **Nivel 1: RStudio (Run > Run all)**
Ejecutar todos los chunks interactivamente en RStudio.

**Criterio de éxito:**

- ✅ Todos los chunks ejecutan sin errores
- ✅ Las funciones matemáticas se aplican correctamente
- ✅ Los valores formateados se muestran correctamente en las ecuaciones

**Método:**
```

1. Abrir .Rmd en RStudio
2. Run > Run All
3. Verificar que no hay errores en chunks de generación
4. Verificar que las ecuaciones se muestran correctamente
```

#### **Nivel 2: Prueba de Diversidad**
Ejecutar la prueba de diversidad de versiones.

**Criterios de éxito:**

- ✅ `test_that("Prueba de diversidad de versiones", ...)` pasa sin errores
- ✅ Se generan al menos 250 versiones únicas
- ✅ Todas las versiones generan ecuaciones válidas

**Método:**
```r
# Dentro del archivo .Rmd, ejecutar el chunk de prueba
test_that("Prueba de diversidad de versiones", {
  versiones <- list()
  for(i in 1:1000) {
    datos_test <- generar_datos()
    versiones[[i]] <- digest::digest(datos_test)
  }
  n_versiones_unicas <- length(unique(versiones))
  expect_true(n_versiones_unicas >= 250)
})
```

**Resultado esperado:**
```
Test passed
✓ Prueba de diversidad de versiones
```

#### **Nivel 3: Generación Masiva**
Ejecutar el script de generación completa.

**Criterios de éxito:**

- ✅ `exams2html()` compila sin errores
- ✅ `exams2pdf()` compila sin errores
- ✅ Las ecuaciones se muestran correctamente en todos los formatos
- ✅ No hay errores de tipo en las funciones matemáticas

### 📋 Checklist de Corrección

- [ ] Identificar todas las ocurrencias de funciones matemáticas sobre variables formateadas
- [ ] Buscar patrones como: `abs(variable_formateada)`, `round(variable_formateada)`, etc.
- [ ] Para cada ocurrencia:
  - [ ] Identificar la variable numérica original
  - [ ] Aplicar la función matemática sobre el valor numérico
  - [ ] Formatear el resultado después de aplicar la función
  - [ ] Usar el valor formateado en la construcción de strings
- [ ] Verificar que todas las ecuaciones se generan correctamente
- [ ] Ejecutar prueba de diversidad
- [ ] Validar compilación en todos los formatos

### 🎯 Casos Aplicables

Este patrón de solución aplica para:

- ✅ Aplicar `abs()` sobre valores negativos antes de formatear
- ✅ Aplicar `round()`, `floor()`, `ceiling()` sobre valores antes de formatear
- ✅ Cualquier función matemática que requiera argumentos numéricos
- ✅ Construcción de ecuaciones matemáticas con valores formateados
- ✅ Generación de opciones de respuesta con valores absolutos

### ⚠️ Funciones Matemáticas Comunes que Causan Este Error

| Función | Ejemplo Incorrecto | Ejemplo Correcto |
|---------|-------------------|------------------|
| `abs()` | `abs(b_formateado)` | `abs(b)` luego formatear |
| `round()` | `round(x_formateado)` | `round(x)` luego formatear |
| `floor()` | `floor(x_formateado)` | `floor(x)` luego formatear |
| `ceiling()` | `ceiling(x_formateado)` | `ceiling(x)` luego formatear |
| `sqrt()` | `sqrt(x_formateado)` | `sqrt(x)` luego formatear |
| `log()` | `log(x_formateado)` | `log(x)` luego formatear |

### 🔗 Archivos de Referencia

**Ejemplo corregido verificado:**

- `/A-Produccion/En-Desarrollo/recta_geometria_analitica_interpretacion_representacion/recta_geometria_analitica_interpretacion_representacion_n2_v1.Rmd`
- **Caso resuelto:** `.claude/docs/casos-resueltos/2025-12-21-recta-abs-formateado.md`

**Líneas corregidas:**

- Línea 160: `abs(b_formateado)` → `abs(b)` luego formatear
- Línea 177: `abs(b_dist1_formateado)` → `abs(b_distractor1)` luego formatear
- Línea 196: `abs(b_formateado)` → `abs(b)` luego formatear
- Línea 219: `abs(b_dist3_formateado)` → `abs(b_distractor3)` luego formatear

### 📅 Historial

| Fecha | Versión | Estado | Validado en | Niveles Validados |
|-------|---------|--------|-------------|-------------------|
| 2025-12-21 | v1.0 | ✅ Verificado | recta_geometria_analitica_interpretacion_representacion_n2_v1.Rmd | Nivel 1 ✅ (RStudio) |

**Pruebas de validación realizadas (v1.0 - 2025-12-21):**

**Nivel 1 - RStudio (Run > Run all):**

- ✅ Todos los chunks ejecutan sin errores
- ✅ Las ecuaciones se generan correctamente
- ✅ No hay errores de tipo en funciones matemáticas
- ✅ Función probada directamente: 10 ejecuciones exitosas

**Nivel 2 - Prueba de Diversidad:**

- ✅ Código corregido y verificado
- ⚠️ **Nota importante**: Si el error persiste, puede ser debido a caché de R/knitr
  - Solución: Reiniciar sesión de R o limpiar caché con `rm(list = ls())` y `knitr::knit_cache$clean()`

**Nivel 3 - Generación Masiva:**

- ⏭️ Pendiente de validación completa

### ⚠️ Nota sobre Caché de R/knitr

Si el error persiste después de corregir el código, puede ser debido a:

1. **Caché de knitr**: Los chunks pueden estar usando versiones en caché
   - **Solución**: Limpiar caché con `knitr::knit_cache$clean()` o eliminar carpeta `*_cache/`
2. **Entorno de R**: Variables en memoria de sesiones anteriores
   - **Solución**: Reiniciar sesión de R o ejecutar `rm(list = ls())`
3. **Archivo no guardado**: Verificar que los cambios se guardaron correctamente
   - **Solución**: Verificar timestamp del archivo y contenido con `grep -n "abs(b_formateado)" archivo.Rmd`

---

## Error 3: Imágenes Python/matplotlib no visibles en exams2pdf

### ❌ Mensaje de Error
```
# No hay mensaje de error explícito, pero la imagen no aparece en el PDF generado
# El PDF se compila correctamente pero la gráfica está ausente
```

**Síntoma:**

- El PDF se genera sin errores de compilación
- La imagen generada por Python existe en el directorio
- La imagen NO se visualiza en el PDF final
- El texto del ejercicio aparece correctamente

### 🔍 Causa Raíz
El uso de `knitr::include_graphics()` para mostrar imágenes generadas por Python/matplotlib no funciona correctamente con `exams2pdf()` debido a problemas de rutas y contexto de compilación.

**Flujo del problema:**

1. Python genera la imagen: `plt.savefig('recta_python.png')` → se guarda en directorio actual
2. Se intenta mostrar con: `knitr::include_graphics("recta_python.png")`
3. Durante `exams2pdf()`, knitr busca la imagen en rutas relativas/absolutas que no coinciden
4. La imagen no se encuentra en el contexto de compilación → No aparece en PDF

**Patrón común:**
Este error ocurre cuando:

- Se generan gráficos con Python/matplotlib usando `py_run_string()`
- Se guarda la imagen con `plt.savefig('nombre.png')`
- Se intenta incluir con `knitr::include_graphics()` en un chunk R

### ✅ Solución Verificada

**Enfoque:** Usar sintaxis markdown simple con `cat()` en lugar de `knitr::include_graphics()`, siguiendo el patrón de archivos funcionales en producción.

#### Código ANTES (incorrecto):

```r
```{r generar_grafico_python, echo=FALSE, results="hide"}
codigo_python <- paste0("
import matplotlib.pyplot as plt
# ... código de generación ...
plt.savefig('recta_python.png', dpi=150, bbox_inches='tight')
plt.close()
")
py_run_string(codigo_python)
```

```{r mostrar_grafico, echo=FALSE, fig.align='center', out.width='50%'}
# ❌ PROBLEMA: knitr::include_graphics() no funciona con exams2pdf
if (file.exists("recta_python.png")) {
  knitr::include_graphics("recta_python.png")
} else {
  warning("El archivo no se encontró")
}
```
```

#### Código DESPUÉS (correcto):

```r
```{r generar_grafico_python, echo=FALSE, results="hide"}
# ✅ Guardar imagen en directorio actual (patrón de archivos funcionales)
codigo_python <- paste0("
import matplotlib
matplotlib.use('Agg')  # Backend sin interfaz gráfica
import matplotlib.pyplot as plt
import numpy as np

# Obtener parámetros desde R
m_py = ", datos$m, "
b_py = ", datos$b, "

# ... código de generación del gráfico ...

# Guardar en el directorio actual (compatible con todos los formatos)
plt.savefig('recta_python.png', dpi=150, bbox_inches='tight', transparent=True)
plt.close()
")

# Ejecutar el código Python
py_run_string(codigo_python)
```

```{r mostrar_grafico, echo=FALSE, results='asis', fig.align='center'}
# ✅ SOLUCIÓN: Usar markdown simple con cat() (patrón de archivos funcionales)
# Detectar si se está generando para Moodle
es_moodle <- (match_exams_call() %in% c("exams2moodle", "exams2qti12", "exams2qti21", "exams2openolat"))

# Ajustar tamaño según formato de salida
if(es_moodle) {
  cat("![](recta_python.png){width=30%}\n\n")  # Más pequeño para Moodle
} else {
  cat("![](recta_python.png){width=50%}\n\n")  # Tamaño estándar para PDF/Word
}
```
```

**Diferencias clave:**

1. ✅ **Guardar imagen simple**: `plt.savefig('recta_python.png')` sin rutas absolutas
2. ✅ **Chunk de visualización**: `results='asis'` (no `fig.align` ni `out.width`)
3. ✅ **Sintaxis markdown**: `cat("![](recta_python.png){width=50%}\n\n")` en lugar de `knitr::include_graphics()`
4. ✅ **Renderizado condicional**: Ajustar tamaño según formato (Moodle vs PDF/Word)

### 🧪 Validación de la Solución

#### **Nivel 1: RStudio (Run > Run all)**
Ejecutar todos los chunks interactivamente en RStudio.

**Criterio de éxito:**

- ✅ Todos los chunks ejecutan sin errores
- ✅ La imagen se genera correctamente
- ✅ La imagen se visualiza en el output (HTML/PDF/Word según YAML)

**Método:**
```

1. Abrir .Rmd en RStudio
2. Run > Run All
3. Verificar que la imagen aparece en el output
```

#### **Nivel 2: Generación Masiva (exams2pdf)**
Ejecutar `exams2pdf()` para verificar que la imagen aparece en el PDF.

**Criterios de éxito:**

- ✅ `exams2pdf()` compila sin errores
- ✅ El PDF contiene la imagen (verificable con `pdfimages -list archivo.pdf`)
- ✅ La imagen se visualiza correctamente en el PDF

**Método:**
```r
library(exams)
set.seed(123)
exams2pdf('archivo.Rmd', n=1, dir='test_pdf', template='plain')
```

**Verificación:**
```bash
# Verificar que el PDF contiene imágenes
pdfimages -list test_pdf/plain1.pdf

# Resultado esperado:
# page   num  type   width height color comp bpc  enc interp  object ID
#    1     0 image     487   734  rgb     3   8  image  no         1  0
```

#### **Nivel 3: Todos los Formatos**
Validar que funciona en todos los formatos de salida.

**Criterios de éxito:**

- ✅ `exams2html()` muestra la imagen correctamente
- ✅ `exams2pdf()` muestra la imagen correctamente
- ✅ `exams2pandoc()` (DOCX) muestra la imagen correctamente
- ✅ `exams2moodle()` muestra la imagen correctamente

### 📋 Checklist de Corrección

- [ ] Identificar chunks que generan imágenes con Python/matplotlib
- [ ] Verificar que `plt.savefig()` guarda en directorio actual (sin rutas absolutas)
- [ ] Reemplazar `knitr::include_graphics()` por sintaxis markdown con `cat()`
- [ ] Cambiar chunk de visualización a `results='asis'`
- [ ] Agregar renderizado condicional para diferentes formatos (Moodle vs PDF/Word)
- [ ] Verificar compilación a PDF con `exams2pdf()`
- [ ] Verificar que la imagen aparece en el PDF (usar `pdfimages -list`)
- [ ] Confirmar visualización correcta en todos los formatos

### 🎯 Casos Aplicables

Este patrón de solución aplica para:

- ✅ Gráficos generados con Python/matplotlib (`py_run_string()`)
- ✅ Imágenes guardadas con `plt.savefig()`
- ✅ Cualquier visualización generada con Python en R/exams
- ✅ Gráficos de rectas, funciones, diagramas, etc. generados con matplotlib

### ⚠️ Casos NO Aplicables

Este patrón NO aplica para:

- ❌ Imágenes TikZ (usar solución del Error 1)
- ❌ Imágenes externas ya existentes (PNG/JPG)
- ❌ Gráficos generados con ggplot2 o base R (usar sistema de figuras de knitr)

### 🔗 Archivos de Referencia

**Ejemplos funcionales verificados en producción:**

- `/A-Produccion/En-Produccion/06-Estadística-Y-Probabilidad/.../accidentalidad-vial-genero-01.Rmd`
- `/A-Produccion/En-Desarrollo/volumen_cilindro_geometrico_metrico_interpretacion/volumen_cilindro_geometrico_metrico_interpretacion_python_n2_v1.Rmd`

**Ejemplo corregido verificado:**

- `/A-Produccion/En-Desarrollo/recta_geometria_analitica_interpretacion_representacion_python/recta_geometria_analitica_python_interpretacion_representacion_n2_v1.Rmd`

**Patrón de referencia:**
Los archivos funcionales en producción usan consistentemente:
```r
cat("![](nombre_imagen.png){width=50%}\n\n")
```
en lugar de `knitr::include_graphics()`.

### 📅 Historial

| Fecha | Versión | Estado | Validado en | Niveles Validados |
|-------|---------|--------|-------------|-------------------|
| 2025-12-21 | v1.0 | ✅ Verificado | recta_geometria_analitica_python_interpretacion_representacion_n2_v1.Rmd | Nivel 2 ✅ (exams2pdf) |

**Pruebas de validación realizadas (v1.0 - 2025-12-21):**

**Nivel 1 - RStudio (Run > Run all):**

- ⏭️ Pendiente de validación por usuario

**Nivel 2 - Generación Masiva (exams2pdf):**

- ✅ `exams2pdf()`: Exitoso
  - PDF generado: 85KB
  - Imágenes incluidas: 2 objetos de imagen detectados con `pdfimages -list`
  - Tamaño de imagen: 487x734 píxeles, RGB
  - Sin errores de compilación
  - Imagen visible en el PDF
- ✅ Solución confirmada y reproducible
- ✅ Patrón basado en archivos funcionales en producción

**Nivel 3 - Todos los Formatos:**

- ⏭️ Pendiente de validación completa

### 💡 Notas Importantes

1. **Patrón de archivos funcionales**: Esta solución se basa en el análisis de archivos `.Rmd` funcionales en producción que usan Python/matplotlib. Todos usan el patrón `cat("![](imagen.png)")` en lugar de `knitr::include_graphics()`.

2. **Compatibilidad con exams2pdf**: El problema específico es con `exams2pdf()`. Para otros formatos (HTML, Word), ambos métodos pueden funcionar, pero el patrón markdown simple es más consistente.

3. **Renderizado condicional**: Es recomendable ajustar el tamaño de la imagen según el formato de salida (más pequeño para Moodle, estándar para PDF/Word).

---

## Error 4: Gráficos como opciones mostrados en grid (no individuales)

### ❌ Síntoma del Error

```
- Las 4 opciones de gráficos se muestran juntas en un solo grid
- El Answerlist tiene texto ("Opción A", "Opción B"...) en lugar de imágenes
- exshuffle no puede mezclar las opciones correctamente
- El estudiante no puede seleccionar una opción específica
```

**Ejemplo visual del error:**

```
┌─────────────────┐  ← Un solo bloque con 4 gráficos
│  [A]   [B]      │
│  [C]   [D]      │
└─────────────────┘

Answerlist:
* Opción A        ← Texto en lugar de imagen
* Opción B
* Opción C
* Opción D
```

### 🔍 Causa Raíz

Uso de `grid.arrange()` o similar para mostrar todos los gráficos en una sola imagen, con el Answerlist conteniendo solo texto descriptivo en lugar de las imágenes individuales.

**Código incorrecto:**

```r
# ❌ PROBLEMA: grid.arrange muestra todo junto
library(gridExtra)
grid.arrange(plot_A, plot_B, plot_C, plot_D, ncol = 2)

# ❌ PROBLEMA: Answerlist con texto
Answerlist
----------
* Opción A
* Opción B
* Opción C
* Opción D
```

### ✅ Solución Verificada

**Enfoque:** Guardar cada gráfico como PNG separado y referenciarlos en el Answerlist.

#### Código DESPUÉS (correcto):

```r
# ✅ CORRECTO: Función que guarda cada gráfico individualmente
crear_y_guardar_boxplot <- function(cuartiles, letra, y_min, y_max, unidad) {
  p <- ggplot(...) + ...

  # OBLIGATORIO: Guardar como archivo PNG individual
  nombre_archivo <- paste0("diagrama_", tolower(letra), ".png")
  ggsave(nombre_archivo, plot = p, width = 4, height = 5, dpi = 150, bg = "white")

  return(p)
}

# Crear los 4 gráficos como archivos separados
plot_A <- crear_y_guardar_boxplot(opciones$A, "A", y_min, y_max, unidad)
plot_B <- crear_y_guardar_boxplot(opciones$B, "B", y_min, y_max, unidad)
plot_C <- crear_y_guardar_boxplot(opciones$C, "C", y_min, y_max, unidad)
plot_D <- crear_y_guardar_boxplot(opciones$D, "D", y_min, y_max, unidad)
```

#### Answerlist (correcto):

```markdown
Answerlist
----------

` ``{r mostrar_opciones, echo=FALSE, results='asis'}
cat("* ![](diagrama_a.png){width=60%}\n")
cat("* ![](diagrama_b.png){width=60%}\n")
cat("* ![](diagrama_c.png){width=60%}\n")
cat("* ![](diagrama_d.png){width=60%}\n")
` ``
```

### 🧪 Validación de la Solución

```r
# Verificar que se generaron los 4 PNG
list.files(pattern = "diagrama_[a-d]\\.png$")
# Debe retornar: "diagrama_a.png" "diagrama_b.png" "diagrama_c.png" "diagrama_d.png"

# Renderizar y verificar visualmente
exams2pdf("archivo.Rmd", n = 1)
# Cada opción (a), (b), (c), (d) debe mostrar su propio gráfico
```

### 📋 Checklist de Corrección

- [ ] Crear función que use `ggsave()` para guardar cada gráfico
- [ ] Nombre de archivo: `diagrama_[letra].png` (minúsculas)
- [ ] Llamar la función para cada opción (A, B, C, D)
- [ ] Answerlist usa `cat("* ![](diagrama_x.png){width=60%}\n")`
- [ ] Verificar que `exshuffle: FALSE` está configurado (SCHOICE con PNGs usa mezcla interna con `sample()`, ver `graficos-como-opciones.md`)

### 🔗 Archivos de Referencia

**Ejemplo funcional:**

- `A-Produccion/Ejemplos-Funcionales-Rmd/estadistica_diagramas_caja_interpretacion_representacion_Nivel2_v2.Rmd`

**Ejemplo corregido:**

- `A-Produccion/01-En-PreDesarrollo/Lab-Manjaro/50/diagrama_caja_estaturas_metacognitivo_interpretacion_n2_schoice_v1.Rmd`

**Regla asociada:**

- `.claude/rules/graficos-como-opciones.md`

### 📅 Historial

| Fecha | Versión | Estado | Validado en |
|-------|---------|--------|-------------|
| 2026-02-07 | v1.0 | ✅ Verificado | diagrama_caja_estaturas_metacognitivo_interpretacion_n2_schoice_v1.Rmd |

---

## Error 5: Gráfico aplastado por escala incompatible (EST-BOX-01)

### ❌ Síntoma del Error

```
- Uno de los gráficos de opciones aparece casi invisible
- El diagrama de caja está "aplastado" contra el fondo de la escala
- El eje Y tiene un rango muy amplio (ej: 1 a 251)
- Los valores del gráfico problemático son muy pequeños (ej: 1-11)
```

**Ejemplo visual:**

```
┌─────────────────┐
│      251        │
│                 │
│                 │  ← Escala de 1 a 251
│       51        │
│                 │
│        1 ■■■■   │  ← Diagrama aplastado aquí (valores 1-11)
└─────────────────┘
```

### 🔍 Causa Raíz

El error conceptual EST-BOX-01 (confusión posición/valor) genera cuartiles con valores de posición (1, 3, 6, 9, 11) en lugar de valores reales (ej: 200-400 puntos). Cuando se comparte una escala con gráficos que tienen valores reales, el gráfico con posiciones queda visualmente inutilizable.

**Código incorrecto:**

```r
# ❌ PROBLEMA: Incluir EST-BOX-01 en ejercicios con gráficos comparativos
error_seleccionado_idx <- sample(1:3, 1)  # Incluye índice 1 (EST-BOX-01)

# El error EST-BOX-01 genera valores 1-11
list(min = 1, q1 = 3, mediana = 6, q3 = 9, max = 11)

# Pero los datos reales tienen valores como 200-400
# → La escala compartida hace ilegible el gráfico con posiciones
```

### ✅ Solución Verificada

**Enfoque:** Excluir EST-BOX-01 del pool de errores para ejercicios que comparan gráficos visualmente.

```r
# ✅ CORRECTO: Solo usar errores que mantienen valores en el mismo rango
errores_validos_para_grafico <- c(2, 3, 4)  # Excluir índice 1 (EST-BOX-01)

# Seleccionar error de los válidos para representación gráfica
error_seleccionado_idx <- sample(errores_validos_para_grafico, 1)

# Generar distractores solo de errores válidos
otros_errores_idx <- setdiff(errores_validos_para_grafico, error_seleccionado_idx)
```

```r
# ✅ CORRECTO: Calcular rango Y basado en valores reales
y_min_global <- min(sapply(opciones_graficos, function(x) x$min)) - 2
y_max_global <- max(sapply(opciones_graficos, function(x) x$max)) + 2
```

### 🧪 Validación de la Solución

```r
# Verificar que todos los gráficos son visualmente distinguibles
exams2pdf("archivo.Rmd", n = 10)

# Inspeccionar visualmente cada PDF
# Todos los diagramas deben ser legibles en la misma escala
```

### 📋 Checklist de Corrección

- [ ] Identificar errores que generan valores fuera del rango de datos
- [ ] Crear lista `errores_validos_para_grafico` excluyendo esos errores
- [ ] Usar `sample(errores_validos_para_grafico, 1)` para selección
- [ ] Calcular `y_min_global` y `y_max_global` solo con valores válidos
- [ ] Verificar visualmente que todos los gráficos son legibles

### 📅 Historial

| Fecha | Versión | Estado | Validado en |
|-------|---------|--------|-------------|
| 2026-02-07 | v1.0 | ✅ Verificado | diagrama_caja_estaturas_metacognitivo_interpretacion_n2_schoice_v1.Rmd |

---

## Error 6: Rango insuficiente para sample() sin reemplazo

### ❌ Mensaje de Error

```
Error en sample.int(length(x), size, replace, prob):
  imposible tomar una muestra mayor que la población cuando 'replace = FALSE'
```

### 🔍 Causa Raíz

Un contexto en el pool tiene un rango numérico (rango_max - rango_min + 1) menor que el número de datos requeridos (n_datos), lo que hace imposible seleccionar valores únicos sin repetición.

**Ejemplo:**

```r
# ❌ PROBLEMA: Rango insuficiente
list(
  tipo_dato = "edades",
  rango_min = 12,
  rango_max = 18,  # Solo 7 valores únicos: 12,13,14,15,16,17,18
  n_datos = 11     # Necesita 11 valores únicos → IMPOSIBLE
)
```

### ✅ Solución Verificada

**Enfoque 1:** Aumentar el rango del contexto problemático.

```r
# ✅ CORRECTO: Rango suficiente
list(
  tipo_dato = "IMC",
  rango_min = 18,
  rango_max = 32,  # 15 valores únicos: 18-32
  n_datos = 11     # Necesita 11 → POSIBLE
)
```

**Enfoque 2:** Agregar validación de rangos antes de seleccionar contexto.

```r
# ✅ CORRECTO: Filtrar contextos válidos
contextos_validos <- Filter(function(ctx) {
  (ctx$rango_max - ctx$rango_min + 1) >= ctx$n_datos
}, contextos)

# Verificar que hay al menos un contexto válido
if(length(contextos_validos) == 0) {
  stop("No hay contextos con rango suficiente para n_datos")
}

ctx <- contextos_validos[[sample(length(contextos_validos), 1)]]
```

### 📋 Checklist de Corrección

- [ ] Identificar contextos con rango < n_datos
- [ ] Opción A: Aumentar rango_max o reducir rango_min
- [ ] Opción B: Agregar filtro de validación
- [ ] Verificar con múltiples renderizaciones (n >= 50)

### 📅 Historial

| Fecha | Versión | Estado | Validado en |
|-------|---------|--------|-------------|
| 2026-02-07 | v1.0 | ✅ Verificado | diagrama_caja_estaturas_metacognitivo_*.Rmd |

**Contextos corregidos:**

- "edades" (12-18) → "IMC" (18-32)
- "tiempos" (12-18) → "distancias" (8-25)

---

## Error 7: Descripción de error conceptual incoherente con paridad de datos

### ❌ Síntoma del Error

```
El ejercicio genera n=7 datos (impar) pero selecciona como respuesta correcta:
"Para un número par de datos, tomó solo uno de los dos valores centrales"

La descripción del error es imposible con datos impares — no hay "dos valores centrales"
cuando n es impar.
```

**Ejemplo concreto reportado:** 7 estudiantes, error EST-MTC-04 seleccionado.

### 🔍 Causa Raíz

**Dos bugs combinados:**

1. **Filtro de errores sin restricción de paridad:** `errores_mediana_idx <- c(1, 2, 3, 4)` permitía seleccionar EST-MTC-04 sin importar si `n` era par o impar.

2. **Hack silencioso en función calcula:** Para n impar, EST-MTC-04 tenía un fallback que devolvía un valor adyacente al central — producía un número diferente de la mediana, pasando el test numérico, pero con una descripción textual incoherente.

3. **Pool de distractores insuficiente:** Al restringir `errores_mediana_idx` a 3 elementos (n impar), `setdiff()` dejaba solo 2 opciones para `sample(..., 3)`, causando error de sample().

**Fallo sistémico:**
- El test solo verificaba `mediana_erronea != mediana_calc` (coherencia numérica)
- No verificaba coherencia semántica: ¿la DESCRIPCIÓN del error aplica a los datos?
- El script `validar_coherencia_matematica.R` no tiene reglas para este tipo de incoherencia
- El detractor no detectó la incoherencia descripción-datos

### ✅ Solución Verificada

**Corrección 1:** Filtrar errores según paridad de n.

```r
# ANTES (incorrecto):
errores_mediana_idx <- c(1, 2, 3, 4)

# DESPUÉS (correcto):
if (n %% 2 == 0) {
  errores_mediana_idx <- c(1, 2, 3, 4)
} else {
  errores_mediana_idx <- c(1, 2, 3)  # Excluir EST-MTC-04 cuando n es impar
}
```

**Corrección 2:** Eliminar hack de calcula para n impar.

```r
# ANTES (hack silencioso):
calcula = function(datos_ord) {
  n <- length(datos_ord)
  if (n %% 2 == 0) {
    datos_ord[n / 2]
  } else {
    pos <- (n + 1) / 2
    datos_ord[max(1, pos - 1)]  # ← Hack: valor arbitrario
  }
}

# DESPUÉS (honesto):
calcula = function(datos_ord) {
  n <- length(datos_ord)
  if (n %% 2 == 0) {
    datos_ord[n / 2]
  } else {
    stop("EST-MTC-04 no debe usarse con n impar")
  }
}
```

**Corrección 3:** Distractores del pool completo de 6 errores.

```r
# ANTES (insuficiente con 3 errores):
otros_idx <- setdiff(errores_mediana_idx, error_idx)
otros_idx <- sample(otros_idx, 3)  # ← Falla si solo hay 2

# DESPUÉS (siempre suficiente):
todos_errores_idx <- seq_along(errores_conceptuales)
otros_idx <- setdiff(todos_errores_idx, error_idx)
otros_idx <- sample(otros_idx, 3)  # 5 opciones → siempre OK
```

**Corrección 4:** Test de coherencia obligatorio.

```r
test_that("EST-MTC-04 nunca se selecciona cuando n es impar", {
  if (error_sel$codigo == "EST-MTC-04") {
    expect_true(n %% 2 == 0,
      info = paste("EST-MTC-04 con n =", n, "(impar)"))
  }
})

test_that("Descripción del error es coherente con los datos (50 semillas)", {
  for (i in 1:50) {
    d <- generar_datos()
    if (d$error_sel$codigo == "EST-MTC-04") {
      expect_true(d$n %% 2 == 0,
        info = paste("Semilla", i, ": EST-MTC-04 con n =", d$n))
    }
  }
})
```

### 🧪 Validación de la Solución

```
Resultado: 0 incoherencias en 200 ejecuciones
Distribución de errores: EST-MTC-01 (66), EST-MTC-02 (58), EST-MTC-03 (54), EST-MTC-04 (22)
HTML: 9/9 seeds OK
PDF: OK
DOCX: OK
```

### 📋 Checklist de Corrección (Generalizable)

- [ ] Verificar que cada error conceptual del pool tiene precondiciones explícitas
- [ ] Filtrar el pool de errores seleccionables según las características de los datos generados
- [ ] Funciones `calcula()` deben fallar explícitamente (`stop()`) si se llaman fuera de contexto
- [ ] Tests deben verificar coherencia semántica (descripción ↔ datos), no solo numérica
- [ ] Pool de distractores debe ser suficiente después del filtrado

### 📅 Historial

| Fecha | Versión | Estado | Validado en |
|-------|---------|--------|-------------|
| 2026-02-13 | v1.0 | ✅ Verificado | Media-Mediana-Moda.Rmd (200 ejecuciones, 0 incoherencias) |

---

## Futuros Errores

*Este espacio se reserva para documentar futuros patrones de error una vez que sean identificados, corregidos y verificados.*

### Template para nuevos errores:

```markdown
## Error X: [Título descriptivo]

### ❌ Mensaje de Error
[Mensaje exacto del error]

### 🔍 Causa Raíz
[Explicación técnica de la causa]

### ✅ Solución Verificada
[Código antes y después]

### 🧪 Validación de la Solución
[Criterios y comandos de prueba]

### 📋 Checklist de Corrección
[Pasos específicos]

### 🔗 Archivos de Referencia
[Ejemplos verificados]

### 📅 Historial
[Tabla de versiones]
```

---

## Contribución

Para agregar un nuevo patrón de error a este documento:

1. ✅ El error debe estar completamente resuelto
2. ✅ La solución debe estar probada y verificada
3. ✅ Debe existir al menos un archivo .Rmd de referencia funcionando
4. ✅ Seguir el template proporcionado
5. ✅ Incluir ejemplos de código completos (antes/después)
6. ✅ Documentar criterios de validación específicos

**No documentar:**

- ❌ Errores sin solución confirmada
- ❌ Soluciones no probadas
- ❌ Casos específicos sin patrón generalizable

---

## Error 8: Corrupción de RNG por test de diversidad

### ❌ Mensaje de Error
```
No hay mensaje de error explícito. El síntoma es que exams2html(n=50)
produce solo 2-3 versiones únicas en lugar de 40+.
```

### 🔍 Causa Raíz
Un chunk `test_that()` dentro del .Rmd usa `set.seed()` en un loop para verificar diversidad
de versiones. Esto modifica `.Random.seed` en el entorno global de R. Cuando R-exams
genera múltiples versiones después, todas comparten el mismo estado RNG corrompido y
producen datos casi idénticos.

**Flujo del problema:**
1. Chunk `data_generation` genera datos aleatorios correctamente
2. Chunk `diversity_test` ejecuta `for(i in 1:300) { set.seed(i*7); ... }`
3. `.Random.seed` global queda en el estado del último `set.seed(300*7)`
4. `exams2html(n=50)` re-ejecuta el .Rmd 50 veces
5. Cada vez, `data_generation` usa el mismo RNG state → datos idénticos

### ✅ Solución Verificada

#### Código ANTES (incorrecto):
```r
test_that("Diversidad de versiones", {
  versiones <- character(300)
  for (i in 1:300) {
    set.seed(i * 7 + 13)
    # ... genera versiones
    versiones[i] <- digest::digest(list(...))
  }
  n_unicas <- length(unique(versiones))
  expect_true(n_unicas >= 200)
})
```

#### Código DESPUÉS (correcto):
```r
test_that("Diversidad de versiones", {
  # GUARDAR estado RNG antes del test
  saved_seed <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else NULL

  versiones <- character(300)
  for (i in 1:300) {
    set.seed(i * 7 + 13)
    # ... genera versiones
    versiones[i] <- digest::digest(list(...))
  }
  n_unicas <- length(unique(versiones))
  expect_true(n_unicas >= 200)

  # RESTAURAR estado RNG después del test
  if (!is.null(saved_seed)) {
    assign(".Random.seed", saved_seed, envir = globalenv())
  } else {
    rm(".Random.seed", envir = globalenv())
  }
})
```

### 🧪 Validación de la Solución
```r
# Antes del fix: exams2html(n=50) → 2 versiones únicas
# Después del fix: exams2html(n=100) → 94 versiones únicas (94%)
```

### 📋 Checklist de Corrección
1. Buscar TODOS los chunks `test_that` que usen `set.seed()`
2. Agregar guardado de `.Random.seed` AL INICIO del test
3. Agregar restauración de `.Random.seed` AL FINAL del test
4. Verificar con `exams2html(n=50)` que diversidad se mantiene

### 📚 Ejemplo Funcional Utilizado
Archivo corregido: `diagrama_venn_generos_musicales_metacognitivo_argumentacion_n3_cloze_v3.Rmd`

### 📅 Historial

| Fecha | Archivo | Antes | Después | Verificado |
|-------|---------|-------|---------|------------|
| 2026-02-27 | diagrama_venn_generos_musicales (Venn) | 2/50 únicas | 94/100 únicas | ✓ |

---

## Error 9: ##ANSWERi## mal ubicados en ejercicio CLOZE

### ❌ Mensaje de Error
```
No hay error de compilación. El síntoma visual es que las opciones de la
Parte 1 aparecen DESPUÉS del texto de la Parte 2 en el PDF/HTML renderizado.
```

### 🔍 Causa Raíz
En ejercicios R-exams tipo CLOZE, cada `##ANSWERi##` es un placeholder que R-exams
reemplaza con el widget de respuesta correspondiente. Si `##ANSWER1##` se coloca
después del texto de la Parte 2, R-exams inserta las opciones de la Parte 1 en la
posición equivocada.

Errores típicos:
1. `##ANSWER1##` colocado después de `**Parte 2.**` en vez de después de `**Parte 1.**`
2. `##ANSWER4##` omitido completamente (la última parte no tiene widget)
3. Chunks R con `cat()` que duplican el contenido del Answerlist (R-exams ya lo renderiza)

### ✅ Solución Verificada

#### Código ANTES (incorrecto):
```markdown
**Parte 1.** ¿Cuál es el error?

` ``{r opciones_display, echo=FALSE, results='asis'}
for (i in 1:4) cat(paste0("- ", opciones[i], "\n"))
` ``

**Parte 2.** ¿Cuál es el valor correcto?

##ANSWER1##
##ANSWER2##

**Parte 3.** Seleccione las afirmaciones verdaderas:

##ANSWER3##
```

#### Código DESPUÉS (correcto):
```markdown
**Parte 1.** ¿Cuál es el error?

##ANSWER1##

**Parte 2.** ¿Cuál es el valor correcto?

##ANSWER2##

**Parte 3.** Seleccione las afirmaciones verdaderas:

##ANSWER3##

**Parte 4.** Verdadero o falso:

##ANSWER4##

Answerlist
----------
* `r opciones_p1[1]`
* `r opciones_p1[2]`
* `r opciones_p1[3]`
* `r opciones_p1[4]`
*
* `r afirmaciones[1]`
* `r afirmaciones[2]`
* `r afirmaciones[3]`
* `r afirmaciones[4]`
* Verdadero
* Falso
```

### 📋 Checklist de Corrección
1. Contar tipos en `exclozetype` (ej: `schoice|num|mchoice|schoice` = 4 partes)
2. Verificar que existen EXACTAMENTE 4 `##ANSWERi##` (uno por parte)
3. Cada `##ANSWERi##` va INMEDIATAMENTE después del texto de su parte
4. NO usar chunks R para mostrar opciones — el Answerlist al final se encarga
5. Para partes `num`: usar `*` vacío en el Answerlist (sin texto)
6. Verificar visualmente en PDF que cada parte tiene su widget en la posición correcta

### 📚 Referencia
Patrón validado en: `promedios_borrados_metacognitivo_argumentacion_n3_cloze_v1.Rmd`

### 📅 Historial

| Fecha | Archivo | Síntoma | Verificado |
|-------|---------|---------|------------|
| 2026-02-27 | diagrama_venn_generos_musicales (Venn) | Opciones Parte 1 aparecían después de Parte 2 | ✓ |

---

## Error 10: NA en comparación while con calcula()

### ❌ Mensaje de Error
```
Error in while (respuesta_erronea == valor_correcto && intentos_error < 20) {
  : valor ausente donde TRUE/FALSE es necesario
Calls: exams2moodle -> ... -> <Anonymous>
```

### 🔍 Causa Raíz
Las funciones `calcula()` en el pool de errores conceptuales pueden retornar `NA` cuando
el `tipo_operacion` seleccionado no coincide con lo que el error espera. Aunque las
precondiciones deberían filtrar estos casos, ciertas combinaciones de RNG producen
estados donde un error pasa la precondición pero su `calcula()` retorna NA para la
operación específica.

En R, `NA == valor` produce `NA` (no TRUE ni FALSE), y `while(NA)` crashea.

### ✅ Solución Verificada

#### Código ANTES (incorrecto):
```r
while (respuesta_erronea == valor_correcto && intentos_error < 20) {
  # ... reintenta otro error
}

if (respuesta_erronea == valor_correcto) {
  # fallback
}
```

#### Código DESPUÉS (correcto):
```r
while ((is.na(respuesta_erronea) || respuesta_erronea == valor_correcto) && intentos_error < 20) {
  # ... reintenta otro error
}

if (is.na(respuesta_erronea) || respuesta_erronea == valor_correcto) {
  # fallback
}
```

### 🧪 Validación de la Solución
```r
# Antes del fix: semillas 29 y 114 crasheaban (de 200 probadas)
# Después del fix: 0 fallos en 200 semillas
```

### 📋 Checklist de Corrección
1. Buscar TODOS los `while` que comparen con resultado de `calcula()`
2. Agregar `is.na(variable) ||` como primera condición
3. Hacer lo mismo para `if` statements que usen el resultado
4. Verificar con 200+ semillas que no hay crashes

### 📚 Ejemplo Funcional Utilizado
Archivo corregido: `diagrama_venn_generos_musicales_metacognitivo_argumentacion_n3_cloze_v3.Rmd`

### 📅 Historial

| Fecha | Archivo | Semillas fallidas | Verificado |
|-------|---------|-------------------|------------|
| 2026-02-27 | diagrama_venn_generos_musicales (Venn) | 2/200 → 0/200 | ✓ |

---

## Error 11: Drift silencioso de hooks tras instalación de plataforma externa

### ❌ Síntoma

Una herramienta como Ruflo, claude-flow, ruv-swarm o flow-nexus se instala (`npx ... init` o equivalente) y reemplaza `.claude/settings.json`. Los hooks ICFES siguen presentes en `.claude/hooks/*.sh` (ejecutables, sintaxis válida) pero **`settings.json` ya no los carga**. El nuevo handler de la plataforma (típicamente `hook-handler.cjs`) no invoca los `.sh` ICFES.

**Verificación:**
```bash
grep -E "rmd-gate|post-exams2|ortografia" .claude/helpers/<wrapper>.cjs
# Si retorna 0 → wrapper NO invoca hooks ICFES
```

**Detección tardía:** el drift puede pasar desapercibido durante semanas porque las reglas siguen documentadas y los binarios siguen existiendo. La única forma fiable de detectarlo es ejecutar `tests/testthat/test_infraestructura_claude.R` (regla #17, invariante I-3).

### 🔍 Causa Raíz

Las herramientas externas tratan `.claude/settings.json` como su propio archivo de configuración y lo sobrescriben en `init`. No respetan los hooks pre-existentes ni preguntan al usuario antes de reemplazar.

### ✅ Solución Verificada (Ruta B — Convivencia)

Re-enganchar los hooks ICFES en paralelo a los del wrapper externo:

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Write|Edit|MultiEdit",
        "hooks": [
          { "type": "command", "command": "sh -c 'exec node \"${CLAUDE_PROJECT_DIR:-.}/.claude/helpers/hook-handler.cjs\" pre-edit'", "timeout": 5000 },
          { "type": "command", "command": "bash \"${CLAUDE_PROJECT_DIR:-.}/.claude/hooks/pre-write-rmd-gate.sh\"", "timeout": 5000 },
          { "type": "command", "command": "echo 'TILDES OBLIGATORIAS...'" }
        ]
      }
    ],
    "PostToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          { "type": "command", "command": "sh -c 'exec node \"${CLAUDE_PROJECT_DIR:-.}/.claude/helpers/hook-handler.cjs\" post-bash'", "timeout": 5000 },
          { "type": "command", "command": "bash \"${CLAUDE_PROJECT_DIR:-.}/.claude/hooks/post-exams2-validation.sh\"", "timeout": 120000 }
        ]
      }
    ]
  }
}
```

### 🧪 Validación

```bash
# Test live: el gate bloquea correctamente
mkdir -p A-Produccion/01-En-PreDesarrollo/_test_$$
echo '{"tool_input":{"file_path":"A-Produccion/01-En-PreDesarrollo/_test_'"$$"'/d.Rmd","content":"x"}}' \
  | bash .claude/hooks/pre-write-rmd-gate.sh
# Esperado: exit 0 con mensaje "⛔ GATE: Archivo .Rmd bloqueado..."
rmdir A-Produccion/01-En-PreDesarrollo/_test_$$
```

### 📋 Checklist de Prevención

- [ ] ANTES de cualquier `npx ... init`: snapshot `.claude/` con tar (regla #17 paso 1).
- [ ] DESPUÉS del init: ejecutar `Rscript tests/testthat/test_infraestructura_claude.R`.
- [ ] Si el test falla: re-enganchar los hooks ICFES en `settings.json` o revertir.

### 📚 Referencias

- Regla #17: `.claude/rules/infraestructura-protegida.md`
- ADR-001: `.claude/docs/ADR/001-convivencia-ruflo-icfes.md`
- Backup pre-Ruflo: `.claude.pre-ruflo-20260425-123652.tar.gz`
- Backup pre-rehook: `.claude/settings.json.pre-icfes-rehook-20260503-171742`

### 📅 Historial

| Fecha | Plataforma | Hooks perdidos | Detección | Recuperación |
|-------|-----------|----------------|-----------|--------------|
| 2026-04-25 | Ruflo (claude-flow v3) | `pre-write-rmd-gate.sh`, `post-exams2-validation.sh` | 8 días después | commit `fb6ba030` (2026-05-03) |

---

## Error 12: `CLAUDE.md` raíz sobrescrito por plantilla genérica de plataforma

### ❌ Síntoma

El archivo `CLAUDE.md` raíz, que era el índice ICFES con identidad del repo, queda reemplazado por una plantilla genérica del estilo `# Claude Code Configuration - <Plataforma> V<N>`. Las reglas ICFES (16 críticas) ya no aparecen en la primera carga que hace Claude Code al abrir el repo.

### 🔍 Causa Raíz

`npx ... init` y wizards similares reemplazan `CLAUDE.md` raíz como parte de su instalación, asumiendo que el repo es nuevo o que el archivo no existe. No respetan contenido pre-existente.

**Particularidad descubierta en sesión 2026-05-03:** el `CLAUDE.md` raíz nunca había estado versionado en git. Aparecía como `??` (untracked). Esto significa que NO se podía restaurar desde git history porque nunca se había commiteado.

### ✅ Solución Verificada (Mezcla con priority ICFES)

Insertar al inicio del `CLAUDE.md` raíz un bloque ICFES priority de ~47 líneas con:
- Identidad del repo: "# Repositorio ICFES R/exams — Configuración del Repo".
- Pointer a `@.claude/CLAUDE.md` (el índice real).
- Reglas absolutas resumidas (no sobrescribir hooks, no editar inmutables, etc.).
- Declaración explícita: "cuando Ruflo y ICFES entren en conflicto, ICFES gana".

Conservar el contenido externo abajo, marcado como descriptivo:

```markdown
# Repositorio ICFES R/exams — Configuración del Repo

> **IDENTIDAD DEL REPO:** Sistema de generación automatizada de ejercicios ICFES tipo
> SCHOICE/CLOZE en R/exams. NO es un demo de claude-flow / Ruflo, ni un repo genérico.

## Fuente de verdad operativa
[...47 líneas ICFES...]

@.claude/CLAUDE.md

---

# Claude Code Configuration - RuFlo V3

> **Nota:** lo que sigue es la configuración de plataforma Ruflo, instalada el
> 2026-04-25. Es **descriptiva**, no normativa para este repo.
[...244 líneas Ruflo originales...]
```

### 🧪 Validación

```bash
head -1 CLAUDE.md | grep -qE "(ICFES|Repositorio ICFES R/exams)" && echo "I-1 OK"
```

### 📋 Checklist de Prevención

- [ ] Versionar `CLAUDE.md` raíz en git desde el primer momento (no dejarlo untracked).
- [ ] Antes de cualquier `init`: `cp CLAUDE.md CLAUDE.md.pre-<plataforma>-<TS>`.
- [ ] Después: verificar invariante I-1 (regla #17).

### 📚 Referencias

- Regla #17: `.claude/rules/infraestructura-protegida.md` (invariante I-1).
- ADR-001 §"Mezcla de CLAUDE.md raíz".
- Commit de fix: `e8bcc2b3` (2026-05-03).

### 📅 Historial

| Fecha | Plataforma | Restauración |
|-------|-----------|--------------|
| 2026-04-25 | Ruflo | Mezcla aplicada en commit `e8bcc2b3` (2026-05-03) |

---

## Error 13: MCP registrado pero sin conectar (paquete fantasma)

### ❌ Síntoma

`claude mcp list` muestra:
```
ruflo: npx -y ruflo@latest mcp start - ✗ Failed to connect
```

El MCP está en `~/.claude.json` pero al arrancar la sesión, el comando falla. Las herramientas `mcp__<plataforma>__*` que aparecen en la lista de tools diferidas no son invocables.

### 🔍 Causa Raíz

Tres posibles:

1. El paquete npm no existe (versión retirada de npm registry).
2. El paquete existe pero `npx -y <paquete>@latest` falla por incompatibilidad de Node, peer dependencies rotas, o `npm error Invalid Version`.
3. El paquete corre pero el comando `mcp start` falla por config inválida.

### ✅ Solución Verificada

Si la funcionalidad ya está cubierta por otro MCP (ej. `ruv-swarm` y `flow-nexus` cubren coordinación + memoria), **desregistrar el MCP roto**:

```bash
claude mcp remove <nombre>
```

Esto edita `~/.claude.json` (fuera del repo). Verificar:

```bash
claude mcp list 2>&1 | grep -v "✗ Failed" | head
```

Si la funcionalidad NO está cubierta por otro MCP:
1. `npx -y <paquete>@latest --version` para diagnosticar.
2. Probar versiones anteriores: `npx <paquete>@<version-anterior>`.
3. Revisar `/home/bootcamp/.npm/_logs/<fecha>-debug-0.log` para detalles.

### 📋 Checklist de Prevención

- [ ] Cada vez que `claude mcp list` reporte `✗ Failed to connect`: investigar de inmediato.
- [ ] No registrar MCPs experimentales en config global; usar config local del proyecto.

### 📅 Historial

| Fecha | MCP | Acción tomada |
|-------|-----|---------------|
| 2026-05-03 | `ruflo` | Desregistrado (cubierto por `ruv-swarm` + `flow-nexus`) |

---

## Error 14: CLI claude-flow falla con `npm error Invalid Version`

### ❌ Mensaje de Error

```
$ npx @claude-flow/cli@latest doctor
npm warn exec The following package was not found and will be installed: @claude-flow/cli@3.6.21
npm error Invalid Version:
npm error A complete log of this run can be found in: /home/bootcamp/.npm/_logs/<fecha>-debug-0.log
```

### 🔍 Causa Raíz

El paquete `@claude-flow/cli` tiene un `package.json` (propio o de un dependiente) con un campo `version` malformado o incompatible con la versión de npm/Node instalada. El error es de la cadena de dependencias, no del comando que se ejecuta.

### ✅ Solución (parcial — aceptación a medias)

El CLI npm de claude-flow no se usa en el día a día del workflow ICFES. Los componentes locales (`hook-handler.cjs` en `.claude/helpers/`, agentes Ruflo en `.claude/agents/`, skills en `.claude/skills/`) funcionan independientemente del CLI.

**Conclusión adoptada:** aceptar el "Ruflo a medias". El CLI roto no bloquea el workflow ICFES.

**Si en el futuro se necesita el CLI:**
```bash
# Diagnóstico
cat /home/bootcamp/.npm/_logs/<fecha>-debug-0.log | grep -E "Invalid|version"

# Probar versiones anteriores
npx @claude-flow/cli@3.5.0 doctor
npx @claude-flow/cli@3.4.0 doctor

# Limpiar caché npm
npm cache clean --force
```

### 📋 Checklist de Prevención

- [ ] No depender del CLI `claude-flow` para flujos críticos del workflow ICFES.
- [ ] Toda funcionalidad importante debe replicarse en scripts locales (`.claude/scripts/`).

### 📅 Historial

| Fecha | Versión rota | Estado |
|-------|--------------|--------|
| 2026-05-03 | `@claude-flow/cli@3.6.21` | Aceptado a medias (no usado en flujo crítico) |

---

## Error 15: Auto-memory bridge sin paquete instalado

### ❌ Síntoma

En cada SessionStart, el log muestra:
```
[AutoMemory] Importing auto memory files into bridge...
  Memory package not available — skipping auto memory import
```

`node .claude/helpers/auto-memory-hook.mjs status` retorna:
```
Package:        ❌ Not found
Store:          ⏸ Not initialized
LearningBridge: ✅ Enabled
MemoryGraph:    ✅ Enabled
AgentScopes:    ✅ Enabled
```

### 🔍 Causa Raíz

El bridge JS de auto-memoria (parte de Ruflo) está OK, pero la implementación del store (probablemente `@ruflo/agentdb` o `@claude-flow/memory`) no está instalada como paquete npm. El bridge intenta cargarlo dinámicamente, falla silenciosamente y omite la importación.

### ✅ Solución (parcial — vivir sin embeddings)

La auto-memoria de Claude Code (los archivos `~/.claude/projects/*/memory/*.md`) sigue funcionando como siempre — son archivos de texto que Claude lee directamente. Lo que NO se obtiene es la búsqueda semántica vectorial con embeddings ONNX 384-dim.

**Conclusión adoptada:** vivir sin embeddings. Los `MEMORY.md` siguen siendo la fuente de verdad de las lecciones del usuario.

**Si en el futuro se quiere búsqueda semántica:**
1. Identificar el paquete npm faltante (`grep -nE "require|import" .claude/helpers/auto-memory-hook.mjs`).
2. `npm install --save <paquete>`.
3. Verificar: `node .claude/helpers/auto-memory-hook.mjs status` debe reportar `Package: ✅ Found`.
4. Importación inicial: `node .claude/helpers/auto-memory-hook.mjs import`.

### 📋 Checklist de Prevención

- [ ] No depender de embeddings semánticos para flujos críticos.
- [ ] Mantener `MEMORY.md` archivos como fuente primaria; embeddings son una feature extra.

### 📅 Historial

| Fecha | Estado | Acción |
|-------|--------|--------|
| 2026-05-03 | Detectado tras instalación Ruflo | Aceptado, los `MEMORY.md` cubren la necesidad |


---

## Error 16: `\pandocbounded` undefined al renderizar PDF con imágenes Markdown sin atributo `width`

### ❌ Mensaje de Error

```
! Undefined control sequence.
l.5 \pandocbounded
                  {\includegraphics[keepaspectratio]{grafico_equilibrio.png}}
The control sequence at the end of the top line
of your error message was never \def'ed.

Error: LaTeX failed to compile <archivo>_1.tex.
```

### 🔍 Causa Raíz

A partir de **pandoc 3.x**, cuando un Markdown contiene una imagen sin atributos explícitos:

```markdown
![](grafico_equilibrio.png)
```

pandoc envuelve el `\includegraphics` en LaTeX con un comando `\pandocbounded{...}` que pretende ajustar bounding box. **Este comando NO está definido en los templates LaTeX que usa R/exams** (ni en los stock de TinyTeX), por lo que la compilación PDF falla.

El problema NO aparece en HTML ni DOCX, solo en PDF (y por extensión NOPS, que también compila LaTeX).

**Por qué FASE 2G "20/20 OK" no lo detectó:** la validación multi-semilla previa no compilaba PDF en el entorno real del usuario (TinyTeX en Manjaro/zsh) o usaba un template parcheado. Las validaciones en sandbox no son suficientes; HAY que renderizar PDF en el entorno destino.

### ✅ Solución Verificada (3 patrones, en orden de preferencia)

#### Patrón A — Bloque R con `cat()` y atributo `{width=...}` (RECOMENDADO)

```r
` ``{r echo=FALSE, results='asis'}
cat("![](grafico_equilibrio.png){width=80%}\n")
` ``
```

**Por qué funciona**: cuando pandoc ve el atributo `{width=80%}`, NO usa `\pandocbounded` — emite directo `\includegraphics[width=0.8\textwidth]{...}`.

Este patrón está validado en producción en `diagrama_venn_encuesta...Rmd` (línea ~1070) y otros ejercicios con gráficos como opciones.

#### Patrón B — RETIRADO el 2026-08-15: pierde la imagen en el PDF

```r
❌ NO USAR — medido: la imagen NO llega al PDF (is_latex_output() es SIEMPRE FALSE bajo R/exams,
   la rama LaTeX nunca corre; se ejecuta la rama else, que emite HTML crudo, y el escritor LaTeX
   de pandoc lo descarta)
` ``{r echo=FALSE, results='asis'}
if (knitr::is_latex_output()) {
  cat("\\includegraphics[width=0.8\\textwidth]{grafico_equilibrio.png}")
} else {
  cat('<img src="grafico_equilibrio.png" width="80%" />')
}
` ``
```

**Sustituto vigente — Patrón B'** (emitir ambos markups sin condicional; pandoc descarta el que
no corresponde a su destino):

```r
` ``{r echo=FALSE, results='asis'}
cat("\\includegraphics[width=0.8\\textwidth]{grafico_equilibrio.png}\n")  # sobrevive solo en LaTeX
cat('<img src="grafico_equilibrio.png" width="80%" />\n')                 # sobrevive solo en HTML
` ``
```

**Cuándo usarlo**: solo si necesitas markups realmente distintos por formato. Para el caso normal
—una imagen con un tamaño— el Patrón A (`{width=...}`) basta y es más simple. Ver
`.claude/rules/markdown-imagenes-pdf.md` Patrón B'.

#### Patrón C — Preamble fix (último recurso, no portable)

Solo si A y B no son aplicables, agregar al template LaTeX:

```latex
\providecommand{\pandocbounded}[1]{#1}
```

NO recomendado porque depende de modificar templates fuera del .Rmd.

### 🚫 Antipatrón PROHIBIDO

```markdown
❌ ![](grafico.png)              # sin atributo → \pandocbounded
❌ ![texto alternativo](g.png)    # sin atributo → \pandocbounded
```

```r
❌ cat("![](g.png)\n")            # genera Markdown sin width → mismo bug
```

### 🧪 Validación de la Solución

```r
# Renderizar en entorno real (no simulado)
exams2pdf("archivo.Rmd", n = 5, dir = "salida_pdf")

# Verificar que el .tex generado NO contiene \pandocbounded
system("grep -L 'pandocbounded' salida_pdf/*.tex")  # debe listar todos los archivos

# Verificar que los PDFs se generaron
ls salida_pdf/*.pdf
```

Suite de tests: `tests/testthat/test_pandocbounded_y_solution_coherence.R` ejecuta este check automáticamente sobre todos los `.Rmd` modificados.

### 📋 Checklist de Corrección

- [ ] Identificar TODAS las imágenes en el `.Rmd` (`grep -n '!\\[' archivo.Rmd`).
- [ ] Reemplazar cada `![](file.png)` por `cat("![](file.png){width=Y%}\n")` dentro de un bloque R.
- [ ] Re-renderizar con `exams2pdf()` en el entorno real con ≥5 semillas.
- [ ] Confirmar `grep -c 'pandocbounded' salida/*.tex` retorna 0 en todos.
- [ ] Inspección visual: el gráfico aparece en el PDF (no se rompió por el cambio).

### 📚 Ejemplo Funcional Utilizado

`A-Produccion/03-En-Produccion/.../diagrama_venn_encuesta_metacognitivo_*.Rmd` línea 1070+ — patrón A validado en 4/4 formatos y 300/300 versiones únicas.

### 🛡️ Defensa Automática

| Capa | Mecanismo | Detecta |
|---|---|---|
| Pre-Write | hook `pre-write-rmd-gate.sh` (futuro) | `![](*.png)` sin width attribute en .Rmd nuevos |
| Post-Render | hook `post-exams2-validation.sh` FASE 2I | `\pandocbounded` en .tex generado |
| Test suite | `test_pandocbounded_y_solution_coherence.R` | Patrón Markdown crudo sin width en .Rmd existentes |
| Agente SCHOICE | pre-flight check en `orquestador-schoice` | Detecta antes de generar |

### 📅 Historial

| Fecha | Archivo afectado | Resultado |
|-------|------------------|-----------|
| 2026-05-03 | `interseccion_ingresos_gastos_metacognitivo_interpretacion_n2_schoice_v1.Rmd` | Detectado por usuario al ejecutar `exams2pdf()`. Fix Patrón A aplicado. 5/5 semillas PDF OK post-fix. |

---

## Error 17: Inconsistencia Solution↔Answerlist por `exshuffle: TRUE` con referencia explícita a letra

### ❌ Síntoma

El estudiante ve:
- En la sección Solution: "La respuesta correcta es la **Opción A** porque..."
- En el Answerlist: la opción marcada como correcta es **(c)**.

El bug es silencioso: el .Rmd compila correctamente, los 4 formatos generan, pero el contenido es incoherente. NO lo detecta validación sintáctica ni de metadatos.

### 🔍 Causa Raíz

Cuando un .Rmd SCHOICE tiene:

```yaml
exshuffle: TRUE        # R-exams re-mezcla las opciones
```

```markdown
Solution
========
La respuesta correcta es la **Opción `r letra_correcta`** porque...
```

El flujo es:

1. `data_generation` calcula `letra_correcta = "A"` (basado en la posición de la opción correcta tras `sample()` interno).
2. R-exams parsea el .Rmd y construye el ejercicio con las opciones en su orden actual.
3. R-exams aplica `exshuffle: TRUE` y **re-mezcla las opciones del Answerlist**, ajustando `exsolution` automáticamente.
4. **PERO** el texto de la Solution (`r letra_correcta`) ya fue evaluado en el paso 1, así que sigue diciendo "Opción A" aunque ahora la correcta esté en posición (c).

Resultado: incoherencia silenciosa entre Solution narrativa y Answerlist.

### ✅ Solución Verificada

Para SCHOICE con opciones gráficas (PNGs) **O** texto con Solution que referencia la letra explícitamente:

```yaml
exshuffle: FALSE       # ✓ NO dejar que R-exams re-mezcle
```

Y en `data_generation`:

```r
# Mezclar internamente
opciones_mezcladas <- sample(todas_opciones)
indice_correcto <- which(names(opciones_mezcladas) == "correcta")

# Vector de solución posicional
sol <- rep(0, 4)
sol[indice_correcto] <- 1

# Letra correspondiente (sincronizada con sample interno)
letras <- c("A", "B", "C", "D")
names(opciones_mezcladas) <- letras
letra_correcta <- letras[indice_correcto]
```

```yaml
exsolution: `r paste(as.integer(sol), collapse="")`
```

**La aleatorización ya está garantizada por `sample()`**: cada renderizado con semilla distinta produce orden distinto. `exshuffle: FALSE` solo evita que R-exams haga una segunda mezcla incoherente.

### 🚫 Antipatrón PROHIBIDO

```yaml
exshuffle: TRUE       # PROHIBIDO si Solution dice "Opción `r letra_correcta`"
```

```markdown
La respuesta correcta es la **Opción A** porque...   ← letra hardcoded
```

### 🧪 Validación de la Solución

```r
# Renderizar 20 semillas dispersas
for (s in c(1, 7, 13, 23, 41, 59, 73, 89, 101, 113, 127, 137, 149, 163, 173, 191, 197, 211, 223, 239)) {
  set.seed(s)
  out <- exams2html("archivo.Rmd", n = 1)

  # Extraer letra mencionada en Solution y posición correcta en exsolution
  # Verificar que coinciden
  assert_solution_matches_answerlist(out)
}
```

Suite: `tests/testthat/test_pandocbounded_y_solution_coherence.R` automatiza esta verificación.

### 📋 Checklist de Corrección

- [ ] Si la Solution referencia `r letra_correcta` o cualquier letra explícita: `exshuffle: FALSE`.
- [ ] Mezcla interna con `sample()` ya garantiza aleatorización (verificar con 20+ semillas: 250+ versiones únicas).
- [ ] `exsolution` debe construirse desde el vector `sol` posicional.
- [ ] La variable `letra_correcta` debe calcularse DESPUÉS del `sample()`.

### 📚 Ejemplo Funcional Utilizado

`A-Produccion/03-En-Produccion/Ejemplos-Funcionales-Rmd/estadistica_diagramas_caja_interpretacion_representacion_Nivel2_v2.Rmd`

### 🛡️ Defensa Automática

| Capa | Mecanismo | Detecta |
|---|---|---|
| Test suite | `test_pandocbounded_y_solution_coherence.R` | Combinación `exshuffle: TRUE` + `r letra_correcta` o "Opción [A-D]" en Solution |
| Hook post-render | `post-exams2-validation.sh` FASE 2I | Mismatch detectado por análisis estático del .Rmd |
| Detractor | dominio `codigo_rexams` | Reporta como objeción ALTA |

### 📅 Historial

| Fecha | Archivo afectado | Resultado |
|-------|------------------|-----------|
| 2026-05-03 | `interseccion_ingresos_gastos_metacognitivo_interpretacion_n2_schoice_v1.Rmd` | Usuario detectó "Opción A en Solution ≠ opción marcada en Answerlist". Fix `exshuffle: FALSE` aplicado. 20/20 semillas coherentes post-fix. |

### 🔗 Reglas Cruzadas

- `.claude/rules/codigo-rmd.md` regla #6 (excepción documentada)
- `.claude/rules/graficos-como-opciones.md` (caso específico SCHOICE con PNGs)

---

## Error 18: Estudiante puede identificar opción correcta por formato gráfico sin verificar datos

**Detectado:** 2026-05-14 (sesión v2 distribucion-contagiados)
**Severidad:** ALTA (el ejercicio mide formato, no comprensión de datos)
**Síntoma observado:** si el conjunto de opciones tiene un solo gráfico de barras (o una sola torta) y ese formato coincide siempre con la opción correcta, el estudiante aprende a responder por formato sin analizar los datos.

### Causa raíz

Cuando las 4 opciones de un SCHOICE gráfico tienen distribución desigual de formatos —por ejemplo, 3 tortas + 1 barra— el estudiante puede inferir la respuesta correcta por eliminación visual sin verificar categorías ni proporciones. En la v1 de `distribucion-contagiados`, la opción correcta era SIEMPRE una torta de 3 sectores. El estudiante que detectaba ese patrón podía acertar sin leer la tabla.

### Consecuencia pedagógica

El ejercicio deja de evaluar la competencia declarada ("Interpretar y transformar información entre formatos") y pasa a evaluar "reconocer el tipo de gráfico que siempre es correcto". La Alfirmación y Evidencia ICFES quedan sin verificar.

### Fix permanente (format diversity principle)

**Principio:** Para todo SCHOICE con opciones gráficas, el formato de la opción correcta DEBE aparecer en al menos 2 de las 4 opciones. Idealmente, usar 2 de un formato + 2 de otro (ej: 2 barras + 2 tortas).

**Regla asociada:** `.claude/rules/graficos-como-opciones.md` §Formato equilibrado

**Implementación en la v2 de distribucion-contagiados:**

| Opción | v1 (frágil) | v2 (robusto) |
|--------|-------------|--------------|
| Correcta | Torta 3 sectores proporcionales | **Barras** 3 categorías + alturas exactas |
| Distractor 1 | Torta + categoría "Otro" | Torta + categoría "Otro" |
| Distractor 2 | Torta áreas iguales | Torta áreas iguales |
| Distractor 3 | Barras + categoría "0-45" | **Barras** categorías correctas + alturas permutadas |

En v2, el estudiante ve 2 barras y 2 tortas. No puede descartar por formato. Debe verificar:
- ¿Están todas las categorías de la tabla? (descarta la torta con "Otro")
- ¿Las áreas/alturas son proporcionales a las frecuencias? (descarta la torta de áreas iguales)
- ¿La altura de cada barra coincide con su categoría específica? (descarta la barra con alturas permutadas)

### Verificación en el código

```r
# Validación obligatoria en data_generation: verificar equilibrio de formatos
formatos <- sapply(opciones_mezcladas, function(x) x$formato)
stopifnot(sum(formatos == "barras") == 2, sum(formatos == "torta") == 2)
```

### 📅 Historial

| Fecha | Archivo afectado | Resultado |
|-------|------------------|-----------|
| 2026-05-14 | `distribucion_contagiados_metacognitivo_interpretacion_n3_schoice_v1.Rmd` | v1 vulnerable (1 formato correcto = único en su tipo). v2 creada con 2 barras + 2 tortas. Verificado 10 semillas: correcta siempre = barras, formato nunca delata. |

### 🔗 Reglas Cruzadas

- `.claude/rules/graficos-como-opciones.md` §Formato equilibrado (nueva sección, 2026-05-14)
- Error 20 (GRAF-BAR-01): patrón de distractor de barras con alturas permutadas que hace posible el equilibrio de formatos
- `.claude/rules/ejercicios-metacognitivos.md` (Progressive Disclosure: el estudiante debe analizar, no reconocer patrones superficiales)

---

## Error 19: Solution con `r letra_correcta` rompe coherencia bajo Moodle re-shuffle

**Detectado:** 2026-05-12 (estudiante real KEVIN A. SILVA, p3c-mat)
**Severidad:** ALTA (silenciosa, solo visible en producción Moodle)
**Síntoma observado:** estudiante seleccionó Opción C → sistema marcó "Incorrecta" → Solution decía "Respuesta correcta: Opción C". Contradicción visible al estudiante.

### Causa raíz

El `.Rmd` tenía:

```rmd
### Respuesta correcta: Opción `r letra_correcta` {#respuesta-correcta-...}
```

con `exshuffle: FALSE` y mezcla interna con `sample()`. La asunción implícita: "exshuffle:FALSE evita re-shuffle".

**Esa asunción es falsa en Moodle.** Moodle tiene un setting independiente "Shuffle answers" en la configuración del cuestionario (no relacionado con exshuffle de R-exams). Cuando está activado, Moodle re-ordena las opciones en tiempo de display PERO no toca el valor de `letra_correcta` ya escrito en la prosa de Solution. Resultado: el estudiante ve "Respuesta correcta: Opción C" pero la opción etiquetada C en su pantalla no es la que R-exams generó como correcta.

### Por qué los validadores no lo detectaron

1. FASES 2A-2H operan sobre R-exams nativo. No simulan Moodle.
2. FASE 2I.3 detecta `exshuffle:TRUE + letra` (Error 17) pero no `exshuffle:FALSE + letra + Moodle shuffle`.
3. 4/4 formatos (HTML/PDF/DOCX/NOPS) pasaban porque ninguno aplica re-shuffle adicional.
4. Multi-semilla (FASE 2G) valida coherencia interna R-exams; el bug solo emerge en el target Moodle.

### Fix permanente (regla #19)

**Regla:** `.claude/rules/solution-letter-independence.md`

Solution NUNCA debe referenciar opciones por letra/posición. SIEMPRE por contenido (`descripcion_corta`), código de error (`GRAF-ARG-NN`) o etiqueta semántica.

Patrones prohibidos en Solution:
- P1: `` `r letra_correcta` `` o `` `r letras[...]` ``
- P2: prosa con letra interpolada
- P3: `cat("**Opción ", l, ...)` en chunk R
- P4: literal "Opción [A-D]" en Markdown

### Patrón correcto (antes/después)

**Antes (frágil):**

````rmd
### Respuesta correcta: Opción `r letra_correcta` {#respuesta-correcta-`r ex_uid`}

```{r}
err_correcto <- errores_conceptuales[[2]]
cat(paste0(err_correcto$descripcion_larga, "\n"))
```

### Análisis de los distractores

```{r}
for (l in letras) {
  opc <- opciones_mezcladas[[l]]
  if (opc$tipo != "correcto") {
    err <- errores_conceptuales[[opc$error_idx]]
    cat(paste0("**Opción ", l, " (", err$codigo, "):** ", err$descripcion_larga))
  }
}
```
````

**Después (robusto):**

````rmd
### Respuesta correcta {#respuesta-correcta-`r ex_uid`}

**Argumento válido:** "`r errores_conceptuales[[2]]$descripcion_corta`"

```{r}
err_correcto <- errores_conceptuales[[2]]
cat(paste0(err_correcto$descripcion_larga, "\n"))
```

### Análisis de los argumentos incorrectos

```{r}
for (l in letras) {
  opc <- opciones_mezcladas[[l]]
  if (opc$tipo != "correcto") {
    err <- errores_conceptuales[[opc$error_idx]]
    cat(paste0(
      "**", err$codigo, " — ", err$nombre, "**\n\n",
      "*Argumento:* \"", err$descripcion_corta, "\"\n\n",
      err$descripcion_larga, "\n\n"
    ))
  }
}
```
````

### Defensas implementadas (4 capas)

1. **Hook FASE 2J** (`post-exams2-validation.sh`): detecta P1-P4 en sección Solution. Bloqueante.
2. **Test estático** (`test_letter_independence.R`): 4 tests + self-test. Falla CI si nuevo .Rmd cae en patrón.
3. **Pre-write gate** (futuro, regla #16): bloquear Write/Edit de .Rmd con patrones prohibidos.
4. **Detractor** (dominio `codigo_rexams`): check explícito de letter-independence.

### Action item: .Rmd legacy con bug

Lista en `tests/testthat/test_letter_independence.R::.legacy_known_letter_dep`. Fix uno por uno y remover de la lista. Total al crear la regla: 8 .Rmd.

### 📅 Historial

| Fecha | Archivo afectado | Resultado |
|-------|------------------|-----------|
| 2026-05-12 | `Comparacion-Lineas-Temporales-Schoice` | Estudiante real (KEVIN A. SILVA) reportó incoherencia en Moodle. Fix: Solution reescrita sin `r letra_correcta`. Regla #19 creada. Commit `86a4b211`. |

### 🔗 Reglas Cruzadas

- `.claude/rules/solution-letter-independence.md` (regla #19, fix principal)
- `.claude/rules/codigo-rmd.md` regla #6 excepción (casos 2 y 3 ahora obsoletos)
- Error 17 (gemelo: exshuffle:TRUE + letra con R-exams)
- `tests/testthat/test_letter_independence.R`
- `.claude/rules/markdown-imagenes-pdf.md` (regla anti-pandocbounded)

---

## Error 20: GRAF-BAR-01 — Gráfico de barras con categorías correctas pero alturas permutadas

**Detectado:** 2026-05-14 (sesión v2 distribucion-contagiados)
**Severidad:** MEDIA (patrón de distractor pedagógico; no es un bug sino un nuevo tipo de distractor a catalogar)
**Código:** GRAF-BAR-01

### Descripción del patrón

Un distractor de gráfico de barras que muestra las categorías correctas del eje horizontal (coinciden con la tabla) pero con alturas que no corresponden a las frecuencias reales — los valores están permutados entre categorías. El estudiante que solo verifica "¿las categorías coinciden?" cae en este distractor. Solo quien verifica "¿la altura de CADA barra coincide con la frecuencia de SU categoría?" lo descarta correctamente.

### Valor pedagógico

Este distractor es más fuerte que los basados en formato (GRAF-TOR-01: categoría extra; GRAF-TOR-02: áreas iguales) porque:

1. **Pasa la verificación de categorías**: las 3 etiquetas del eje X coinciden con la tabla
2. **Pasa la verificación de formato**: es un gráfico de barras, formato perfectamente válido
3. **Solo falla en la verificación de valores por categoría**: hay que comparar cada barra con su celda correspondiente en la tabla

Esto lo hace particularmente adecuado para ejercicios de Nivel 3 (DOK 3), donde el estudiante debe realizar verificación sistemática y no solo reconocimiento de patrones.

### Uso del patrón

El distractor se construye permutando las frecuencias entre las categorías, con una guardia que garantiza que la permutación NO coincida con el orden original:

```r
# Generar frecuencias permutadas para el distractor de barras
freq_perm <- frecuencias
while (TRUE) {
  freq_perm <- sample(frecuencias)
  if (!all(freq_perm == frecuencias)) break  # Garantiza que al menos una cambió
}

# Las categorías son LAS MISMAS que la tabla
cats_distractor <- categorias_tabla  # "45-64", "65-74", "75+"
# Pero las alturas están asignadas a categorías equivocadas
vals_distractor <- as.numeric(freq_perm)
```

### Verificación en el código

```r
# Validar que las categorías del distractor de barras coinciden con la tabla
# pero las alturas NO (eso es lo que lo hace ser un distractor)
stopifnot(all(cats_distractor == categorias_tabla))
stopifnot(!all(vals_distractor == as.numeric(frecuencias)))
```

### 📅 Historial

| Fecha | Archivo | Resultado |
|-------|---------|-----------|
| 2026-05-14 | `distribucion_contagiados_metacognitivo_interpretacion_n3_schoice_v2.Rmd` | GRAF-BAR-01 implementado como distractor 3. Combinado con GRAF-TOR-01 y GRAF-TOR-02 para lograr 2 barras + 2 tortas. |

### 🔗 Reglas Cruzadas

- `.claude/rules/graficos-como-opciones.md` §Formato equilibrado (la diversidad de formatos requiere distractores en ambos formatos)
- Error 18 (format-based guessing): GRAF-BAR-01 es la pieza que permite equilibrar formatos sin sacrificar calidad de distractores
- `.claude/rules/ejercicios-metacognitivos.md` (pool de errores conceptuales con códigos documentados)
- `Error 5 (EST-BOX-01)` — otro error de gráficos por confusión valor/posición, pero en boxplots

---

## Error 21: `No counter 'none' defined` — tablas Markdown con pandoc ≥3.7 (RStudio)

### ❌ Mensaje de Error

```
! LaTeX Error: No counter 'none' defined.

Error: LaTeX failed to compile <archivo>_1.tex. See ...
```

Ocurre al ejecutar `exams2pdf()` / `exams2nops()` **desde RStudio**, aunque la validación del pipeline (terminal) diera 4/4 OK.

### 🔍 Causa Raíz

RStudio usa su **pandoc bundleado** (`/usr/lib/rstudio/resources/app/bin/quarto/bin/tools/x86_64/pandoc`, v**3.8.3**), distinto del pandoc de terminal (**3.6**) que usa `Rscript` y la validación del pipeline. Pandoc **≥3.7** envuelve toda tabla `longtable` (lo que produce un pipe table Markdown de `knitr::kable(format="markdown")` o `cat("| ...")`) con:

```latex
{\def\LTcaptype{none} % do not increment counter
\begin{longtable}[]{@{}...@{}}
...
\end{longtable}
}
```

`\def\LTcaptype{none}` asume un contador LaTeX `none` que la plantilla minimalista de R-exams **no define**. Pandoc 3.6 no emite ese wrapper → el bug es invisible en terminal. Gemelo del **Error 16** (`\pandocbounded`): env-específico por versión de pandoc.

### ✅ Solución Verificada

Al inicio de la sección `Question`, un bloque raw LaTeX (ignorado en HTML/DOCX), con guardia para no redefinir en NOPS multi-ítem (fence de 3 backticks con `{=latex}`):

```
\makeatletter\@ifundefined{c@none}{\newcounter{none}}{}\makeatother
```

### 🧪 Validación de la Solución

Probado con **pandoc 3.8.3 (RStudio) y 3.6 (terminal)**:

| Llamada | pandoc 3.8.3 | pandoc 3.6 |
|---|---|---|
| `exams2pdf(f, n=1)` | ✓ | ✓ |
| `exams2nops(rep(f,3), n=1)` | ✓ | ✓ |
| `exams2html(f, n=1)` | ✓ (sin fuga LaTeX) | — |
| `exams2pandoc(f, type="docx")` | ✓ | — |

Reproducir el entorno RStudio en terminal:
```r
Sys.setenv(RSTUDIO_PANDOC = "/usr/lib/rstudio/resources/app/bin/quarto/bin/tools/x86_64")
stopifnot(as.character(rmarkdown::pandoc_version()) >= "3.7")
```

### 📋 Checklist de Corrección

1. ¿El `.Rmd` usa tabla Markdown (`kable(format="markdown")` o `cat("| ...")`)? → requiere el guard.
2. Insertar el bloque `{=latex}` con `\@ifundefined{c@none}{\newcounter{none}}{}` al inicio de `Question`.
3. Re-render con pandoc de RStudio (PDF + NOPS×N) y con pandoc de terminal.
4. Confirmar que el HTML no muestra LaTeX crudo.

### 📅 Historial

| Fecha | Archivo | Causa | Fix | Resultado |
|-------|---------|-------|-----|-----------|
| 2026-06-03 | rango_colesterol_..._n3_schoice_v1.Rmd | pandoc 3.8.3 RStudio + `\LTcaptype{none}` | guard `\newcounter{none}` en Question | PDF/NOPS×3/HTML/DOCX OK (commit `d22caf93`) |

### 📚 Referencias

- Regla #20: `.claude/rules/markdown-tablas-pandoc.md`
- Regla #18 / Error 16 (`\pandocbounded`): mismo patrón de diferencia de pandoc en entorno destino
- Hook FASE 2K (`ERR_TABLA_NONE`) + `tests/testthat/test_markdown_tablas_none_guard.R`
- Memoria: `feedback_pandoc_ltcaptype_none.md`

---

## Error 22: Bucle `repeat`/`while` sin cota en `data_generation` → cuelgue en ~1-2% de semillas

### ❌ Mensaje de Error

No hay mensaje: el proceso **se congela** (sin error, sin salida). `exams2html/pdf(n=200)`, la prueba de diversidad o el multi-semilla quedan colgados indefinidamente en una fracción de las semillas. Con timeout por semilla se observa:

```
Cuelgues (timeout): 9  -> seeds: 23,229,298,351,404,433,527,564,587   (de 600)
```

### 🔍 Causa Raíz

Un bucle de reintento que **resamplea hasta cumplir una condición que puede ser imposible** para ciertos datos. Caso real (`rango_colesterol_..._cloze_v1`, Parte 6):

```r
# ❌ ANTI-PATRÓN
es_verdadero_p6 <- sample(c(TRUE, FALSE), 1)
rango_mas_grande <- max(rangos)
if (es_verdadero_p6) {
  repeat {                                           # sin contador, sin límite
    vals_extra <- sample(rango_var[1]:rango_var[2], n_controles, replace = TRUE)
    rango_extra <- max(vals_extra) - min(vals_extra)
    if (rango_extra > rango_mas_grande) break        # IMPOSIBLE si rango_mas_grande >= span
  }
}
```

El rango máximo alcanzable por `vals_extra` es `span = rango_var[2] - rango_var[1]`. Cuando los datos generan `rango_mas_grande >= span` (sucede en ~2% de las semillas), la condición `rango_extra > rango_mas_grande` **nunca se cumple** → bucle infinito. Aun sin llegar al caso imposible, cuando `rango_mas_grande = span - 1` el bucle puede tardar miles de iteraciones (probabilidad ínfima de muestrear ambos extremos en 5 draws) → cuelgue práctico.

### ✅ Solución Verificada

**Construcción determinista del valor objetivo en lugar de reintentar.** Se elige el rango objetivo dentro del rango factible y se construyen los valores garantizando ese rango exacto:

```r
# ✅ CORRECTO — sin bucles, termina siempre
rango_mas_grande <- max(rangos)
span_var <- rango_var[2] - rango_var[1]
# pick_int: entero uniforme en [a,b], seguro cuando a==b (evita la trampa sample(escalar,1))
pick_int <- function(a, b) if (a >= b) a else sample(a:b, 1L)

if (rango_mas_grande >= span_var) {
  es_verdadero_p6 <- FALSE                      # "Verdadero" sería imposible -> forzar Falso
} else {
  es_verdadero_p6 <- sample(c(TRUE, FALSE), 1)
}
if (es_verdadero_p6) rango_objetivo <- pick_int(rango_mas_grande + 1, span_var)
else                 rango_objetivo <- pick_int(0, rango_mas_grande)

base_extra <- pick_int(rango_var[1], rango_var[2] - rango_objetivo)
relleno_extra <- if (rango_objetivo == 0) rep(base_extra, n_controles - 2)
                 else sample(base_extra:(base_extra + rango_objetivo), n_controles - 2, replace = TRUE)
vals_extra <- sample(c(base_extra, base_extra + rango_objetivo, relleno_extra))
rango_extra <- max(vals_extra) - min(vals_extra)
stopifnot(rango_extra == rango_objetivo,
          es_verdadero_p6 == (rango_extra > rango_mas_grande))
```

**Trampa adicional cubierta** (`sample()` con vector de longitud 1): `sample(x, k)` cuando `length(x)==1` reinterpreta `x` como `1:x`. Por eso `pick_int(a,a)` retorna `a` y el relleno usa `rep()` cuando `rango_objetivo == 0`.

### 🧪 Validación de la Solución

| Métrica | Antes del fix | Después del fix |
|---|---|---|
| Cuelgues (600 semillas, timeout 4s) | 9 | **0** |
| Semillas con `rmg >= span` | 12 (3 contadas + 9 colgadas) | 12 (todas → P6=Falso, sin cuelgue) |
| Diversidad (300 generaciones) | colgaba | **300/300 únicas, 0 fallos** |
| Multi-semilla Nivel 5 (20) | — | **100% APROBADO** |
| Invariantes P1-P6 violadas | 0 | 0 |

### 📋 Checklist de Corrección

1. ¿Hay un `repeat {` o `while(...)` que resamplea hasta cumplir una condición?
2. ¿La condición puede ser **imposible** para algún dato válido (objetivo fuera del rango alcanzable)?
3. Reemplazar por construcción determinista del valor objetivo, o agregar **contador + `max_intentos`** con `stopifnot`/fallback.
4. Cuidar la trampa `sample(escalar, k)` (usar `pick_int`/`rep`).
5. Validar con stress test de timeout por semilla (≥ 200 semillas).

### 📅 Historial

| Fecha | Archivo | Causa | Fix | Resultado |
|-------|---------|-------|-----|-----------|
| 2026-06-05 | rango_colesterol_..._n3_cloze_v1.Rmd | `repeat` Parte 6 con condición imposible si `rango_mas_grande >= span` | construcción determinista con `pick_int` | 0 cuelgues / 300 únicas / FASE 2A APROBADO |

### 📚 Referencias

- Test de regresión: `tests/testthat/test_data_generation_no_hang.R` (detector estático `repeat` sin cota + guard runtime con timeout)
- `codigo-rmd.md` regla #9 (guardia `is.na()` en `while` con `calcula()`) — patrón hermano de robustez en bucles
- Memoria: `feedback_repeat_sin_cota_cuelgue.md`

## Error 23: Etiquetas de texto solapadas en diagramas dinámicos (cuña angular estrecha)

### ❌ Síntoma

En un diagrama generado dinámicamente (R/`grid`, TikZ o matplotlib), una etiqueta de texto se **solapa con una línea, un punto o un eje** en SOLO algunas versiones (no en todas). Caso real (`desplazamiento_avion_aeropuerto_..._n3_schoice_v1`, 2026-06-28): la etiqueta del ángulo `"30°"` se montaba sobre la línea verde y el punto naranja cuando el ángulo era el **mínimo del pool** (30°). HTML/PDF rinden "sin error" — el defecto es puramente visual y solo aparece en un subconjunto del espacio de parámetros aleatorios.

### 🔍 Causa Raíz

Colocar la etiqueta con una heurística que **ignora la geometría real**. Dos causas combinadas:

1. **Cuña estrecha + texto ancho**: la etiqueta del ángulo va sobre la bisectriz, dentro de la cuña entre dos rectas separadas por `ángulo`. La distancia perpendicular del CENTRO del texto a cada recta es `radio·sin(ángulo/2)`; con ángulos pequeños se hace diminuta. Y el texto horizontal tiene ancho (~30 px para `"NN°"`): su borde más cercano a la recta reduce esa holgura en `media_anchura·cos(ángulo/2)`. La fórmula anterior fijaba el radio según la **longitud del vector** (`Lpx`), no según el ángulo ni el ancho del texto → con 30° y radio 34 quedaban ~8.8 px de centro y ~2.8 px de borde → toque.
2. **Colisión con marcador móvil**: el punto está sobre una recta a radio `Lpx` (variable por versión); la etiqueta sobre la bisectriz a radio fijo. Cuando `Lpx ≈ radio_etiqueta` ambos quedan a la misma "altura" separados solo por `ángulo/2` → se tocan en vectores de longitud media.
3. **Ángulo grande + piso bajo (descubierto 2026-06-28)**: para ángulos grandes (p.ej. 70°) la fórmula `(8+11·cos(semi))/sin(semi)` da ~30 (por debajo del piso), así que se usa el piso. Con piso 34 la etiqueta queda cerca del vértice y la **recta casi horizontal** (la otra frontera de la cuña ancha) clipa el borde superior del texto. La validación inicial (montajes en miniatura) NO lo detectó; se vio sólo al ampliar el HTML ×2.4. Fix: subir el piso a 50 → holgura `50·sin(35°) ≈ 28 px`.

### ✅ Solución Verificada

Radio de la etiqueta consciente del **ancho del texto** y de la **posición del marcador**:

```r
# ❌ ANTES — solo dependía de Lpx
rang <- 34 + max(0, 26 - Lpx)

# ✅ DESPUÉS
semi  <- (angulo/2) * pi/180
R_fit <- max(50, (8 + 11 * cos(semi)) / sin(semi))   # 8 = holgura; 11 = media-anchura aprox de "NN°"; piso 50 cubre ángulos grandes
rang  <- if (abs(Lpx - R_fit) < 22) Lpx + 24 else R_fit   # si el punto cae a la altura del label, más allá del punto
```

### 🧪 Validación de la Solución

Grilla COMPLETA de parámetros (333 combos válidos `total×avanzada×ángulo`; 37 por ángulo) × 4 tipos de diagrama (correcta/recorrida/suma/perp) × ángulos {30,40,55,70}; montajes + recortes ampliados → 0 solapamientos en ángulos pequeños/medios. **Corrección 2026-06-28**: el ángulo 70° (cuña ancha) SÍ clipaba con el piso 34 (la miniatura lo ocultó; visible al ampliar ×2.4 el HTML autocontenido). Se subió el piso a 50 y se revalidó con **40 semillas reales** (ángulos 30–70, 4 cuadrantes, vectores 20–200 km), ampliando ×2.4 los 13 casos de 60–70° y el caso corto+70° → **0 solapamientos**. `exams2html/pdf/pandoc(docx)/nops` compilan; diversidad sustantiva PASS.

### 📋 Checklist de Corrección (generalizable)

1. ¿La etiqueta se coloca relativa a elementos geométricos cuya posición depende de parámetros aleatorios (ángulo, longitud)?
2. ¿El radio/offset de la etiqueta considera el ÁNGULO de la cuña y el ANCHO del texto, no solo una distancia radial?
3. ¿Puede un marcador móvil (punto, flecha) coincidir con la posición de la etiqueta? Si sí, empujarla más allá del marcador.
4. **Verificación visual del caso EXTREMO a ALTA magnificación**: renderizar el ángulo mínimo Y el máximo (cuña estrecha Y cuña ancha) y los vectores más corto Y más largo; ampliar los recortes ≥×2.4 (las miniaturas ocultan toques marginales de 2–3 px — fue exactamente lo que pasó con el ángulo 70°). Leer los PNGs (Flujo B).

### 📅 Historial

| Fecha | Archivo | Causa | Fix | Resultado |
|-------|---------|-------|-----|-----------|
| 2026-06-28 | desplazamiento_avion_aeropuerto_..._n3_schoice_v1.Rmd | radio del label del ángulo solo dependía de `Lpx`; cuña de 30° muy estrecha para texto horizontal | radio = `max(34, (8+11·cos(semi))/sin(semi))` + esquive del punto según `Lpx` | 0 solapamientos en 333×4 combos; PDF/HTML OK |
| 2026-06-28 | (mismo) | piso `R_fit=34` insuficiente para ángulos grandes (70°): la recta casi horizontal clipaba el borde superior del label (la miniatura lo ocultó) | piso 34 → 50 (empuja el label por la bisectriz, holgura ~28 px) | 0 solapamientos en 40 semillas reales (áng. 30–70, 4 cuad., 20–200 km) con zoom ×2.4; 4 formatos OK |

### 📚 Referencias

- Función `dibujar_diagrama()` del ejercicio (chunk `data_generation`).
- Reglas `flujo-b-obligatorio.md` + `graficador-secuencial.md` (las 5 coherencias, coherencia visual).
- Incidente G (orquestador-schoice) / Incidente I (orquestador-cloze): verificación del caso extremo de parámetros en diagramas dinámicos.
- Memoria: `pendiente-solapamiento-diagramas-avion` (resuelto 2026-06-28).

## Error 24: Predictibilidad posicional de la respuesta correcta (cuadrante/posición fija)

### ❌ Síntoma

La opción correcta es fácil de adivinar por su **posición/orientación visual**, aunque su valor cambie entre versiones. Caso real (`desplazamiento_avion_aeropuerto_..._n3_schoice_v1`, 2026-06-28): la opción correcta (y las distractoras `recorrida`/`suma`) se dibujaban siempre en modo `"ne"` → la respuesta correcta **siempre aparecía en el primer cuadrante (NE)**. El estudiante aprende "la correcta apunta arriba-derecha" sin analizar distancia ni dirección.

### 🔍 Causa Raíz

Una dimensión de la respuesta correcta (posición, orientación, cuadrante, celda de grilla, altura relativa) está **fija o hardcoded**, mientras solo varía otra dimensión (el valor numérico). La diversidad por valor enmascara la predictibilidad posicional.

**Por qué el validador no lo atrapa**: `validar_diversidad_sustantiva.R` (regla #22) toma un *fingerprint del VALOR* de la respuesta correcta. Si el valor varía (aquí: la distancia), reporta `PASS` (39/40 únicos) **aunque la posición/orientación sea invariante**. Es un punto ciego: diversidad de valor ≠ diversidad posicional.

### ✅ Solución Verificada

Aleatorizar la orientación/posición GLOBAL de la escena por versión, aplicando la MISMA transformación a TODAS las opciones (preserva la estructura relativa correcta) y reflejándola en el texto:

```r
# Orientación global aleatoria → el cuadrante de la correcta varía (NE/NO/SE/SO)
orientaciones <- list(
  list(quad="NE", th_axis=90,  dir_sign=-1, eje="norte", lado="este"),
  list(quad="NO", th_axis=90,  dir_sign= 1, eje="norte", lado="oeste"),
  list(quad="SE", th_axis=270, dir_sign= 1, eje="sur",   lado="este"),
  list(quad="SO", th_axis=270, dir_sign=-1, eje="sur",   lado="oeste")
)
orient <- orientaciones[[sample(length(orientaciones), 1)]]
dir_desc <- paste0(angulo_direccion, "° al ", orient$lado, " del ", orient$eje)  # texto coherente
# th_line = th_axis + dir_sign*angulo  → la dirección cae en cualquier cuadrante
```

El texto del enunciado y de la Solution usan `dir_desc` (coherente con el cuadrante elegido).

**Calidad del distractor direccional (refinación, regla #22 §P5)**: el distractor de "dirección equivocada" NO debe ser un outlier obvio (giro de 180°, longitud única, otro cuadrante muy alejado), porque se elimina por percepción y no por razonamiento. Debe ser un **cuasi-acierto plausible**: un **reflejo respecto al eje vertical (lado opuesto este↔oeste) a la distancia correcta**. Misma magnitud, mismo ángulo, solo cambia el lado → obliga a verificar la dirección. (En el incidente, el distractor se llamaba "perpendicular" pero se dibujaba a 180°: el nombre debe describir el error real.)

```r
# Distractor de dirección = REFLEJO al lado opuesto del MISMO eje, a la distancia correcta (no 180°)
th_axis_espejo  <- orient$th_axis        # mismo eje (norte/sur)
dir_sign_espejo <- -orient$dir_sign      # lado opuesto (este<->oeste)
dibujar_diagrama("diagrama_perp.png", km(distancia_restante), distancia_restante, ..., th_axis_espejo, dir_sign_espejo)
```

### 🧪 Validación de la Solución

8+ renders reales → la respuesta correcta aparece en los 4 cuadrantes (NE/NO/SE/SO); el texto del enunciado coincide con el diagrama en cada versión; `validar_diversidad_sustantiva.R --n 40` → PASS (39/40); PDF/HTML compilan; sin solapamientos (Error 23 preservado en los 4 cuadrantes).

### 📋 Checklist de Corrección (generalizable)

1. ¿La posición/orientación/cuadrante/celda de la opción correcta es SIEMPRE la misma entre versiones?
2. ¿Solo varía el valor (número, distancia) pero no la ubicación visual?
3. Aleatorizar la dimensión posicional con la MISMA transformación para todas las opciones, y reflejarla en el texto.
4. **Verificación**: renderizar ≥8 versiones y confirmar que la correcta cambia de posición/orientación, no solo de valor. El `PASS` del validador de diversidad por valor NO es suficiente.

### 📅 Historial

| Fecha | Archivo | Causa | Fix | Resultado |
|-------|---------|-------|-----|-----------|
| 2026-06-28 | desplazamiento_avion_aeropuerto_..._n3_schoice_v1.Rmd | correcta siempre en cuadrante NE (modo `"ne"` fijo) | orientación global aleatoria (NE/NO/SE/SO) + texto `dir_desc` coherente + distractor en cuadrante opuesto | correcta en los 4 cuadrantes; diversidad PASS; sin solapes |
| 2026-06-28 | desplazamiento_avion_aeropuerto_..._n3_schoice_v1.Rmd | distractor de dirección = giro 180° a otra distancia (outlier obvio, eliminable de un vistazo) | reflejo este↔oeste a la **distancia correcta** (cuasi-acierto) + renombrar GEO-DES-01 "Dirección reflejada (lado opuesto del eje)" | distractor plenamente diagnóstico; 6 renders correcta-vs-espejo sin solapes; diversidad PASS (39/40) |

### 📚 Referencias

- Regla #22 `diversidad-sustantiva.md` § "P4: Predictibilidad posicional/orientacional" y § "P5: Distractor direccional/posicional como outlier obvio".
- Error 23 (solape de etiquetas) — mismo ejercicio, fix complementario.
- `graficos-como-opciones.md` § "Formato Equilibrado" — principio gemelo (el distractor no debe ser un outlier perceptual).
- Incidente F (orquestador-schoice) / Incidente H (orquestador-cloze): diversidad sustantiva ampliada a la dimensión posicional + calidad del distractor.

## Error 25: Fuga de la respuesta correcta por el NOMBRE DEL ARCHIVO en Moodle

### ❌ Síntoma

En un SCHOICE con opciones gráficas, los PNG de las opciones se guardaban con nombres **semánticos** (`diagrama_correcta.png`, `diagrama_perp.png`, `diagrama_recorrida.png`, `diagrama_suma.png`) en lugar de nombres neutrales por letra. Caso real (`desplazamiento_avion_aeropuerto_..._n3_schoice_v1`, 2026-07-28): al exportar con `exams2moodle()`, el XML resultante incluye literalmente `src="@@PLUGINFILE@@/diagrama_correcta.png"`. Un estudiante que use "Inspeccionar elemento" en el navegador (o revise el nombre del archivo descargado) lee directamente cuál opción es la correcta, sin necesidad de razonar sobre el contenido matemático.

**Matiz importante**: en `exams2html()` este defecto **NO ocurre**, porque R-exams incrusta las imágenes como `data:image/png;base64,...` — el nombre de archivo original no sobrevive al HTML autocontenido. El canal vulnerable es específicamente **Moodle** (y cualquier exportación que referencie archivos por nombre en vez de incrustarlos, p.ej. QTI). Por eso el defecto puede pasar completamente desapercibido si la validación visual solo se hace sobre HTML — el patrón visual FASE 2B (preview PDF→PNG) tampoco lo detecta, porque el PDF tampoco expone nombres de archivo al lector.

### 🔍 Causa Raíz

La función que genera y guarda los diagramas usaba el **rol semántico** de cada opción (correcta/distractor) como parte del nombre de archivo — por conveniencia de depuración durante el desarrollo — en lugar de renombrar cada PNG a una letra neutral (`diagrama_a.png`, `diagrama_b.png`...) **después** de la mezcla interna con `sample()`. La regla `graficos-como-opciones.md` ya exigía nombres neutrales por letra desde su v3.0 (2026-02-07), pero el ejercicio incumplía este requisito y ninguna capa de validación existente (hook, tests, detractor) comprobaba el XML de salida de `exams2moodle()` en busca de nombres semánticos filtrados — todas las verificaciones previas se centraban en HTML/PDF.

### ✅ Solución Verificada

Renombrar los PNG a nombres neutrales por letra **DESPUÉS** de la mezcla interna, y referenciar el archivo vía el campo de la lista mezclada (nunca el nombre original semántico) tanto en el Answerlist como en la Solution:

```r
# ❌ ANTES — nombres semánticos, delatan el rol en el XML de Moodle
dibujar_diagrama("diagrama_correcta.png", ...)
dibujar_diagrama("diagrama_perp.png", ...)
dibujar_diagrama("diagrama_recorrida.png", ...)
dibujar_diagrama("diagrama_suma.png", ...)
# Answerlist:
cat("* ![](diagrama_correcta.png){width=60%}\n")   # <- el nombre YA revela cuál es
```

```r
# ✅ DESPUÉS — mezcla interna primero, renombrado a letra neutral DESPUÉS
opciones_mezcladas <- sample(list(
  correcta  = list(archivo_tmp = "diagrama_correcta.png", ...),
  perp      = list(archivo_tmp = "diagrama_perp.png", ...),
  recorrida = list(archivo_tmp = "diagrama_recorrida.png", ...),
  suma      = list(archivo_tmp = "diagrama_suma.png", ...)
))
letras <- c("A", "B", "C", "D")
for (i in seq_along(letras)) {
  archivo_neutral <- paste0("diagrama_", tolower(letras[i]), ".png")
  file.rename(opciones_mezcladas[[i]]$archivo_tmp, archivo_neutral)
  opciones_mezcladas[[i]]$archivo <- archivo_neutral
}
names(opciones_mezcladas) <- letras
# Answerlist (usa el campo, nunca un literal semántico):
cat(paste0("* ![](", opciones_mezcladas[[l]]$archivo, "){width=60%}\n"))
```

### 🧪 Validación de la Solución

`exams2moodle("archivo.Rmd", n = 1)` seguido de un `grep` sobre el XML generado buscando cualquier nombre de archivo semántico:

```bash
grep -oE 'diagrama_[a-z]+\.png' moodle_output/*.xml | sort -u
```

Antes del fix: aparecían `diagrama_correcta.png`, `diagrama_perp.png`, `diagrama_recorrida.png`, `diagrama_suma.png` — el rol legible en texto plano dentro del XML. Después del fix: solo `diagrama_a.png`, `diagrama_b.png`, `diagrama_c.png`, `diagrama_d.png` (0 coincidencias de nombres semánticos), confirmado en múltiples semillas. `exams2html()` no mostró diferencia (ya usaba base64 antes y después).

### 📋 Checklist de Corrección (generalizable)

1. ¿Los PNG de opciones gráficas se guardan con nombre neutral (letra) o con el rol semántico (correcta/distractor/tipo de error)?
2. ¿El renombrado a letra ocurre DESPUÉS de la mezcla `sample()` (para que la letra asignada no sea predecible entre semillas)?
3. ¿El Answerlist y la Solution referencian el archivo vía el campo de la estructura mezclada (`opciones_mezcladas[[l]]$archivo`), nunca un literal con el nombre semántico original?
4. ¿Se verificó explícitamente con `exams2moodle()` + `grep` del XML? La validación en HTML/PDF **no es suficiente** — ambos canales ocultan el nombre de archivo original (base64 / incrustación directa).
5. ¿Aplica el mismo chequeo a otros metadatos no visuales que acompañen la opción (ver Error 26 y regla #22 §P6)?

### 📅 Historial

| Fecha | Archivo | Causa | Fix | Resultado |
|-------|---------|-------|-----|-----------|
| 2026-07-28 | desplazamiento_avion_aeropuerto_..._n3_schoice_v1.Rmd | PNGs nombrados por rol semántico (`diagrama_correcta.png`, etc.), visibles en texto plano dentro del XML de `exams2moodle()` | Renombrado a letra neutral (`diagrama_a.png`...) DESPUÉS de la mezcla interna; Answerlist/Solution referencian `opciones_mezcladas[[l]]$archivo` | 0 nombres semánticos en el XML de Moodle; HTML sin cambios (ya usaba base64) |

### 📚 Referencias

- Regla `graficos-como-opciones.md` (ya exigía nombres por letra desde v3.0; sección reforzada con el caso Moodle explícito en esta misma sesión).
- Regla #22 `diversidad-sustantiva.md` § "P6: Fuga de la respuesta por metadato no visual" (patrón gemelo, generalizado a cualquier canal de metadatos: nombre de archivo, orden alfabético, id del elemento).
- Error 17 / Error 19 (Solution con letra hardcoded) — mismo principio: el contenido accesible al estudiante no debe depender de un artefacto filtrable ajeno al razonamiento matemático.

## Error 26: Diagrama degenerado por escala relativa (vector casi nulo)

### ❌ Síntoma

En un diagrama dinámico donde la escala de dibujo se calcula en función de la SUMA de dos magnitudes de la escena (`escala_px_km <- 120/(distancia_total + distancia_avanzada)`), cuando una magnitud individual (`distancia_restante = distancia_total - distancia_avanzada`) es pequeña respecto a la otra, el vector correspondiente se dibuja con apenas **~17 px** sobre un lienzo de 460 px: el marcador queda pegado al origen, la dirección deja de ser legible, y la etiqueta de la distancia (p.ej. "20 km") — sujeta al piso `rtext <- max(Lpx, 58)` — queda flotando a 58 px del origen, sugiriendo visualmente una distancia mayor que la representada. Caso real (`desplazamiento_avion_aeropuerto_..._n3_schoice_v1`, 2026-07-28). Frecuencia **medida**: 2/37 combinaciones válidas (5,4%) en enumeración exhaustiva de un ángulo dado; 4/60 (6,7%) en muestreo aleatorio amplio.

### 🔍 Causa Raíz

La escala de dibujo (px por km) se calculaba en función de la magnitud TOTAL de la escena (`distancia_total + distancia_avanzada`), pero el filtro de generación de parámetros solo garantizaba **validez matemática** (`distancia_restante > 0`), no **legibilidad visual**. Ninguna condición impedía que `distancia_restante` fuera arbitrariamente pequeña en proporción a la escena completa, produciendo vectores de pocos píxeles que el ojo humano no puede interpretar como una dirección ni una magnitud fiable — y cuyo piso de etiqueta (pensado para evitar solapes, ver Error 23) agravaba el problema al desconectar visualmente la etiqueta del vector real.

### ✅ Solución Verificada

Condición de proporción mínima en el filtro de generación de parámetros, exigiendo que la magnitud menor de la escena sea al menos una fracción `f` de la magnitud total. El umbral `f = 0.25` se eligió por **barrido empírico** (no arbitrario):

| `f` | Vector mínimo resultante | Combinaciones conservadas | Distancias distintas |
|-----|---------------------------|---------------------------|-----------------------|
| 0.20 | ~24 px (insuficiente, cerca del piso) | — | — |
| **0.25** | **~30 px** | **34/37** | **10** |
| 0.30 | ~44 px | 31/37 (pierde 6) | menos variedad |

```r
# ❌ ANTES — sin restricción de legibilidad, solo validez matemática
distancia_restante <- distancia_total - distancia_avanzada
stopifnot(distancia_restante > 0)   # matemáticamente válido, pero puede ser ilegible en píxeles

# ✅ DESPUÉS — condición de proporción mínima (f = 0.25, elegido por barrido)
f <- 0.25
combinaciones_validas <- combinaciones_validas[
  (combinaciones_validas$distancia_total - combinaciones_validas$distancia_avanzada) >=
    f * (combinaciones_validas$distancia_total + combinaciones_validas$distancia_avanzada)
, ]
```

Complementariamente, se añadió una línea guía punteada cuando el piso de la etiqueta la separa visiblemente del marcador (mismo mecanismo de piso descrito en el Error 23):

```r
# Línea guía punteada cuando la etiqueta se aleja del marcador por el piso rtext
if (rtext - Lpx > 15) {
  segments(x_marcador, y_marcador, x_etiqueta, y_etiqueta, lty = "dotted", col = "gray50")
}
```

### 🧪 Validación de la Solución

Barrido de `f` en {0.20, 0.25, 0.30} sobre la grilla COMPLETA de combinaciones válidas por ángulo, midiendo (a) tamaño en píxeles del vector más pequeño resultante y (b) número de combinaciones descartadas. `f = 0.25` fue el punto de mejor balance: elimina los vectores degenerados (<30 px) conservando 34/37 combinaciones y 10 distancias distintas (frente a solo 31/37 con `f = 0.30`). Verificado que tras el fix no quedan vectores por debajo del piso legible; la diversidad sustantiva (regla #22) se preservó.

### 📋 Checklist de Corrección (generalizable)

1. ¿La escala de dibujo depende de la magnitud TOTAL de la escena en vez de garantizar un tamaño mínimo por elemento individual?
2. ¿Existe un piso de tamaño en píxeles explícito para el vector/marcador más pequeño posible dado el rango completo de parámetros?
3. ¿El umbral de proporción mínima (`f`) se determinó mediante barrido empírico (midiendo px resultantes Y combinaciones descartadas), no eligiendo un valor arbitrario?
4. ¿Cuando el piso de la etiqueta la separa del marcador, existe una guía visual (línea punteada) que conecte ambos para no sugerir una magnitud falsa?
5. ¿Se verificó con ENUMERACIÓN EXHAUSTIVA (no solo muestreo aleatorio) la frecuencia de combinaciones degeneradas, antes y después del fix?

### 📅 Historial

| Fecha | Archivo | Causa | Fix | Resultado |
|-------|---------|-------|-----|-----------|
| 2026-07-28 | desplazamiento_avion_aeropuerto_..._n3_schoice_v1.Rmd | escala global (`120/(distancia_total+distancia_avanzada)`) sin piso de legibilidad por vector individual; `distancia_restante` pequeña → vector ~17 px, etiqueta flotante | Filtro de proporción mínima `(distancia_total - distancia_avanzada) >= 0.25*(distancia_total + distancia_avanzada)` (f=0.25 por barrido) + línea guía punteada cuando `rtext - Lpx > 15` | 0 vectores degenerados; 34/37 combinaciones conservadas; 10 distancias distintas |

### 📚 Referencias

- Función `dibujar_diagrama()` / filtro de generación de parámetros del ejercicio (chunk `data_generation`).
- Error 23 (solape de etiquetas por cuña angular) — mismo ejercicio, defecto complementario de legibilidad geométrica (ambos derivan del mismo piso `rtext`/`R_fit`).
- Regla #22 `diversidad-sustantiva.md` — la legibilidad visual de cada elemento no debe sacrificarse al maximizar el número de combinaciones matemáticamente válidas.

## Error 27: Pool de errores conceptuales del mismo tamaño que el número de distractores

### ❌ Síntoma

No hay ningún mensaje de error. El ejercicio pasa TODO el arsenal en verde: `validar_coherencia_matematica.R` reporta APROBADO 0 errores (incluidas las Capas A/B/C de validación semántica y el Nivel 5A-5E de correctitud de respuesta), `validar_diversidad_sustantiva.R` sale con exit 0, y el render compila sin fallos en los 4 formatos. Y aun así, el **tipo** de error conceptual que ve el estudiante es idéntico en el 100 % de las versiones: entre semilla y semilla solo cambia el valor numérico sustituido, nunca cuál distractor conceptual aparece.

### 🔍 Causa Raíz

`errores_conceptuales` se declaraba con exactamente tantas entradas como distractores tiene el ítem (3 para un SCHOICE de 4 opciones), y las tres se usaban siempre, sin ningún `sample()` sobre el pool. La diversidad del render puede seguir siendo alta por otras vías (contextos narrativos, mezcla de opciones, reflexiones metacognitivas), lo que enmascara por completo la pobreza real del pool de errores.

**Por qué ningún validador lo detecta**:
- `validar_diversidad_sustantiva.R` (regla #22) mide la variación del **valor** de la respuesta correcta entre versiones, no la variación del **tipo** de distractor conceptual seleccionado.
- `validar_coherencia_matematica.R` valida cada error individualmente (que su `precondicion` se cumpla, que `calcula()` sea determinista, que el distractor difiera de la respuesta correcta), pero nunca evalúa **cuántas entradas** tiene el pool completo ni si se está muestreando un subconjunto de él.
- La Capa B (escáner de 21 keywords semánticas) cubre propiedades de conjuntos de datos estadísticos — paridad, cuartiles, outliers, modalidad. En dominios de **combinatoria** no existe ninguna regla aplicable: su APROBADO no certifica la corrección conceptual del pool, es simplemente un dominio fuera de su cobertura. Es un punto ciego del validador por dominio, no una garantía universal.

### ✅ Solución Verificada

Ampliar el pool más allá del número de distractores y seleccionar un subconjunto por versión con `sample()` sobre los índices que cumplen su `precondicion`:

```r
# ❌ ANTES — pool del mismo tamaño que los distractores, sin sample()
errores_conceptuales <- list(
  list(codigo = "COMB-PER-01", ...),
  list(codigo = "COMB-PER-02", ...),
  list(codigo = "COMB-PER-03", ...)
)
vals <- vapply(errores_conceptuales, function(e) e$calcula(n), numeric(1))
```

```r
# ✅ DESPUÉS — pool ampliado (5) + selección aleatoria de 3 por versión
errores_conceptuales <- list(
  list(codigo = "COMB-PER-01", precondicion = function(p) TRUE, ...),
  list(codigo = "COMB-PER-02", precondicion = function(p) TRUE, ...),
  list(codigo = "COMB-PER-03", precondicion = function(p) TRUE, ...),
  list(codigo = "COMB-PER-04", precondicion = function(p) TRUE, ...),
  list(codigo = "COMB-PER-05", precondicion = function(p) TRUE, ...)
)
aplicables <- which(sapply(errores_conceptuales, function(e) e$precondicion(list(n = n))))
sel <- sort(safe_sample(aplicables, 3L, replace = FALSE))
errores_sel <- errores_conceptuales[sel]
```

**Excepción canónica**: cuando la versión debe reproducir verbatim un ítem oficial del cuadernillo ICFES, se fuerzan los distractores oficiales en lugar de sortear el pool:

```r
# Excepción: si la versión es la instancia canónica del ítem original (contexto 1, n=4),
# se fuerzan los códigos oficiales del cuadernillo en vez de sortear
es_canonica <- (ctx_idx == 1L && n == 4L)
if (es_canonica) {
  sel <- which(sapply(errores_conceptuales, function(e) e$codigo) %in%
               c("COMB-PER-01", "COMB-PER-02", "COMB-PER-04"))
} else {
  sel <- sort(safe_sample(aplicables, 3L, replace = FALSE))
}
all_vals <- c(24L, 64L, 16L, 4L)  # clave + 3 distractores oficiales del cuadernillo
stopifnot(setequal(all_vals, c(24L, 64L, 16L, 4L)))
```

### 🧪 Validación de la Solución

Mediciones reales del caso que motivó este error (`permutaciones-pescadores-venia-n4`, 2026-07-29):

| Métrica | Antes | Después |
|---|---|---|
| Entradas del pool | 3 | 5 |
| Ternas de error distintas alcanzadas en 300 versiones | 1 | 10 de 10 posibles |
| Versiones únicas en 300 evaluaciones | 280/300 | 297/300 |
| Rango de la respuesta correcta por magnitud | siempre el 3.º | 3.º o 4.º |
| Combinaciones verificadas por el verificador | 3 valores del parámetro | 30 (3 valores × C(5,3)=10), enumeración exhaustiva |

Tras el cambio: `validar_coherencia_matematica.R` → APROBADO 0 errores; `verificar_render.R` → V1-V8 todo verde; `validar_diversidad_sustantiva.R --n 40` → exit 0.

### 📋 Checklist de Corrección (generalizable)

1. ¿El número de entradas de primer nivel de `errores_conceptuales` es igual al número de distractores del ítem?
2. ¿Existe un `sample()` (o `safe_sample()`) sobre los índices aplicables antes de fijar los errores usados en la versión, o se usa siempre el pool completo?
3. Si el ítem debe reproducir un cuadernillo ICFES verbatim, ¿está declarada explícitamente la excepción canónica con su propio `stopifnot` de verificación?
4. Tras ampliar el pool, ¿se re-enumeró el espacio COMPLETO de combinaciones (pool × slots × valores del parámetro), verificando unicidad de opciones y coherencia de la razón máx/clave?
5. ¿Se re-ejecutó el arsenal completo (coherencia matemática, diversidad sustantiva, render 4 formatos) después de ampliar el pool?

### 📅 Historial

| Fecha | Archivo | Causa | Fix | Resultado |
|-------|---------|-------|-----|-----------|
| 2026-07-29 | permutaciones_pescadores_metacognitivo_formulacion_n4_schoice_v1.Rmd | pool de 3 errores == número de distractores, sin `sample()` — detectado por auditoría adversarial | pool ampliado a 5 + `sample()` sobre índices aplicables + excepción canónica para el ítem oficial verbatim | 10/10 ternas alcanzadas, 297/300 versiones únicas, arsenal completo APROBADO |

### 📚 Referencias

- Regla #1 `ejercicios-metacognitivos.md` (línea 188 — «Mínimo 4-6 errores por ejercicio», sección OBLIGATORIA «Pool de Errores Conceptuales»).
- Regla #22 `diversidad-sustantiva.md` — mide diversidad de VALOR, no de TIPO de distractor; punto ciego complementario a este error.
- Incidente N de `.claude/agents/orquestador-schoice.md` / Incidente P de `.claude/agents/orquestador-cloze.md`.
- `A-Produccion/02-En-Desarrollo/permutaciones-pescadores-venia-n4/.claude/rules/permutaciones-parametricas.md`.

---

## Error 28: La exclusión por texto solo cubre la clave VIGENTE cuando el ítem tiene dos claves mutuamente excluyentes

### ❌ Síntoma

El estudiante ve **dos opciones que afirman el mismo rango numérico con veredictos opuestos**: una marcada correcta y otra incorrecta. Sin mensaje de error: el render compila, la unicidad textual pasa (los textos difieren), el balance 2 Sí + 2 No se cumple y el arsenal completo da verde.

### 🔍 Causa Raíz

El ítem implementa la defensa de la regla #22 §P4-bis con **dos claves mutuamente excluyentes**: una de veredicto «No» cuando la afirmación del enunciado es falsa, y una alternativa de veredicto «Sí» cuando es verdadera. La guarda anti-colisión comparaba cada candidato contra `txt_clave`, es decir contra la clave **vigente en esa versión**:

```r
# ❌ ANTES — solo cubre la rama cuya clave comparte plantilla con el distractor
txt_clave <- errores_conceptuales[[idx_ok]]$descripcion_corta
sin_colision <- function(idx) idx[vapply(idx, function(i)
  errores_conceptuales[[i]]$descripcion_corta != txt_clave, logical(1L))]
```

Funciona en la rama donde la clave y el distractor comparten plantilla. En la otra rama la clave se redacta distinto, el literal ya no coincide y el distractor **pasa el filtro afirmando el rango correcto con el veredicto contrario**. El resultado es una opción que se contradice a sí misma (declara «No» mientras su justificación reafirma lo afirmado), lo que además la vuelve descartable por absurda.

La clave NO vigente es la firma exacta de la colisión: comparte plantilla con el distractor, así que cuando los rangos coinciden sus textos son **idénticos**.

### ✅ Solución Verificada

```r
# ✅ DESPUÉS — comparar contra AMBAS claves, vigente y excluida
txt_claves <- c(errores_conceptuales[[idx_ok_no]]$descripcion_corta,
                errores_conceptuales[[idx_ok_si]]$descripcion_corta)
sin_colision <- function(idx) idx[vapply(idx, function(i)
  !(errores_conceptuales[[i]]$descripcion_corta %in% txt_claves), logical(1L))]
```

### 🧪 Validación de la Solución

Enumeración de 600 versiones + prueba de mutación con contrato de sonda (Incidente P):

- Sano: 0 violaciones sobre 600 versiones.
- Mutante M1 (revertir a comparar solo contra la clave vigente): la mutación llegó al **entorno** (`txt_claves` colapsa a un valor único) y murió por **su propia sonda**, `I6_rango_clave`, con 3 casos y sin ruido de otras sondas → `cazado_por_su_sonda`.
- Los 3 casos son exactamente las combinaciones con `(pmin + pmax) · pa == 1`.

### 📋 Checklist de Corrección

1. ¿El ítem tiene más de una clave posible (defensa §P4-bis)? Si sí, toda guarda anti-colisión debe recorrer **todas** las claves, no la vigente.
2. ¿La guarda compara por lo que el estudiante LEE, no por código de error?
3. ¿Hay una prueba de mutación que revierta la guarda y muera por su propia sonda?

### 📚 Referencias

- Regla #22 `diversidad-sustantiva.md` §P4-bis; Incidente F (`INC-DIV-COSMETICA`) y P (`INC-MUTANTE-SONDA`) de `orquestador-schoice.md`.
- Memoria `feedback_colision_textual_distractor_clave.md` — este error es su continuación: la colisión no era solo textual.

---

## Error 29: La clave alternativa de §P4-bis reabre INC-SINO-BINARIO en distractores escritos para una sola clave

### ❌ Síntoma

Una opción declara «Sí» (endosa la afirmación del enunciado) mientras su justificación afirma **un rango distinto del afirmado**. Es decir, su justificación apoya la conclusión contraria a la que declara. Todo el arsenal en verde.

### 🔍 Causa Raíz

Los distractores se escribieron cuando la afirmación del enunciado era **siempre** el rango del complemento lineal, que es justamente lo que ellos afirman: eran coherentes por construcción. Al introducir la clave alternativa para que el veredicto no sea invariante (regla #22 §P4-bis), la afirmación pasa a ser en la mitad de las versiones el rango **correcto**, y esos distractores quedan diciendo «Sí, porque [otro rango]».

Es la lección general: **una defensa nueva puede invalidar la premisa sobre la que se escribió el pool existente**. §P4-bis cambia qué significa el enunciado, no solo cuál opción es la clave.

### ✅ Solución Verificada

Declarar explícitamente qué opciones afirman un rango propio y excluirlas de la rama donde ya no son coherentes:

```r
# en cada entrada "si" del pool
veredicto = "si",
afirma_rango_area = TRUE,   # o FALSE si solo enuncia un método, sin rango propio

# en la selección
if (afirmacion_es_verdadera) {
  idx_si <- idx_si[!vapply(idx_si, function(i)
    isTRUE(errores_conceptuales[[i]]$afirma_rango_area), logical(1L))]
}
```

### 🧪 Validación de la Solución

Sonda **auto-verificada** antes de usarla (extrae el rango de la ÚLTIMA construcción `entre X % y Y %`, porque la primera puede ser lo que la opción dice que *ocupa*, no lo que queda libre) y con **control negativo** (al revertir el fix del Error 28 vuelve a detectar sus 3 casos):

| | antes | después |
|---|---|---|
| Opciones cuya justificación contradice su conclusión | **81 / 600 (13,5 %)** | **0 / 600** |

Una primera versión de esta sonda daba 280 falsos positivos por tomar los dos primeros números del texto. Sin el control negativo, un «0 incoherencias» no distingue «no hay defecto» de «la sonda no mide».

### ⚠️ Efecto colateral — leer el Error 30

Este fix **desplazó el defecto de canal**. Ver Error 30 antes de aplicarlo tal cual.

### 📚 Referencias

- Incidente D (`INC-SINO-BINARIO`) de `orquestador-schoice.md`; regla #22 §P4-bis.

---

## Error 30: La sonda de diagnosticidad agrega sobre versiones sin condicionar por rama

### ❌ Síntoma

`validar_diagnosticidad.R` reporta `PASS` (H1 en torno al 40-50 %, umbral 70 %) y sin embargo, **dentro de una de las ramas estructurales del ítem, la clave es identificable en el 100 % de las versiones** por una señal de superficie.

### 🔍 Causa Raíz

Un ítem con clave alternante (regla #22 §P4-bis) tiene **dos ramas estructuralmente distintas**. Las sondas H1/H2 promedian sobre todas las versiones sin condicionar por rama, así que un reparto 100 % / 0 % se lee como ~50 % y queda por debajo del umbral. Es el mismo punto ciego que dio origen a la sonda H3: un patrón que solo existe *entre* versiones no lo ve una sonda que mira *cada* versión.

**Agravante — el defecto lo introdujo el fix del Error 29.** El distractor excluido de la rama verdadera era el único más largo que la clave; al quitarlo, la clave quedó siendo **determinísticamente** la opción más larga de esa rama.

### 📊 Medición

Medido de forma independiente por dos auditores con semillas distintas (concordancia dentro del ruido de muestreo):

| | rama verdadera | rama falsa |
|---|---|---|
| clave = única opción más larga | **100 %** | 0 % |
| token del procedimiento identifica la clave | **100 %** | — |

Acierto **sin razonar** (azar = 25 %): heurística «la más larga» **50,5 %**; heurística «la que nombra el procedimiento» **62,9 %**.

Longitudes medianas del pool: clave alternativa 125 caracteres; su rival más largo disponible en esa rama, 99.

### ✅ Tratamiento

1. **Medir condicionando por rama**, no solo el agregado del validador. Mientras `validar_diagnosticidad.R` no lo soporte, es verificación manual: agrupar por el flag que define la rama y recalcular H1/H2 dentro de cada grupo.
2. Al igualar la extensión de las opciones, comprobar que no se crea la **señal inversa**: si la clave pasa a no ser NUNCA la más larga, «descartar la más larga» se convierte en una heurística de eliminación que sube el azar de 25 % a 33 %.
3. Regla general: **un fix de diagnosticidad puede desplazar el defecto de canal** (de la semántica a la longitud, de la longitud al léxico). Medir el ítem completo después de cada fix, no solo la dimensión que se corrigió.

### 📚 Referencias

- Regla #22 `diversidad-sustantiva.md` §P4-bis; `validar_diagnosticidad.R` (sondas H1/H2/H3).
- Error 29 — el fix que lo introdujo.

---

## Error 31: `validar_multisemilla.R` falla siempre bajo `Rscript` — la guarda de su fallback es código inalcanzable

### ❌ Mensaje de Error

```
Error en sys.frame(1): no hay tantas estructuras en la pila
Calls: dirname -> sys.frame
Ejecución interrumpida
```

### 🔍 Causa Raíz

`.claude/scripts/validar_multisemilla.R`, línea 21:

```r
script_dir <- dirname(sys.frame(1)$ofile)
if (is.null(script_dir) || script_dir == "") {   # <- INALCANZABLE
  ...fallback por rutas conocidas...
}
```

Bajo `Rscript` no existe el frame 1, así que `sys.frame(1)` **lanza un error** antes de que `dirname()` devuelva nada: la comprobación `is.null()` de la línea siguiente nunca llega a evaluarse. El fallback existe, está bien escrito y es **código muerto**.

Es el mismo modo de fallo que la «guarda inalcanzable» ya documentada en memoria: una condición escrita para un valor que nunca llega porque la expresión revienta primero.

### 📊 Alcance

Verificado que el fallo es **del script, no del ejercicio**:

| Invocación | exit |
|---|---|
| con el `.Rmd` auditado | 1 |
| con un ejemplo canónico intacto de `Ejemplos-Funcionales-Rmd/` | 1 |
| sin argumentos | 1 |

`post-exams2-validation.sh` (FASE 2G) lo invoca exactamente así:

```bash
MULTISEED_OUTPUT=$(cd "$CWD" && Rscript "$SCRIPT_MULTISEMILLA" "$RMD_FILE" --n 20 2>&1)
```

y luego incrementa `ERRORES_TOTALES` si el exit no es 0. La consecuencia es un **falso ROJO permanente**: la FASE 2G suma un error en todo ejercicio del repositorio. Un gate que siempre falla es un gate que se aprende a ignorar.

### ✅ Solución Verificada (aplicada 2026-08-09)

El archivo real es `SOURCES/scripts_validacion/validar_multisemilla.R`; `.claude/scripts/` solo
contiene un **symlink** (modo `120000` en git). Editar la ruta de `.claude/scripts/` no funciona.

La resolución de la propia ruta pasa a cuatro pasos por orden de fiabilidad, cada uno aislado:

```r
.resolver_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)          # 1. Rscript
  hit <- grep("^--file=", args, value = TRUE)
  if (length(hit) > 0) {
    d <- tryCatch(dirname(normalizePath(sub("^--file=", "", hit[1]), mustWork = TRUE)),
                  error = function(e) "")
    if (length(d) == 1L && nzchar(d)) return(d)
  }
  d <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) "")   # 2. source()
  if (!is.null(d) && length(d) == 1L && nzchar(d)) return(d)
  ""
}
```

más `git rev-parse --show-toplevel` (3) y las rutas relativas conocidas (4) dentro de
`.cargar_dependencia()`, que **aborta con `stop()`** si ninguna candidata existe.

**Segundo defecto corregido a la vez**: la versión anterior recorría las rutas relativas y, si
ninguna existía, terminaba el bucle **sin cargar nada** y continuaba. El fallo aparecía mucho
después como «no se pudo encontrar la función», que no apunta a la causa.

### 🧪 Validación de la Solución

Los cuatro modos de invocación, con el exit real medido sin tuberías que lo enmascaren:

| Invocación | Antes | Después |
|---|---|---|
| sin argumentos | exit 1 (crash) | **exit 2** + imprime el uso (contrato declarado en su cabecera) |
| desde la raíz, por el symlink | exit 1 | **APROBADO** |
| desde un cwd ajeno (`/tmp`) | exit 1 | **APROBADO** |
| vía `source()` | — | función disponible |

**Prueba de mutación sobre una COPIA en `/tmp`**, no sobre el archivo del repo: el mutante con el
patrón viejo sale con exit 1 y su salida contiene `sys.frame`, así que ambas aserciones del test
disparan. *(Lección aparte: la primera vez se mutó el archivo real y el paso de restaurar quedó en
un job en segundo plano — durante unos minutos el repo tuvo en disco el validador roto. Mutar
siempre una copia.)*

### 🛡️ Defensa permanente

`tests/testthat/test_validar_multisemilla_invocable.R` (enganchado al runner, suite crítica):

1. **Barrido de todo el arsenal** — ningún `.R` de `.claude/scripts/` ni de
   `SOURCES/scripts_validacion/` puede usar `sys.frame(<literal>)` fuera de un `tryCatch`.
2. **Invocabilidad real** bajo `Rscript` desde un `tempdir()`, por el symlink y por la ruta real:
   la salida no contiene `sys.frame`, el exit es 2 y aparece el uso.
3. **Contrato del fix**: el fuente declara `--file=`, `tryCatch` y el mensaje de aborto.

**El detector es del índice LITERAL, no de `sys.frame` a secas.** La primera versión marcaba
cualquier `sys.frame(`, y daba un **falso positivo** en `stress_test_visual.R:34`, que usa
`sys.frame(i)` dentro de `for (i in seq_len(sys.nframe()))` — bajo `Rscript` el cuerpo simplemente
no se ejecuta, y además prueba `--file=` primero. Ese script es correcto: sale con 2, no con 1.

### 📋 Checklist

1. Toda resolución de la propia ruta bajo `Rscript` debe ir en `tryCatch`, o resolverse desde `commandArgs()` (como hace `validar_diagnosticidad.R`, que no tiene el problema).
2. Una guarda cuya condición se evalúa DESPUÉS de la expresión que puede reventar no es una guarda.
3. Al leer la salida de un hook, distinguir «falló» de «no se pudo ejecutar».

### 📅 Historial

| Fecha | Componente | Causa | Estado |
|-------|-----------|-------|--------|
| 2026-08-09 | `SOURCES/scripts_validacion/validar_multisemilla.R` | `sys.frame(1)` revienta antes de la guarda `is.null()`; y el bucle de rutas relativas podía terminar sin cargar nada | **RESUELTO** — resolución en 4 pasos + aborto explícito + `test_validar_multisemilla_invocable.R` en el runner. FASE 2G deja de ser un falso ROJO |

---

## Error 32: Un fix de coherencia introduce una fuga LÉXICA, y ninguna sonda del arsenal la ve

### ❌ Síntoma

Se corrige un defecto semántico sustituyendo cadenas homogéneas por justificaciones redactadas en lenguaje natural, una por cada caso. El arsenal completo sigue en verde —coherencia `APROBADO`, `validar_diagnosticidad.R` `PASS`, diversidad, ortografía, todos los formatos— y sin embargo el ítem pasa a resolverse **sin leer el enunciado**, solo comparando las palabras de las cuatro opciones.

Caso medido (`area-jardin-lote-porcentaje-n4/cloze`, 2026-08-09): **88,4 % de acierto frente al 25 % de azar**, peor que el defecto §P4-bis que el diseño ya vigilaba (que daría 50 %).

### 🔍 Causa Raíz

Al redactar cada justificación «a su aire» se cuelan **regularidades léxicas y gramaticales que correlacionan con el rol de la opción**:

1. **Token exclusivo de la clave.** Solo la justificación del método correcto contenía la palabra «jardín», y ese método nunca es distractor. Consecuencia doble: la opción con «jardín» era la clave en el 100 % de una rama, y su **ausencia** anunciaba que la otra rama estaba activa — la presencia del token predecía la rama en 800/800 versiones.
2. **Forma gramatical que separa clave de distractores.** Cuatro de las seis justificaciones erróneas eran prescriptivas («hay que sumar…», «basta restar…») y la correcta declarativa («el área **es** el producto…»). «Entre los dos Sí, elige el declarativo» acertaba el 100 % de las veces en que aplicaba.
3. **Sesgo algebraico del pool**, que el fix no creó pero cuyo peso aumentó: para `a, b ∈ (0,1)` se cumple `1-ab > max{1-a, 1-b, (1-a)(1-b), 1-(a+b)/2}` y `1-ab < (1-a)+(1-b)`. Es decir, 4 de 6 métodos erróneos producen **siempre** un rango menor que el correcto, 1 siempre mayor y 1 mixto: «elige el rango mayor» acertaba el 77 %.

**Por qué el arsenal no lo detecta:** `validar_diagnosticidad.R` calcula `pw <- primera palabra de cada opción`. Con opciones que empiezan por «Sí»/«No», **H2 mide exactamente eso y da 0 %**, y H3 mide invariancia del veredicto, que estaba equilibrada. **Ninguna sonda inspecciona los tokens del cuerpo de la opción.** Es el mismo punto ciego que la regla #22 §P6 generaliza —«cualquier metadato que revele el rol de la opción»—, solo que aquí el metadato es léxico en vez de un nombre de archivo.

### ✅ Solución Verificada

**Molde único y paralelo**: todas las justificaciones comparten sujeto, verbo y vocabulario, y se diferencian **solo en la operación**, que es lo que el ítem evalúa.

```r
# ❌ ANTES — cada una a su aire: "jardín" solo en la correcta, y 4 prescriptivas
producto   = "el área del jardín es el producto de sus dos fracciones",
lineal     = "basta restar del 100 % el porcentaje del largo",
suma       = "hay que sumar el complemento del largo y el del ancho",
```

```r
# ✅ DESPUÉS — mismo molde "el área sin jardín es …", todas declarativas
producto   = "el área sin jardín es el complemento del producto de las dos fracciones",
lineal     = "el área sin jardín es el complemento de la fracción del largo",
suma       = "el área sin jardín es la suma de los complementos de las dos fracciones",
```

Y **estratificar por el lado del rango** cuando el pool está algebraicamente sesgado: sortear primero si el distractor irá por encima o por debajo del valor correcto, y elegir el método después.

### 🧪 Validación de la Solución

| medida (800 versiones) | antes | después |
|---|---|---|
| la presencia de un token predice la rama | 100 % | 48,8 % (= azar) |
| «entre los dos Sí, elige el declarativo» | 100 % | N/A: ya no hay dos formas |
| rama «No»: la clave es el rango mayor | 77,3 % | 54,6 % |
| **acierto sin leer el enunciado** | **88,4 %** | **24,2 %** |

### 📋 Prueba de aceptación (ejecutable)

Ningún token de más de 2 caracteres puede ser exclusivo de la clave en ≥70 % de las versiones, **ni dentro de cada rama por separado**:

```r
tok <- function(t) unique(tolower(unlist(strsplit(
  gsub("[^[:alnum:]áéíóúñü ]", " ", t), " +"))))
# por versión: setdiff(tok(clave), unlist(lapply(distractores, tok)))
```

En el caso corregido devuelve el conjunto vacío; el token más frecuente («producto») baja al 31 %.

### ⚠️ Lección transferible

**Al sustituir cadenas homogéneas por texto redactado, se cambia un eje de variación por otro.** Antes de dar por bueno el fix hay que medir el ítem **completo**, no solo la dimensión corregida: es la tercera vez en la misma sesión que un fix desplaza el defecto de canal (semántica → longitud → léxico). Y `PASS` de `validar_diagnosticidad.R` **no** acredita ausencia de fuga léxica: sus sondas miran la primera palabra y la longitud, nunca el vocabulario.

### 📅 Historial

| Fecha | Archivo | Causa | Fix | Resultado |
|-------|---------|-------|-----|-----------|
| 2026-08-09 | `area_jardin_lote_..._n4_cloze_v1.Rmd` | justificaciones redactadas sin molde común tras corregir `INC-SINO-BINARIO` | molde declarativo paralelo + estratificación del distractor por lado del rango | acierto sin razonar 88,4 % → 24,2 % |

### 📚 Referencias

- Errores 29 y 30 (el fix que lo originó y el desplazamiento de canal previo).
- Regla #22 §P6 — el principio general: ningún rasgo que revele el rol de la opción.
- Haladyna, Downing & Rodriguez (2002), *Applied Measurement in Education* 15(3):309-334 — «Avoid giving clues to the right answer»: *word repeats* entre enunciado y clave.
