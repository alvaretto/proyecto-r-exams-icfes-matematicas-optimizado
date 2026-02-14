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

---

## Error 1: Imagen PNG no encontrada en compilación PDF

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

#### Código DESPUÉS (correcto):

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

### 📋 Checklist de Corrección

- [ ] Identificar chunks que usan `include_tikz()`
- [ ] Mover `include_tikz()` fuera del chunk de generación
- [ ] Crear chunk de renderizado condicional con `knitr::is_latex_output()`
- [ ] Para LaTeX: usar `cat()` para insertar código TikZ directamente
- [ ] Para HTML: mantener `include_tikz()`
- [ ] Verificar compilación a PDF
- [ ] Verificar compilación a HTML
- [ ] Confirmar visualización correcta en ambos formatos

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
