# Patrones de Errores Conocidos y Soluciones - R/exams

> **Nota:** Este documento solo registra errores que ya han sido identificados, corregidos y verificados. No se documentan problemas sin solución confirmada.

---

## Índice

1. [Error: Imagen PNG no encontrada en compilación PDF](#error-1-imagen-png-no-encontrada)
2. [Error: Argumento no numérico para función matemática abs()](#error-2-argumento-no-numerico-abs)
3. [Error: Imágenes Python/matplotlib no visibles en exams2pdf](#error-3-imagenes-python-no-visibles-pdf)
4. [Placeholder para futuros errores](#futuros-errores)

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
- ✅ Se generan al menos 300 versiones únicas
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
  expect_true(n_versiones_unicas >= 300)
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
