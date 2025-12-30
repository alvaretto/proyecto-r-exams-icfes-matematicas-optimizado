---
description: Genera código R (ggplot2) para la imagen matemática, nativo para R-exams.
---

# Generar Código R

Genera código R completo usando ggplot2, **optimizado nativamente para integración directa con R-exams**.

## Estructura Base para R-exams

El código R es el más nativo para R-exams y debe generarse pensando en su uso directo dentro de archivos `.Rmd`:

```r
# ============================================
# CÓDIGO R PARA R-EXAMS (NATIVO)
# Archivo: output_r.R
# ============================================
# INSTRUCCIONES DE USO EN R-EXAMS:
#
# 1. Incluir directamente en chunk R del archivo .Rmd:
#    ```{r grafico, echo=FALSE, fig.height=6, fig.width=9}
#    source("output_r.R")
#    print(p)  # p es el objeto ggplot
#    ```
#
# 2. O copiar el código directamente al chunk
#
# 3. Para variantes, modificar las variables marcadas con # PARAM
#
# ============================================

library(ggplot2)
library(scales)

# Tu código aquí

ggsave("output_r.png", p, width = 9, height = 6, dpi = 150)
```

## Consideraciones para R-exams

### 1. Compatibilidad con exams2pdf/exams2html

- **Usar ggplot2**: Es el estándar para gráficos en R-exams
- **Evitar dependencias innecesarias**: Solo ggplot2, scales, dplyr si es necesario
- **Código autocontenido**: Todo debe funcionar con source() o copy-paste
- **Guardar como objeto ggplot**: Para flexibilidad en el .Rmd

### 1.1 Formato de Números (Locale Español)

**IMPORTANTE:** Para evitar el warning `'big.mark' y 'decimal.mark' son ambos '.'`:

```r
# ❌ INCORRECTO - genera warning
labels = function(x) format(x, big.mark = ".", scientific = FALSE)

# ✅ CORRECTO - formato español (. para miles, , para decimales)
labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
```

**Uso en scale_y_continuous:**
```r
scale_y_continuous(
  breaks = seq(15e6, 45e6, by = 5e6),
  labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
)
```

### 1.2 Inclusión de Gráficos en R-exams (PATRÓN OBLIGATORIO)

**CRÍTICO:** R-exams NO captura correctamente chunks `{r grafico}` con `print(p)`.

**Usar SIEMPRE este patrón:**

```r
# En el chunk data generation:

# 1. Crear el gráfico
p <- ggplot(...) + ...

# 2. Guardar como archivo PNG
ggsave("grafico.png", plot = p, width = 8, height = 5, dpi = 150)

# 3. Registrar como suplemento de R-exams
include_supplement("grafico.png")
```

```markdown
# En la sección Question del .Rmd:

La siguiente gráfica muestra...

![](grafico.png)

Según la gráfica...
```

**¿Por qué este patrón?**
- `ggsave()`: Guarda el gráfico como archivo independiente
- `include_supplement()`: Registra el archivo para que R-exams lo incluya en el examen
- `![](grafico.png)`: Sintaxis Markdown estándar para incluir imágenes

**❌ NUNCA usar:**
```r
# Esto NO funciona en R-exams:
```{r grafico, echo = FALSE}
print(p)
```
```

### 2. Estructura Nativa para R-exams

```r
# === PARÁMETROS (PARAM: modificables para variantes) ===
# Estos valores se definen aquí y pueden modificarse fácilmente
# o ser sobrescritos antes de ejecutar el código

# Etiquetas (traducibles)
TITULO <- ""
XLABEL <- "Eje X"
YLABEL <- "Eje Y"

# === COLORES (PARAM: modificables para variantes) ===
colores <- c(
  "Serie1" = "#00BFFF",
  "Serie2" = "#000000",
  "Serie3" = "#CC6600"
)

# === DATOS (PARAM: generables dinámicamente) ===
# En R-exams, estos datos pueden generarse con sample(), runif(), etc.
datos <- data.frame(
  x = c(1960, 1970, 1980, 1990, 2000, 2010),
  y = c(20, 30, 35, 40, 42, 43),
  serie = "Serie1"
)
```

### 3. Integración Directa con R-exams .Rmd

```r
# En el archivo ejercicio.Rmd:

# ```{r data generation, echo = FALSE, results = "hide"}
# # Generar datos aleatorios para variantes
# set.seed(sample(1:1000, 1))
# valores_x <- seq(1960, 2010, by = 10)
# valores_y <- cumsum(sample(5:15, length(valores_x), replace = TRUE)) + 20
#
# # Cargar función de generación de gráfico
# source("output_r.R")
# grafico <- generar_grafico(valores_x, valores_y)
# ```

# ```{r questionplot, echo = FALSE, results = "asis"}
# print(grafico)
# ```
```

### 4. Función Reutilizable para Variantes

```r
#' Genera gráfico parametrizable para R-exams
#'
#' @param datos_x Vector de valores X
#' @param datos_y Vector de valores Y
#' @param titulo Título del gráfico
#' @param xlabel Etiqueta eje X
#' @param ylabel Etiqueta eje Y
#' @param color Color de la línea/puntos
#' @return Objeto ggplot
generar_grafico <- function(datos_x, datos_y,
                            titulo = "",
                            xlabel = "Eje X",
                            ylabel = "Eje Y",
                            color = "#0066CC") {

  datos <- data.frame(x = datos_x, y = datos_y)

  p <- ggplot(datos, aes(x = x, y = y)) +
    geom_line(color = color, linewidth = 1) +
    geom_point(color = color, size = 2) +
    labs(title = titulo, x = xlabel, y = ylabel) +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "#CCCCCC", linewidth = 0.5),
      panel.grid.minor = element_blank()
    )

  return(p)
}
```

## Proceso

1. **Leer Análisis Inicial y Lecciones Aprendidas**:
   - Cargar `outputs/analisis_inicial.json` para reutilizar análisis estructurado
   - Si existe `outputs/lecciones_aprendidas.json`, leer lecciones de TikZ y Python para aplicar estrategias exitosas
   - Usar elementos visuales, paleta de colores y recomendaciones técnicas específicas para R
   - Aplicar lecciones aprendidas de lenguajes previos (ej: colores RGB que funcionaron bien)

2. **Actualizar Estado del Workflow**:
   - Usar skill `gestionar-estado-graficador` para iniciar fase R
   - Validar que Python esté validado o al menos iniciado (flexible)
   - Establecer `r.estado` como "en_iteracion"
   - Establecer `r.iteracion_actual` como 1 (primera iteración)
   - Registrar `r.timestamp_inicio` con timestamp actual
   - Actualizar `fase_actual` como "r_iteracion"
   - Actualizar `timestamp_ultima_actualizacion`

3. **Implementa (Pensando en R-exams nativo)**:
   - Datos en formato data.frame según elementos_visuales
   - **Variables parametrizables al inicio del archivo**
   - Capas de ggplot2 (geom_point, geom_line, etc.)
   - **Colores en vector nombrado para fácil modificación**
   - Escalas (scale_x_*, scale_y_*, scale_color_*) según elementos_visuales.ejes y paleta_colores
   - Anotaciones (annotate, geom_text) según anotaciones del análisis
   - **Etiquetas como variables para traducción/variantes**
   - Temas y estilos personalizados
   - Aplicar recomendaciones_tecnicas.r del análisis
   - **Guardar como objeto ggplot y como archivo PNG/PDF**

4. **Valida**:
   - El código debe ejecutarse sin errores con R 4.x
   - Usa gramática de gráficos correctamente
   - **Compatible directamente con R-exams .Rmd**
   - Incluye comentarios explicativos
   - **Marca secciones parametrizables**

5. **Después de generar**:
   - Guarda el código en `outputs/output_r.R`
   - Añade sección "Código R" en `outputs/reporte_matematico.md` con el código generado
   - Ejecuta el código con Rscript para generar PNG (hook automático)
   - Ejecuta automáticamente el comando `/comparar-similitud-visual r`

**IMPORTANTE**: Este comando NO debe preguntar al usuario durante iteraciones. La pregunta se hace al alcanzar el umbral en `/auto-refinar-grafico`.

## Plantilla R-exams Compatible (Nativa)

```r
#!/usr/bin/env Rscript
# ============================================
# CÓDIGO R PARA R-EXAMS (NATIVO)
# Archivo: output_r.R
# ============================================
# INSTRUCCIONES DE USO EN R-EXAMS:
#
# Opción 1 - Source en chunk R:
#    ```{r grafico, echo=FALSE, fig.height=6, fig.width=9}
#    source("output_r.R")
#    print(p)
#    ```
#
# Opción 2 - Usar función parametrizable:
#    ```{r}
#    source("output_r.R")
#    p <- generar_grafico(datos_x, datos_y, titulo = "Mi Título")
#    print(p)
#    ```
#
# Opción 3 - Copiar código directamente al chunk
#
# ============================================

library(ggplot2)
library(scales)

# === PARÁMETROS (PARAM: modificables para variantes) ===
TITULO <- ""
XLABEL <- "Eje X"
YLABEL <- "Eje Y"

# === COLORES (PARAM: modificables para variantes) ===
colores <- c(
  "Serie 1" = "#00BFFF",  # Cyan
  "Serie 2" = "#000000",  # Negro
  "Serie 3" = "#CC6600",  # Naranja/marrón
  "Serie 4" = "#0066CC",  # Azul
  "Serie 5" = "#FF9900"   # Naranja
)

# === ESTILOS DE LÍNEA (PARAM) ===
linetypes <- c(
  "Serie 1" = "dotted",
  "Serie 2" = "dashed",
  "Serie 3" = "solid",
  "Serie 4" = "solid",
  "Serie 5" = "solid"
)

# === DATOS (PARAM: generables dinámicamente en R-exams) ===
# En R-exams puedes generar estos datos con:
# set.seed(sample(1:1000, 1))
# valores <- cumsum(sample(5:15, 6, replace = TRUE))

serie1 <- data.frame(
  x = c(1960, 1970, 1980, 1990, 2000, 2010),
  y = c(20e6, 30e6, 35e6, 40e6, 42e6, 43e6),
  serie = "Serie 1"
)

# === FUNCIÓN PRINCIPAL (reutilizable) ===
generar_grafico <- function(datos = serie1,
                            titulo = TITULO,
                            xlabel = XLABEL,
                            ylabel = YLABEL,
                            colores_custom = colores,
                            output_file = NULL) {
  #' Genera gráfico ggplot2 para R-exams
  #'
  #' @param datos Data frame con columnas x, y, serie
  #' @param titulo Título del gráfico
  #' @param xlabel Etiqueta eje X
  #' @param ylabel Etiqueta eje Y
  #' @param colores_custom Vector nombrado de colores
  #' @param output_file Si se especifica, guarda el gráfico
  #' @return Objeto ggplot

  p <- ggplot(datos, aes(x = x, y = y, color = serie, linetype = serie)) +
    geom_line(linewidth = 1) +

    # Escalas
    scale_color_manual(values = colores_custom, name = NULL) +
    scale_linetype_manual(values = linetypes, name = NULL) +
    scale_y_continuous(
      labels = function(x) format(x, big.mark = ".", scientific = FALSE)
    ) +

    # Etiquetas
    labs(title = titulo, x = xlabel, y = ylabel) +

    # Tema
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "#CCCCCC", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right",
      legend.background = element_blank()
    )

  # Guardar si se especifica archivo
  if (!is.null(output_file)) {
    ggsave(output_file, plot = p, width = 9, height = 6, dpi = 150)
    ggsave(sub("\\.png$", ".pdf", output_file), plot = p, width = 9, height = 6)
    cat("Gráfico guardado:", output_file, "\n")
  }

  return(p)
}

# === EJECUCIÓN PRINCIPAL ===
# Crear gráfico con valores por defecto
p <- generar_grafico(output_file = "r_final.png")

# Mostrar en consola (útil para R-exams)
# print(p)
```

## Ejemplo de Uso en Archivo R-exams .Rmd

```markdown
Question
========

La siguiente gráfica muestra la evolución de la población de varios países:

```{r grafico, echo=FALSE, fig.height=6, fig.width=9, fig.cap=""}
# Cargar función de generación
source("output_r.R")

# Generar datos aleatorios para esta variante
set.seed(`r sample(1:10000, 1)`)
datos_variante <- data.frame(
  x = seq(1960, 2010, by = 10),
  y = cumsum(sample(3:8, 6, replace = TRUE)) * 1e6 + 15e6,
  serie = "País A"
)

# Generar gráfico
p <- generar_grafico(
  datos = datos_variante,
  xlabel = "Año",
  ylabel = "Población"
)
print(p)
```

¿En qué año la población superó los 30 millones?

Answerlist
----------
* 1970
* 1980
* 1990
* 2000

Solution
========

Observando la gráfica...

Meta-information
================
exname: poblacion_paises
extype: schoice
exsolution: 0100
```

## Opciones

- `--refinar`: Refina código existente basado en última comparación
- `--forzar`: Regenera desde cero ignorando código previo
- `--formato png|svg|pdf`: Especifica formato de salida (default: png)
- `--r-exams`: Genera código optimizado para R-exams (default: activado)

## Referencias

- `skills/generar-codigo-r/skill.md` - Plantillas y mejores prácticas
- `skills/gestionar-estado-graficador/skill.md` - Skill de gestión de estado del workflow
- `skills/transferir-conocimiento-grafico/skill.md` - Skill de transferencia de conocimiento (si existe)
- `.claude/schemas/analisis_inicial.schema.json` - Esquema del análisis estructurado
- Documentación R-exams: https://www.r-exams.org/
- Documentación ggplot2: https://ggplot2.tidyverse.org/
- Hooks automáticos se encargan de ejecución

