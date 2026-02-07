# Regla: Gráficos Como Opciones Individuales (OBLIGATORIO)

## Principio Fundamental

**Cuando las opciones de respuesta de un ejercicio SCHOICE son gráficos, CADA gráfico DEBE ser una imagen PNG separada referenciada individualmente en el Answerlist.**

Esta regla NO tiene excepciones. Nunca usar `grid.arrange()` o similar para mostrar todos los gráficos juntos.

---

## ⚠️ REGLA CRÍTICA: Sin Títulos con Letras en Gráficos

**Los gráficos de opciones NUNCA deben tener títulos con letras (A, B, C, D).**

R-exams con `exshuffle: TRUE` mezcla las opciones y asigna automáticamente las letras (a), (b), (c), (d). Si los gráficos tienen títulos fijos, el orden visual no coincidirá con las letras asignadas por R-exams.

### ❌ PROHIBIDO: Títulos con letras

```r
# ❌ INCORRECTO - El título "A" no se reordena cuando R-exams mezcla
labs(title = "A")
labs(title = paste0("Opción ", letra))
```

### ✅ CORRECTO: Sin título o título genérico

```r
# ✅ CORRECTO - R-exams asigna (a), (b), (c), (d) automáticamente
labs(title = NULL)
```

---

## Patrón Correcto (Basado en Ejemplo Funcional)

### 1. Mezclar opciones internamente + exshuffle:TRUE

```r
# Crear lista de todas las opciones
todas_opciones <- list(
  correcta = stats_correctas,
  distractor1 = distractor1,
  distractor2 = distractor2,
  distractor3 = distractor3
)

# Mezclar internamente
opciones_mezcladas <- sample(todas_opciones)

# Identificar la posición de la respuesta correcta
indice_correcto <- which(names(opciones_mezcladas) == "correcta")

# Crear el vector de solución para r-exams
solucion <- rep(0, 4)
solucion[indice_correcto] <- 1

# Crear mapeo entre letras y estadísticas
letras <- c("A", "B", "C", "D")
mapeo_letras_stats <- setNames(names(opciones_mezcladas), letras)

# Identificar qué letra corresponde a la respuesta correcta
letra_correcta <- names(mapeo_letras_stats)[which(mapeo_letras_stats == "correcta")]

# Asignar nombres de letras a las opciones
names(opciones_mezcladas) <- letras
```

### 2. Generar gráficos con nombres de letra (sin título)

```r
# Función para crear y GUARDAR gráfico sin título
crear_y_guardar_grafico <- function(datos, letra, ...) {
  p <- ggplot(datos, aes(...)) +
    geom_...() +
    # ✅ SIN TÍTULO - R-exams asigna las letras automáticamente
    labs(title = NULL, x = NULL, y = NULL) +
    theme_minimal()

  # Guardar con nombre de letra (diagrama_a.png, etc.)
  nombre_archivo <- paste0("diagrama_", tolower(letra), ".png")
  ggsave(nombre_archivo, plot = p, width = 4, height = 5, dpi = 150, bg = "white")

  return(p)
}

# Generar los 4 gráficos
plot_A <- crear_y_guardar_grafico(opciones_mezcladas$A, "A", ...)
plot_B <- crear_y_guardar_grafico(opciones_mezcladas$B, "B", ...)
plot_C <- crear_y_guardar_grafico(opciones_mezcladas$C, "C", ...)
plot_D <- crear_y_guardar_grafico(opciones_mezcladas$D, "D", ...)
```

### 3. Answerlist con imágenes de letras

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

### 4. Solution con diagrama correcto dinámico

```markdown
### Respuesta Correcta: `r letra_correcta`

**Justificación matemática:**
[...]

**Por lo tanto**, la respuesta correcta es la **opción `r letra_correcta`**...

` ``{r mostrar_diagrama_correcto, echo=FALSE, results='asis'}
letra_correcta_lower <- tolower(letra_correcta)
nombre_archivo <- paste0("diagrama_", letra_correcta_lower, ".png")
cat("![](", nombre_archivo, "){width=70%}", sep = "")
` ``
```

### 5. Meta-information correcta

```yaml
extype: schoice
exsolution: `r paste(as.integer(solucion), collapse="")`
exshuffle: TRUE         # OBLIGATORIO - R-exams mezcla adicionalmente
```

---

## ¿Por Qué Mezcla Interna + exshuffle:TRUE?

La mezcla interna permite:
1. Conocer `letra_correcta` para mostrarla en la Solution
2. Generar archivos con nombres consistentes (diagrama_a.png, etc.)
3. Cada renderizado produce un orden diferente

El `exshuffle: TRUE` adicional:
1. Añade otra capa de aleatorización en R-exams
2. Garantiza que diferentes formatos (HTML, PDF, Moodle) también tengan orden diferente
3. Es OBLIGATORIO según las reglas del sistema

---

## Antipatrones PROHIBIDOS

### ❌ 1. Títulos con letras en gráficos

```r
# ❌ PROHIBIDO - Las letras quedan fijas visualmente
labs(title = "A")
labs(title = letra)
```

### ❌ 2. grid.arrange() para mostrar opciones juntas

```r
# ❌ PROHIBIDO - Todos los gráficos en una sola imagen
library(gridExtra)
grid.arrange(plot_A, plot_B, plot_C, plot_D, ncol = 2)
```

### ❌ 3. exshuffle: FALSE

```yaml
# ❌ PROHIBIDO - exshuffle SIEMPRE debe ser TRUE
exshuffle: FALSE
```

---

## Errores Visuales a Evitar

### Error EST-BOX-01 (Confusión posición/valor)

**PROHIBIDO en ejercicios con gráficos comparativos.**

Este error genera valores 1-11 (posiciones) en lugar de valores reales (ej: 150-190 cm).

```r
# ❌ PROHIBIDO en ejercicios gráficos
errores_conceptuales[[1]]  # EST-BOX-01

# ✅ CORRECTO - Solo usar errores que mantienen el rango de valores
errores_validos_para_grafico <- c(2, 3, 4)  # Excluir índice 1
```

### Escala del Eje Y

Calcular el rango basándose en TODOS los valores de las opciones:

```r
# ✅ CORRECTO - Rango que incluye todos los valores
y_min_global <- min(sapply(opciones_graficos, function(x) x$min)) - 2
y_max_global <- max(sapply(opciones_graficos, function(x) x$max)) + 2
```

---

## Checklist Pre-Generación

- [ ] ¿Los gráficos NO tienen título con letras (`labs(title = NULL)`)?
- [ ] ¿Hay mezcla interna con `sample()` + tracking de `letra_correcta`?
- [ ] ¿Los archivos usan nombres con letras (`diagrama_a.png`, etc.)?
- [ ] ¿`exshuffle: TRUE` está en Meta-information?
- [ ] ¿Se excluyeron errores fuera de rango (EST-BOX-01)?
- [ ] ¿El eje Y tiene un rango que incluye todos los valores?

---

## Checklist Post-Generación

- [ ] ¿Las opciones aparecen en orden (a), (b), (c), (d)?
- [ ] ¿Los gráficos NO tienen títulos con letras visibles?
- [ ] ¿Cada opción muestra un gráfico diferente?
- [ ] ¿La Solution indica correctamente la opción correcta?
- [ ] ¿Al renderizar múltiples veces, las opciones se mezclan?

---

## Ejemplo Funcional de Referencia

Ver archivos validados:
```
A-Produccion/03-En-Produccion/Ejemplos-Funcionales-Rmd/
  estadistica_diagramas_caja_interpretacion_representacion_Nivel2_v2.Rmd
```

---

## Integración con Detractor

El dominio `visual` del detractor DEBE verificar:

1. Gráficos guardados como PNGs individuales
2. **Gráficos SIN títulos con letras**
3. Answerlist referencia imágenes con letras
4. Escala compartida apropiada para todas las opciones
5. No hay errores que generen valores fuera de rango
6. `exshuffle: TRUE` está presente

---

## Resumen de la Regla

| Aspecto | ❌ PROHIBIDO | ✅ CORRECTO |
|---------|-------------|-------------|
| Título del gráfico | `labs(title = "A")` | `labs(title = NULL)` |
| Nombre de archivo | N/A | `diagrama_a.png` |
| Mezcla de opciones | Sin mezcla | Interna + exshuffle:TRUE |
| Solution | Sin indicar opción | Indica `letra_correcta` |
| exshuffle | FALSE | TRUE (OBLIGATORIO) |

---

**Versión**: 3.0
**Fecha**: 2026-02-07
**Estado**: ACTIVO Y OBLIGATORIO
**Excepciones**: NINGUNA

### Cambios v3.0 (2026-02-07)
- **Patrón basado en ejemplo funcional**: Mezcla interna + exshuffle:TRUE
- **Tracking de letra_correcta**: Para mostrar en Solution
- **Nombres con letras**: `diagrama_a.png` (no numéricos)
- **exshuffle: TRUE OBLIGATORIO**: Sin excepciones
- **Solution dinámico**: Muestra opción correcta con variable
