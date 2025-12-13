# 🔧 SOLUCIÓN: Gráficas TikZ no se muestran en Moodle

## 📋 PROBLEMA IDENTIFICADO

**Archivo afectado:**
- `01-teorema_pitagoras_entrenamiento_completo_cloze_geometrico_metrico_formulacion_ejecucion_n2_cloze_v1.Rmd`

**Síntoma:**
- ✅ La gráfica TikZ se renderiza correctamente con `exams2html()`
- ❌ La misma gráfica NO se muestra correctamente con `exams2moodle()`

**Causa raíz:**
- Moodle no soporta adecuadamente `include_tikz()` con formato automático
- Se requiere generación condicional de formatos según el tipo de salida

---

## 🔍 ANÁLISIS DE EJEMPLOS FUNCIONALES

### **Ejemplo 1: gastos_carro (Lab-Manjaro/01-S1-2024B/)**
**Estrategia:** Usa Python con matplotlib para generar PNG/PDF directamente

```r
# Genera archivos PNG y PDF con Python
py_run_string(codigo_python_graficas)

# Muestra con sintaxis condicional
if (es_moodle) {
  cat("![](grafica.png){width=300px max-width=100%}")
} else {
  cat("![](grafica.png){width=70%}")
}
```

**Ventajas:**
- ✅ Compatible con Moodle Windows
- ✅ Control total sobre el renderizado
- ❌ Requiere Python/matplotlib instalado

---

### **Ejemplo 2: probabilidad_intervalos_curva (06-Estadística-Y-Probabilidad/.../Probabilidad-Intervalos-Curva-13-S1-2024B/)**
**Estrategia:** Usa TikZ con generación condicional de formatos

```r
# Función para generar en formato apropiado
generar_tabla_multi_formato <- function(codigo_tikz, nombre_base) {
  if (identical(typ, "pdf") || identical(typ, "nops") || identical(typ, "tex")) {
    # Para PDF/LaTeX: generar PDF vectorial
    include_tikz(codigo_tikz, name = nombre_base, format = "pdf", ...)
  } else {
    # Para HTML/pandoc/moodle: generar PNG para compatibilidad
    include_tikz(codigo_tikz, name = nombre_base, format = "png", ...)
  }
}

# Mostrar con formato condicional
fmt_tikz <- if (identical(typ, "nops")) "pdf" else if (identical(typ, "pandoc")) "html" else typ
markup_tikz <- if (identical(typ, "moodle")) "moodle" else "markdown"

include_tikz(codigo_tikz, name = "grafico", markup = markup_tikz, 
             format = fmt_tikz, packages = c("tikz"), width = "8cm")
```

**Ventajas:**
- ✅ Compatible con Moodle
- ✅ Mantiene TikZ (vectorial para PDF, PNG para Moodle)
- ✅ No requiere dependencias externas adicionales
- ✅ **SOLUCIÓN ELEGIDA**

---

## ✅ SOLUCIÓN IMPLEMENTADA

### **Cambio 1: Generación de gráfica TikZ con formato condicional**

**ANTES (líneas 626-670):**
```r
```{r generar_tikz, echo=FALSE, results="hide"}
tikz_triangulo_pedagogico <- '...'
tikz_dinamico <- gsub("CATETO_A", ..., tikz_triangulo_pedagogico)

# Problema: formato fijo, no condicional
include_tikz(tikz_dinamico,
             name = "triangulo_pedagogico",
             markup = "markdown",
             format = typ,  # ← PROBLEMA: typ puede no ser compatible con Moodle
             library = c("babel", "calc"),
             packages = c("tikz", "xcolor"))
```

**DESPUÉS (líneas 628-680):**
```r
```{r generar_triangulo_tikz, echo=FALSE, results="hide"}
# Código TikZ dinámico
codigo_tikz_triangulo <- paste0("
\\begin{tikzpicture}[scale=1.5]
  % ... código TikZ con valores dinámicos ...
\\end{tikzpicture}
")

# Función para generar en formato apropiado (patrón de probabilidad_intervalos_curva)
generar_triangulo_multi_formato <- function(codigo_tikz, nombre_base) {
  if (identical(typ, "pdf") || identical(typ, "nops") || identical(typ, "tex")) {
    # Para PDF/LaTeX: generar PDF vectorial
    include_tikz(codigo_tikz, name = nombre_base, format = "pdf", 
                 packages = c("tikz"), width = "8cm")
  } else {
    # Para HTML/pandoc/moodle: generar PNG para compatibilidad
    include_tikz(codigo_tikz, name = nombre_base, format = "png", 
                 packages = c("tikz"), width = "8cm")
  }
}

# Generar el triángulo en el formato apropiado
generar_triangulo_multi_formato(codigo_tikz_triangulo, "triangulo_pedagogico")
```

---

### **Cambio 2: Mostrar imagen con formato condicional**

**ANTES (líneas 721-731):**
```r
```{r mostrar_tikz, echo=FALSE, results="asis"}
if (es_moodle) {
  cat("![](triangulo_pedagogico.png){width=300px max-width=100%}")
} else {
  cat("![](triangulo_pedagogico.png){width=70%}")
}
```

**DESPUÉS (líneas 689-701):**
```r
```{r mostrar_triangulo, echo=FALSE, results="asis"}
# Formato condicional siguiendo patrón de probabilidad_intervalos_curva
fmt_tikz <- if (identical(typ, "nops")) "pdf" else if (identical(typ, "pandoc")) "html" else typ
markup_tikz <- if (identical(typ, "moodle")) "moodle" else "markdown"

include_tikz(codigo_tikz_triangulo,
             name = "triangulo_pedagogico",
             markup = markup_tikz,
             format = fmt_tikz,
             packages = c("tikz"),
             width = "8cm")
```

---

## 🎯 VENTAJAS DE LA SOLUCIÓN

### **1. Compatibilidad Universal**
- ✅ **HTML**: Genera PNG optimizado
- ✅ **PDF**: Genera PDF vectorial de alta calidad
- ✅ **Moodle**: Genera PNG con markup específico de Moodle
- ✅ **Word**: Compatible con formato pandoc

### **2. Mantiene TikZ**
- ✅ No requiere reescribir código en Python
- ✅ Aprovecha la precisión matemática de TikZ
- ✅ Código más limpio y mantenible

### **3. Basada en Ejemplos Funcionales**
- ✅ Patrón probado en `probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1.Rmd`
- ✅ Usado exitosamente en ejercicios cloze del repositorio
- ✅ Documentado en archivos de referencia

---

## 📊 VALIDACIÓN

### **Pruebas requeridas:**

1. **Compilar con exams2html():**
   ```r
   source("SemilleroUnico_v2.R")
   # Verificar que la gráfica se muestra correctamente
   ```

2. **Compilar con exams2moodle():**
   ```r
   source("SemilleroMoodle_v2.R")
   # Verificar que la gráfica se muestra en el XML de Moodle
   ```

3. **Compilar con exams2pdf():**
   ```r
   exams2pdf("01-teorema_pitagoras_entrenamiento_completo_cloze_geometrico_metrico_formulacion_ejecucion_n2_cloze_v1.Rmd")
   # Verificar calidad vectorial del PDF
   ```

### **Archivos generados esperados:**
- `triangulo_pedagogico.png` (para HTML/Moodle)
- `triangulo_pedagogico.pdf` (para PDF/LaTeX)

---

## 📚 REFERENCIAS

### **Archivos de ejemplo consultados:**

1. **gastos_carro_graficas_comparacion_interpretacion_representacion_n2_opA_cloze_v1.Rmd**
   - Ubicación: `Lab-Manjaro/01-S1-2024B/gastos_carro_graficas_comparacion_interpretacion_representacion_n2_op*_v1/`
   - Estrategia: Python + matplotlib
   - Líneas clave: 352-451 (generación), 459-467 (mostrar)

2. **probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1.Rmd**
   - Ubicación: `06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/Probabilidad-Intervalos-Curva-13-S1-2024B/`
   - Estrategia: TikZ con formato condicional ✅ **PATRÓN USADO**
   - Líneas clave: 514-540 (función multi-formato), 621-627 (mostrar)

---

## 🔄 PRÓXIMOS PASOS

1. ✅ **Implementación completada**
2. ⏳ **Validar compilación con exams2html()**
3. ⏳ **Validar compilación con exams2moodle()**
4. ⏳ **Verificar visualización en Moodle Windows**
5. ⏳ **Documentar resultados de pruebas**

---

## 💡 LECCIONES APRENDIDAS

### **Para futuros ejercicios con gráficas TikZ en formato Cloze:**

1. **SIEMPRE usar generación condicional de formatos:**
   ```r
   if (identical(typ, "pdf") || identical(typ, "nops") || identical(typ, "tex")) {
     format = "pdf"  # Vectorial para LaTeX
   } else {
     format = "png"  # Raster para HTML/Moodle
   }
   ```

2. **SIEMPRE especificar markup condicional:**
   ```r
   markup_tikz <- if (identical(typ, "moodle")) "moodle" else "markdown"
   ```

3. **Consultar ejemplos funcionales ANTES de implementar:**
   - `/A-Produccion/Ejemplos-Funcionales-Rmd/`
   - `06-Estadística-Y-Probabilidad/.../Probabilidad-Intervalos-Curva-13-S1-2024B/`

---

**Fecha de implementación:** 2025-09-30  
**Autor:** Sistema ICFES R-exams 2025  
**Estado:** ✅ Implementado, pendiente validación

