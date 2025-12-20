# Patrones de Errores Conocidos y Soluciones - R/exams

> **Nota:** Este documento solo registra errores que ya han sido identificados, corregidos y verificados. No se documentan problemas sin solución confirmada.

---

## Índice
1. [Error: Imagen PNG no encontrada en compilación PDF](#error-1-imagen-png-no-encontrada)
2. [Placeholder para futuros errores](#futuros-errores)

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
