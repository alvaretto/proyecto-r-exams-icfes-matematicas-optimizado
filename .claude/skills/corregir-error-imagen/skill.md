---
name: corregir-error-imagen
description: Ejecuta 📚 SUBFASE 3A para ERR_G1 - Corrección de imágenes faltantes basada en ejemplos.
---

# Skill: 📚 SUBFASE 3A - Corrección de Imágenes Faltantes (ERR_G1)

## ⚡ CONTEXTO: Ciclo de Validación y Corrección Automática

Este skill ejecuta la **SUBFASE 3A: CORRECCIÓN BASADA EN EJEMPLOS** para ERR_G1:

```
⚡ FASE 3: Decisión y Acción
    │
    └── ✓ CON ERROR ERR_G1 (File not found):
            │
            ├── 📚 SUBFASE 3A: Corrección basada en ejemplos ← ESTE SKILL
            │       ↓
            ├── 🔄 SUBFASE 3B: Revalidación (volver a FASE 1)
            │
            └── 📊 SUBFASE 3C: Documentar solución
```

## Propósito
Corrige automáticamente errores de compilación LaTeX causados por archivos de imagen
no encontrados (`File 'nombre.png' not found`), basándose en ejemplos funcionales.

## ⚠️ PASO OBLIGATORIO: Consultar Ejemplos Funcionales

**ANTES de aplicar cualquier corrección:**

```bash
# Consultar ejemplos funcionales con renderizado condicional
ls /A-Produccion/Ejemplos-Funcionales-Rmd/

# Buscar patrones de is_latex_output
grep -l "is_latex_output" /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd

# Buscar patrones de include_tikz
grep -l "include_tikz" /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd
```

## Contexto del Error

### Error típico:
```
Package pdftex.def Error: File `imagen.png' not found: using draft setting.
Error: LaTeX failed to compile archivo.tex
```

### Causa raíz:
El uso de `include_tikz()` genera archivos PNG/PDF en directorios temporales que no son accesibles durante la compilación LaTeX final por `exams2pdf()`.

## Solución Automatizada

### Patrón de corrección:

**ANTES (INCORRECTO):**
```r
```{r generar_diagrama, echo=FALSE, results="hide"}
tikz_code <- generar_tikz_funcion(params)

include_tikz(tikz_code,
             name = "mi_diagrama",
             markup = "markdown",
             format = typ,
             packages = c("tikz", "xcolor"),
             width = "8cm")
```

**Uso en Question:**
```markdown
![](mi_diagrama.png){width=50%}
```

**DESPUÉS (CORRECTO):**
```r
```{r generar_diagrama, echo=FALSE, results="hide"}
# Solo generar el código TikZ, NO renderizarlo aquí
tikz_code <- generar_tikz_funcion(params)
```

**Uso en Question:**
```r
```{r mostrar_diagrama, echo=FALSE, results='asis', fig.align='center'}
# Detectar formato de salida
es_latex <- knitr::is_latex_output()

if (es_latex) {
  # Para PDF: insertar código TikZ directamente
  cat("\\begin{center}\n")
  cat(tikz_code)
  cat("\n\\end{center}\n")
} else {
  # Para HTML: usar include_tikz
  include_tikz(tikz_code,
               name = "mi_diagrama",
               markup = "markdown",
               format = typ,
               packages = c("tikz", "xcolor"),
               width = "8cm")
  cat("\n\n")
}
```

## Algoritmo de corrección

1. **Detectar el error:**
   - Buscar mensajes: `File '*.png' not found`
   - Identificar chunk que usa `include_tikz()`

2. **Aplicar la corrección:**
   - Eliminar la llamada a `include_tikz()` del chunk de generación
   - Crear nuevo chunk con condicional `knitr::is_latex_output()`
   - Para LaTeX: insertar código TikZ directamente con `cat()`
   - Para HTML: mantener `include_tikz()`

3. **Verificar la solución:**
   - Compilar a PDF con `exams2pdf()`
   - Compilar a HTML con `exams2html()`
   - Confirmar que ambos formatos funcionan

## Casos de uso

### Caso 1: Diagramas geométricos

- Cilindros, conos, prismas, pirámides
- Polígonos, triángulos, circunferencias
- Gráficas de funciones

### Caso 2: Gráficos estadísticos

- Histogramas, diagramas de barras
- Gráficos de dispersión
- Curvas de distribución

### Caso 3: Diagramas de probabilidad

- Árboles de probabilidad
- Diagramas de Venn
- Espacios muestrales

## 🔄 SUBFASE 3B: Revalidación Obligatoria

**DESPUÉS de aplicar correcciones:**

```
⚠️ OBLIGATORIO: Volver automáticamente a FASE 1
→ Ejecutar validar-renderizado (exams2html, pdf, docx, nops)
→ Ejecutar validar-coherencia
→ Verificar que ERR_G1 está resuelto
→ REPETIR si persisten errores
```

## 📊 SUBFASE 3C: Documentar Solución (Solo si éxito)

**Solo después de revalidación exitosa:**

1. Documentar error y solución en `.claude/docs/patrones-errores-conocidos.md`
2. Incluir ejemplo funcional utilizado
3. Registrar código antes/después

## ⛔ CONDICIONES CRÍTICAS

1. ❌ **NO terminar** con ERR_G1 sin resolver
2. ✓ **SIEMPRE** consultar ejemplos funcionales ANTES de corregir
3. ✓ **SIEMPRE** ejecutar SUBFASE 3B después de correcciones
4. ✓ **Ejemplos funcionales** = Fuente de verdad ABSOLUTA

## Referencias

- `/A-Produccion/Ejemplos-Funcionales-Rmd/` (FUENTE DE VERDAD)
- `.claude/Mermaid_Chart.txt` (diagrama de flujo oficial)
- `.claude/docs/patrones-errores-conocidos.md` (Error 1)

## Ejecución del skill

Cuando el usuario invoca `/corregir-error-imagen` o cuando detectas el error automáticamente:

1. 📚 Consultar ejemplos funcionales PRIMERO
2. Leer el archivo .Rmd problemático
3. Identificar chunks con `include_tikz()`
4. Extraer patrón de solución de ejemplo funcional
5. Aplicar el patrón de corrección (renderizado condicional)
6. Guardar el archivo corregido
7. 🔄 Ejecutar SUBFASE 3B: Volver a FASE 1 (revalidación)
8. Si éxito → SUBFASE 3C: Documentar solución
9. Si falla → Repetir con solución alternativa
