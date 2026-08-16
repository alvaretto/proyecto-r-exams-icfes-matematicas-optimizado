# Patron de Renderizado Condicional para ERR_G1 — RETIRADO (ver sustituto vigente abajo)

> ⚠️ **El patrón "renderizado condicional" (`if (knitr::is_latex_output()) ... else ...`) que da
> nombre a este documento fue RETIRADO el 2026-08-15.** Medido con fixtures renderizados:
> `knitr::is_latex_output()` es **SIEMPRE FALSE** bajo R/exams — los 5 pipelines (html, pdf, docx,
> nops, moodle) tejen siempre a Markdown y delegan en pandoc, así que durante el `knit` no hay
> destino LaTeX que detectar. La rama LaTeX nunca se ejecuta; si la rama `else` emite HTML/PNG por
> `include_graphics()`, el escritor LaTeX de pandoc descarta ese HTML crudo y **la figura
> desaparece en el PDF sin error ni warning**. Ver `.claude/rules/codigo-rmd.md` regla #1 y
> `.claude/rules/markdown-imagenes-pdf.md` Patrón B' para la medición completa y el sustituto.
> La sección "DESPUES (CORRECTO)" de este documento queda corregida más abajo; el resto se
> conserva como referencia histórica del síntoma ERR_G1.

## Contexto del Error

```
Package pdftex.def Error: File `imagen.png' not found: using draft setting.
Error: LaTeX failed to compile archivo.tex
```

## Causa Raiz

El uso de `include_tikz()` genera archivos PNG/PDF en directorios temporales
que no son accesibles durante la compilacion LaTeX final por `exams2pdf()`.

## Patron de Correccion

### ANTES (INCORRECTO)

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

### DESPUES (CORRECTO, actualizado 2026-08-15) — una sola llamada, SIN condicional

```r
```{r generar_diagrama, echo=FALSE, results="hide"}
# Solo generar el codigo TikZ, NO renderizarlo aqui
tikz_code <- generar_tikz_funcion(params)
```

**Uso en Question:**

```r
```{r mostrar_diagrama, echo=FALSE, results='asis', fig.align='center'}
# ✅ Una sola llamada, sin ramificar por is_latex_output(): markup="markdown" hace que
# pandoc enrute correctamente por tipo de bloque en los 5 destinos (html/pdf/docx/nops/moodle)
include_tikz(tikz_code,
             name = "mi_diagrama",
             markup = "markdown",
             format = typ,
             packages = c("tikz", "xcolor"),
             width = "8cm")
cat("\n\n")
```

**Por qué NO hace falta condicional**: verificado sobre fixture — con `markup = "markdown"` el
`.tex` generado contiene `\includegraphics[width=8cm,…]{mi_diagrama.png}` y el HTML contiene
`<img …>`; cero fuga en ningún destino. Ver regla #21 §Familia 2 (medición sobre los cinco
pipelines) y regla #18 §Patrón B'.

## Casos de Uso

### Diagramas Geometricos

- Cilindros, conos, prismas, piramides
- Poligonos, triangulos, circunferencias
- Graficas de funciones

### Graficos Estadisticos

- Histogramas, diagramas de barras
- Graficos de dispersion
- Curvas de distribucion

### Diagramas de Probabilidad

- Arboles de probabilidad
- Diagramas de Venn
- Espacios muestrales

## Verificacion

Despues de aplicar la correccion:

1. Compilar a PDF con `exams2pdf()`
2. Compilar a HTML con `exams2html()`
3. Confirmar que ambos formatos funcionan
