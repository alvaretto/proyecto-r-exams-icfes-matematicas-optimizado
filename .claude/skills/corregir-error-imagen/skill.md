---
name: corregir-error-imagen
description: Corrige errores de imágenes faltantes reemplazando \includegraphics por código TikZ inline.
---

# Skill: Corrector de Errores de Imágenes en R/exams

## Propósito
Identifica y corrige automáticamente errores de compilación LaTeX causados por archivos de imagen no encontrados (típicamente `File 'nombre.png' not found`).

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

## Referencias

Ver documentación completa en:
- `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/docs/patrones-errores-conocidos.md`

## Ejecución del skill

Cuando el usuario invoca `/corregir-error-imagen` o cuando detectas el error automáticamente:

1. Leer el archivo .Rmd problemático
2. Identificar chunks con `include_tikz()`
3. Aplicar el patrón de corrección
4. Guardar el archivo corregido
5. Ejecutar prueba de compilación
6. Informar resultados al usuario
