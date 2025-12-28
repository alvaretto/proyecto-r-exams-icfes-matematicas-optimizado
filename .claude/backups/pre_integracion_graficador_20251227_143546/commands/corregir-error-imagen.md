---
description: Ejecuta 📚 SUBFASE 3A para ERR_G1 - Corrección de imágenes faltantes basada en ejemplos.
---

# 📚 SUBFASE 3A: Corrección de Imágenes Faltantes (ERR_G1)

## ⚡ CONTEXTO: Ciclo de Validación y Corrección Automática

Este comando ejecuta la **SUBFASE 3A: CORRECCIÓN BASADA EN EJEMPLOS** para ERR_G1:

```
⚡ FASE 3: Decisión y Acción
    │
    └── ✓ CON ERROR ERR_G1 (File not found):
            │
            ├── 📚 SUBFASE 3A: Corrección basada en ejemplos ← ESTE COMANDO
            │       ↓
            ├── 🔄 SUBFASE 3B: Revalidación (volver a FASE 1)
            │
            └── 📊 SUBFASE 3C: Documentar solución
```

Detecta y corrige errores del tipo:
```
! Package pdftex.def Error: File 'imagen.png' not found
```

## ⚠️ PASO OBLIGATORIO: Consultar Ejemplos Funcionales

**ANTES de aplicar cualquier corrección:**

```bash
# Consultar ejemplos funcionales
ls /A-Produccion/Ejemplos-Funcionales-Rmd/

# Buscar patrones de renderizado condicional
grep -l "is_latex_output" /A-Produccion/Ejemplos-Funcionales-Rmd/*.Rmd
```

## Diagnóstico

El error ocurre cuando el archivo .Rmd usa `\includegraphics{imagen.png}` en lugar de generar la imagen con código TikZ.

## Solución

### Paso 1: Localizar el archivo .Rmd problemático
```bash
# Buscar en En-Desarrollo
find A-Produccion/En-Desarrollo -name "*.Rmd" -type f
```

### Paso 2: Identificar el uso de \includegraphics
```bash
grep -n "includegraphics" A-Produccion/En-Desarrollo/*.Rmd
```

### Paso 3: Consultar ejemplos funcionales con TikZ
```bash
# Buscar ejemplos con figuras geométricas 3D
grep -r "begin{tikzpicture}" A-Produccion/En-Produccion/05-Geometría/ | head -5
```

### Paso 4: Reemplazar con código TikZ

**NUNCA usar `\includegraphics` para figuras matemáticas.**

En su lugar, usar chunk TikZ:

```r
```{r generar_tikz, echo=FALSE, results="asis"}
tikz_code <- '
\\begin{tikzpicture}[scale=1.2]
  % Código TikZ para la figura
  % Consultar ejemplos en A-Produccion/En-Produccion/
\\end{tikzpicture}
'

include_tikz(tikz_code,
             name = "figura_geometrica",
             markup = "markdown",
             format = typ,
             library = c("3d", "babel"),
             packages = c("tikz", "xcolor", "pgfplots"),
             width = "10cm")
```
```

### Paso 5: Para cilindros específicamente

Consultar ejemplos en:
- `/A-Produccion/En-Produccion/05-Geometría/`
- Buscar patrones de cilindros 3D con TikZ

Estructura típica de cilindro:
```latex
\begin{tikzpicture}[scale=1.5]
  % Base inferior (elipse)
  \draw[thick] (0,0) ellipse (2cm and 0.5cm);
  
  % Líneas laterales
  \draw[thick] (-2,0) -- (-2,4);
  \draw[thick] (2,0) -- (2,4);
  
  % Base superior (elipse)
  \draw[thick] (0,4) ellipse (2cm and 0.5cm);
  
  % Etiquetas
  \node at (0,-0.8) {Radio: \textbf{\textit{r}}};
  \node at (2.5,2) {Altura: \textbf{\textit{h}}};
\end{tikzpicture}
```

## 🔄 SUBFASE 3B: Revalidación Obligatoria

**DESPUÉS de aplicar correcciones:**

```
⚠️ OBLIGATORIO: Volver automáticamente a FASE 1
→ Ejecutar /validar-renderizado
→ Ejecutar /validar-coherencia
→ Verificar que ERR_G1 está resuelto
→ REPETIR si persisten errores
```

## 📊 SUBFASE 3C: Documentar Solución (Solo si éxito)

**Solo después de revalidación exitosa:**
- Documentar en `.claude/docs/patrones-errores-conocidos.md`

## ⛔ CONDICIONES CRÍTICAS

1. ❌ **NO terminar** con ERR_G1 sin resolver
2. ✓ **SIEMPRE** consultar ejemplos funcionales ANTES de corregir
3. ✓ **SIEMPRE** ejecutar SUBFASE 3B después de correcciones
4. ✓ **Ejemplos funcionales** = Fuente de verdad ABSOLUTA

## Regla de Oro

**SIEMPRE** generar figuras geométricas con TikZ, **NUNCA** con `\includegraphics`.

Consultar `/A-Produccion/Ejemplos-Funcionales-Rmd/` antes de escribir código TikZ.

## Referencias

- `/A-Produccion/Ejemplos-Funcionales-Rmd/` (FUENTE DE VERDAD)
- `.claude/Mermaid_Chart.txt` (diagrama de flujo oficial)
- `.claude/docs/patrones-errores-conocidos.md`

