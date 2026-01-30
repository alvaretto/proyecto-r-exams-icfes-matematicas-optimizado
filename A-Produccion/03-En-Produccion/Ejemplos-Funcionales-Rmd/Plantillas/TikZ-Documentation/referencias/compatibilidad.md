# 🔧 Guía de Compatibilidad TikZ con R-exams

## 🎯 **Análisis de Compatibilidad por Formato**

### ✅ **exams2pdf - Compatibilidad Excelente**
- **Procesamiento**: Directo con LaTeX/pdfLaTeX
- **Bibliotecas soportadas**: Todas las estándar de TikZ
- **Limitaciones**: Ninguna significativa
- **Recomendaciones**: 
  - Usar como formato de referencia para desarrollo
  - Validar primero en PDF antes de otros formatos

### ⚠️ **exams2html - Compatibilidad Media**
- **Procesamiento**: TikZ → PDF → PNG/SVG → HTML
- **Problemas comunes**:
  - Colores personalizados con `\definecolor`
  - Bibliotecas avanzadas (`shadows`, `fadings`)
  - Fuentes no estándar
- **Soluciones**:
  - Usar colores predefinidos: `red`, `blue`, `green!50`
  - Evitar efectos de sombreado complejos
  - Mantener bibliotecas básicas

### ⚠️ **exams2moodle - Compatibilidad Media**
- **Procesamiento**: Similar a HTML
- **Consideraciones adicionales**:
  - Tamaño de imagen limitado
  - Resolución fija
  - Compatibilidad con navegadores
- **Optimizaciones**:
  - Usar `scale` apropiado en tikzpicture
  - Evitar texto muy pequeño
  - Colores con buen contraste

### 🔄 **exams2pandoc - Compatibilidad Variable**
- **Dependencias**: Pandoc + LaTeX
- **Factores**: Versión de Pandoc y configuración
- **Recomendaciones**:
  - Probar con versión específica de Pandoc
  - Usar templates compatibles
  - Validar conversión a diferentes formatos

### 📄 **exams2nops - Compatibilidad Alta**
- **Procesamiento**: Similar a PDF
- **Consideraciones**:
  - Optimizado para impresión
  - Escala de grises recomendada
  - Evitar colores muy claros

## 🛠️ **Patrones de Código Compatibles**

### ✅ **Código Base Recomendado**
```latex
\begin{tikzpicture}[scale=1.0]
  % Usar coordenadas simples
  \coordinate (A) at (0,0);
  \coordinate (B) at (2,0);
  \coordinate (C) at (1,1.73);
  
  % Colores estándar
  \draw[blue, thick] (A) -- (B) -- (C) -- cycle;
  \fill[red!30] (A) -- (B) -- (C) -- cycle;
  
  % Etiquetas simples
  \node[below] at (A) {A};
  \node[below] at (B) {B};
  \node[above] at (C) {C};
\end{tikzpicture}
```

### ⚠️ **Código que Requiere Adaptación**
```latex
% PROBLEMÁTICO
\definecolor{customBlue}{RGB}{60,120,180}
\usetikzlibrary{shadows.blur}
\pgfmathsetmacro{\radius}{2.5}

% ADAPTADO
% Usar variables R en lugar de \pgfmathsetmacro
% Usar colores estándar
\begin{tikzpicture}
  \draw[blue!70] (0,0) circle (`r radio_valor`);
\end{tikzpicture}
```

### ❌ **Código No Compatible**
```latex
% EVITAR
\usetikzlibrary{fadings}
\tikzfading[name=fade out]
\path[fill=blue, path fading=fade out] ...
```

## 🔍 **Checklist de Validación**

### 📋 **Pre-implementación**
- [ ] ¿Usa solo bibliotecas básicas de TikZ?
- [ ] ¿Evita colores personalizados con `\definecolor`?
- [ ] ¿No depende de fuentes específicas?
- [ ] ¿Tiene escala apropiada (no muy grande/pequeña)?
- [ ] ¿El texto es legible en diferentes resoluciones?

### 🧪 **Testing Multi-formato**
- [ ] Compila correctamente en `exams2pdf`
- [ ] Genera imagen válida en `exams2html`
- [ ] Funciona en `exams2moodle` sin errores
- [ ] Compatible con `exams2pandoc`
- [ ] Imprime correctamente en `exams2nops`

### 🔧 **Optimización**
- [ ] Tamaño de archivo de imagen < 500KB
- [ ] Resolución adecuada para web (150-300 DPI)
- [ ] Colores con contraste suficiente
- [ ] Texto legible en tamaño mínimo

## 🛠️ **Herramientas de Diagnóstico**

### 📊 **Script de Prueba R**
```r
# Función para probar compatibilidad TikZ
test_tikz_compatibility <- function(rmd_file) {
  formats <- c("pdf", "html", "moodle", "pandoc")
  results <- list()
  
  for(format in formats) {
    tryCatch({
      if(format == "pdf") {
        exams2pdf(rmd_file, n = 1, name = "test")
      } else if(format == "html") {
        exams2html(rmd_file, n = 1, name = "test")
      } else if(format == "moodle") {
        exams2moodle(rmd_file, n = 1, name = "test")
      } else if(format == "pandoc") {
        exams2pandoc(rmd_file, n = 1, name = "test")
      }
      results[[format]] <- "✅ SUCCESS"
    }, error = function(e) {
      results[[format]] <- paste("❌ ERROR:", e$message)
    })
  }
  
  return(results)
}
```

### 🔍 **Análisis de Errores Comunes**
```r
# Detectar patrones problemáticos en código TikZ
analyze_tikz_code <- function(tikz_code) {
  issues <- list()
  
  # Detectar colores personalizados
  if(grepl("\\\\definecolor", tikz_code)) {
    issues$colors <- "⚠️ Colores personalizados detectados"
  }
  
  # Detectar bibliotecas problemáticas
  problematic_libs <- c("shadows", "fadings", "decorations")
  for(lib in problematic_libs) {
    if(grepl(paste0("\\\\usetikzlibrary\\{.*", lib), tikz_code)) {
      issues[[lib]] <- paste("⚠️ Biblioteca problemática:", lib)
    }
  }
  
  # Detectar \pgfmathsetmacro
  if(grepl("\\\\pgfmathsetmacro", tikz_code)) {
    issues$macros <- "⚠️ Usar variables R en lugar de \\pgfmathsetmacro"
  }
  
  return(issues)
}
```

## 📚 **Bibliotecas TikZ por Compatibilidad**

### ✅ **Alta Compatibilidad**
- **Básicas**: `calc`, `positioning`, `arrows`
- **Geometría**: `intersections`, `through`
- **Matemáticas**: `math`, `fpu`

### ⚠️ **Compatibilidad Media**
- **3D**: `3d` (requiere validación)
- **Decoraciones**: `decorations.markings` (básicas)
- **Patrones**: `patterns` (simples)

### ❌ **Baja Compatibilidad**
- **Efectos**: `shadows`, `fadings`, `blur`
- **Avanzadas**: `external`, `spy`, `mindmap`
- **Específicas**: `calendar`, `er`, `petri`

## 🔄 **Proceso de Adaptación**

### 1️⃣ **Análisis Inicial**
```bash
# Buscar patrones problemáticos
grep -n "definecolor\|usetikzlibrary\|pgfmathsetmacro" archivo.tikz
```

### 2️⃣ **Adaptación Automática**
```r
# Script para adaptación básica
adapt_tikz_for_rexams <- function(tikz_code) {
  # Reemplazar colores personalizados
  tikz_code <- gsub("\\\\definecolor\\{.*\\}\\{.*\\}\\{.*\\}", "", tikz_code)
  
  # Simplificar bibliotecas
  tikz_code <- gsub("\\\\usetikzlibrary\\{.*shadows.*\\}", "", tikz_code)
  
  # Convertir \pgfmathsetmacro a comentarios para revisión manual
  tikz_code <- gsub("\\\\pgfmathsetmacro", "% TODO: Convertir a variable R - \\\\pgfmathsetmacro", tikz_code)
  
  return(tikz_code)
}
```

### 3️⃣ **Validación Manual**
- Revisar conversiones automáticas
- Probar en múltiples formatos
- Ajustar parámetros según necesidad
- Documentar cambios realizados

### 4️⃣ **Testing Final**
- Generar 5 variantes del ejercicio
- Probar en todos los formatos objetivo
- Verificar calidad visual
- Documentar en guía de compatibilidad

---

**Última actualización**: `r Sys.Date()`  
**Versión**: 1.0  
**Mantenedor**: Proyecto ICFES R-exams