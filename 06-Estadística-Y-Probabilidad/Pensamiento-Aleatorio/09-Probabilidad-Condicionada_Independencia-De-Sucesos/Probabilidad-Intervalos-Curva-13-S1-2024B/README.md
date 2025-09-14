# Ejercicio 13: Probabilidad e Interpretación de Gráficos - R/exams

## Descripción General

Este directorio contiene dos versiones del ejercicio sobre interpretación de gráficos de probabilidad y representación de datos en tablas:

- **`probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd`**: Versión con tablas generadas como imágenes PNG usando Python/matplotlib
- **`probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd`**: Versión avanzada con tablas vectoriales generadas usando TikZ nativo de LaTeX

## Contenido del Ejercicio

### Competencia ICFES
- **Competencia**: Interpretación y representación
- **Nivel de dificultad**: 2
- **Componente**: Aleatorio (Estadística)
- **Contexto**: Matemático

### Descripción Pedagógica
El ejercicio presenta una curva de distribución dividida en tres intervalos con probabilidades específicas. Los estudiantes deben identificar cuál de las cuatro tablas proporcionadas representa correctamente la información mostrada en el gráfico.

### Características Técnicas
- **Tipo de pregunta**: Selección múltiple (schoice)
- **Barajado automático**: `exshuffle: TRUE`
- **Solución fija**: `exsolution: 1000` (opción A correcta antes del barajado)
- **Diversidad**: >300 combinaciones únicas de parámetros

## ✨ Mejoras Recientes Implementadas

### **Reducción de Tamaño de Fuente en Tablas TikZ**
- **Comando aplicado**: `\small` en función `generar_tabla_tikz`
- **Beneficio**: Mejora significativa en la presentación visual de las tablas
- **Legibilidad**: Mantenida completamente en todos los formatos de salida
- **Compatibilidad**: Verificada en PDF vectorial, HTML, DOCX y Moodle
- **Aplicación**: Todas las opciones de tabla (A, B, C, D) con tamaño reducido

### **Renombrado según Normas ICFES**
- **Nomenclatura aplicada**: `tema_subtema_tipo_competencia_componente_nivel_version.Rmd`
- **Consistencia**: Alineado con estándares del proyecto RepositorioMatematicasICFES_R_Exams
- **Mantenibilidad**: Nombres descriptivos y organizados jerárquicamente
- **Identificación**: Contenido y competencia ICFES inmediatamente reconocibles

## Comparación de Versiones

| Característica | PNG (v1) | TikZ (tikz_v1) |
|---|---|---|
| **Generación de tablas** | Python/matplotlib → PNG | TikZ/LaTeX → PDF/PNG |
| **Calidad visual** | Rasterizada (fija) | Vectorial (escalable) |
| **Tamaño de fuente** | Fijo en matplotlib | Optimizado con `\small` |
| **Dependencias** | reticulate + matplotlib | TikZ nativo |
| **Compatibilidad** | Todos los formatos | Todos los formatos |
| **Mantenimiento** | Dos lenguajes (R+Python) | R puro |
| **Tipografía** | matplotlib fonts | LaTeX fonts (consistente) |
| **Tamaño archivos** | PNG ~50KB cada tabla | PDF ~15KB cada tabla |

## Archivos Principales

### Archivos de Ejercicio
- `probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd` - Versión PNG con Python/matplotlib
- `probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd` - Versión TikZ vectorial
- `Copia de 13.Rmd` - Respaldo de la versión original

### Scripts de Generación
- `SemilleroUnico_v2.R` - Generación individual
- `SemilleroMoodle_v2.R` - Generación para Moodle
- `SemilleroCloze.R` - Generación tipo Cloze

### Templates LaTeX
- `pcielo.tex` - Template principal
- `pcielo_nosol.tex` - Template sin soluciones
- `solpcielo.tex` - Template solo soluciones

### Documentación
- `README.md` - Este archivo
- `WALKTHROUGH.md` - Guía paso a paso
- `CONVERSION_TIKZ_TABLAS.md` - Documentación técnica de conversión
- `CORRECCION_ERROR_VARIABLE_LONGITUD_CERO.md` - Correcciones aplicadas
- `OPTIMIZACIONES_APLICADAS.md` - Historial de optimizaciones

## Formatos de Salida Soportados

### ✅ PDF (exams2pdf)
- **PNG v1**: Imágenes PNG embebidas
- **TikZ tikz_v1**: Tablas vectoriales nativas de alta calidad

### ✅ HTML (rmarkdown::render)
- **PNG v1**: Imágenes PNG en navegador
- **TikZ tikz_v1**: Conversión automática TikZ → PNG

### ✅ DOCX (exams2pandoc)
- **PNG v1**: Imágenes PNG embebidas en Word
- **TikZ tikz_v1**: Imágenes convertidas embebidas

### ✅ Moodle (exams2moodle)
- **PNG v1**: Referencias PNG en XML
- **TikZ tikz_v1**: Referencias de imagen en XML

## Estructura de Datos Generados

### Variables Aleatorias
```r
p_central <- sample(seq(0.40, 0.55, by = 0.01), 1)  # Probabilidad central
p_lateral <- (1 - p_central) / 2                    # Probabilidades laterales
limite1 <- sample(3:6, 1)                           # Primer límite
ancho_central <- sample(2:6, 1)                     # Ancho del intervalo central
limite2 <- limite1 + ancho_central                  # Segundo límite
limite_sup <- 14                                    # Límite superior fijo
```

### Opciones de Respuesta
- **Opción A (Correcta)**: Intervalos individuales con probabilidades correctas
- **Opción B (Distractor)**: Intervalos acumulativos (0 ≤ x ≤ a, 0 ≤ x ≤ b, etc.)
- **Opción C (Distractor)**: Permutación de probabilidades (central ↔ laterales)
- **Opción D (Distractor)**: Asignación incorrecta de probabilidades

## Archivos Generados

### Imágenes de Tablas
- `tabla_opcion_a.png/.pdf` - Tabla opción A (correcta)
- `tabla_opcion_b.png/.pdf` - Tabla opción B (acumulativa)
- `tabla_opcion_c.png/.pdf` - Tabla opción C (permutada)
- `tabla_opcion_d.png/.pdf` - Tabla opción D (incorrecta)

### Gráfico Principal
- `prob_dist_grafico.png/.pdf` - Curva de distribución con intervalos

### Directorio `salida/`
- Archivos PDF, DOCX, XML generados por R/exams
- Copias de todas las imágenes para compatibilidad

## Requisitos del Sistema

### Para 13.Rmd (Versión PNG)
```r
library(exams)
library(knitr)
library(reticulate)  # Para integración Python
```

### Para 13-TikZ.Rmd (Versión Vectorial)
```r
library(exams)
library(knitr)
# TikZ incluido en header-includes del YAML
```

### Paquetes LaTeX Requeridos
```latex
\usepackage{tikz}
\usepackage{pgfplots}
\usepackage{booktabs}
\usepackage{array}
```

## Comandos de Generación

### Generación Individual
```r
# PDF
exams2pdf("probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd", n=1, dir="salida")
exams2pdf("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd", n=1, dir="salida")

# HTML
rmarkdown::render("probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd", "html_document")
rmarkdown::render("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd", "html_document")

# DOCX
exams2pandoc("probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd", n=1, dir="salida")
exams2pandoc("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd", n=1, dir="salida")

# Moodle
exams2moodle("probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd", n=1, dir="salida")
exams2moodle("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd", n=1, dir="salida")
```

### Generación Masiva
```r
# Usar scripts especializados
source("SemilleroUnico_v2.R")
source("SemilleroMoodle_v2.R")
```

## Recomendaciones de Uso

### Cuándo usar la versión PNG (v1)
- ✅ Entornos sin TikZ instalado
- ✅ Compatibilidad máxima garantizada
- ✅ Desarrollo rápido sin configuración LaTeX

### Cuándo usar la versión TikZ (tikz_v1)
- ✅ Calidad visual máxima requerida
- ✅ Documentos PDF profesionales
- ✅ Mantenimiento a largo plazo
- ✅ Integración completa con LaTeX

## Solución de Problemas

### Error "variable de longitud cero"
- **Causa**: Variables del objeto `datos` no definidas
- **Solución**: Verificar ejecución completa del chunk `data_generation`

### Imágenes no aparecen en DOCX
- **Causa**: Archivos no copiados al directorio `salida/`
- **Solución**: Verificar chunk `copiar_archivos_salida`

### Error de compilación TikZ
- **Causa**: Paquetes LaTeX faltantes
- **Solución**: Instalar `texlive-pictures` o equivalente

## Contribución y Desarrollo

### Estructura de Commits
- Usar prefijo `probabilidad_intervalos_curva_*_v1.Rmd:` o `*_tikz_v1.Rmd:` según el archivo modificado
- Incluir descripción detallada de cambios
- Documentar impacto en compatibilidad

### Testing
- Probar los 4 formatos principales antes de commit
- Verificar diversidad de versiones (>300 combinaciones)
- Validar que `exshuffle` funcione correctamente

## Licencia y Créditos

Parte del proyecto **RepositorioMatematicasICFES_R_Exams** para la generación automatizada de ejercicios de matemáticas tipo ICFES usando el framework R/exams.
