# Ejercicio 13: Probabilidad e Interpretación de Gráficos - R/exams

## Descripción General

Este directorio contiene tres versiones del ejercicio sobre interpretación de gráficos de probabilidad y representación de datos en tablas:

- **`probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd`**: Versión con tablas generadas como imágenes PNG usando Python/matplotlib
- **`probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd`**: Versión avanzada con tablas vectoriales generadas usando TikZ nativo de LaTeX
- **`probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1_2.Rmd`**: ⭐ **NUEVA** - Versión con incremento de dificultad y formato cloze avanzado

## Contenido del Ejercicio

### Competencia ICFES
- **Competencia**: Interpretación y representación
- **Nivel de dificultad**: 2 (versiones v1 y tikz_v1) | **3** (versión cloze_v1_2)
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

### 🆕 **Nueva Versión con Incremento de Dificultad (Septiembre 2025)**
**Archivo**: `probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1_2.Rmd`

#### **Características Principales**
- **🎯 Nivel**: 2 → **3** (Media → Media-Alta)
- **📊 Formato**: Cloze con **9 elementos** (8 numéricas + 1 schoice)
- **🔢 Precisión**: **3 decimales** (0.XXX) con tolerancia 0.005
- **📈 Variabilidad**: **5,208 versiones únicas** (incremento +150%)
- **🧮 Análisis**: Pasos complementarios de probabilidad añadidos
- **📉 Gráficos**: Centro de distribución variable y desviación adaptativa

#### **Nuevos Elementos de Evaluación**
- **Paso 7**: Cálculo de probabilidad fuera del intervalo central
- **Paso 8**: Identificación del intervalo con mayor probabilidad
- **Rangos Ampliados**: Probabilidad central (0.35-0.65), límites (2-8), límite superior variable (15-18)

#### **Documentación Especializada**
- **[WALKTHROUGH_INCREMENTO_DIFICULTAD.md](WALKTHROUGH_INCREMENTO_DIFICULTAD.md)** - Guía detallada de mejoras
- **[TECHNICAL_DOCUMENTATION.md](TECHNICAL_DOCUMENTATION.md)** - Especificaciones técnicas completas

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

| Característica | PNG (v1) | TikZ (tikz_v1) | **Cloze Avanzado (cloze_v1_2)** ⭐ |
|---|---|---|---|
| **Generación de tablas** | Python/matplotlib → PNG | TikZ/LaTeX → PDF/PNG | **TikZ dinámico + Cloze** |
| **Calidad visual** | Rasterizada (fija) | Vectorial (escalable) | **Vectorial adaptativo** |
| **Tipo de evaluación** | Selección múltiple | Selección múltiple | **Cloze (9 elementos)** |
| **Nivel de dificultad** | 2 (Media) | 2 (Media) | **3 (Media-Alta)** |
| **Precisión requerida** | Básica | Básica | **3 decimales (0.XXX)** |
| **Análisis complementario** | No | No | **Sí (probabilidades inversas)** |
| **Variabilidad** | ~300 versiones | ~300 versiones | **5,208 versiones** |
| **Gráficos** | Estáticos | Estáticos | **Dinámicos (centro variable)** |
| **Dependencias** | reticulate + matplotlib | TikZ nativo | **TikZ nativo** |
| **Compatibilidad** | Todos los formatos | Todos los formatos | **Todos los formatos** |
| **Mantenimiento** | Dos lenguajes (R+Python) | R puro | **R puro** |
| **Tipografía** | matplotlib fonts | LaTeX fonts (consistente) | **LaTeX fonts (consistente)** |
| **Tamaño archivos** | PNG ~50KB cada tabla | PDF ~15KB cada tabla | **PDF ~20KB optimizado** |

## Archivos Principales

### Archivos de Ejercicio
- `probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd` - Versión PNG con Python/matplotlib
- `probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd` - Versión TikZ vectorial
- **`probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1_2.Rmd`** ⭐ - **Versión avanzada con incremento de dificultad**
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
- `README.md` - Este archivo (índice general)
- **`WALKTHROUGH_INCREMENTO_DIFICULTAD.md`** ⭐ - **Guía detallada de mejoras v1_2**
- **`TECHNICAL_DOCUMENTATION.md`** ⭐ - **Especificaciones técnicas v1_2**
- `WALKTHROUGH.md` - Guía paso a paso (versiones anteriores)
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

### Variables Aleatorias (Versiones v1 y tikz_v1)
```r
p_central <- sample(seq(0.40, 0.55, by = 0.01), 1)  # Probabilidad central
p_lateral <- (1 - p_central) / 2                    # Probabilidades laterales
limite1 <- sample(3:6, 1)                           # Primer límite
ancho_central <- sample(2:6, 1)                     # Ancho del intervalo central
limite2 <- limite1 + ancho_central                  # Segundo límite
limite_sup <- 14                                    # Límite superior fijo
```

### 🆕 Variables Aleatorias Ampliadas (Versión cloze_v1_2)
```r
# Rangos ampliados para mayor variabilidad y dificultad
p_central <- sample(seq(0.35, 0.65, by = 0.01), 1)  # Probabilidad central (31 valores)
p_lateral <- (1 - p_central) / 2                    # Probabilidades laterales
limite1 <- sample(2:8, 1)                           # Primer límite (7 valores)
ancho_central <- sample(3:8, 1)                     # Ancho del intervalo central (6 valores)
limite2 <- limite1 + ancho_central                  # Segundo límite
limite_sup <- sample(15:18, 1)                      # Límite superior variable (4 valores)

# Gráfico dinámico
centro_distribucion <- (limite1 + limite2) / 2      # Centro variable
desviacion_std <- max(2.0, (limite_sup - limite1) / 6)  # Desviación adaptativa

# Total combinaciones: 31 × 7 × 6 × 4 = 5,208 versiones únicas
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

# 🆕 Nueva versión avanzada con incremento de dificultad
exams2pdf("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1_2.Rmd", n=1, dir="salida")

# HTML
rmarkdown::render("probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd", "html_document")
rmarkdown::render("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd", "html_document")
exams2html("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1_2.Rmd", n=1, dir="salida")

# DOCX
exams2pandoc("probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd", n=1, dir="salida")
exams2pandoc("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd", n=1, dir="salida")
exams2pandoc("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1_2.Rmd", n=1, dir="salida")

# Moodle
exams2moodle("probabilidad_intervalos_curva_interpretacion_representacion_n2_v1.Rmd", n=1, dir="salida")
exams2moodle("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_v1.Rmd", n=1, dir="salida")
exams2moodle("probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1_2.Rmd", n=1, dir="salida")
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
