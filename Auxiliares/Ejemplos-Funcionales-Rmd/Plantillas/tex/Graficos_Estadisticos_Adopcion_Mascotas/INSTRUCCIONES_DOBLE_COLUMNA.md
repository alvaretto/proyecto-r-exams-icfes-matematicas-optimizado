# 📋 INSTRUCCIONES COMPLETAS - SISTEMA DOBLE COLUMNA LEGAL

## 🎯 Objetivo
Generar exámenes en formato **legal (8.5" x 14")** con **doble columna y línea central** usando únicamente las salidas:
- **exams2pdf**: Documentos PDF imprimibles
- **exams2nops**: Exámenes escaneables 
- **exams2pandoc**: Documentos Word (DOCX)

## 📁 Archivos Creados

### 1. `SemilleroUnico_v2_DobleColumna.R`
**Archivo principal** - Versión adaptada del SemilleroUnico_v2.R original que:
- ✅ Genera únicamente PDF, NOPS y Pandoc
- ✅ Usa formato legal (8.5" x 14")
- ✅ Configura doble columna con línea central
- ✅ Optimiza gráficos para columnas (3.4" x 2.5")
- ✅ Usa plantillas .tex especializadas

### 2. `Configuracion_DobleColumna_Rmd.R`
**Herramientas de configuración** - Funciones para:
- ✅ Adaptar archivos .Rmd existentes automáticamente
- ✅ Verificar disponibilidad de plantillas
- ✅ Copiar plantillas al directorio actual
- ✅ Configuración manual paso a paso

## 🚀 Uso Rápido

### Opción 1: Uso Directo (Recomendado)
```r
# 1. Editar el archivo principal
# Cambiar línea 20 en SemilleroUnico_v2_DobleColumna.R:
archivo_examen <- "tu_archivo.Rmd"  # Poner tu archivo aquí

# 2. Ejecutar
source("SemilleroUnico_v2_DobleColumna.R")
```

### Opción 2: Con Adaptación Automática
```r
# 1. Cargar herramientas
source("Configuracion_DobleColumna_Rmd.R")

# 2. Adaptar tu archivo .Rmd
adaptar_rmd_doble_columna("tu_archivo.Rmd")

# 3. Ejecutar generador
source("SemilleroUnico_v2_DobleColumna.R")
```

## 📊 Configuración de Gráficos

### Automática (Recomendada)
El sistema configura automáticamente:
- **Ancho**: 3.4 pulgadas (ajustado a columnas)
- **Alto**: 2.5 pulgadas (proporción óptima)
- **Resolución**: 300 DPI (calidad de impresión)
- **Ajuste**: `\columnwidth` (se adapta automáticamente)

### Manual (Para archivos .Rmd existentes)
Agregar al inicio del primer chunk de R:
```r
knitr::opts_chunk$set(
  fig.width = 3.4,
  fig.height = 2.5,
  dpi = 300,
  out.width = "\\columnwidth",
  fig.align = "center"
)
```

## 🔧 Plantillas .tex Utilizadas

### Disponibles en el Sistema
- **PDF**: `plain_legal_2col.tex` - Plantilla básica optimizada
- **Pandoc**: `pandoc_legal_2col.tex` - Para documentos Word
- **Alternativas**: `pcielo_legal_2col.tex` - Versión institucional

### Características de las Plantillas
- ✅ Tamaño legal (8.5" x 14" / 21.59 cm x 35.56 cm)
- ✅ Doble columna con línea separadora (0.4pt)
- ✅ Márgenes optimizados (1.5 cm)
- ✅ Separación entre columnas (0.5 cm)
- ✅ Configuración automática de gráficos

## 📂 Estructura de Salida

```
salida_doble_columna/
├── tu_archivo_legal2col_pdf_1.pdf
├── tu_archivo_legal2col_pdf_2.pdf
├── ...
├── tu_archivo_legal2col_nops_1.pdf
├── tu_archivo_legal2col_nops_2.pdf
├── ...
├── tu_archivo_legal2col_docx_1.docx
├── tu_archivo_legal2col_docx_2.docx
└── ...
```

## ⚙️ Configuración Personalizada

### Cambiar Número de Copias
```r
# En SemilleroUnico_v2_DobleColumna.R línea 21:
copias <- 10  # Cambiar por el número deseado
```

### Cambiar Número de Preguntas
```r
# En SemilleroUnico_v2_DobleColumna.R línea 22:
numpreg <- 5  # Cambiar por el número deseado
```

### Cambiar Directorio de Salida
```r
# En SemilleroUnico_v2_DobleColumna.R línea 25:
dir_salida <- "mi_directorio_personalizado"
```

## 🔍 Verificación del Sistema

### Verificar Plantillas Disponibles
```r
source("Configuracion_DobleColumna_Rmd.R")
verificar_plantillas()
```

### Copiar Plantillas al Directorio Actual
```r
source("Configuracion_DobleColumna_Rmd.R")
copiar_plantillas()
```

## 📝 Adaptación de Archivos .Rmd Existentes

### Cambios Necesarios para Doble Columna

1. **Gráficos**: Reducir tamaño para ajustarse a columnas
2. **Tablas**: Máximo 3 columnas, usar formato vertical
3. **Texto**: Párrafos cortos, evitar líneas muy largas
4. **Fórmulas**: Verificar que se ajusten al ancho de columna

### Problemas Comunes y Soluciones

#### Gráficos muy grandes
```r
# Solución: Usar configuración específica en chunks
{r, fig.width=3.2, fig.height=2.3}
```

#### Tablas que se salen
```r
# Solución: Usar tablas más compactas
kable(datos, format = "latex", booktabs = TRUE) %>%
  kable_styling(font_size = 8)
```

#### Chunks de Python problemáticos
```r
# Solución: Comentar o usar eval=FALSE
{python, eval=FALSE}
```

## 🎉 Ventajas del Sistema

- ✅ **Optimización de espacio**: Doble columna aprovecha mejor el papel legal
- ✅ **Legibilidad**: Líneas más cortas son más fáciles de leer
- ✅ **Profesional**: Formato similar a revistas académicas
- ✅ **Eficiencia**: Más contenido por página
- ✅ **Compatibilidad**: Funciona con todos los tipos de examen

## 📞 Soporte

Para problemas o dudas:
1. Verificar que las plantillas estén disponibles
2. Revisar la configuración de gráficos en archivos .Rmd
3. Consultar los archivos de ejemplo en el directorio de plantillas

---

**Fecha**: 2025-01-23  
**Versión**: 1.0  
**Compatibilidad**: R-Exams, LaTeX, Pandoc
