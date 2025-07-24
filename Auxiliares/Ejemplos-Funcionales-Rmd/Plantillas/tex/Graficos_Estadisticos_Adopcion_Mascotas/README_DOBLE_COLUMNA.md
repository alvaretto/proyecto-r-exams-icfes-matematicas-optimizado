# Generador de Exámenes con Doble Columna y Tamaño Legal

## Descripción

Este sistema permite generar exámenes en formato **legal (8.5" x 14")** con **doble columna y línea central** usando R/exams. Los formatos de salida incluyen:

- **exams2pdf**: Documentos PDF imprimibles
- **exams2nops**: Exámenes escaneables 
- **exams2pandoc**: Documentos Word (DOCX)

## Características Principales

### Formato de Página
- **Tamaño**: Legal (8.5" x 14" / 21.59 cm x 35.56 cm)
- **Columnas**: Doble columna con línea central divisoria
- **Márgenes**: Optimizados para máximo aprovechamiento del espacio
- **Separación**: 0.5 cm entre columnas

### Optimizaciones para Doble Columna
- **Gráficos**: Ancho máximo 3.4 pulgadas, altura 2.5 pulgadas
- **Resolución**: 300 DPI para alta calidad de impresión
- **Fuentes**: Tamaño optimizado para legibilidad en columnas
- **Espaciado**: Ajustado para mejor distribución del contenido

## Archivos Incluidos

### Plantillas LaTeX
- `plain_legal_2col.tex`: Plantilla básica para PDF
- `exam_legal_2col.tex`: Plantilla para exámenes escaneables (NOPS)
- `pcielo_legal_2col.tex`: Plantilla institucional personalizada

### Scripts R
- `SemilleroTotal_2col_v1.R`: Script principal adaptado para doble columna
- `test_doble_columna.R`: Script de prueba rápida
- `prueba_doble_columna.Rmd`: Archivo de ejemplo para pruebas

## Uso Básico

### Opción 1: Usar el Script Principal

```r
# Cargar el script
source("SemilleroTotal_2col_v1.R")

# El script se ejecutará automáticamente con interfaz interactiva
# Seleccionar opción 7 para "DOBLE COLUMNA"
```

### Opción 2: Ejecutar Prueba Rápida

```r
# Ejecutar script de prueba
source("test_doble_columna.R")
```

### Opción 3: Uso Directo

```r
library(exams)

# Configurar gráficos para doble columna
knitr::opts_chunk$set(
  fig.width = 3.4,
  fig.height = 2.5,
  dpi = 300,
  out.width = "\\columnwidth"
)

# Generar PDF
exams2pdf("tu_archivo.Rmd",
          n = 5,
          template = "plain_legal_2col.tex",
          dir = "salida")

# Generar NOPS
exams2nops("tu_archivo.Rmd",
           n = 5,
           template = "exam_legal_2col.tex",
           dir = "salida")

# Generar Pandoc
exams2pandoc("tu_archivo.Rmd",
             n = 5,
             template = "plain_legal_2col.tex",
             type = "docx",
             dir = "salida")
```

## Recomendaciones para Contenido

### Gráficos
- **Ancho máximo**: 3.4 pulgadas
- **Altura recomendada**: 2.5 pulgadas o menos
- **Formato**: Usar `out.width = "\\columnwidth"` en chunks de R
- **Calidad**: DPI mínimo de 300 para impresión

### Tablas
- **Ancho**: Máximo 3 columnas para ajustarse bien
- **Texto**: Usar fuentes más pequeñas si es necesario
- **Formato**: Preferir tablas verticales sobre horizontales

### Texto
- **Párrafos**: Mantener párrafos cortos
- **Listas**: Usar viñetas compactas
- **Fórmulas**: Verificar que se ajusten al ancho de columna

## Estructura de Directorios

```
Graficos_Estadisticos_Adopcion_Mascotas/
├── SemilleroTotal_2col_v1.R          # Script principal
├── test_doble_columna.R               # Script de prueba
├── prueba_doble_columna.Rmd           # Archivo de ejemplo
├── plain_legal_2col.tex              # Plantilla PDF básica
├── exam_legal_2col.tex               # Plantilla NOPS
├── pcielo_legal_2col.tex             # Plantilla institucional
├── README_DOBLE_COLUMNA.md            # Este archivo
└── salida_legal_2col/                 # Directorio de salida
    ├── pdf/
    ├── nops/
    └── pandoc/
```

## Solución de Problemas

### Error: "Template not found"
- Verificar que las plantillas .tex estén en el directorio correcto
- Usar rutas absolutas si es necesario

### Gráficos muy grandes
- Ajustar `fig.width` y `fig.height` en chunks de R
- Usar `out.width = "\\columnwidth"`

### Texto que se sale de las columnas
- Reducir tamaño de fuente
- Dividir contenido en párrafos más cortos
- Usar saltos de línea manuales si es necesario

### LaTeX no disponible
```bash
# Ubuntu/Debian
sudo apt install texlive-latex-extra texlive-fonts-recommended

# Verificar instalación
pdflatex --version
```

## Ejemplos de Configuración

### Para archivos .Rmd existentes
Agregar al inicio del chunk de configuración:

```r
# Configuración para doble columna
knitr::opts_chunk$set(
  fig.width = 3.4,
  fig.height = 2.5,
  dpi = 300,
  out.width = "\\columnwidth",
  fig.align = "center"
)
```

### Para gráficos ggplot2
```r
ggplot(datos, aes(x, y)) +
  geom_point() +
  theme_minimal() +
  theme(
    text = element_text(size = 10),
    plot.title = element_text(size = 12),
    axis.text = element_text(size = 8)
  )
```

## Contacto y Soporte

Para reportar problemas o sugerir mejoras, contactar al equipo de desarrollo del proyecto R-Exams de la I.E. Pedacito de Cielo.

---

**Fecha de creación**: 2025-01-23  
**Versión**: 1.0  
**Autor**: Transformación Pedagógica R-Exams
