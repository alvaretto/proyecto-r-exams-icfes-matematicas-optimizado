# Ejercicio: Proporciones en Encuesta Deportiva

## Descripción
Ejercicio de matemáticas para ICFES que evalúa la interpretación de gráficos de barras horizontales y el manejo de proporciones en el contexto de encuestas deportivas.

## Archivo Principal
- **`proporciones_encuesta_deportiva_v1.Rmd`**: Archivo principal del ejercicio

## Competencias Evaluadas
- **Competencia**: Interpretación y representación
- **Componente**: Aleatorio y sistemas de datos
- **Afirmación**: Interpreta información presentada en tablas y gráficos
- **Nivel de dificultad**: Medio
- **Tiempo estimado**: 3 minutos

## Características del Ejercicio

### Aleatorización Avanzada (>10 parámetros)
1. **Contexto**: 6 tipos diferentes (canal deportes, streaming, etc.)
2. **Competición**: 6 competiciones (Champions, Mundial, etc.)
3. **Términos usuarios**: 5 variaciones (suscriptores, seguidores, etc.)
4. **Equipos**: 15 equipos disponibles, selección aleatoria de 5
5. **Población total**: 7 valores posibles (30K-100K)
6. **Tamaño muestra**: 8 valores posibles (80-150)
7. **Valores del gráfico**: Generación coherente que suma exactamente la muestra
8. **Colores**: 5 paletas diferentes para el gráfico
9. **Orden equipos**: Aleatorización con el mayor valor siempre primero
10. **Distractores**: Generación dinámica de errores plausibles

### Tecnologías Utilizadas
- **Gráfico de barras**: Código Python con matplotlib (reticulate)
- **Expresiones matemáticas**: LaTeX integrado
- **Estructura**: R-exams compatible con múltiples formatos

### Validaciones Matemáticas
- Suma exacta de valores = tamaño de muestra
- Rangos apropiados (8-40% del total por equipo)
- Variabilidad mínima (≥2 valores únicos)
- Valores positivos
- Ningún equipo domina >40%

## Problema Matemático
El ejercicio presenta una encuesta sobre preferencias de equipos para ganar una competición deportiva. Los estudiantes deben:

1. Interpretar un gráfico de barras horizontales
2. Distinguir entre muestra y población total
3. Aplicar correctamente las proporciones
4. Evitar errores comunes de interpretación

### Respuesta Correcta
La opción correcta interpreta que los datos del gráfico se refieren a la **muestra** (ej: 80-150 personas), no a la población total (ej: 30,000-100,000 usuarios).

### Distractores Típicos
- Confundir muestra con población total
- Generalizar incorrectamente sobre equipos no encuestados
- Malinterpretar el tamaño de la muestra

## Archivos de Soporte
- **`ejecutar_pruebas_encuesta_deportiva.R`**: Script de pruebas completas
- **`README_encuesta_deportiva.md`**: Esta documentación

## Formatos de Salida Compatibles
- **PDF**: `exams2pdf()` con template "solpcielo"
- **DOCX**: `exams2pandoc()` con template "pcielo.tex"
- **HTML**: `exams2html()` para visualización
- **Moodle XML**: `exams2moodle()` para LMS
- **NOPS**: `exams2nops()` para exámenes escaneables

## Uso del Ejercicio

### Ejecución Básica
```r
library(exams)

# Generar una versión HTML
exams2html("proporciones_encuesta_deportiva_v1.Rmd")

# Generar PDF
exams2pdf("proporciones_encuesta_deportiva_v1.Rmd", 
          template = "solpcielo")

# Generar para Moodle
exams2moodle("proporciones_encuesta_deportiva_v1.Rmd")
```

### Ejecución de Pruebas
```r
source("ejecutar_pruebas_encuesta_deportiva.R")
```

## Variabilidad del Ejercicio
- **Mínimo 300 variantes distintas** garantizadas
- **Coherencia matemática** en todas las versiones
- **Distractores adaptativos** según los valores generados
- **Contextos realistas** y variados

## Correcciones Aplicadas
- **Función `generar_valores_coherentes()`**: Completamente reescrita para garantizar suma exacta
- **Validaciones robustas**: Sin dependencias externas (testthat)
- **Manejo de intercambios**: Actualización correcta de variables después de reordenamientos
- **Verificaciones múltiples**: Suma, rangos, variabilidad, positividad

## Estado del Ejercicio
✅ **COMPLETAMENTE FUNCIONAL**
✅ **VALIDADO MATEMÁTICAMENTE**
✅ **LISTO PARA PRODUCCIÓN**

## Autor
Sistema R-Exams ICFES - Enero 2025

## Notas Técnicas
- Requiere R con paquetes: `exams`, `reticulate`, `knitr`
- Python necesario para generación de gráficos
- Compatible con múltiples sistemas operativos
- Optimizado para rendimiento y estabilidad
