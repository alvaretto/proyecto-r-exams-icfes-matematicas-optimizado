---
output:
  html_document: default
  word_document: default
  pdf_document: default
---

# Fracciones y Reparto Proporcional - 22-S2-2025-SEDQ

<div align="center">
  <img src="https://www.r-exams.org/images/rexams_logo.svg" alt="R-exams Logo" width="150"/>
</div>

## Descripción General

Este ejercicio implementa problemas de **fracciones y reparto proporcional** en contextos deportivos y de competencias. Los estudiantes deben calcular montos exactos de premios distribuidos entre diferentes puestos usando fracciones, desarrollando competencias en pensamiento numérico y resolución de problemas.

## Características Principales

- **🎯 Alta aleatorización**: Más de 300 variantes únicas por ejercicio
- **📊 Visualización dinámica**: Tablas generadas con TikZ y colores aleatorios
- **🔄 Múltiples versiones**: 4 versiones del ejercicio (v1-v4) con diferentes enfoques
- **✅ Pruebas unitarias**: Validación automática de coherencia matemática
- **📱 Múltiples formatos**: PDF, HTML, Word, Moodle XML, NOPS
- **🎓 Metadatos ICFES**: Alineado con estándares educativos colombianos

## Metadatos ICFES

```yaml
icfes:
  competencia: Resolución de problemas
  componente: Numérico-variacional
  afirmacion: Resuelve problemas que requieren el uso de fracciones y porcentajes
  evidencia: Utiliza fracciones para resolver problemas de reparto proporcional
  nivel: Medio
  tematica: Fracciones y operaciones con fracciones
```

## Estructura del Ejercicio

### Contexto del Problema
- **Escenario**: Competencias deportivas en diferentes contextos (ciudades, municipios, etc.)
- **Participantes**: Grupos de edad variados (menores de 15 años, categoría juvenil, etc.)
- **Premios**: Montos entre 30-400 millones de pesos (múltiplos de 2)
- **Distribución**: Tres puestos con fracciones específicas

### Lógica Matemática (Versión 4 - Más Reciente)
1. **Primer puesto**: Recibe una fracción directa del premio total
2. **Segundo puesto**: Recibe una fracción del dinero restante después del primer puesto
3. **Tercer puesto**: Recibe el dinero restante

Esta lógica garantiza matemáticamente que: **primer puesto > segundo puesto > tercer puesto**

### Conjuntos de Fracciones Validados
```
c("3/8", "4/7")   # 37.50% > 35.71% > 26.79%
c("2/5", "3/5")   # 40.00% > 36.00% > 24.00%
c("1/2", "2/3")   # 50.00% > 33.33% > 16.67%
c("4/9", "3/4")   # 44.44% > 41.67% > 13.89%
c("5/12", "2/3")  # 41.67% > 38.89% > 19.44%
```

## Archivos Principales

### Ejercicios (.Rmd)
- **`fracciones_reparto_premio_v1.Rmd`**: Versión básica - pregunta por el tercer puesto
- **`fracciones_reparto_premio_v2.Rmd`**: Versión mejorada con nueva lógica
- **`fracciones_reparto_premio_v3.Rmd`**: Versión optimizada
- **`fracciones_reparto_premio_v4.Rmd`**: Versión más reciente - pregunta por el segundo puesto

### Scripts de Generación
- **`SemilleroUnico_v2.R`**: Genera 1-5 copias en múltiples formatos
- **`SemilleroMoodle_v2.R`**: Genera 500 preguntas para bancos masivos

### Pruebas y Validación
- **`pruebas_unitarias_v1.R`**: Pruebas especializadas para la versión 1
- **`reporte_pruebas_v1_*.RData`**: Reportes de ejecución de pruebas

### Documentación
- **`walkthrough.md`**: Análisis técnico detallado del código
- **`docus/twitter.md`**: Resumen promocional del ejercicio

## Aleatorización Implementada

### Variables Contextuales
- **Contextos**: 12 tipos (ciudad, municipio, distrito, etc.) con concordancia de género
- **Competencias**: 12 tipos (carrera, maratón, torneo, etc.) con concordancia de género
- **Grupos de edad**: 10 variantes (menores de 15 años, categoría juvenil, etc.)
- **Términos del enunciado**: 5 variantes por cada término clave

### Variables Matemáticas
- **Premios totales**: 186 valores posibles (30-400 millones, múltiplos de 2)
- **Conjuntos de fracciones**: 10 combinaciones validadas matemáticamente
- **Paletas de colores**: 5 esquemas profesionales para visualización

### Estrategia de Distractores (v4)
- **Generación aleatoria**: Sin patrones predecibles
- **Rangos amplios**: Desde 15% hasta 450% del valor correcto
- **Errores conceptuales**: Confusiones con otros puestos
- **Selección aleatoria**: Los 3 distractores se eligen completamente al azar

## Guía de Uso

### Generación Rápida
```r
# Cargar librería
library(exams)

# Generar 5 versiones en PDF
exams2pdf("fracciones_reparto_premio_v4.Rmd", n = 5)

# Generar para Moodle
exams2moodle("fracciones_reparto_premio_v4.Rmd", n = 30)
```

### Generación Masiva
```r
# Ejecutar script de generación
source("SemilleroMoodle_v2.R")  # 500 preguntas para Moodle
source("SemilleroUnico_v2.R")   # Múltiples formatos
```

### Ejecución de Pruebas
```r
# Ejecutar pruebas unitarias
source("pruebas_unitarias_v1.R")
```

## Formatos de Salida

### Archivos Generados (carpeta `salida/`)
- **PDF**: `fracciones_reparto_premio_v*_*.pdf`
- **Word**: `fracciones_reparto_premio_v*_*.docx`
- **Moodle XML**: `fracciones_reparto_premio_v*_.xml`
- **NOPS**: `fracciones_reparto_premio_v*_nops_*.pdf`

### Visualizaciones
- **Tablas TikZ**: Distribución de premios con colores aleatorios
- **Gráficos auxiliares**: `tabla_distribucion.png/pdf`

## Evolución de Versiones

### v1 → v2
- Implementación de nueva lógica matemática
- Mejora en la aleatorización de contextos

### v2 → v3
- Optimización de cálculos
- Ampliación del rango de premios

### v3 → v4
- **Cambio de pregunta**: De tercer puesto a segundo puesto
- **Distractores aleatorios**: Eliminación de patrones predecibles
- **Rango ampliado**: Premios de 30-400 millones
- **Concordancia de género**: Sistema mejorado para términos

## Estadísticas del Ejercicio

- **Variantes totales**: 300+ combinaciones únicas
- **Tiempo de generación**: ~2-3 segundos por ejercicio
- **Formatos soportados**: 5 tipos diferentes
- **Validaciones**: 15+ pruebas unitarias automáticas
- **Nivel de dificultad**: Medio (según estándares ICFES)

## Requisitos Técnicos

### Software Necesario
- **R**: Versión 4.0 o superior
- **Paquetes R**: exams, knitr, testthat, reticulate
- **LaTeX**: Para generación de PDF
- **TikZ**: Para gráficos vectoriales

### Dependencias Opcionales
- **Python**: Para visualizaciones avanzadas (vía reticulate)
- **Pandoc**: Para conversión entre formatos

## Contribuciones y Mejoras

### Próximas Mejoras Planificadas
- [ ] Implementación de fracciones mixtas
- [ ] Contextos internacionales (olimpiadas, mundiales)
- [ ] Visualización 3D de distribución de premios
- [ ] Integración con calculadora de fracciones

### Cómo Contribuir
1. Revisar la documentación técnica en `walkthrough.md`
2. Ejecutar pruebas unitarias antes de modificar
3. Mantener la aleatorización robusta (300+ variantes)
4. Documentar cambios en el walkthrough
5. Validar coherencia matemática

## Contacto y Soporte

Para consultas técnicas o sugerencias:
- **Repositorio**: [GitHub - proyecto-r-exams-icfes-matematicas-optimizado](https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado)
- **Documentación técnica**: `walkthrough.md`
- **Ejemplos de uso**: Carpeta `salida/`

---

**Última actualización**: Enero 2025  
**Versión actual**: v4  
**Estado**: Producción - Estable  
**Mantenedor**: Proyecto R-Exams ICFES Matemáticas
