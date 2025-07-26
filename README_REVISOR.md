# Sistema Revisor-Rmd

Sistema integral de validación y corrección para archivos R Markdown (.Rmd) del proyecto "RepositorioMatematicasICFES_R_Exams". Diseñado específicamente para preguntas matemáticas de opción múltiple (schoice) con única respuesta válida.

## 🎯 Características Principales

- **Validación de Código**: Análisis de sintaxis R/Python, buenas prácticas y compatibilidad
- **Validación Matemática**: Coherencia numérica, principios matemáticos y exactitud de fórmulas
- **Validación Visual**: Dimensiones, legibilidad, márgenes y calidad gráfica
- **Validación Lingüística**: Concordancia, ortografía, terminología y estilo académico
- **Validación de Lógica**: Coherencia pregunta-opciones, respuestas únicas y distractores
- **Validación de Imágenes**: Formatos, dimensiones, accesibilidad y contenido matemático
- **Validaciones Especializadas**: Específicas del proyecto ICFES (diversidad, semillas, plantillas)

## 🚀 Instalación

### Instalación Automática (Recomendada)

```bash
# Ejecutar instalador automático
./install.sh
```

### Instalación Manual

```bash
# Hacer ejecutable el script principal
chmod +x revisor-rmd.sh

# Instalar dependencias R
Rscript -e "install.packages(c('exams', 'yaml', 'stringr', 'tools', 'digest', 'parallel'), repos='https://cran.r-project.org')"

# Crear directorios necesarios
mkdir -p logs reports backups .cache
```

## 📖 Uso Básico

### Ejemplos de Uso

```bash
# Validar un archivo específico
./revisor-rmd.sh ejercicio.Rmd

# Validar todos los archivos en un directorio
./revisor-rmd.sh /ruta/a/ejercicios/

# Generar reporte HTML
./revisor-rmd.sh ejercicio.Rmd --format html --output reportes/

# Usar configuración personalizada
./revisor-rmd.sh ejercicio.Rmd --config mi_config.yaml

# Aplicar correcciones automáticas con backup
./revisor-rmd.sh ejercicio.Rmd --auto-fix --backup

# Validación básica en modo silencioso
./revisor-rmd.sh directorio/ --strictness basic --quiet
```

## 🔍 Tipos de Validación

### 1. Validación de Código
- Sintaxis correcta de R y Python
- Uso de librerías requeridas (exams, reticulate, matplotlib)
- Buenas prácticas de programación
- Compatibilidad con exams2pdf, exams2moodle, etc.

### 2. Validación Matemática
- Coherencia numérica (porcentajes suman 100%)
- Rangos válidos (probabilidades entre 0-1)
- Precisión decimal apropiada
- Principios matemáticos básicos

### 3. Validación Visual
- Dimensiones de figuras (recomendado: 4.0x3.2, 3.8x3.5)
- Tamaños de fuente legibles (9-10 pts)
- Márgenes apropiados para papel legal
- Configuración de plt.subplots_adjust()

### 4. Validación Lingüística
- Concordancia de género: "un/una" según sustantivo
- Ortografía y acentuación
- Terminología matemática consistente
- Estilo académico formal

### 5. Validación de Lógica
- Metadatos requeridos (extype, exsolution, exname)
- Coherencia pregunta-opciones
- Respuesta única correcta
- Distractores plausibles

### 6. Validación de Imágenes (NUEVO)
- **Detección automática** de referencias a imágenes en múltiples formatos:
  - Markdown: `![alt text](imagen.png)`
  - HTML: `<img src="imagen.png">`
  - R knitr: `include_graphics("imagen.png")`
  - LaTeX: `\includegraphics{imagen.png}`

- **Validación de existencia**: Verificar que archivos de imagen existan
- **Formatos apropiados**: PNG, PDF, SVG preferidos sobre JPG/JPEG
- **Propiedades técnicas**: Tamaño de archivo, dimensiones, resolución
- **Accesibilidad**: Texto alternativo descriptivo
- **Contenido matemático**: Detección de imágenes con contenido matemático
- **Organización**: Sugerencias para estructura de directorios

### 7. Validaciones Especializadas
- Compatibilidad con estándares ICFES
- Diversidad de versiones (≥300 únicas)
- Semillas consistentes para reproducibilidad
- Compatibilidad con plantillas LaTeX específicas

## 🧪 Estado del Proyecto

### ✅ Funcionalidades Implementadas

- [x] **Sistema de orquestación completo** (revisor-rmd.sh + main_validator.R)
- [x] **7 módulos de validación especializados**:
  - Validación de código R/Python
  - Validación matemática (coherencia numérica, fórmulas)
  - Validación visual (dimensiones, legibilidad)
  - Validación lingüística (concordancia, ortografía)
  - Validación de lógica (estructura de ejercicios)
  - **Validación de imágenes (NUEVO)** (formatos, accesibilidad, contenido)
  - Validaciones especializadas ICFES
- [x] **Sistema de configuración YAML** personalizable
- [x] **Generador de reportes** (HTML, PDF, consola, JSON)
- [x] **Utilidades auxiliares** (logging, parsing, manejo de archivos)
- [x] **Documentación completa** (manual de usuario, guía de desarrollo)
- [x] **Ejemplos y casos de prueba**
- [x] **Instalador automático**

### 🔬 Validaciones de Imágenes Implementadas

| Aspecto | Validaciones | Estado |
|---------|-------------|--------|
| **Detección** | Markdown, HTML, knitr, LaTeX | ✅ |
| **Existencia** | Verificación de archivos | ✅ |
| **Formatos** | PNG, PDF, SVG (preferidos), JPG, GIF (permitidos) | ✅ |
| **Técnicas** | Tamaño archivo, dimensiones | ✅ |
| **Accesibilidad** | Texto alternativo, longitud mínima | ✅ |
| **Contenido** | Detección de contenido matemático | ✅ |
| **Organización** | Estructura de directorios, nomenclatura | ✅ |

## 🚀 Demostración con Imágenes

### Ejemplo de Uso con Imágenes

```bash
# Validar archivo con imágenes
./revisor-rmd.sh examples/good/ejemplo_con_imagen.Rmd --format console

# Resultado:
# ✅ Imagen encontrada: cuadrado_ejemplo.png
# ✅ Formato de imagen apropiado: png
# ✅ Texto alternativo apropiado
```

### Ejemplo con Problemas de Imágenes

```bash
# Validar archivo con problemas
./revisor-rmd.sh examples/bad/ejemplo_imagen_problemas.Rmd --format console

# Resultado:
# ❌ Errores encontrados:
#   • Imagen no encontrada: imagenes/figura_inexistente.png
#   • Imagen sin texto alternativo (alt text)
#   • Formato de imagen no óptimo: bmp
```

## 📊 Métricas del Sistema

- **Líneas de código**: ~3,000 líneas
- **Módulos de validación**: 7 módulos especializados
- **Tipos de referencias de imagen**: 4 formatos detectados
- **Formatos de reporte**: 4 formatos (HTML, PDF, consola, JSON)
- **Ejemplos incluidos**: 4 archivos de prueba (incluyendo casos con imágenes)

## 📞 Soporte

Para soporte técnico o preguntas sobre el sistema:

1. **Consultar documentación**: `docs/user-guide/manual-usuario.md`
2. **Revisar logs**: `logs/revisor-rmd.log`
3. **Probar con ejemplos**: `examples/`
4. **Verificar configuración**: `config/`

## 🎉 Conclusión

El **Sistema Revisor-Rmd** ahora incluye **validación completa de imágenes**, una funcionalidad crítica para archivos R Markdown que contienen escenarios matemáticos ICFES con imágenes de referencia. El sistema puede:

- ✅ **Detectar automáticamente** referencias a imágenes en múltiples formatos
- ✅ **Validar existencia** y propiedades técnicas de archivos de imagen
- ✅ **Verificar accesibilidad** con texto alternativo descriptivo
- ✅ **Sugerir mejores prácticas** para organización y formatos
- ✅ **Generar reportes detallados** con información específica sobre imágenes

Esta nueva funcionalidad asegura que los ejercicios matemáticos con componentes visuales cumplan con todos los estándares de calidad, accesibilidad y compatibilidad técnica del proyecto RepositorioMatematicasICFES_R_Exams.
