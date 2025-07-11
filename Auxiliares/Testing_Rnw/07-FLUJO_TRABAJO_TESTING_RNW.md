# 🧪 FLUJO DE TRABAJO PARA TESTING DE ARCHIVOS .RNW

## 📋 Descripción General

Este documento describe el flujo de trabajo completo para realizar testing de archivos `.Rnw` (LaTeX + R) en el proyecto ICFES R-exams. El sistema permite validar la estructura, ejecutar código R y Python, verificar coherencia matemática y probar la compilación a diferentes formatos.

## 🎯 Objetivos del Sistema de Testing

1. **Validar estructura**: Verificar que los archivos `.Rnw` tengan la estructura LaTeX correcta
2. **Ejecutar código R**: Extraer y ejecutar chunks de R para detectar errores
3. **Validar Python**: Probar chunks de Python con matplotlib y numpy
4. **Coherencia matemática**: Verificar que los cálculos y datos sean coherentes
5. **Compilación**: Probar que los archivos se compilen correctamente a HTML, PDF y Moodle

## 📁 Estructura de Archivos

```
Auxiliares/
├── testing_rnw_utils.R              # Utilidades básicas para .Rnw
├── testing_python_chunks.R          # Testing específico de Python
├── testing_coherencia_matematica.R  # Validación matemática
├── testing_compilacion_rnw.R        # Testing de compilación
└── test_rnw_completo.R              # Script principal integrado
```

## 🚀 Uso Rápido

### Opción 1: Script desde línea de comandos
```bash
# Testear un archivo específico
Rscript test_rnw_completo.R archivo.Rnw

# Testear archivo en directorio actual (detección automática)
Rscript test_rnw_completo.R
```

### Opción 2: Desde R interactivo / RStudio
```r
# Configurar directorio de trabajo
setwd("Auxiliares/Testing_Rnw")

# Cargar el script principal
source("06-test_rnw_completo.R")

# Ejecutar suite completa
resultados <- ejecutar_test_suite_completa("../../archivo.Rnw")

# Ejecutar sin compilación (más rápido)
resultados <- ejecutar_test_suite_completa("../../archivo.Rnw", incluir_compilacion = FALSE)
```

### Opción 3: Testing Avanzado en RStudio
```r
# === CONFIGURACIÓN COMPLETA EN RSTUDIO ===

# 1. Configurar entorno
setwd("Auxiliares/Testing_Rnw")

# 2. Verificar dependencias (primera vez)
source("08-configurar_testing.R")

# 3. Cargar sistema según necesidad:

# Para testing rápido:
source("05-test_rnw_simple.R")
resultados_rapidos <- test_basico_rnw("../../archivo.Rnw")

# Para testing completo:
source("06-test_rnw_completo.R")
resultados_completos <- ejecutar_test_suite_completa("../../archivo.Rnw")

# Para testing modular:
source("01-testing_rnw_utils.R")
validacion <- validar_estructura_rnw("../../archivo.Rnw")
codigo <- extraer_codigo_r_desde_rnw("../../archivo.Rnw")
ejecucion <- ejecutar_codigo_r_extraido(codigo)
```

## 🔧 Configuración Inicial

### Dependencias Requeridas

**Paquetes R:**
```r
install.packages(c("testthat", "exams", "knitr", "reticulate", "stringr", "tools"))
```

**Software del sistema:**
- Python 3.x con matplotlib, numpy
- LaTeX (pdflatex) para compilación PDF
- Pandoc para conversiones

### Verificar Configuración
```r
# Cargar utilidades
source("Auxiliares/testing_rnw_utils.R")

# Verificar que todo esté configurado
configurar_entorno_compilacion()
```

## 📊 Fases del Testing

### Fase 1: Validación de Estructura
- ✅ Verifica `\documentclass`, `\begin{document}`, `\end{document}`
- ✅ Comprueba balance de chunks (`<<>>=` y `@`)
- ✅ Detecta presencia de `\begin{question}` y `\begin{solution}`
- ✅ Calcula puntuación de validez estructural

### Fase 2: Extracción y Ejecución de Código R
- ✅ Extrae todos los chunks de R del archivo
- ✅ Ejecuta el código en entorno controlado
- ✅ Captura errores y warnings
- ✅ Mide tiempo de ejecución

### Fase 3: Validación de Chunks Python
- ✅ Detecta chunks con `engine='python'`
- ✅ Configura matplotlib en modo no interactivo
- ✅ Ejecuta código Python y valida generación de archivos
- ✅ Verifica disponibilidad de librerías (numpy, matplotlib)

### Fase 4: Coherencia Matemática
- ✅ Extrae variables matemáticas (porcentajes, precios, cantidades)
- ✅ Valida rangos numéricos (porcentajes 0-100, precios positivos)
- ✅ Verifica coherencia entre respuestas correctas y distractores
- ✅ Valida cálculos matemáticos específicos

### Fase 5: Compilación (Opcional)
- ✅ Compila a HTML con exams2html()
- ✅ Compila a PDF con exams2pdf() (requiere LaTeX)
- ✅ Compila a Moodle XML con exams2moodle()
- ✅ Valida que los archivos generados sean correctos

## 📈 Interpretación de Resultados

### Estados Generales
- **✅ APROBADO**: ≥80% tests exitosos, sin errores críticos
- **⚠️ APROBADO CON OBSERVACIONES**: 60-79% tests exitosos
- **❌ REPROBADO**: <60% tests exitosos o errores críticos

### Métricas Clave
- **Puntuación de validez estructural**: Debe ser ≥80%
- **Tiempo de ejecución R**: Debe ser <30 segundos
- **Porcentaje éxito Python**: Debe ser ≥80% si hay chunks Python
- **Coherencia matemática**: Debe ser TRUE
- **Porcentaje compilación**: Debe ser ≥80% para formatos testeados

## 🛠️ Funciones Principales

### Utilidades Básicas (`testing_rnw_utils.R`)
```r
# Extraer código R de archivo .Rnw
codigo <- extraer_codigo_r_desde_rnw("archivo.Rnw")

# Ejecutar código extraído
resultado <- ejecutar_codigo_r_extraido(codigo)

# Validar estructura del archivo
validacion <- validar_estructura_rnw("archivo.Rnw")

# Crear suite de tests personalizada
test_suite <- crear_test_suite_rnw("archivo.Rnw")
test_suite()  # Ejecutar
```

### Testing de Python (`testing_python_chunks.R`)
```r
# Configurar Python para testing
configurar_python_testing()

# Extraer solo chunks de Python
chunks_python <- extraer_chunks_python("archivo.Rnw")

# Validar todos los chunks Python
validacion <- validar_chunks_python_rnw("archivo.Rnw")
```

### Coherencia Matemática (`testing_coherencia_matematica.R`)
```r
# Validar coherencia completa (requiere entorno de ejecución)
validacion <- validar_coherencia_completa(entorno)

# Crear tests de coherencia
test_coherencia <- crear_test_coherencia_rnw(entorno)
test_coherencia()
```

### Compilación (`testing_compilacion_rnw.R`)
```r
# Testear compilación completa
resultados <- test_compilacion_completa("archivo.Rnw", c("html", "moodle"))

# Compilar formato específico
resultado_html <- compilar_a_html("archivo.Rnw")
resultado_pdf <- compilar_a_pdf("archivo.Rnw")
resultado_moodle <- compilar_a_moodle("archivo.Rnw")
```

## 🎯 Guía Específica para RStudio

### Configuración Inicial en RStudio
```r
# === PRIMERA VEZ EN RSTUDIO ===

# 1. Abrir RStudio en el directorio del proyecto
# 2. Configurar directorio de trabajo
setwd("Auxiliares/Testing_Rnw")

# 3. Verificar que todo esté configurado
source("08-configurar_testing.R")

# 4. Leer documentación si es necesario
file.edit("00-README.md")
file.edit("07-FLUJO_TRABAJO_TESTING_RNW.md")
```

### Flujo de Trabajo Diario en RStudio
```r
# === USO DIARIO EN RSTUDIO ===

# 1. Configurar directorio (siempre)
setwd("Auxiliares/Testing_Rnw")

# 2. Para desarrollo rápido (recomendado)
source("05-test_rnw_simple.R")
archivo <- "../../ruta/al/archivo.Rnw"
resultados <- test_basico_rnw(archivo)

# 3. Ver resultados inmediatos
print(resultados$estado)
print(paste("Éxito:", resultados$porcentaje_exito, "%"))

# 4. Si hay problemas, usar debugging
if (resultados$porcentaje_exito < 100) {
  source("01-testing_rnw_utils.R")
  validacion <- validar_estructura_rnw(archivo)
  print(validacion$errores)
}
```

### Testing Completo en RStudio (Antes de Commits)
```r
# === TESTING COMPLETO EN RSTUDIO ===

# 1. Cargar sistema completo
source("06-test_rnw_completo.R")

# 2. Ejecutar testing exhaustivo
archivo <- "../../ruta/al/archivo.Rnw"
resultados <- ejecutar_test_suite_completa(
  archivo,
  incluir_compilacion = TRUE,
  formatos_compilacion = c("html", "moodle"),
  generar_reporte = TRUE
)

# 3. Analizar resultados detallados
cat("Estado final:", resultados$estado_general, "\n")
cat("Tests ejecutados:", resultados$total_tests, "\n")
cat("Tests exitosos:", resultados$tests_exitosos_count, "\n")
cat("Tiempo total:", round(resultados$tiempo_total, 2), "segundos\n")

# 4. Ver errores críticos si los hay
if (length(resultados$errores_criticos) > 0) {
  cat("Errores críticos encontrados:\n")
  for (error in resultados$errores_criticos) {
    cat("  -", error, "\n")
  }
}
```

### Debugging Avanzado en RStudio
```r
# === DEBUGGING EN RSTUDIO ===

# 1. Cargar utilidades
source("01-testing_rnw_utils.R")

# 2. Análisis paso a paso
archivo <- "../../ruta/al/archivo.Rnw"

# Paso 1: Estructura
cat("🔍 Verificando estructura...\n")
validacion <- validar_estructura_rnw(archivo)
print(paste("Puntuación:", round(validacion$puntuacion_validez * 100, 1), "%"))
if (length(validacion$errores) > 0) {
  cat("Errores de estructura:\n")
  for (error in validacion$errores) cat("  -", error, "\n")
}

# Paso 2: Extracción de código
cat("\n📝 Extrayendo código...\n")
codigo <- extraer_codigo_r_desde_rnw(archivo)
cat("Chunks R:", codigo$total_chunks_r, "\n")
cat("Chunks Python:", codigo$total_chunks_python, "\n")

# Paso 3: Ejecución
if (codigo$total_chunks_r > 0) {
  cat("\n⚡ Ejecutando código R...\n")
  ejecucion <- ejecutar_codigo_r_extraido(codigo)
  if (ejecucion$exito) {
    cat("✅ Ejecución exitosa\n")
    cat("Variables creadas:", length(ejecucion$variables_creadas), "\n")
    cat("Tiempo:", round(ejecucion$tiempo_ejecucion, 2), "s\n")
  } else {
    cat("❌ Error en ejecución:", ejecucion$error, "\n")
  }
}
```

## 🔍 Ejemplo de Uso Completo

```r
# 1. Cargar el sistema
setwd("Auxiliares/Testing_Rnw")
source("06-test_rnw_completo.R")

# 2. Ejecutar testing completo
archivo <- "../../06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/01-Variables-Cualitativas_Distribucion-De-Frecuencias/Gas_natural_porcentaje_maximo_aleatorio_interpretacion_representacion_n2_v1/consumo_gas_natural_porcentaje_maximo_aleatorio_interpretacion_representacion.Rnw"

resultados <- ejecutar_test_suite_completa(archivo)

# 3. Revisar resultados
print(resultados$estado_general)
print(paste("Éxito:", resultados$porcentaje_exito, "%"))

# 4. Ver detalles específicos
if (!resultados$validacion_estructura$chunks_balanceados) {
  print("⚠️ Chunks desbalanceados detectados")
}

if (resultados$ejecucion_r$exito) {
  print("✅ Código R ejecutado correctamente")
} else {
  print(paste("❌ Error R:", resultados$ejecucion_r$error))
}
```

## 📄 Reportes Generados

El sistema genera automáticamente reportes en formato Markdown con:
- Resumen ejecutivo del testing
- Detalles de cada fase
- Métricas y estadísticas
- Recomendaciones para corrección

Archivo: `REPORTE_TESTING_[nombre]_[timestamp].md`

## 🚨 Solución de Problemas Comunes

### Error: "Python no está disponible"
```r
# Configurar Python manualmente
library(reticulate)
use_python("/usr/bin/python3")
```

### Error: "pdflatex no está disponible"
```bash
# Ubuntu/Debian
sudo apt-get install texlive-latex-base texlive-latex-extra

# macOS
brew install --cask mactex
```

### Error: "Chunks desbalanceados"
- Verificar que cada `<<>>=` tenga su correspondiente `@`
- Revisar que no haya chunks anidados

### Error: "Coherencia matemática fallida"
- Verificar cálculos de porcentajes
- Comprobar que las opciones incluyan la respuesta correcta
- Validar rangos de variables numéricas

## 🔄 Integración con Flujo de Desarrollo

### Pre-commit Hook
```bash
#!/bin/bash
# Ejecutar testing antes de commit
for archivo in $(git diff --cached --name-only | grep '\.Rnw$'); do
  Rscript test_rnw_completo.R "$archivo"
  if [ $? -ne 0 ]; then
    echo "❌ Testing falló para $archivo"
    exit 1
  fi
done
```

### CI/CD Pipeline
```yaml
# .github/workflows/test-rnw.yml
name: Test RNW Files
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Setup R
        uses: r-lib/actions/setup-r@v2
      - name: Install dependencies
        run: Rscript -e "install.packages(c('testthat', 'exams', 'reticulate'))"
      - name: Test RNW files
        run: |
          for file in $(find . -name "*.Rnw"); do
            Rscript test_rnw_completo.R "$file"
          done
```

## 📚 Referencias

- [R-exams Documentation](https://www.r-exams.org/)
- [testthat Package](https://testthat.r-lib.org/)
- [reticulate Package](https://rstudio.github.io/reticulate/)
- [knitr Documentation](https://yihui.org/knitr/)

---

**Última actualización**: 2025-01-11  
**Versión**: 1.0  
**Autor**: Sistema de Testing ICFES
