# 🧪 SISTEMA DE TESTING PARA ARCHIVOS .RNW

## 📋 Descripción

Este directorio contiene un sistema completo de testing para archivos `.Rnw` (LaTeX + R) del proyecto ICFES R-exams. El sistema permite validar estructura, ejecutar código R y Python, verificar coherencia matemática y probar compilación a diferentes formatos.

## 📁 Estructura de Archivos (Orden de Lectura/Ejecución)

```
Auxiliares/Testing_Rnw/
├── 00-README.md                           # 📖 Este archivo (documentación principal)
├── 01-testing_rnw_utils.R                 # 🔧 Utilidades básicas para .Rnw
├── 02-testing_python_chunks.R             # 🐍 Testing específico de chunks Python
├── 03-testing_coherencia_matematica.R     # 🔢 Validación de coherencia matemática
├── 04-testing_compilacion_rnw.R           # 🔨 Testing de compilación a formatos
├── 05-test_rnw_simple.R                   # ⚡ Script de testing rápido
├── 06-test_rnw_completo.R                 # 🧪 Script de testing completo
└── 07-FLUJO_TRABAJO_TESTING_RNW.md        # 📚 Documentación detallada del flujo
```

## 🚀 Uso Rápido

### Opción 1: Testing Rápido (Recomendado para desarrollo)
```bash
cd "Auxiliares/Testing_Rnw"
Rscript 05-test_rnw_simple.R "../../ruta/al/archivo.Rnw"
```

### Opción 2: Testing Completo (Para validación final)
```bash
cd "Auxiliares/Testing_Rnw"
Rscript 06-test_rnw_completo.R "../../ruta/al/archivo.Rnw"
```

### Opción 3: Desde R Interactivo / RStudio
```r
# Cambiar al directorio de testing
setwd("Auxiliares/Testing_Rnw")

# Cargar utilidades básicas
source("01-testing_rnw_utils.R")

# Ejecutar testing básico
source("05-test_rnw_simple.R")
resultados <- test_basico_rnw("../../ruta/al/archivo.Rnw")
```

### Opción 4: Testing Avanzado desde RStudio
```r
# === TESTING AVANZADO DESDE RSTUDIO ===

# 1. Configurar entorno
setwd("Auxiliares/Testing_Rnw")

# 2. Cargar sistema completo
source("06-test_rnw_completo.R")

# 3. Definir archivo
archivo <- "../../06-Estadística-Y-Probabilidad/.../archivo.Rnw"

# 4. Ejecutar testing completo
cat("🧪 Iniciando testing avanzado...\n")
resultados <- ejecutar_test_suite_completa(archivo, incluir_compilacion = TRUE)

# 5. Mostrar resumen
cat("\n📊 RESUMEN FINAL:\n")
cat("Estado:", resultados$estado_general, "\n")
cat("Éxito:", resultados$porcentaje_exito, "%\n")
cat("Tiempo:", round(resultados$tiempo_total, 2), "s\n")
```

### Opción 5: Testing Modular desde RStudio
```r
# Para testing por componentes específicos
setwd("Auxiliares/Testing_Rnw")

# 1. Validar solo estructura
source("01-testing_rnw_utils.R")
validacion <- validar_estructura_rnw("../../archivo.Rnw")
print(validacion)

# 2. Solo extracción y ejecución de código R
codigo <- extraer_codigo_r_desde_rnw("../../archivo.Rnw")
ejecucion <- ejecutar_codigo_r_extraido(codigo)

# 3. Solo coherencia matemática (si código ejecutó bien)
if (ejecucion$exito) {
  source("03-testing_coherencia_matematica.R")
  coherencia <- validar_coherencia_completa(ejecucion$entorno)
  print(coherencia$coherencia_general)
}
```

## 🎯 Funcionalidades Principales

### 1. **Validación de Estructura** (`01-testing_rnw_utils.R`)
- ✅ Verifica estructura LaTeX válida
- ✅ Comprueba balance de chunks R
- ✅ Detecta elementos requeridos (question, solution)
- ✅ Extrae y ejecuta código R en entorno controlado

### 2. **Testing de Python** (`02-testing_python_chunks.R`)
- ✅ Detecta chunks con `engine='python'`
- ✅ Configura matplotlib en modo no interactivo
- ✅ Valida generación de gráficos
- ✅ Verifica disponibilidad de librerías

### 3. **Coherencia Matemática** (`03-testing_coherencia_matematica.R`)
- ✅ Valida rangos numéricos (porcentajes 0-100)
- ✅ Verifica coherencia entre respuestas y distractores
- ✅ Valida cálculos matemáticos específicos
- ✅ Detecta inconsistencias en datos generados

### 4. **Compilación** (`04-testing_compilacion_rnw.R`)
- ✅ Compila a HTML con exams2html()
- ✅ Compila a PDF con exams2pdf()
- ✅ Compila a Moodle XML con exams2moodle()
- ✅ Valida archivos generados

## 📊 Interpretación de Resultados

### Estados de Testing
- **✅ APROBADO**: ≥80% tests exitosos, sin errores críticos
- **⚠️ APROBADO CON OBSERVACIONES**: 60-79% tests exitosos
- **❌ REPROBADO**: <60% tests exitosos o errores críticos

### Métricas Clave
- **Estructura válida**: Puntuación ≥80%
- **Ejecución R**: Sin errores, tiempo <30s
- **Python**: ≥80% chunks exitosos (si aplica)
- **Coherencia**: Sin errores matemáticos
- **Compilación**: ≥80% formatos exitosos

## 🔧 Configuración Inicial

### Dependencias R
```r
install.packages(c("testthat", "exams", "knitr", "reticulate", "stringr", "tools"))
```

### Software del Sistema
- **Python 3.x** con matplotlib, numpy
- **LaTeX** (pdflatex) para compilación PDF
- **Pandoc** para conversiones

### Verificar Configuración
```r
source("01-testing_rnw_utils.R")
# Las utilidades se cargarán automáticamente
```

## 🎯 Uso Específico en RStudio

### Configuración Inicial en RStudio
```r
# 1. Abrir RStudio y navegar al proyecto
# 2. Configurar directorio de trabajo
setwd("Auxiliares/Testing_Rnw")

# 3. Verificar configuración del sistema (primera vez)
source("08-configurar_testing.R")
```

### Testing Rápido en RStudio (Recomendado para desarrollo)
```r
# Cargar sistema de testing rápido
source("05-test_rnw_simple.R")

# Definir archivo a testear
archivo <- "../../06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/01-Variables-Cualitativas_Distribucion-De-Frecuencias/Gas_natural_porcentaje_maximo_aleatorio_interpretacion_representacion_n2_v1/consumo_gas_natural_porcentaje_maximo_aleatorio_interpretacion_representacion.Rnw"

# Ejecutar testing
resultados <- test_basico_rnw(archivo)

# Ver resultados
print(resultados$estado)
print(paste("Éxito:", resultados$porcentaje_exito, "%"))
```

### Testing Completo en RStudio (Para validación final)
```r
# Cargar sistema completo
source("06-test_rnw_completo.R")

# Ejecutar testing completo con compilación
resultados <- ejecutar_test_suite_completa(
  archivo,
  incluir_compilacion = TRUE,
  formatos_compilacion = c("html", "moodle")
)

# Analizar resultados detallados
cat("Estado:", resultados$estado_general, "\n")
cat("Tests ejecutados:", resultados$total_tests, "\n")
cat("Errores críticos:", length(resultados$errores_criticos), "\n")
```

### Debugging en RStudio
```r
# Para identificar problemas específicos
source("01-testing_rnw_utils.R")

# 1. Verificar estructura
validacion <- validar_estructura_rnw(archivo)
if (validacion$puntuacion_validez < 0.8) {
  print("Problemas de estructura:")
  print(validacion$errores)
}

# 2. Probar extracción de código
codigo <- extraer_codigo_r_desde_rnw(archivo)
print(paste("Chunks R encontrados:", codigo$total_chunks_r))

# 3. Probar ejecución
if (codigo$total_chunks_r > 0) {
  ejecucion <- ejecutar_codigo_r_extraido(codigo)
  if (!ejecucion$exito) {
    print(paste("Error en ejecución:", ejecucion$error))
  }
}
```

## 💡 Ejemplos de Uso

### Ejemplo 1: Testing Básico
```r
setwd("Auxiliares/Testing_Rnw")
source("05-test_rnw_simple.R")

# Testear archivo específico
archivo <- "../../06-Estadística-Y-Probabilidad/.../archivo.Rnw"
resultados <- test_basico_rnw(archivo)

# Ver resultados
print(resultados$estado)
print(paste("Éxito:", resultados$porcentaje_exito, "%"))
```

### Ejemplo 2: Testing con testthat
```r
source("01-testing_rnw_utils.R")

# Crear suite personalizada
test_suite <- crear_test_suite_rnw("archivo.Rnw")
test_suite()  # Ejecutar tests
```

### Ejemplo 3: Validación Específica
```r
source("03-testing_coherencia_matematica.R")

# Primero ejecutar el código R
source("01-testing_rnw_utils.R")
codigo <- extraer_codigo_r_desde_rnw("archivo.Rnw")
resultado <- ejecutar_codigo_r_extraido(codigo)

# Luego validar coherencia
if (resultado$exito) {
  validacion <- validar_coherencia_completa(resultado$entorno)
  print(validacion$coherencia_general)
}
```

## 🚨 Solución de Problemas

### Error: "No se encontró el archivo"
- Verificar que la ruta sea relativa al directorio `Testing_Rnw`
- Usar rutas completas si es necesario

### Error: "Python no disponible"
```r
library(reticulate)
use_python("/usr/bin/python3")  # Ajustar ruta según sistema
```

### Error: "Chunks desbalanceados"
- Verificar que cada `<<>>=` tenga su `@` correspondiente
- Revisar que no haya chunks anidados

### Error: "Compilación falló"
- Verificar que LaTeX esté instalado
- Comprobar que no haya errores en el código R

## 💡 Consejos Específicos para RStudio

### Configuración Recomendada
- **Directorio de trabajo**: Siempre usar `setwd("Auxiliares/Testing_Rnw")`
- **Encoding**: Configurar UTF-8 en RStudio (Tools > Global Options > Code > Saving)
- **Rutas**: Usar rutas relativas desde Testing_Rnw (`../../archivo.Rnw`)

### Flujo de Trabajo Eficiente
1. **Desarrollo**: Usar `05-test_rnw_simple.R` para iteraciones rápidas
2. **Validación**: Usar `06-test_rnw_completo.R` antes de commits
3. **Debugging**: Usar `01-testing_rnw_utils.R` para análisis detallado
4. **Configuración**: Ejecutar `08-configurar_testing.R` solo una vez

### Atajos Útiles en RStudio
```r
# Crear función personalizada para testing rápido
test_rapido <- function(archivo) {
  setwd("Auxiliares/Testing_Rnw")
  source("05-test_rnw_simple.R")
  return(test_basico_rnw(paste0("../../", archivo)))
}

# Uso: test_rapido("ruta/al/archivo.Rnw")
```

### Integración con Proyectos RStudio
```r
# Agregar al .Rprofile del proyecto
if (interactive()) {
  cat("🧪 Sistema de testing .Rnw disponible\n")
  cat("💡 Usar: setwd('Auxiliares/Testing_Rnw') para comenzar\n")
}
```

## 🔄 Integración con Flujo de Trabajo

### Pre-commit Hook
```bash
#!/bin/bash
cd "Auxiliares/Testing_Rnw"
for archivo in $(git diff --cached --name-only | grep '\.Rnw$'); do
  Rscript 05-test_rnw_simple.R "../../$archivo"
  if [ $? -ne 0 ]; then
    echo "❌ Testing falló para $archivo"
    exit 1
  fi
done
```

### Alias Útiles
```bash
# Agregar al .bashrc o .zshrc
alias test-rnw='cd "Auxiliares/Testing_Rnw" && Rscript 05-test_rnw_simple.R'
alias test-rnw-full='cd "Auxiliares/Testing_Rnw" && Rscript 06-test_rnw_completo.R'
```

## 📈 Próximas Mejoras

- [ ] Integración con GitHub Actions
- [ ] Reportes HTML interactivos
- [ ] Testing de performance
- [ ] Validación de accesibilidad
- [ ] Métricas de calidad de código

## 📞 Soporte

Para problemas o mejoras:

1. Revisar la documentación detallada en `07-FLUJO_TRABAJO_TESTING_RNW.md`
2. Verificar configuración de dependencias
3. Consultar ejemplos en este README

---

**Última actualización**: 2025-01-11  
**Versión**: 1.0  
**Autor**: Sistema de Testing ICFES
