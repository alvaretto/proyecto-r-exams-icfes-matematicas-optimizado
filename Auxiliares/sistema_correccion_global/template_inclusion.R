# ===============================================================================
# TEMPLATE PARA INCLUSIÓN DEL SISTEMA DE CORRECCIÓN EN NUEVOS EJERCICIOS
# ===============================================================================
# Archivo: template_inclusion.R
# Propósito: Código template para incluir en nuevos archivos .Rmd
# ===============================================================================

# ===============================================================================
# CÓDIGO PARA COPIAR EN NUEVOS ARCHIVOS .RMD
# ===============================================================================

# Agregar después de las librerías en el chunk de inicio:

```{r inicio, include=FALSE}
# Librerías esenciales
library(exams)
library(ggplot2)
library(knitr)
library(reticulate)
library(testthat)
library(data.table)
library(readxl)
library(datasets)
library(stringr)
library(digest)

# ⭐ CARGAR SISTEMA GLOBAL DE CORRECCIÓN SEMÁNTICA ⭐
source("Auxiliares/sistema_correccion_global/funciones_correccion.R")

# Configurar Python si es necesario
use_python("/usr/bin/python3", required = TRUE)

# Configuración global
typ <- match_exams_device()
options(scipen = 999)
knitr::opts_chunk$set(
  warning = FALSE, 
  message = FALSE,
  fig.keep = "all",
  dev = c("png", "pdf"),
  dpi = 150,
  echo = FALSE,
  results = "hide"
)

# Semilla aleatoria para diversidad de versiones
set.seed(sample(1:100000, 1))
```

# ===============================================================================
# MODIFICAR LA FUNCIÓN generar_datos() PARA USAR CORRECCIONES AUTOMÁTICAS
# ===============================================================================

```{r data_generation, echo=FALSE, results="hide"}
# Función principal de generación de datos
generar_datos <- function() {
  # ... tu lógica de generación de datos aquí ...

  # Al final, antes del return, aplicar correcciones automáticas:
  datos_corregidos <- procesar_datos_con_correcciones(datos)

  return(datos_corregidos)
}

# Generar datos del ejercicio con correcciones automáticas
datos <- generar_datos()

# ⭐ NUEVO SISTEMA: Variables textuales contextuales
vt <- datos$variables_texto  # Alias para facilitar uso

# Variables específicas con concordancia correcta
elemento <- datos$elemento_texto
condicion <- datos$condicion_texto
elemento_registrados <- vt$registrados
elemento_matriculados <- vt$matriculados
elemento_certificados <- vt$certificados
elemento_beneficiarios <- vt$beneficiarios

# Variables básicas
entidad <- datos$entidad
total <- datos$total
# ... otras variables ...
```

# ===============================================================================
# AGREGAR PRUEBAS DE CORRECCIÓN AUTOMÁTICA
# ===============================================================================

```{r correction_tests, echo=FALSE, results="hide"}
# Pruebas automáticas del sistema de corrección
test_that("Pruebas del sistema de corrección global", {
  # Probar corrección de concordancia
  expect_equal(corregir_todos_errores_concordancia("familias matriculados"), 
               "familias matriculadas")
  expect_equal(corregir_todos_errores_concordancia("empresas registrados"), 
               "empresas registradas")
  
  # Probar que los datos generados no tienen errores
  expect_false(str_detect(datos$elemento_texto, "matriculados|registrados|certificados"))
  expect_false(str_detect(datos$condicion_texto, "matriculados|registrados|certificados"))
  
  # Validar coherencia de datos
  errores <- validar_coherencia(datos$entidad, datos$elemento, 
                               datos$condicion, datos$total)
  expect_true(length(errores) == 0 || all(str_detect(errores, "Total fuera de rango")))
})
```

# ===============================================================================
# USO EN SECCIONES QUESTION Y SOLUTION
# ===============================================================================

# En la sección Question, usar las variables corregidas:
Question
========

Según el `r entidad`, en el país solo `r numerador` de cada `r denominador` `r elemento` están `r condicion`. Si el total de `r elemento` `r elemento_registrados` es `r format(total, big.mark = ".", decimal.mark = ",")`, al realizar la operación `r format(total, big.mark = ".", decimal.mark = ",")` por `r numerador`/`r denominador` se calcularía:

Answerlist
----------
- el doble de `r elemento` `r elemento_registrados`.
- la cantidad de `r elemento` `r condicion`.
- el promedio de `r elemento` `r elemento_registrados`.
- el porcentaje de `r elemento` `r condicion`.

# En la sección Solution, también usar las variables corregidas:
Solution
========

Para resolver este problema, debemos analizar qué representa la operación matemática `r format(total, big.mark = ".", decimal.mark = ",")` × `r numerador`/`r denominador`.

**Análisis del problema:**

- Total de `r elemento` registrados: `r format(total, big.mark = ".", decimal.mark = ",")`
- Proporción de `r elemento` `r condicion`: `r numerador` de cada `r denominador`

# ===============================================================================
# INSTRUCCIONES DE IMPLEMENTACIÓN
# ===============================================================================

# PASO 1: Copiar el código del chunk de inicio (incluyendo la línea source)
# PASO 2: Modificar tu función generar_datos() para usar procesar_datos_con_correcciones()
# PASO 3: Agregar el chunk de pruebas de corrección
# PASO 4: Usar las variables elemento_texto y condicion_texto en Question y Solution
# PASO 5: Probar que el archivo compile sin errores

# ===============================================================================
# VERIFICACIÓN RÁPIDA
# ===============================================================================

# Para verificar que todo funciona, ejecutar:
# source("Auxiliares/sistema_correccion_global/funciones_correccion.R")
# corregir_todos_errores_concordancia("familias matriculados")
# # Debe retornar: "familias matriculadas"

cat("📋 Template de inclusión cargado\n")
cat("📖 Instrucciones disponibles arriba\n")
cat("🎯 Copiar y adaptar el código según las instrucciones\n")
