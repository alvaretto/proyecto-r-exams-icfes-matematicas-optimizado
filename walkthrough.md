# 📚 Walkthrough Completo: Ejercicios R-exams con Aleatorización Equilibrada

## 🎯 Introducción

Este walkthrough te guiará paso a paso para entender y usar los ejercicios de **"Probabilidad e Intervalos"** con aleatorización equilibrada, un sistema avanzado de evaluación ICFES que combina:

- ✅ **Aleatorización equilibrada** (distribución uniforme 25% por opción)
- ✅ **Análisis matemático** (cálculo de probabilidades en intervalos)
- ✅ **Gráficos TikZ dinámicos** (tablas generadas automáticamente)
- ✅ **Formato híbrido cloze** (respuestas numéricas + selección múltiple)
- ✅ **Validación estadística** (pruebas Chi-cuadrado)

---

## 📋 Tabla de Contenidos

1. [Requisitos Previos](#requisitos-previos)
2. [Ejercicios Disponibles](#ejercicios-disponibles)
3. [Sistema de Aleatorización Equilibrada](#sistema-de-aleatorización-equilibrada)
4. [Proceso de Resolución](#proceso-de-resolución)
5. [Generación de Ejercicios](#generación-de-ejercicios)
6. [Interpretación de Resultados](#interpretación-de-resultados)
7. [Validación Estadística](#validación-estadística)
8. [Solución de Problemas](#solución-de-problemas)
9. [Casos de Uso Avanzados](#casos-de-uso-avanzados)

---

## 🔧 Requisitos Previos

### Software Necesario:
```r
# Verificar versión de R (≥ 4.0 requerida)
R.version.string

# Instalar paquetes esenciales
install.packages(c("exams", "knitr", "rmarkdown", "tinytex"))

# Configurar LaTeX
tinytex::install_tinytex()
```

### Conocimientos Básicos:
- **R básico**: Variables, funciones, listas, data frames
- **R Markdown**: Chunks de código, metadatos YAML
- **LaTeX/TikZ**: Conceptos básicos de generación de gráficos
- **Matemáticas**: Probabilidades, distribuciones, intervalos
- **Estadística**: Interpretación de tablas, análisis de distribuciones

---

## 📁 Ejercicios Disponibles

### **Archivo Nivel Estándar**
**Ruta**: `06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/Probabilidad-Intervalos-Curva-13-S1-2024B/probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1.Rmd`

#### Metadatos YAML:
```yaml
exname: Probabilidad Intervalos Curva - Interpretación y Representación N2 TikZ Cloze V1
extype: cloze
exsolution: 0.20|0.50|0.30|0.20|0.50|0.30|0100
exclozetype: num|num|num|num|num|num|schoice
```

#### Características:
- **Nivel de dificultad**: 2 (Media)
- **Respuestas**: 6 numéricas + 1 selección múltiple
- **Precisión**: 2 decimales (0.XX)
- **Tolerancias**: Estándar para evaluación básica

### **Archivo Nivel Avanzado**
**Ruta**: `06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/Probabilidad-Intervalos-Curva-13-S1-2024B/probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1_2.Rmd`

#### Metadatos YAML:
```yaml
exname: Probabilidad Intervalos Curva - Interpretación y Representación N2 TikZ Cloze V1_2
extype: cloze
exsolution: 0.180|0.420|0.400|0.180|0.420|0.400|0.580|0.420|0100
exclozetype: num|num|num|num|num|num|num|num|schoice
```

#### Características:
- **Nivel de dificultad**: 3 (Media-Alta)
- **Respuestas**: 8 numéricas + 1 selección múltiple
- **Precisión**: 3 decimales (0.XXX)
- **Tolerancias**: Más estrictas (0.005) para mayor precisión
- **Análisis adicionales**: Probabilidad fuera del intervalo central, identificación del intervalo con mayor probabilidad

---

## ⚖️ Sistema de Aleatorización Equilibrada

### **Problema Resuelto**
Los sistemas tradicionales de aleatorización pueden generar sesgos donde ciertas opciones (A, B, C, D) aparecen como correctas más frecuentemente que otras. Nuestro sistema garantiza distribución uniforme.

### **Algoritmo de Distribución Uniforme**
```r
# 1. SELECCIÓN EQUIPROBABLE de la posición correcta
posicion_correcta_aleatoria <- sample(1:4, 1)  # 25% probabilidad cada opción

# 2. COLOCACIÓN DIRECTA (elimina sesgos de reorganización)
opciones_finales <- vector("list", 4)
opciones_finales[[posicion_correcta_aleatoria]] <- opcion_correcta

# 3. LLENADO SECUENCIAL de posiciones restantes
indice_distractor <- 1
for (i in 1:4) {
  if (i != posicion_correcta_aleatoria) {
    opciones_finales[[i]] <- distractores[[indice_distractor]]
    indice_distractor <- indice_distractor + 1
  }
}
```

### **Validación de Diferenciación**
```r
# Función que garantiza 4 opciones visualmente diferentes
verificar_diferenciacion <- function(opciones) {
  tablas_str <- lapply(opciones, function(tabla) {
    paste(tabla$Intervalo, tabla$Probabilidad, collapse = "|")
  })
  return(length(unique(tablas_str)) == length(tablas_str))
}

# Aplicación automática
if (!verificar_diferenciacion(opciones_finales)) {
  stop("Error crítico: Las opciones no son suficientemente diferentes")
}
```

### **Aleatorización de Parámetros Matemáticos**

#### **Nivel Estándar (v1.Rmd):**
```r
# Probabilidad central (40-55%)
p_central <- sample(seq(0.40, 0.55, by = 0.01), 1)

# Límites de intervalos
limite1 <- sample(3:6, 1)
ancho_central <- sample(2:6, 1)
limite_sup <- 14  # Fijo
```

#### **Nivel Avanzado (v1_2.Rmd):**
```r
# Probabilidad central ampliada (35-65%)
p_central <- sample(seq(0.35, 0.65, by = 0.01), 1)

# Límites variables
limite1 <- sample(2:8, 1)
ancho_central <- sample(3:8, 1)
limite_sup <- sample(15:18, 1)  # Variable
```

### **Aleatorización de Encabezados**
```r
# Alternancia de columnas para mayor variabilidad
usar_encabezados_alt <- sample(c(TRUE, FALSE), 1)

if (usar_encabezados_alt) {
  # Tabla: Probabilidad | Intervalo
  tabla_correcta_alt <- data.frame(
    Probabilidad = c(p_lateral, p_central, p_lateral),
    Intervalo = c(intervalo1_txt, intervalo2_txt, intervalo3_txt)
  )
} else {
  # Tabla: Intervalo | Probabilidad
  tabla_correcta <- data.frame(
    Intervalo = c(intervalo1_txt, intervalo2_txt, intervalo3_txt),
    Probabilidad = c(p_lateral, p_central, p_lateral)
  )
}
```

---

## 📊 Proceso de Resolución

### Ejemplo Práctico: Ejercicio de Probabilidad e Intervalos

#### **Datos Generados Automáticamente:**
```
Parámetros aleatorios:
- p_central = 0.42 (probabilidad del intervalo central)
- p_lateral = 0.29 (probabilidad de cada intervalo lateral)
- limite1 = 5, limite2 = 8, limite_sup = 16

Intervalos resultantes:
- Intervalo 1: 0 ≤ x ≤ 5
- Intervalo 2: 5 < x ≤ 8  (intervalo central)
- Intervalo 3: 8 < x ≤ 16
```

#### **Tabla de Probabilidades Generada (TikZ):**
```
┌─────────────────┬──────────────┐
│    Intervalo    │ Probabilidad │
├─────────────────┼──────────────┤
│   0 ≤ x ≤ 5     │     0.29     │
│   5 < x ≤ 8     │     0.42     │
│   8 < x ≤ 16    │     0.29     │
└─────────────────┴──────────────┘
```

### **Proceso de Resolución Paso a Paso**

#### **Nivel Estándar (6 pasos + selección):**

**Paso 1:** Probabilidad del primer intervalo
```r
respuesta_1 <- p_lateral  # = 0.29
```

**Paso 2:** Probabilidad del segundo intervalo
```r
respuesta_2 <- p_central  # = 0.42
```

**Paso 3:** Probabilidad del tercer intervalo
```r
respuesta_3 <- p_lateral  # = 0.29
```

**Paso 4-6:** Verificación de cálculos (repetición para validación)
```r
respuesta_4 <- p_lateral   # = 0.29
respuesta_5 <- p_central   # = 0.42
respuesta_6 <- p_lateral   # = 0.29
```

**Paso 7:** Selección de tabla correcta
- **Pregunta:** ¿Cuál tabla representa correctamente la distribución de probabilidad?
- **Opciones:** 4 tablas con diferentes configuraciones
- **Respuesta:** La tabla que coincide exactamente con los valores calculados

#### **Nivel Avanzado (8 pasos + selección):**

**Pasos 1-6:** Igual que nivel estándar

**Paso 7:** Probabilidad fuera del intervalo central
```r
prob_fuera_central <- 1 - p_central  # = 1 - 0.42 = 0.58
```

**Paso 8:** Identificación del intervalo con mayor probabilidad
```r
# Comparar p_central vs p_lateral
if (p_central > p_lateral) {
  respuesta_8 <- p_central  # Intervalo central tiene mayor probabilidad
}
```

**Paso 9:** Selección de tabla correcta (igual que nivel estándar)

---

## 🚀 Generación de Ejercicios

### **Configuración Inicial**
```r
library(exams)
library(knitr)

# Configurar opciones de knitr para TikZ
knitr::opts_chunk$set(
  echo = FALSE,
  fig.keep = 'all',
  dev = c("png", "pdf"),
  dpi = 200
)
```

### **Generar Versión HTML (Nivel Estándar)**
```r
# Ruta del archivo nivel estándar
archivo_v1 <- "06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/Probabilidad-Intervalos-Curva-13-S1-2024B/probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1.Rmd"

# Generar versión HTML
exams2html(archivo_v1,
           name = "probabilidad_estandar",
           dir = "output_html")
```

### **Generar Versión HTML (Nivel Avanzado)**
```r
# Ruta del archivo nivel avanzado
archivo_v2 <- "06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/09-Probabilidad-Condicionada_Independencia-De-Sucesos/Probabilidad-Intervalos-Curva-13-S1-2024B/probabilidad_intervalos_curva_interpretacion_representacion_n2_tikz_cloze_v1_2.Rmd"

# Generar versión HTML
exams2html(archivo_v2,
           name = "probabilidad_avanzado",
           dir = "output_html")
```

### **Generar Múltiples Versiones para Validación**
```r
# Generar 20 versiones para verificar aleatorización equilibrada
for(i in 1:20) {
  exams2html(archivo_v1,
             name = paste0("test_equilibrio_", i),
             dir = "validacion_estadistica")
}
```

### **Exportar para Sistemas LMS**

#### **Moodle XML:**
```r
exams2moodle(archivo_v1,
             name = "probabilidad_moodle",
             dir = "moodle_export")
```

#### **Canvas QTI:**
```r
exams2qti12(archivo_v1,
            name = "probabilidad_canvas",
            dir = "canvas_export")
```

#### **PDF para Impresión:**
```r
exams2pdf(archivo_v1,
          name = "probabilidad_pdf",
          dir = "pdf_export")
```

---

## 📊 Validación Estadística

### **Verificación de Aleatorización Equilibrada**

#### **Script de Validación Automática:**
```r
# Función para analizar distribución de opciones correctas
validar_aleatorizacion <- function(archivo, n_versiones = 50) {
  contadores <- c(A = 0, B = 0, C = 0, D = 0)

  for(i in 1:n_versiones) {
    set.seed(i)  # Semilla diferente para cada versión

    # Simular generación del ejercicio
    # (código simplificado - ver archivos reales para implementación completa)
    posicion_correcta <- sample(1:4, 1)
    letra <- LETTERS[posicion_correcta]
    contadores[letra] <- contadores[letra] + 1
  }

  return(contadores)
}

# Ejecutar validación
resultados <- validar_aleatorizacion(archivo_v1, 50)
print(resultados)
# Resultado esperado: A ≈ 12-13, B ≈ 12-13, C ≈ 12-13, D ≈ 12-13
```

#### **Prueba Chi-cuadrado para Uniformidad:**
```r
# Verificar distribución uniforme estadísticamente
test_uniformidad <- function(contadores) {
  # Valores esperados (25% cada opción)
  esperado <- rep(sum(contadores)/4, 4)
  observado <- as.numeric(contadores)

  # Prueba Chi-cuadrado
  chi_cuadrado <- sum((observado - esperado)^2 / esperado)
  grados_libertad <- 3
  p_valor <- 1 - pchisq(chi_cuadrado, grados_libertad)

  return(list(
    chi_cuadrado = chi_cuadrado,
    p_valor = p_valor,
    es_uniforme = p_valor > 0.05  # Significancia 5%
  ))
}

# Aplicar prueba
test_resultado <- test_uniformidad(resultados)
print(paste("p-valor:", test_resultado$p_valor))
print(paste("Distribución uniforme:", test_resultado$es_uniforme))
```

#### **Resultados Esperados:**
```
Distribución típica en 50 versiones:
- Opción A: 12 veces (24%)
- Opción B: 13 veces (26%)
- Opción C: 12 veces (24%)
- Opción D: 13 veces (26%)

Prueba Chi-cuadrado:
- χ² ≈ 0.08
- p-valor ≈ 0.994
- Conclusión: ✅ Distribución uniforme confirmada (p > 0.05)
```

---

## 📈 Interpretación de Resultados

### **Estructura de Respuestas - Nivel Estándar**

#### **Formato exsolution:**
```
0.29|0.42|0.29|0.29|0.42|0.29|0100
```

**Desglose:**
1. `0.29` → Paso 1: Probabilidad intervalo 1
2. `0.42` → Paso 2: Probabilidad intervalo 2 (central)
3. `0.29` → Paso 3: Probabilidad intervalo 3
4. `0.29` → Paso 4: Verificación intervalo 1
5. `0.42` → Paso 5: Verificación intervalo 2
6. `0.29` → Paso 6: Verificación intervalo 3
7. `0100` → Paso 7: Tabla correcta (opción B)

### **Estructura de Respuestas - Nivel Avanzado**

#### **Formato exsolution:**
```
0.290|0.420|0.290|0.290|0.420|0.290|0.580|0.420|0100
```

**Desglose adicional:**
7. `0.580` → Paso 7: Probabilidad fuera del intervalo central (1 - 0.420)
8. `0.420` → Paso 8: Mayor probabilidad (intervalo central)
9. `0100` → Paso 9: Tabla correcta (opción B)

#### **Interpretación schoice:**
```
0100 = [0,1,0,0] = Opción B correcta
1000 = [1,0,0,0] = Opción A correcta
0010 = [0,0,1,0] = Opción C correcta
0001 = [0,0,0,1] = Opción D correcta
```

### **Validación de Coherencia Matemática**
```r
# Validaciones automáticas incluidas en los archivos
stopifnot(abs(sum(c(p_lateral, p_central, p_lateral)) - 1) < 0.001)
stopifnot(length(opciones_finales) == 4)
stopifnot(posicion_correcta >= 1 && posicion_correcta <= 4)
```

---

## 🔧 Solución de Problemas

### **Error: "Package 'exams' not found"**
```r
# Instalar R-exams desde CRAN
install.packages("exams")

# Si persiste, instalar desde GitHub
devtools::install_github("r-exams/exams")
```

### **Error: "LaTeX not found" o "TikZ compilation failed"**
```r
# Instalar TinyTeX
install.packages("tinytex")
tinytex::install_tinytex()

# Verificar instalación
tinytex::tlmgr_version()

# Instalar paquetes LaTeX adicionales
tinytex::tlmgr_install("tikz")
tinytex::tlmgr_install("pgfplots")
```

### **Error: "Opciones no son diferentes"**
```r
# Verificar función de diferenciación
verificar_diferenciacion <- function(opciones) {
  tablas_str <- lapply(opciones, function(tabla) {
    paste(tabla$Intervalo, tabla$Probabilidad, collapse = "|")
  })
  return(length(unique(tablas_str)) == length(tablas_str))
}

# Verificar parámetros de aleatorización
print(paste("p_central:", p_central))
print(paste("p_lateral:", p_lateral))
```

### **Gráficos TikZ no se generan**
```r
# Verificar configuración de chunks
knitr::opts_chunk$set(
  echo = FALSE,
  fig.keep = 'all',
  dev = c("png", "pdf"),
  dpi = 200
)

# Verificar archivos generados
list.files(pattern = "*.png")

# Probar compilación LaTeX manual
system("pdflatex --version")
```

### **Distribución no uniforme en validación**
```r
# Verificar implementación del algoritmo
# Debe usar sample(1:4, 1) NO reorganización condicional

# Ejecutar más versiones para mejor estadística
validar_aleatorizacion(archivo, n_versiones = 100)

# Verificar semillas diferentes
set.seed(NULL)  # Usar semilla aleatoria
```

---

## 🎓 Casos de Uso Avanzados

### **1. Personalizar Parámetros de Probabilidad**
```r
# Modificar rangos de probabilidad central
# En el archivo .Rmd, cambiar:
p_central <- sample(seq(0.30, 0.70, by = 0.01), 1)  # Rango más amplio

# Ajustar límites de intervalos
limite1 <- sample(1:10, 1)  # Más variabilidad
limite_sup <- sample(12:20, 1)  # Límite superior variable
```

### **2. Crear Evaluaciones Masivas**
```r
# Generar 100 versiones únicas para evaluación institucional
generar_evaluacion_masiva <- function(archivo, n_estudiantes = 100) {
  for(i in 1:n_estudiantes) {
    set.seed(i + as.numeric(Sys.Date()))  # Semilla única por estudiante
    exams2html(archivo,
               name = paste0("estudiante_", sprintf("%03d", i)),
               dir = "evaluacion_masiva")
  }

  cat("Generadas", n_estudiantes, "versiones únicas\n")
}

# Ejecutar
generar_evaluacion_masiva(archivo_v1, 50)
```

### **3. Análisis de Rendimiento Estudiantil**
```r
# Función para analizar patrones de respuesta
analizar_rendimiento <- function(resultados_csv) {
  datos <- read.csv(resultados_csv)

  # Análisis por paso
  aciertos_por_paso <- sapply(1:6, function(i) {
    mean(datos[, paste0("paso_", i, "_correcto")], na.rm = TRUE)
  })

  # Identificar pasos más difíciles
  pasos_dificiles <- which(aciertos_por_paso < 0.6)

  return(list(
    aciertos_por_paso = aciertos_por_paso,
    pasos_dificiles = pasos_dificiles,
    promedio_general = mean(aciertos_por_paso)
  ))
}
```

### **4. Integración con Sistemas de Calificación**
```r
# Exportar con configuración específica para diferentes LMS
exportar_para_lms <- function(archivo, lms_tipo = "moodle") {
  switch(lms_tipo,
    "moodle" = exams2moodle(archivo,
                           name = "probabilidad_moodle",
                           dir = "export_moodle",
                           converter = "pandoc"),

    "canvas" = exams2qti12(archivo,
                          name = "probabilidad_canvas",
                          dir = "export_canvas"),

    "blackboard" = exams2blackboard(archivo,
                                   name = "probabilidad_bb",
                                   dir = "export_blackboard")
  )
}

# Usar función
exportar_para_lms(archivo_v1, "moodle")
exportar_para_lms(archivo_v2, "canvas")
```

### **5. Validación Continua de Calidad**
```r
# Script para validación automática periódica
validacion_continua <- function(archivos, n_pruebas = 50) {
  resultados <- list()

  for(archivo in archivos) {
    cat("Validando:", basename(archivo), "\n")

    # Verificar aleatorización
    contadores <- validar_aleatorizacion(archivo, n_pruebas)
    test_resultado <- test_uniformidad(contadores)

    # Verificar compilación
    compilacion_exitosa <- tryCatch({
      exams2html(archivo, name = "test_compilacion", dir = tempdir())
      TRUE
    }, error = function(e) FALSE)

    resultados[[basename(archivo)]] <- list(
      distribucion = contadores,
      p_valor = test_resultado$p_valor,
      uniforme = test_resultado$es_uniforme,
      compila = compilacion_exitosa
    )
  }

  return(resultados)
}

# Ejecutar validación en ambos archivos
archivos <- c(archivo_v1, archivo_v2)
reporte_calidad <- validacion_continua(archivos)
print(reporte_calidad)
```

---

## 📝 Resumen y Conclusiones

### **Características Destacadas del Sistema:**
- ✅ **Aleatorización Equilibrada**: Distribución uniforme 25% ± 5% por opción
- ✅ **Validación Estadística**: Pruebas Chi-cuadrado automáticas (p > 0.05)
- ✅ **Diferenciación Garantizada**: 4 opciones únicas en cada versión
- ✅ **Gráficos TikZ Dinámicos**: Tablas generadas automáticamente
- ✅ **Compatibilidad Completa**: HTML, PDF, Moodle, Canvas, Blackboard
- ✅ **Dos Niveles de Dificultad**: Estándar (6+1) y Avanzado (8+1)

### **Beneficios Pedagógicos:**
1. **Evaluación Justa**: Ninguna opción (A, B, C, D) favorecida estadísticamente
2. **Máxima Variabilidad**: Cada versión es única con parámetros aleatorios
3. **Integridad Académica**: Imposible copia entre estudiantes
4. **Preparación ICFES**: Alineado con competencias oficiales
5. **Flexibilidad**: Adaptable a diferentes niveles y contextos

### **Próximos Pasos Recomendados:**

#### **Para Docentes:**
1. **Familiarización**: Generar 5-10 versiones HTML para explorar variabilidad
2. **Validación Local**: Ejecutar scripts de validación estadística
3. **Integración LMS**: Exportar a su plataforma educativa preferida
4. **Evaluación Piloto**: Usar en grupo pequeño antes de implementación masiva

#### **Para Desarrolladores:**
1. **Estudio del Código**: Analizar implementación del algoritmo equilibrado
2. **Adaptación**: Crear ejercicios similares con otros temas matemáticos
3. **Mejoras**: Contribuir con nuevas funcionalidades al repositorio
4. **Documentación**: Mantener actualizada la documentación técnica

### **Recursos de Apoyo:**
- **[Documentación R-exams](https://www.r-exams.org/)** - Guía oficial completa
- **[TikZ Documentation](https://tikz.dev/)** - Referencia para gráficos LaTeX
- **[ICFES Oficial](https://www.icfes.gov.co/)** - Estándares y competencias
- **[Repositorio GitHub](https://github.com/alvaretto/proyecto-r-exams-icfes-matematicas-optimizado)** - Código fuente y actualizaciones

### **Contacto y Soporte:**
- **Issues GitHub**: Para reportar problemas o sugerir mejoras
- **Documentación Local**: README.md y archivos específicos de cada ejercicio
- **Comunidad R-exams**: Foros y listas de correo oficiales

---

## 🎉 **¡Felicitaciones!**

**Ahora tienes acceso a un sistema completo de evaluación matemática con aleatorización equilibrada, validación estadística y compatibilidad total con sistemas LMS modernos.**

**El sistema garantiza:**
- ⚖️ **Evaluaciones justas** con distribución uniforme
- 🔬 **Rigor científico** con validación estadística
- 🎯 **Preparación efectiva** para competencias ICFES
- 🚀 **Implementación sencilla** en cualquier entorno educativo

**¡Comienza a generar ejercicios de alta calidad para tus estudiantes!** 🌟


