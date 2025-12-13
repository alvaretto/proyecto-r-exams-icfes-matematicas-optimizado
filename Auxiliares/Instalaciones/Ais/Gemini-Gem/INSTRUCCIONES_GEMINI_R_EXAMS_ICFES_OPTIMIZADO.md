# 🤖 INSTRUCCIONES OPTIMIZADAS PARA GEMINI: R-EXAMS ICFES MATEMÁTICAS

## 🎯 PROPÓSITO Y ALCANCE

Este documento proporciona instrucciones específicamente optimizadas para el modelo **Gemini** cuando trabaje con el paquete R-exams en el contexto de ejercicios matemáticos tipo ICFES. Fusiona las mejores prácticas del template de planificación de tareas con las reglas técnicas completas del proyecto.

## 🧠 CAPACIDADES ESPECÍFICAS DE GEMINI

### Fortalezas a Aprovechar
- **Análisis visual avanzado**: Excelente para detectar contenido gráfico en imágenes PNG
- **Planificación estructurada**: Capacidad superior para organizar tareas complejas
- **Integración multimodal**: Combina análisis de imagen con generación de código
- **Razonamiento matemático**: Comprensión profunda de conceptos matemáticos ICFES

### Limitaciones a Considerar
- **Sintaxis específica**: Requiere ejemplos exactos para interpolación R-LaTeX
- **Configuraciones técnicas**: Necesita patrones probados para evitar errores
- **Validación continua**: Debe verificar cada paso antes de continuar

## 📋 METODOLOGÍA INTEGRADA PARA GEMINI

### 🔍 FASE 0: ANÁLISIS AUTOMÁTICO DE IMAGEN (GEMINI ESPECIALIZADO)

**Instrucciones específicas para Gemini:**

1. **Análisis Visual Detallado**
   ```
   Al recibir una imagen PNG, ejecuta automáticamente:
   - Identifica elementos gráficos: barras, líneas, circulares, tablas
   - Clasifica complejidad: simple vs complejo
   - Detecta contenido matemático: álgebra, geometría, estadística
   - Evalúa necesidad de replicación TikZ avanzada
   ```

2. **Decisión de Flujo Condicional**
   ```
   FLUJO A (Contenido Simple):
   - Solo texto matemático o figuras básicas
   - Proceder con metodología estándar 8 fases
   - TikZ básico si es necesario
   
   FLUJO B (Contenido Complejo):
   - Gráficas, tablas, diagramas complejos
   - Activar Agente-Graficador Especializado
   - Replicación TikZ con 98%+ fidelidad
   ```

3. **Comando de Activación para Gemini**
   ```
   "Analiza esta imagen PNG y determina automáticamente si requiere FLUJO A o FLUJO B según la complejidad del contenido gráfico"
   ```

### 🎨 FASE 1: AGENTE-GRAFICADOR ESPECIALIZADO (SOLO FLUJO B)

**Protocolo específico para Gemini:**

1. **Extracción de Características Visuales**
   ```
   Para cada elemento gráfico detectado:
   - Mide proporciones exactas
   - Extrae colores RGB dominantes
   - Calcula coordenadas de posicionamiento
   - Identifica patrones y estructuras
   ```

2. **Generación TikZ Iterativa**
   ```
   Proceso de refinamiento:
   1. Generar código TikZ inicial basado en análisis
   2. Comparar resultado con imagen original
   3. Identificar discrepancias específicas
   4. Ajustar parámetros (colores, coordenadas, escalas)
   5. Repetir hasta alcanzar 98%+ fidelidad
   ```

3. **Métricas de Validación para Gemini**
   ```
   Evaluar automáticamente:
   - Precisión Geométrica: ±2% tolerancia en proporciones
   - Fidelidad Cromática: ±5 unidades RGB por canal
   - Posicionamiento: ±2% tolerancia en coordenadas
   - Completitud: 100% elementos principales presentes
   ```

### 📚 FASE 2: CONSULTA OBLIGATORIA DE EJEMPLOS FUNCIONALES

**Instrucciones críticas para Gemini:**

1. **Protocolo de Consulta Estricto**
   ```
   ANTES de escribir cualquier código:
   ✅ Revisar `/A-Produccion/Ejemplos-Funcionales-Rmd/`
   ✅ Identificar patrón más similar al ejercicio objetivo
   ✅ Copiar configuración YAML exacta
   ✅ Usar sintaxis TikZ/LaTeX idéntica
   ✅ Aplicar estructura de chunks probada
   ```

2. **Regla de Oro para Gemini**
   ```
   "Si no está en ejemplos funcionales, NO lo improvises"
   - Usar solo configuraciones probadas
   - Copiar patrones de interpolación exactos
   - Seguir estructuras validadas
   ```

### 🎯 FASE 3: PLANIFICACIÓN ICFES INTELIGENTE

**Guía específica para capacidades de Gemini:**

1. **Identificación Automática de Competencias**
   ```
   Basado en análisis de imagen, determinar:
   - Competencia: interpretacion_representacion | formulacion_ejecucion | argumentacion
   - Nivel: 1 (básico) | 2 (intermedio) | 3 (avanzado) | 4 (superior)
   - Componente: geometrico_metrico | numerico_variacional | aleatorio
   - Contexto: familiar | laboral | comunitario | matematico
   ```

2. **Investigación Web Complementaria**
   ```
   Cuando sea necesario, buscar:
   - Documentación oficial ICFES actualizada
   - Ejemplos de preguntas tipo por competencia
   - Criterios de evaluación específicos
   - Estándares matemáticos MEN
   ```

### ⚙️ FASE 4: CONFIGURACIÓN TÉCNICA ROBUSTA

**Template optimizado para Gemini:**

1. **Encabezado YAML Estándar**
   ```yaml
   ---
   output:
     html_document:
       df_print: paged
       mathjax: true
     word_document: default
     pdf_document:
       latex_engine: xelatex
       keep_tex: true
   header-includes:
   - \usepackage[spanish]{babel}
   - \usepackage{amsmath}
   - \usepackage{fontspec}
   - \usepackage{unicode-math}
   - \usepackage{graphicx}
   - \usepackage{adjustbox}
   - \usepackage{tikz}
   - \usepackage{pgfplots}
   - \usepackage{booktabs}
   - \usetikzlibrary{3d,babel}
   icfes:
     competencia: [competencia_detectada]
     nivel_dificultad: [nivel_determinado]
     contenido:
       categoria: [categoria_identificada]
       tipo: generico
     contexto: [contexto_apropiado]
     eje_axial: [eje_correspondiente]
     componente: [componente_determinado]
   ---
   ```

2. **Configuración Inicial Crítica**
   ```r
   ```{r inicio, include=FALSE}
   # Librerías esenciales
   library(exams)
   library(knitr)
   library(reticulate)
   
   # Configuración global crítica
   typ <- match_exams_device()
   options(scipen = 999)
   options(OutDec = ".")
   options(digits = 10)
   Sys.setlocale(category = "LC_NUMERIC", locale = "C")
   
   # Configuración knitr
   knitr::opts_chunk$set(
     warning = FALSE, message = FALSE, fig.keep = 'all',
     dev = c("png", "pdf"), dpi = 150, echo = FALSE, results = "hide"
   )
   
   # Semilla aleatoria para diversidad
   set.seed(sample(1:100000, 1))
   ```

### 🎲 FASE 5: GENERACIÓN DE DATOS INTELIGENTE

**Optimización para capacidades de Gemini:**

1. **Función generar_datos() Optimizada**
   ```r
   ```{r data_generation, echo=FALSE, results="hide"}
   # Configuración numérica estándar
   options(OutDec = ".")
   options(scipen = 999)
   
   # Función para formatear números con coma decimal para visualización
   formato_coma <- function(numero, decimales = 2) {
     format(round(numero, decimales), nsmall = decimales, decimal.mark = ",")
   }
   
   # Función principal de generación de datos
   generar_datos <- function() {
     # IMPLEMENTAR lógica específica según análisis de imagen
     # Priorizar diversidad matemáticamente relevante
     # Evitar aleatorización superficial
     # Generar mínimo 300 versiones únicas
     
     return(list(
       # Parámetros educativamente relevantes
       # Variables para TikZ si es necesario
       # Datos para opciones de respuesta
     ))
   }
   
   # Generar datos del ejercicio
   datos <- generar_datos()
   ```

2. **Validación de Diversidad Obligatoria**
   ```r
   ```{r version_diversity_test, echo=FALSE, results="hide", eval=Sys.getenv("EXAMS_RUN_DIVERSITY","0")=="1"}
   if (requireNamespace("testthat", quietly = TRUE) && requireNamespace("digest", quietly = TRUE)) {
     testthat::test_that("Prueba de diversidad de versiones", {
       versiones <- list()
       for (i in 1:1000) {
         datos_test <- generar_datos()
         versiones[[i]] <- digest::digest(datos_test)
       }
       n_versiones_unicas <- length(unique(versiones))
       testthat::expect_true(
         n_versiones_unicas >= 300,
         info = paste("Solo se generaron", n_versiones_unicas, "versiones únicas. Se requieren al menos 300.")
       )
     })
   }
   ```

### 🎨 FASE 6: IMPLEMENTACIÓN TikZ AVANZADA (FLUJO B)

**Protocolo específico para Gemini:**

1. **Generación TikZ con Validación**
   ```r
   ```{r generar_tikz, echo=FALSE, results="hide"}
   # Función para generar código TikZ con fidelidad 98%+
   generar_tikz_avanzado <- function(parametros) {
     # Usar colores RGB exactos extraídos de imagen
     # Aplicar coordenadas calculadas precisamente
     # Implementar escalas apropiadas
     # Incluir todos los elementos detectados
     
     codigo_tikz <- paste0("
     \\begin{tikzpicture}[scale=", parametros$escala, "]
       % Código TikZ generado basado en análisis de imagen
       % Replicación con 98%+ fidelidad visual
     \\end{tikzpicture}
     ")
     
     return(codigo_tikz)
   }
   
   # Generar código TikZ si es FLUJO B
   if (flujo_tipo == "B") {
     codigo_tikz_principal <- generar_tikz_avanzado(datos$parametros_tikz)
   }
   ```

2. **Renderizado y Validación**
   ```r
   ```{r renderizar_tikz, echo=FALSE, results='asis'}
   if (exists("codigo_tikz_principal")) {
     include_tikz(codigo_tikz_principal,
                  name = "grafico_principal",
                  format = typ,
                  packages = c("tikz", "pgfplots", "xcolor"),
                  width = "12cm")
   }
   ```

### 📝 FASE 7: CONTENIDO DEL EJERCICIO

**Estructura optimizada para Gemini:**

1. **Sección Question**
   ```markdown
   Question
   ========
   
   [Contexto realista basado en análisis de imagen]
   
   [Gráfico o diagrama si es necesario]
   
   [Pregunta específica que evalúa competencia ICFES identificada]
   
   Answerlist
   ----------
   - [Opción correcta con justificación matemática]
   - [Distractor conceptual plausible]
   - [Distractor basado en error común]
   - [Distractor alternativo educativo]
   ```

2. **Sección Solution**
   ```markdown
   Solution
   ========
   
   [Explicación detallada del proceso de solución]
   
   [Justificación matemática completa]
   
   [Gráficos de apoyo si es necesario]
   
   Answerlist
   ----------
   - Verdadero. [Explicación de por qué es correcta]
   - Falso. [Explicación del error conceptual]
   - Falso. [Explicación del error común]
   - Falso. [Explicación del error alternativo]
   ```

### ⚠️ FASE 8: VALIDACIÓN Y CORRECCIÓN DE ERRORES

**Checklist específico para Gemini:**

1. **Validación Continua Durante Implementación**
   ```
   Después de cada chunk:
   □ ¿Compiló sin errores?
   □ ¿La sintaxis sigue ejemplos funcionales?
   □ ¿Las variables se interpolan correctamente?
   □ ¿No hay caracteres especiales Unicode?
   □ ¿La estructura es coherente?
   ```

2. **Corrección de Errores Recurrentes**
   ```
   Verificar automáticamente:
   - Concordancia de género: "El conteo" no "La conteo"
   - Posicionamiento TikZ: texto → tabla → pregunta
   - Opciones únicas: 4 valores diferentes
   - Compilación LaTeX: paquetes completos
   - Estructura R-exams: YAML correcto
   ```

## 🎯 COMANDOS OPTIMIZADOS PARA GEMINI

### Comando Principal
```
"Aplica las instrucciones optimizadas para Gemini: analiza esta imagen PNG, determina el flujo apropiado (A o B), y genera un ejercicio R-exams ICFES completo siguiendo todos los protocolos establecidos"
```

### Comandos Específicos por Fase
```
FASE 0: "Ejecuta análisis automático de imagen para determinar FLUJO A o B"
FASE 1: "Activa Agente-Graficador para replicación TikZ 98%+ fidelidad"
FASE 2: "Consulta ejemplos funcionales antes de cualquier implementación"
FASE 8: "Aplica validación continua y corrección de errores recurrentes"
```

## 📊 MÉTRICAS DE ÉXITO PARA GEMINI

### Indicadores de Calidad
- ✅ **Análisis de imagen**: 100% detección correcta de contenido gráfico
- ✅ **Fidelidad TikZ**: 98%+ similitud visual cuando aplique
- ✅ **Diversidad**: 300+ versiones únicas verificadas
- ✅ **Compilación**: 100% éxito en HTML, PDF, Word
- ✅ **Alineación ICFES**: Competencia y nivel apropiados
- ✅ **Calidad pedagógica**: Distractores conceptualmente válidos

### Tiempo Estimado por Fase
- **Análisis de imagen**: 2-3 minutos
- **Replicación TikZ** (si aplica): 15-25 minutos
- **Generación de datos**: 5-10 minutos
- **Contenido del ejercicio**: 10-15 minutos
- **Validación final**: 5 minutos
- **Total**: 25-45 minutos según complejidad

## 🔧 ESTADO DEL SISTEMA

**✅ COMPLETAMENTE OPERATIVO**

Todas las metodologías están integradas y optimizadas específicamente para las capacidades del modelo Gemini. El sistema está listo para generar ejercicios R-exams ICFES de alta calidad con replicación gráfica avanzada cuando sea necesario.

## 🚨 RESTRICCIONES CRÍTICAS PARA GEMINI

### Limitaciones Técnicas Obligatorias

1. **Caracteres Especiales Unicode**
   ```
   ❌ NUNCA USAR: α, β, π, ∑, ∫, ≤, ≥, ≠, etc.
   ✅ USAR SIEMPRE: $\alpha$, $\beta$, $\pi$, $\sum$, $\int$, $\le$, $\ge$, $\neq$

   Ejemplo correcto: "La función $f(x) = \pi x^2$ tiene derivada $f'(x) = 2\pi x$"
   Ejemplo incorrecto: "La función f(x) = π x² tiene derivada f'(x) = 2π x"
   ```

2. **Configuración de Tolerancias**
   ```
   Para ejercicios tipo cloze:
   - Tolerancia 0 para respuestas schoice (exactitud requerida)
   - Tolerancia ≥ 1 para respuestas numéricas grandes (monetarios)
   - Tolerancia 0.01-0.1 para respuestas decimales pequeñas
   - Documentar siempre con comentarios explicativos
   ```

3. **Formato Numérico Estándar**
   ```
   - Sin separador de miles en respuestas
   - Punto como separador decimal: options(OutDec = ".")
   - Eliminar notación científica: options(scipen = 999)
   - Usar formato_coma() solo para visualización
   ```

### Errores Críticos a Evitar

1. **Interpolación Incorrecta en TikZ**
   ```
   ❌ Incorrecto: \\draw[', variable, ',thick]
   ✅ Correcto: \\draw[cyan,thick]
   ```

2. **Chunks Mal Cerrados**
   ```
   ❌ Incorrecto: Dejar ``` extra al final
   ✅ Correcto: Verificar cierre correcto de cada chunk
   ```

3. **Concordancia de Género**
   ```
   ❌ Incorrecto: "La conteo de elementos"
   ✅ Correcto: "El conteo de elementos"
   ```

## 📚 RECURSOS OBLIGATORIOS PARA GEMINI

### Archivos de Consulta Prioritaria

1. **Ejemplos Funcionales** (CONSULTA OBLIGATORIA)
   ```
   Ubicación: /A-Produccion/Ejemplos-Funcionales-Rmd/
   Propósito: Patrones probados y validados
   Uso: ANTES de escribir cualquier código
   ```

2. **Documentación TikZ**
   ```
   Ubicación: /Auxiliares/TikZ-Documentation/
   Contenido: Templates, compatibilidad, validación
   Uso: Para replicación gráfica avanzada
   ```

3. **Metodología de Corrección**
   ```
   Ubicación: /Auxiliares/METODOLOGIA_Correccion_Errores_Recurrentes_ICFES_R_Exams.md
   Contenido: Detección y corrección sistemática
   Uso: Durante y después de implementación
   ```

### Bibliotecas de Soluciones

1. **Soluciones de Errores Comunes**
   ```
   Ubicación: /Auxiliares/BIBLIOTECA_Soluciones_Errores_Comunes.md
   Contenido: Correcciones probadas por categoría
   Uso: Aplicar soluciones validadas
   ```

2. **Checklist de Validación**
   ```
   Ubicación: /Auxiliares/CHECKLIST_Validacion_Archivos_Rmd.md
   Contenido: Verificaciones sistemáticas
   Uso: Validación final obligatoria
   ```

## 🎯 SISTEMA AVANZADO DE DISTRACTORES PARA GEMINI

### Implementación Inteligente

1. **Generación de Distractores Conceptuales**
   ```r
   # Sistema de 8+ tipos de errores conceptuales
   generar_distractores_avanzados <- function(respuesta_correcta, contexto) {
     distractores <- list()

     # Tipo 1: Confusión con concepto relacionado
     distractores$confusion_concepto <- generar_confusion_conceptual(respuesta_correcta)

     # Tipo 2: Error de cálculo común
     distractores$error_calculo <- generar_error_calculo(respuesta_correcta)

     # Tipo 3: Aplicación incorrecta de fórmula
     distractores$formula_incorrecta <- generar_formula_incorrecta(respuesta_correcta)

     # Tipo 4-8: Otros errores específicos según contexto
     # ...

     return(distractores)
   }
   ```

2. **Sistema de Valores Duplicados**
   ```r
   # 30% probabilidad de opciones con mismo valor numérico
   # pero justificaciones diferentes
   permitir_valores_duplicados <- sample(c(TRUE, FALSE), 1, prob = c(0.3, 0.7))

   if (permitir_valores_duplicados) {
     # Incluir justificación incorrecta para valor correcto
     justificacion_incorrecta <- paste0(
       "La respuesta es ", valor_correcto,
       " porque [razonamiento matemáticamente plausible pero incorrecto]"
     )
   }
   ```

### Validación de Distractores

1. **Verificación de Unicidad Textual**
   ```r
   # Las 4 opciones deben ser textualmente únicas
   test_that("Opciones textualmente únicas", {
     expect_equal(length(unique(todas_opciones)), 4,
                  info = "Las 4 opciones deben ser textualmente diferentes")
   })
   ```

2. **Diversidad Pedagógica**
   ```r
   # Verificar que distractores reflejen errores reales
   test_that("Distractores pedagógicamente válidos", {
     expect_true(all(sapply(distractores, es_error_conceptual_real)),
                 info = "Todos los distractores deben representar errores conceptuales reales")
   })
   ```

## 🔄 PROTOCOLO DE VALIDACIÓN CONTINUA PARA GEMINI

### Validación Durante Implementación

1. **Después de Cada Chunk**
   ```
   Verificar automáticamente:
   □ Compilación exitosa sin errores
   □ Sintaxis idéntica a ejemplos funcionales
   □ Variables R interpoladas correctamente
   □ No hay caracteres Unicode
   □ Estructura coherente con patrón probado

   Si alguna verificación falla: PARAR y consultar ejemplos
   ```

2. **Señales de Alerta Inmediata**
   ```
   🚨 PARAR SI:
   - Aparecen errores de compilación inesperados
   - La sintaxis no coincide con ejemplos funcionales
   - Variables no se interpolan correctamente
   - Hay caracteres especiales Unicode en el código
   - La estructura difiere de patrones probados
   ```

### Validación Final Completa

1. **Checklist Técnico**
   ```
   □ Encabezado YAML completo y correcto
   □ Configuración inicial sin errores
   □ Función generar_datos() con 300+ versiones
   □ Prueba de diversidad implementada
   □ TikZ (si aplica) con 98%+ fidelidad
   □ Contenido Question/Solution completo
   □ Meta-information correcta
   □ Sin caracteres Unicode en todo el documento
   ```

2. **Checklist Pedagógico**
   ```
   □ Competencia ICFES claramente evaluada
   □ Nivel de dificultad apropiado
   □ Contexto realista y relevante
   □ 4 opciones textualmente únicas
   □ Distractores conceptualmente válidos
   □ Explicación detallada en Solution
   □ Justificación matemática completa
   ```

## 🌐 INVESTIGACIÓN WEB INTELIGENTE PARA GEMINI

### Fuentes Oficiales Prioritarias

1. **Documentación ICFES Oficial**
   ```
   Sitios web prioritarios:
   - www.icfes.gov.co (documentos oficiales)
   - Guías de orientación SABER 11
   - Documentos de competencias matemáticas
   - Ejemplos oficiales de preguntas tipo
   ```

2. **Búsquedas Recomendadas**
   ```
   Términos específicos para Gemini:
   "competencia interpretación representación matemáticas ICFES 2024"
   "formulación ejecución matemáticas SABER 11"
   "argumentación matemáticas ICFES niveles desempeño"
   "estándares competencias matemáticas Colombia MEN"
   ```

### Validación de Información

1. **Criterios de Confiabilidad**
   ```
   Priorizar:
   - Documentación oficial ICFES/MEN
   - Fechas de publicación recientes (2023-2025)
   - Coherencia con ejemplos funcionales existentes
   - Múltiples fuentes oficiales que coincidan
   ```

2. **Integración con Ejercicio**
   ```
   Usar información investigada para:
   - Validar competencia seleccionada
   - Confirmar nivel de dificultad apropiado
   - Verificar contexto y formato
   - Asegurar alineación con estándares oficiales
   ```

## 📊 MÉTRICAS DE CALIDAD ESPECÍFICAS PARA GEMINI

### Indicadores Cuantitativos

1. **Fidelidad Visual** (Solo FLUJO B)
   ```
   Métricas automáticas:
   - Precisión geométrica: ±2% tolerancia
   - Fidelidad cromática: ±5 unidades RGB
   - Posicionamiento: ±2% coordenadas
   - Completitud: 100% elementos principales
   ```

2. **Diversidad de Versiones**
   ```
   Estándares obligatorios:
   - Mínimo 300 versiones únicas verificadas
   - Test automático con digest::digest()
   - Diversidad matemáticamente relevante
   - Evitar aleatorización superficial
   ```

### Indicadores Cualitativos

1. **Calidad Pedagógica**
   ```
   Evaluación manual:
   - Distractores reflejan errores reales de estudiantes
   - Contexto realista y relevante para nivel
   - Explicación clara y completa en Solution
   - Alineación perfecta con competencia ICFES
   ```

2. **Robustez Técnica**
   ```
   Verificación sistemática:
   - Compilación exitosa en HTML, PDF, Word
   - Sin errores de sintaxis o interpolación
   - Configuración de tolerancias apropiada
   - Formato numérico consistente
   ```

## 🎯 COMANDOS FINALES OPTIMIZADOS PARA GEMINI

### Comando Maestro
```
"Ejecuta las instrucciones completas optimizadas para Gemini: analiza la imagen PNG, determina automáticamente FLUJO A o B, consulta ejemplos funcionales obligatoriamente, genera ejercicio R-exams ICFES completo con validación continua y corrección de errores, asegurando 98%+ fidelidad visual si aplica y 300+ versiones únicas"
```

### Comandos de Emergencia
```
"Aplica protocolo anti-errores: consulta ejemplos funcionales y corrige implementación"
"Ejecuta validación completa: técnica y pedagógica según checklist"
"Investiga información ICFES oficial para validar competencia y nivel"
```

---

*Documento optimizado para Gemini v1.0 - Proyecto RepositorioMatematicasICFES_R_Exams*
*Fusión completa de Template de Planificación + Reglas Técnicas Completas*
