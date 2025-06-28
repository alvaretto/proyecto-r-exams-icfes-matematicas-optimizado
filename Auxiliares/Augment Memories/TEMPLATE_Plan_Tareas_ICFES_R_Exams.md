# 🎯 PLAN MAESTRO: Generación/Corrección de Ejercicios ICFES R-exams

## 📋 ESTRUCTURA COMPLETA DE TAREAS

### 🎨 **FASE 1: Análisis de Imagen y Metodología TikZ Avanzada**
*Priorizar TikZ para cualquier gráfica, aplicando metodología avanzada desde el inicio*

- [ ] **📁 1.1 Preparación del Archivo**
  - Colocar nueva imagen en directorio de trabajo bajo `Lab/` (cualquier subdirectorio)
  - **Ejemplo**: `/Lab/Prueba-Temporal_TikZ/nueva_imagen.png` o `/Lab/[proyecto]/imagen.png`
  - Verificar formato PNG y calidad de imagen
  - Crear directorio de trabajo específico si es necesario

- [ ] **🔍 1.2 Análisis Visual Detallado**
  - **OBLIGATORIO**: Analizar imagen compartida identificando:
    - Elementos geométricos (figuras, medidas, ángulos)
    - Elementos algebraicos (ecuaciones, gráficas, funciones)
    - Elementos estadísticos (tablas, gráficos, datos)
    - Colores específicos (extraer códigos RGB exactos)
    - Posicionamiento y proporciones precisas
    - Texto y etiquetas matemáticas

- [ ] **🎨 1.3 Aplicar Metodología TikZ Avanzada**
  - **PRIORIDAD MÁXIMA**: En caso de gráficas (matemáticas o no), generar con código TikZ
  - **OBLIGATORIO**: Consultar `Auxiliares/TikZ-Documentation/TikZ-ICFES-Guide.md` para patrones validados
  - **OBLIGATORIO**: Revisar `/Auxiliares/Ejemplos-Funcionales-Rmd/` para configuraciones TikZ exitosas
  - Replicar elementos visuales con fidelidad 98%+
  - Implementar colores RGB exactos
  - Posicionamiento preciso de componentes
  - Sistema de coordenadas optimizado

- [ ] **🔍 1.4 Consultar Ejemplos Funcionales**
  - **OBLIGATORIO**: Revisar `/Auxiliares/Ejemplos-Funcionales-Rmd/` para patrones exitosos
  - Identificar configuraciones técnicas probadas para TikZ
  - Verificar estructuras de chunks exitosas en archivos FUERA de Lab
  - **🐍 Alternativo**: Solo si TikZ no es viable, consultar `Auxiliares/Python-Documentation/Python-ICFES-Guide.md`

### 📋 **FASE 2: Planificación ICFES y Concepto Matemático**
*Definir estructura del ejercicio ICFES basado en análisis visual*

- [ ] **🎯 2.1 Identificar Competencia ICFES**
  - Determinar competencia: `interpretacion_representacion` | `formulacion_ejecucion` | `argumentacion`
  - Establecer nivel de dificultad: 1, 2, 3, o 4
  - Definir componente: `geometrico_metrico` | `numerico_variacional` | `aleatorio`
  - Seleccionar contexto: `familiar` | `laboral` | `comunitario` | `matematico`
  - **🌐 Investigar en web**: Buscar información oficial ICFES actualizada sobre competencias matemáticas

- [ ] **📊 2.2 Definir Concepto Matemático**
  - Establecer concepto principal basado en análisis visual: álgebra/geometría/estadística
  - Determinar tipo de problema específico
  - Verificar alineación con competencia seleccionada
  - **🌐 Investigar en web**: Consultar documentación oficial sobre contenidos matemáticos ICFES actualizados

- [ ] **🌐 2.3 Investigación Web Complementaria (si es necesario)**
  - Buscar información oficial ICFES sobre competencia específica seleccionada
  - Consultar ejemplos oficiales de preguntas tipo
  - Verificar contextos y niveles de dificultad actualizados
  - Validar definiciones y criterios de evaluación oficiales
  - Contrastar con documentación MEN sobre estándares matemáticos

---

### ⚙️ **FASE 3: Configuración Técnica Base**
*Implementar estructura técnica siguiendo ejemplos funcionales con TikZ integrado*

- [ ] **📄 3.1 Encabezado YAML Completo con TikZ**
  ```yaml
  ---
  output:
    html_document: default
    word_document: default
    pdf_document:
      keep_tex: true
      extra_dependencies: ["graphicx", "float", "tikz", "xcolor"]
  
  # Metadatos ICFES
  icfes:
    competencia: [competencia_seleccionada]
    nivel_dificultad: [1-4]
    contenido:
      categoria: [algebra_calculo|geometria|estadistica]
      tipo: [generico|no_generico]
    contexto: [familiar|laboral|comunitario|matematico]
    eje_axial: [eje1|eje2|eje3|eje4]
    componente: [geometrico_metrico|numerico_variacional|aleatorio]
  ---
  ```

- [ ] **🔧 3.2 Chunk Setup Inicial con TikZ**
  - Configurar `Sys.setlocale()` y `options(OutDec = ".")`
  - **PRIORIDAD**: Establecer opciones LaTeX y TikZ desde el inicio
  - Cargar librerías esenciales: `exams`, `reticulate`, `digest`, `testthat`, `knitr`
  - Configurar `knitr::opts_chunk$set()` con parámetros apropiados para TikZ
  - Establecer semilla aleatoria: `set.seed(sample(1:100000, 1))`

- [ ] **🎨 3.3 Configuración TikZ Prioritaria**
  - **🎨 OBLIGATORIO**: Consultar `Auxiliares/TikZ-Documentation/referencias/compatibilidad.md` para configuración validada
  - Aplicar configuración LaTeX: `options(tikzLatex = "pdflatex")`
  - Configurar bibliotecas TikZ validadas: básicas, calc, positioning, arrows
  - **🎨 Usar templates**: `Auxiliares/TikZ-Documentation/templates-rexams/` para diagramas validados
  - Verificar compatibilidad multi-formato según checklist de compatibilidad

- [ ] **🐍 3.4 Configuración Python-R (Solo si TikZ no es viable)**
  - **🐍 Consultar**: `Auxiliares/Python-Documentation/referencias/compatibilidad-python.md` para setup validado
  - Configurar `use_python("/usr/bin/python3", required = TRUE)` o `use_python(Sys.which("python"), required = TRUE)`
  - Establecer `knitr::knit_engines$set(python = ...)` según patrón exitoso
  - Verificar configuración matplotlib: `matplotlib.rcParams['font.size'] = 9`
  - **🐍 Usar templates**: `Auxiliares/Python-Documentation/templates-rexams/` para gráficos validados
  - Validar transferencia R→Python: `variable_python = r.variable_r`

---

### 🎲 **FASE 4: Generación de Datos Aleatorios**
*Crear función de generación con mínimo 300 versiones únicas*

- [ ] **🔢 4.1 Función generar_datos()**
  - Implementar aleatorización de contextos (mínimo 8-10 escenarios)
  - Generar parámetros numéricos variables con rangos realistas
  - Incluir aleatorización de nombres, colores, unidades
  - Asegurar coherencia matemática en todos los casos
  - Retornar lista estructurada con todos los parámetros

- [ ] **✅ 4.2 Prueba de Diversidad**
  ```r
  test_that("Prueba de diversidad de versiones", {
    versiones <- list()
    for(i in 1:1000) {
      datos_test <- generar_datos()
      versiones[[i]] <- digest::digest(datos_test)
    }
    
    n_versiones_unicas <- length(unique(versiones))
    expect_true(n_versiones_unicas >= 300,
                info = paste("Solo se generaron", n_versiones_unicas,
                            "versiones únicas. Se requieren al menos 300."))
  })
  ```

- [ ] **🛡️ 4.3 Validaciones Matemáticas**
  - Validar rangos de valores realistas
  - Verificar coherencia entre parámetros relacionados
  - Implementar manejo de casos extremos
  - Asegurar que no hay divisiones por cero o valores inválidos

---

### 📈 **FASE 5: Visualizaciones y Gráficos**
*PRIORIZAR TikZ para cualquier gráfica, usar Python solo como alternativa*

- [ ] **🎨 5.1 Diagramas TikZ (PRIORIDAD MÁXIMA)**
  - **🎨 OBLIGATORIO**: Consultar `Auxiliares/TikZ-Documentation/TikZ-ICFES-Guide.md` para patrones exitosos validados
  - **🎨 Usar templates**: `Auxiliares/TikZ-Documentation/templates-rexams/icfes-aligned/` para diagramas específicos
  - **Patrones exitosos validados**:
    - **Tablas**: `tabla-datos-template.tikz` (patrón más compatible)
    - **Venn**: `diagrama-venn-template.tikz` (basado en DVenn_All_GenMus_01.Rmd)
    - **Geometría**: Templates parametrizables con variables R
    - **Funciones**: Gráficas matemáticas con coordenadas precisas
    - **Estadística**: Histogramas, barras, circulares con TikZ
  - Usar `include_tikz()` con packages validados: `c("tikz", "colortbl", "xcolor")`
  - Configurar width apropiado según template: "6cm" para tablas, "5cm" para Venn
  - Establecer `markup = "markdown"` según patrón exitoso
  - **Aplicar fidelidad 98%** con imagen original usando coordenadas exactas

- [ ] **🐍 5.2 Gráficos Python/matplotlib (Solo si TikZ no es viable)**
  - Usar `py_run_string()` con sintaxis corregida
  - Configurar `matplotlib.rcParams` apropiadamente
  - Implementar `plt.plot()` con sintaxis verificada en ejemplos
  - Guardar con `plt.savefig()` en alta resolución

- [ ] **📊 5.3 Gráficos Python-matplotlib (Alternativa secundaria)**
  - **🐍 Consultar**: `Auxiliares/Python-Documentation/Python-ICFES-Guide.md` para patrones exitosos validados
  - **🐍 Usar templates**: `Auxiliares/Python-Documentation/templates-rexams/icfes-aligned/` para gráficos específicos
  - **Patrones exitosos validados**:
    - **Barras**: `grafico-barras-template.py` (basado en I_1796473-Opc-A2v2.Rmd)
    - **Circulares**: `grafico-circular-template.py` (basado en I_1796473-Opc-A2.Rmd)
    - **Funciones**: `funciones-lineales-template.py` (basado en vuelo_acrobatico_A.Rmd)
  - Usar transferencia R→Python validada: `variable_python = r.variable_r`
  - Configurar chunks: `echo=FALSE, message=FALSE, results="hide"`
  - **OBLIGATORIO**: Incluir `plt.show()` al final de cada chunk Python

- [ ] **📊 5.4 Gráficos ggplot2 (Última alternativa)**
  - Implementar con `theme_minimal()` solo si TikZ y Python no son viables
  - Usar colores aleatorios para diversidad
  - Configurar DPI 150+ para calidad
  - Incluir etiquetas claras y leyendas

---

### 📝 **FASE 6: Contenido del Ejercicio**
*Desarrollar Question, Solution y Meta-information*

- [ ] **❓ 6.1 Sección Question**
  - Redactar contexto realista y relevante
  - Formular pregunta clara según competencia ICFES
  - **🎯 Crear 4 opciones con sistema avanzado de distractores:**
    - Generar 8+ tipos diferentes de distractores (confusión conceptual, errores de cálculo, posiciones incorrectas, etc.)
    - **30% probabilidad**: Incluir valores duplicados con justificaciones diferentes (ej: "mediana es 30 porque promedio centrales" vs "mediana es 30 porque suma/división")
    - **70% probabilidad**: Mantener todos los valores diferentes (modo tradicional)
    - Selección estratégica: 1 distractor con valor duplicado + 2 con valores diferentes
    - Verificar que las 4 opciones sean textualmente únicas
    - Asegurar distractores plausibles y educativos
  - Incluir gráficos/tablas si es necesario

- [ ] **💡 6.2 Sección Solution**
  - Proporcionar explicación detallada del proceso
  - Incluir justificación matemática completa
  - Crear Answerlist con Verdadero/Falso para cada opción
  - Explicar por qué cada distractor es incorrecto

- [ ] **📋 6.3 Meta-information**
  ```
  Meta-information
  ================
  exname: [nombre_descriptivo]
  extype: schoice
  exsolution: [patrón_respuesta]
  exshuffle: TRUE
  exsection: [sección_temática]
  ```

---

### 🔧 **FASE 7: Corrección de Errores Recurrentes**
*Aplicar metodología avanzada de detección y corrección sistemática*

- [ ] **🔍 7.1 Detección Automática de Errores**
  - **OBLIGATORIO**: Consultar `/Auxiliares/METODOLOGIA_Correccion_Errores_Recurrentes_ICFES_R_Exams.md`
  - Ejecutar detección de 5 categorías de errores:
    * **A) Gramaticales/Concordancia**: Verificar "El conteo" vs "La cantidad" (no "La conteo")
    * **B) Posicionamiento TikZ**: Confirmar orden texto → tabla → pregunta
    * **C) Generación de datos**: Validar opciones únicas, anti-duplicados
    * **D) Compilación LaTeX**: Verificar paquetes, caracteres especiales
    * **E) Estructura R-exams**: Revisar YAML, include_tikz, variables
  - Aplicar función `detectar_errores_comunes(archivo_rmd)`

- [ ] **📚 7.2 Aplicar Soluciones Probadas**
  - **OBLIGATORIO**: Consultar `/Auxiliares/BIBLIOTECA_Soluciones_Errores_Comunes.md`
  - **OBLIGATORIO**: Re-consultar ejemplos funcionales en `/Auxiliares/Ejemplos-Funcionales-Rmd/`
  - Implementar correcciones sistemáticas por categoría:
    * **A1**: Sistema automático de concordancia de género
    * **B2**: Reordenar elementos TikZ (texto primero, tabla después)
    * **C1**: Implementar generación de opciones únicas robusta
    * **D1**: Configurar paquetes LaTeX completos
    * **E2**: Configurar include_tikz con parámetros completos
  - Aplicar soluciones validadas sin introducir nuevos errores

- [ ] **✅ 7.3 Checklist de Validación Sistemática**
  - **OBLIGATORIO**: Ejecutar `/Auxiliares/CHECKLIST_Validacion_Archivos_Rmd.md`
  - **Pre-compilación**:
    * [ ] Verificar concordancia de género en variables dinámicas
    * [ ] Confirmar orden correcto en elementos TikZ
    * [ ] Validar unicidad en opciones de respuesta
    * [ ] Revisar configuración completa de paquetes
  - **Post-compilación**:
    * [ ] Verificar output visual (tabla después de texto)
    * [ ] Confirmar que todas las opciones son diferentes
    * [ ] Revisar gramática en resultado final
    * [ ] Validar cálculos matemáticos

- [ ] **🔄 7.4 Corrección Iterativa**
  - Aplicar protocolo de corrección rápida (< 5 minutos) para errores comunes
  - Usar protocolo de corrección compleja (> 5 minutos) para errores múltiples
  - Documentar nuevos patrones de error encontrados
  - Actualizar biblioteca de soluciones si es necesario

---

### 🔍 **FASE 8: Validación y Testing Final**
*Verificar funcionamiento completo después de correcciones*

- [ ] **🧪 8.1 Testing Automatizado Post-Corrección**
  - Ejecutar pruebas de diversidad de versiones
  - Verificar validaciones matemáticas
  - Comprobar coherencia de datos generados
  - **🎯 Validar sistema avanzado de distractores:**
    - Verificar que las 4 opciones sean textualmente únicas
    - Comprobar funcionamiento de valores duplicados (30% casos)
    - Validar selección estratégica de distractores
    - Confirmar justificaciones alternativas apropiadas
    - Probar múltiples generaciones para verificar diversidad

- [ ] **🔧 8.2 Validación de Correcciones Aplicadas**
  - **VERIFICAR**: Que todas las correcciones de Fase 7 se mantienen
  - Confirmar que no se introdujeron nuevos errores
  - **🌐 Si persisten errores**: Investigar información oficial ICFES actualizada
  - Re-aplicar correcciones basadas en patrones exitosos
  - **🐍 Errores Python**: Consultar `Auxiliares/Python-Documentation/referencias/compatibilidad-python.md` para soluciones
  - **🎨 Errores TikZ**: Consultar `Auxiliares/TikZ-Documentation/referencias/compatibilidad.md` para soluciones
  - **⚠️ OBLIGATORIO - Error "\pandocbounded"**: Para corregir cualquier error relacionado con "pandocbounded" buscar soluciones en `/Auxiliares/Ejemplos-Funcionales-Rmd/`

- [ ] **🛠️ 8.3 Validación Específica TikZ/Python**
  - **🎨 TikZ**: Ejecutar `source("Auxiliares/TikZ-Documentation/validar_tikz_compatibility.R")` si aplica
  - **🐍 Python**: Usar herramientas de `Auxiliares/Python-Documentation/templates-rexams/multi-formato/python-rexams-system.R`
  - Verificar compatibilidad multi-formato según checklists específicos
  - Probar generación en PDF, HTML, y Moodle
  - Validar que gráficos/diagramas se rendericen correctamente

- [ ] **✅ 8.4 Compilación Final Validada**
  - Verificar compilación HTML: `rmarkdown::render(archivo, 'html_document')`
  - Probar compilación PDF: `rmarkdown::render(archivo, 'pdf_document')`
  - Confirmar compilación Word: `rmarkdown::render(archivo, 'word_document')`
  - Validar que todos los gráficos se generen correctamente
  - **CONFIRMAR**: Que todas las correcciones de errores recurrentes funcionan correctamente

---

## 🎯 **METODOLOGÍA TIKZ AVANZADA PARA NUEVAS IMÁGENES PNG**

### 📋 **PROTOCOLO PASO A PASO PARA REPLICACIÓN DE IMÁGENES**

#### **PASO 1: Preparación del Archivo**
```bash
# Colocar nueva imagen en directorio de trabajo bajo Lab/
/Lab/[proyecto]/nueva_imagen.png
# Ejemplo: /Lab/Prueba-Temporal_TikZ/nueva_imagen.png
```

#### **PASO 2: Análisis Visual Detallado**
1. **Identificar elementos matemáticos** (figuras, ecuaciones, gráficas)
2. **Extraer colores RGB** exactos de la imagen
3. **Medir proporciones** y posicionamiento relativo
4. **Catalogar texto** y etiquetas matemáticas

#### **PASO 3: Consulta de Ejemplos Funcionales**
1. **OBLIGATORIO**: Revisar `/Auxiliares/Ejemplos-Funcionales-Rmd/`
2. **Identificar patrones TikZ** similares exitosos
3. **Extraer configuraciones** técnicas probadas
4. **Planificación** de la estructura TikZ

#### **PASO 4: Implementación Sistemática**

##### **4.1 Generación del Código TikZ**
- Aplicar metodología TikZ avanzada
- RGB colors exactos
- Posicionamiento preciso
- Estilos reutilizables

##### **4.2 Creación del .Rmd**
- Estructura completa R-exams
- Sistema de aleatorización
- Generación de distractores
- Meta-información ICFES

##### **4.3 Configuración de Salidas**
- Actualizar SemilleroUnico_v2.R
- Configurar formatos exams2*
- Verificar compatibilidad

##### **4.4 Pruebas y Validación**
- Generar HTML, PDF, Moodle
- Verificar fidelidad visual
- Comprobar funcionalidad

### 🔧 **PLANTILLA DE ARCHIVOS GENERADOS**

Para cada nueva imagen, se crearán:
```
📁 Lab/[proyecto]/
├── 📄 [nombre_ejercicio]_v1.Rmd          # Ejercicio principal
├── 📄 SemilleroUnico_v2.R                # Configuración actualizada
├── 📁 salida/
│   ├── 🌐 [nombre]_test.html             # Salida HTML
│   ├── 📄 [nombre]_test.pdf              # Salida PDF
│   └── 🎓 [nombre]_moodle.xml            # Salida Moodle
└── 📄 REPORTE_[NOMBRE].md                # Documentación
```

### ⚡ **COMANDO RÁPIDO PARA EMPEZAR**

Comando para nueva imagen:
> **"Aplica la metodología TikZ avanzada a esta nueva imagen PNG para generar un ejercicio R-exams completo con salidas exams2*"**

---

## 🎯 **CRITERIOS DE CALIDAD OBLIGATORIOS**

### ✅ **Aleatorización Avanzada:**
- Mínimo 300 versiones únicas verificadas
- Contextos, valores, colores, nombres variables
- Orden aleatorio de opciones

### ✅ **Robustez Matemática:**
- Validaciones de coherencia
- Manejo de casos extremos
- Precisión numérica apropiada

### ✅ **Calidad Gráfica:**
- Resolución mínima 150 DPI
- Etiquetas claras y legibles
- Colores contrastantes

### ✅ **Alineación ICFES:**
- Competencia claramente evaluada
- Nivel de dificultad apropiado
- Contexto realista y relevante
- Distractores plausibles y educativos

### ✅ **Sistema Avanzado de Distractores:**
- **Diversidad**: Mínimo 8 tipos diferentes de errores conceptuales
- **Valores Duplicados**: 30% probabilidad de opciones con mismo valor numérico pero justificaciones diferentes
- **Selección Estratégica**: 1 distractor duplicado + 2 diferentes cuando aplique
- **Verificación Textual**: Las 4 opciones siempre textualmente únicas
- **Justificaciones Alternativas**: Múltiples explicaciones incorrectas para valores correctos
- **Pedagogía**: Distractores que reflejan errores comunes de estudiantes

---

## 🌐 **INVESTIGACIÓN WEB PARA MATEMÁTICAS ICFES**

### 🎯 **Fuentes Oficiales Prioritarias:**
- **ICFES Oficial**: `www.icfes.gov.co`, Documentos oficiales, guías de orientación
- **Ministerio de Educación**: Estándares básicos de competencias matemáticas
- **Documentos SABER 11**: Estructura de pruebas, niveles de desempeño
- **Guías de orientación actualizadas**: Competencias, contenidos, contextos

### 🔍 **Búsquedas Recomendadas:**
```
"competencia argumentación matemáticas ICFES 2025"
"interpretación representación matemáticas SABER 11"
"formulación ejecución matemáticas ICFES"
"niveles desempeño matemáticas ICFES"
"estándares competencias matemáticas Colombia"
```

### 📊 **Información a Investigar:**
- **Competencias**: Definiciones oficiales, ejemplos, criterios de evaluación
- **Contenidos**: Categorías actualizadas (álgebra, geometría, estadística)
- **Contextos**: Tipos de situaciones evaluadas (familiar, laboral, etc.)
- **Niveles**: Descriptores de desempeño por nivel de dificultad
- **Ejemplos**: Preguntas tipo, estructuras, formatos

### ⚠️ **Criterios de Validación:**
- Priorizar documentación oficial ICFES/MEN
- Verificar fecha de publicación (preferir 2023-2025)
- Contrastar con múltiples fuentes oficiales
- Validar coherencia con ejemplos funcionales existentes

---

## 🎯 **IMPLEMENTACIÓN DEL SISTEMA AVANZADO DE DISTRACTORES**

### 📝 **Código Base para Distractores con Valores Duplicados:**

```r
# DECISIÓN ALEATORIA: ¿Permitir valores duplicados con justificaciones diferentes?
# 30% de probabilidad de generar opciones con mismo valor pero diferentes justificaciones
permitir_valores_duplicados <- sample(c(TRUE, FALSE), 1, prob = c(0.3, 0.7))

# SISTEMA AMPLIADO DE DISTRACTORES (8+ opciones para mayor diversidad)
afirmaciones_incorrectas <- c()

# DISTRACTOR 1: Confundir concepto principal con media
media_calculada <- round(mean(datos_ordenados), 1)
afirmaciones_incorrectas <- c(afirmaciones_incorrectas,
  paste0("La [concepto] es ", media_calculada, " porque se calcula sumando todos los valores y dividiendo por el número de datos"))

# DISTRACTOR 2-8: [Implementar según el concepto matemático específico]
# - Confusión con moda, extremos, posiciones incorrectas
# - Errores de cálculo comunes
# - Aplicación incorrecta de fórmulas
# - Interpretaciones erróneas del procedimiento

# JUSTIFICACIONES ALTERNATIVAS para el valor correcto (pero con razonamiento incorrecto)
justificaciones_incorrectas_valor_correcto <- c(
  paste0("La [concepto] es ", valor_correcto, " porque representa el punto medio del rango"),
  paste0("La [concepto] es ", valor_correcto, " porque es el valor que mejor representa el conjunto"),
  paste0("La [concepto] es ", valor_correcto, " porque se obtiene al aplicar la fórmula básica")
)

# LÓGICA DE SELECCIÓN ADAPTADA
if(permitir_valores_duplicados) {
  # Incluir 1 justificación incorrecta para el valor correcto + 2 valores diferentes
  # [Implementar lógica de selección estratégica]
} else {
  # Modo tradicional: todos los valores diferentes
  # [Implementar selección estándar]
}

# VERIFICACIÓN FINAL: Asegurar 4 opciones textualmente únicas
expect_equal(length(unique(todas_afirmaciones)), 4,
            info = "Las 4 opciones deben ser textualmente diferentes")
```

### 🧪 **Pruebas Específicas para Distractores:**

```r
test_that("Prueba del sistema avanzado de distractores", {
  for(i in 1:50) {
    datos_test <- generar_datos()

    # Verificar opciones textualmente únicas
    expect_equal(length(unique(datos_test$opciones)), 4,
                info = "Las 4 opciones deben ser textualmente diferentes")

    # Verificar diversidad de distractores
    valores_numericos <- extraer_valores_numericos(datos_test$opciones)
    expect_true(length(unique(valores_numericos)) >= 2,
               info = "Debe haber al menos 2 valores numéricos diferentes")

    # Verificar respuesta correcta presente
    expect_true(datos_test$afirmacion_correcta %in% datos_test$opciones,
               info = "La respuesta correcta debe estar presente")
  }
})
```

---

## 🔧 **COMANDOS DE USO RÁPIDO**

### Para investigar información ICFES:
```
brave_web_search_brave-search: "término específico ICFES matemáticas 2025"
web-fetch: [URL oficial ICFES]
```

### Para crear tareas nuevas:
```
add_tasks con esta estructura como base
```

### Para actualizar progreso:
```
update_tasks con task_id y nuevo state
```

### Para compilar y probar:
```
rmarkdown::render('archivo.Rmd', 'html_document')
```

---

## 📌 **NOTAS IMPORTANTES**

### 🔍 **Investigación Obligatoria:**
- **SIEMPRE** investigar información teórica ICFES en web cuando sea necesario
- Priorizar documentación oficial y actualizada (2023-2025)
- Validar competencias, contenidos y contextos con fuentes oficiales
- Contrastar definiciones con ejemplos funcionales existentes

### 🛠️ **Desarrollo Técnico:**
- **SIEMPRE** consultar ejemplos funcionales ANTES de cualquier generación, corrección u optimización
- Seguir patrones técnicos probados en `/Auxiliares/Ejemplos-Funcionales-Rmd/`
- Aplicar configuraciones exitosas de chunks, librerías y sintaxis
- **⚠️ OBLIGATORIO - Error "\pandocbounded"**: Para corregir cualquier error relacionado con "pandocbounded" buscar soluciones en `/Auxiliares/Ejemplos-Funcionales-Rmd/`

### ⚠️ **RESTRICCIÓN CRÍTICA - CARACTERES ESPECIALES:**
- **NO USAR CARACTERES ESPECIALES UNICODE** en ninguna parte del código R-exams
- **USAR ÚNICAMENTE**:
  - Expresiones LaTeX para símbolos matemáticos: `$\alpha$`, `$\beta$`, `$\pi$`, `$\sum$`, `$\int$`, etc.
  - Sintaxis TikZ para diagramas y figuras geométricas
  - Caracteres ASCII estándar para texto
- **EVITAR COMPLETAMENTE**:
  - Símbolos Unicode: α, β, π, ∑, ∫, ≤, ≥, ≠, etc.
  - Caracteres especiales directos en el texto
  - Emojis o símbolos decorativos
- **EJEMPLO CORRECTO**: `La función $f(x) = \pi x^2$ tiene derivada $f'(x) = 2\pi x$`
- **EJEMPLO INCORRECTO**: `La función f(x) = π x² tiene derivada f'(x) = 2π x`

### 🎯 **Calidad Final:**
- Combinar investigación teórica oficial con implementación técnica probada
- Asegurar alineación perfecta entre competencia ICFES y ejercicio desarrollado
- Validar que el ejercicio cumple estándares oficiales actualizados
- **VERIFICAR** que no hay caracteres Unicode en todo el documento

### 🎯 **Sistema de Distractores Avanzado:**
- **IMPLEMENTAR SIEMPRE** el sistema de valores duplicados con justificaciones diferentes
- Generar mínimo 8 tipos de distractores para máxima diversidad pedagógica
- Verificar que los distractores reflejen errores conceptuales reales de estudiantes
- Asegurar que las justificaciones alternativas sean matemáticamente plausibles pero incorrectas
- Probar múltiples generaciones para confirmar variedad en combinaciones de opciones

---

## 🌐 **BÚSQUEDA RECURSIVA DE RECURSOS TikZ Y PYTHON**

### 🎯 **Enriquecimiento Continuo de Documentación**

#### **🎨 BÚSQUEDA RECURSIVA TikZ**
- [ ] **🔍 Identificar Necesidades TikZ Específicas**
  - Analizar ejercicio actual para determinar tipo de diagrama requerido
  - Consultar `Auxiliares/TikZ-Documentation/TikZ-ICFES-Guide.md` para gaps identificados
  - Verificar si templates existentes cubren la necesidad
  - **🌐 Buscar recursos web** si no existe template apropiado

- [ ] **🌐 Fuentes Web TikZ Prioritarias**
  - **TeXample.net**: http://www.texample.net/tikz/ (buscar por categoría matemática)
  - **PGFPlots Gallery**: http://pgfplots.sourceforge.net/gallery.html (gráficos estadísticos)
  - **GitHub**: Repositorios "tikz mathematics", "tikz education"
  - **Overleaf**: Templates TikZ matemáticos y educativos

- [ ] **🔧 Adaptación TikZ para R-exams**
  - Simplificar código encontrado según `Auxiliares/TikZ-Documentation/referencias/compatibilidad.md`
  - Convertir `\pgfmathsetmacro` a variables R
  - Usar colores estándar en lugar de personalizados
  - Validar con `source("Auxiliares/TikZ-Documentation/validar_tikz_compatibility.R")`

#### **🐍 BÚSQUEDA RECURSIVA PYTHON**
- [ ] **🔍 Identificar Necesidades Python Específicas**
  - Analizar ejercicio actual para determinar tipo de gráfico requerido
  - Consultar `Auxiliares/Python-Documentation/Python-ICFES-Guide.md` para gaps identificados
  - Verificar si templates existentes cubren la necesidad
  - **🌐 Buscar recursos web** si no existe template apropiado

- [ ] **🌐 Fuentes Web Python Prioritarias**
  - **Matplotlib Gallery**: https://matplotlib.org/stable/gallery/ (ejemplos oficiales)
  - **Python for Education**: Recursos educativos con matplotlib
  - **GitHub**: Repositorios "matplotlib education", "python mathematics"
  - **Jupyter Notebooks**: Ejemplos educativos de visualización

- [ ] **🔧 Adaptación Python para R-exams**
  - Simplificar código encontrado según `Auxiliares/Python-Documentation/referencias/compatibilidad-python.md`
  - Usar solo matplotlib y numpy (bibliotecas validadas)
  - Implementar transferencia R→Python: `variable_python = r.variable_r`
  - Configurar chunks: `echo=FALSE, message=FALSE, results="hide"`
  - **OBLIGATORIO**: Agregar `plt.show()` al final

#### **📥 INTEGRACIÓN AL PROYECTO**
- [ ] **📁 Organizar Nuevos Recursos**
  - **TikZ**: Guardar en `Auxiliares/TikZ-Documentation/` según clasificación ICFES
  - **Python**: Guardar en `Auxiliares/Python-Documentation/` según clasificación ICFES
  - Documentar fuente original y adaptaciones realizadas
  - Crear template reutilizable si el recurso es valioso

- [ ] **✅ Validar Nuevos Recursos**
  - Probar compatibilidad multi-formato (PDF, HTML, Moodle)
  - Verificar que funciona con variables aleatorias
  - Documentar en guías principales si es exitoso
  - Agregar a templates disponibles para futuros ejercicios

### 🎯 **Criterios de Selección para Búsqueda Recursiva**

#### **✅ Recursos TikZ Prioritarios**
- Diagramas geométricos 2D y 3D para pensamiento espacial
- Tablas y esquemas para presentación de datos
- Diagramas de Venn y conjuntos para pensamiento aleatorio
- Construcciones geométricas para geometría métrica

#### **✅ Recursos Python Prioritarios**
- Gráficos estadísticos avanzados para pensamiento aleatorio
- Funciones matemáticas para pensamiento variacional
- Representaciones numéricas para pensamiento numérico
- Visualizaciones geométricas 2D para pensamiento espacial

#### **🔧 Criterios de Compatibilidad**
- **TikZ**: Compatible con `include_tikz()` y bibliotecas básicas
- **Python**: Compatible con matplotlib/numpy y transferencia R→Python
- **R-exams**: Funciona en PDF, HTML, y Moodle sin errores
- **ICFES**: Alineado con competencias y niveles de dificultad
- **Escalabilidad**: Soporta múltiples variantes aleatorias

### ⚡ **Implementación Inmediata**
1. **Identificar necesidad específica** del ejercicio actual
2. **Buscar recurso apropiado** en fuentes web prioritarias
3. **Adaptar según guías de compatibilidad** específicas
4. **Validar funcionamiento** en múltiples formatos
5. **Documentar y compartir** si es exitoso para futuros ejercicios




