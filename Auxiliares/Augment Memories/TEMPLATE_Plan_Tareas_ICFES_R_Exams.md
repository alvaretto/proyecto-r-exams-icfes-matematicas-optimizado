---
output:
  html_document: default
  pdf_document: default
  word_document: default
---
# 🎯 PLAN MAESTRO: Generación/Corrección de Ejercicios ICFES R-exams

## 📋 ESTRUCTURA COMPLETA DE TAREAS

### 🎨 **FASE 1: Análisis Automático de Imagen y Sistema Condicional TikZ**
*Sistema inteligente de detección automática de contenido gráfico con flujos especializados*

### 🧠 **FASE 0: Análisis de Patrones Pedagógicos (NUEVA)**
*Identificación de patrones de sobre-ingeniería vs valor educativo real*

- [ ] **🔍 0.1 Análisis Profundo de Patrones Existentes**
  - Identificar aleatorización superficial vs diversidad matemáticamente relevante
  - Detectar complejidad técnica innecesaria vs sofisticación que aporta valor
  - Evaluar distractores débiles vs errores conceptuales reales
  - Aplicar "malicia indígena" para detectar sobre-ingeniería

- [ ] **⚖️ 0.2 Balanceo Técnico-Pedagógico**
  - Principio: "Elegancia técnica donde aporta valor educativo real"
  - Simplificar elementos que no mejoran la experiencia matemática
  - Mantener sofisticación en aspectos educativamente relevantes
  - Respetar enfoque original del problema

- [ ] **📁 1.1 Preparación del Archivo**
  - Colocar nueva imagen en directorio de trabajo bajo `Lab/` (cualquier subdirectorio)
  - **Ejemplo**: `/Lab/Prueba-Temporal_TikZ/nueva_imagen.png` o `/Lab/[proyecto]/imagen.png`
  - Verificar formato PNG y calidad de imagen
  - Crear directorio de trabajo específico si es necesario

- [ ] **🤖 1.2 ANÁLISIS AUTOMÁTICO DE CONTENIDO GRÁFICO**
  - **🔍 1.2.1 Detección Automática de Elementos Visuales**
    - **PROCESO AUTOMÁTICO**: Analizar imagen PNG para detectar:
      * **Gráficas**: Barras, líneas, circulares, histogramas, dispersión, boxplots
      * **Tablas**: Numéricas, textuales, mixtas, con/sin encabezados
      * **Diagramas**: Matemáticos, estadísticos, geométricos, probabilidad
      * **Elementos Híbridos**: Gráfica + tabla en misma imagen
      * **Contenido Simple**: Solo texto, formas básicas, elementos no gráficos
    - **OUTPUT**: Clasificación binaria con justificación del análisis

  - **⚡ 1.2.2 DECISIÓN DE FLUJO CONDICIONAL**
    - **FLUJO A** (Sin gráficas/tablas detectadas):
      * Ejecutar secuencia estándar FASES 1-8
      * TikZ solo para elementos simples (texto, formas básicas)
      * Continuar con metodología TikZ básica
    - **FLUJO B** (Con gráficas/tablas detectadas):
      * **ACTIVAR**: Agente-Graficador Especializado TikZ
      * **OBJETIVO**: Replicación de alta fidelidad (98%+)
      * **PROCESO**: Iterativo hasta validación usuario-sistema

- [ ] **🎯 1.3 AGENTE-GRAFICADOR ESPECIALIZADO (Solo FLUJO B)**
  - **🎨 1.3.1 Activación del Agente-Graficador**
    - **FUNCIÓN EXCLUSIVA**: Replicación de alta fidelidad de elementos gráficos/tabulares
    - **TECNOLOGÍA**: Código TikZ avanzado con características especializadas:
      * Colores RGB exactos extraídos de imagen original
      * Posicionamiento preciso mediante coordenadas calculadas
      * Estilos reutilizables y escalables
      * Configuración completa de paquetes LaTeX necesarios
    - **BASE METODOLÓGICA**: Expandir "Metodología TikZ Avanzada" existente

  - **🔄 1.3.2 Proceso Iterativo de Replicación**
    - **OBJETIVO**: Alcanzar 98%+ fidelidad visual antes de continuar
    - **PROCESO**:
      1. Generar código TikZ inicial basado en tipo detectado
      2. Renderizar y comparar con imagen original
      3. Identificar discrepancias específicas
      4. Ajustar parámetros (colores, coordenadas, proporciones)
      5. Repetir hasta alcanzar criterios de fidelidad
    - **MÉTRICAS DE FIDELIDAD**:
      * Precisión Geométrica (25%): Proporciones, ángulos, escalas
      * Fidelidad Cromática (25%): Colores RGB exactos, contrastes
      * Posicionamiento (25%): Ubicación relativa de elementos
      * Completitud (25%): Todos los elementos presentes

  - **✅ 1.3.3 Validación Usuario-Sistema de Fidelidad**
    - **COMPARACIÓN VISUAL**: Lado a lado (PNG original vs TikZ renderizado)
    - **CHECKLIST FIDELIDAD**: Elementos críticos verificados
    - **MÉTRICAS CUANTIFICABLES**: Coordenadas, colores, dimensiones
    - **APROBACIÓN REQUERIDA**: Usuario confirma 98%+ fidelidad antes de continuar
    - **CRITERIOS DE ACEPTACIÓN**: Todos los elementos principales replicados correctamente

  - **🚀 1.3.4 Integración R-exams Validada**
    - **CÓDIGO TIKZ DOCUMENTADO**: Listo para inserción en chunks específicos
    - **COMPATIBILIDAD VERIFICADA**: include_tikz() y configuración YAML completa
    - **VARIABLES R INTEGRADAS**: Parametrización para aleatorización
    - **MULTI-FORMATO**: Funciona en exams2html, exams2pdf, exams2moodle

- [ ] **🔍 1.4 Consultar Ejemplos Funcionales (Ambos Flujos) - PROTOCOLO ESTRICTO**
  - **OBLIGATORIO ABSOLUTO**: Revisar `/Auxiliares/Ejemplos-Funcionales-Rmd/` ANTES de escribir cualquier código
  - **FLUJO A**: Identificar configuraciones técnicas básicas para TikZ simple
  - **FLUJO B**: Consultar templates avanzados para gráficas complejas
  - **COPIAR PATRONES EXACTOS**: No improvisar, usar sintaxis idéntica a ejemplos probados
  - **VERIFICAR ESTRUCTURAS**: Chunks, configuraciones LaTeX, interpolación de variables
  - **🐍 Alternativo**: Solo si TikZ no es viable, consultar `Auxiliares/Python-Documentation/Python-ICFES-Guide.md`

- [ ] **⚡ 1.5 PROTOCOLO ANTI-ERRORES DE IMPLEMENTACIÓN**
  - **🎯 1.5.1 Auto-Verificación Pre-Implementación**
    - **CHECKLIST OBLIGATORIO ANTES DE ESCRIBIR CÓDIGO**:
      * ✅ ¿Consulté TODOS los ejemplos funcionales relevantes?
      * ✅ ¿Identifiqué el patrón exacto a seguir?
      * ✅ ¿Entiendo la sintaxis TikZ/LaTeX del ejemplo?
      * ✅ ¿Sé cómo interpolar variables R correctamente?
      * ✅ ¿Tengo clara la estructura de chunks necesaria?
    - **REGLA DE ORO**: "Si no está en ejemplos funcionales, no lo improvises"

  - **🔍 1.5.2 Validación Paso a Paso Durante Implementación**
    - **COMPILACIÓN INCREMENTAL**: Probar cada chunk antes de continuar
    - **VERIFICACIÓN DE SINTAXIS**: Comparar cada línea con ejemplos funcionales
    - **INTERPOLACIÓN SEGURA**: Verificar variables R en código TikZ/LaTeX
    - **NO AVANZAR** hasta que sección actual funcione correctamente

  - **⚠️ 1.5.3 Señales de Alerta Críticas**
    - **PARAR INMEDIATAMENTE** si:
      * Interpolando variables complejas en TikZ sin ejemplo
      * Mezclando sintaxis R y LaTeX sin patrón probado
      * Algo "parece que debería funcionar" sin verificación
      * Improvisando configuraciones no vistas en ejemplos
    - **ACCIÓN**: Volver a ejemplos funcionales y copiar patrón exacto

  - **📋 1.5.4 Protocolo de Auto-Verificación Final**
    - **ANTES DE ENTREGAR CUALQUIER .RMD**:
      * □ ¿Sintaxis TikZ idéntica a ejemplos probados?
      * □ ¿Variables R interpoladas correctamente?
      * □ ¿No hay chunks extra o caracteres sobrantes?
      * □ ¿Estructura completa verificada contra ejemplos?
      * □ ¿Compilación exitosa sin errores?

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
  - **CONFIGURACIÓN NUMÉRICA CRÍTICA**:
    * `Sys.setlocale(category = "LC_NUMERIC", locale = "C")`
    * `options(OutDec = ".")`
    * `options(scipen = 999)` - Eliminar notación científica
    * `options(digits = 10)` - Precisión numérica apropiada
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

### 🎲 **FASE 4: Generación de Datos Aleatorios Inteligente**
*Crear función de generación con mínimo 300 versiones únicas - ENFOQUE OPTIMIZADO*

- [ ] **🔢 4.1 Función generar_datos() Optimizada**
  - **CONFIGURACIÓN NUMÉRICA INICIAL**:
    * `options(OutDec = ".")` - Punto como separador decimal
    * `options(scipen = 999)` - Eliminar notación científica
    * `options(digits = 10)` - Precisión apropiada
  - **FUNCIONES DE FORMATO ESTÁNDAR**:
    * `formatear_entero()` - Para valores monetarios sin separador de miles
    * `formato_estandar()` - Para números con decimales controlados
  - **PRIORIZAR**: Diversidad matemáticamente relevante sobre aleatorización superficial
  - **EVITAR**: Exceso de contextos que no aportan valor educativo (máximo 3-5 opciones)
  - **ENFOCAR**: Parámetros que cambien la experiencia matemática del estudiante
  - **IMPLEMENTAR**: Diferentes tipos de problemas (ej: ganancia baja/media/alta)
  - **MANTENER**: Coherencia matemática y realismo en todos los casos
  - Retornar lista estructurada con parámetros educativamente relevantes

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
  - **🎯 Crear 4 opciones con distractores conceptuales optimizados:**
    - **PRIORIZAR**: Errores conceptuales reales que cometen estudiantes
    - **EVITAR**: Distractores artificiales o demasiado complejos
    - **IMPLEMENTAR**: Máximo 3-5 tipos de distractores bien fundamentados
    - **ENFOCAR**: Confusiones conceptuales específicas del tema matemático
    - **MANTENER**: Simplicidad en la generación sin sobre-ingeniería
    - Verificar que las 4 opciones sean textualmente únicas
    - Asegurar distractores pedagógicamente efectivos
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
  extype: schoice|cloze
  exsolution: [patrón_respuesta]
  exclozetype: [Para cloze: schoice|num|string separados por |]
  extol: [Para cloze: tolerancias separadas por |]
  exshuffle: TRUE
  exsection: [sección_temática]
  ```

  **CONFIGURACIÓN CRÍTICA PARA TIPO CLOZE**:
  - **Tolerancias numéricas**: ≥ 1 para valores monetarios grandes
  - **Tolerancias schoice**: 0 (exactitud requerida)
  - **Ejemplo**: `extol: 0|0|1|1|0|1|0` (schoice=0, numéricas=1)
  - **Documentar**: Comentarios explicativos sobre tolerancias

---

### ⚙️ **FASE 6.5: Configuración de Tolerancias para Evaluación Automática (NUEVA)**
*Configurar tolerancias apropiadas para respuestas tipo cloze y validar evaluación correcta*

- [ ] **🎯 6.5.1 Identificación de Tipos de Respuesta**
  - **Mapear estructura de respuestas**: schoice vs numéricas
  - **Documentar posiciones**: Crear comentarios explicativos
  - **Ejemplo**: `tipos_respuesta <- c("schoice", "schoice", "num", "num", "schoice", "num", "schoice")`

- [ ] **⚙️ 6.5.2 Configuración de Tolerancias Apropiadas**
  - **Para respuestas schoice**: Tolerancia 0 (exactitud requerida)
  - **Para respuestas numéricas**:
    * Valores monetarios grandes (>1000): tolerancia ≥ 1
    * Valores decimales pequeños (<10): tolerancia 0.01-0.1
    * Porcentajes: tolerancia 0.1-1 según precisión requerida
  - **Ejemplo**: `tolerancias <- c(0, 0, 1, 1, 0, 1, 0)`

- [ ] **📝 6.5.3 Documentación de Configuración**
  - **Comentarios explicativos**:
    ```r
    # Tolerancias para respuestas numéricas (solo aplica a las numéricas)
    # Estructura: schoice, schoice, num, num, schoice, num, schoice
    # Para respuestas numéricas monetarias: tolerancia 1 (permite diferencias mínimas de redondeo)
    # Para respuestas schoice: tolerancia 0 (exactitud requerida)
    tolerancias <- c(0, 0, 1, 1, 0, 1, 0)
    ```

- [ ] **✅ 6.5.4 Validación de Tolerancias**
  - **Test automático para verificar configuración**:
    ```r
    test_that("Validación de tolerancias configuradas correctamente", {
      expect_equal(length(tolerancias), length(tipos_respuesta))
      # Verificar que numéricas tienen tolerancia > 0
      posiciones_numericas <- which(tipos_respuesta == "num")
      expect_true(all(tolerancias[posiciones_numericas] > 0))
      # Verificar que schoice tienen tolerancia 0
      posiciones_schoice <- which(tipos_respuesta == "schoice")
      expect_true(all(tolerancias[posiciones_schoice] == 0))
    })
    ```

- [ ] **🔍 6.5.5 Prueba de Evaluación Automática**
  - **Simular respuestas correctas**: Verificar que se evalúan como correctas
  - **Probar variaciones dentro de tolerancia**: Confirmar aceptación
  - **Validar respuestas fuera de tolerancia**: Confirmar rechazo
  - **Documentar casos de prueba**: Para referencia futura

---

### 🔧 **FASE 7: Corrección de Errores Recurrentes y Validación Continua**
*Aplicar metodología avanzada de detección y corrección sistemática integrada durante implementación*

- [ ] **⚡ 7.0 VALIDACIÓN CONTINUA DURANTE IMPLEMENTACIÓN (NUEVO)**
  - **APLICAR DURANTE FASES 3-6**: No esperar hasta el final para corregir errores
  - **VERIFICACIÓN CHUNK POR CHUNK**:
    * Después de cada chunk: Compilar y verificar funcionamiento
    * Comparar sintaxis con ejemplos funcionales inmediatamente
    * Corregir errores de interpolación de variables al momento
    * Validar estructura LaTeX/TikZ antes de continuar
  - **PROTOCOLO DE PARADA**: Si algo no funciona, PARAR y consultar ejemplos funcionales
  - **PREVENCIÓN > CORRECCIÓN**: Evitar errores en lugar de corregirlos después

- [ ] **🔍 7.1 Detección Automática de Errores (Complementaria)**
  - **OBLIGATORIO**: Consultar `/Auxiliares/METODOLOGIA_Correccion_Errores_Recurrentes_ICFES_R_Exams.md`
  - Ejecutar detección de 5 categorías de errores:
    * **A) Gramaticales/Concordancia**: Verificar "El conteo" vs "La cantidad" (no "La conteo")
    * **B) Posicionamiento TikZ**: Confirmar orden texto → tabla → pregunta
    * **C) Generación de datos**: Validar opciones únicas, anti-duplicados
    * **D) Compilación LaTeX**: Verificar paquetes, caracteres especiales, interpolación variables
    * **E) Estructura R-exams**: Revisar YAML, include_tikz, variables, chunks extra
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
  - **🎯 Validar configuración de tolerancias**:
    - Confirmar que tolerancias están configuradas apropiadamente
    - Probar evaluación automática con respuestas correctas
    - Verificar que respuestas dentro de tolerancia se aceptan
    - Validar que respuestas fuera de tolerancia se rechazan
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

## 🤖 **AGENTE-GRAFICADOR ESPECIALIZADO TikZ**
*Sistema avanzado de replicación gráfica de alta fidelidad para contenido complejo*

### 📋 **ESPECIFICACIONES TÉCNICAS DEL AGENTE-GRAFICADOR**

#### **🎯 Función y Objetivo**
- **Función Exclusiva**: Replicación de alta fidelidad (98%+) de elementos gráficos y tabulares complejos
- **Activación**: Automática cuando se detecta contenido gráfico/tabular en FLUJO B
- **Objetivo**: Generar código TikZ avanzado que replique visualmente la imagen original
- **Integración**: Compatible con sistema R-exams y configuración YAML completa

#### **🔧 Tecnologías y Algoritmos Especializados**

##### **🎨 Extracción de Colores RGB Exactos**
```r
# Algoritmo de detección de colores dominantes
extraer_colores_imagen <- function(ruta_imagen) {
  # Implementar análisis de histograma de colores
  # Extraer paleta RGB principal
  # Convertir a códigos TikZ compatibles
  colores_rgb <- c("#FF5733", "#33FF57", "#3357FF")  # Ejemplo
  return(colores_rgb)
}
```

##### **📐 Sistema de Medición Proporcional Automática**
```r
# Cálculo de coordenadas y proporciones precisas
calcular_coordenadas_tikz <- function(elementos_detectados) {
  # Analizar posicionamiento relativo
  # Calcular escalas apropiadas
  # Generar sistema de coordenadas TikZ
  coordenadas <- list(x = c(0, 2, 4), y = c(0, 1.5, 3))
  return(coordenadas)
}
```

##### **🎯 Templates Especializados por Tipo de Gráfica**
- **Gráficas de Barras**: Template con barras parametrizables y etiquetas
- **Gráficas Circulares**: Template con sectores y leyendas automáticas
- **Gráficas de Líneas**: Template con puntos, líneas y ejes
- **Tablas Complejas**: Template con celdas, bordes y formato
- **Histogramas**: Template con bins y distribuciones
- **Diagramas de Dispersión**: Template con puntos y tendencias
- **Boxplots**: Template con cuartiles y valores atípicos

#### **⚙️ Configuración LaTeX Avanzada**
```yaml
# Paquetes LaTeX necesarios para Agente-Graficador
tikz_packages_avanzados:
  - tikz
  - pgfplots
  - xcolor
  - colortbl
  - amsmath
  - array
  - calc
  - positioning
  - decorations.markings
```

### 🔄 **PROTOCOLO DE REPLICACIÓN ITERATIVA**

#### **Fase 1: Análisis y Detección**
1. **Identificar tipo de gráfica** (barras, líneas, circular, tabla, etc.)
2. **Extraer elementos clave** (ejes, etiquetas, datos, colores)
3. **Seleccionar template apropiado** de biblioteca especializada
4. **Calcular parámetros iniciales** (coordenadas, escalas, colores)

#### **Fase 2: Generación Inicial**
1. **Aplicar template seleccionado** con parámetros calculados
2. **Generar código TikZ inicial** con variables R integradas
3. **Renderizar primera versión** usando include_tikz()
4. **Comparar con imagen original** visualmente

#### **Fase 3: Refinamiento Iterativo**
1. **Identificar discrepancias específicas**:
   - Colores no exactos
   - Proporciones incorrectas
   - Elementos faltantes o mal posicionados
   - Escalas inadecuadas
2. **Ajustar parámetros sistemáticamente**
3. **Re-renderizar y comparar**
4. **Repetir hasta alcanzar 98%+ fidelidad**

#### **Fase 4: Validación Final**
1. **Presentar comparación lado a lado** (original vs replicado)
2. **Aplicar checklist de fidelidad visual**
3. **Solicitar aprobación usuario**
4. **Documentar código final optimizado**

### 📊 **SISTEMA DE MÉTRICAS DE FIDELIDAD VISUAL**

#### **✅ Criterios Cuantificables (98%+ Requerido)**

##### **🎯 Precisión Geométrica (25%)**
- Proporciones entre elementos: ±2% tolerancia
- Ángulos y orientaciones: ±1° tolerancia
- Escalas relativas: ±3% tolerancia
- **Métrica**: Comparación de ratios dimensionales

##### **🌈 Fidelidad Cromática (25%)**
- Colores RGB exactos: ±5 unidades por canal
- Contrastes relativos: ±10% tolerancia
- Saturación y brillo: ±8% tolerancia
- **Métrica**: Distancia euclidiana en espacio RGB

##### **📍 Posicionamiento (25%)**
- Ubicación relativa de elementos: ±2% tolerancia
- Alineación de componentes: ±1% tolerancia
- Espaciado entre elementos: ±3% tolerancia
- **Métrica**: Comparación de coordenadas normalizadas

##### **✔️ Completitud (25%)**
- Todos los elementos principales presentes: 100%
- Etiquetas y texto replicados: 100%
- Estructura general mantenida: 100%
- **Métrica**: Checklist binario de elementos

#### **🔍 Checklist de Validación Visual**
- [ ] **Estructura General**: ¿Se mantiene la organización visual?
- [ ] **Elementos Principales**: ¿Están todos los componentes clave?
- [ ] **Colores**: ¿Son visualmente indistinguibles del original?
- [ ] **Proporciones**: ¿Las relaciones dimensionales son correctas?
- [ ] **Texto y Etiquetas**: ¿Son legibles y están bien posicionados?
- [ ] **Calidad General**: ¿Un observador casual notaría diferencias?

### 🎨 **BIBLIOTECA DE TEMPLATES ESPECIALIZADOS**

#### **📊 Template: Gráfica de Barras**
```latex
% Template parametrizable para gráficas de barras
\begin{tikzpicture}[scale=`r escala_grafica`]
  \begin{axis}[
    ybar,
    bar width=`r ancho_barra`pt,
    xlabel={`r etiqueta_x`},
    ylabel={`r etiqueta_y`},
    xticklabels={`r paste(etiquetas_x, collapse=",")`},
    ymin=0, ymax=`r max_y`,
    xtick=data,
    nodes near coords,
    every node near coord/.append style={font=\footnotesize}
  ]
  \addplot[fill=`r color_barras`] coordinates {
    `r paste(sprintf("(%d,%g)", seq_along(valores_y), valores_y), collapse=" ")`
  };
  \end{axis}
\end{tikzpicture}
```

#### **🥧 Template: Gráfica Circular**
```latex
% Template parametrizable para gráficas circulares
\begin{tikzpicture}[scale=`r escala_circular`]
  \pie[
    color={`r paste(colores_sectores, collapse=",")`},
    radius=`r radio_grafica`,
    text=legend
  ]{`r paste(sprintf("%g/%s", valores_porcentajes, etiquetas_sectores), collapse=",")`}
\end{tikzpicture}
```

#### **📈 Template: Tabla de Datos**
```latex
% Template parametrizable para tablas complejas
\begin{tikzpicture}
\node[inner sep=0pt] {
  \begin{tabular}{|`r paste(rep("c", ncol_tabla), collapse="|")`|}
    \hline
    `r paste(sprintf("\\textbf{%s}", nombres_columnas), collapse=" & ")` \\
    \hline
    `r paste(apply(datos_tabla, 1, function(x) paste(x, collapse=" & ")), collapse=" \\\\\n    \\hline\n    ")` \\
    \hline
  \end{tabular}
};
\end{tikzpicture}
```

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

### ✅ **Aleatorización Inteligente:**
- **Estándar del proyecto**: Mínimo 300 versiones únicas verificadas
- **Diversidad matemáticamente relevante**: Enfocar en aspectos que cambien la experiencia matemática
- **Evitar aleatorización superficial**: No diversificar elementos cosméticos sin valor educativo
- **Distractores conceptuales**: Representar errores reales que cometen estudiantes
- **Simplicidad técnica**: Mantener elegancia sin sobre-ingeniería

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

### 🤖 **Para Sistema Condicional Automático:**
```
# Activar análisis automático de imagen PNG
"Aplica el sistema condicional automático a esta imagen PNG para detectar contenido gráfico y activar el flujo apropiado"

# Activar Agente-Graficador Especializado (solo si se detecta contenido gráfico)
"Activa el Agente-Graficador Especializado TikZ para replicar esta imagen con 98%+ fidelidad visual"

# Validar fidelidad visual
"Ejecuta la validación de fidelidad visual comparando el TikZ generado con la imagen original"
```

### 🎯 **Para Flujos Específicos:**
```
# FLUJO A (sin gráficas detectadas)
"Ejecuta FLUJO A estándar para imagen sin contenido gráfico complejo"

# FLUJO B (con gráficas detectadas)
"Ejecuta FLUJO B con Agente-Graficador para imagen con contenido gráfico/tabular"
```

### 🎨 **Para Metodología TikZ Tradicional:**
```
# Comando original (ahora integrado en sistema condicional)
"Aplica la metodología TikZ avanzada a esta nueva imagen PNG para generar un ejercicio R-exams completo con salidas exams2*"
```

### 📋 **Para Gestión de Tareas:**
```
# Investigar información ICFES
brave_web_search_brave-search: "término específico ICFES matemáticas 2025"
web-fetch: [URL oficial ICFES]

# Crear tareas nuevas
add_tasks con esta estructura como base

# Actualizar progreso
update_tasks con task_id y nuevo state

# Compilar y probar
rmarkdown::render('archivo.Rmd', 'html_document')
```

### ⚙️ **Para Configuración de Tolerancias:**
```
# Verificar configuración de tolerancias
"Revisa y corrige la configuración de tolerancias para evaluación automática en ejercicios tipo cloze"

# Aplicar corrección estándar
"Aplica tolerancia 0 para schoice y tolerancia ≥ 1 para respuestas numéricas con valores grandes"

# Validar evaluación automática
"Valida que las respuestas idénticas a la solución se evalúen correctamente como correctas"

# Documentar configuración
"Agrega comentarios explicativos sobre la configuración de tolerancias y su justificación"
```

---

## 🤖 **DOCUMENTACIÓN DEL SISTEMA CONDICIONAL AUTOMÁTICO**

### 📊 **Diagrama de Flujo de Decisión**

```
📷 IMAGEN PNG PROPORCIONADA
           ↓
🤖 ANÁLISIS AUTOMÁTICO DE CONTENIDO
           ↓
    ¿Contiene gráficas/tablas?
           ↓
    ┌─────────────────┐
    ↓                 ↓
🔄 FLUJO A          🎯 FLUJO B
(Sin gráficas)      (Con gráficas)
    ↓                 ↓
📋 Proceso          🤖 Agente-Graficador
   Estándar            Especializado
   (8 Fases)              ↓
    ↓               🔄 Proceso Iterativo
✅ Ejercicio           (hasta 98% fidelidad)
   Completo              ↓
                   ✅ Validación Usuario
                         ↓
                   📋 Continuar 8 Fases
                         ↓
                   ✅ Ejercicio Completo
```

### 🔍 **Criterios de Detección Automática**

#### **✅ Contenido que ACTIVA FLUJO B (Agente-Graficador)**
- **Gráficas Estadísticas**:
  * Gráficas de barras (verticales/horizontales)
  * Gráficas circulares (pie charts)
  * Histogramas con bins definidos
  * Gráficas de líneas con puntos de datos
  * Diagramas de dispersión (scatter plots)
  * Boxplots con cuartiles visibles

- **Tablas de Datos**:
  * Tablas con filas y columnas estructuradas
  * Matrices numéricas organizadas
  * Tablas de frecuencia con encabezados
  * Tablas mixtas (texto + números)

- **Diagramas Matemáticos**:
  * Diagramas de Venn con conjuntos
  * Árboles de probabilidad
  * Figuras geométricas con medidas
  * Planos cartesianos con funciones

- **Elementos Híbridos**:
  * Combinación gráfica + tabla
  * Diagrama + datos numéricos
  * Múltiples elementos gráficos

#### **⭕ Contenido que MANTIENE FLUJO A (Proceso Estándar)**
- **Texto Simple**: Solo enunciados matemáticos
- **Ecuaciones Básicas**: Fórmulas sin representación gráfica
- **Figuras Geométricas Simples**: Formas básicas sin datos complejos
- **Problemas Verbales**: Contextos sin elementos visuales complejos

### 🎯 **Especificaciones del Agente-Graficador por Tipo**

#### **📊 Gráficas de Barras**
- **Template**: `grafica-barras-avanzada.tikz`
- **Parámetros Detectados**: Altura barras, colores, etiquetas ejes
- **Fidelidad Objetivo**: 98%+ en proporciones y colores
- **Tiempo Estimado**: 15-25 minutos proceso completo

#### **🥧 Gráficas Circulares**
- **Template**: `grafica-circular-avanzada.tikz`
- **Parámetros Detectados**: Ángulos sectores, colores, leyendas
- **Fidelidad Objetivo**: 98%+ en proporciones y distribución
- **Tiempo Estimado**: 20-30 minutos proceso completo

#### **📈 Tablas de Datos**
- **Template**: `tabla-datos-avanzada.tikz`
- **Parámetros Detectados**: Estructura, contenido, formato
- **Fidelidad Objetivo**: 98%+ en organización y presentación
- **Tiempo Estimado**: 10-20 minutos proceso completo

### 🔄 **Protocolo de Validación Usuario-Sistema**

#### **Fase 1: Presentación Comparativa**
1. **Mostrar lado a lado**: Imagen original vs TikZ generado
2. **Destacar elementos clave** replicados
3. **Señalar diferencias** si las hay
4. **Calcular métricas** de fidelidad automáticas

#### **Fase 2: Checklist Interactivo**
- [ ] **¿Los colores son visualmente idénticos?**
- [ ] **¿Las proporciones se mantienen correctas?**
- [ ] **¿Todos los elementos están presentes?**
- [ ] **¿El posicionamiento es preciso?**
- [ ] **¿La calidad general es aceptable?**

#### **Fase 3: Decisión y Continuación**
- **Si ≥98% fidelidad**: Aprobar y continuar con FASES 2-8
- **Si <98% fidelidad**: Repetir proceso iterativo
- **Si problemas técnicos**: Consultar ejemplos funcionales

### 📋 **Ejemplos de Uso por Tipo de Imagen**

#### **Ejemplo 1: Imagen con Gráfica de Barras**
```
INPUT: imagen_barras_ventas.png
DETECCIÓN: Gráfica de barras verticales detectada
FLUJO: B (Agente-Graficador)
TEMPLATE: grafica-barras-avanzada.tikz
PROCESO: Extracción colores → Cálculo proporciones → Generación TikZ
VALIDACIÓN: 98.5% fidelidad alcanzada
RESULTADO: Ejercicio R-exams con gráfica TikZ replicada
```

#### **Ejemplo 2: Imagen con Tabla de Datos**
```
INPUT: tabla_estadisticas.png
DETECCIÓN: Tabla 4x5 con encabezados detectada
FLUJO: B (Agente-Graficador)
TEMPLATE: tabla-datos-avanzada.tikz
PROCESO: Extracción estructura → Formateo → Generación TikZ
VALIDACIÓN: 99.1% fidelidad alcanzada
RESULTADO: Ejercicio R-exams con tabla TikZ replicada
```

#### **Ejemplo 3: Imagen Solo Texto**
```
INPUT: problema_verbal.png
DETECCIÓN: Solo texto matemático, sin gráficas
FLUJO: A (Proceso Estándar)
PROCESO: FASES 1-8 estándar con TikZ básico
RESULTADO: Ejercicio R-exams tradicional
```

---

## 📌 **NOTAS IMPORTANTES**

### 🤖 **Sistema Condicional Automático:**
- **NUEVO**: El sistema detecta automáticamente contenido gráfico y activa flujos especializados
- **FLUJO A**: Para imágenes sin gráficas complejas (proceso estándar 8 fases)
- **FLUJO B**: Para imágenes con gráficas/tablas (Agente-Graficador + 8 fases)
- **VALIDACIÓN OBLIGATORIA**: 98%+ fidelidad visual antes de continuar con ejercicio completo
- **COMPATIBILIDAD TOTAL**: Mantiene todas las funcionalidades existentes del TEMPLATE

### 🎯 **Agente-Graficador Especializado:**
- **ACTIVACIÓN**: Automática cuando se detecta contenido gráfico/tabular
- **OBJETIVO**: Replicación de alta fidelidad (98%+) usando TikZ avanzado
- **PROCESO**: Iterativo hasta alcanzar criterios de calidad visual
- **INTEGRACIÓN**: Compatible con sistema R-exams y aleatorización 300+ versiones
- **TEMPLATES**: Biblioteca especializada por tipo de gráfica (barras, circular, tabla, etc.)

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

### ⚙️ **Configuración de Tolerancias (CRÍTICO):**
- **PROBLEMA COMÚN**: Tolerancias en 0 para respuestas numéricas causan evaluación incorrecta
- **SOLUCIÓN ESTÁNDAR**:
  * Tolerancia 0 para respuestas schoice (exactitud requerida)
  * Tolerancia ≥ 1 para respuestas numéricas con valores grandes (monetarios)
  * Tolerancia 0.01-0.1 para respuestas numéricas con valores pequeños
- **DOCUMENTAR SIEMPRE**: Comentarios explicativos sobre configuración de tolerancias
- **VALIDAR**: Tests automáticos para verificar evaluación correcta

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

---

## 📋 **ANEXO: PROTOCOLO ANTI-ERRORES DE IMPLEMENTACIÓN**

### 🎯 **PROTOCOLO COMPLETO DE PREVENCIÓN DE ERRORES**

#### **📚 FASE 0: CONSULTA OBLIGATORIA PRE-IMPLEMENTACIÓN**
```
ANTES DE ESCRIBIR UNA SOLA LÍNEA DE CÓDIGO:

✅ PASO 1: Abrir `/Auxiliares/Ejemplos-Funcionales-Rmd/`
✅ PASO 2: Identificar ejemplo más similar al ejercicio objetivo
✅ PASO 3: Estudiar estructura completa del ejemplo
✅ PASO 4: Copiar configuración YAML exacta
✅ PASO 5: Copiar estructura de chunks exacta
✅ PASO 6: Identificar patrones de interpolación de variables
✅ PASO 7: Entender configuración TikZ/LaTeX específica

REGLA ABSOLUTA: "No improvises. Copia patrones probados."
```

#### **⚡ VALIDACIÓN CONTINUA DURANTE IMPLEMENTACIÓN**
```
DESPUÉS DE CADA CHUNK:

□ ¿Compiló sin errores?
□ ¿La sintaxis es idéntica al ejemplo funcional?
□ ¿Las variables R se interpolan correctamente?
□ ¿No hay caracteres extra o faltantes?

SI ALGUNA RESPUESTA ES "NO": PARAR y consultar ejemplos funcionales
```

#### **🚨 SEÑALES DE ALERTA CRÍTICAS**
```
PARAR INMEDIATAMENTE SI:

🔴 Estás interpolando variables complejas en TikZ sin ejemplo
🔴 Estás mezclando sintaxis R y LaTeX sin patrón probado
🔴 Algo "parece que debería funcionar" sin verificación
🔴 Estás improvisando configuraciones no vistas en ejemplos
🔴 Aparecen errores de compilación inesperados

ACCIÓN: Volver a ejemplos funcionales y copiar patrón exacto
```

#### **📋 CHECKLIST FINAL OBLIGATORIO**
```
ANTES DE ENTREGAR CUALQUIER .RMD:

□ ¿Consulté TODOS los ejemplos funcionales relevantes?
□ ¿La sintaxis TikZ es idéntica a ejemplos probados?
□ ¿Las variables R se interpolan correctamente?
□ ¿No hay chunks extra o caracteres sobrantes?
□ ¿La estructura completa sigue patrones probados?
□ ¿Compilación exitosa sin errores?
□ ¿Aplicé metodología de corrección de errores?
□ ¿Verifiqué funcionamiento en múltiples formatos?

SOLO ENTREGAR SI TODAS LAS RESPUESTAS SON "SÍ"
```

#### **🎯 ERRORES MÁS COMUNES A EVITAR**
1. **Interpolación incorrecta**: `\\draw[', variable, ',thick]` → `\\draw[cyan,thick]`
2. **Chunks extra**: Verificar que no sobren ``` al final
3. **Sintaxis mixta**: No mezclar R y LaTeX sin patrón probado
4. **Configuraciones inventadas**: Solo usar configuraciones de ejemplos funcionales
5. **Variables no definidas**: Verificar que todas las variables existan

#### **🔧 PROTOCOLO DE RECUPERACIÓN DE ERRORES**
```
SI ENCUENTRAS ERRORES:

1. NO intentes "arreglar rápido"
2. PARA la implementación
3. CONSULTA ejemplos funcionales
4. IDENTIFICA el patrón correcto
5. COPIA la sintaxis exacta
6. PRUEBA compilación
7. CONTINÚA solo si funciona

"Mejor perder 5 minutos consultando que 30 minutos debuggeando"
```

### 🎯 **IMPLEMENTACIÓN INMEDIATA DEL PROTOCOLO**

**Este protocolo debe aplicarse OBLIGATORIAMENTE en todas las fases del template, especialmente:**
- **FASE 1.4-1.5**: Consulta y validación pre-implementación
- **FASES 3-6**: Validación continua durante implementación
- **FASE 7**: Corrección sistemática final

**El objetivo es PREVENIR errores, no corregirlos después.**
