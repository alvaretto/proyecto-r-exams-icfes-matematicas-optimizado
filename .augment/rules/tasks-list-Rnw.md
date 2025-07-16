---
type: "agent_requested"
description: "tasks-list para la generación estructurada de archivos .Rnw"
---
# PLAN MAESTRO: Generacion/Correccion de Ejercicios ICFES R-exams (.Rnw)

## ESTRUCTURA COMPLETA DE TAREAS

### **FASE 1: Analisis Automatico de Imagen y Sistema Condicional TikZ**
*Sistema inteligente de deteccion automatica de contenido grafico con flujos especializados*

- [ ] **1.1 Preparacion del Archivo**
  - Colocar nueva imagen en directorio de trabajo bajo `Lab/` (cualquier subdirectorio)
  - **Ejemplo**: `/Lab/Prueba-Temporal_TikZ/nueva_imagen.png` o `/Lab/[proyecto]/imagen.png`
  - Verificar formato PNG y calidad de imagen
  - Crear directorio de trabajo especifico si es necesario

- [ ] **1.2 ANALISIS AUTOMATICO DE CONTENIDO GRAFICO**
  - **1.2.1 Deteccion Automatica de Elementos Visuales**
    - **PROCESO AUTOMATICO**: Analizar imagen PNG para detectar:
      * **Graficas**: Barras, lineas, circulares, histogramas, dispersion, boxplots
      * **Tablas**: Numericas, textuales, mixtas, con/sin encabezados
      * **Diagramas**: Matematicos, estadisticos, geometricos, probabilidad
      * **Elementos Hibridos**: Grafica + tabla en misma imagen
      * **Contenido Simple**: Solo texto, formas basicas, elementos no graficos
    - **OUTPUT**: Clasificacion binaria con justificacion del analisis

  - **1.2.2 DECISION DE FLUJO CONDICIONAL**
    - **FLUJO A** (Sin graficas/tablas detectadas):
      * Ejecutar secuencia estandar FASES 1-8
      * TikZ solo para elementos simples (texto, formas basicas)
      * Continuar con metodologia TikZ basica
    - **FLUJO B** (Con graficas/tablas detectadas):
      * **ACTIVAR**: Agente-Graficador Especializado TikZ
      * **OBJETIVO**: Replicacion de alta fidelidad (98%+)
      * **PROCESO**: Iterativo hasta validacion usuario-sistema

- [ ] **1.3 AGENTE-GRAFICADOR ESPECIALIZADO (Solo FLUJO B)**
  - **1.3.1 Activacion del Agente-Graficador**
    - **FUNCION EXCLUSIVA**: Replicacion de alta fidelidad de elementos graficos/tabulares
    - **TECNOLOGIA**: Codigo TikZ avanzado con caracteristicas especializadas:
      * Colores RGB exactos extraidos de imagen original
      * Posicionamiento preciso mediante coordenadas calculadas
      * Estilos reutilizables y escalables
      * Configuracion completa de paquetes LaTeX necesarios
    - **BASE METODOLOGICA**: Expandir "Metodologia TikZ Avanzada" existente

  - **1.3.2 Proceso Iterativo de Replicacion**
    - **OBJETIVO**: Alcanzar 98%+ fidelidad visual antes de continuar
    - **PROCESO**:
      1. Generar codigo TikZ inicial basado en tipo detectado
      2. Renderizar y comparar con imagen original
      3. Identificar discrepancias especificas
      4. Ajustar parametros (colores, coordenadas, proporciones)
      5. Repetir hasta alcanzar criterios de fidelidad
    - **METRICAS DE FIDELIDAD**:
      * Precision Geometrica (25%): Proporciones, angulos, escalas
      * Fidelidad Cromatica (25%): Colores RGB exactos, contrastes
      * Posicionamiento (25%): Ubicacion relativa de elementos
      * Completitud (25%): Todos los elementos presentes

  - **1.3.3 Validacion Usuario-Sistema de Fidelidad**
    - **COMPARACION VISUAL**: Lado a lado (PNG original vs TikZ renderizado)
    - **CHECKLIST FIDELIDAD**: Elementos criticos verificados
    - **METRICAS CUANTIFICABLES**: Coordenadas, colores, dimensiones
    - **APROBACION REQUERIDA**: Usuario confirma 98%+ fidelidad antes de continuar
    - **CRITERIOS DE ACEPTACION**: Todos los elementos principales replicados correctamente

  - **1.3.4 Integracion R-exams Validada**
    - **CODIGO TIKZ DOCUMENTADO**: Listo para insercion en chunks especificos
    - **COMPATIBILIDAD VERIFICADA**: include_tikz() y configuracion LaTeX completa
    - **VARIABLES R INTEGRADAS**: Parametrizacion para aleatorizacion
    - **MULTI-FORMATO**: Funciona en exams2html, exams2pdf, exams2moodle, exams2pandoc, exams2nops (schoice)

- [ ] **1.4 Consultar Ejemplos Funcionales (Ambos Flujos) - PROTOCOLO ESTRICTO**
  - **OBLIGATORIO ABSOLUTO**: Revisar `/Auxiliares/Ejemplos-Funcionales-Rmd/Rnw/` ANTES de escribir cualquier codigo
  - **FLUJO A**: Identificar configuraciones tecnicas basicas para TikZ simple
  - **FLUJO B**: Consultar templates avanzados para graficas complejas
  - **COPIAR PATRONES EXACTOS**: No improvisar, usar sintaxis identica a ejemplos probados
  - **VERIFICAR ESTRUCTURAS**: Chunks, configuraciones LaTeX, interpolacion de variables
  - **Alternativo**: Solo si TikZ no es viable, consultar `Auxiliares/Python-Documentation/Python-ICFES-Guide.md`

- [ ] **1.5 PROTOCOLO ANTI-ERRORES DE IMPLEMENTACION**
  - **1.5.1 Auto-Verificacion Pre-Implementacion**
    - **CHECKLIST OBLIGATORIO ANTES DE ESCRIBIR CODIGO**:
      * Consulte TODOS los ejemplos funcionales relevantes?
      * Identifique el patron exacto a seguir?
      * Entiendo la sintaxis TikZ/LaTeX del ejemplo?
      * Se como interpolar variables R correctamente?
      * Tengo clara la estructura de chunks necesaria?
    - **REGLA DE ORO**: "Si no esta en ejemplos funcionales, no lo improvises"

  - **1.5.2 Validacion Paso a Paso Durante Implementacion**
    - **COMPILACION INCREMENTAL**: Probar cada chunk antes de continuar
    - **VERIFICACION DE SINTAXIS**: Comparar cada linea con ejemplos funcionales
    - **INTERPOLACION SEGURA**: Verificar variables R en codigo TikZ/LaTeX
    - **NO AVANZAR** hasta que seccion actual funcione correctamente

  - **1.5.3 Senales de Alerta Criticas**
    - **PARAR INMEDIATAMENTE** si:
      * Interpolando variables complejas en TikZ sin ejemplo
      * Mezclando sintaxis R y LaTeX sin patron probado
      * Algo "parece que deberia funcionar" sin verificacion
      * Improvisando configuraciones no vistas en ejemplos
    - **ACCION**: Volver a ejemplos funcionales y copiar patron exacto

  - **1.5.4 Protocolo de Auto-Verificacion Final**
    - **ANTES DE ENTREGAR CUALQUIER .RNW**:
      * Sintaxis TikZ identica a ejemplos probados?
      * Variables R interpoladas correctamente?
      * No hay chunks extra o caracteres sobrantes?
      * Estructura completa verificada contra ejemplos?
      * Compilacion exitosa sin errores?

### **FASE 2: Planificacion ICFES y Concepto Matematico**
*Definir estructura del ejercicio ICFES basado en analisis visual*

- [ ] **2.1 Identificar Competencia ICFES**
  - Determinar competencia: `interpretacion_representacion` | `formulacion_ejecucion` | `argumentacion`
  - Establecer nivel de dificultad: 1, 2, 3, o 4
  - Definir componente: `geometrico_metrico` | `numerico_variacional` | `aleatorio`
  - Seleccionar contexto: `familiar` | `laboral` | `comunitario` | `matematico`
  - **Investigar en web**: Buscar informacion oficial ICFES actualizada sobre competencias matematicas

- [ ] **2.2 Definir Concepto Matematico**
  - Establecer concepto principal basado en analisis visual: algebra/geometria/estadistica
  - Determinar tipo de problema especifico
  - Verificar alineacion con competencia seleccionada
  - **Investigar en web**: Consultar documentacion oficial sobre contenidos matematicos ICFES actualizados

- [ ] **2.3 Investigacion Web Complementaria (si es necesario)**
  - Buscar informacion oficial ICFES sobre competencia especifica seleccionada
  - Consultar ejemplos oficiales de preguntas tipo
  - Verificar contextos y niveles de dificultad actualizados
  - Validar definiciones y criterios de evaluacion oficiales
  - Contrastar con documentacion MEN sobre estandares matematicos

---

### **FASE 3: Configuracion Tecnica Base**
*Implementar estructura tecnica siguiendo ejemplos funcionales con TikZ integrado*

- [ ] **3.1 Estructura LaTeX Base para .Rnw**
  ```latex
  \documentclass[10pt,a4paper]{article}

  %% paquetes basicos
  \usepackage[utf8]{inputenc}
  \usepackage[spanish]{babel}
  \usepackage{a4wide,color,verbatim,Sweave,url,xargs,amsmath,booktabs,longtable}
  \usepackage{graphicx,float}
  \usepackage{tikz,xcolor}
  \usepackage{enumitem}

  %% bibliotecas TikZ segun necesidad
  \usetikzlibrary{automata,positioning,calc,arrows}

  %% entornos para exams
  \newenvironment{question}{\item}{}
  \newenvironment{solution}{\comment}{\endcomment}
  \newenvironment{answerlist}{\renewcommand{\labelenumii}{(\alph{enumii})}\begin{enumerate}}{\end{enumerate}}

  %% comandos para metadatos exams
  \newcommand{\exname}[1]{\def\@exname{#1}}
  \newcommand{\extype}[1]{\def\@extype{#1}}
  \newcommand{\exsolution}[1]{\def\@exsolution{#1}}
  \newcommand{\exshuffle}[1]{\def\@exshuffle{#1}}
  \newcommand{\exsection}[1]{\def\@exsection{#1}}

  %% configuracion parrafos
  \setlength{\parskip}{0.7ex plus0.1ex minus0.1ex}
  \setlength{\parindent}{0em}

  \begin{document}
  \SweaveOpts{concordance=TRUE}

  \begin{enumerate}
  ```

- [ ] **3.2 Chunk Setup Inicial con Sintaxis .Rnw**
  ```latex
  <<echo=FALSE, results=hide>>=
  # Configuracion inicial
  Sys.setlocale("LC_ALL", "C")
  options(OutDec = ".")

  # Librerias esenciales
  library(exams)
  library(digest)
  library(testthat)
  library(knitr)

  # Configuracion TikZ si es necesario
  typ <- match_exams_device()
  if(match_exams_call() == "exams2nops") typ <- "tex"

  # Semilla aleatoria
  set.seed(sample(1:100000, 1))
  @
  ```

- [ ] **3.3 Configuracion TikZ Prioritaria**
  - **OBLIGATORIO**: Consultar `Auxiliares/TikZ-Documentation/referencias/compatibilidad.md` para configuracion validada
  - Aplicar configuracion LaTeX: `options(tikzLatex = "pdflatex")`
  - Configurar bibliotecas TikZ validadas: basicas, calc, positioning, arrows
  - **Usar templates**: `Auxiliares/TikZ-Documentation/templates-rexams/` para diagramas validados
  - Verificar compatibilidad multi-formato segun checklist de compatibilidad

- [ ] **3.4 Configuracion Python-R (Solo si TikZ no es viable)**
  - **Consultar**: `Auxiliares/Python-Documentation/referencias/compatibilidad-python.md` para setup validado
  - Configurar `use_python("/usr/bin/python3", required = TRUE)` o `use_python(Sys.which("python"), required = TRUE)`
  - Establecer `knitr::knit_engines$set(python = ...)` segun patron exitoso
  - Verificar configuracion matplotlib: `matplotlib.rcParams['font.size'] = 9`
  - **Usar templates**: `Auxiliares/Python-Documentation/templates-rexams/` para graficos validados
  - Validar transferencia R->Python: `variable_python = r.variable_r`

---

### **FASE 4: Generacion de Datos Aleatorios**
*Crear funcion de generacion con minimo 300 versiones unicas*

- [ ] **4.1 Funcion generar_datos()**
  - Implementar aleatorizacion de contextos (minimo 8-10 escenarios)
  - Generar parametros numericos variables con rangos realistas
  - Incluir aleatorizacion de nombres, colores, unidades
  - Asegurar coherencia matematica en todos los casos
  - Retornar lista estructurada con todos los parametros

- [ ] **4.2 Prueba de Diversidad con Sintaxis .Rnw**
  ```latex
  <<echo=FALSE, results=hide>>=
  # Prueba de diversidad de versiones
  test_that("Prueba de diversidad de versiones", {
    versiones <- list()
    for(i in 1:1000) {
      datos_test <- generar_datos()
      versiones[[i]] <- digest::digest(datos_test)
    }

    n_versiones_unicas <- length(unique(versiones))
    expect_true(n_versiones_unicas >= 300,
                info = paste("Solo se generaron", n_versiones_unicas,
                            "versiones unicas. Se requieren al menos 300."))
  })
  @
  ```

- [ ] **4.3 Validaciones Matematicas**
  - Validar rangos de valores realistas
  - Verificar coherencia entre parametros relacionados
  - Implementar manejo de casos extremos
  - Asegurar que no hay divisiones por cero o valores invalidos

---

### [GRAFICO] **FASE 5: Visualizaciones y Graficos**
*PRIORIZAR TikZ para cualquier grafica, usar Python solo como alternativa*

- [ ] **[DISENO] 5.1 Diagramas TikZ (PRIORIDAD MAXIMA)**
  - **[DISENO] OBLIGATORIO**: Consultar `Auxiliares/TikZ-Documentation/TikZ-ICFES-Guide.md` para patrones exitosos validados
  - **[DISENO] Usar templates**: `Auxiliares/TikZ-Documentation/templates-rexams/icfes-aligned/` para diagramas especificos
  - **Patrones exitosos validados**:
    - **Tablas**: `tabla-datos-template.tikz` (patron mas compatible)
    - **Venn**: `diagrama-venn-template.tikz` (basado en DVenn_All_GenMus_01.Rnw)
    - **Geometria**: Templates parametrizables con variables R
    - **Funciones**: Graficas matematicas con coordenadas precisas
    - **Estadistica**: Histogramas, barras, circulares con TikZ
  - Usar `include_tikz()` con packages validados: `c("tikz", "colortbl", "xcolor")`
  - Configurar width apropiado segun template: "6cm" para tablas, "5cm" para Venn
  - Establecer `markup = "latex"` segun patron exitoso para .Rnw
  - **Aplicar fidelidad 98%** con imagen original usando coordenadas exactas

- [ ] **[PYTHON] 5.2 Graficos Python/matplotlib (Solo si TikZ no es viable)**
  - Usar `py_run_string()` con sintaxis corregida
  - Configurar `matplotlib.rcParams` apropiadamente
  - Implementar `plt.plot()` con sintaxis verificada en ejemplos
  - Guardar con `plt.savefig()` en alta resolucion

- [ ] **[DATOS] 5.3 Graficos Python-matplotlib (Alternativa secundaria)**
  - **[PYTHON] Consultar**: `Auxiliares/Python-Documentation/Python-ICFES-Guide.md` para patrones exitosos validados
  - **[PYTHON] Usar templates**: `Auxiliares/Python-Documentation/templates-rexams/icfes-aligned/` para graficos especificos
  - **Patrones exitosos validados**:
    - **Barras**: `grafico-barras-template.py` (basado en I_1796473-Opc-A2v2.Rnw)
    - **Circulares**: `grafico-circular-template.py` (basado en I_1796473-Opc-A2.Rnw)
    - **Funciones**: `funciones-lineales-template.py` (basado en vuelo_acrobatico_A.Rnw)
  - Usar transferencia R->Python validada: `variable_python = r.variable_r`
  - Configurar chunks: `echo=FALSE, message=FALSE, results="hide"`
  - **OBLIGATORIO**: Incluir `plt.show()` al final de cada chunk Python

- [ ] **[DATOS] 5.4 Graficos ggplot2 (Ultima alternativa)**
  - Implementar con `theme_minimal()` solo si TikZ y Python no son viables
  - Usar colores aleatorios para diversidad
  - Configurar DPI 150+ para calidad
  - Incluir etiquetas claras y leyendas

---

### [ESCRIBIR] **FASE 6: Contenido del Ejercicio**
*Desarrollar Question, Solution y Meta-information*

- [ ] **[PREGUNTA] 6.1 Seccion Question**
  - Redactar contexto realista y relevante
  - Formular pregunta clara segun competencia ICFES
  - **[META] Crear 4 opciones con sistema avanzado de distractores:**
    - Generar 8+ tipos diferentes de distractores (confusion conceptual, errores de calculo, posiciones incorrectas, etc.)
    - **30% probabilidad**: Incluir valores duplicados con justificaciones diferentes (ej: "mediana es 30 porque promedio centrales" vs "mediana es 30 porque suma/division")
    - **70% probabilidad**: Mantener todos los valores diferentes (modo tradicional)
    - Seleccion estrategica: 1 distractor con valor duplicado + 2 con valores diferentes
    - Verificar que las 4 opciones sean textualmente unicas
    - Asegurar distractores plausibles y educativos
  - Incluir graficos/tablas si es necesario

- [ ] **[IDEA] 6.2 Seccion Solution**
  - Proporcionar explicacion detallada del proceso
  - Incluir justificacion matematica completa
  - Crear Answerlist con Verdadero/Falso para cada opcion
  - Explicar por que cada distractor es incorrecto

- [ ] **[LISTA] 6.3 Meta-information con Sintaxis .Rnw**
  ```latex
  %% META-INFORMATION (al final del documento, antes de \end{enumerate})
  \exname{[nombre_descriptivo]}
  \extype{schoice}
  \exsolution{\Sexpr{mchoice2string(solutions)}}
  \exshuffle{TRUE}
  \exsection{[seccion_tematica]}

  \end{enumerate}
  \end{document}
  ```

---

### [HERRAM] **FASE 7: Correccion de Errores Recurrentes y Validacion Continua**
*Aplicar metodologia avanzada de deteccion y correccion sistematica integrada durante implementacion*

- [ ] **[RAPIDO] 7.0 VALIDACION CONTINUA DURANTE IMPLEMENTACION (NUEVO)**
  - **APLICAR DURANTE FASES 3-6**: No esperar hasta el final para corregir errores
  - **VERIFICACION CHUNK POR CHUNK**:
    * Despues de cada chunk: Compilar y verificar funcionamiento
    * Comparar sintaxis con ejemplos funcionales inmediatamente
    * Corregir errores de interpolacion de variables al momento
    * Validar estructura LaTeX/TikZ antes de continuar
  - **PROTOCOLO DE PARADA**: Si algo no funciona, PARAR y consultar ejemplos funcionales
  - **PREVENCION > CORRECCION**: Evitar errores en lugar de corregirlos despues

- [ ] **[LUPA] 7.1 Deteccion Automatica de Errores (Complementaria)**
  - **OBLIGATORIO**: Consultar `/Auxiliares/METODOLOGIA_Correccion_Errores_Recurrentes_ICFES_R_Exams.md`
  - Ejecutar deteccion de 5 categorias de errores:
    * **A) Gramaticales/Concordancia**: Verificar "El conteo" vs "La cantidad" (no "La conteo")
    * **B) Posicionamiento TikZ**: Confirmar orden texto -> tabla -> pregunta
    * **C) Generacion de datos**: Validar opciones unicas, anti-duplicados
    * **D) Compilacion LaTeX**: Verificar paquetes, caracteres especiales, interpolacion variables
    * **E) Estructura R-exams**: Revisar estructura LaTeX, include_tikz, variables, chunks extra
  - Aplicar funcion `detectar_errores_comunes(archivo_rnw)`

- [ ] **[LIBROS] 7.2 Aplicar Soluciones Probadas**
  - **OBLIGATORIO**: Consultar `/Auxiliares/BIBLIOTECA_Soluciones_Errores_Comunes.md`
  - **OBLIGATORIO**: Re-consultar ejemplos funcionales en `/Auxiliares/Ejemplos-Funcionales-Rmd/Rnw/`
  - Implementar correcciones sistematicas por categoria:
    * **A1**: Sistema automatico de concordancia de genero
    * **B2**: Reordenar elementos TikZ (texto primero, tabla despues)
    * **C1**: Implementar generacion de opciones unicas robusta
    * **D1**: Configurar paquetes LaTeX completos
    * **E2**: Configurar include_tikz con parametros completos
  - Aplicar soluciones validadas sin introducir nuevos errores

- [ ] **[OK] 7.3 Checklist de Validacion Sistematica**
  - **OBLIGATORIO**: Ejecutar `/Auxiliares/CHECKLIST_Validacion_Archivos_Rmd.md`
  - **Pre-compilacion**:
    * [ ] Verificar concordancia de genero en variables dinamicas
    * [ ] Confirmar orden correcto en elementos TikZ
    * [ ] Validar unicidad en opciones de respuesta
    * [ ] Revisar configuracion completa de paquetes
  - **Post-compilacion**:
    * [ ] Verificar output visual (tabla despues de texto)
    * [ ] Confirmar que todas las opciones son diferentes
    * [ ] Revisar gramatica en resultado final
    * [ ] Validar calculos matematicos

- [ ] **[CICLO] 7.4 Correccion Iterativa**
  - Aplicar protocolo de correccion rapida (< 5 minutos) para errores comunes
  - Usar protocolo de correccion compleja (> 5 minutos) para errores multiples
  - Documentar nuevos patrones de error encontrados
  - Actualizar biblioteca de soluciones si es necesario

---

### [LUPA] **FASE 8: Validacion y Testing Final**
*Verificar funcionamiento completo despues de correcciones*

- [ ] **[TEST] 8.1 Testing Automatizado Post-Correccion**
  - Ejecutar pruebas de diversidad de versiones
  - Verificar validaciones matematicas
  - Comprobar coherencia de datos generados
  - **[META] Validar sistema avanzado de distractores:**
    - Verificar que las 4 opciones sean textualmente unicas
    - Comprobar funcionamiento de valores duplicados (30% casos)
    - Validar seleccion estrategica de distractores
    - Confirmar justificaciones alternativas apropiadas
    - Probar multiples generaciones para verificar diversidad

- [ ] **[HERRAM] 8.2 Validacion de Correcciones Aplicadas**
  - **VERIFICAR**: Que todas las correcciones de Fase 7 se mantienen
  - Confirmar que no se introdujeron nuevos errores
  - **[WEB] Si persisten errores**: Investigar informacion oficial ICFES actualizada
  - Re-aplicar correcciones basadas en patrones exitosos
  - **[PYTHON] Errores Python**: Consultar `Auxiliares/Python-Documentation/referencias/compatibilidad-python.md` para soluciones
  - **[DISENO] Errores TikZ**: Consultar `Auxiliares/TikZ-Documentation/referencias/compatibilidad.md` para soluciones
  - **OBLIGATORIO - Errores LaTeX**: Para corregir cualquier error relacionado con compilacion LaTeX buscar soluciones en `/Auxiliares/Ejemplos-Funcionales-Rmd/Rnw/`

- [ ] **[TOOLS] 8.3 Validacion Especifica TikZ/Python**
  - **[DISENO] TikZ**: Ejecutar `source("Auxiliares/TikZ-Documentation/validar_tikz_compatibility.R")` si aplica
  - **[PYTHON] Python**: Usar herramientas de `Auxiliares/Python-Documentation/templates-rexams/multi-formato/python-rexams-system.R`
  - Verificar compatibilidad multi-formato segun checklists especificos
  - Probar generacion en PDF, HTML, y Moodle
  - Validar que graficos/diagramas se rendericen correctamente

- [ ] **[OK] 8.4 Compilacion Final Validada**
  - Verificar compilacion HTML: `exams2html(archivo.Rnw)`
  - Probar compilacion PDF: `exams2pdf(archivo.Rnw)`
  - Confirmar compilacion Moodle: `exams2moodle(archivo.Rnw)`
  - Validar que todos los graficos se generen correctamente
  - **CONFIRMAR**: Que todas las correcciones de errores recurrentes funcionan correctamente

---

## [AUTO] **AGENTE-GRAFICADOR ESPECIALIZADO TikZ**
*Sistema avanzado de replicacion grafica de alta fidelidad para contenido complejo*

### [LISTA] **ESPECIFICACIONES TECNICAS DEL AGENTE-GRAFICADOR**

#### **[META] Funcion y Objetivo**
- **Funcion Exclusiva**: Replicacion de alta fidelidad (98%+) de elementos graficos y tabulares complejos
- **Activacion**: Automatica cuando se detecta contenido grafico/tabular en FLUJO B
- **Objetivo**: Generar codigo TikZ avanzado que replique visualmente la imagen original
- **Integracion**: Compatible con sistema R-exams y configuracion LaTeX completa

#### **[HERRAM] Tecnologias y Algoritmos Especializados**

##### **[DISENO] Extraccion de Colores RGB Exactos**
```latex
<<echo=FALSE, results=hide>>=
# Algoritmo de deteccion de colores dominantes
extraer_colores_imagen <- function(ruta_imagen) {
  # Implementar analisis de histograma de colores
  # Extraer paleta RGB principal
  # Convertir a codigos TikZ compatibles
  colores_rgb <- c("#FF5733", "#33FF57", "#3357FF")  # Ejemplo
  return(colores_rgb)
}
@
```

##### **Sistema de Medicion Proporcional Automatica**
```latex
<<echo=FALSE, results=hide>>=
# Calculo de coordenadas y proporciones precisas
calcular_coordenadas_tikz <- function(elementos_detectados) {
  # Analizar posicionamiento relativo
  # Calcular escalas apropiadas
  # Generar sistema de coordenadas TikZ
  coordenadas <- list(x = c(0, 2, 4), y = c(0, 1.5, 3))
  return(coordenadas)
}
@
```

##### **[META] Templates Especializados por Tipo de Grafica**
- **Graficas de Barras**: Template con barras parametrizables y etiquetas
- **Graficas Circulares**: Template con sectores y leyendas automaticas
- **Graficas de Lineas**: Template con puntos, lineas y ejes
- **Tablas Complejas**: Template con celdas, bordes y formato
- **Histogramas**: Template con bins y distribuciones
- **Diagramas de Dispersion**: Template con puntos y tendencias
- **Boxplots**: Template con cuartiles y valores atipicos

#### **[CONFIG] Configuracion LaTeX Avanzada**
```latex
% Paquetes LaTeX necesarios para Agente-Graficador
\usepackage{tikz}
\usepackage{pgfplots}
\usepackage{xcolor}
\usepackage{colortbl}
\usepackage{amsmath}
\usepackage{array}
\usetikzlibrary{calc}
\usetikzlibrary{positioning}
\usetikzlibrary{decorations.markings}
```

### [CICLO] **PROTOCOLO DE REPLICACION ITERATIVA**

#### **Fase 1: Analisis y Deteccion**
1. **Identificar tipo de grafica** (barras, lineas, circular, tabla, etc.)
2. **Extraer elementos clave** (ejes, etiquetas, datos, colores)
3. **Seleccionar template apropiado** de biblioteca especializada
4. **Calcular parametros iniciales** (coordenadas, escalas, colores)

#### **Fase 2: Generacion Inicial**
1. **Aplicar template seleccionado** con parametros calculados
2. **Generar codigo TikZ inicial** con variables R integradas
3. **Renderizar primera version** usando include_tikz()
4. **Comparar con imagen original** visualmente

#### **Fase 3: Refinamiento Iterativo**
1. **Identificar discrepancias especificas**:
   - Colores no exactos
   - Proporciones incorrectas
   - Elementos faltantes o mal posicionados
   - Escalas inadecuadas
2. **Ajustar parametros sistematicamente**
3. **Re-renderizar y comparar**
4. **Repetir hasta alcanzar 98%+ fidelidad**

#### **Fase 4: Validacion Final**
1. **Presentar comparacion lado a lado** (original vs replicado)
2. **Aplicar checklist de fidelidad visual**
3. **Solicitar aprobacion usuario**
4. **Documentar codigo final optimizado**

### [DATOS] **SISTEMA DE METRICAS DE FIDELIDAD VISUAL**

#### **[OK] Criterios Cuantificables (98%+ Requerido)**

##### **[META] Precision Geometrica (25%)**
- Proporciones entre elementos: +/-2% tolerancia
- Angulos y orientaciones: +/-1 grados tolerancia
- Escalas relativas: +/-3% tolerancia
- **Metrica**: Comparacion de ratios dimensionales

##### **[COLOR] Fidelidad Cromatica (25%)**
- Colores RGB exactos: +/-5 unidades por canal
- Contrastes relativos: +/-10% tolerancia
- Saturacion y brillo: +/-8% tolerancia
- **Metrica**: Distancia euclidiana en espacio RGB

##### **[UBICACION] Posicionamiento (25%)**
- Ubicacion relativa de elementos: +/-2% tolerancia
- Alineacion de componentes: +/-1% tolerancia
- Espaciado entre elementos: +/-3% tolerancia
- **Metrica**: Comparacion de coordenadas normalizadas

##### **[CHECK] Completitud (25%)**
- Todos los elementos principales presentes: 100%
- Etiquetas y texto replicados: 100%
- Estructura general mantenida: 100%
- **Metrica**: Checklist binario de elementos

#### **[LUPA] Checklist de Validacion Visual**
- [ ] **Estructura General**: Se mantiene la organizacion visual?
- [ ] **Elementos Principales**: Estan todos los componentes clave?
- [ ] **Colores**: Son visualmente indistinguibles del original?
- [ ] **Proporciones**: Las relaciones dimensionales son correctas?
- [ ] **Texto y Etiquetas**: Son legibles y estan bien posicionados?
- [ ] **Calidad General**: Un observador casual notaria diferencias?

### [DISENO] **BIBLIOTECA DE TEMPLATES ESPECIALIZADOS**

#### **[DATOS] Template: Grafica de Barras**
```latex
% Template parametrizable para graficas de barras
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

#### **[CIRCULAR] Template: Grafica Circular**
```latex
% Template parametrizable para graficas circulares
\begin{tikzpicture}[scale=`r escala_circular`]
  \pie[
    color={`r paste(colores_sectores, collapse=",")`},
    radius=`r radio_grafica`,
    text=legend
  ]{`r paste(sprintf("%g/%s", valores_porcentajes, etiquetas_sectores), collapse=",")`}
\end{tikzpicture}
```

#### **[GRAFICO] Template: Tabla de Datos**
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

## [META] **METODOLOGIA TIKZ AVANZADA PARA NUEVAS IMAGENES PNG**

### [LISTA] **PROTOCOLO PASO A PASO PARA REPLICACION DE IMAGENES**

#### **PASO 1: Preparacion del Archivo**
```bash
# Colocar nueva imagen en directorio de trabajo bajo Lab/
/Lab/[proyecto]/nueva_imagen.png
# Ejemplo: /Lab/Prueba-Temporal_TikZ/nueva_imagen.png
```

#### **PASO 2: Analisis Visual Detallado**
1. **Identificar elementos matematicos** (figuras, ecuaciones, graficas)
2. **Extraer colores RGB** exactos de la imagen
3. **Medir proporciones** y posicionamiento relativo
4. **Catalogar texto** y etiquetas matematicas

#### **PASO 3: Consulta de Ejemplos Funcionales**
1. **OBLIGATORIO**: Revisar `/Auxiliares/Ejemplos-Funcionales-Rmd/Rnw/`
2. **Identificar patrones TikZ** similares exitosos
3. **Extraer configuraciones** tecnicas probadas
4. **Planificacion** de la estructura TikZ

#### **PASO 4: Implementacion Sistematica**

##### **4.1 Generacion del Codigo TikZ**
- Aplicar metodologia TikZ avanzada
- RGB colors exactos
- Posicionamiento preciso
- Estilos reutilizables

##### **4.2 Creacion del .Rnw**
- Estructura completa R-exams
- Sistema de aleatorizacion
- Generacion de distractores
- Meta-informacion ICFES

##### **4.3 Configuracion de Salidas**
- Actualizar SemilleroUnico_v2.R
- Configurar formatos exams2*
- Verificar compatibilidad

##### **4.4 Pruebas y Validacion**
- Generar HTML, PDF, Moodle
- Verificar fidelidad visual
- Comprobar funcionalidad

### [HERRAM] **PLANTILLA DE ARCHIVOS GENERADOS**

Para cada nueva imagen, se crearan:
```
[CARPETA] Lab/[proyecto]/
├── [DOC] [ejercicio]_[competencia]_[componente]_n[nivel[1, 2, 3, 4]]_v[versión].Rnw          # Ejercicio principal
├── [DOC] SemilleroUnico_v2.R                # Configuracion actualizada
├── [CARPETA] salida/
│   ├── [WEB] [nombre]_test.html             # Salida HTML
│   ├── [DOC] [nombre]_test.pdf              # Salida PDF
    └── [MOODLE] [nombre]_moodle.xml            # Salida Moodle
└── [DOC] REPORTE_[NOMBRE].md                # Documentacion
```

### [RAPIDO] **COMANDO RAPIDO PARA EMPEZAR**

Comando para nueva imagen:
> **"Aplica la metodologia TikZ avanzada a esta nueva imagen PNG para generar un ejercicio R-exams completo con salidas exams2*"**

---

## [META] **CRITERIOS DE CALIDAD OBLIGATORIOS**

### [OK] **Aleatorizacion Avanzada:**
- Minimo 300 versiones unicas verificadas
- Contextos, valores, colores, nombres variables
- Orden aleatorio de opciones

### [OK] **Robustez Matematica:**
- Validaciones de coherencia
- Manejo de casos extremos
- Precision numerica apropiada

### [OK] **Calidad Grafica:**
- Resolucion minima 150 DPI
- Etiquetas claras y legibles
- Colores contrastantes

### [OK] **Alineacion ICFES:**
- Competencia claramente evaluada
- Nivel de dificultad apropiado
- Contexto realista y relevante
- Distractores plausibles y educativos

### [OK] **Sistema Avanzado de Distractores:**
- **Diversidad**: Minimo 8 tipos diferentes de errores conceptuales
- **Valores Duplicados**: 30% probabilidad de opciones con mismo valor numerico pero justificaciones diferentes
- **Seleccion Estrategica**: 1 distractor duplicado + 2 diferentes cuando aplique
- **Verificacion Textual**: Las 4 opciones siempre textualmente unicas
- **Justificaciones Alternativas**: Multiples explicaciones incorrectas para valores correctos
- **Pedagogia**: Distractores que reflejan errores comunes de estudiantes

---

## [WEB] **INVESTIGACION WEB PARA MATEMATICAS ICFES**

### [META] **Fuentes Oficiales Prioritarias:**
- **ICFES Oficial**: `www.icfes.gov.co`, Documentos oficiales, guias de orientacion
- **Ministerio de Educacion**: Estandares basicos de competencias matematicas
- **Documentos SABER 11**: Estructura de pruebas, niveles de desempeno
- **Guias de orientacion actualizadas**: Competencias, contenidos, contextos

### [LUPA] **Busquedas Recomendadas:**
```
"competencia argumentacion matematicas ICFES 2025"
"interpretacion representacion matematicas SABER 11"
"formulacion ejecucion matematicas ICFES"
"niveles desempeno matematicas ICFES"
"estandares competencias matematicas Colombia"
```

### [DATOS] **Informacion a Investigar:**
- **Competencias**: Definiciones oficiales, ejemplos, criterios de evaluacion
- **Contenidos**: Categorias actualizadas (algebra, geometria, estadistica)
- **Contextos**: Tipos de situaciones evaluadas (familiar, laboral, etc.)
- **Niveles**: Descriptores de desempeno por nivel de dificultad
- **Ejemplos**: Preguntas tipo, estructuras, formatos

### **Criterios de Validacion:**
- Priorizar documentacion oficial ICFES/MEN
- Verificar fecha de publicacion (preferir 2023-2025)
- Contrastar con multiples fuentes oficiales
- Validar coherencia con ejemplos funcionales existentes

---

## [META] **IMPLEMENTACION DEL SISTEMA AVANZADO DE DISTRACTORES**

### [ESCRIBIR] **Codigo Base para Distractores con Valores Duplicados:**

```latex
<<echo=FALSE, results=hide>>=
# DECISION ALEATORIA: Permitir valores duplicados con justificaciones diferentes?
# 30% de probabilidad de generar opciones con mismo valor pero diferentes justificaciones
permitir_valores_duplicados <- sample(c(TRUE, FALSE), 1, prob = c(0.3, 0.7))

# SISTEMA AMPLIADO DE DISTRACTORES (8+ opciones para mayor diversidad)
afirmaciones_incorrectas <- c()

# DISTRACTOR 1: Confundir concepto principal con media
media_calculada <- round(mean(datos_ordenados), 1)
afirmaciones_incorrectas <- c(afirmaciones_incorrectas,
  paste0("La [concepto] es ", media_calculada, " porque se calcula sumando todos los valores y dividiendo por el numero de datos"))

# DISTRACTOR 2-8: [Implementar segun el concepto matematico especifico]
# - Confusion con moda, extremos, posiciones incorrectas
# - Errores de calculo comunes
# - Aplicacion incorrecta de formulas
# - Interpretaciones erroneas del procedimiento

# JUSTIFICACIONES ALTERNATIVAS para el valor correcto (pero con razonamiento incorrecto)
justificaciones_incorrectas_valor_correcto <- c(
  paste0("La [concepto] es ", valor_correcto, " porque representa el punto medio del rango"),
  paste0("La [concepto] es ", valor_correcto, " porque es el valor que mejor representa el conjunto"),
  paste0("La [concepto] es ", valor_correcto, " porque se obtiene al aplicar la formula basica")
)

# LOGICA DE SELECCION ADAPTADA
if(permitir_valores_duplicados) {
  # Incluir 1 justificacion incorrecta para el valor correcto + 2 valores diferentes
  # [Implementar logica de seleccion estrategica]
} else {
  # Modo tradicional: todos los valores diferentes
  # [Implementar seleccion estandar]
}

# VERIFICACION FINAL: Asegurar 4 opciones textualmente unicas
expect_equal(length(unique(todas_afirmaciones)), 4,
            info = "Las 4 opciones deben ser textualmente diferentes")
@
```

### [TEST] **Pruebas Especificas para Distractores:**

```latex
<<echo=FALSE, results=hide>>=
test_that("Prueba del sistema avanzado de distractores", {
  for(i in 1:50) {
    datos_test <- generar_datos()

    # Verificar opciones textualmente unicas
    expect_equal(length(unique(datos_test$opciones)), 4,
                info = "Las 4 opciones deben ser textualmente diferentes")

    # Verificar diversidad de distractores
    valores_numericos <- extraer_valores_numericos(datos_test$opciones)
    expect_true(length(unique(valores_numericos)) >= 2,
               info = "Debe haber al menos 2 valores numericos diferentes")

    # Verificar respuesta correcta presente
    expect_true(datos_test$afirmacion_correcta %in% datos_test$opciones,
               info = "La respuesta correcta debe estar presente")
  }
})
@
```

---

## [HERRAM] **COMANDOS DE USO RAPIDO**

### [AUTO] **Para Sistema Condicional Automatico:**
```
# Activar analisis automatico de imagen PNG
"Aplica el sistema condicional automatico a esta imagen PNG para detectar contenido grafico y activar el flujo apropiado"

# Activar Agente-Graficador Especializado (solo si se detecta contenido grafico)
"Activa el Agente-Graficador Especializado TikZ para replicar esta imagen con 98%+ fidelidad visual"

# Validar fidelidad visual
"Ejecuta la validacion de fidelidad visual comparando el TikZ generado con la imagen original"
```

### [META] **Para Flujos Especificos:**
```
# FLUJO A (sin graficas detectadas)
"Ejecuta FLUJO A estandar para imagen sin contenido grafico complejo"

# FLUJO B (con graficas detectadas)
"Ejecuta FLUJO B con Agente-Graficador para imagen con contenido grafico/tabular"
```

### [DISENO] **Para Metodologia TikZ Tradicional:**
```
# Comando original (ahora integrado en sistema condicional)
"Aplica la metodologia TikZ avanzada a esta nueva imagen PNG para generar un ejercicio R-exams completo con salidas exams2*"
```

### [LISTA] **Para Gestion de Tareas:**
```
# Investigar informacion ICFES
brave_web_search_brave-search: "termino especifico ICFES matematicas 2025"
web-fetch: [URL oficial ICFES]

# Crear tareas nuevas
add_tasks con esta estructura como base

# Actualizar progreso
update_tasks con task_id y nuevo state

# Compilar y probar
exams2html('archivo.Rnw')
```

---

## [LIBROS] **EJEMPLO COMPLETO DE ESTRUCTURA .RNW**

### [META] **Template Base para Nuevo Ejercicio .Rnw:**

```latex
\documentclass[10pt,a4paper]{article}

%% paquetes basicos
\usepackage[utf8]{inputenc}
\usepackage[spanish]{babel}
\usepackage{a4wide,color,verbatim,Sweave,url,xargs,amsmath,booktabs,longtable}
\usepackage{graphicx,float}
\usepackage{tikz,xcolor}
\usepackage{enumitem}

%% bibliotecas TikZ segun necesidad
\usetikzlibrary{automata,positioning,calc,arrows}

%% entornos para exams
\newenvironment{question}{\item}{}
\newenvironment{solution}{\comment}{\endcomment}
\newenvironment{answerlist}{\renewcommand{\labelenumii}{(\alph{enumii})}\begin{enumerate}}{\end{enumerate}}

%% comandos para metadatos exams
\newcommand{\exname}[1]{\def\@exname{#1}}
\newcommand{\extype}[1]{\def\@extype{#1}}
\newcommand{\exsolution}[1]{\def\@exsolution{#1}}
\newcommand{\exshuffle}[1]{\def\@exshuffle{#1}}
\newcommand{\exsection}[1]{\def\@exsection{#1}}

%% configuracion parrafos
\setlength{\parskip}{0.7ex plus0.1ex minus0.1ex}
\setlength{\parindent}{0em}

\begin{document}
\SweaveOpts{concordance=TRUE}

\begin{enumerate}

<<echo=FALSE, results=hide>>=
# Configuracion inicial
library(exams)
library(digest)
library(testthat)

# Configuracion TikZ
typ <- match_exams_device()
if(match_exams_call() == "exams2nops") typ <- "tex"

# Semilla aleatoria
set.seed(sample(1:100000, 1))

# Funcion generar_datos()
generar_datos <- function() {
  # Implementar aleatorizacion aqui
  # Retornar lista con todos los parametros
}

# Generar datos para este ejercicio
datos <- generar_datos()
@

\begin{question}

[Texto del ejercicio con variables: \Sexpr{datos$variable}]

<<echo=FALSE, results=tex>>=
# Si se necesita TikZ:
include_tikz(codigo_tikz, name = "diagrama", format = typ,
  library = c("tikz", "positioning"),
  width = "5cm")
@

<<echo=FALSE, results=tex>>=
answerlist(datos$opciones)
@

\end{question}

\begin{solution}

[Explicacion detallada de la solucion]

<<echo=FALSE, results=tex>>=
answerlist(datos$explicaciones)
@

\end{solution}

%% META-INFORMATION
\exname{Nombre del Ejercicio}
\extype{schoice}
\exsolution{\Sexpr{mchoice2string(datos$solutions)}}
\exshuffle{TRUE}
\exsection{Seccion Tematica}

\end{enumerate}
\end{document}
```

### [OK] **Puntos Clave de la Estructura .Rnw:**

1. **Documento LaTeX completo** con `\documentclass` y `\begin{document}`
2. **Chunks con sintaxis Sweave**: `<<opciones>>=` codigo `@`
3. **Meta-informacion con comandos LaTeX**: `\exname{}`, `\extype{}`, etc.
4. **Variables interpoladas con `\Sexpr{}`** en el texto LaTeX
5. **Configuracion TikZ con `include_tikz()`** para graficos
6. **Estructura exams estandar**: question, solution, answerlist
7. **Sin YAML headers** - todo es LaTeX puro

---

## [AUTO] **DOCUMENTACION DEL SISTEMA CONDICIONAL AUTOMATICO**

### [DATOS] **Diagrama de Flujo de Decision**

```
[CAMARA] IMAGEN PNG PROPORCIONADA
           v
[AUTO] ANALISIS AUTOMATICO DE CONTENIDO
           v
    Contiene graficas/tablas?
           v
    ┌─────────────────┐
    v                 v
[CICLO] FLUJO A          [META] FLUJO B
(Sin graficas)      (Con graficas)
    v                 v
[LISTA] Proceso          [AUTO] Agente-Graficador
   Estandar            Especializado
   (8 Fases)              v
    v               [CICLO] Proceso Iterativo
[OK] Ejercicio           (hasta 98% fidelidad)
   Completo              v
                   [OK] Validacion Usuario
                         v
                   [LISTA] Continuar 8 Fases
                         v
                   [OK] Ejercicio Completo
```

### [LUPA] **Criterios de Deteccion Automatica**

#### **[OK] Contenido que ACTIVA FLUJO B (Agente-Graficador)**
- **Graficas Estadisticas**:
  * Graficas de barras (verticales/horizontales)
  * Graficas circulares (pie charts)
  * Histogramas con bins definidos
  * Graficas de lineas con puntos de datos
  * Diagramas de dispersion (scatter plots)
  * Boxplots con cuartiles visibles

- **Tablas de Datos**:
  * Tablas con filas y columnas estructuradas
  * Matrices numericas organizadas
  * Tablas de frecuencia con encabezados
  * Tablas mixtas (texto + numeros)

- **Diagramas Matematicos**:
  * Diagramas de Venn con conjuntos
  * Arboles de probabilidad
  * Figuras geometricas con medidas
  * Planos cartesianos con funciones

- **Elementos Hibridos**:
  * Combinacion grafica + tabla
  * Diagrama + datos numericos
  * Multiples elementos graficos

#### **[CIRCULO] Contenido que MANTIENE FLUJO A (Proceso Estandar)**
- **Texto Simple**: Solo enunciados matematicos
- **Ecuaciones Basicas**: Formulas sin representacion grafica
- **Figuras Geometricas Simples**: Formas basicas sin datos complejos
- **Problemas Verbales**: Contextos sin elementos visuales complejos

### [META] **Especificaciones del Agente-Graficador por Tipo**

#### **[DATOS] Graficas de Barras**
- **Template**: `grafica-barras-avanzada.tikz`
- **Parametros Detectados**: Altura barras, colores, etiquetas ejes
- **Fidelidad Objetivo**: 98%+ en proporciones y colores
- **Tiempo Estimado**: 15-25 minutos proceso completo

#### **[CIRCULAR] Graficas Circulares**
- **Template**: `grafica-circular-avanzada.tikz`
- **Parametros Detectados**: Angulos sectores, colores, leyendas
- **Fidelidad Objetivo**: 98%+ en proporciones y distribucion
- **Tiempo Estimado**: 20-30 minutos proceso completo

#### **[GRAFICO] Tablas de Datos**
- **Template**: `tabla-datos-avanzada.tikz`
- **Parametros Detectados**: Estructura, contenido, formato
- **Fidelidad Objetivo**: 98%+ en organizacion y presentacion
- **Tiempo Estimado**: 10-20 minutos proceso completo

### [CICLO] **Protocolo de Validacion Usuario-Sistema**

#### **Fase 1: Presentacion Comparativa**
1. **Mostrar lado a lado**: Imagen original vs TikZ generado
2. **Destacar elementos clave** replicados
3. **Senalar diferencias** si las hay
4. **Calcular metricas** de fidelidad automaticas

#### **Fase 2: Checklist Interactivo**
- [ ] **Los colores son visualmente identicos?**
- [ ] **Las proporciones se mantienen correctas?**
- [ ] **Todos los elementos estan presentes?**
- [ ] **El posicionamiento es preciso?**
- [ ] **La calidad general es aceptable?**

#### **Fase 3: Decision y Continuacion**
- **Si >=98% fidelidad**: Aprobar y continuar con FASES 2-8
- **Si <98% fidelidad**: Repetir proceso iterativo
- **Si problemas tecnicos**: Consultar ejemplos funcionales

### [LISTA] **Ejemplos de Uso por Tipo de Imagen**

#### **Ejemplo 1: Imagen con Grafica de Barras**
```
INPUT: imagen_barras_ventas.png
DETECCION: Grafica de barras verticales detectada
FLUJO: B (Agente-Graficador)
TEMPLATE: grafica-barras-avanzada.tikz
PROCESO: Extraccion colores -> Calculo proporciones -> Generacion TikZ
VALIDACION: 98.5% fidelidad alcanzada
RESULTADO: Ejercicio R-exams con grafica TikZ replicada
```

#### **Ejemplo 2: Imagen con Tabla de Datos**
```
INPUT: tabla_estadisticas.png
DETECCION: Tabla 4x5 con encabezados detectada
FLUJO: B (Agente-Graficador)
TEMPLATE: tabla-datos-avanzada.tikz
PROCESO: Extraccion estructura -> Formateo -> Generacion TikZ
VALIDACION: 99.1% fidelidad alcanzada
RESULTADO: Ejercicio R-exams con tabla TikZ replicada
```

#### **Ejemplo 3: Imagen Solo Texto**
```
INPUT: problema_verbal.png
DETECCION: Solo texto matematico, sin graficas
FLUJO: A (Proceso Estandar)
PROCESO: FASES 1-8 estandar con TikZ basico
RESULTADO: Ejercicio R-exams tradicional
```

---

## [NOTA] **NOTAS IMPORTANTES**

### [AUTO] **Sistema Condicional Automatico:**
- **NUEVO**: El sistema detecta automaticamente contenido grafico y activa flujos especializados
- **FLUJO A**: Para imagenes sin graficas complejas (proceso estandar 8 fases)
- **FLUJO B**: Para imagenes con graficas/tablas (Agente-Graficador + 8 fases)
- **VALIDACION OBLIGATORIA**: 98%+ fidelidad visual antes de continuar con ejercicio completo
- **COMPATIBILIDAD TOTAL**: Mantiene todas las funcionalidades existentes del TEMPLATE

### [META] **Agente-Graficador Especializado:**
- **ACTIVACION**: Automatica cuando se detecta contenido grafico/tabular
- **OBJETIVO**: Replicacion de alta fidelidad (98%+) usando TikZ avanzado
- **PROCESO**: Iterativo hasta alcanzar criterios de calidad visual
- **INTEGRACION**: Compatible con sistema R-exams y aleatorizacion 300+ versiones
- **TEMPLATES**: Biblioteca especializada por tipo de grafica (barras, circular, tabla, etc.)

### [LUPA] **Investigacion Obligatoria:**
- **SIEMPRE** investigar informacion teorica ICFES en web cuando sea necesario
- Priorizar documentacion oficial y actualizada (2023-2025)
- Validar competencias, contenidos y contextos con fuentes oficiales
- Contrastar definiciones con ejemplos funcionales existentes

### [TOOLS] **Desarrollo Tecnico:**
- **SIEMPRE** consultar ejemplos funcionales ANTES de cualquier generacion, correccion u optimizacion
- Seguir patrones tecnicos probados en `/Auxiliares/Ejemplos-Funcionales-Rmd/Rnw/`
- Aplicar configuraciones exitosas de chunks, librerias y sintaxis
- **OBLIGATORIO - Errores LaTeX**: Para corregir cualquier error relacionado con compilacion LaTeX buscar soluciones en `/Auxiliares/Ejemplos-Funcionales-Rmd/Rnw/`

### **RESTRICCION CRITICA - CARACTERES ESPECIALES:**
- **NO USAR CARACTERES ESPECIALES UNICODE** en ninguna parte del codigo R-exams
- **USAR UNICAMENTE**:
  - Expresiones LaTeX para simbolos matematicos: `$\alpha$`, `$\beta$`, `$\pi$`, `$\sum$`, `$\int$`, etc.
  - Sintaxis TikZ para diagramas y figuras geometricas
  - Caracteres ASCII estandar para texto
- **EVITAR COMPLETAMENTE**:
  - Simbolos Unicode: alpha, beta, pi, suma, integral, menor-igual, mayor-igual, diferente, etc.
  - Caracteres especiales directos en el texto
  - Emojis o simbolos decorativos
- **EJEMPLO CORRECTO**: `La funcion $f(x) = \pi x^2$ tiene derivada $f'(x) = 2\pi x$`
- **EJEMPLO INCORRECTO**: `La funcion f(x) = pi x^2 tiene derivada f'(x) = 2pi x`

### [META] **Calidad Final:**
- Combinar investigacion teorica oficial con implementacion tecnica probada
- Asegurar alineacion perfecta entre competencia ICFES y ejercicio desarrollado
- Validar que el ejercicio cumple estandares oficiales actualizados
- **VERIFICAR** que no hay caracteres Unicode en todo el documento

### [META] **Sistema de Distractores Avanzado:**
- **IMPLEMENTAR SIEMPRE** el sistema de valores duplicados con justificaciones diferentes
- Generar minimo 8 tipos de distractores para maxima diversidad pedagogica
- Verificar que los distractores reflejen errores conceptuales reales de estudiantes
- Asegurar que las justificaciones alternativas sean matematicamente plausibles pero incorrectas
- Probar multiples generaciones para confirmar variedad en combinaciones de opciones

---

## [WEB] **BUSQUEDA RECURSIVA DE RECURSOS TikZ Y PYTHON**

### [META] **Enriquecimiento Continuo de Documentacion**

#### **[DISENO] BUSQUEDA RECURSIVA TikZ**
- [ ] **[LUPA] Identificar Necesidades TikZ Especificas**
  - Analizar ejercicio actual para determinar tipo de diagrama requerido
  - Consultar `Auxiliares/TikZ-Documentation/TikZ-ICFES-Guide.md` para gaps identificados
  - Verificar si templates existentes cubren la necesidad
  - **[WEB] Buscar recursos web** si no existe template apropiado

- [ ] **[WEB] Fuentes Web TikZ Prioritarias**
  - **TeXample.net**: http://www.texample.net/tikz/ (buscar por categoria matematica)
  - **PGFPlots Gallery**: http://pgfplots.sourceforge.net/gallery.html (graficos estadisticos)
  - **GitHub**: Repositorios "tikz mathematics", "tikz education"
  - **Overleaf**: Templates TikZ matematicos y educativos

- [ ] **[HERRAM] Adaptacion TikZ para R-exams**
  - Simplificar codigo encontrado segun `Auxiliares/TikZ-Documentation/referencias/compatibilidad.md`
  - Convertir `\pgfmathsetmacro` a variables R
  - Usar colores estandar en lugar de personalizados
  - Validar con `source("Auxiliares/TikZ-Documentation/validar_tikz_compatibility.R")`

#### **[PYTHON] BUSQUEDA RECURSIVA PYTHON**
- [ ] **[LUPA] Identificar Necesidades Python Especificas**
  - Analizar ejercicio actual para determinar tipo de grafico requerido
  - Consultar `Auxiliares/Python-Documentation/Python-ICFES-Guide.md` para gaps identificados
  - Verificar si templates existentes cubren la necesidad
  - **[WEB] Buscar recursos web** si no existe template apropiado

- [ ] **[WEB] Fuentes Web Python Prioritarias**
  - **Matplotlib Gallery**: https://matplotlib.org/stable/gallery/ (ejemplos oficiales)
  - **Python for Education**: Recursos educativos con matplotlib
  - **GitHub**: Repositorios "matplotlib education", "python mathematics"
  - **Jupyter Notebooks**: Ejemplos educativos de visualizacion

- [ ] **[HERRAM] Adaptacion Python para R-exams**
  - Simplificar codigo encontrado segun `Auxiliares/Python-Documentation/referencias/compatibilidad-python.md`
  - Usar solo matplotlib y numpy (bibliotecas validadas)
  - Implementar transferencia R->Python: `variable_python = r.variable_r`
  - Configurar chunks: `echo=FALSE, message=FALSE, results="hide"`
  - **OBLIGATORIO**: Agregar `plt.show()` al final

#### **[ENTRADA] INTEGRACION AL PROYECTO**
- [ ] **[CARPETA] Organizar Nuevos Recursos**
  - **TikZ**: Guardar en `Auxiliares/TikZ-Documentation/` segun clasificacion ICFES
  - **Python**: Guardar en `Auxiliares/Python-Documentation/` segun clasificacion ICFES
  - Documentar fuente original y adaptaciones realizadas
  - Crear template reutilizable si el recurso es valioso

- [ ] **[OK] Validar Nuevos Recursos**
  - Probar compatibilidad multi-formato (PDF, HTML, Moodle)
  - Verificar que funciona con variables aleatorias
  - Documentar en guias principales si es exitoso
  - Agregar a templates disponibles para futuros ejercicios

### [META] **Criterios de Seleccion para Busqueda Recursiva**

#### **[OK] Recursos TikZ Prioritarios**
- Diagramas geometricos 2D y 3D para pensamiento espacial
- Tablas y esquemas para presentacion de datos
- Diagramas de Venn y conjuntos para pensamiento aleatorio
- Construcciones geometricas para geometria metrica

#### **[OK] Recursos Python Prioritarios**
- Graficos estadisticos avanzados para pensamiento aleatorio
- Funciones matematicas para pensamiento variacional
- Representaciones numericas para pensamiento numerico
- Visualizaciones geometricas 2D para pensamiento espacial

#### **[HERRAM] Criterios de Compatibilidad**
- **TikZ**: Compatible con `include_tikz()` y bibliotecas basicas
- **Python**: Compatible con matplotlib/numpy y transferencia R->Python
- **R-exams**: Funciona en PDF, HTML, y Moodle sin errores
- **ICFES**: Alineado con competencias y niveles de dificultad
- **Escalabilidad**: Soporta multiples variantes aleatorias

### [RAPIDO] **Implementacion Inmediata**
1. **Identificar necesidad especifica** del ejercicio actual
2. **Buscar recurso apropiado** en fuentes web prioritarias
3. **Adaptar segun guias de compatibilidad** especificas
4. **Validar funcionamiento** en multiples formatos
5. **Documentar y compartir** si es exitoso para futuros ejercicios

---

## [LISTA] **ANEXO: PROTOCOLO ANTI-ERRORES DE IMPLEMENTACION**

### [META] **PROTOCOLO COMPLETO DE PREVENCION DE ERRORES**

#### **[LIBROS] FASE 0: CONSULTA OBLIGATORIA PRE-IMPLEMENTACION**
```
ANTES DE ESCRIBIR UNA SOLA LINEA DE CODIGO:

[OK] PASO 1: Abrir `/Auxiliares/Ejemplos-Funcionales-Rmd/Rnw/`
[OK] PASO 2: Identificar ejemplo mas similar al ejercicio objetivo
[OK] PASO 3: Estudiar estructura completa del ejemplo
[OK] PASO 4: Copiar configuracion LaTeX exacta
[OK] PASO 5: Copiar estructura de chunks exacta
[OK] PASO 6: Identificar patrones de interpolacion de variables
[OK] PASO 7: Entender configuracion TikZ/LaTeX especifica

REGLA ABSOLUTA: "No improvises. Copia patrones probados."
```

#### **[RAPIDO] VALIDACION CONTINUA DURANTE IMPLEMENTACION**
```
DESPUES DE CADA CHUNK:

□ ¿Compilo sin errores?
□ ¿La sintaxis es identica al ejemplo funcional?
□ ¿Las variables R se interpolan correctamente?
□ ¿No hay caracteres extra o faltantes?

SI ALGUNA RESPUESTA ES "NO": PARAR y consultar ejemplos funcionales
```

#### **[ALERTA] SENALES DE ALERTA CRITICAS**
```
PARAR INMEDIATAMENTE SI:

[ROJO] Estas interpolando variables complejas en TikZ sin ejemplo
[ROJO] Estas mezclando sintaxis R y LaTeX sin patron probado
[ROJO] Algo "parece que deberia funcionar" sin verificacion
[ROJO] Estas improvisando configuraciones no vistas en ejemplos
[ROJO] Aparecen errores de compilacion inesperados

ACCION: Volver a ejemplos funcionales y copiar patron exacto
```

#### **[LISTA] CHECKLIST FINAL OBLIGATORIO**
```
ANTES DE ENTREGAR CUALQUIER .RNW:

□ ¿Consulte TODOS los ejemplos funcionales relevantes?
□ ¿La sintaxis TikZ es identica a ejemplos probados?
□ ¿Las variables R se interpolan correctamente?
□ ¿No hay chunks extra o caracteres sobrantes?
□ ¿La estructura completa sigue patrones probados?
□ ¿Compilacion exitosa sin errores?
□ ¿Aplice metodologia de correccion de errores?
□ ¿Verifique funcionamiento en multiples formatos?

SOLO ENTREGAR SI TODAS LAS RESPUESTAS SON "SI"
```

#### **[META] ERRORES MAS COMUNES A EVITAR**
1. **Interpolacion incorrecta**: `\\draw[', variable, ',thick]` -> `\\draw[cyan,thick]`
2. **Chunks extra**: Verificar que no sobren ``` al final
3. **Sintaxis mixta**: No mezclar R y LaTeX sin patron probado
4. **Configuraciones inventadas**: Solo usar configuraciones de ejemplos funcionales
5. **Variables no definidas**: Verificar que todas las variables existan

#### **[HERRAM] PROTOCOLO DE RECUPERACION DE ERRORES**
```
SI ENCUENTRAS ERRORES:

1. NO intentes "arreglar rapido"
2. PARA la implementacion
3. CONSULTA ejemplos funcionales
4. IDENTIFICA el patron correcto
5. COPIA la sintaxis exacta
6. PRUEBA compilacion
7. CONTINUA solo si funciona

"Mejor perder 5 minutos consultando que 30 minutos debuggeando"
```

### [META] **IMPLEMENTACION INMEDIATA DEL PROTOCOLO**

**Este protocolo debe aplicarse OBLIGATORIAMENTE en todas las fases del template, especialmente:**
- **FASE 1.4-1.5**: Consulta y validacion pre-implementacion
- **FASES 3-6**: Validacion continua durante implementacion
- **FASE 7**: Correccion sistematica final

**El objetivo es PREVENIR errores, no corregirlos despues.**
