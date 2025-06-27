# CHECKLIST DE VALIDACIÓN SISTEMÁTICA PARA ARCHIVOS .RMD ICFES

## Lista de Verificación Completa para R-exams ICFES

---

## 🔍 FASE 1: VALIDACIÓN PRE-COMPILACIÓN

### ✅ A. ESTRUCTURA Y CONFIGURACIÓN BÁSICA

#### A1. Header YAML
- [ ] Incluye `output: html_document: default`
- [ ] Incluye `output: word_document: default`  
- [ ] Incluye `output: pdf_document: default`
- [ ] No hay caracteres especiales en el header

#### A2. Configuración Setup
- [ ] `Sys.setlocale(category = "LC_NUMERIC", locale = "C")` presente
- [ ] `options(OutDec = ".")` configurado
- [ ] `options(tikzLatex = "pdflatex")` establecido
- [ ] Paquetes TikZ completos en `tikzLatexPackages`
- [ ] `library(exams)` cargado
- [ ] `typ <- match_exams_device()` definido

#### A3. Configuración Knitr
- [ ] `warning = FALSE` establecido
- [ ] `message = FALSE` establecido
- [ ] `fig.cap = ""` configurado
- [ ] `dev = c("png", "pdf")` especificado
- [ ] `dpi = 150` o superior

---

### ✅ B. GENERACIÓN DE VARIABLES Y DATOS

#### B1. Semilla Aleatoria
- [ ] `set.seed(sample(1:10000, 1))` presente
- [ ] Semilla se establece antes de cualquier aleatorización

#### B2. Variables de Contexto
- [ ] Contextos matemáticos definidos y aleatorizados
- [ ] Términos de vocabulario variados (figura, puntos, cantidad)
- [ ] **CRÍTICO**: Concordancia de género verificada
  - [ ] "cantidad" → "La cantidad" ✅
  - [ ] "número" → "El número" ✅  
  - [ ] "total" → "El total" ✅
  - [ ] "suma" → "La suma" ✅
  - [ ] "conteo" → "El conteo" ✅

#### B3. Generación de Datos Matemáticos
- [ ] Rangos de aleatorización apropiados (no muy pequeños/grandes)
- [ ] Validación de que los datos generados son matemáticamente válidos
- [ ] **CRÍTICO**: Verificación de valores únicos cuando sea necesario
- [ ] Datos de prueba generan resultados esperados

#### B4. Sistema de Distractores
- [ ] **CRÍTICO**: Mínimo 4 opciones únicas generadas
- [ ] Distractores matemáticamente plausibles
- [ ] Respuesta correcta incluida en opciones finales
- [ ] Validación anti-duplicados implementada
- [ ] Verificación de que `length(unique(opciones_finales)) == 4`

---

### ✅ C. CÓDIGO TIKZ Y VISUALIZACIÓN

#### C1. Estructura TikZ Básica
- [ ] `\begin{tikzpicture}` y `\end{tikzpicture}` balanceados
- [ ] Escala apropiada definida (`scale=1.0` o similar)
- [ ] Colores RGB definidos correctamente
- [ ] Estilos TikZ configurados

#### C2. Posicionamiento de Elementos
- [ ] **CRÍTICO**: Orden correcto de elementos:
  1. [ ] Figuras/diagramas principales (parte superior)
  2. [ ] Texto explicativo (posición intermedia alta)
  3. [ ] Tabla de datos (posición intermedia baja)  
  4. [ ] Pregunta (posición inferior)
- [ ] Coordenadas Y en orden descendente lógico
- [ ] Espaciado mínimo de 1.0 unidad entre elementos principales
- [ ] No hay superposiciones visuales

#### C3. Elementos Específicos
- [ ] Puntos/círculos con tamaño consistente (`circle (0.1)`)
- [ ] Líneas con grosor apropiado (`line width=1pt`)
- [ ] Texto con ancho limitado (`text width=12cm`)
- [ ] Tablas con formato correcto y bordes

#### C4. Configuración include_tikz
- [ ] Parámetro `name` descriptivo y único
- [ ] `markup = "markdown"` especificado
- [ ] `format = typ` incluido
- [ ] `packages` completos para compilación
- [ ] `width` apropiado (típicamente "14cm")

---

### ✅ D. CONTENIDO Y TEXTO

#### D1. Sección Question
- [ ] Introducción matemática clara y precisa
- [ ] Contexto del problema bien establecido
- [ ] Instrucciones específicas para el estudiante
- [ ] **CRÍTICO**: Gramática y concordancia correctas
- [ ] Variables dinámicas integradas naturalmente

#### D2. Sección Answerlist
- [ ] Exactamente 4 opciones listadas
- [ ] Formato consistente (`* opción`)
- [ ] **CRÍTICO**: Todas las opciones son numéricamente diferentes
- [ ] Opciones en orden lógico (típicamente ascendente)

#### D3. Sección Solution
- [ ] Explicación paso a paso del proceso de solución
- [ ] Fórmulas matemáticas correctas en LaTeX
- [ ] Verificación con datos conocidos
- [ ] Cálculo explícito para la respuesta
- [ ] Análisis de distractores comunes
- [ ] Answerlist de solución con Verdadero/Falso correctos

#### D4. Meta-information
- [ ] `exname` descriptivo y único
- [ ] `extype: schoice` especificado
- [ ] `exsolution` con patrón binario correcto
- [ ] `exshuffle: TRUE` incluido
- [ ] `exsection` apropiada (ej: "Argumentación matemática")
- [ ] `exextra` campos completos (Type, Level, Language, Course)

---

## 🔍 FASE 2: VALIDACIÓN POST-COMPILACIÓN

### ✅ E. VERIFICACIÓN VISUAL

#### E1. Layout y Posicionamiento
- [ ] **CRÍTICO**: Tabla aparece DESPUÉS del texto explicativo
- [ ] Elementos no se superponen
- [ ] Espaciado visual apropiado
- [ ] Figuras/diagramas claramente visibles
- [ ] Texto legible y bien formateado

#### E2. Contenido Renderizado
- [ ] **CRÍTICO**: Todas las opciones de respuesta son diferentes
- [ ] Gramática correcta en todo el texto renderizado
- [ ] Variables dinámicas se muestran correctamente
- [ ] Fórmulas matemáticas renderizadas apropiadamente
- [ ] Tablas con formato correcto y datos alineados

#### E3. Funcionalidad R-exams
- [ ] Archivo compila sin errores
- [ ] Genera múltiples versiones exitosamente
- [ ] Respuesta correcta identificada apropiadamente
- [ ] Aleatorización funciona correctamente

---

## 🔍 FASE 3: VALIDACIÓN MATEMÁTICA Y PEDAGÓGICA

### ✅ F. CORRECCIÓN MATEMÁTICA

#### F1. Cálculos y Fórmulas
- [ ] Fórmula principal implementada correctamente
- [ ] Cálculos intermedios verificados
- [ ] Respuesta correcta matemáticamente válida
- [ ] Distractores basados en errores comunes reales
- [ ] Rangos de valores apropiados para el nivel educativo

#### F2. Coherencia de Datos
- [ ] Datos de entrada consistentes con el contexto
- [ ] Progresiones/secuencias matemáticamente correctas
- [ ] Unidades de medida apropiadas y consistentes
- [ ] Escalas y proporciones realistas

### ✅ G. CALIDAD PEDAGÓGICA

#### G1. Claridad del Problema
- [ ] Enunciado claro y sin ambigüedades
- [ ] Contexto relevante para estudiantes ICFES
- [ ] Nivel de dificultad apropiado
- [ ] Información suficiente para resolver el problema

#### G2. Opciones de Respuesta
- [ ] Distractores pedagógicamente útiles
- [ ] Opciones plausibles y no obviamente incorrectas
- [ ] Rango de valores apropiado
- [ ] No hay pistas inadvertidas en las opciones

---

## 🚨 ERRORES CRÍTICOS QUE DETIENEN LA VALIDACIÓN

### ❌ Errores de Bloqueo Total
1. **Concordancia de género incorrecta** (ej: "La conteo")
2. **Opciones de respuesta duplicadas**
3. **Tabla aparece antes del texto explicativo**
4. **Errores de compilación LaTeX/TikZ**
5. **Variables no definidas en chunks**

### ⚠️ Errores Graves que Requieren Corrección
1. Posicionamiento incorrecto de elementos TikZ
2. Fórmulas matemáticas incorrectas
3. Distractores no plausibles
4. Problemas de encoding o caracteres especiales
5. Meta-información incompleta

---

## 📋 CHECKLIST RÁPIDO (2 MINUTOS)

### ✅ Verificación Express
- [ ] **Gramática**: ¿"El conteo" o "La cantidad"? (no "La conteo")
- [ ] **Orden**: ¿Texto → Tabla → Pregunta?
- [ ] **Opciones**: ¿4 valores diferentes?
- [ ] **Compilación**: ¿Sin errores?
- [ ] **Visual**: ¿Tabla después del texto?

### 🔧 Herramientas de Validación Automática

```r
# Función de validación rápida
validacion_rapida <- function(archivo_rmd) {
  errores <- detectar_errores_comunes(archivo_rmd)
  if(length(errores) == 0) {
    cat("✅ Validación básica APROBADA\n")
  } else {
    cat("❌ Errores detectados:\n")
    for(i in 1:length(errores)) {
      cat(paste0("  - ", names(errores)[i], ": ", errores[[i]], "\n"))
    }
  }
}
```

---

**Nota**: Este checklist debe ejecutarse ANTES de considerar cualquier archivo .Rmd como completo y listo para uso en producción.
