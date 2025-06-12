# 🎯 PROMPT PARA GENERACIÓN DE EJERCICIOS ICFES MATEMÁTICAS EN R-EXAMS
## Reglas Completas Tipo 2 - Prevención de Errores Recurrentes

### 📋 PROTOCOLO OBLIGATORIO DE VERIFICACIÓN PREVIA

**ANTES de generar cualquier código, DEBES:**

1. **CONSULTAR EJEMPLOS FUNCIONALES OBLIGATORIAMENTE:**
   ```
   - Revisar `/Auxiliares/Ejemplos_Funcionales.md/fracciones_reparto_premio_v1.Rmd` para configuración LaTeX
   - Revisar `/Auxiliares/Ejemplos_Funcionales.md/Ejemplo_01.Rmd` para código Python correcto
   - Usar EXACTAMENTE las mismas configuraciones que funcionan
   ```

2. **VERIFICAR CONFIGURACIONES CRÍTICAS:**
   - ✅ `matplotlib.use('Agg')` SIEMPRE al inicio del código Python
   - ✅ `options(tikzLatex = "pdflatex")` y `options(tikzXelatex = FALSE)`
   - ✅ Configuración YAML simplificada sin `xelatex`
   - ✅ `match_exams_call()` para detección de formato

### 🔧 CONFIGURACIONES OBLIGATORIAS

#### **1. YAML Header (USAR EXACTAMENTE ESTA ESTRUCTURA):**
```yaml
---
output:
  html_document: default
  pdf_document:
    keep_tex: true
    extra_dependencies: ["graphicx", "float", "tikz", "xcolor"]
  word_document: default
icfes:
  competencia: [competencia_específica]
  nivel_dificultad: [1-4]
  contenido:
    categoria: [algebra_calculo|geometria|estadistica]
    tipo: [generico|no_generico]
  contexto: [familiar|laboral|comunitario|matematico]
  eje_axial: [eje1|eje2|eje3|eje4]
  componente: [geometrico_metrico|numerico_variacional|aleatorio]
---
```

#### **2. Chunk de Setup (COPIAR EXACTAMENTE):**
```r
```{r setup, include=FALSE}
# Configuración para todos los formatos de salida
Sys.setlocale(category = "LC_NUMERIC", locale = "C")
options(OutDec = ".")

# Configurar el motor LaTeX globalmente
options(tikzLatex = "pdflatex")
options(tikzXelatex = FALSE)
options(tikzLatexPackages = c(
  "\\usepackage{tikz}",
  "\\usepackage{colortbl}",
  "\\usepackage{xcolor}",
  "\\usepackage{graphicx}",
  "\\usepackage{float}"
))

library(exams)
library(reticulate)
library(digest)
library(testthat)
library(knitr)

typ <- match_exams_device()
options(scipen = 999)
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  fig.showtext = FALSE,
  fig.cap = "",
  fig.keep = 'all',
  dev = c("png", "pdf"),
  dpi = 150,
  fig.pos = "H"
)

# Configuración para chunks de Python
knitr::knit_engines$set(python = function(options) {
  knitr::engine_output(options, options$code, '')
})

# Asegurar que Python esté correctamente configurado
use_python(Sys.which("python"), required = TRUE)

# Semilla aleatoria para diversidad de versiones
set.seed(sample(1:100000, 1))
```

#### **3. Chunk de Generación de Datos:**
```r
```{r data_generation, echo=FALSE, results="hide"}
# Función principal de generación de datos
generar_datos <- function() {
  # IMPLEMENTAR LÓGICA ESPECÍFICA SEGÚN EL PROBLEMA
  # Debe generar al menos 300 versiones únicas
  # Incluir validaciones y manejo de errores
  # Retornar lista con todos los parámetros necesarios
  
  return(list(
    # Variables del problema
  ))
}

# Generar datos del ejercicio
datos <- generar_datos()

# Extraer variables individuales para facilitar uso
# [Definir variables específicas según el problema]
```

#### **4. Chunk de Prueba de Diversidad (OBLIGATORIO):**
```r
```{r version_diversity_test, echo=FALSE, results="hide"}
# Prueba obligatoria de diversidad de versiones
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

#### **5. Código Python (USAR EXACTAMENTE ESTA ESTRUCTURA):**
```r
```{r generar_graficos_python, echo=FALSE, results="hide"}
options(OutDec = ".")  # Asegurar punto decimal en este chunk

# Código Python OBLIGATORIO con matplotlib.use('Agg')
codigo_base_python <- "
import matplotlib
matplotlib.use('Agg')  # CRÍTICO: NUNCA OMITIR ESTA LÍNEA
import matplotlib.pyplot as plt
import numpy as np
matplotlib.rcParams['font.size'] = 9

# Variables del problema
[variables_específicas] = %s
"

# Reemplazar valores en el código Python base
codigo_python_base <- sprintf(codigo_base_python,
                            # Parámetros específicos del problema
                            )

# Código para cada opción de respuesta
codigo_python_opcion_a <- paste0(codigo_python_base, "
# Opción A: CORRECTA
plt.figure(figsize=(8, 5))
# [Código específico del gráfico]
plt.savefig('opcion_a.png', dpi=150, bbox_inches='tight')
plt.close()
")

# Ejecutar los códigos de Python para generar las gráficas
py_run_string(codigo_python_opcion_a)
# [Repetir para otras opciones]
```

#### **6. Inclusión de Imágenes (USAR EXACTAMENTE ESTA ESTRUCTURA):**
```r
```{r options, echo=FALSE, results='asis'}
# Detectar si se está generando para Moodle u otros formatos
formatos_moodle <- c("exams2moodle", "exams2qti12", "exams2qti21", "exams2openolat")
es_moodle <- (match_exams_call() %in% formatos_moodle)

# Mostrar las opciones con tamaño apropiado
if (es_moodle) {
  # Tamaño para Moodle
  cat("- ![](opcion_a.png){width=80%}\n\n")
  cat("- ![](opcion_b.png){width=80%}\n\n")
  cat("- ![](opcion_c.png){width=80%}\n\n")
  cat("- ![](opcion_d.png){width=80%}\n\n")
} else {
  # Tamaño para PDF/Word
  cat("- ![](opcion_a.png){width=90%}\n\n")
  cat("- ![](opcion_b.png){width=90%}\n\n")
  cat("- ![](opcion_c.png){width=90%}\n\n")
  cat("- ![](opcion_d.png){width=90%}\n\n")
}
```

#### **7. Etiquetas de Sección (EVITAR DUPLICADOS):**
```markdown
### Análisis del problema {#analisis-problema-`r sample(1:10000, 1)`}
### Cálculo de proporciones {#calculo-proporciones-`r sample(1:10000, 1)`}
### Gráfica correcta {#grafica-correcta-`r sample(1:10000, 1)`}
```

#### **8. Sección Solution con Imágenes:**
```r
```{r solucion_grafica, echo=FALSE, results='asis', fig.align='center'}
# Detectar formato y ajustar tamaño
if (es_moodle) {
  cat("![](opcion_a.png){width=80%}")
} else {
  cat("![](opcion_a.png){width=90%}")
}
```

### ⚠️ ERRORES CRÍTICOS A EVITAR

#### **NUNCA hacer esto:**

**❌ Configuración YAML incorrecta:**
```yaml
# ❌ INCORRECTO - Causa error \pandocbounded
output:
  pdf_document:
    latex_engine: xelatex  # ❌ NO USAR
header-includes:
- \usepackage{fontspec}    # ❌ NO USAR
- \usepackage{unicode-math} # ❌ NO USAR
```

**❌ Código Python sin matplotlib.use('Agg'):**
```python
# ❌ INCORRECTO - Causa errores de matplotlib
import matplotlib.pyplot as plt  # ❌ SIN matplotlib.use('Agg')
```

**❌ Configuración LaTeX incorrecta:**
```r
# ❌ INCORRECTO - Causa conflictos
options(tikzLatex = "xelatex")     # ❌ NO USAR
options(tikzXelatex = TRUE)        # ❌ NO USAR
```

**❌ Etiquetas duplicadas:**
```markdown
### Análisis del problema  # ❌ SIN ID único
```

**❌ Imágenes sin detección de formato:**
```r
# ❌ INCORRECTO - Causa problemas de renderizado
cat("![](imagen.png)")  # ❌ SIN detección de formato
```

### 🔍 LISTA DE VERIFICACIÓN OBLIGATORIA

**Antes de entregar el código, verificar:**

- [ ] ✅ `matplotlib.use('Agg')` está presente al inicio del código Python
- [ ] ✅ Configuración LaTeX usa `pdflatex` (NO `xelatex`)
- [ ] ✅ YAML header es simple (sin `latex_engine: xelatex`)
- [ ] ✅ Todas las secciones tienen IDs únicos con `sample(1:10000, 1)`
- [ ] ✅ Imágenes usan `match_exams_call()` para detección de formato
- [ ] ✅ Configuración de chunks sigue ejemplos funcionales exactamente
- [ ] ✅ Test de diversidad de versiones incluido y funcional
- [ ] ✅ Configuración de reticulate correcta con `use_python()`
- [ ] ✅ Semilla aleatoria configurada correctamente
- [ ] ✅ Metadatos ICFES completos

### 📚 REFERENCIAS OBLIGATORIAS

**Siempre consultar estos archivos antes de generar código:**
1. `/Auxiliares/Ejemplos_Funcionales.md/fracciones_reparto_premio_v1.Rmd` - Configuración LaTeX robusta
2. `/Auxiliares/Ejemplos_Funcionales.md/Ejemplo_01.Rmd` - Código Python correcto
3. Cualquier archivo funcional del proyecto para patrones específicos

### 🚨 PROTOCOLO DE CORRECCIÓN DE ERRORES

**Si encuentras errores:**
1. **Error `\pandocbounded`** → Verificar configuración LaTeX y matplotlib
2. **Errores de Python** → Consultar Ejemplo_01.md
3. **Problemas con gráficos** → Revisar código Python en ejemplos
4. **Falla configuración LaTeX** → Usar configuración de fracciones_reparto_premio_v1.Rmd
5. **Errores de chunks** → Seguir estructura de Ejemplo_01.md
6. **Problemas con reticulate** → Verificar configuración en ejemplos

### 🎯 INSTRUCCIÓN FINAL CRÍTICA

**"Si encuentras CUALQUIER error relacionado con LaTeX, Pandoc, matplotlib o compilación, DETENTE inmediatamente y consulta los ejemplos funcionales. NO improvises configuraciones. USA EXACTAMENTE los patrones que funcionan. El script SemilleroUnico_v2.R ha sido funcional con numerosos archivos .Rmd anteriores, por lo que cualquier error viene del archivo .Rmd específico, no del script de ejecución."**

---

## 📊 ESTRUCTURA COMPLETA DEL ARCHIVO .RMD

### Orden obligatorio de secciones:
1. YAML Header
2. Chunk setup
3. Chunk data_generation
4. Chunk version_diversity_test
5. Chunk generar_graficos_python
6. Question
7. Answerlist con chunk options
8. Solution
9. Answerlist de solución
10. Meta-information

### 🏆 RESULTADO ESPERADO

Un archivo .Rmd completamente funcional que:
- ✅ Compila sin errores en todos los formatos
- ✅ Genera mínimo 300 versiones únicas
- ✅ Es compatible con SemilleroUnico_v2.R
- ✅ Sigue todas las mejores prácticas del proyecto
- ✅ Previene errores recurrentes como `\pandocbounded`
