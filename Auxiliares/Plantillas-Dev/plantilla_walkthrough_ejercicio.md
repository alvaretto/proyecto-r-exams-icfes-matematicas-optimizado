---
output:
  html_document: default
  pdf_document: default
---
# Walkthrough Exhaustivo: 

## Ejercicio de [NOMBRE_EJERCICIO] - [COMPETENCIA_ICFES]

> **PARA NOVATOS:** Este documento explica paso a paso como funciona un ejercicio avanzado de matematicas que se genera automaticamente con cientos de versiones diferentes. No te preocupes si no entiendes todo al principio.

## Descripcion General del Proyecto

> **Que es esto?** Imagina que tienes un profesor que puede crear infinitas versiones del mismo problema de matematicas, cada una con numeros y contextos diferentes pero evaluando la misma habilidad de **[COMPETENCIA]**. Eso es exactamente lo que hace este proyecto.

Este es un **ejercicio de matematicas avanzado** que ensena sobre [CONCEPTO_PRINCIPAL] usando [CONTEXTO_APLICACION]. 

**Que aprenderan los estudiantes?**

- [OBJETIVO_APRENDIZAJE_1]
- [OBJETIVO_APRENDIZAJE_2]
- [OBJETIVO_APRENDIZAJE_3]
- [OBJETIVO_APRENDIZAJE_4]

**Ejemplo conceptual:** [ANALOGIA_SIMPLE_DEL_CONCEPTO]

## Competencia ICFES Evaluada

> **Para novatos:** ICFES es el examen de Estado colombiano. Cada pregunta evalua una "competencia" especifica (una habilidad matematica). Aqui te explicamos que evalua este ejercicio:

**Competencia:** [COMPETENCIA] *(Descripcion de la competencia)*
**Nivel:** [NIVEL] ([DESCRIPCION_NIVEL]) *(Descripcion del nivel de dificultad)*
**Componente:** [COMPONENTE] *(Area matematica especifica)*
**Contexto:** [CONTEXTO] *(Tipo de situacion)*
**Eje Axial:** [EJE] *(Clasificacion tecnica ICFES)*

> **Ejemplo practico:** [DESCRIPCION_EJEMPLO_CONCRETO]

### Metadatos ICFES Completos
```yaml
icfes:
  competencia:
    - [COMPETENCIA_VALOR]                    # [DESCRIPCION_COMPETENCIA]
  nivel_dificultad: [NIVEL_NUMERO]          # [DESCRIPCION_NIVEL] (1-4 escala)
  contenido:
    categoria: [CATEGORIA]                   # Area matematica
    tipo: [TIPO]                            # Aplicabilidad
  contexto: [CONTEXTO_VALOR]                # Tipo de situaciones
  eje_axial: [EJE_VALOR]                    # Clasificacion ICFES
  componente: [COMPONENTE_VALOR]            # Pensamiento matematico
```

## Archivos del Proyecto

### Archivo Principal:
- `[NOMBRE_ARCHIVO].Rmd` - El ejercicio completo (no lo abras si eres principiante)

### Scripts Faciles de Usar:

- `SemilleroUnico_v2.R` - **RECOMENDADO** Genera el ejercicio en diferentes formatos
- `SemilleroMoodle_v2.R` - Genera ejercicios para Moodle (plataforma educativa)

### Carpeta de Resultados:

- `salida/` - Aqui apareceran los ejercicios generados

### Otros archivos:

- `walkthrough.md` - Esta guia que estas leyendo
- `[NOMBRE_ARCHIVO].html` - Vista previa del ejercicio
- Archivos `.tex` - Plantillas tecnicas (no los toques)

## Como usar este ejercicio (Guia Facil)

### Opcion 1: La Mas Facil (Recomendada para principiantes)

1. **Abre RStudio** (el programa para usar R)
2. **Abre el archivo** `SemilleroUnico_v2.R` 
3. **Presiona el boton "Source"** (o Ctrl+Shift+Enter)
4. **Espera** El programa hara todo automaticamente
5. **Ve a la carpeta "salida"** para ver los ejercicios creados

**Que obtienes?**

- Un archivo HTML (para ver en navegador)
- Un archivo PDF (para imprimir)
- Un archivo Word (para editar)
- Un archivo NOPS (para escanear respuestas)

### Opcion 2: Solo para Moodle

1. **Abre RStudio**
2. **Abre el archivo** `SemilleroMoodle_v2.R`
3. **Presiona "Source"**
4. **Ve a la carpeta "salida"** para encontrar el archivo XML de Moodle

### Opcion 3: Ver Vista Previa

1. **Abre tu navegador web**
2. **Abre el archivo** `[NOMBRE_ARCHIVO].html`
3. **Ya puedes ver como se ve el ejercicio**

## Que hace especial a este ejercicio

### 1. [CARACTERISTICA_ESPECIAL_1]

- [DESCRIPCION_DETALLADA_1]
- [BENEFICIO_1]
- [EJEMPLO_1]

### 2. [CARACTERISTICA_ESPECIAL_2]

- [DESCRIPCION_DETALLADA_2]
- [BENEFICIO_2]
- [EJEMPLO_2]

### 3. [CARACTERISTICA_ESPECIAL_3]

- [DESCRIPCION_DETALLADA_3]
- [BENEFICIO_3]
- [EJEMPLO_3]

## Como esta hecho el ejercicio (Para curiosos)

### Informacion Tecnica (ICFES):

- **Tipo:** [TIPO_EJERCICIO]
- **Dificultad:** Nivel [NIVEL] ([DESCRIPCION_DIFICULTAD])
- **Tema:** [TEMA_MATEMATICO]
- **Contexto:** [DESCRIPCION_CONTEXTO]

### **[BLOQUE] Partes del Ejercicio:**

#### **[GRAFICO] 1. [ELEMENTO_VISUAL_PRINCIPAL]**

- Se crea automaticamente con **[HERRAMIENTA_GRAFICO]**
- [DESCRIPCION_ELEMENTO_VISUAL]
- [CARACTERISTICAS_VISUALES]
- Se guarda como imagen PNG

#### **[PREGUNTA] 2. Las Opciones de Respuesta**

- Hay **4 opciones** (A, B, C, D)
- Solo **1 es correcta**
- Las otras 3 son **errores comunes** que cometen los estudiantes
- Se mezclan aleatoriamente

#### **3. La Explicacion**

- Explica **paso a paso** por que la respuesta es correcta
- Usa **matematicas simples** para demostrarlo
- Ayuda a entender el concepto
- Incluye ejemplos con numeros

#### **[ALEATORIO] 4. La Variacion Automatica**

- [VARIACION_1]
- [VARIACION_2]
- [VARIACION_3]
- Cada ejercicio es unico!

## [ESTADO] **Como saber si funciona bien?**

### **[ANALISIS] Senales de que Todo Esta Bien:**

1. **No hay errores rojos** en RStudio
2. **Se crean archivos** en la carpeta "salida"
3. **[VALIDACION_ESPECIFICA_1]**
4. **[VALIDACION_ESPECIFICA_2]**
5. **[VALIDACION_ESPECIFICA_3]**

### **[CARPETA] Que archivos deberias ver?**

**En la carpeta "salida" encontraras:**

- Archivos **PDF** (para imprimir)
- Archivos **Word** (para editar)
- Archivos **HTML** (para ver en navegador)
- Archivos **XML** (para Moodle)
- Archivos **NOPS** (para escanear)

### **[ALERTA] Algo salio mal?**

**Si ves errores rojos:**

1. **Revisa** que tengas R y RStudio instalados
2. **Verifica** que tengas las librerias necesarias
3. **Asegurate** de estar en la carpeta correcta
4. **Intenta** ejecutar `SemilleroUnico_v2.R` de nuevo

**Si no se crean archivos:**

- Revisa que la carpeta "salida" exista
- Verifica que tengas permisos de escritura
- Intenta cerrar y abrir RStudio

## [META] **Que aprenden los estudiantes con este ejercicio?**

### **[DOCUMENTACION] Conceptos Matematicos:**

- **[CONCEPTO_1]:** [DESCRIPCION_CONCEPTO_1]
- **[CONCEPTO_2]:** [DESCRIPCION_CONCEPTO_2]
- **[CONCEPTO_3]:** [DESCRIPCION_CONCEPTO_3]
- **[CONCEPTO_4]:** [DESCRIPCION_CONCEPTO_4]

### **[INTELIGENTE] Habilidades que Desarrollan:**

- **[HABILIDAD_1]:** [DESCRIPCION_HABILIDAD_1]
- **[HABILIDAD_2]:** [DESCRIPCION_HABILIDAD_2]
- **[HABILIDAD_3]:** [DESCRIPCION_HABILIDAD_3]
- **[HABILIDAD_4]:** [DESCRIPCION_HABILIDAD_4]

### Por que es Importante

- **[IMPORTANCIA_1]:** [DESCRIPCION_IMPORTANCIA_1]
- **[IMPORTANCIA_2]:** [DESCRIPCION_IMPORTANCIA_2]
- **[IMPORTANCIA_3]:** [DESCRIPCION_IMPORTANCIA_3]
- **[IMPORTANCIA_4]:** [DESCRIPCION_IMPORTANCIA_4]

## Que hacer ahora

### Si eres profesor:

1. **Ejecuta** `SemilleroUnico_v2.R` para crear ejercicios
2. **Usa** los archivos PDF para imprimir examenes
3. **Sube** los archivos XML a Moodle si usas esa plataforma
4. **Explica** a los estudiantes [CONCEPTO_CLAVE_PARA_EXPLICAR]

### Si eres tecnico/programador:

1. **Revisa** el archivo `.Rmd` para entender la estructura
2. **Modifica** los scripts si necesitas cambios especificos
3. **Experimenta** con diferentes configuraciones
4. **Documenta** cualquier cambio que hagas

### Si eres estudiante:

1. **Abre** el archivo HTML para ver el ejercicio
2. **Intenta** resolver el problema antes de ver la respuesta
3. **Lee** la explicacion para entender el concepto
4. **Practica** con conceptos similares

### Quieres hacer cambios?

- **No toques el archivo .Rmd** (a menos que sepas programar)
- **Usa** los scripts que ya estan listos
- **Haz** una copia de seguridad antes de cambiar algo
- **Pide ayuda** a alguien con experiencia si tienes dudas

---

## Necesitas Ayuda?

**Este ejercicio esta disenado para ser facil de usar, pero si tienes problemas:**

1. **Revisa** esta guia paso a paso
2. **Verifica** que tengas todo instalado correctamente
3. **Intenta** los pasos mas simples primero
4. **No tengas miedo** de experimentar (siempre puedes empezar de nuevo)

**Recuerda:** Este ejercicio esta hecho para ayudar a los estudiantes a aprender matematicas de manera divertida y visual. Disfrutalo.

---

**Creado**: [FECHA_CREACION]
**Nivel**: Principiante-amigable con analisis exhaustivo
**Estado**: [ESTADO_ACTUAL]
**Actualizado**: [FECHA_ACTUALIZACION]
**Documentacion**: [NIVEL_DOCUMENTACION]

---

## Mejores Practicas y Consejos para Principiantes

### Lista de Verificacion Antes de Usar

**Para Profesores:**

- [ ] Verificar que RStudio este instalado y funcionando
- [ ] Confirmar que los paquetes R necesarios esten instalados
- [ ] Probar con `SemilleroUnico_v2.R` antes de usar en clase
- [ ] Revisar la carpeta `salida/` para confirmar que se generan archivos
- [ ] Preparar explicacion del concepto de "[CONCEPTO_CLAVE]"

**Para Tecnicos:**

- [ ] Validar que Python este correctamente configurado con reticulate
- [ ] Verificar que [HERRAMIENTA_GRAFICO] genere graficos sin errores
- [ ] Confirmar que las [NUMERO] paletas de colores funcionen correctamente
- [ ] Probar generacion en multiples formatos (PDF, HTML, Word, Moodle)
- [ ] Ejecutar pruebas unitarias para validar diversidad de versiones

### Errores Comunes y Soluciones

**Error: "No se puede encontrar Python"**
```r
# Solucion: Configurar Python manualmente
use_python("/usr/bin/python3", required = TRUE)  # Linux/Mac
use_python("C:/Python39/python.exe", required = TRUE)  # Windows
```

**Error: "[HERRAMIENTA_GRAFICO] no encontrado"**
```bash
# Solucion: Instalar [HERRAMIENTA_GRAFICO]
pip install [HERRAMIENTA_GRAFICO] [DEPENDENCIAS]
```

**Error: "Los graficos no se muestran"**
```r
# Solucion: Verificar configuracion de dispositivos
options(device = "png")
```

### Consejos de Uso Pedagogico

**Para Maximizar el Aprendizaje:**

1. **Antes del ejercicio**: Explicar [CONCEPTO_PREVIO_NECESARIO]
2. **Durante el ejercicio**: Permitir que los estudiantes discutan en grupos pequenos
3. **Despues del ejercicio**: Revisar la solucion paso a paso, enfatizando [ASPECTO_CLAVE]
4. **Extension**: Conectar con ejemplos de la vida real ([EJEMPLOS_VIDA_REAL])

**Preguntas Guia para Estudiantes:**

- "[PREGUNTA_GUIA_1]"
- "[PREGUNTA_GUIA_2]"
- "[PREGUNTA_GUIA_3]"
- "[PREGUNTA_GUIA_4]"

### Personalizacion Segura

**Cambios que SI puedes hacer:**

- Modificar el numero de versiones generadas en los scripts
- Cambiar los nombres de archivos de salida
- Ajustar el tamano de las imagenes para diferentes usos
- Traducir comentarios a otros idiomas

**Cambios que NO debes hacer (sin experiencia):**

- Modificar el archivo .Rmd principal
- Cambiar la logica de generacion de datos aleatorios
- Alterar las validaciones matematicas
- Modificar el codigo [LENGUAJE_GRAFICO] de graficos

### Metricas de Exito

**Indicadores de que el ejercicio funciona bien:**

- Se generan al menos 300 versiones unicas en las pruebas
- Los graficos se ven claros y profesionales
- [VALIDACION_MATEMATICA_ESPECIFICA]
- Los estudiantes pueden explicar [CONCEPTO_OBJETIVO]
- Los distractores generan discusion y reflexion

### Extensiones Posibles

**Para usuarios avanzados:**

- Agregar mas contextos ([CONTEXTOS_ADICIONALES])
- Incluir [VARIACIONES_AVANZADAS]
- Desarrollar versiones para diferentes niveles educativos
- Crear ejercicios complementarios sobre el mismo concepto

---

# ANALISIS EXHAUSTIVO DEL CODIGO POR BLOQUES

> **Para novatos:** Esta seccion explica como el codigo genera automaticamente datos diferentes cada vez. Es como tener una maquina que inventa numeros y contextos realistas.

## Bloque 1: Configuracion YAML (Lineas [RANGO_LINEAS])

```yaml
---
output:
  html_document: default
  word_document: default
  pdf_document: default
icfes:
  competencia:
    - [COMPETENCIA_VALOR]
  nivel_dificultad: [NIVEL]
  contenido:
    categoria: [CATEGORIA]
    tipo: [TIPO]
  contexto: [CONTEXTO]
  eje_axial: [EJE]
  componente: [COMPONENTE]
---
```

**Que hace este bloque?**

- **Define formatos de salida**: El ejercicio puede generarse como HTML (web), Word (documento) o PDF (imprimible)
- **Establece metadatos ICFES**: Especifica que evalua [COMPETENCIA] de nivel [NIVEL]
- **Configura competencias**: Indica que es un ejercicio de [CATEGORIA] en contexto [CONTEXTO]

> **Para principiantes:** Piensa en esto como la "etiqueta" del ejercicio que le dice al sistema que tipo de pregunta es y como debe generarla.

## Bloque 2: Configuracion del Entorno (Lineas [RANGO_LINEAS])

```r
```{r setup, include=FALSE}
# Configuracion para todos los formatos de salida
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

library(exams)      # Para generar ejercicios
library(reticulate) # Para usar Python dentro de R
library(digest)     # Para crear identificadores unicos
library(testthat)   # Para pruebas automaticas
library(knitr)      # Para generar documentos
library(stringr)    # Para manipular texto

typ <- match_exams_device()
options(scipen = 999)  # Evitar notacion cientifica
```

**Que hace este bloque?**
- **Configura el entorno**: Establece configuraciones para que todo funcione correctamente
- **Carga bibliotecas**: Importa las herramientas necesarias para generar ejercicios
- **Configura LaTeX**: Prepara el sistema para generar PDFs con formulas matematicas
- **Configura Python**: Permite usar Python para crear graficos avanzados

> **[META] Analogia simple:** Es como preparar una cocina antes de cocinar: encender el horno, sacar los utensilios, y tener todos los ingredientes listos.

## Bloque 3: Generacion de Datos Aleatorios (Lineas [RANGO_LINEAS])

```r
```{r data_generation, echo=FALSE, results="hide"}
# Funcion principal de generacion de datos para competencia [COMPETENCIA]
generar_datos <- function() {
  # Contextos aleatorios ampliados para mayor diversidad
  contextos <- list(
    list([PARAMETROS_CONTEXTO_1]),
    list([PARAMETROS_CONTEXTO_2]),
    # ... [NUMERO] contextos mas
  )

  contexto_sel <- sample(contextos, 1)[[1]]

  # Aleatorizar [PARAMETROS_PRINCIPALES]
  [LOGICA_GENERACION_ALEATORIA]
```

**Que hace este bloque?**

- **Crea contextos aleatorios**: Selecciona entre [NUMERO] tipos diferentes de [CONTEXTOS_DISPONIBLES]
- **Genera [PARAMETROS] realistas**: [DESCRIPCION_PARAMETROS]
- **Mantiene coherencia matematica**: Asegura que los numeros sean realistas y validos

> **Ejemplo practico:** [EJEMPLO_CONCRETO_GENERACION]

## Bloque 4: Pruebas Automaticas (Lineas [RANGO_LINEAS])

```r
# Verificar diversidad de versiones (solo en modo testing)
if(exists("testing_mode") && testing_mode) {
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
```

**Que hace este bloque?**

- **Verifica diversidad**: Genera 1000 versiones y confirma que al menos 300 sean unicas
- **Valida matematicas**: Verifica que [VALIDACIONES_ESPECIFICAS]
- **Prueba coherencia**: Asegura que [COHERENCIAS_ESPECIFICAS]

> **Para principiantes:** Es como un inspector de calidad que verifica que la maquina este funcionando correctamente antes de usarla.

## Bloque 5: Generacion de [ELEMENTO_VISUAL] con [HERRAMIENTA] (Lineas [RANGO_LINEAS])

```[LENGUAJE_CODIGO]
# [DESCRIPCION_SISTEMA_VISUAL]
[CODIGO_EJEMPLO_VISUAL]

# Seleccionar [ELEMENTO_ALEATORIO]
[LOGICA_SELECCION]

# Crear [TIPO_GRAFICO]
[CODIGO_CREACION_GRAFICO]

# [MEJORAS_VISUALES_ESPECIFICAS]
[CODIGO_MEJORAS]
```

**Que hace este bloque?**

- **Usa [HERRAMIENTA] para [PROPOSITO]**: [DESCRIPCION_HERRAMIENTA]
- **[NUMERO] [ELEMENTOS_VISUALES]**: [DESCRIPCION_ELEMENTOS]
- **Mejoras visuales**: [DESCRIPCION_MEJORAS]
- **Configuracion profesional**: [DESCRIPCION_CONFIGURACION]

> **[DISENO] Analogia artistica:** [ANALOGIA_VISUAL]

## Bloque 6: Generacion de Distractores Inteligentes (Lineas [RANGO_LINEAS])

```r
```{r generar_distractores, echo=FALSE, results="hide"}
# SISTEMA AVANZADO DE DISTRACTORES para competencia [COMPETENCIA]
permitir_conceptos_duplicados <- sample(c(TRUE, FALSE), 1, prob = c(0.3, 0.7))

# DISTRACTOR 1: [TIPO_ERROR_1]
[CODIGO_DISTRACTOR_1]

# DISTRACTOR 2: [TIPO_ERROR_2]
[CODIGO_DISTRACTOR_2]

# DISTRACTOR 3: [TIPO_ERROR_3]
[CODIGO_DISTRACTOR_3]
```

**Que hace este bloque?**

- **Crea distractores inteligentes**: Genera respuestas incorrectas que reflejan errores conceptuales comunes
- **Adapta segun contexto**: Los distractores cambian segun [PARAMETROS_CONTEXTO]
- **Simula errores reales**: Incluye confusiones tipicas entre [CONCEPTOS_CONFUNDIDOS]
- **Mezcla aleatoriamente**: Presenta las opciones en orden aleatorio para evitar patrones

> **[META] Ejemplo de distractor:** [EJEMPLO_DISTRACTOR_CONCRETO]

---

## [BLOQUE] **CASOS DE USO PRACTICOS**

### **[DOCUMENTACION] Caso 1: Profesor de Matematicas**

#### **Situacion:**
"[DESCRIPCION_SITUACION_PROFESOR]"

#### **Solucion paso a paso:**

1. **Abrir** `SemilleroUnico_v2.R`
2. **Cambiar** `numpreg <- [NUMERO]`
3. **Cambiar** `copias <- [NUMERO]`
4. **Ejecutar** el script
5. **Usar** los archivos PDF de la carpeta "salida"
6. **[ACCION_FINAL]**

#### **Resultado:**

- [RESULTADO_1]
- [RESULTADO_2]
- [RESULTADO_3]
- [RESULTADO_4]

### **[DOCUMENTACION] Caso 2: Coordinador de Moodle**

#### **Situacion:**
"[DESCRIPCION_SITUACION_MOODLE]"

#### **Solucion paso a paso:**

1. **Abrir** `SemilleroMoodle_v2.R`
2. **Verificar** que dice `copias <- [NUMERO]`
3. **Ejecutar** el script
4. **Ir** a la carpeta "salida"
5. **Encontrar** el archivo `.xml`
6. **Subir** a Moodle usando "Importar preguntas"

#### **Resultado:**

- [RESULTADO_MOODLE_1]
- [RESULTADO_MOODLE_2]
- [RESULTADO_MOODLE_3]

### **[DOCUMENTACION] Caso 3: Estudiante Curioso**

#### **Situacion:**
"[DESCRIPCION_SITUACION_ESTUDIANTE]"

#### **Solucion paso a paso:**

1. **Abrir** navegador web
2. **Buscar** archivo `[NOMBRE_ARCHIVO].html`
3. **Hacer doble clic** para abrirlo
4. **Ver** el ejercicio completo
5. **Intentar** resolverlo
6. **Leer** la explicacion

#### **Resultado:**

- [RESULTADO_ESTUDIANTE_1]
- [RESULTADO_ESTUDIANTE_2]
- [RESULTADO_ESTUDIANTE_3]

---

## [BLOQUE] **PREGUNTAS FRECUENTES (FAQ)**

### **[PREGUNTA] Puedo usar esto sin saber programar?**
**SI** - Los scripts estan listos. Solo presiona "Source" y listo.

### **[PREGUNTA] Necesito instalar algo especial?**

**[LISTA] Necesitas:**

- R (gratis)
- RStudio (gratis)
- Python se instala automaticamente

### **[PREGUNTA] Cada ejercicio es realmente diferente?**
**SI** - Cambian [ELEMENTOS_QUE_CAMBIAN]. Hay mas de 300 combinaciones posibles.

### **[PREGUNTA] Puedo modificar las preguntas?**
**[ALERTA] CUIDADO** - Como principiante, mejor usa los scripts tal como estan. Modificar requiere conocimiento avanzado.

### **[PREGUNTA] Funciona en cualquier computadora?**
**SI** - Windows, Mac, Linux. Solo necesitas R y RStudio.

### **[PREGUNTA] Los estudiantes pueden hacer trampa?**
**[SEGURO] DIFICIL** - Cada ejercicio es diferente, asi que no pueden copiarse facilmente.

### **[PREGUNTA] Cuanto tiempo toma generar ejercicios?**
**[TIEMPO] RAPIDO** - Entre 30 segundos y 2 minutos, dependiendo de cuantos generes.

### **[PREGUNTA] Puedo usar esto para otros temas de matematicas?**
**[DOCUMENTACION] ESTE EJERCICIO** - Solo para [TEMA_ESPECIFICO]. Pero el sistema R-exams puede hacer otros temas.

### **[PREGUNTA] Es gratis?**
**[DINERO] TOTALMENTE GRATIS** - R, RStudio, Python, todo es software libre.

### **[PREGUNTA] Necesito internet?**
**[WEB] SOLO PARA INSTALAR** - Una vez instalado, funciona sin internet.

---

## [BLOQUE] **GLOSARIO DE TERMINOS (Para que no te pierdas)**

### **[MANUAL] Terminos Tecnicos Explicados Simplemente**

#### **R**

- **Que es**: Lenguaje de programacion para estadistica
- **Para que**: Hacer calculos y graficos matematicos
- **Analogia**: Como una calculadora super poderosa

#### **RStudio**

- **Que es**: Programa para usar R mas facilmente
- **Para que**: Interfaz amigable para R
- **Analogia**: Como Word es para escribir, RStudio es para R

#### **Python**

- **Que es**: Otro lenguaje de programacion
- **Para que**: En este ejercicio, hacer graficos bonitos
- **Analogia**: Como un artista que dibuja los graficos

#### **R-exams**

- **Que es**: Herramienta especial para crear examenes
- **Para que**: Convertir ejercicios en diferentes formatos
- **Analogia**: Como una fotocopiadora que hace diferentes tipos de copias

#### **Markdown (.Rmd)**

- **Que es**: Formato que mezcla texto y codigo
- **Para que**: Escribir ejercicios que se ven bonitos
- **Analogia**: Como un documento de Word que puede hacer calculos

#### **PDF**

- **Que es**: Formato de documento que se ve igual en todas partes
- **Para que**: Imprimir ejercicios
- **Analogia**: Como una foto del ejercicio

#### **HTML**

- **Que es**: Formato de paginas web
- **Para que**: Ver ejercicios en navegador
- **Analogia**: Como una pagina web del ejercicio

#### **XML**

- **Que es**: Formato especial para intercambiar informacion
- **Para que**: Subir ejercicios a Moodle
- **Analogia**: Como un idioma que entiende Moodle

#### **NOPS**

- **Que es**: Formato para examenes escaneables
- **Para que**: Calificacion automatica con escaner
- **Analogia**: Como las hojas de respuesta del ICFES

#### **Script**

- **Que es**: Archivo con instrucciones para la computadora
- **Para que**: Automatizar tareas
- **Analogia**: Como una receta que sigue la computadora

#### **Libreria/Paquete**

- **Que es**: Conjunto de herramientas adicionales
- **Para que**: Agregar funciones especiales a R
- **Analogia**: Como apps que instalas en tu telefono

---

## **INSTRUCCIONES PARA USAR ESTA PLANTILLA**

### **Como Personalizar Esta Plantilla:**

1. **Buscar y reemplazar** todos los marcadores `[TEXTO_EN_MAYUSCULAS]` con la informacion especifica del ejercicio
2. **Completar las secciones** con detalles tecnicos del ejercicio particular
3. **Adaptar los ejemplos** a la tematica especifica del ejercicio
4. **Verificar coherencia** entre todas las secciones
5. **Probar** que todas las instrucciones funcionen correctamente

### **Marcadores Principales a Reemplazar:**

- `[NOMBRE_EJERCICIO]` - Nombre descriptivo del ejercicio
- `[COMPETENCIA_ICFES]` - Competencia ICFES evaluada
- `[CONCEPTO_PRINCIPAL]` - Concepto matematico principal
- `[CONTEXTO_APLICACION]` - Contexto de aplicacion del ejercicio
- `[HERRAMIENTA_GRAFICO]` - Herramienta usada para graficos (matplotlib, ggplot2, etc.)
- `[NUMERO]` - Numeros especificos (paletas, versiones, etc.)
- `[ELEMENTO_VISUAL_PRINCIPAL]` - Tipo de elemento visual principal

### **Secciones que Requieren Atencion Especial:**

- **Metadatos ICFES**: Deben coincidir exactamente con el archivo .Rmd
- **Bloques de codigo**: Adaptar a la estructura real del ejercicio
- **Casos de uso**: Personalizar segun el contexto educativo especifico
- **Validaciones**: Incluir las validaciones matematicas especificas del ejercicio

---

**[FECHA] Creado**: [FECHA_CREACION]
**[META] Nivel**: Plantilla para documentacion exhaustiva
**Estado**: Lista para personalizar
**[ACTUALIZADO] Actualizado**: [FECHA_ACTUALIZACION]
**[DOCUMENTACION] Documentacion**: Plantilla completa para walkthroughs de ejercicios R-exams
