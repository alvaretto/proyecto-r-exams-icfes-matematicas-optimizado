---
output:
  html_document: default
  pdf_document: default
---
# Walkthrough Exhaustivo: 

## Ejercicio de Empaques Tetra Pak - Argumentacion Matematica

> **PARA NOVATOS:** Este documento explica paso a paso como funciona un ejercicio avanzado de matematicas que se genera automaticamente con cientos de versiones diferentes. No te preocupes si no entiendes todo al principio.

## Descripcion General del Proyecto

> **Que es esto?** Imagina que tienes un profesor que puede crear infinitas versiones del mismo problema de matematicas, cada una con numeros y contextos diferentes pero evaluando la misma habilidad de **argumentacion matematica**. Eso es exactamente lo que hace este proyecto.

Este es un **ejercicio de matematicas avanzado** que ensena sobre porcentajes, proporciones y escalamiento usando empaques de leche (Tetra Pak). 

**Que aprenderan los estudiantes?**

- Que los porcentajes no cambian cuando algo se hace mas pequeno o mas grande (invariancia bajo escalamiento)
- A analizar si una afirmacion matematica es correcta o incorrecta usando argumentacion logica
- A distinguir entre cantidades absolutas y proporciones relativas
- A pensar de manera critica sobre problemas matematicos complejos

**Ejemplo conceptual:** Si una pizza tiene 50% queso y 50% masa, una pizza mas pequena seguira teniendo 50% queso y 50% masa. Los porcentajes no cambian aunque la pizza sea mas pequena.

## Competencia ICFES Evaluada

> **Para novatos:** ICFES es el examen de Estado colombiano. Cada pregunta evalua una "competencia" especifica (una habilidad matematica). Aqui te explicamos que evalua este ejercicio:

**Competencia:** Argumentacion *(Justificar por que una afirmacion matematica es verdadera o falsa)*
**Nivel:** 3 (Avanzado) *(Requiere razonamiento matematico profundo)*
**Componente:** Aleatorio (Estadistica) *(Trabaja con datos, porcentajes y proporciones)*
**Contexto:** Comunitario *(Situaciones de la vida real, como empaques de alimentos)*
**Eje Axial:** Eje 4 *(Clasificacion tecnica ICFES)*

> **Ejemplo practico:** El ejercicio presenta un grafico circular con la composicion de un empaque Tetra Pak y una afirmacion falsa: "Si reducimos el empaque a la mitad, los porcentajes tambien se reducen a la mitad". El estudiante debe identificar por que esta afirmacion es incorrecta.

### Metadatos ICFES Completos
```yaml
icfes:
  competencia:
    - argumentacion                    # Justificar afirmaciones matematicas
  nivel_dificultad: 3                 # Avanzado (1-4 escala)
  contenido:
    categoria: estadistica             # Area matematica
    tipo: generico                     # Aplicable a multiples contextos
  contexto: comunitario               # Situaciones de la vida real
  eje_axial: eje4                     # Clasificacion ICFES
  componente: aleatorio               # Pensamiento estadistico
```

## Archivos del Proyecto

### Archivo Principal:
- `empaques_tetra_pak_argumentacion_n3_v1.Rmd` - El ejercicio completo (no lo abras si eres principiante)

### Scripts Faciles de Usar:

- `SemilleroUnico_v2.R` - **RECOMENDADO** Genera el ejercicio en diferentes formatos
- `SemilleroMoodle_v2.R` - Genera ejercicios para Moodle (plataforma educativa)

### Carpeta de Resultados:

- `salida/` - Aqui apareceran los ejercicios generados

### Otros archivos:

- `walkthrough.md` - Esta guia que estas leyendo
- `empaques_tetra_pak_argumentacion_n3_v1.html` - Vista previa del ejercicio
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
2. **Abre el archivo** `empaques_tetra_pak_argumentacion_n3_v1.html`
3. **Ya puedes ver como se ve el ejercicio**

## Que hace especial a este ejercicio

### 1. Colores Bonitos y Diferentes

- Cada vez que generes el ejercicio, tendra **colores diferentes**
- Hay **25 combinaciones** de colores distintas
- Los colores siempre se ven bien juntos
- Nunca te aburriras de ver el mismo color

### 2. Numeros Faciles de Leer

- Los porcentajes en el grafico tienen **fondo negro**
- El texto es **blanco y grande**
- Se puede leer perfectamente en cualquier color
- No mas numeros que no se ven

### 3. Ejercicios Siempre Diferentes

- Cada vez que lo uses, el ejercicio cambia
- Diferentes materiales (papel, plastico, aluminio)
- Diferentes porcentajes (pero siempre suman 100%)
- Diferentes tamanos de empaque
- Perfecto para hacer muchos examenes

## Como esta hecho el ejercicio (Para curiosos)

### Informacion Tecnica (ICFES):

- **Tipo:** Argumentacion matematica
- **Dificultad:** Nivel 3 (avanzado)
- **Tema:** Estadistica y porcentajes
- **Contexto:** Situaciones de la vida real

### **[BLOQUE] Partes del Ejercicio:**

#### **[GRAFICO] 1. El Grafico Circular**

- Se crea automaticamente con **Python**
- Muestra los porcentajes de cada material
- Tiene colores bonitos y numeros legibles
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

- Cambia los materiales (papel, plastico, etc.)
- Cambia los porcentajes
- Cambia el tamano del empaque
- Cada ejercicio es unico!

## [ESTADO] **Como saber si funciona bien?**

### **[ANALISIS] Senales de que Todo Esta Bien:**

1. **No hay errores rojos** en RStudio
2. **Se crean archivos** en la carpeta "salida"
3. **El grafico se ve bonito** con colores y numeros legibles
4. **Los porcentajes suman 100%** (siempre)
5. **Cada vez que lo ejecutes** se ven colores diferentes

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

- **Porcentajes:** Entender que representan partes de un todo
- **Proporciones:** Saber que no cambian cuando algo se hace mas grande o pequeno
- **Razonamiento logico:** Analizar si una afirmacion es verdadera o falsa
- **Pensamiento critico:** Identificar errores en argumentos matematicos

### **[INTELIGENTE] Habilidades que Desarrollan:**

- **Argumentacion:** Explicar por que algo es correcto o incorrecto
- **Analisis:** Examinar informacion y sacar conclusiones
- **Resolucion de problemas:** Aplicar matematicas a situaciones reales
- **Comunicacion matematica:** Expresar ideas matematicas claramente

### Por que es Importante

- **Vida real:** Los porcentajes estan en todas partes (descuentos, estadisticas, etc.)
- **Pensamiento cientifico:** Aprender a cuestionar y verificar afirmaciones
- **Preparacion ICFES:** Desarrolla competencias evaluadas en las pruebas
- **Base para matematicas avanzadas:** Fundamentos solidos para temas mas complejos

## Que hacer ahora

### Si eres profesor:

1. **Ejecuta** `SemilleroUnico_v2.R` para crear ejercicios
2. **Usa** los archivos PDF para imprimir examenes
3. **Sube** los archivos XML a Moodle si usas esa plataforma
4. **Explica** a los estudiantes el concepto de porcentajes invariantes

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

---

## Mejores Practicas y Consejos para Principiantes

### Lista de Verificacion Antes de Usar

**Para Profesores:**

- [ ] Verificar que RStudio este instalado y funcionando
- [ ] Confirmar que los paquetes R necesarios esten instalados
- [ ] Probar con `SemilleroUnico_v2.R` antes de usar en clase
- [ ] Revisar la carpeta `salida/` para confirmar que se generan archivos
- [ ] Preparar explicacion del concepto de "invariancia bajo escalamiento"

**Para Tecnicos:**

- [ ] Validar que Python este correctamente configurado con reticulate
- [ ] Verificar que matplotlib genere graficos sin errores
- [ ] Confirmar que las 25 paletas de colores funcionen correctamente
- [ ] Probar generacion en multiples formatos (PDF, HTML, Word, Moodle)
- [ ] Ejecutar pruebas unitarias para validar diversidad de versiones

### Errores Comunes y Soluciones

**Error: "No se puede encontrar Python"**
```r
# Solucion: Configurar Python manualmente
use_python("/usr/bin/python3", required = TRUE)  # Linux/Mac
use_python("C:/Python39/python.exe", required = TRUE)  # Windows
```

**Error: "matplotlib no encontrado"**
```bash
# Solucion: Instalar matplotlib
pip install matplotlib numpy
```

**Error: "Los graficos no se muestran"**
```r
# Solucion: Verificar configuracion de dispositivos
options(device = "png")
```

### Consejos de Uso Pedagogico

**Para Maximizar el Aprendizaje:**

1. **Antes del ejercicio**: Explicar que son los porcentajes y por que no cambian con el tamano
2. **Durante el ejercicio**: Permitir que los estudiantes discutan en grupos pequenos
3. **Despues del ejercicio**: Revisar la solucion paso a paso, enfatizando el razonamiento
4. **Extension**: Conectar con ejemplos de la vida real (recetas, mezclas, etc.)

**Preguntas Guia para Estudiantes:**

- "Que significa que algo sea un porcentaje?"
- "Si tienes una pizza y la cortas por la mitad, cambia el porcentaje de queso?"
- "Por que crees que la afirmacion es falsa?"
- "Puedes explicar tu razonamiento a un companero?"

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
- Modificar el codigo Python de graficos

### Metricas de Exito

**Indicadores de que el ejercicio funciona bien:**

- Se generan al menos 300 versiones unicas en las pruebas
- Los graficos se ven claros y profesionales
- Los porcentajes siempre suman exactamente 100%
- Los estudiantes pueden explicar por que la afirmacion es falsa
- Los distractores generan discusion y reflexion

### Extensiones Posibles

**Para usuarios avanzados:**

- Agregar mas contextos (empaques de medicamentos, cosmeticos, etc.)
- Incluir factores de escalamiento mas complejos (raiz2, pi/2, etc.)
- Desarrollar versiones para diferentes niveles educativos
- Crear ejercicios complementarios sobre el mismo concepto

---

**Creado**: Junio 2025  
**Nivel**: Principiante-amigable con analisis exhaustivo  
**Estado**: Listo para usar con documentacion completa  
**Actualizado**: Con analisis detallado por bloques de codigo  
**Documentacion**: Exhaustiva para principiantes y usuarios avanzados

---

# ANALISIS EXHAUSTIVO DEL CODIGO POR BLOQUES

> **Para novatos:** Esta seccion explica como el codigo genera automaticamente datos diferentes cada vez. Es como tener una maquina que inventa numeros y contextos realistas.

## Bloque 1: Configuracion YAML (Lineas 1-15)

```yaml
---
output:
  html_document: default
  word_document: default
  pdf_document: default
icfes:
  competencia:
    - argumentacion
  nivel_dificultad: 3
  contenido:
    categoria: estadistica
    tipo: generico
  contexto: comunitario
  eje_axial: eje4
  componente: aleatorio
---
```

**Que hace este bloque?**

- **Define formatos de salida**: El ejercicio puede generarse como HTML (web), Word (documento) o PDF (imprimible)
- **Establece metadatos ICFES**: Especifica que evalua argumentacion matematica de nivel avanzado
- **Configura competencias**: Indica que es un ejercicio de estadistica en contexto comunitario

> **Para principiantes:** Piensa en esto como la "etiqueta" del ejercicio que le dice al sistema que tipo de pregunta es y como debe generarla.

## Bloque 2: Configuracion del Entorno (Lineas 17-63)**

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

## Bloque 3: Generacion de Datos Aleatorios (Lineas 65-149)**

```r
```{r data_generation, echo=FALSE, results="hide"}
# Funcion principal de generacion de datos para competencia ARGUMENTACION
generar_datos <- function() {
  # Contextos aleatorios ampliados para mayor diversidad
  contextos <- list(
    list(producto = "empaques de Tetra Pak", empresa = "industria alimentaria", 
         material_principal = "carton", uso = "conservacion de alimentos"),
    list(producto = "envases multicapa", empresa = "industria de bebidas", 
         material_principal = "polimero", uso = "proteccion de liquidos"),
    # ... 6 contextos mas
  )
  
  contexto_sel <- sample(contextos, 1)[[1]]
  
  # Aleatorizar porcentajes de materiales (manteniendo suma = 100%)
  porcentaje_principal <- sample(60:80, 1)
  porcentaje_restante <- 100 - porcentaje_principal
  porcentaje_secundario <- sample(15:min(30, porcentaje_restante-5), 1)
  porcentaje_terciario <- porcentaje_restante - porcentaje_secundario
```

**Que hace este bloque?**

- **Crea contextos aleatorios**: Selecciona entre 8 tipos diferentes de empaques (Tetra Pak, envases multicapa, etc.)
- **Genera porcentajes realistas**: Crea tres porcentajes que siempre suman 100% (ej: 70%, 20%, 10%)
- **Mantiene coherencia matematica**: Asegura que los numeros sean realistas y validos

> **Ejemplo practico:** Una vez puede generar "empaques de Tetra Pak con 75% carton, 18% polietileno, 7% aluminio" y otra vez "envases multicapa con 68% polimero, 22% carton, 10% aluminio".

## Bloque 4: Pruebas Automaticas (Lineas 150-185)**

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
- **Valida matematicas**: Verifica que los porcentajes siempre sumen 100%
- **Prueba coherencia**: Asegura que los factores de escalamiento sean correctos

> **Para principiantes:** Es como un inspector de calidad que verifica que la maquina este funcionando correctamente antes de usarla.

## Bloque 5: Generacion de Graficos con Python (Lineas 187-317)**

```python
# Sistema avanzado de paletas de colores aleatorias
paletas_colores = [
    # Paleta 1: Colores vibrantes modernos
    ['#FF6B6B', '#4ECDC4', '#45B7D1', '#96CEB4', '#FFEAA7'],
    # Paleta 2: Tonos tierra naturales
    ['#8B4513', '#CD853F', '#DEB887', '#F4A460', '#D2691E'],
    # ... 23 paletas mas
]

# Seleccionar una paleta aleatoria
paleta_seleccionada = random.choice(paletas_colores)

# Crear grafico circular
plt.figure(figsize=(8, 8))
wedges, texts, autotexts = plt.pie(porcentajes, labels=materiales, autopct='%1.0f%%',
                                   colors=colores_seleccionados, startangle=90,
                                   textprops={'fontsize': 12, 'weight': 'bold'})

# Agregar rectangulos negros como fondo de las etiquetas de porcentaje
for autotext in autotexts:
    # Calcular posicion y tamano del rectangulo
    x, y = autotext.get_position()
    ancho_rect = len(texto) * 0.090
    alto_rect = 0.12
    
    # Agregar rectangulo negro como fondo
    rect = plt.Rectangle((rect_x, rect_y), ancho_rect, alto_rect, 
                        facecolor='black', alpha=0.8, zorder=1)
    ax.add_patch(rect)
```

**Que hace este bloque?**

- **Usa Python para graficos**: Aprovecha matplotlib para crear graficos circulares profesionales
- **25 paletas de colores**: Selecciona aleatoriamente entre paletas vibrantes, naturales, oceanicas, etc.
- **Mejoras visuales**: Agrega rectangulos negros detras de los porcentajes para mejor legibilidad
- **Configuracion profesional**: Ajusta tamanos, fuentes y posicionamiento para maxima claridad

> **[DISENO] Analogia artistica:** Es como tener un artista que pinta el mismo grafico de 25 maneras diferentes, cada una con su propia paleta de colores unica.

## Bloque 6: Generacion de Distractores Inteligentes (Lineas 321-399)**

```r
```{r generar_distractores, echo=FALSE, results="hide"}
# SISTEMA AVANZADO DE DISTRACTORES para competencia ARGUMENTACION
permitir_conceptos_duplicados <- sample(c(TRUE, FALSE), 1, prob = c(0.3, 0.7))

# DISTRACTOR 1: Confundir escalamiento lineal con volumetrico
afirmaciones_incorrectas <- c(afirmaciones_incorrectas,
  paste0("Los porcentajes se duplicarian al haber menos espacio vacio dentro del empaque"))

# DISTRACTOR 2: Confundir con conservacion de masa
afirmaciones_incorrectas <- c(afirmaciones_incorrectas,
  paste0("Los porcentajes se conservarian sin importar el tamano del empaque"))

# DISTRACTOR 3: Error sobre proporcionalidad
if(escala_reduccion == "la mitad") {
  afirmaciones_incorrectas <- c(afirmaciones_incorrectas,
    paste0("Los porcentajes se reducirian a la octava parte porque todas las caras se reducen a la mitad"))
}
```

**Que hace este bloque?**

- **Crea distractores inteligentes**: Genera respuestas incorrectas que reflejan errores conceptuales comunes
- **Adapta segun contexto**: Los distractores cambian segun el factor de reduccion (mitad, tercio, etc.)
- **Simula errores reales**: Incluye confusiones tipicas entre escalamiento lineal y volumetrico
- **Mezcla aleatoriamente**: Presenta las opciones en orden aleatorio para evitar patrones

> **[META] Ejemplo de distractor:** Si el empaque se reduce a la mitad, un distractor podria decir "Los porcentajes se reducen a la octava parte" (confundiendo reduccion lineal con volumetrica).

## Bloque 7: Generacion de la Pregunta (Lineas 400-441)**

```r
Question
========

Los `r contexto$producto` son elaborados con `r materiales[1]`, `r materiales[2]` y `r materiales[3]`, distribuidos en 6 capas, lo cual evita el contacto del alimento con el medio externo. La grafica muestra la distribucion porcentual aproximada de los materiales de un `r contexto$producto`:

```{r mostrar_grafico, echo=FALSE, results='asis', fig.align='center'}
# Detectar si se esta generando para Moodle
es_moodle <- (match_exams_call() %in% c("exams2moodle", "exams2qti12", "exams2qti21", "exams2openolat"))

# Mostrar el grafico generado con Python con ancho controlado
if(es_moodle) {
  cat("![](grafico_composicion.png){width=40%}")  # Mas pequeno para Moodle
} else {
  cat("![](grafico_composicion.png){width=60%}")  # Tamano normal para PDF/Word
}
```

Una persona afirma que los porcentajes de los materiales en el empaque son validos para un empaque de `r empaque_original`, pero que si se construye con la misma tecnica un empaque de `r escala_reduccion`, reduciendo las dimensiones a `r escala_reduccion`, entonces los porcentajes tambien se reducen a `r escala_reduccion`.

**Esta afirmacion es falsa porque:**

Answerlist
----------
- `r opciones_mezcladas[1]`
- `r opciones_mezcladas[2]`
- `r opciones_mezcladas[3]`
- `r opciones_mezcladas[4]`
```

**Que hace este bloque?**

- **Construye la pregunta dinamicamente**: Usa las variables generadas para crear el texto
- **Adapta el grafico segun formato**: Tamano diferente para Moodle vs PDF/Word
- **Presenta la afirmacion falsa**: Establece el escenario que el estudiante debe analizar
- **Lista las opciones**: Presenta las 4 opciones de respuesta en orden aleatorio

> **[META] Ejemplo dinamico:** Si se genero "envases multicapa" con "polimero, carton, aluminio" y escala "la mitad", la pregunta se adaptara automaticamente a estos valores.

## Bloque 8: Solucion Detallada (Lineas 442-513)**

```r
Solution
========

Para resolver este problema de **argumentacion matematica**, debemos analizar la afirmacion falsa y determinar por que es incorrecta segun los principios de escalamiento y proporcionalidad.

**Analisis de la afirmacion falsa:**

La persona afirma que al reducir las dimensiones del empaque a `r escala_reduccion`, los porcentajes de materiales tambien se reducen a `r escala_reduccion`. Esta afirmacion es **matematicamente incorrecta**.

**Demostracion matematica:**

Si el empaque original tiene volumen V y el reducido tiene volumen `r round(factor_volumen, 4)`V:

- `r materiales[1]` original: `r porcentajes[1]`% de V
- `r materiales[1]` reducido: `r porcentajes[1]`% de `r round(factor_volumen, 4)`V = `r round(porcentajes[1] * factor_volumen, 2)`% de V

Pero como porcentaje del nuevo volumen total: `r round(porcentajes[1] * factor_volumen, 2)`% de V / `r round(factor_volumen, 4)`V = **`r porcentajes[1]`%**

**Conclusion:**

La afirmacion correcta es: **"`r afirmacion_correcta`"**

Los porcentajes son **invariantes bajo escalamiento uniforme** porque representan proporciones relativas, no cantidades absolutas.
```

**Que hace este bloque?**

- **Explica el concepto**: Detalla por que la afirmacion es falsa usando principios matematicos
- **Proporciona demostracion**: Muestra calculos especificos con los valores generados
- **Usa terminologia tecnica**: Introduce conceptos como "invariantes bajo escalamiento uniforme"
- **Conecta con la respuesta correcta**: Vincula la explicacion con la opcion correcta seleccionada

> **[PEDAGOGICO] Valor educativo:** Esta seccion ensena no solo la respuesta correcta, sino el razonamiento matematico completo detras de ella.

## Caracteristicas Avanzadas del Sistema de Aleatorizacion

### Sistema Anti-Patron Implementado

```r
# DECISION ALEATORIA: Permitir valores duplicados con justificaciones diferentes?
# 30% de probabilidad de generar opciones con mismo concepto pero diferentes explicaciones
permitir_conceptos_duplicados <- sample(c(TRUE, FALSE), 1, prob = c(0.3, 0.7))
```

**Por que es importante esto?**

- **Evita memorizacion**: Los estudiantes no pueden memorizar patrones de respuestas
- **Fuerza comprension**: Deben entender el concepto, no solo reconocer la respuesta
- **Simula examenes reales**: Los examenes ICFES tienen variaciones impredecibles

### Sistema de Paletas de Colores Profesional

El ejercicio incluye **25 paletas de colores diferentes**:

1. **Colores vibrantes modernos**: Para captar atencion
2. **Tonos tierra naturales**: Para contextos ecologicos
3. **Colores oceanicos**: Para sensacion de frescura
4. **Tonos pasteles suaves**: Para reducir fatiga visual
5. **Colores corporativos elegantes**: Para contextos profesionales
6. **Y 20 paletas mas...**

> **[DISENO] Impacto visual:** Cada generacion del ejercicio se ve fresca y atractiva, manteniendo el interes del estudiante.

### Validaciones Matematicas Robustas

```r
# Verificar que los porcentajes sumen 100%
expect_equal(sum(datos_test$porcentajes), 100,
            info = "Los porcentajes deben sumar exactamente 100%")

# Verificar que el factor volumetrico sea correcto
expect_equal(datos_test$factor_volumen, datos_test$factor_lineal^3,
            info = "El factor volumetrico debe ser el cubo del factor lineal")

# Verificar rangos validos de porcentajes
expect_true(all(datos_test$porcentajes >= 5 & datos_test$porcentajes <= 80),
           info = "Los porcentajes deben estar en rangos realistas")
```

**Estas validaciones garantizan:**

- **Coherencia matematica**: Todos los calculos son correctos
- **Realismo**: Los porcentajes reflejan composiciones reales de empaques
- **Diversidad**: Se generan al menos 300 versiones unicas

### Pedagogia de Distractores Inteligentes

Los distractores estan disenados para capturar **errores conceptuales especificos**:

1. **Confusion dimensional**: Mezclar escalamiento lineal (1D) con volumetrico (3D)
2. **Error de conservacion**: No entender que las proporciones se mantienen
3. **Malinterpretacion de densidad**: Confundir densidad con composicion porcentual
4. **Error de superficie**: Aplicar conceptos de area a problemas de volumen

> **[META] Objetivo pedagogico:** Cada distractor ensena algo importante al estudiante, incluso si elige la respuesta incorrecta.

---

## Comparacion con Otros Ejercicios del Repositorio

### Nivel de Complejidad

| Ejercicio | Nivel ICFES | Parametros Aleatorios | Paletas de Color | Validaciones |
|-----------|-------------|----------------------|------------------|--------------|
| **Empaques Tetra Pak** | 3 (Avanzado) | 15+ | 25 | Robustas |
| Juegos Deportivos | 2 (Intermedio) | 12+ | 8 | Estandar |
| Grafico Circular Bienes | 2 (Intermedio) | 10+ | 5 | Basicas |

### Caracteristicas Unicas de Este Ejercicio

1. **Competencia de Argumentacion**: Unico en evaluar justificacion matematica
2. **Integracion Python-R**: Graficos mas sofisticados que otros ejercicios
3. **Sistema de rectangulos negros**: Innovacion visual para legibilidad
4. **Validaciones exhaustivas**: Mas pruebas automaticas que ejercicios similares

### Aplicabilidad Curricular

**Temas matematicos cubiertos:**

- Porcentajes y proporciones
- Escalamiento geometrico
- Invariancia matematica
- Argumentacion logica
- Analisis de afirmaciones

**Conexiones interdisciplinarias:**

- Quimica (composicion de materiales)
- Fisica (escalamiento y volumen)
- Ingenieria (diseno de empaques)
- Ciencias ambientales (materiales sostenibles)

---

# DOCUMENTACION EXHAUSTIVA POR BLOQUES (Para Principiantes)

## BLOQUE 1: Entendiendo el Archivo Principal (.Rmd)

### **Que es un archivo .Rmd?**

- **R Markdown**: Es como un documento de Word, pero que puede ejecutar codigo
- **Extension .Rmd**: Significa "R Markdown"
- **Contiene**: Texto normal + codigo de programacion + matematicas
- **Resultado**: Se convierte en ejercicios bonitos (PDF, HTML, etc.)

### **Por que NO debes abrirlo si eres principiante?**

- **Es muy tecnico**: Tiene codigo en R y Python mezclado
- **Puede confundirte**: Veras muchos simbolos raros
- **No es necesario**: Los scripts ya estan listos para usar
- **Riesgo**: Podrias cambiar algo sin querer

### **Que hace exactamente este archivo?**

1. **Genera datos aleatorios** (porcentajes, materiales, tamanos)
2. **Crea un grafico circular** con Python
3. **Escribe la pregunta** con los datos generados
4. **Crea 4 opciones** de respuesta (solo 1 correcta)
5. **Genera la explicacion** detallada de la solucion

---

## BLOQUE 2: Los Scripts Magicos (Los que SI debes usar)

### **[ARCHIVO] SemilleroUnico_v2.R - Tu Mejor Amigo**

#### **Que hace este script?**

- **Genera TODO**: PDF, Word, HTML, NOPS en una sola ejecucion
- **Es automatico**: Solo presionas un boton y esperas
- **Crea multiples formatos**: Para diferentes usos
- **Guarda todo**: En la carpeta "salida"

#### **Como funciona por dentro? (Explicacion simple)**
```r
# 1. Carga las herramientas necesarias
library(exams)  # La herramienta principal

# 2. Define que archivo usar
archivo_examen <- "empaques_tetra_pak_argumentacion_n3_v1.Rmd"

# 3. Configura cuantas copias hacer
copias <- 1  # Hace 1 copia de cada formato

# 4. Genera diferentes formatos automaticamente
# - HTML (para ver en navegador)
# - PDF (para imprimir)
# - Word (para editar)
# - NOPS (para escanear respuestas)
```

#### **Que significa cada parte?**

- **`library(exams)`**: "Cargar la caja de herramientas"
- **`copias <- 1`**: "Hacer 1 ejercicio de cada tipo"
- **`dir_salida <- "salida"`**: "Guardar todo en la carpeta 'salida'"
- **`exams2pdf(...)`**: "Crear version PDF"
- **`exams2html(...)`**: "Crear version HTML"

### **[ARCHIVO] SemilleroMoodle_v2.R - Para Plataformas Educativas**

#### **Que hace este script?**

- **Solo para Moodle**: Crea archivos XML especiales
- **Multiples versiones**: Hace 5 ejercicios diferentes
- **Facil de subir**: El archivo XML se sube directo a Moodle

#### **Cuando usarlo?**

- **Si tienes Moodle**: Tu escuela usa esta plataforma
- **Para examenes online**: Los estudiantes responden en computadora
- **Para calificacion automatica**: Moodle califica solo

---

## [BLOQUE] **BLOQUE 3: La Carpeta "salida" - Donde Aparece la Magia**

### **Que encontraras aqui?**

#### **[ARCHIVO] Archivos PDF**

- **Nombre**: `empaques_tetra_pak_argumentacion_n3_v1_1.pdf`
- **Para que**: Imprimir y dar a estudiantes
- **Contiene**: Pregunta + opciones (sin respuesta)
- **Ventaja**: Se ve profesional, facil de leer

#### **[ARCHIVO] Archivos Word (.docx)**

- **Nombre**: `empaques_tetra_pak_argumentacion_n3_v1_1.docx`
- **Para que**: Editar si necesitas cambiar algo
- **Contiene**: Todo el ejercicio editable
- **Ventaja**: Puedes modificar texto, agregar instrucciones

#### **[ARCHIVO] Archivos HTML**

- **Nombre**: `empaques_tetra_pak_argumentacion_n3_v1_semillero.html`
- **Para que**: Ver en navegador, proyectar en clase
- **Contiene**: Ejercicio interactivo con colores
- **Ventaja**: Se ve bonito, facil de proyectar

#### **[ARCHIVO] Archivos XML (Moodle)**

- **Nombre**: `empaques_tetra_pak_argumentacion_n3_v1_.xml`
- **Para que**: Subir a plataforma Moodle
- **Contiene**: Ejercicio en formato especial
- **Ventaja**: Calificacion automatica

#### **[ARCHIVO] Archivos NOPS**

- **Nombre**: `empaques_tetra_pak_argumentacion_n3_v1_nops_.rds`
- **Para que**: Examenes escaneables (como ICFES)
- **Contiene**: Formato especial para escanear
- **Ventaja**: Calificacion automatica con escaner

---

## [BLOQUE] **BLOQUE 4: El Grafico Circular - Corazon del Ejercicio**

### **Como se crea el grafico?**

#### **Paso 1: Generacion de Datos**
```
Materiales aleatorios: Papel, Polietileno, Aluminio
Porcentajes aleatorios: 75%, 20%, 5% (siempre suman 100%)
Colores aleatorios: Se elige 1 de 25 paletas diferentes
```

#### **Paso 2: Creacion Visual**

- **Python hace el grafico**: Usa matplotlib (herramienta de graficos)
- **Colores bonitos**: De la paleta seleccionada aleatoriamente
- **Numeros legibles**: Fondo negro, texto blanco y grande
- **Se guarda**: Como imagen PNG

#### **Paso 3: Integracion**

- **Se incluye**: En el ejercicio automaticamente
- **Se adapta**: Al tamano correcto para cada formato
- **Se optimiza**: Para verse bien en pantalla e impresion

### **Por que cambian los colores cada vez?**

- **25 paletas disponibles**: Desde colores vibrantes hasta pasteles
- **Seleccion aleatoria**: Cada ejecucion elige una diferente
- **Coherencia**: Los colores de cada paleta se ven bien juntos
- **Variedad**: Nunca te aburres del mismo aspecto

---

## [BLOQUE] **BLOQUE 5: Las Matematicas del Ejercicio (Explicacion Simple)**

### **Que concepto ensena?**

#### **La Idea Principal: Porcentajes Invariantes**

- **Concepto**: Los porcentajes no cambian cuando algo se hace mas grande o pequeno
- **Ejemplo**: Si una pizza tiene 50% queso, una pizza mas pequena sigue teniendo 50% queso
- **En empaques**: Si un empaque tiene 75% papel, uno mas pequeno sigue teniendo 75% papel

#### **Por que es importante?**

- **Error comun**: Estudiantes piensan que porcentajes cambian con el tamano
- **Aplicacion real**: Recetas, mezclas, composiciones quimicas
- **Pensamiento critico**: Analizar afirmaciones matematicas

### **Como funciona la pregunta?**

#### **Estructura del Problema:**

1. **Contexto**: Empaque Tetra Pak con 3 materiales
2. **Grafico**: Muestra porcentajes de cada material
3. **Afirmacion falsa**: "Si el empaque se hace mas pequeno, los porcentajes tambien se reducen"
4. **Pregunta**: Por que es falsa esta afirmacion?

#### **Las 4 Opciones de Respuesta:**

- **1 Correcta**: Explica que los porcentajes se mantienen iguales
- **3 Incorrectas**: Errores comunes que cometen estudiantes
  - Confundir cantidades absolutas con relativas
  - Pensar en densidad de materiales
  - Confundir superficie con volumen

---

## [BLOQUE] **BLOQUE 6: Solucion de Problemas Paso a Paso**

### **[ALERTA] Problema: "No se ejecuta el script"**

#### **Sintomas:**

- Errores rojos en RStudio
- No se crean archivos
- Mensajes de error

#### **Soluciones:**

1. **Verificar instalacion de R y RStudio**
   - Tienes R instalado? (version 4.0 o superior)
   - Tienes RStudio instalado?

2. **Instalar librerias necesarias**
   ```r
   install.packages("exams")
   install.packages("reticulate")
   ```

3. **Verificar Python**

   - El ejercicio necesita Python para los graficos
   - RStudio deberia encontrarlo automaticamente

### **[ALERTA] Problema: "Se ejecuta pero no veo archivos"**

#### **Sintomas:**

- No hay errores
- Pero no aparecen archivos en "salida"

#### **Soluciones:**

1. **Verificar carpeta "salida"**
   - Existe la carpeta?
   - Estas mirando en el lugar correcto?

2. **Permisos de escritura**

   - Puedes crear archivos en esa carpeta?
   - Intenta crear un archivo de texto manualmente

3. **Reiniciar RStudio**

   - Cierra y abre RStudio
   - Vuelve a ejecutar el script

### **[ALERTA] Problema: "El grafico no se ve bien"**

#### **Sintomas:**

- Grafico borroso
- Numeros no se leen
- Colores extranos

#### **Soluciones:**

1. **Verificar Python**
   - Esta instalado matplotlib?
   - Ejecuta: `pip install matplotlib`

2. **Actualizar librerias**
   ```r
   update.packages()
   ```

3. **Generar de nuevo**

   - A veces es solo un error temporal
   - Ejecuta el script otra vez

---

## [BLOQUE] **BLOQUE 7: Personalizacion Basica (Para Valientes Principiantes)**

### **[ALERTA] ADVERTENCIA IMPORTANTE**

- **Haz copia de seguridad**: Antes de cambiar CUALQUIER cosa
- **Cambia de a poco**: Un cambio a la vez
- **Prueba inmediatamente**: Despues de cada cambio

### **[HERRAMIENTAS] Cambios Seguros que Puedes Hacer**

#### **En SemilleroUnico_v2.R:**

##### **Cambiar numero de copias:**
```r
# Busca esta linea:
copias <- 1

# Cambiala por:
copias <- 5  # Para hacer 5 ejercicios diferentes
```

##### **Cambiar numero de preguntas:**
```r
# Busca esta linea:
numpreg <- 5

# Cambiala por:
numpreg <- 10  # Para hacer examen de 10 preguntas
```

##### **Cambiar nombre de institucion (en NOPS):**
```r
# Busca esta linea:
institution = "I. E. Pedacito de Cielo"

# Cambiala por:
institution = "Tu Colegio Aqui"
```

### **[NO] Cambios que NO Debes Hacer (Como Principiante)**

- **No toques el archivo .Rmd**: Es muy complejo
- **No cambies rutas de archivos**: Puedes romper todo
- **No modifiques codigo Python**: Esta integrado de manera compleja
- **No cambies configuraciones avanzadas**: Sin entender que hacen

---

## [BLOQUE] **BLOQUE 8: Casos de Uso Practicos**

### **[DOCUMENTACION] Caso 1: Profesor de Matematicas**

#### **Situacion:**
"Quiero crear un examen de 20 preguntas sobre porcentajes para mis 30 estudiantes"

#### **Solucion paso a paso:**

1. **Abrir** `SemilleroUnico_v2.R`
2. **Cambiar** `numpreg <- 20`
3. **Cambiar** `copias <- 30`
4. **Ejecutar** el script
5. **Usar** los archivos PDF de la carpeta "salida"
6. **Imprimir** y distribuir

#### **Resultado:**

- 30 examenes diferentes
- Cada uno con 20 preguntas
- Todos sobre el mismo concepto
- Listos para imprimir

### **[DOCUMENTACION] Caso 2: Coordinador de Moodle**

#### **Situacion:**

"Necesito subir ejercicios a la plataforma Moodle de mi institucion"

#### **Solucion paso a paso:**

1. **Abrir** `SemilleroMoodle_v2.R`
2. **Verificar** que dice `copias <- 5`
3. **Ejecutar** el script
4. **Ir** a la carpeta "salida"
5. **Encontrar** el archivo `.xml`
6. **Subir** a Moodle usando "Importar preguntas"

#### **Resultado:**

- 5 versiones del ejercicio en Moodle
- Calificacion automatica
- Feedback inmediato para estudiantes

### **[DOCUMENTACION] Caso 3: Estudiante Curioso**

#### **Situacion:**

"Quiero ver como se ve el ejercicio antes de que me lo pongan en el examen"

#### **Solucion paso a paso:**

1. **Abrir** navegador web
2. **Buscar** archivo `empaques_tetra_pak_argumentacion_n3_v1.html`
3. **Hacer doble clic** para abrirlo
4. **Ver** el ejercicio completo
5. **Intentar** resolverlo
6. **Leer** la explicacion

#### **Resultado:**

- Comprension del concepto
- Practica antes del examen
- Menos ansiedad en la prueba real

---

## [BLOQUE] **BLOQUE 9: Preguntas Frecuentes (FAQ)**

### **[PREGUNTA] Puedo usar esto sin saber programar?**
**SI** - Los scripts estan listos. Solo presiona "Source" y listo.

### **[PREGUNTA] Necesito instalar algo especial?**

**[LISTA] Necesitas:**

- R (gratis)
- RStudio (gratis)
- Python se instala automaticamente

### **[PREGUNTA] Cada ejercicio es realmente diferente?**
**SI** - Cambian materiales, porcentajes, colores, tamanos. Hay mas de 300 combinaciones posibles.

### **[PREGUNTA] Puedo modificar las preguntas?**
**[ALERTA] CUIDADO** - Como principiante, mejor usa los scripts tal como estan. Modificar requiere conocimiento avanzado.

### **[PREGUNTA] Funciona en cualquier computadora?**
**SI** - Windows, Mac, Linux. Solo necesitas R y RStudio.

### **[PREGUNTA] Los estudiantes pueden hacer trampa?**
**[SEGURO] DIFICIL** - Cada ejercicio es diferente, asi que no pueden copiarse facilmente.

### **[PREGUNTA] Cuanto tiempo toma generar ejercicios?**
**[TIEMPO] RAPIDO** - Entre 30 segundos y 2 minutos, dependiendo de cuantos generes.

### **[PREGUNTA] Puedo usar esto para otros temas de matematicas?**
**[DOCUMENTACION] ESTE EJERCICIO** - Solo para porcentajes y proporciones. Pero el sistema R-exams puede hacer otros temas.

### **[PREGUNTA] Es gratis?**
**[DINERO] TOTALMENTE GRATIS** - R, RStudio, Python, todo es software libre.

### **[PREGUNTA] Necesito internet?**
**[WEB] SOLO PARA INSTALAR** - Una vez instalado, funciona sin internet.

---

## [BLOQUE] **BLOQUE 10: Glosario de Terminos (Para que no te pierdas)**

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

**[FECHA] Creado**: Junio 2025\
**[META] Nivel**: Principiante-amigable con documentacion exhaustiva\
**Estado**: Listo para usar con guia completa\
**[ACTUALIZADO] Actualizado**: Para personas novatas en programacion y matematicas\
**[DOCUMENTACION] Documentacion**: Exhaustiva por bloques para maxima comprension
