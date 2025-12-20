# FLUJO DE GENERACIÓN DE ARCHIVOS .Rmd

*Evaluación de Preguntas ICFES - Matemáticas Saber 11°*

## 1. ENTRADA DEL USUARIO

El usuario aporta una imagen con un escenario matemático correspondiente a las pruebas ICFES Saber 11°.

## 2. ANÁLISIS DE CARACTERÍSTICAS ICFES MATEMÁTICAS

El workflow determina qué propósitos y características de ICFES matemáticas tiene la pregunta:

### 2.1 Nivel de Dificultad

Clasificación de complejidad de la pregunta basada en los niveles de desempeño del ICFES:

| **Nivel** | **Descripción** |
|-----------|-----------------|
| **1** | Lee información puntual en tablas o gráficas con escala explícita. Puntaje: 0-35 |
| **2** | Compara datos, identifica valores representativos, cambia gráficas a tablas. Puntaje: 36-50 |
| **3** | Selecciona gráficas según contexto, compara información con manipulaciones aritméticas, reconoce desarrollos planos. Puntaje: 51-70 |
| **4** | Nivel avanzado con comprensión profunda de conceptos matemáticos complejos. Puntaje: 71-100 |

### 2.2 Competencia (Seleccionar SOLO 1)

Las competencias matemáticas evaluadas son:

- **Interpretación y Representación (34%)**
  - Comprende y transforma información cuantitativa y esquemática en distintos formatos
  - Da cuenta de características básicas de información en series, gráficas, tablas y esquemas
  - Transforma la representación de una o más piezas de información

- **Formulación y Ejecución (43%)**
  - Plantea y ejecuta estrategias para solucionar problemas en diversos contextos
  - Diseña planes de solución con información cuantitativa o esquemática
  - Ejecuta y resuelve problemas que involucran información cuantitativa

- **Argumentación (23%)**
  - Valida o refuta conclusiones, estrategias y soluciones
  - Plantea afirmaciones que sustentan o refutan interpretaciones
  - Argumenta a favor o en contra de procedimientos de solución
  - Establece la validez de una solución propuesta

### 2.3 Componente (Seleccionar SOLO 1)

Categorías conceptuales sobre las cuales se realizan los desempeños:

- **Numérico-Variacional**
  - Pensamiento numérico y sistemas numéricos
  - Pensamiento variacional y sistemas algebraicos y analíticos
  - Números racionales, proporciones, operaciones aritméticas
  - Variación, cambio, funciones, ecuaciones y expresiones algebraicas

- **Geométrico-Métrico**
  - Pensamiento espacial y sistemas geométricos
  - Pensamiento métrico y sistemas de medidas
  - Figuras planas y tridimensionales, transformaciones, medición

- **Aleatorio**
  - Pensamiento aleatorio y sistemas de datos
  - Estadística descriptiva, probabilidad, interpretación de datos
  - Tablas, gráficas, medidas de tendencia central

### 2.4 Tipo de Pensamiento Matemático

Identificar el tipo de pensamiento predominante:

- Pensamiento Numérico
- Pensamiento Espacial
- Pensamiento Métrico
- Pensamiento Variacional
- Pensamiento Aleatorio

### 2.5 Tarea Matemática

Describir la acción específica que debe realizar el estudiante:

- Interpretar, calcular, comparar, analizar, resolver, justificar, modelar, etc.

### 2.6 Contenidos Matemáticos Curriculares

Categorización por área de conocimiento matemático:

#### A. Álgebra y Cálculo

**Contenidos Genéricos:**

- Números racionales (fracciones, razones, decimales, porcentajes)
- Propiedades operaciones aritméticas (suma, resta, multiplicación, división, potenciación)
- Relaciones lineales, afines, razones de cambio

**Contenidos No Genéricos:**

- Expresiones algebraicas, propiedades y operaciones
- Representación gráfica y algebraica de funciones
- Funciones racionales, trigonométricas, polinomiales, exponenciales, logarítmicas
- Sucesiones y límites

#### B. Geometría

**Contenidos Genéricos:**

- Triángulos, círculos, paralelogramos, esferas, cilindros y sus medidas
- Relaciones de paralelismo y ortogonalidad
- Desigualdad triangular
- Sistemas de coordenadas cartesianas

**Contenidos No Genéricos:**

- Sólidos y figuras geométricas complejas (pirámides, polígonos >4 lados)
- Relaciones de congruencia y semejanza
- Teoremas clásicos (Pitágoras, Tales)
- Coordenadas polares y tridimensionales
- Transformaciones en el plano (traslaciones, rotaciones, homotecias, reflexiones)

#### C. Estadística

**Contenidos Genéricos y No Genéricos:**

- Tablas y gráficas estadísticas (barras, circulares, líneas)
- Medidas de tendencia central (media, mediana, moda)
- Probabilidad básica y eventos aleatorios
- Interpretación de datos y resultados estadísticos

### 2.7 Eje Axial Disciplinar

Determinar si la pregunta tiene un enfoque:

- **Puramente Matemático**
  - Contexto abstracto o matemático formal

- **Aplicado/Contextualizado**
  - Situaciones de la vida cotidiana, ciencias, economía, etc.

## 3. FORMATO DE EJERCICIO R/EXAMS Y ANÁLISIS VISUAL

Determinar el tipo de ejercicio a generar según R/exams y analizar componentes visuales:

### 3.1 Schoice (Single-Choice)

**Pregunta de selección única**

**Características:**

- Solo UNA respuesta correcta de una lista de alternativas
- El estudiante debe seleccionar la única opción correcta
- Formato estándar de preguntas ICFES
- Todas las opciones se presentan al mismo nivel

**Cuándo usar schoice:**

- Pregunta directa con una sola respuesta correcta
- Ejercicios de cálculo con resultado único
- Identificación de conceptos o propiedades
- Selección de gráficas o figuras correctas

*Ejemplo típico:*

*"¿Cuál es el valor de x en la ecuación 2x + 5 = 13?"*

- A) x = 3
- B) x = 4
- C) x = 5
- D) x = 6

### 3.2 Cloze (Pregunta Compuesta)

**Pregunta con múltiples elementos de respuesta**

**Características:**

- Combina MÚLTIPLES tipos de respuestas en una sola pregunta
- Puede incluir: respuestas numéricas, de texto, selección única, selección múltiple
- Útil para problemas con varios pasos o sub-preguntas relacionadas
- Formato más complejo que schoice

**Cuándo usar cloze:**

- Problemas con múltiples pasos que requieren diferentes tipos de respuesta
- Ejercicios que integran varias habilidades matemáticas
- Situaciones que requieren completar espacios con diferentes tipos de información
- Análisis de datos con múltiples cálculos o interpretaciones

*Ejemplo típico:*

*"Dada la función f(x) = 2x² - 3x + 1:*

- *1. El valor de f(2) es: _____ (respuesta numérica)*
- *2. El vértice está en: (selección única: A, B, C, D)*
- *3. La función es creciente en: _____ (respuesta de texto)"*

### 3.3 Diferencias Clave

| **Aspecto** | **Schoice** | **Cloze** |
|-------------|-------------|-----------|
| **Estructura** | Una pregunta, una respuesta | Una pregunta, múltiples elementos de respuesta |
| **Complejidad** | Simple | Compleja |
| **Tipos de respuesta** | Un solo tipo | Múltiples tipos combinados |
| **Uso típico ICFES** | Mayoría de preguntas estándar | Problemas integrados complejos |

### 3.4 Análisis Visual del Enunciado

Determinar si el enunciado de la pregunta contiene elementos gráficos:

**A. Con Gráficos Matemáticos**

- Funciones representadas en plano cartesiano
- Figuras geométricas (triángulos, círculos, polígonos, sólidos)
- Gráficas estadísticas (barras, circulares, líneas, histogramas)
- Diagramas matemáticos (árboles de probabilidad, diagramas de Venn)
- Tablas con datos numéricos

**B. Con Gráficos No Matemáticos**

- Imágenes de contexto (escenarios reales, objetos)
- Diagramas ilustrativos no formales
- Representaciones pictóricas

**C. Sin Gráficos**

- Solo texto y/o expresiones matemáticas escritas
- Enunciado verbal sin apoyo visual

### 3.5 Análisis de Opciones de Respuesta

Evaluar las características de las opciones de respuesta presentadas:

**A. Opciones Textuales/Numéricas**

- Respuestas expresadas como texto
- Valores numéricos
- Expresiones algebraicas escritas
- *Evaluación: ¿Son adecuadas, claras y mutuamente excluyentes?*

**B. Opciones con Gráficos**

- Cada opción incluye una gráfica o figura diferente
- Representaciones visuales como alternativas
- *Evaluación: ¿Las gráficas son legibles y correctamente diferenciadas?*
- *¿Hay correspondencia clara con el enunciado?*

**C. Opciones Mixtas**

- Combinación de texto/números y elementos gráficos
- Verificar coherencia y claridad en la presentación

## 4. PROCESO DE GENERACIÓN Y OUTPUT

### 4.1 Análisis Integral

El sistema realiza un análisis completo considerando:

1. Alineación con Estándares Básicos de Competencias (EBC)
2. Correspondencia con Matriz de Referencia ICFES
3. Nivel de complejidad apropiado
4. Formato R/exams adecuado (schoice o cloze)
5. Presencia y tipo de elementos gráficos
6. Contexto y situación planteada

### 4.2 Generación del Archivo .Rmd

El workflow genera un archivo R Markdown con:

- Metadatos completos de la pregunta (extype: schoice o cloze)
- Clasificación según todas las categorías analizadas
- Referencias a documentos oficiales ICFES
- Código para generación de gráficos cuando sea necesario
- Estructura apropiada según el tipo de ejercicio seleccionado
- Análisis pedagógico y sugerencias de mejora

## 5. DOCUMENTOS DE REFERENCIA

Este workflow se basa en:

- Estándares Básicos de Competencias en Matemáticas (MEN)
- Marco de Referencia - Prueba de Matemáticas Saber 11° (ICFES)
- Matriz de Referencia Matemáticas 11°
- Guía de Orientación Saber 11°
- Documentación R/exams (https://www.r-exams.org/)
- Repositorio proyecto-r-exams-icfes-matematicas-optimizado

---

Si se entienden las diferentes secciones y sus subsecciones de este flujo de trabajo, 
las veremos como ramificaciones de un árbol de decisión. Cada una de esas ramificaciones 
podría estar representada por una única plantilla