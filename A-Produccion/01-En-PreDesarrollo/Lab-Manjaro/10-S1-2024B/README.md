#  README - Ejercicio de Entrenamiento del Teorema de Pitágoras

## 🎯 **Propósito del Ejercicio**

Este directorio contiene un ejercicio R-exams de tipo `cloze` (híbrido) diseñado para el entrenamiento completo del **Teorema de Pitágoras**. El ejercicio evalúa la competencia de **Formulación y Ejecución** en un nivel de dificultad 2, dentro del componente **Geométrico-Métrico**.

El objetivo principal es que el estudiante demuestre su capacidad para:
1.  Identificar correctamente los catetos y la hipotenusa en un triángulo rectángulo.
2.  Aplicar la fórmula del Teorema de Pitágoras ($a^2 + b^2 = c^2$) para calcular un lado desconocido.
3.  Interpretar un diagrama geométrico generado dinámicamente con TikZ.
4.  Resolver problemas que implican tanto el cálculo de la hipotenusa como el de uno de los catetos.

---

## ✨ **Características Principales**

-   **🔢 Tipo de Ejercicio**: `cloze` (híbrido) con 3 preguntas numéricas.
-   **⚙️ Competencia ICFES**: Formulación y Ejecución.
-   **📊 Componente ICFES**: Geométrico-Métrico.
-   **📉 Nivel de Dificultad**: 2 (Básico - Intermedio).
-   **🤖 Aleatorización Robusta**:
    -   Los valores de los catetos se generan aleatoriamente, asegurando que siempre formen ternas pitagóricas (lados enteros).
    -   Se selecciona aleatoriamente qué lado del triángulo debe ser calculado (cateto a, cateto b, o hipotenusa c).
    -   El diagrama del triángulo se rota y se invierte aleatoriamente para evitar la memorización visual.
-   **🎨 Gráfico Dinámico con TikZ**: Se genera una imagen de un triángulo rectángulo única para cada versión del ejercicio. Las etiquetas (*a*, *b*, *c*) y los valores conocidos se posicionan dinámicamente. El lado a calcular se marca con una "x".

---

## 🛠️ **Estructura del Archivo `.Rmd`**

El archivo `01-teorema_pitagoras_entrenamiento_completo_cloze_geometrico_metrico_formulacion_ejecucion_n2_cloze_v1.Rmd` sigue la estructura estándar del proyecto:

1.  **Chunk `data_generation`**:
    -   Se genera una terna pitagórica primitiva y se escala por un factor común para crear diversidad.
    -   Se decide aleatoriamente cuál de los tres lados (`a`, `b`, o `c`) será la incógnita.
    -   Se preparan las variables para el diagrama TikZ y para la solución.

2.  **Chunk `generar_tikz`**:
    -   Construye un diagrama de un triángulo rectángulo usando TikZ.
    -   Aplica transformaciones aleatorias (`rotate`, `xscale=-1`) para cambiar la orientación del triángulo.
    -   Etiqueta los lados con sus valores correspondientes o con una "x" si es la incógnita.

3.  **Sección `Question`**:
    -   Presenta el contexto y el diagrama del triángulo.
    -   Pide al estudiante calcular el valor del lado faltante ("x") y responder a preguntas conceptuales sobre la identificación de catetos e hipotenusa.

4.  **Sección `Solution`**:
    -   Muestra el cálculo paso a paso para encontrar el lado desconocido, despejando la variable correcta de la fórmula del Teorema de Pitágoras.

5.  **Sección `Meta-information`**:
    -   Define el ejercicio como `cloze` con tres respuestas numéricas (`num|num|num`).
    -   Establece la solución correcta y una tolerancia de `0.01` para los cálculos.

---

## 🚀 **Uso y Compilación**

Para generar una versión de este ejercicio, utiliza los comandos estándar de R-exams en la consola de R.

```r
library(exams)

# Generar una versión en HTML
exams2html("Lab-Manjaro/10-S1-2024B/01-teorema_pitagoras_entrenamiento_completo_cloze_geometrico_metrico_formulacion_ejecucion_n2_cloze_v1.Rmd")

# Generar una versión para Moodle
exams2moodle("Lab-Manjaro/10-S1-2024B/01-teorema_pitagoras_entrenamiento_completo_cloze_geometrico_metrico_formulacion_ejecucion_n2_cloze_v1.Rmd", name = "entrenamiento-pitagoras")
```
