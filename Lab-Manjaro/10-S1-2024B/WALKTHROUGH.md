# 🚶‍♂️ Walkthrough: Resolución del Ejercicio del Teorema de Pitágoras

## 🎯 **Objetivo**

Este walkthrough te guiará a través del proceso de resolución del ejercicio de entrenamiento del Teorema de Pitágoras. Entenderás cómo se generan los datos, cómo interpretar el diagrama TikZ y qué pasos seguir para completar las tres partes del problema tipo `cloze`.

---

## 🎲 **Fase 1: Generación de Datos Aleatorios**

Cada vez que se compila el ejercicio, ocurren varios procesos de aleatorización en segundo plano para crear un problema único.

1.  **Selección de una Terna Pitagórica**: El sistema elige una terna pitagórica primitiva (un conjunto de tres números enteros `a, b, c` tal que `a^2 + b^2 = c^2`, como `3, 4, 5`).
2.  **Escalamiento**: La terna se multiplica por un factor aleatorio (por ejemplo, 2) para generar una nueva terna (`6, 8, 10`). Esto asegura una amplia variedad de problemas.
3.  **Asignación de la Incógnita**: El sistema decide aleatoriamente qué lado será el desconocido. Hay tres posibilidades:
    *   **Calcular la hipotenusa (c)**: Se te darán los valores de los dos catetos (`a` y `b`).
    *   **Calcular el cateto a**: Se te darán los valores del cateto `b` y la hipotenusa `c`.
    *   **Calcular el cateto b**: Se te darán los valores del cateto `a` y la hipotenusa `c`.
4.  **Orientación del Triángulo**: El diagrama del triángulo se rota y/o se invierte de forma aleatoria. Esto significa que la hipotenusa no siempre estará en la misma posición, obligándote a identificar los lados por su relación con el ángulo recto y no por su ubicación.

---

## 🎨 **Fase 2: Interpretación del Diagrama TikZ**

El ejercicio te presentará un diagrama de un triángulo rectángulo. Debes observar lo siguiente:

-   **El Ángulo Recto**: Identifica el símbolo del cuadrado (└) que marca el ángulo de 90°.
-   **Los Catetos**: Son los dos lados que forman el ángulo recto. En el diagrama, estarán etiquetados como `a` y `b`.
-   **La Hipotenusa**: Es el lado opuesto al ángulo recto. Siempre es el lado más largo y estará etiquetado como `c`.
-   **La Incógnita**: El lado que necesitas calcular estará marcado con una **"x"** en lugar de un valor numérico.

**Ejemplo de Interpretación:**
Imagina que el diagrama muestra:
-   Lado `a = 6`
-   Lado `b = x`
-   Lado `c = 10`

De esto, debes deducir que conoces un cateto (`a`) y la hipotenusa (`c`), y tu tarea es calcular el otro cateto (`b`).

---

## ✍️ **Fase 3: Proceso de Resolución (Ejemplo Práctico)**

Siguiendo el ejemplo anterior (`a=6`, `c=10`, `b=x`), así es como resolverías las tres partes del ejercicio `cloze`.

### **Parte 1: Identificar la Hipotenusa**

> **Pregunta 1:** "En el triángulo mostrado, el valor de la **hipotenusa** es:"

**Solución**: La hipotenusa es el lado `c`, opuesto al ángulo recto. Su valor es **10**.
*Respuesta a introducir: `10`*

### **Parte 2: Identificar el Cateto Conocido**

> **Pregunta 2:** "Uno de los **catetos** tiene un valor de:"

**Solución**: Los catetos son `a` y `b`. El cateto conocido es `a`. Su valor es **6**.
*Respuesta a introducir: `6`*

### **Parte 3: Calcular el Lado Faltante**

> **Pregunta 3:** "Aplicando el Teorema de Pitágoras, el valor del lado faltante **x** es:"

**Solución**:
1.  **Fórmula base**: $a^2 + b^2 = c^2$
2.  **Necesitamos calcular un cateto (b)**, así que despejamos la fórmula: $b^2 = c^2 - a^2$
3.  **Sustituir valores**: $b^2 = 10^2 - 6^2$
4.  **Calcular potencias**: $b^2 = 100 - 36$
5.  **Restar**: $b^2 = 64$
6.  **Calcular la raíz cuadrada**: $b = \sqrt{64}$
7.  **Resultado**: $b = 8$

El valor del lado faltante `x` es **8**.
*Respuesta a introducir: `8`*

---

## ✅ **Resumen de la Solución**

Para este ejemplo, las respuestas que deberías introducir en los campos del ejercicio `cloze` son:
1.  `10`
2.  `6`
3.  `8`

Recuerda que en cada nueva versión del ejercicio, los números, la incógnita y la orientación del triángulo cambiarán, pero el proceso de identificación y cálculo será siempre el mismo. ¡Mucha suerte!
