---
name: skill-retroalimentacion
description: >
  Genera retroalimentación científica estilo ICFES para la sección Solution de ejercicios R-exams.
  OBLIGATORIO, AUTOMÁTICO y PERMANENTE para TODO ejercicio .Rmd.
  Incluye justificación matemática detallada de respuesta correcta Y análisis diagnóstico
  de CADA opción incorrecta usando el patrón "Es posible que los estudiantes que eligen..."
license: Proyecto Educativo - IE Pedacito de Cielo
compatibility: Integración obligatoria con generar-schoice y generar-cloze
metadata:
  author: alvaretto
  version: "1.0"
  language: es
  model_recommendation: opus
  fuente_oficial: "ICFES - Guía de Orientación Matemáticas 11° Cuadernillo 2-2023"
activation:
  type: automatic
  triggers:
    - generar-schoice
    - generar-cloze
    - edit .Rmd Solution section
allowed-tools:
  - Read
  - Write
  - Edit
---

# Skill: Retroalimentación Científica Estilo ICFES

## Principio Fundamental

**TODO ejercicio .Rmd DEBE incluir una sección Solution con retroalimentación científica completa siguiendo el estándar oficial ICFES.**

Esta regla es **OBLIGATORIA**, **AUTOMÁTICA** y **PERMANENTE**. No hay excepciones.

---

## Estructura ICFES de Retroalimentación

Basado en la Guía de Orientación ICFES Matemáticas 11° (páginas 22-51), TODA sección Solution debe incluir:

### 1. Encabezado Diagnóstico (OBLIGATORIO)

```markdown
Solution
========

### Información de la Pregunta

| Aspecto | Descripción |
|---------|-------------|
| **Competencia** | [Interpretación y representación / Formulación y ejecución / Argumentación] |
| **Componente** | [Numérico-variacional / Geométrico-métrico / Aleatorio] |
| **Afirmación** | [Descripción de lo que evalúa según estándar MEN] |
| **Evidencia** | [Lo que el estudiante demuestra al responder correctamente] |
| **Tarea** | [Acción específica que el estudiante debe realizar] |
| **Nivel de dificultad** | [N1/N2/N3/N4] |
```

### 2. ¿Qué Evalúa Esta Pregunta? (OBLIGATORIO)

```markdown
### ¿Qué evalúa esta pregunta?

Esta pregunta evalúa la capacidad del estudiante para [DESCRIPCIÓN ESPECÍFICA
de la habilidad o conocimiento matemático evaluado, en 2-3 oraciones].
```

### 3. Justificación de la Respuesta Correcta (OBLIGATORIO)

```markdown
### Respuesta Correcta: [LETRA]

**Justificación matemática:**

[Explicación paso a paso con fórmulas LaTeX]

**Paso 1:** [Descripción del primer paso]
$$[Fórmula LaTeX]$$

**Paso 2:** [Descripción del segundo paso]
$$[Fórmula LaTeX]$$

[Continuar hasta el resultado final]

**Por lo tanto**, la respuesta correcta es **[LETRA]** porque [conclusión].
```

### 4. Análisis de Opciones No Válidas (OBLIGATORIO PARA CADA UNA)

```markdown
### Opciones No Válidas

**Opción [LETRA]:**
Es posible que los estudiantes que eligen la opción [LETRA] [DESCRIPCIÓN DEL
ERROR CONCEPTUAL ESPECÍFICO]. Este error se presenta cuando [CAUSA RAÍZ DEL
ERROR]. Para evitar este error, el estudiante debe [ESTRATEGIA CORRECTIVA].

**Opción [LETRA]:**
Es posible que los estudiantes que eligen la opción [LETRA] [DESCRIPCIÓN DEL
ERROR CONCEPTUAL ESPECÍFICO]. Este error se presenta cuando [CAUSA RAÍZ DEL
ERROR]. Para evitar este error, el estudiante debe [ESTRATEGIA CORRECTIVA].

[Repetir para CADA opción incorrecta]
```

### 5. Reflexión Metacognitiva (OBLIGATORIO)

```markdown
### Reflexión Metacognitiva

`r sample(reflexiones_metacognitivas, 1)`

**Estrategias para evitar errores comunes:**
1. [Estrategia específica 1]
2. [Estrategia específica 2]
3. [Verificación final recomendada]
```

---

## Patrones de Error por Componente

### Componente Numérico-Variacional

| Código Error | Descripción | Patrón "Es posible que..." |
|--------------|-------------|---------------------------|
| ALG-OPE-01 | Inversión de operación | "...inviertan el orden de las operaciones o apliquen la operación inversa incorrectamente" |
| ALG-SIG-01 | Error de signo | "...cometan errores al operar con signos negativos, especialmente en productos o cocientes" |
| ALG-DIS-01 | Distributiva incorrecta | "...no apliquen correctamente la propiedad distributiva al expandir expresiones" |
| ARI-FRA-01 | Suma de fracciones | "...sumen numeradores y denominadores directamente sin buscar denominador común" |
| ARI-POR-01 | Porcentaje como cantidad | "...confundan el valor del porcentaje con la cantidad que representa" |

### Componente Geométrico-Métrico

| Código Error | Descripción | Patrón "Es posible que..." |
|--------------|-------------|---------------------------|
| GEO-ARE-01 | Confusión área/perímetro | "...confundan las fórmulas de área y perímetro o apliquen la fórmula incorrecta" |
| GEO-UNI-01 | Error de unidades | "...olviden convertir unidades o mezclen unidades incompatibles en el cálculo" |
| GEO-ESC-01 | Error de escala | "...no apliquen correctamente el factor de escala en problemas de semejanza" |
| GEO-ANG-01 | Ángulos complementarios/suplementarios | "...confundan ángulos complementarios (90°) con suplementarios (180°)" |

### Componente Aleatorio

| Código Error | Descripción | Patrón "Es posible que..." |
|--------------|-------------|---------------------------|
| EST-MTC-01 | Confusión medidas centrales | "...confundan la media con la mediana o la moda, o calculen la medida incorrecta" |
| EST-PRO-01 | Probabilidad > 1 | "...obtengan probabilidades mayores que 1 al no normalizar correctamente" |
| EST-GRA-01 | Lectura incorrecta de gráfico | "...lean incorrectamente la escala del eje vertical o confundan las categorías" |
| EST-FRE-01 | Frecuencia vs frecuencia relativa | "...confundan frecuencia absoluta con frecuencia relativa o porcentaje" |

---

## Ejemplos Completos de Retroalimentación ICFES

### Ejemplo 1: Estadística (Gráficos de Barras)

```markdown
Solution
========

### Información de la Pregunta

| Aspecto | Descripción |
|---------|-------------|
| **Competencia** | Interpretación y representación |
| **Componente** | Aleatorio |
| **Afirmación** | Interpretar información presentada en tablas y gráficos |
| **Evidencia** | Representa un conjunto de datos mediante gráficos de barras |
| **Nivel de dificultad** | N2 |

### ¿Qué evalúa esta pregunta?

Esta pregunta evalúa la capacidad del estudiante para representar gráficamente
información estadística presentada en forma de porcentajes, convirtiendo estos
valores a cantidades absolutas y seleccionando la representación visual correcta.

### Respuesta Correcta: A

**Justificación matemática:**

Se debe determinar la cantidad de personas interesadas en adoptar cada tipo de
mascota de la siguiente manera:

**Paso 1:** Calcular cantidad para gatos (30%)
$$120 \times \frac{30}{100} = 120 \times 0.30 = 36 \text{ personas}$$

**Paso 2:** Calcular cantidad para perros (45%)
$$120 \times \frac{45}{100} = 120 \times 0.45 = 54 \text{ personas}$$

**Paso 3:** Calcular cantidad para hámsteres (25%)
$$120 \times \frac{25}{100} = 120 \times 0.25 = 30 \text{ personas}$$

**Por lo tanto**, la respuesta correcta es **A** porque representa correctamente
las barras con alturas 36, 54 y 30 para gatos, perros y hámsteres respectivamente.

### Opciones No Válidas

**Opción B:**
Es posible que los estudiantes que eligen la opción B pongan en las barras de
cada mascota el valor correspondiente al porcentaje (30, 45, 25) sin convertir
el porcentaje a cantidad de personas. Este error se presenta cuando el estudiante
no comprende que el porcentaje es una proporción que debe aplicarse al total.
Para evitar este error, el estudiante debe recordar que:
$$\text{Cantidad} = \text{Total} \times \frac{\text{Porcentaje}}{100}$$

**Opción C:**
Es posible que los estudiantes que eligen la opción C confundan la relación
entre las categorías e intercambien los valores de gatos y perros. Este error
se presenta cuando el estudiante no verifica cuidadosamente la correspondencia
entre cada categoría y su valor. Para evitar este error, el estudiante debe
etiquetar claramente cada cálculo con su categoría correspondiente.

**Opción D:**
Es posible que los estudiantes que eligen la opción D calculen incorrectamente
dividiendo el total por el porcentaje en lugar de multiplicar. Este error
produce valores como $120 \div 30 = 4$. Para evitar este error, el estudiante
debe recordar que "porcentaje de" significa multiplicación, no división.

### Reflexión Metacognitiva

Identificar errores en representaciones gráficas nos ayuda a comprender mejor
las relaciones entre porcentajes y cantidades absolutas. La metacognición es
fundamental para detectar y corregir nuestros propios errores.

**Estrategias para evitar errores comunes:**
1. Siempre verificar que los porcentajes sumen 100%
2. Calcular cada cantidad usando la fórmula: Total × (Porcentaje/100)
3. Verificar que las cantidades calculadas sumen el total original
```

### Ejemplo 2: Álgebra (Ecuaciones)

Ver [plantilla completa de Solution](references/plantilla-solution.md) para más ejemplos.

---

## Integración con Otros Skills

### Activación Automática

```
generar-schoice → [Genera Question + Answerlist]
                       ↓
              skill-retroalimentacion [AUTOMÁTICO]
                       ↓
              [Genera Solution completa]
                       ↓
              [Validación ciclo-validacion.md]
```

### En generar-schoice y generar-cloze

Cuando se genera la sección Solution, este skill se activa automáticamente y
DEBE producir:

1. ✅ Encabezado diagnóstico con competencia/componente/afirmación
2. ✅ Sección "¿Qué evalúa?"
3. ✅ Justificación matemática de respuesta correcta con LaTeX
4. ✅ Análisis de CADA opción incorrecta con patrón "Es posible que..."
5. ✅ Reflexión metacognitiva con estrategias

---

## Checklist Pre-Generación

Antes de generar la sección Solution:

- [ ] ¿Pool de errores conceptuales definido con códigos?
- [ ] ¿Cada error tiene descripción del patrón "Es posible que..."?
- [ ] ¿Justificación matemática incluye fórmulas LaTeX paso a paso?
- [ ] ¿Se conocen los metadatos ICFES (competencia, componente, afirmación)?

## Checklist Post-Generación

Después de generar la Solution:

- [ ] ¿Tiene encabezado diagnóstico completo?
- [ ] ¿Tiene sección "¿Qué evalúa?"?
- [ ] ¿Justificación de respuesta correcta tiene pasos numerados con LaTeX?
- [ ] ¿CADA opción incorrecta tiene análisis "Es posible que..."?
- [ ] ¿Cada análisis incluye: error + causa raíz + estrategia correctiva?
- [ ] ¿Tiene reflexión metacognitiva con estrategias?

---

## Antipatrones PROHIBIDOS

### 1. Solution mínima sin análisis

```markdown
❌ Solution
========
La respuesta correcta es A.
```

### 2. Análisis superficial de distractores

```markdown
❌ **Opción B:** Esta opción es incorrecta.
```

### 3. Sin justificación matemática

```markdown
❌ La respuesta es A porque es la única que cumple con las condiciones.
```

### 4. Sin análisis de errores individuales

```markdown
❌ Las demás opciones son incorrectas por errores de cálculo.
```

---

## Pool de Reflexiones Metacognitivas (OBLIGATORIO)

```r
reflexiones_metacognitivas <- c(
  "Identificar errores en el razonamiento de otros nos ayuda a evitar cometerlos nosotros mismos. La metacognición es fundamental para el aprendizaje matemático.",
  "Analizar por qué una respuesta es incorrecta fortalece la comprensión profunda del concepto. Este proceso de autoevaluación mejora significativamente el desempeño.",
  "Los errores más frecuentes en este tipo de problemas están relacionados con [área específica]. Reconocerlos es el primer paso para superarlos.",
  "Cuando identificamos el tipo de error conceptual, podemos diseñar estrategias específicas para evitarlo en el futuro.",
  "La diferencia entre un error de cálculo y un error conceptual es importante: el primero es mecánico, el segundo requiere revisar la comprensión del concepto."
)
```

---

## Referencias

- **Fuente oficial**: ICFES - Guía de Orientación Matemáticas 11° Cuadernillo 2-2023 (pp. 22-51)
- **Plantilla completa**: [references/plantilla-solution.md](references/plantilla-solution.md)
- **Regla metacognitiva**: `.claude/rules/ejercicios-metacognitivos.md`
- **Ciclo validación**: `.claude/rules/ciclo-validacion.md`
- **Códigos de error**: `.claude/rules/ejercicios-metacognitivos.md` → Taxonomía de Códigos

---

**Versión**: 1.0
**Fecha**: 2026-02-07
**Estado**: ACTIVO, OBLIGATORIO, AUTOMÁTICO, PERMANENTE
**Excepciones**: NINGUNA
