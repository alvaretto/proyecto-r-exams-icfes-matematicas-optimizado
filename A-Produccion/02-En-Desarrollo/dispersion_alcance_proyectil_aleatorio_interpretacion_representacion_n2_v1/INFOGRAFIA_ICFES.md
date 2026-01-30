# Ejercicio ICFES: Dispersión y Tipo de Relación en Gráficas

## Ficha Técnica del Ejercicio

| Atributo | Valor |
|----------|-------|
| **Conjunto** | 4 variantes (opc_A, opc_B, opc_C, opc_D) |
| **Tipo** | SCHOICE (Selección Única) |
| **Nivel de Dificultad** | 2 (Puntaje 36-50) |
| **Competencia** | Interpretación y Representación |
| **Componente** | Aleatorio |
| **Contexto** | Matemático/Científico |
| **Contenido** | Estadística - No Genérico |
| **Eje Axial** | Aplicado |

---

## Las 4 Variantes del Ejercicio

| Archivo | Contexto | Relación | Dispersión ↑ con | Respuesta |
|---------|----------|----------|------------------|-----------|
| `*_opc_A.Rmd` | Resorte (Hooke) | Lineal | Fuerza (X) | **A** |
| `*_opc_B.Rmd` | Resorte (Hooke) | Lineal | Elongación (Y) | **B** |
| `*_opc_C.Rmd` | Proyectil | No lineal | Ángulo (X) | **C** |
| `*_opc_D.Rmd` | Proyectil | No lineal | Alcance (Y) | **D** |

### Matriz de Respuestas

```
              │ Dispersión ↑ con    │ Dispersión ↑ con
              │ Variable X          │ Variable Y
──────────────┼─────────────────────┼─────────────────────
   LINEAL     │   A (correcta)      │   B (correcta)
──────────────┼─────────────────────┼─────────────────────
 NO LINEAL    │   C (correcta)      │   D (correcta)
```

---

## Nivel de Desempeño Evaluado

### Nivel 2 (Puntaje 36 a 50)

Este ejercicio evalúa las siguientes habilidades del Nivel 2:

> - **Compara datos de dos variables presentadas en una misma gráfica** sin necesidad de hacer operaciones matemáticas.
> - **Identifica valores o puntos representativos** en diferentes tipos de registro a partir del significado que tienen en la situación.
> - **Toma decisiones sobre la veracidad o falsedad de una afirmación** cuando esta se puede explicar verbalizando la lectura directa que se hace de la información.

### Alineación con el Ejercicio

| Habilidad Nivel 2 | Cómo se Evalúa |
|-------------------|----------------|
| Comparar datos de dos variables | Analizar relación entre ejes X y Y en la gráfica de dispersión |
| Identificar valores representativos | Identificar patrones de dispersión y puntos de máximo/tendencia |
| Decidir veracidad/falsedad | Seleccionar la opción que describe correctamente el comportamiento |

---

## Competencia: Interpretación y Representación

### Definición ICFES

> Esta competencia se relaciona con la habilidad para **comprender y transformar la información presentada en formatos distintos** como tablas, gráficas, conjuntos de datos, diagramas, esquemas, etcétera; así como la capacidad de utilizar estas representaciones para **extraer información relevante** que permita, entre otras cosas, establecer relaciones matemáticas e **identificar tendencias y patrones**.

### Aplicación en el Ejercicio

El estudiante debe:

1. **Comprender** una gráfica de dispersión (scatter plot)
2. **Extraer información** sobre el tipo de relación (lineal vs no lineal)
3. **Identificar patrones** de dispersión (dónde hay mayor variabilidad)
4. **Establecer relaciones** entre las variables

### Evidencias Evaluadas

| Evidencia | Aplicación |
|-----------|------------|
| Da cuenta de las características básicas de la información presentada | Identifica si la relación es lineal o no lineal |
| Transforma la representación de una o más piezas de información | Traduce el patrón visual de dispersión a una descripción verbal |

---

## Componente: Aleatorio

### Definición ICFES

El componente Aleatorio corresponde a las categorías conceptuales relacionadas con:

- Representación e interpretación de datos
- Medidas de tendencia central y dispersión
- Nociones de probabilidad
- Análisis de gráficas estadísticas

### Aplicación en el Ejercicio

- **Gráficas de dispersión**: Interpretación de scatter plots
- **Dispersión de datos**: Identificar donde hay mayor/menor variabilidad
- **Heterocedasticidad**: Varianza no constante a lo largo de los datos

---

## Contenido Matemático por Variante

### Variantes A y B: Ley de Hooke (Lineal)

$$x = \frac{F}{k}$$

| Variable | Significado |
|----------|-------------|
| x | Elongación del resorte (cm) |
| F | Fuerza aplicada (N) |
| k | Constante del resorte (N/cm) |

**Características:**

- Relación **lineal** (proporcionalidad directa)
- Gráfica: **recta** ascendente
- Contexto: Experimento de física con resorte

### Variantes C y D: Movimiento de Proyectil (No Lineal)

$$R = \frac{v_0^2 \sin(2\theta)}{g}$$

| Variable | Significado |
|----------|-------------|
| R | Alcance horizontal (m) |
| v₀ | Velocidad inicial (m/s) |
| θ | Ángulo de lanzamiento (rad) |
| g | Gravedad (9.8 m/s²) |

**Características:**

- Relación **no lineal** (función senoidal)
- Gráfica: **parábola** con máximo en θ ≈ 0.78 rad (45°)
- Contexto: Experimento de cinemática

---

## Estructura de la Pregunta

### Enunciado (varía según contexto)

**Variantes A/B (Resorte):**
> Un experimento consiste en medir la elongación de un resorte en función de la fuerza aplicada. En la gráfica se registran los resultados de [N] mediciones realizadas con el mismo resorte.

**Variantes C/D (Proyectil):**
> Un experimento consiste en medir el alcance horizontal de un proyectil en función del ángulo con el que se lanza. En la gráfica se registran los resultados de [N] lanzamientos realizados con la misma velocidad inicial.

### Opciones de Respuesta (común a todos)

| Opción | Estructura |
|--------|------------|
| A | [lineal] + [dispersión con variable independiente] |
| B | [lineal] + [dispersión con variable dependiente] |
| C | [no lineal] + [dispersión con variable independiente] |
| D | [no lineal] + [dispersión con variable dependiente] |

---

## Gráficas de los Ejercicios

### opc_A: Lineal + Dispersión crece con X

```
Elongación (cm)
    14 |                    * *     disperso →
    12 |                  * * *
    10 |               * * *
     8 |            * * *
     6 |         * * *
     4 |      * *
     2 |   * *                      ← concentrado
     0 +-------------------------> Fuerza (N)
       0    2    4    6    8   10   12
```

### opc_B: Lineal + Dispersión crece con Y

```
Elongación (cm)
    14 |                    *   *   disperso ↑
    12 |                  * * *
    10 |               * * *
     8 |            * *
     6 |         * *
     4 |      **
     2 |   **                       concentrado ↓
     0 +-------------------------> Fuerza (N)
```

### opc_C: No lineal + Dispersión crece con X

```
Alcance (m)
    14 |
    12 |        * * *
    10 |      * * * * *      *  *   disperso →
     8 |    * * * * * * *   *  *  *
     6 |  * * *       * * * *
     4 | * *             * * *
     2 |*                    *  *   ← concentrado
     0 +-------------------------> Ángulo (rad)
       0   0.4   0.8   1.2   1.6
```

### opc_D: No lineal + Dispersión crece con Y

```
Alcance (m)
    14 |
    12 |        * * *        disperso (Y alto) ↑
    10 |      * * * * *
     8 |    * * * * * * *
     6 |  * *         * *
     4 | *               *   concentrado (Y bajo) ↓
     2 |*                 *
     0 +-------------------------> Ángulo (rad)
       0   0.4   0.8   1.2   1.6
```

---

## Análisis de Distractores

### Para variantes A/B (Lineal)

| Opción | Si A es correcta | Si B es correcta |
|--------|------------------|------------------|
| A | ✓ Correcta | ✗ Error en variable de dispersión |
| B | ✗ Error en variable de dispersión | ✓ Correcta |
| C | ✗ Error en tipo de relación | ✗ Error en tipo + dispersión |
| D | ✗ Error en tipo + dispersión | ✗ Error en tipo de relación |

### Para variantes C/D (No lineal)

| Opción | Si C es correcta | Si D es correcta |
|--------|------------------|------------------|
| A | ✗ Error en tipo + dispersión | ✗ Error en tipo de relación |
| B | ✗ Error en tipo de relación | ✗ Error en tipo + dispersión |
| C | ✓ Correcta | ✗ Error en variable de dispersión |
| D | ✗ Error en variable de dispersión | ✓ Correcta |

---

## Relación con Porcentaje de Preguntas ICFES

### Distribución por Competencia

| Competencia | Porcentaje | Este Ejercicio |
|-------------|------------|----------------|
| Interpretación y Representación | 34% | **✓** |
| Formulación y Ejecución | 43% | |
| Argumentación | 23% | |

Este ejercicio contribuye al **34%** de preguntas de Interpretación y Representación.

---

## Variabilidad del Ejercicio

### Parámetros Aleatorios por Variante

| Variante | Parámetros | Combinaciones Datos |
|----------|------------|---------------------|
| A, B | k (9) × n (21) × ruido (5) | ~945 |
| C, D | v₀ (16) × n (21) × ruido (6) | ~2,016 |

### Variantes Textuales (común)

| Término | Opciones |
|---------|----------|
| Lineal | lineal, proporcional, de tipo lineal, directamente proporcional |
| No lineal | no lineal, no proporcional, parabólico, cuadrático |
| Disperso | más disperso, con mayor variabilidad, más variable |

**Combinaciones textuales**: 4⁵ = 1,024

### Total de Versiones Únicas

| Variante | Cálculo | Total |
|----------|---------|-------|
| A, B | 945 × 1,024 | ~968,000 |
| C, D | 2,016 × 1,024 | ~2,064,000 |

---

## Recomendaciones Pedagógicas

### Para el Docente

1. **Antes del ejercicio**: Revisar conceptos de:

   - Gráficas de dispersión
   - Relaciones lineales vs no lineales
   - Heterocedasticidad (dispersión variable)

2. **Durante el ejercicio**: Guiar al estudiante a:

   - Observar la forma general (¿recta o curva?)
   - Identificar patrones de dispersión
   - Relacionar dispersión con la variable correcta

3. **Después del ejercicio**: Discutir por qué la dispersión puede variar

### Para el Estudiante

1. **Paso 1**: Observar forma general (lineal = recta, no lineal = curva)
2. **Paso 2**: Identificar dónde hay más "nube" de puntos
3. **Paso 3**: Determinar si la dispersión aumenta con X o con Y
4. **Paso 4**: Seleccionar la combinación correcta

---

## Metadatos R-exams

```yaml
# Varía según archivo:
exname: [nombre_descriptivo]
extype: schoice
exsolution: [1000|0100|0010|0001]  # Según variante
exshuffle: TRUE
exsection: Estadística/Gráficas de Dispersión
```

---

**Última actualización**: 2025-12-30
**Versión**: 2.0 (Consolidado para 4 variantes)

*Alineado con: Marco de Referencia ICFES Matemáticas Saber 11*
