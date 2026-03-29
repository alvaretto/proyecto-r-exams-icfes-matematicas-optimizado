# Walkthrough: mediana_moda_farmaceutica_v3

> Tutorial generado automáticamente. Explica paso a paso cómo funciona
> este ejercicio .Rmd y cómo crear uno similar.

## Mapa del ejercicio

| Aspecto | Valor |
|---------|-------|
| Tipo | SCHOICE |
| Tema matemático | Mediana y moda en conjuntos de 5 datos |
| Patrón metacognitivo | Evaluación de afirmación |
| Nivel ICFES | 3 |
| Competencia | Argumentación |
| Componente | Aleatorio |
| ¿Tiene gráficos? | No |
| DOK / Bloom / SOLO | 3 / Evaluar / Relacional |
| Complejidad estimada | Media-Alta |
| Líneas de código | 287 |
| Variantes | 2 (A: respuesta SÍ, B: respuesta NO) |

---

## 1. Vista de pájaro

Imagina que eres juez en un debate matemático. Alguien afirma: "El mínimo de estos cinco datos es tal valor". Tu trabajo no es solo decir si tiene razón o no, sino evaluar la *justificación* que da. ¿Su razonamiento es válido? ¿O llega a la conclusión correcta por un camino equivocado?

Este ejercicio presenta un escenario farmacéutico: cinco estudios de efectividad de un tratamiento. Se conocen solo dos estadísticos (la moda y la mediana) y se pregunta si una afirmación sobre el valor mínimo es correcta. El estudiante debe elegir entre cuatro justificaciones razonadas — no números, sino argumentos.

Lo que hace especial a este ejercicio es el **mecanismo de variantes**: con probabilidad 50/50, la pregunta cambia completamente. En la variante A, se pregunta si el mínimo es la moda (y la respuesta es SÍ). En la variante B, se pregunta si el mínimo es la mediana (y la respuesta es NO). Ambas variantes comparten la misma demostración matemática de fondo, pero invierten la pregunta.

---

## 2. Anatomía del .Rmd

```
┌─────────────────────────────────────────────────┐
│ data_generation (líneas 18–223)                 │  ← "La cocina"
│   ├── generar_datos_validos()                   │     Genera moda y mediana válidas
│   ├── variante ← sample(c("A","B"), 1)          │     Moneda al aire: ¿pregunta SÍ o NO?
│   ├── opciones[1..4] via sprintf()              │     4 argumentos razonados
│   ├── solution_explanation via sprintf()         │     Demostración completa (~70 líneas)
│   └── reflexiones_metacognitivas                │     Pool de 4 frases
├─────────────────────────────────────────────────┤
│ validacion_datos (líneas 226–249)               │  ← "El inspector de calidad"
│   └── 7 tests con testthat                      │     Verifica coherencia en cada render
├─────────────────────────────────────────────────┤
│ Question (líneas 251–262)                       │  ← "La vitrina"
│   ├── Enunciado: contexto farmacéutico          │     Moda, mediana, 5 estudios
│   ├── Pregunta evaluativa                       │     "¿Es CORRECTO afirmar que...?"
│   └── Answerlist: 4 justificaciones             │     Cada una con razonamiento
├─────────────────────────────────────────────────┤
│ Solution (líneas 264–267)                       │  ← "El manual del profesor"
│   └── solution_explanation interpolada          │     5 pasos + tabla + refutaciones
├─────────────────────────────────────────────────┤
│ Meta-information (líneas 269–287)               │  ← "La etiqueta"
│   ├── extype: schoice, exshuffle: TRUE          │
│   └── ICFES 6D + DOK/Bloom/SOLO                │     13 campos de metadatos
└─────────────────────────────────────────────────┘
```

---

## 3. Bloque por bloque

### 3.1 Generación de datos válidos

```r
generar_datos_validos <- function() {
  # Moda: un porcentaje entre 30% y 95%
  # ¿Por qué 30-95? Valores menores serían poco realistas para efectividad
  # farmacéutica. Valores mayores de 95 dejarían poco espacio para la mediana.
  moda <- sample(30:95, 1)

  # Offset máximo: la mediana debe ser mayor que la moda, pero ≤ 100%
  # Si moda = 90, solo puede subir hasta 100, entonces max_offset = 10
  # Si moda = 50, puede subir hasta 75, entonces max_offset = 25
  max_offset <- min(25, 100 - moda)

  # Mediana: siempre mayor que la moda, nunca mayor que 100
  mediana <- moda + sample(1:max_offset, 1)

  # Tres contratos que DEBEN cumplirse. Si alguno falla, el render
  # se detiene con error ruidoso en vez de producir datos malos.
  stopifnot(moda < mediana)        # La lógica del ejercicio lo exige
  stopifnot(moda >= 30 && moda <= 95)  # Rango realista
  stopifnot(mediana <= 100)        # Un porcentaje no supera 100

  return(list(moda = moda, mediana = mediana))
}
```

**Analogía**: `generar_datos_validos()` es como una máquina expendedora que solo acepta monedas de cierto tamaño. Si le metes una moneda que no cabe (ej: mediana > 100), la máquina se detiene y te dice "moneda inválida" en lugar de tragársela y dar un producto equivocado.

**¿Qué pasaría si...?**

- cambiaras `sample(30:95, 1)` a `sample(85:99, 1)`: con moda = 99, `max_offset = 1`, así la mediana siempre sería 100%. Se pierde diversidad.
- eliminaras `stopifnot(moda < mediana)`: si un bug hiciera que moda ≥ mediana, la demostración sería matemáticamente falsa y el estudiante vería una solución incoherente.

### 3.2 El mecanismo de variantes (el corazón del ejercicio)

```r
# Moneda al aire: ¿preguntamos sobre la moda o sobre la mediana?
variante <- sample(c("A", "B"), 1)
```

Este `sample` decide todo lo que sigue:

| Aspecto | Variante A | Variante B |
|---------|-----------|-----------|
| **Pregunta** | ¿El mínimo fue `moda`%? | ¿El mínimo fue `mediana`%? |
| **Respuesta** | **SÍ** | **NO** |
| **correct_index** | 3 | 2 |
| **Opción correcta** | "Sí, porque la mediana debe tener dos valores mayores y dos menores..." | "No, porque la moda aparece exactamente dos veces en las posiciones inferiores..." |

**Analogía**: Es como tener dos versiones de un examen de conducir. Ambas muestran la misma intersección, pero una pregunta "¿Puede girar a la derecha?" (Sí) y la otra "¿Puede girar a la izquierda?" (No). La regla de tránsito es la misma; lo que cambia es qué se pregunta.

**¿Por qué dos variantes?** Multiplica por 2 la diversidad del ejercicio. Un estudiante que recibe variante A ve una pregunta con respuesta "Sí"; otro recibe variante B con respuesta "No". Esto evita que circule una sola respuesta.

### 3.3 Construcción de opciones (variante A como ejemplo)

```r
if (variante == "A") {
  valor_pregunta <- moda      # Se pregunta por la moda
  correct_index <- 3          # La opción 3 es la correcta

  opciones <- c(
    # Opción 1: Trampa — razonamiento falso que llega a la conclusión correcta
    # Dice "Sí" pero por razones equivocadas (moda = resultado de todos los demás)
    sprintf("Sí, porque la mediana es el resultado de uno de los estudios y la moda
             el de todos los demás, entonces el menor es %d%%.", moda),

    # Opción 2: Trampa — dice "No" con contraejemplo falso
    sprintf("No, porque la mediana podría ser esa, por dos estudios muy cercanos
             a 100%%, dos iguales y uno menor que %d%%.", moda),

    # Opción 3: CORRECTA — captura la restricción estructural exacta
    sprintf("Sí, porque la mediana debe tener dos valores mayores y dos menores,
             y la moda garantiza que los dos menores son %d%%.", moda),

    # Opción 4: Trampa — dice "No" con error de conteo
    sprintf("No, porque la mediana es resultado de un estudio y la moda el de otros
             dos, luego alguno de los restantes podría ser menor que %d%%.", moda)
  )
}
```

**Diseño de distractores**: No son números aleatorios sino *razonamientos equivocados* plausibles. Cada distractor representa un tipo de error conceptual:

| Opción | Error que representa |
|--------|---------------------|
| 1 (A) | Conclusión correcta con razonamiento falso ("la moda son los 4 restantes" — contradice la definición) |
| 2 (A) | Contraejemplo inválido (ignora la restricción moda < mediana) |
| 4 (A) | Confusión en la distribución de valores (asume espacio libre debajo de la moda) |

**Nota sobre `%%`**: En `sprintf`, `%d` es un placeholder para número entero. Para mostrar un símbolo de porcentaje literal, se escribe `%%`. Esto es una regla de `sprintf`, no de R en general.

### 3.4 La demostración en la solución

La `solution_explanation` es un string gigante construido con `sprintf()` que contiene ~70 líneas de texto con ~30 placeholders `%d` y un `%s` (para la reflexión metacognitiva).

Incluye 6 secciones pedagógicas:

1. **Análisis de la afirmación**: Veredicto directo (CORRECTA o INCORRECTA)
2. **Procedimiento correcto paso a paso**: Demostración en 5 pasos con notación LaTeX
3. **Propiedades del concepto**: Tabla resumen de mediana, moda y restricción clave
4. **Tabla de datos ordenados**: Muestra la estructura x₁ ≤ x₂ ≤ x₃ ≤ x₄ ≤ x₅ con valores
5. **Reflexión metacognitiva**: Frase aleatoria del pool
6. **Estrategia para verificar** + **Análisis de opciones incorrectas**: Refutación de cada distractor

**¿Por qué tantos argumentos en `sprintf`?**

Los valores de `moda` y `mediana` aparecen docenas de veces en la demostración. Cada aparición necesita un `%d` con su argumento correspondiente. Los argumentos están agrupados con comentarios (ej: `# 5. Paso 4: "$x_3 = %d$%%"`) para que un editor pueda rastrear cuál argumento corresponde a cuál placeholder. Si agregas o quitas un `%d` del texto sin ajustar la lista de argumentos, `sprintf` producirá un error.

### 3.5 Pool de reflexiones metacognitivas

```r
reflexiones_metacognitivas <- c(
  "Analizar la relación entre la mediana y la moda nos permite deducir
   la estructura de un conjunto de datos sin conocer todos los valores.",
  "La moda y la mediana proporcionan información complementaria: la moda
   indica repetición y la mediana indica posición central.",
  "Verificar afirmaciones sobre estadísticos requiere razonamiento lógico,
   no solo cálculo. Es fundamental considerar todas las restricciones
   simultáneamente.",
  "En problemas con pocos datos, las propiedades de los estadísticos
   imponen restricciones fuertes que determinan la estructura del conjunto."
)
reflexion <- reflexiones_metacognitivas[sample(length(reflexiones_metacognitivas), 1)]
```

Una frase se elige al azar e se interpola en la demostración vía `%s`. Esto añade variabilidad textual sin afectar la lógica matemática.

### 3.6 Chunk de validación (el inspector de calidad)

```r
test_that("Validaciones de coherencia", {
  # 1. ¿La opción marcada como correcta tiene el texto esperado?
  if (variante == "A") {
    expect_true(correct_index == 3)
    expect_true(grepl("Sí, porque la mediana debe tener dos valores mayores",
                      opciones[correct_index]))
  } else {
    expect_true(correct_index == 2)
    expect_true(grepl("No, porque la moda", opciones[correct_index]))
  }

  # 2. ¿Los datos de ejemplo verifican las propiedades matemáticas?
  datos_ejemplo <- c(moda, moda, mediana, mediana + 1, mediana + 2)
  expect_true(median(datos_ejemplo) == mediana)      # Mediana es x₃
  expect_true(min(datos_ejemplo) == moda)             # Mínimo es la moda
  expect_true(sum(datos_ejemplo == moda) == 2)        # Moda aparece 2 veces
  expect_true(sum(datos_ejemplo > mediana) == 2)      # 2 valores por encima

  # 3. ¿La restricción fundamental se cumple?
  expect_true(moda < mediana)
})
```

**Analogía**: Este chunk es como un inspector que revisa cada producto antes de salir de la fábrica. Si alguien cambia accidentalmente `correct_index` o modifica los rangos de generación, el inspector detiene la línea de producción.

**¿Qué pasaría si eliminaras este chunk?** El ejercicio seguiría renderizando, pero si un editor futuro introduce un bug, el error pasaría desapercibido hasta que un estudiante vea una pregunta incoherente.

### 3.7 La pregunta (Question)

```markdown
Question
========
Una empresa farmacéutica calculó el porcentaje de efectividad de un
tratamiento para una enfermedad. Para ello, hizo cinco estudios y, de
cada estudio, se conoció el porcentaje de efectividad que tuvo el
tratamiento. De los cinco porcentajes, solo hay una moda que es `r moda`%
y la mediana es `r mediana`%.

¿Es CORRECTO afirmar que la efectividad mínima mostrada en los estudios
fue de `r valor_pregunta`%?
```

Observa que `valor_pregunta` cambia según la variante: en A es `moda`, en B es `mediana`. El estudiante no sabe cuál variante recibió — solo ve un número concreto.

### 3.8 Metadatos (Meta-information)

```yaml
exname: Efectividad tratamiento farmaceutico ICFES
extype: schoice
exsolution: `r solution_string`    # "0010" (var. A) o "0100" (var. B)
exshuffle: TRUE                    # R-exams mezcla el orden de opciones
extol: 0.01
```

**¿Por qué `exshuffle: TRUE`?** Como las opciones son texto (no gráficos) y la Solution no referencia "Opción A/B/C/D" por letra, R-exams puede reordenar libremente sin romper la coherencia.

**¿Por qué `mchoice2string(1:4 == correct_index)` en vez de escribir `"0010"` directamente?** Porque si alguien cambiara `correct_index` sin actualizar el string, la solución marcada sería incorrecta. Con `mchoice2string`, la solución se *deriva* del mismo `correct_index` — fuente única de verdad.

---

## 4. Patrones clave (Las reglas del juego)

### 4.1 Variantes simétricas con inversión de respuesta

El ejercicio duplica su diversidad sin duplicar su complejidad. La variante A (SÍ) y la variante B (NO) comparten la misma demostración matemática de fondo. Lo que cambia es qué estadístico se pregunta y cuál opción es la correcta. Esto es eficiente: una sola pieza de lógica matemática genera dos preguntas distintas.

### 4.2 Distractores como razonamientos (no como números)

Las opciones no son valores numéricos sino argumentos completos con "Sí, porque..." o "No, porque...". Esto obliga al estudiante a evaluar la *validez lógica* del razonamiento, no solo comparar números. El distractor más peligroso es la opción 1 de variante A: llega a la conclusión correcta ("Sí") pero con un razonamiento falso.

### 4.3 `stopifnot` como contrato de invariantes

Los tres `stopifnot` no son validación de input del usuario — son afirmaciones de que la lógica interna es correcta. Si el generador produjera datos que violan estas condiciones, el error se manifiesta inmediatamente durante el renderizado, no silenciosamente en un examen incorrecto.

### 4.4 `sprintf` con argumentos comentados

Los bloques de `sprintf` tienen comentarios como `# 5. Paso 4: "$x_3 = %d$%%"` que mapean cada argumento a su placeholder. Sin estos comentarios, sería casi imposible mantener la coherencia cuando hay 30+ placeholders.

### 4.5 Validación integrada en el .Rmd

El chunk `validacion_datos` ejecuta tests en cada renderizado. Esto garantiza que si el código cambia, los errores se detectan inmediatamente — no en un pipeline separado que podría no ejecutarse.

---

## 5. La lógica matemática (para un novato)

Esta es la demostración que sustenta todo el ejercicio. Entenderla es necesario para modificarlo.

**Premisas**: 5 datos, moda única = M, mediana = Me, con M < Me.

**Paso 1 — La moda aparece exactamente 2 veces.**
Para ser moda, M debe aparecer al menos 2 veces. ¿Puede aparecer 3 o más? Si apareciera 3 veces, como los datos ordenados son x₁ ≤ x₂ ≤ x₃ ≤ x₄ ≤ x₅, los tres M incluirían la posición central x₃. Pero x₃ = Me ≠ M (porque M < Me). Contradicción. Entonces M aparece exactamente 2 veces.

**Paso 2 — Las dos apariciones de M están en x₁ y x₂.**
Como M < Me = x₃, M no puede estar en x₃ ni en posiciones superiores (que son ≥ x₃ > M). Solo quedan x₁ y x₂, que son exactamente 2 posiciones — las 2 apariciones de M.

**Paso 3 — El mínimo es M.**
Como x₁ = M, el valor mínimo es la moda, no la mediana.

**Analogía**: Imagina 5 personas ordenadas por estatura en una fila. Sabes que hay exactamente 2 personas con la misma estatura (la moda) y esa estatura es menor que la de la persona del centro (la mediana). Entonces las 2 personas iguales están al inicio de la fila — son las más bajas.

---

## 6. Cómo crear un ejercicio similar (Receta paso a paso)

1. [ ] **Elegir un escenario con pocos datos** (5-7) donde las propiedades de los estadísticos impongan restricciones fuertes
2. [ ] **Identificar una demostración deductiva** que permita determinar algo con certeza a partir de los estadísticos
3. [ ] **Diseñar dos variantes** con respuestas invertidas (SÍ/NO) para maximizar diversidad
4. [ ] **Escribir 4 justificaciones** por variante: una correcta y tres con errores de razonamiento específicos
5. [ ] **Incluir la trampa "conclusión correcta, razonamiento falso"** — es el distractor más pedagógico
6. [ ] **Construir la Solution** con los 5 pasos de la demostración + refutación de cada distractor
7. [ ] **Agregar `stopifnot`** para garantizar que los datos cumplen las premisas de la demostración
8. [ ] **Agregar chunk `testthat`** que verifique coherencia en cada renderizado
9. [ ] **Usar `mchoice2string`** para derivar `exsolution` del `correct_index`
10. [ ] **Agregar pool de reflexiones metacognitivas** (mínimo 4 frases variadas)
11. [ ] **Renderizar en 4 formatos**: HTML, PDF, DOCX, NOPS
12. [ ] **Validar diversidad**: las combinaciones de (moda, mediana, variante) deben generar 200+ versiones únicas

---

## 7. Errores comunes y cómo evitarlos

| # | Error | Qué pasa | Cómo evitarlo |
|---|-------|----------|---------------|
| 1 | Cambiar `correct_index` sin ajustar `opciones` | `exsolution` marca una opción cuyo texto es un distractor | El chunk `testthat` lo detecta — nunca eliminar ese chunk |
| 2 | Eliminar `stopifnot` | Datos inválidos (moda ≥ mediana) producen demostración falsa | Mantener los 3 `stopifnot` como contratos de invariante |
| 3 | Agregar/quitar `%d` en `sprintf` sin ajustar argumentos | Error de R: "too few/many arguments" en el renderizado | Seguir la convención de comentarios numerados en los argumentos |
| 4 | Cambiar texto de opciones sin actualizar refutaciones en Solution | La Solution cita fragmentos de opciones que ya no existen | Buscar las citas textuales en `solution_explanation` y actualizarlas |
| 5 | Poner `moda > 95` | `max_offset` se reduce mucho, poca diversidad en la mediana | Respetar el rango `30:95` que balancea realismo y diversidad |
| 6 | Usar `exshuffle: FALSE` sin `sample()` interno | Las opciones siempre aparecen en el mismo orden | En este ejercicio, `exshuffle: TRUE` es correcto porque no hay gráficos ni referencias a letras |

---

## 8. Glosario rápido

| Término | Significado |
|---------|-------------|
| **SCHOICE** | Selección única — 1 correcta, 3 distractores |
| **exshuffle** | Si R-exams mezcla automáticamente el orden de las opciones |
| **exsolution** | String binario que marca la opción correcta (ej: "0010" = opción 3) |
| **mchoice2string** | Función de R-exams que convierte un vector lógico a string binario |
| **sprintf** | Función de R para interpolar valores en un string con placeholders (`%d`, `%s`) |
| **stopifnot** | Detiene la ejecución si la condición es falsa — actúa como contrato |
| **testthat** | Paquete de R para tests unitarios (`expect_true`, `test_that`) |
| **variante** | Versión del ejercicio (A o B) que cambia la pregunta y la respuesta correcta |
| **Progressive Disclosure** | Revelar información gradualmente, de fácil a difícil |
| **Evaluación de afirmación** | Patrón metacognitivo donde el estudiante juzga si un argumento es válido |
| **DOK 3** | Pensamiento estratégico — requiere razonamiento no rutinario |
| **Bloom: Evaluar** | El estudiante juzga la validez de un razonamiento, no solo calcula |

---

*Generado con `/generar-walkthrough` — 2026-03-25*
