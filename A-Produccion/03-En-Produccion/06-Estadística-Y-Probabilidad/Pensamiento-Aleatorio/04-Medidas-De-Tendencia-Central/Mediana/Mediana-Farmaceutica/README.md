# Mediana y Moda — Ejercicio Farmaceutica

Ejercicio tipo ICFES Saber 11 sobre relacion entre mediana y moda en un conjunto de 5 datos. El estudiante debe argumentar si es posible determinar el valor minimo a partir de estos dos estadisticos.

## Versiones

### v1 — Pool de contextos narrativos

**Archivo:** `mediana_moda_farmaceutica_v1.Rmd`

- 8 plantillas narrativas diferentes (congreso, laboratorio, periodistica, etc.)
- Multiples protagonistas por plantilla (medicamentos, vacunas, agencias)
- La pregunta siempre es afirmativa: "¿Es CORRECTO que el minimo fue [moda]%?" → Respuesta: SI
- Solucion con reflexion metacognitiva aleatoria
- Complejidad alta en el codigo R (muchas variantes de contexto)

### v2 — Contexto unico, solucion mejorada

**Archivo:** `mediana_moda_farmaceutica_v2.Rmd`

- Contexto fijo: empresa farmaceutica, cinco estudios de efectividad
- Codigo simplificado y mas mantenible
- La pregunta siempre es afirmativa: "¿Es CORRECTO que el minimo fue [moda]%?" → Respuesta: SI
- Solucion paso a paso con demostracion formal (5 pasos)
- Tabla de datos ordenados, analisis de opciones incorrectas
- Validacion automatica con testthat

### v3 — Variante SI/NO aleatoria

**Archivo:** `mediana_moda_farmaceutica_v3.Rmd`

- Basada en v2 (mismo contexto y estilo)
- Elige aleatoriamente (~50/50) entre dos variantes:
  - **Variante A (SI):** "¿El minimo fue [moda]%?" → Correcto, la moda ocupa x1 y x2
  - **Variante B (NO):** "¿El minimo fue [mediana]%?" → Incorrecto, el minimo es [moda]%, no [mediana]%
- Opciones de respuesta y distractores adaptados a cada variante
- Solucion completa diferente para cada variante (misma calidad y extension)
- La variante B obliga al estudiante a reconocer que la mediana esta en x3, no en x1

### v4 — Contextos narrativos + inversion minimo/maximo

**Archivo:** `mediana_moda_farmaceutica_v4.Rmd`

- Basada en v3 (misma logica matematica y variantes A/B)
- **3 ejes de aleatorizacion combinados:**
  - **6 contextos narrativos:** farmaceutica, colegio, fabrica, robotica, calificaciones, encuesta
  - **2 relaciones:** moda < mediana (pregunta sobre minimo) o moda > mediana (pregunta sobre maximo)
  - **2 variantes:** A (respuesta SI) o B (respuesta NO)
- Total: 6 × 2 × 2 = **24 combinaciones estructurales** (× ~1650 numericas = ~39.600 versiones potenciales)
- Solucion construida por secciones con paste0() (no sprintf) — mas mantenible
- Variables parametricas: extremo, pos_moda, comparador_sing, etc. — definidas una vez
- Contexto ICFES dinamico: `exextra[Contexto]` refleja el contexto seleccionado
- Corrige inconsistencia v3: nivel_dificultad ahora es 3 (coherente con exextra[Nivel]: 3)

## Metadata ICFES

| Campo | Valor |
|-------|-------|
| Competencia | Argumentacion |
| Componente | Aleatorio |
| Evidencia | Argumenta sobre la estructura de un conjunto de datos a partir de sus estadisticos descriptivos |
| Nivel | 3 |
| DOK | 3 (Pensamiento estrategico) |
| Bloom | Evaluar |
| SOLO | Relacional |
| Contexto | Laboral |
| Tipo | schoice |

## Generacion de examenes

```r
library(exams)

# 10 versiones en PDF (con shuffle de opciones)
set.seed(2026)
exams2pdf("mediana_moda_farmaceutica_v4.Rmd", n=10, template="plain")

# 10 versiones para Moodle
set.seed(2026)
exams2moodle("mediana_moda_farmaceutica_v4.Rmd", n=10)

# Otros formatos
exams2html("mediana_moda_farmaceutica_v4.Rmd", n=5)
exams2nops("mediana_moda_farmaceutica_v4.Rmd", n=30)  # hojas de respuesta opticas
```

## Datos aleatorios

- Relacion: moda < mediana (pregunta minimo) o moda > mediana (pregunta maximo)
- Moda: 30–100 (uniforme, rango ajustado por relacion)
- Mediana: 30–100 (uniforme, rango ajustado por relacion)
- Offset entre moda y mediana: 1 a 25
- La logica matematica funciona con cualquier par valido en ambas direcciones

## Ejes de aleatorizacion (v4)

| Eje | Valores | Impacto |
|-----|---------|---------|
| Contexto narrativo | 6 plantillas | Enunciado, unidad, texto de pregunta |
| Relacion moda-mediana | 2 (menor, mayor) | Pregunta sobre minimo vs maximo |
| Variante | 2 (A=SI, B=NO) | Opcion correcta, solucion |
| Moda | 30–95 (o derivado) | Valores numericos |
| Mediana | derivado de moda ± offset | Valores numericos |
| Reflexion | 4 frases | Cierre metacognitivo |

## Evolucion

| Version | Fecha | Cambio principal |
|---------|-------|-----------------|
| v1 | — | Pool de 8 contextos narrativos, respuesta siempre SI |
| v2 | — | Contexto unico, solucion formal paso a paso, testthat |
| v3 | 2026-03-24 | Variante aleatoria SI/NO con opciones y solucion adaptadas |
| v4 | 2026-03-25 | 6 contextos + inversion minimo/maximo + paste0 (24 combinaciones) |
