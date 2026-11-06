# EPA-2023-MAT-11-2-09 — ítem oficial, transcripción verbatim

> **Por qué existe este archivo.** El recorte `originales/orig_pregunta_09.png` es una imagen: no se
> puede citar con grep. Este archivo es su transcripción verbatim, cotejada contra el recorte el
> 2026-11-05.
>
> **Contrasta contra este archivo y, ante la duda, contra el recorte; nunca contra tu recuerdo.**

## Fuente

- Cuadernillo: Evaluar para Avanzar 2023, Matemáticas 11-2, pregunta 09.
- Recorte de trabajo: [`originales/orig_pregunta_09.png`](originales/orig_pregunta_09.png), 982 × 434 px,
  md5 `f396b0a9cb152493570664a9eda4a338`. Es la misma imagen que `01.png` de
  `docus/Math/2023-Matematicas-11-2/09/` y de la carpeta `Variación-Lineal-Auto-Viajero-09/` de producción.
- Clave y competencia: `docus/Math/2023-Matematicas-11-2/EPA_ClavesTotal.xls`, hoja «Claves Full»,
  columna «Item 09 - S1»: competencia **A** (Argumentación) y clave **C**.
- **Sin ficha de alineación:** este cuadernillo no está en el corpus de `Todo-Pajaro`.

**No usar como fuente** `2023-Matematicas-11-2-09.md`: su opción C no coincide con lo impreso, y transcribe el
Paso 3 sin las unidades invertidas, que son la trampa del ítem.

## Enunciado (verbatim)

> Se sabe que un auto que viaja con rapidez constante, a las 8:00 a. m. ya se ha alejado 20 km del pueblo
> donde inició y a las 10:00 a. m. está a 100 km de dicho pueblo. Para calcular la rapidez media en km/h de
> dicho auto, se realizaron los siguientes pasos:
>
> **Paso 1.** Se calcula cuánta distancia se recorrió: 100 km − 20 km = 80 km
> **Paso 2.** Se calcula el tiempo que transcurrió: 10 h − 8 h = 2 h
> **Paso 3.** Se halla el cociente entre los valores obtenidos: 80 h/2 km = 40 h/km
>
> ¿En qué paso se cometió un error y por qué?

## Opciones (verbatim, con su letra oficial)

- **A.** En el paso 1, ya que la distancia recorrida es 100 km y no 80 km
- **B.** En el paso 2, ya que el tiempo transcurrido es 8 h − 10 h = −2 h
- **C.** En el paso 3, ya que el cociente es 80 km/2 h ← **clave oficial**
- **D.** En el paso 1, ya que la distancia recorrida es 20 km − 100 km = −20 km

## La trampa del ítem (regla #24, H-2)

El error impreso es deliberado: el Paso 3 escribe `80 h/2 km = 40 h/km`, con las unidades invertidas. El
número 40 es correcto; lo que falla es la razón. **No se normaliza ni se «corrige».** Es la pregunta.

Los pasos 1 y 2 son correctos. Las opciones A, B y D son argumentos falsos sobre pasos sin error.

## Clasificación

Solo la competencia es oficial, de `EPA_ClavesTotal.xls`. El resto lo fijó el paso 1 (2026-11-05) contra el
catálogo canónico; el paso 10 lee esta tabla con `--item`.

| Campo | Valor |
|---|---|
| Competencia | Argumentación |
| Componente | Numérico-variacional |
| Afirmación | Valida procedimientos y estrategias matemáticas utilizadas para dar solución a problemas. |
| Evidencia | Argumenta a favor o en contra de un procedimiento para resolver un problema a la luz de criterios presentados o establecidos. |
| Nivel | 3 |
| Descriptor | D3.10 — Justifica afirmaciones utilizando planteamientos y operaciones aritméticas o haciendo uso directo de un concepto, es decir, a partir de un único argumento. |
| Estándar asociado | Resuelvo y formulo problemas que involucren magnitudes cuyos valores medios se suelen definir indirectamente como razones entre valores de otras magnitudes, como la velocidad media, la aceleración media y la densidad media. (10°-11°, Pensamiento métrico) |
| Contenido | Álgebra y Cálculo (genérico) |
| Contexto | Familiar o personal |
| Tarea | Frente a un procedimiento paso a paso que aplica el cociente entre dos magnitudes y contiene un error en una de sus operaciones, identificar en cuál paso se cometió el error y por qué la operación realizada es incorrecta. |
| DOK | 2 |
| Bloom | Evaluar |
| SOLO | Relacional |

**Fuentes y decisiones del paso 1:**

- Afirmación, Evidencia (`Arg_E2`), Descriptor y Estándar (`Métrico_10-11`): literales de `evidencias-mat.json`,
  `niveles-mat.json` y `estandares-mat-ebc.json`. La Tarea usa el patrón 1 de
  `procedimiento_paso_a_paso_argumentar_error` (`tareas-canonicas-mat.json`). Se descartó `Arg_E3` porque el objeto
  evaluado es el procedimiento, no el resultado final.
- **Componente: el clasificador (Haiku) propuso Geométrico-métrico, y se corrigió.** Se apoyaba en que el EBC trae la
  velocidad media en el pensamiento métrico. El Marco de referencia 2019, que manda en la jerarquía de fuentes, pone
  «Relaciones lineales y afines y razones de cambio (tasas de interés, tasas cambiarias velocidad, aceleración, etc.)»
  en los contenidos genéricos de Álgebra y cálculo, y esa categoría es la de los «Pensamientos numérico y
  variacional». El corpus coincide: los 6 ítems de la alineación 2025-2 con vehículos y rapidez o velocidad (007, 049,
  124, 128, 139 y 153) están en Numérico-variacional.
- **El Estándar sigue siendo el métrico** porque es el único del EBC que nombra la velocidad media. EBC y Marco son
  taxonomías distintas: el cruce es deliberado y no es un error de ficha.
- **Contexto: el clasificador propuso científico, y se corrigió.** La Tabla 2 del Marco incluye «transporte» en los
  contextos familiares o personales.

## Regla de uso

La versión nueva se contrasta con este archivo. Se puede variar lo que el flujo exige variar (regla #22),
pero la instancia canónica conserva el impreso, incluido el error del Paso 3 (regla #24, H-2).
