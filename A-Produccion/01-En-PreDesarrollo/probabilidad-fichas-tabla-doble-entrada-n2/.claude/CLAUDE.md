# Invariantes locales — `probabilidad-fichas-tabla-doble-entrada-n2`

> Regla #17 / Incidente `INC-CLAUDE-LOCAL` (pre-flight 19 del orquestador). Estas invariantes
> **prevalecen sobre el criterio genérico** dentro de este subproyecto. Si alguna contradice una
> regla del repo raíz, gana la del raíz y el conflicto **se reporta**, no se resuelve en silencio.
> Las fijó el profesor al encargar el ejercicio (2026-10-26). Relajar cualquiera exige su firma
> (regla #24 H-5).

**Origen:** pregunta impresa **32** del simulacro SAI «Nivelación 6» de grado 10, Sesión 2. La fuente
está en este directorio: `Sesion-02.pdf` (página 5 del PDF, mitad izquierda = página impresa 10) y sus
recortes en `graficador/originales/`. Texto verbatim, tabla celda a celda, clave y clasificación:
[`ITEM_OFICIAL.md`](../ITEM_OFICIAL.md). **Contrasta contra ese archivo y el recorte, no contra tu recuerdo.**

## I-1 · Fidelidad al ítem: las opciones son pares de tipos de ficha

La pregunta es la original y las cuatro opciones nombran **pares de tipos de ficha** (color, figura y,
en algunas, categoría), como en el impreso. **Prohibido** el patrón que convierte las opciones en
descripciones de errores conceptuales. La capa metacognitiva obligatoria (regla #1) va en la
**Solution**: diagnóstico de cada distractor (qué lectura de la tabla lleva a elegirlo), reflexión y
estrategia.

Guardia: un `stopifnot` en `data_generation` que rechace opciones con vocabulario de error
(«estudiante», «error», «confundió», «sumó», «olvidó»…).

## I-2 · Instancia canónica verbatim

Una instancia reproduce **exactamente** el impreso: el enunciado (Eduardo, antiguo juego chino), la
tabla (Círculo | Cuadrado: Blanca-Par 12 | 11 · Blanca-Impar 8 | 15 · Negra-Par 16 | 20 · Negra-Impar
10 | 8), las cuatro opciones con su texto y, como clave, el **contenido** de la opción B del impreso
(«Una ficha blanca con un cuadrado y una ficha negra con un círculo.»). La **letra** no se fija: con
opciones de texto `exshuffle: TRUE` es obligatorio (`codigo-rmd` #6; con `FALSE`, `ERR_C4`) y Moodle
vuelve a mezclar (regla #19). *Aclaración del coordinador tras el dry-run (2026-10-26), por exigencia
del raíz.* Se implementa como **excepción canónica** con su `stopifnot` (frases y celdas comparadas
contra literales). Las demás versiones varían la tabla
y el contexto (regla #11: mínimo 6 plantillas, sin el patrón «Un(a) [oficio], [nombre], registró…»).

## I-3 · Nivel 2 / D2.3 sin inflar el DOK

`exextra[Nivel]: 2`, Descriptor D2.3, DOK 2 y el verbo de Bloom real (*Aplicar* es legítimo). Si el
ejercicio generado resultara DOK ≥ 3, **se pregunta al profesor** antes de tocar el nivel o el nombre
(`_n2_`). El DOK nunca se mueve para cuadrar.

## I-4 · Mismo espacio muestral

Todos los eventos que comparan las opciones salen **de la misma bolsa**, con el mismo total de casos
posibles: es la condición del descriptor D2.3. Prohibido introducir probabilidad condicional («si la
ficha es blanca…»), extracciones que cambien el denominador o eventos de bolsas distintas.

## I-5 · Una sola clave, construida por diseño

Exactamente **una** opción con frecuencias iguales; en las otras tres, distintas. La igualdad de la
clave se **construye** (familia F1: nada de remuestrear tablas hasta que aparezca) y la unicidad se
verifica **enumerando** el espacio de generación con un script propio del subproyecto, no solo con
N = 100. Guardias: `stopifnot(sum(iguales) == 1)` en cada versión y opciones únicas (regla #10, sin
`_neg_`).

## I-6 · Pool de errores (≥ 4), anclado en las justificaciones de la ficha

| Lectura errónea (de la ficha) | En la canónica |
|---|---|
| Supone que las combinaciones «opuestas» de color y figura deben coincidir | A: 20 frente a 28 |
| Lee una sola categoría y toma por iguales valores cercanos | C: 11 frente a 10 |
| Toma las celdas que se cruzan sin sumar las categorías | D: 8 frente a 20 |
| Al menos una más, de diseño propio | — |

**Ajuste del 2026-10-27 (decisión del profesor, WAIT_USER #3).** En la canónica, C y D son celdas
sueltas: «lee una sola categoría» (11 es par y 10 es impar) y «sin sumar las categorías» son falsos ahí
(detractor FASE 2C, H1 de r1). Por eso, **solo en la canónica**:
- C se analiza con **PROB-CER** («toma por iguales dos cantidades cercanas»: 11 y 10 difieren en una
  ficha), que recoge la segunda mitad de la lectura de la ficha; sus textos genéricos valen para celdas
  sueltas.
- D conserva **PROB-CRU** con textos propios de celda suelta («toma las celdas que se cruzan en la
  tabla», sin «sumar»), guardados en `TXT_CANON`.

En las versiones no canónicas, **desde la decisión del profesor del 2026-10-27 (canal de estímulo)**,
se sortean solo PROB-UNA y PROB-CER, y el espejo (PROB-SIM) va siempre. PROB-CRU queda en el pool,
porque I-6 exige ≥ 4, pero solo se usa en la canónica (D), que es donde la ficha lo ancla. Guardia: en
las no canónicas, PROB-UNA es la única opción cuyos dos tipos repiten un valor de celda: ni la clave,
ni el espejo, ni PROB-CER repiten.

Guardia (detractores del 2026-10-27): un error cuyas fichas son celdas sueltas debe cumplir una de dos
condiciones. O declara `vale_celda_suelta = TRUE` (hoy solo PROB-CER), o trae en `TXT_CANON` los cuatro
campos visibles (hoy PROB-CRU). Además, ningún texto que muestre puede usar vocabulario de suma o de
agregación («sum…», «deja fuera», «una categoría», «reún…», «varias celdas», «dos celdas de», «celdas
combinadas», «cada tipo»). Poner esa marca en un error cuyos textos genéricos supongan dos celdas
relaja I-6. Mutantes M10a-d, M13, M14, M17-M19 de `verificar_espacio.R`.

`calcula()` pura, `precondicion` por versión y selección por `sample()` sobre los aplicables (regla
#1; pre-flight 20 del orquestador).

## I-7 · Redacción del impreso

«Si se eligen al azar dos fichas de la bolsa, …» es del impreso y se conserva en la canónica. Si un
detractor la juzga ambigua, se **propone** al profesor; no se corrige por iniciativa propia (H-5). La
Solution sí puede aclarar la lectura (se compara la probabilidad de cada tipo de ficha). Desde el
2026-10-27 (detractor r3, H1) toda cifra n/N de la Solution y del feedback dice «en cada extracción» o
«una ficha extraída». Con dos extracciones, P(al menos una de 26 en 100) = 0,454, no 26/100.

Guardia de la pregunta (2026-10-27): en todas las versiones, `nchar(pregunta) == 123` y la suma
ponderada por posición es 720499, ambas calculadas desde `ITEM_OFICIAL.md` (mutante M15).

Guardia (riesgo R2 del dry-run, 2026-10-26): con la lectura «exactamente una de las dos fichas es del
tipo T», la probabilidad es proporcional a n(N − n), así que dos tipos empatan también cuando
n1 + n2 = N. Por eso cada opción cumple `n1 + n2 < N` (`stopifnot`) y todas las celdas son ≥ 1. En la
canónica ningún par es complementario: B es la única igualdad con las tres lecturas (al menos una,
exactamente una, ambas).

## I-8 · Fuera de la canónica, las cuatro opciones son agregadas

> **DECISIÓN DEL PROFESOR (2026-10-26)**, tomada en la pausa WAIT_USER #1 tras el dry-run, entre tres
> vías: todas agregadas (elegida), sortear el tipo de clave (descartada: en las versiones con clave de
> celdas sueltas basta leer sin sumar, y eso cambia el «¿Qué evalúa?» de la ficha) y medir primero
> (descartada: el resultado previsible obligaba a corregir igual y gastaba una pasada de §P7-D).
> Relajarla exige su firma (H-5).

**Lo medido** (dry-run, enumeración exacta sobre la canónica): la clave es una de las dos opciones
cortas (A = B = 66 caracteres; C = D = 102) y «categoría» solo aparece en los distractores. La regla
«elegir entre las dos cortas» acierta el 50 %: **+25 pp** sobre el 25 % del azar. La sonda H1 no lo ve,
porque busca una opción única en el extremo y aquí hay empates.

**Lo que rige:**

- En las versiones **no canónicas**, cada una de las cuatro opciones nombra un par de tipos
  **agregados**: cada tipo suma dos celdas, combinando dos de las tres dimensiones (color × figura,
  color × categoría, categoría × figura) de maneras distintas. Ninguna opción es de celdas sueltas.
- Así el canal corta/larga desaparece por construcción y **toda versión exige sumar**, que es lo que
  evalúa la ficha («Sumar frecuencias de una tabla y comparar la probabilidad de eventos…»).
- Los errores de la ficha (I-6) se encarnan en opciones agregadas cuyas **celdas parciales engañan**:
  por ejemplo, un par cuyas celdas de una sola categoría coinciden pero cuyas sumas difieren (lectura de
  una sola categoría). Desde el 2026-10-27, el cruce sin sumar (PROB-CRU) ya no se sortea fuera de la
  canónica (ver I-6); en su lugar va PROB-CER, cuyas sumas difieren en una ficha.
- Longitudes y vocabulario equilibrados entre opciones, verificados con la batería §P7 (familias de
  formato y léxico, más al menos una regla relacional). La clave no puede caer siempre en la misma
  combinación de dimensiones (§P4).
- La **canónica** conserva su estructura verbatim (A y B agregadas, C y D de celda suelta). Su canal de
  +25 pp se mide **aparte** y se declara como residuo de la instancia canónica: el profesor lo aceptó al
  elegir esta vía (override firmado de §P7-E, limitado a la canónica).
- **Ampliado por alcance (decisión del profesor, 2026-10-27):** el override cubre **cualquier regla de
  la batería congelada** en la instancia canónica, no una cifra. Allí la regla relacional «cruzada en su
  molde» da +75 pp, porque A y B son las únicas agregadas y cualquier regla que las separe da 0 % o 100 %.
  **Condición:** la canónica sale a lo sumo en el **10 %** de las versiones. El exceso en la mezcla es
  p_canon × 75 pp: +7,5 pp esperados. A eso se suma el ruido de muestreo de la parte no canónica: en
  las rondas del 2026-10-27 se midió entre +7,1 y +8,4. **La firma es por alcance (P_CANON ≤ 0,10), no
  por cifra**, así que una medición que roce el corte de +8 no la invalida. Guarda:
  `stopifnot(P_CANON <= 0.10)` junto al sorteo, con el sorteo anclado en el verificador; subir la
  proporción exige otra firma.
- Tras construir el pool agregado, volver a verificar por enumeración la unicidad (I-5) y la guarda de
  R2 (I-7).

## Decisiones del profesor (registro)

| Fecha | Decisión | Alcance |
|---|---|---|
| 2026-10-26 | **Flujo B: sí.** La tabla se reproduce como imagen, con sus celdas combinadas y el estilo del impreso (TikZ → Python → R hasta ≥ 98 % contra `graficador/originales/orig_enunciado_tabla.png`; el lenguaje lo elige el profesor en WAIT_USER #2) | PNG regenerado por versión, nombre neutral con sufijo `fig_id` (regla #4 v6.1), `{width=...}` (regla #18), texto alternativo con los datos de la tabla |
| 2026-10-26 | **Opciones agregadas fuera de la canónica** (I-8) | Versiones no canónicas; la canónica queda verbatim con su residuo declarado |
| 2026-10-26 | **Lenguaje gráfico: Python** (reticulate/matplotlib), elegido por el profesor en el chat principal antes de ver la tabla comparativa | Las tres versiones se generan igual (regla #3). Si Python no alcanza el 98 %, se vuelve a preguntar en WAIT_USER #2 |
| 2026-10-26 | **Umbral del Flujo B: se acepta el 98,3 % de Python, que es interpolado** (con lectura literal de la rúbrica ningún lenguaje llega a 98, porque falta la fuente Myriad Condensed del impreso), **con un ajuste**: el cuerpo de la tabla («Par», «Impar» y los números) va en tipo normal, como en el impreso; encabezados y «Blanca»/«Negra» quedan en negrita | Relajación firmada (H-5), limitada a la lectura de la rúbrica de similitud. La fidelidad celda a celda (H-3) no se relaja |
| 2026-10-27 | **Umbral del Flujo B: se firma el 96,8 %** que da la misma rúbrica tras el ajuste de peso (cuerpo en Source Sans 3 Regular, dilatación 0,45), en lugar del 98,3 % de la negrita. El tipo normal es más fiel al impreso; la rúbrica baja porque el escaneo engorda la tinta (curva medida: 0,25 → 94,2; 0,45 → 96,8; 0,60 → 97,3, ya seminegrita) | Sustituye la cifra de la fila anterior (detractor FASE 2C, H2 de r1 y H3 de r2). H-3 no se relaja |
| 2026-10-27 | **C de la canónica se analiza con PROB-CER** (ajuste de I-6) | Solo la instancia canónica |
| 2026-10-27 | **Residuo de la canónica firmado por alcance, con p_canon ≤ 0,10** (I-8) | Override de §P7-E; subir la proporción de canónicas exige otra firma |
| 2026-10-27 | **PROB-CRU solo en la canónica** (canal de estímulo, detractor FASE 2C r4, H1): en las no canónicas la regla «descarta la pareja cuyos dos tipos repiten un valor de celda» acertaba el 66,4 % (+41,4 pp), porque la clave nunca repite y PROB-UNA/PROB-CRU siempre (PROB-CRU además pasaba su cruce al espejo). Las no canónicas combinan PROB-UNA y PROB-CER más el espejo, sin repeticiones casuales: PROB-UNA es la única opción que repite, y la regla acierta 1/3 (**+8,3 pp, el mismo nivel del ítem oficial** con esa regla), que se **firma como residuo estructural** | Generador no canónico (I-6, I-8); cuenta como pasada de diagnosticidad (§P7-D) |
| 2026-10-27 | **Canales del detractor FASE 2C r5 (segundo desplazamiento; regla de parada).** (1) Se **firma como residuo estructural** la regla compuesta «descarta la pareja que repite y su gemela de molde»: 1/2 en las no canónicas (+25 pp), +20 pp en la mezcla, 0 % en el ítem oficial. Es el mismo hecho que el 1/3 ya firmado (la trampa repite, la clave no) y cerrarlo exigiría ampliar el pool. (2) **Pasada 2 de §P7-D, numérica:** las celdas de la clave salen del mismo rango que las demás (3..24, con `a` y `b` independientes), en lugar de 5..20. Simulado antes sobre copia: «algún número fuera de 5–20» +21,7 → −1,9 pp; «número mayor» +10,4 → −1,3; «celdas más juntas» +21,6 → +8,0 pp (zona del corte, no obliga; mezcla +4,7; oficial −25), que **se declara como residuo** | Generador no canónico; §P7-D: quedan una pasada y el cierre con residuo |
| 2026-10-27 | **Cierre por la regla de parada (detractor FASE 2C r6): se firma POR ALCANCE «cualquier encadenamiento de las firmas de repetición con reglas de magnitud sobre las celdas».** Cifras exactas (depuración M = 50.000 del detractor; réplica propia N = 2.000 en `salida/r6/cadenas_replica_depuracion_n2000.txt`): «repite» + «celdas más juntas» 47,6 % (+22,6 pp; mezcla +17,8); compuesta + «celdas más juntas» 64,8 % (+39,8 pp; mezcla +33,3); «celdas más juntas» sola 32,6 % (+7,6 pp). Las tres dan 0 % en el ítem oficial. No nacen de la pasada 2, que las bajó unos 15 pp; la causa es que la clave tiene sumas iguales, así que sus pares de celdas comparten el centro s/2. Exigen aprender entre versiones que la pareja que repite nunca es la clave. El verificador (E4c) las mide y falla si superan la cifra firmada | Cierre de §P7-D con residuo declarado (pasadas usadas: 2 de 3); la evidencia de aula dirá si alguien las explota |
| 2026-10-27 | Tabla a `{width=100%}` (auditoría visual a 360 px) y `exextra[TipoMetacognicion]: evaluacion_afirmacion` (describe la tarea del estudiante; el análisis de error ajeno sigue en la Solution, I-1) | Presentación y metadatos |

## Alineación

Los `exextra[...]` se copian **literales** de la ficha `MAT-2026-07-032`, con tildes (`Formulación y
ejecución`, `6°-7°`). Afirmación, Evidencia (FyE_E3), Descriptor (D2.3) y Estándar se cotejaron carácter
a carácter con el catálogo canónico el 2026-10-26: coinciden. El rótulo impreso por el editor («Comp.:
Interpretación y representación») **no es oficial** y no se usa. Todo-Pajaro es de **solo lectura**:
una discrepancia se reporta, no se corrige allí.
