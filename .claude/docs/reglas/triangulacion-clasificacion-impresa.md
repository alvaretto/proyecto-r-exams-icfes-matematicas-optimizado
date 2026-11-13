# Regla #27 — Clasificación impresa en el original: triangular y decidir

> Versión compacta, la que se carga en cada sesión: `.claude/rules/triangulacion-clasificacion-impresa.md`. Este es el texto
> íntegro: origen, mediciones, procedimiento, primer caso e historial. Se lee antes de aplicar la regla a un original que trae
> un rótulo distinto del de SAI, antes de decidir contra la ficha y antes de modificar la regla.

## Principio

Si el original escaneado trae impresa una clasificación del ítem (competencia, componente, afirmación o evidencia, nivel,
descriptor, estándar, o códigos que los cifran), esa línea es un **voto**. No se recorta como ruido y no se copia como verdad:
se transcribe, se decodifica con el catálogo canónico y se triangula, dimensión por dimensión, con las dos fuentes locales
(la ficha de alineación y una clasificación propia hecha a ciegas). Cada discrepancia se cierra con una **decisión**
registrada. Dejarla como «observación para el profesor» no es un cierre.

## Origen

Instrucción del profesor del 2026-11-13, durante el paso 1 de `serie-seno-aproximacion-n3` (`MAT-2026-07-017`):

> «Cuando el archivo Original traiga (Componente, competencia, nivel, descriptor y/o similares) triangular con local y tomar
> decisiones. Conviértelo en Regla/LEY»

Lo que la motivó: el orquestador vio en la plana el rótulo «A: 2.3 M / Compt.: Numérico-variacional / Comp.: Formulación y
ejecución», notó que «2.3» podía no ser la evidencia que asigna la ficha y lo dejó como «observación declarada para el
profesor (no cambia la clasificación)», sin tercer voto y sin decisión.

## Qué cambia frente a la práctica anterior

| Antes | Desde esta regla |
|---|---|
| El recorte del ítem dejaba fuera la línea del rótulo, «que no es oficial» (`ITEM_OFICIAL.md` de `vueltas-llanta-perimetro-n4`) | El recorte de trabajo la incluye y el rótulo se transcribe literal; el recorte del voto a ciegas la excluye |
| La ficha se clasificó sin ver el rótulo y la concordancia se publicó como dato informativo del simulacro entero | La concordancia se resuelve ítem por ítem, en las dimensiones que el rótulo trae |
| Una diferencia quedaba como observación para el profesor | Se decide por mayoría de tres votos; al profesor solo se le pregunta el empate a tres |

Que la ficha se haya clasificado a ciegas del rótulo no se toca: es lo que hace que F y E sean votos independientes.

## Procedimiento

### T-1 Transcribir

- En el paso 1 (`analisis_icfes`) se mira el recorte del ítem **con** la franja que queda bajo las opciones.
- El rótulo se copia literal, con su puntuación, en `ITEM_OFICIAL.md`.
- **Cuál rótulo es el del ítem.** En los cuadernillos SAI el rótulo va debajo de las opciones de su ítem. El que queda justo
  encima del número impreso pertenece al ítem anterior. Medido en `Originales/pagina_005.jpg`: sobre el «17.» está «A: 2.3 M /
  Compt.: Aleatorio / Comp.: Formulación y ejecución», que es de la pregunta 16; el de la 17 está bajo sus cuatro fracciones y
  dice «Numérico-variacional». Rige la regla #24 H-4: ancla en el número impreso y crop al borde del contenido.
- Si el original no trae clasificación impresa, se declara en `ITEM_OFICIAL.md` («Rótulo impreso: no trae») y la regla termina ahí.

### T-2 Decodificar

- Cada código se traduce al **texto literal** del catálogo canónico (`catalogos-oficiales-mat/` en Matemáticas).
- Se cita la clave de lectura y cómo se comprobó. Los identificadores internos del catálogo (`FyE_E3`) son una convención
  propia: que coincidan con el número impreso se comprueba contra la fuente oficial, no se supone.
- Un código que no se puede decodificar no vota en esa dimensión, y se declara.

**Clave de lectura del rótulo SAI (Los Tres Editores), comprobada el 2026-11-13:**

| Trozo del rótulo | Significado | Comprobación |
|---|---|---|
| `A: X.Y` | Evidencia número X.Y de la Guía de orientación Saber 11.° 2026-2, Tablas 14 a 16 (págs. 31-33; md5 del PDF `de3957834512ac791e9faebc3cf44c6f`). X es la competencia (1 Interpretación y representación, 2 Formulación y ejecución, 3 Argumentación) y, con ella, la afirmación | `pdftotext -f 31 -l 33`: 1.1 y 1.2; 2.1 «Diseña planes…», 2.2 «Ejecuta un plan…», 2.3 «Resuelve un problema…»; 3.1, 3.2 y 3.3. El orden coincide con `IyR_E1-E2`, `FyE_E1-E3` y `Arg_E1-E3` del catálogo |
| `M` | Sin decodificar (todas las preguntas de la plana lo traen); no vota | — |
| `Compt.:` | Componente | Texto directo |
| `Comp.:` | Competencia | Texto directo; su valor coincide con el primer dígito de `A:` en las 50 preguntas (nota de procedencia de la ficha del simulacro) |

El rótulo SAI no trae nivel, descriptor ni estándar: esas dimensiones no se triangulan con él.

### T-3 Tres votos

Solo en las dimensiones que el impreso trae:

| Voto | Fuente | Condición |
|---|---|---|
| **E** (impreso) | El rótulo decodificado en T-2 | — |
| **F** (ficha) | La ficha de alineación local del ítem | Si la ficha se hizo viendo el rótulo, se declara: F y E dejan de ser independientes |
| **P** (propia) | Un agente distinto del que escribe el ejercicio (`ClasificadorICFES`), lanzado sin `name:` | Ve el recorte **sin** el rótulo y el catálogo; no ve la ficha, la transcripción ni `ITEM_OFICIAL.md`. Su reporte cierra declarando si la ceguera se cumplió; si se rompió, el voto no cuenta y se repite |

Las dimensiones que el impreso no trae siguen la ficha, como antes.

### T-4 Decidir

| Resultado | Decisión |
|---|---|
| 3 de 3 | Concordante. Se usa ese valor |
| 2 de 3, con la ficha en la mayoría | Se usa la ficha. El voto discrepante queda en la tabla |
| 2 de 3, con la ficha en minoría (E = P ≠ F) | El ejercicio adopta la mayoría en `ITEM_OFICIAL.md` y en `exextra`. La ficha se reporta al profesor como candidata a corrección: vive en Todo-Pajaro y no se edita desde este repo |
| 1-1-1, o sin voto P utilizable | No hay decisión autónoma. Se le pregunta al profesor en lenguaje llano, con el texto literal de cada candidato y lo que el ítem le pide hacer al estudiante |

Además:

- **Quién imprime.** Si la clasificación la imprime el ICFES (cuadernillo o guía oficial del ítem), prevalece sobre la ficha y
  los otros dos votos se registran igual. La de un editor comercial es un voto entre tres. Es el mismo criterio con el que una
  clave oficial corrige una errata del editor (regla #24 H-2, aclaración de la v3.41.1).
- **Literalidad.** El valor decidido existe carácter por carácter en el catálogo (regla #7) y lo comprueba
  `validar_literalidad_exextra.py --item` en el paso 10.
- **Nombre del archivo.** Competencia, componente y nivel forman parte del nombre del `.Rmd`. Si la decisión cambia alguno con
  el `.Rmd` ya escrito, se reabre con `workflow-state.sh reopen` y se renombra (regla #16 y `NOMENCLATURA_ARCHIVOS_RMD.md`).
- **Peso del rótulo.** El rótulo solo nunca le gana a la ficha: hace falta que la clasificación a ciegas coincida con él.

### T-5 Registrar

En `ITEM_OFICIAL.md`, sección «Triangulación de la clasificación», y la misma tabla en el reporte del paso 10:

```markdown
Rótulo impreso (literal): «…»

| Dimensión | Impreso (E) | Ficha (F) | Propia a ciegas (P) | Decisión | Razón |
|---|---|---|---|---|---|
| Competencia | … | … | … | … | 3 de 3 |
```

La tabla nunca tiene dos columnas: `validar_literalidad_exextra.py --item` lee como «Campo | Valor» toda fila de dos columnas
del archivo. Sin esa tabla con todas sus decisiones no se sella `validar_icfes`.

## Mediciones

Concordancia del rótulo SAI con la ficha en el simulacro «Nivelación 6» de décimo (50 preguntas de Matemáticas, julio de 2026),
tomada de la nota de procedencia de la ficha (`Alineacion-curricular-de-items-Matematicas-Decimo-Jul-2026.md`, clasificación del
2026-10-17 hecha sin ver el rótulo):

| Dimensión | Coinciden | Porcentaje |
|---|---:|---:|
| Competencia | 39 de 50 | 78 % |
| Componente | 45 de 50 | 90 % |
| Evidencia | 21 de 50 | 42 % |

En evidencia, el rótulo y la ficha difieren en más de la mitad de las preguntas. Por eso el rótulo no puede copiarse como verdad
ni la diferencia dejarse sin resolver: 29 de los 50 ítems de este simulacro llegan al paso 1 con ella.

## Primer caso: `MAT-2026-07-017` (2026-11-13)

Rótulo impreso (literal): «A: 2.3 M / Compt.: Numérico-variacional / Comp.: Formulación y ejecución».

| Dimensión | Impreso (E) | Ficha (F) | Propia a ciegas (P) | Decisión | Razón |
|---|---|---|---|---|---|
| Competencia | Formulación y ejecución | Formulación y ejecución | Formulación y ejecución | Formulación y ejecución | 3 de 3 |
| Componente | Numérico-variacional | Numérico-variacional | Numérico-variacional | Numérico-variacional | 3 de 3 |
| Afirmación | La de la competencia 2 | La de Formulación y ejecución | La de Formulación y ejecución | La de Formulación y ejecución | 3 de 3 |
| Evidencia | 2.3 «Resuelve un problema que involucra información cuantitativa o esquemática.» | 2.2 «Ejecuta un plan de solución para un problema que involucra información cuantitativa o esquemática.» | 2.2 «Ejecuta un plan de solución…» (confianza media; segunda opción, 2.3) | **2.2 «Ejecuta un plan de solución…»** | 2 de 3, con la ficha en la mayoría |

- Voto P: `ClasificadorICFES` sobre un recorte de 640 × 383 px sin la línea del rótulo (caja 150, 1395, 790, 1778 de
  `pagina_005.jpg`), con `evidencias-mat.json` como única fuente. Cerró con «CEGUERA: cumplida». Su razón: el enunciado ya fija
  el plan (usar los tres primeros términos) y el estudiante lo ejecuta.
- Nivel, descriptor y estándar: el rótulo no los trae; siguen la ficha.
- Resultado: la clasificación del ejercicio no cambia, y la discrepancia en la evidencia queda decidida en vez de pendiente.

## Límites y defensa

- **Sin guarda mecánica (v1.0).** La regla viaja en el contexto de cada sesión y de cada subagente porque su compacta está en
  `.claude/rules/`. La aplican el paso 1 y el paso 10 de los orquestadores y la audita el detractor en el dominio ICFES. Nada
  ejecutable impide sellar `validar_icfes` sin la tabla.
- **Dónde está nombrada (desde la v3.43.1 del índice, 2026-11-13):** los dos orquestadores (párrafo del paso 1 y frase del
  paso 10) y sus comandos, los skills `analizar-icfes` y `validar-icfes`, el agente `clasificador-icfes` (modo voto a ciegas) y
  el dominio ICFES del detractor. Las definiciones de los agentes se cargan de HEAD: rigen desde la sesión siguiente al commit.
- **Guarda posible**, si la regla se incumple: un validador que exija en `ITEM_OFICIAL.md` la línea «Rótulo impreso» y, cuando
  no diga «no trae», la tabla con sus seis columnas y una decisión por fila.
- **Solo Matemáticas tiene catálogo canónico montado en este repo.** En otra área, T-2 usa el catálogo de esa área en
  Todo-Pajaro, y si no existe, el voto E no se puede decodificar y se declara.
- **Ejercicios anteriores:** se ajustan en el próximo ciclo de cada uno, sin migración en bloque.

## Historial

| Versión | Fecha | Cambio |
|---|---|---|
| 1.0 | 2026-11-13 | Regla nueva, por instrucción del profesor. Clave de lectura del rótulo SAI comprobada contra la Guía 2026-2. Primer caso: `MAT-2026-07-017` |
| 1.0 | 2026-11-13 | Sin cambio de norma: la regla queda nombrada en los orquestadores, sus comandos, `analizar-icfes`, `validar-icfes`, `clasificador-icfes` y el dominio ICFES del detractor (v3.43.1 del índice) |
