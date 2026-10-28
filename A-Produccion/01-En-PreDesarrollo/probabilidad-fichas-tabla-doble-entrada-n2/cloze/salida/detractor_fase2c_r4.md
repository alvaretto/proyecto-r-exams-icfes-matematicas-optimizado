# Auditoría detractora FASE 2C, pasada r4: CLOZE `probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1`

**Fecha:** 2026-10-28
**Artefacto:** `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd`
- md5 `310c314673c5d188d1385f2148b3b1b0`: recalculado.
- Sin la línea 1008, el md5 es `945560602547bcd7bce941c5a1ad7ccc`: también recalculado.

**Independencia:** no escribí ni corregí el ejercicio y no hice r1, r2 ni r3. Trabajé en solo lectura. Las mediciones corrieron en el scratchpad. Comprobé que mi corrida no dejó archivos en el repo.
**Hallazgos:** 5. Ninguno es de CORRECCIÓN, ninguno es crítico ni alto; 2 son de severidad media y 3 baja. Ninguno pide cambiar el `.Rmd`.

## Resumen ejecutivo

**El cambio es exactamente una línea y hace lo que se pretende en Moodle.**
- En R/exams 2.4.2, `expoints` se asigna por posición, un peso por parte en el orden de `exclozetype`.
- Las 30 preguntas del XML del aula traen `3:MULTICHOICE_V|1:NUMERICAL|1:NUMERICAL|1:NUMERICAL|1:MULTICHOICE_V|1:MULTIRESPONSE` y `defaultgrade 8`.
- Moodle 4.5 recalcula la nota como Σ wᵢ·fᵢ / Σ wᵢ. La Parte 1 pasa de 1/6 a **3/8** de la pregunta.
- Claves, Solution, pool y guardas son los de r3, por el md5.
- **No hay mecanismo de sesgo en la Parte 5**: R/exams permuta cada gap con `o <- sample(len)`, independiente de la clave, y Moodle no vuelve a mezclar `MULTICHOICE_V`. Con N = 100 medí 47/100 (p = 0,62).
- El exportador corregido es correcto: 5 de 6 mutantes mueren y el que sobrevive es latente (H4).

**Lo que falta es documentación y despliegue, no el `.Rmd`.**
- **H1:** las firmas de hoy solo viven en el JSON del CLOZE. El H1 de r3 sigue abierto: ni el HANDOFF ni el `.claude/CLAUDE.md` local nombran el CLOZE.
- **H2:** los pesos también suben la pregunta de 6 a 8 puntos en cualquier cuestionario de Moodle, y eso no está declarado.

## Respuestas a los seis puntos del encargo

### 1. Semántica de `expoints` y su efecto en cada formato

**R/exams 2.4.2**
- `read_metainfo` lee `expoints` como numérico separado por `|`. En los cloze hace `expoints <- rep(expoints, length.out = slength)`: un peso por parte, por posición. **No comprueba la longitud** (ver H3).
- `make_question_moodle23` escribe `{points2[i]:TIPO:` por gap y `<defaultgrade>sum(points)</defaultgrade>`. Con pesos enteros, `points2 = points`.

**XML del aula** (`moodle_aula/probabilidad_fichas_cloze.xml`, n = 30)
- La secuencia `3:MULTICHOICE_V|…|1:MULTIRESPONSE` aparece en 30/30 preguntas, con `defaultgrade` 8 y `penalty` 0.
- El gap de peso 3 es el de la Parte 1: cuatro pares de tipos, con la clave marcada `=` (línea 44).
- El XML de `render_cloze.R` (`salida/moodle/cloze.xml`, 15:44:39) trae 5× `3:MULTICHOICE_V` y `defaultgrade 8`. Eso prueba que sale de la versión nueva.

**Moodle** (código de `MOODLE_405_STABLE`; la versión del servidor del aula no la comprobé)
- Al importar, `defaultmark` arranca en 0 y suma el peso de cada gap (`questiontype.php:419` y `:579`). `import_multianswer` no lee el `<defaultgrade>` del XML, pero el resultado coincide: 8.
- La calificación es `$fractionsum += $subfraction * $subq->defaultmark` dividido entre la suma de pesos (`question.php:297-311`).
- **Resultado:** la Parte 1 vale 3/8 (37,5 %) y cada una de las otras 1/8. Antes valía 1/6.

**Otros formatos**
- HTML, PDF y DOCX ignoran los pesos sin aviso: no aparece «points», «puntos» ni «3|1» en `salida/html`, `salida/tex` ni en el `pdftotext` de `salida/pdf`.
- Es lo esperable, porque esos formatos no califican, y no rompe nada. Pero si se califica en papel, la hoja no le dice a nadie que la Parte 1 pesa el triple.
- NOPS: N/A.

**Ningún script anula `expoints`.** El argumento `points` de `exams2moodle` se pasa a `xexams`, y ningún Semillero lo pasa con valor:
- `SemilleroUnico_v2.R` usa `points = NULL`;
- `SemilleroMoodle_v2.R` lo tiene comentado;
- `SemilleroCloze.R` no lo usa.

### 2. El exportador corregido

Repliqué su guarda literal sobre el XML real y le apliqué mutantes:

| Mutante | Resultado |
|---|---|
| Original | PASA (60 verticales, 30 multirrespuesta, 90 numéricos, 0 imágenes) |
| M1: Parte 1 a peso 1 en una sola pregunta | MUERE (`1|1|1|1|1|1`) |
| M2: `<img>` dentro del gap de la Parte 1 | MUERE (img = 1). **Con el patrón viejo `\{1:`, 0 detectadas**: el control era vacuo para la Parte 1 en cuanto tuvo peso 3 |
| M3: Parte 5 como desplegable | MUERE |
| M5: el peso 3 pasado a la Parte 2 | MUERE (`1|3|…`) |
| M4: `<img>` después de un `\}` escapado dentro del gap | **SOBREVIVE** (H4; hoy 0/30 gaps contienen `\}`) |

Un matiz: con el exportador viejo y el `.Rmd` nuevo, el conteo `vertical == 2n` habría fallado (30 ≠ 60). Ese conteo no era vacuo; el vacuo era el de imágenes. La guarda de pesos es posicional, y basta porque el orden de `##ANSWERi##` está fijado (V2) y M5 muere.

### 3. Nada más cambió respecto de r3

- **El md5 lo prueba en las dos direcciones.** `grep` encuentra `expoints` solo en la línea 1008.
- **Verificador:** `verificar_espacio_cloze_r4.txt` es idéntico byte a byte al de las 13:35. Es lo esperable: el `data_generation` es el mismo y el verificador no lee `expoints` (H3, H5). El E1 (la Parte 1 es el SCHOICE) sigue OK.
- **`render_cloze.R`** (12:27) y **`verificar_espacio_cloze.R`** (12:56) no se tocaron desde antes de r3.
- **El exportador es nuevo desde r3**, así que lo audité completo.

### 4. Posición de la clave de la Parte 5

**No hay mecanismo de sesgo: es ruido.**

**Por qué no puede haberlo**
- En el `.Rmd`, `o5 <- sample(2L)` y `sol_p5 <- nivel_p5 == mayor` (líneas 759-762).
- `read_exercise` de R/exams 2.4.2 llama, para cada gap schoice o mchoice de un cloze, a `shuffle_choice`. Con `exshuffle: TRUE` (lógico), esa función hace `o <- sample(len)`; la solución solo se consulta en la rama numérica.
- Una permutación uniforme e independiente de la clave, compuesta con cualquier permutación previa, sigue siendo uniforme. Por tanto, P(clave en la 1.ª posición) = 1/2 exacto, haga lo que haga `o5`.
- En Moodle, `MULTICHOICE_V` es `ANSWER_REGEX_ANSWER_TYPE_MULTICHOICE_REGULAR`, con `shuffleanswers = 0` (`questiontype.php:355`, `:373`, `:454-456`). El estudiante ve el orden del XML.

**Mediciones**

| Medición | Clave en la 1.ª posición |
|---|---|
| Mía, N = 100 (regla #23): `set.seed(4417)`, 100 versiones seguidas con `xexams`, el mismo read y shuffle de `exams2moodle` | **47/100** (p = 0,62). Antes del shuffle: 47/100. Tabla antes × después: 21/26/26/27, independientes. Parte 1: 30/23/24/23 (χ², p = 0,71) |
| XML del aula r4 (n = 30, 15:46) | 15/30 |
| Las cuatro del encargo | 60/100, 14/30, 17/24 y 60/100. **14/30 = 46,7 % no supera el 50 %**. La de `set.seed(20261028)` es la corriente que usó r3 en su exportación de 100 |
| Todo junto, sin la de 60/100 repetida | 153/284 = 53,9 % (p = 0,21) |

- Las tres previas distintas suman 91/154 (p = 0,029). Pero se midieron porque parecían altas, y la de 17/24 puede compartir semillas con la de 60/100.
- **Evidencia:** script en `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/ca84678d-286e-418b-a9bc-e7a8cd20624b/scratchpad/med_p5/medir_p5.R`, salida en `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/ca84678d-286e-418b-a9bc-e7a8cd20624b/scratchpad/med_p5/salida_4417.txt`.

### 5. Las notas de los pasos 3-6 del `ejercicio_state.json`

| Afirmación | Artefacto | Estado |
|---|---|---|
| md5 310c… / sin la línea, 9455… | recalculado | OK |
| «Verificador propio … APROBADO, 0 fallos» | `verificar_espacio_cloze_r4.txt` | Existe, pero no prueba sobre qué versión corrió (H5) |
| «la Solution no cambia» | md5 | OK |
| Render r4 en HTML, PDF, DOCX y Moodle | `render_r4.log`; archivos de 15:44:28 a 15:44:39; `moodle/cloze.xml` con peso 3 | OK |
| 30/30 con la secuencia de pesos, `defaultgrade 8`, 60/30/90, 0 imágenes | `exportar_r4.log` + XML (`defaultgrade` solo está en el XML) | OK |
| «mutante con la Parte 1 a peso 1: muerto» | ninguno en `salida/` | Es cierto (lo reproduje), pero no tiene artefacto (H5) |
| Arsenal: md5, 0/7, 2A, 2G 100/100, 2H con 10 semillas | `informe.txt` | OK |
| «2I-2O OK» | `informe.txt:81` y `:91` | Exagera: la 2K es N/A y la 2M no se ejecutó (H5) |
| «disparado … tras el render del 2026-10-28 16:01» | solo el nombre del directorio `20261028-160113` | Ningún artefacto dice qué comando fue |

**Sellado:** los cuatro pasos se sellaron en 22 s (16:02:45 → 16:03:07). Cada uno tiene un artefacto anterior al sello, así que la auditoría por artefacto lo acepta.

### 6. Overrides firmados de `SUB/.claude/CLAUDE.md`

**Los pesos no relajan ningún I-1…I-8.**
- Esas invariantes tratan del contenido de la Parte 1, que no cambió.
- Los pesos sí suben la parte de la nota expuesta al residuo canónico de I-8, de 1/6 a 3/8 del CLOZE. Eso sigue por debajo del SCHOICE, donde la Parte 1 es el 100 % y donde se firmó el residuo.
- La firma es por alcance (P_CANON ≤ 0,10) y se cumple (mutante M14). No hace falta otra firma de I-8.

**Sí hace falta registrar tres decisiones de hoy.** El texto que propongo está en H1. No edité nada.

## Hallazgos

### H1 — MEDIA (proceso): las decisiones del 2026-10-28 solo viven en el JSON del CLOZE

**Qué se cuestiona**
- El `.claude/CLAUDE.md` local no menciona `expoints`, ni el CLOZE, ni ninguna fecha 2026-10-28: su registro termina en la fila 164, del 2026-10-27.
- `HANDOFF.md:12` afirma que ahí están «todas las firmas del profesor», y la línea 14 sigue diciendo «11/11» del SCHOICE.
- El residuo H3 (`ejercicio_state.json:89`) y la herencia del Flujo B (`:28`) quedan sin registro persistente.

**Por qué**
- Regla #9 v1.5: el alcance del detractor incluye los overrides firmados y las invariantes locales.
- Regla #24 H-5: relajar exige firma humana rastreable. Aceptar el residuo H3 y extender al CLOZE el 96,8 % firmado para el SCHOICE son relajaciones.

**Riesgo concreto.** Quien retome con «Continúa con probabilidad-fichas-…» no sabe que el CLOZE existe. Podría:
- reabrir H3, aunque §P7-D está agotado;
- «corregir» `expoints`;
- regenerar la tabla creyendo que el 96,8 % solo vale para el SCHOICE.

**Alternativa propuesta** para el `.claude/CLAUDE.md` local:
- una sección «CLOZE (`cloze/`)»: I-1…I-8 rigen la Parte 1, que por código es el SCHOICE (E1);
- **I-9 · Pesos:** `expoints: 3|1|1|1|1|1`, la Parte 1 vale 3/8 y la pregunta 8 en Moodle; guarda en `exportar_moodle_cloze.R`; cambiarlos exige firma;
- cuatro filas nuevas en el registro, todas con fecha 2026-10-28:

| Decisión | Alcance |
|---|---|
| Pesos `3\|1\|1\|1\|1\|1` (WAIT_USER #3); la pregunta vale 8 en Moodle | CLOZE; cierra r1 H8 |
| Residuo estructural H3 de r1 firmado: la Parte 6 se resuelve sin la tabla y la Parte 5 presupone n1 ≠ n2 | Partes 5 y 6; reabrirlo exige otra firma |
| Herencia del Flujo B (Python del SCHOICE, sin regenerar) y del 96,8 % de la fila del 2026-10-27; H-3 no se relaja | Ampliación de alcance |
| Aprobación para el aula condicionada a r4, más los residuos de cierre del CLOZE: r1 H7 (22/100 en la Parte 5), lectura condicional con \|n1 − n2\| = 1, exclusión de X con empate, regla cruzada +6,7 pp en depuración | CLOZE |

En el HANDOFF: una sección del CLOZE con su estado, el exportador del aula, los pesos y los pendientes (pasos 8 a 11).

**Veredicto:** MODIFICAR (documentación).

### H2 — MEDIA (calificación en Moodle): los pesos también cambian lo que vale la pregunta en el cuestionario, de 6 a 8

**Qué se cuestiona.** `defaultgrade 8` está anotado (`ejercicio_state.json:52`), pero no su consecuencia.

**Por qué** (fuente de nivel 1, código de Moodle 4.5)
- Al importar, `defaultmark` es la suma de los pesos (`questiontype.php:579`).
- Al añadir la pregunta a un cuestionario, la nota máxima del espacio toma ese `defaultmark` (`mod/quiz/locallib.php:1772`).

**Riesgo concreto.** En un cuestionario que mezcle este CLOZE con otras preguntas, pesa un 33 % más que antes, y 8:1 frente al SCHOICE del mismo ítem. La decisión del profesor era interna a la pregunta. No afecta a la corrección de las respuestas.

**Alternativa propuesta**
- Mantener `3|1|1|1|1|1`. Con pesos no enteros, R/exams multiplica por 10ᵏ y avisa (`points2` en `make_question_moodle23`): 2,25|0,75… daría pesos 225|75 y un `defaultmark` de 600.
- Si se mezcla con otras preguntas, fijar en el cuestionario la «Puntuación máxima» del espacio. Moodle reescala la fracción y conserva la proporción 3:1:1:1:1:1 (`question.php:304-311`).
- Decírselo al profesor y anotarlo en el HANDOFF.

**Veredicto:** MANTENER el `.Rmd`; MODIFICAR la documentación del aula.

### H3 — BAJA (guarda): R/exams recicla `expoints` sin avisar y la única guarda vive en el exportador

**Qué se cuestiona**
- R/exams hace `rep(expoints, length.out = slength)` sin aviso.
- El `stopifnot` de la línea 818 cubre `exclozetype`, `exsolution` y `extol`, pero no los pesos, que son un literal (no hay `expoints_str`).
- `verificar_espacio_cloze.R` no menciona `expoints`.
- La guarda está solo en `exportar_moodle_cloze.R:43`.
- A las 16:04:41, después del sellado, aparecieron en `cloze/` tres exportadores más: `SemilleroMoodle_v2.R`, `SemilleroUnico_v2.R` y `SemilleroCloze.R`. Ya llevan `MULTICHOICE_V`, pero no tienen ninguna guarda de pesos.

**Por qué.** Memoria `feedback_invariante_sin_guardia`: si ningún mutante muere al revertirla, la invariante es documentación.

**Riesgo concreto.** Baja probabilidad. Una reapertura que añada, quite o reordene una parte sin tocar la línea 1008 movería los pesos en silencio por esas otras rutas. Por el exportador muere (M5).

**Alternativa propuesta** (endurecer es autónomo según H-5, y no toca el `.Rmd`): un E-check en el verificador que lea la línea de metadatos y exija `c(3,1,1,1,1,1)` con la longitud de `exclozetype`, con su mutante. En la próxima reapertura, generar `expoints` con R dentro del `stopifnot` de la línea 818.

**Veredicto:** MODIFICAR el verificador; no bloquea.

### H4 — BAJA (exportador): el control de imágenes se corta en el primer `}` del gap, aunque esté escapado

**Qué se cuestiona.** El patrón `[^}]*` de la línea 28. Moodle exige escapar `}` dentro de un gap como `\}`, y R/exams lo hace en los gaps de texto (`gsub("}", "\\}", …)`). M4 sobrevive.

**Riesgo concreto.** Hoy es latente: 0/30 preguntas tienen `\}`, y las Partes 2 a 6 no admiten cifras (`.Rmd`:802).

**Alternativa propuesta** (probada):

```r
gregexpr("\\{[0-9]+:(?:MULTICHOICE|MULTIRESPONSE)(?:\\\\.|[^}\\\\])*", x, perl = TRUE)
```

- Sobre el original da lo mismo que el patrón actual: 90 gaps, 0 imágenes, largo medio 271.
- Detecta M2 y M4.

**Veredicto:** MODIFICAR (opcional).

### H5 — BAJA (trazabilidad): tres afirmaciones de las notas sin artefacto o con más alcance del que prueban

1. **«2I-2O OK»** (`:60`): la 2K es «⊘ N/A» y la 2M es solo el recordatorio de ejecutar el auditor visual. Lo correcto es decir que no se ejecutó porque el cambio no altera el HTML, y escribirlo así.
2. **El mutante del exportador** (`:52`): no tiene artefacto. Propongo guardar la corrida en `salida/`.
3. **El verificador** (`:37`): su salida no lleva md5 (el script no lo calcula), es idéntica a la de las 13:35 y no lee `expoints`. Prueba que no hubo regresión, cosa que el md5 ya prueba, pero no qué versión verificó. Que imprima el md5 del `.Rmd` en la cabecera.

**Veredicto:** MODIFICAR las notas.

## Verificado sin objeción

- El cambio es de una sola línea (md5 en los dos sentidos).
- Los pesos son posicionales, uno por gap y en el orden de `exclozetype`, en R/exams y en el XML.
- Moodle califica como Σ w·f / Σ w.
- Moodle no vuelve a mezclar `MULTICHOICE_V`.
- La Parte 5 es uniforme por construcción y lo medí. La Parte 1 también es uniforme.
- Mueren los mutantes M1, M2, M3 y M5.
- El render r4 viene de la versión nueva.
- Ningún script pasa `points`.
- I-1…I-8 no se relajan.
- r3 sigue valiendo para claves, Solution, pool y guardas: **no hay hallazgos de CORRECCIÓN**.

## No auditado

- La importación en el servidor Moodle del aula: lo contrasté con el código de 4.5, pero no comprobé la versión instalada.
- La inspección visual del PDF y el HTML de r4: el cambio no altera nada visible (lo comprobé con grep).
- El DOCX: no lo abrí.
- Los tres Semillero de `cloze/` de las 16:04: solo busqué `points`, `cloze` y `pesos` con grep.
- Los datos crudos de las cuatro mediciones previas: no los tuve.

## Veredicto global: APROBAR_CON_CAMBIOS

Ninguna objeción es crítica ni alta, así que la FASE 3 no queda bloqueada. Antes de usar el CLOZE en el aula:
1. Registrar las firmas y la sección del CLOZE (H1).
2. Declarar al profesor la nota máxima en el cuestionario (H2).

Los demás son opcionales: H3 y H4 (endurecer) y H5 (corregir las notas). Después, sellar `detractor_fase2c` con este reporte y seguir con los pasos 8 a 11.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/.claude/CLAUDE.md | AUSENTE: expoints
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/.claude/CLAUDE.md | AUSENTE: 2026-10-28
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/.claude/CLAUDE.md:164 | (auditoría visual a 360 px) y
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/HANDOFF.md | AUSENTE: CLOZE
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/HANDOFF.md:12 | todas las firmas del profesor
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/HANDOFF.md:14 | ## Estado: 11/11, aprobado para el aula por el profesor el 2026-10-27
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/ejercicio_state.json:89 | H3 de r1 declarado residuo estructural.
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/ejercicio_state.json:28 | "origen": "herencia",
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/salida/moodle_aula/probabilidad_fichas_cloze.xml:133 | <defaultgrade>8</defaultgrade>
H2 | /tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/ca84678d-286e-418b-a9bc-e7a8cd20624b/scratchpad/moodle_src/question_type_multianswer_questiontype.php:579 | $question->defaultmark += $wrapped->defaultmark;
H2 | /tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/ca84678d-286e-418b-a9bc-e7a8cd20624b/scratchpad/moodle_src/question_type_multianswer_question.php:304 | $fractionsum += $subfraction * $subq->defaultmark;
H2 | /tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/ca84678d-286e-418b-a9bc-e7a8cd20624b/scratchpad/moodle_src/mod_quiz_locallib.php:1772 | $slot->maxmark = $DB->get_field('question', 'defaultmark', ['id' => $questionid]);
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/ejercicio_state.json:52 | defaultgrade 8, 0 desplegables
H3 | /tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/ca84678d-286e-418b-a9bc-e7a8cd20624b/scratchpad/rexams_read_metainfo.txt:148 | expoints <- rep(expoints, length.out = slength)
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:818 | stopifnot(all(lengths(strsplit(c(exclozetype_str, exsolution_str, extol_str)
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd | AUSENTE: expoints_str
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/verificar_espacio_cloze.R | AUSENTE: expoints
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/exportar_moodle_cloze.R:43 | length(pesos) == n, all(pesos == "3|1|1|1|1|1"),
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/SemilleroMoodle_v2.R:113 | cloze = list(cloze_schoice_display = "MULTICHOICE_V"),
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/SemilleroMoodle_v2.R | AUSENTE: pesos
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/exportar_moodle_cloze.R:28 | MULTIRESPONSE)[^}]*", x))[[1]]))
H4 | /tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/ca84678d-286e-418b-a9bc-e7a8cd20624b/scratchpad/rexams_make_question_moodle23.txt:359 | gsub("}", "\\}", solution[[i]][j]
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:802 | "V11 por valor: las Partes 2 a 6 no imprimen cifras"
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/ejercicio_state.json:60 | 2H PASA (10 semillas), 2I-2O OK
H5 | /home/bootcamp/.cache/post-exams2-validation/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1/20261028-160113-595160/informe.txt:81 | FASE 2K: el .Rmd no usa tablas markdown
H5 | /home/bootcamp/.cache/post-exams2-validation/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1/20261028-160113-595160/informe.txt:91 | Paso del workflow — ejecutar el auditor visual masivo
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/ejercicio_state.json:52 | mutante con la Parte 1 a peso 1: muerto
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/ejercicio_state.json:37 | Verificador propio sobre la versión nueva: APROBADO, 0 fallos
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/verificar_espacio_cloze.R | AUSENTE: md5sum
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
