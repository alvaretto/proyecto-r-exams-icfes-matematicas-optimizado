<!-- Reporte íntegro del AgenteDetractor de la FASE 2C r5 (tanda S2, .Rmd 6c9d7705), copiado de su transcripción por el coordinador el 2026-11-03 (mensaje final: 2026-11-03T12:39:09.868Z). -->

# FASE 2C r5: auditoría del SCHOICE `totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1`, tanda S2

**Fecha:** 2026-11-03.
**Objeto:** el `.Rmd` con md5 `6c9d77058b32fdd8291a1ebaab581c8f` y `verificar_espacio.R` con md5 `bd6eda09d54c8dae355371e291df95f6`. Comprobé los dos con `md5sum` antes de empezar y coinciden.
**Independencia:** no audité ninguna versión anterior de este ejercicio y no escribí el artefacto. Trabajé solo en lectura: no modifiqué nada del repo y todas mis salidas están en `/home/bootcamp/.cache/fase2c_r5/`.
**Objeciones:** 5. No hay críticas ni altas; hay 1 media y 4 bajas. **No hay ninguna de CORRECCIÓN.**

## Resumen

La clave, la Solution y el feedback son correctos en las 100 versiones que comprobé con una función de verdad propia. Esa función lee solo el texto emitido y la tabla mostrada. La canónica reproduce el impreso, y lo comprobé contra `ITEM_OFICIAL.md` y contra el PNG.

Repetí `verificar_espacio.R`: sale con exit 4 y su salida es idéntica línea por línea a la registrada. Las 24 cifras de la decisión 2 a y las de «O-1 corregido» son las firmadas. MP1, MP2 y los mutantes del 23 al 33 mueren cada uno por su sonda.

Los hallazgos están en las salvaguardas y en el registro, no en el ítem:
- **H1.** Los controles positivos de S1-Sol fórmula y S1-Sol signo siguen sin notar que su cuenta baja. Es el mismo defecto que la S1b corrigió en el patrón, y lo demuestro con un mutante.
- **H2.** El residuo «fórmula sin unidad seguida de un signo» está bien contado, pero «0 partidas en 42 casos» exagera lo que se midió.
- **H3.** La cláusula V1 tiene una guarda con el prefijo «L-6:», pero no está en la tabla firmada ni en la candidata.
- **H4 y H5.** El instrumento HTML afirma algo falso sobre su propio código, y el `ok()` de S7 es tautológico.

## Hallazgos

### H1. Los controles positivos de S1-Sol fórmula y S1-Sol signo siguen ciegos a una caída de su cuenta (MEDIA, cobertura del verificador)

**Qué se cuestiona.**
- Control de S1-Sol fórmula: `n_dentro >= N` (línea 799). Hoy vale 1158 y el umbral es 100, así que seguiría en OK aunque cayera un 91 %. La S1b arregló el patrón que había bajado a 428, pero dejó el umbral.
- Control nuevo de S1-Sol signo: `all(v_dentro_s > 0)` (línea 820). Hoy suma 730, unos 7,3 por versión, y solo exige al menos uno por versión.
- Las tres sondas son una lista negra de separadores. `PAT_S1F` (línea 795) solo admite espacio normal o U+00A0 entre `$` y la unidad.

**Evidencia** (`/home/bootcamp/.cache/fase2c_r5/indep.R`, semillas 1 a 100). Tomé un mutante en memoria: la lista de totales del procedimiento correcto (línea 649) saca la unidad de la fórmula con U+2009, es decir `$ kilos.` con espacio fino. Resultado:

| Sonda o control | Original | Mutante |
|---|---|---|
| La sonda S1 (texto plano) | 0 | 0 |
| La sonda S1-Sol fórmula | 0 | 0 |
| La sonda S1-Sol signo | 0 | 0 |
| Unidad dentro de la fórmula | 1158 | 958 (sigue ≥ 100: OK) |
| Unidad con el signo dentro | 730, en 100/100 versiones | 530, en 100/100 versiones (OK) |
| Unidades fuera con U+2009 | 0 | 200 (nadie lo ve) |

Estas son las únicas partes del verificador que leen la Solution emitida. El mutante, por tanto, pasaría sin ser visto. U+2060, que propuso el auditor de la 6b de S1, cae en el mismo hueco por la clase de `PAT_S1F`. Eso lo deduzco del regex, no lo medí.

**Alternativa** (endurecer, autónomo; solo toca el verificador):
1. Cablear los conteos exactos, que son deterministas con las semillas fijas: `n_dentro == 1158` y `sum(v_dentro_s) == 730`. Es el mismo criterio que ya rige las cifras firmadas.
2. Añadir un mutante que quite un solo sitio y obligue a que el control falle.
3. Ampliar la clase de las sondas a cualquier carácter que no sea letra ni cifra entre `$` y la unidad, por ejemplo `\$[^A-Za-z0-9$\n]{0,3}(kilos?|cursos?)\b`.

**Veredicto:** MODIFICAR.

### H2. Residuo «fórmula sin unidad seguida de un signo»: el conteo es correcto, pero la evidencia de «0 partidas» está sobrevalorada (BAJA, visual y medición)

**Qué se cuestiona.** El HANDOFF dice: «El mecanismo es el mismo, pero en HTML hubo 0 partidas en 100 semillas por ancho (42 casos)».

**Evidencia.**
- **El conteo es correcto.** Reproduje los 42 casos en las semillas 1 a 100: son exactamente las 42 versiones en que el distractor de G es ERR-RESTA. Es el único `$.` sin unidad del fuente, en la línea 262.
- **Los 42 casos no son 42 ensayos.** La fórmula abre su propio párrafo tras un prefijo fijo: «La cifra δ sale de restar los promedios: $máx − mín = δ$.», en las líneas 261-262. Solo varía el número de dígitos de δ. En cada ancho, los 42 casos comparten prácticamente la misma geometría: son 2 observaciones (360 y 320 px de `exams2html`), no 84.
- **Aunque fueran independientes, el 0 no descarta el riesgo.** Con la tasa de V1 en los cierres con unidad (34/730, un 4,7 % a 360 px) se esperarían 2,0 casos partidos en 42, y la probabilidad de ver 0 sería de un 13 %.
- **No se midió en el Moodle del colegio.** No se renderizó la caja real (310 y 270 px, con MathJax). La cifra de 40 sale de un regex sobre el XML, que solo cuenta casos.

**Riesgo concreto.** Es tipográfico y solo afecta a la Solution: un «.» al principio de renglón. Es el mismo defecto que el profesor mandó cerrar con V1′.

**Alternativa.**
- (a) Antes del WAIT_USER #3, redactar el residuo como «0 en las dos geometrías medidas; sin medir en la caja del Moodle del colegio».
- (b) O cerrarlo como V1′: `p$delta, "\\text{.}$ Esa resta…"`, y generalizar la sonda de signo a toda fórmula (`\$[.,:;!?)\]»”…]`). Pasaría de 42 casos por 100 versiones a 0. Toca la Solution, así que exige reapertura y decide el profesor.

**Veredicto:** MODIFICAR la redacción; el cierre es opcional.

### H3. La cláusula V1 tiene una guarda «L-6:», pero no está ni en la tabla firmada ni en la candidata (BAJA, registro de invariantes, regla #9 v1.5)

**Qué se cuestiona.**
- El `.Rmd` tiene dos `stopifnot` con el prefijo «L-6:»: el de la línea 591 («la unidad que cierra una fórmula va dentro de la fórmula») y el de la línea 596 (el del signo).
- La tabla firmada de L-6 (líneas 1320-1332 del CLAUDE.md local) tiene 6 filas y ninguna trata de fórmulas.
- La tabla candidata (líneas 1473-1477) tiene una sola fila, la del signo (V1′).
- V1 solo aparece como relato, en la línea 1392.

**Riesgo concreto.** Al firmar en el WAIT_USER #3 «la cláusula candidata», V1 quedaría como una guarda sin firmar que lleva el nombre de una ley firmada.

**Alternativa.** Añadir a la tabla candidata esta fila:
- Exige: «la unidad que cierra una fórmula va dentro de ella», `$… = T\ \text{kilos}$`.
- Guardas: el `stopifnot` de la línea 591 y la sonda S1-Sol fórmula, con un control exacto según H1.
- Mutantes: M30 y M31.

**Veredicto:** MODIFICAR el registro.

### H4. `cap_s1b2.py` afirma que no lleva caracteres literales y lleva 10 U+00A0 literales (BAJA, instrumento)

**Qué se cuestiona.** La línea 12 del instrumento dice «Todas las clases de caracteres van con escapes … sin caracteres literales». Sin embargo, `UN`, `EL`, `MU` y las comparaciones `nb` y `s[0]` (desde la línea 37) llevan U+00A0 literal: conté 10.

**La medición es correcta.** El denominador «cifra-unidad» vale 2403 en las cinco versiones e incluye los 2003 pares con U+00A0 fuera de la tabla. Con solo espacios normales en la clase, 4cffbe8b habría dado 400. El problema es que es la misma trampa que vigila el mutante 29: la herramienta de edición convierte el escape en literal sin avisar, y aquí volvió a pasar sin que nada lo detectara.

**Alternativa.** Reescribir las clases con `\u00a0`, que JavaScript acepta dentro de un regex, y añadir `assert "\u00a0" not in JS`. La otra opción es corregir la descripción.

**Veredicto:** MODIFICAR.

### H5. El `ok()` de S7 es tautológico y el campo `lado` no tiene consumidor (BAJA, guarda inalcanzable)

**Qué se cuestiona.**
- `rasgos()` calcula `lado` a partir de `afs$gdis$x > d$D` (línea 79). El `ok()` de la línea 108 exige `lado == "encima"` y `X > D`, que equivale a pedir `X > D`.
- En la canónica X = 40 y D = 20 están fijados por la guarda verbatim, que aborta antes. Por eso el `ok()` no puede fallar por sí solo.
- En `verificar_espacio.R` ninguna sonda lee `x$lado`: solo aparece en las líneas 79 y 108-109. El CLOZE arma el suyo en `rec()`, línea 117.

**Alternativa.** Hay dos opciones:
- Quitar el campo y el `ok()`.
- Convertirlo en una guarda real del canal 2 en las versiones no canónicas: `all(vapply(c(NAT, CU$CURSOS, CU$PROMEDIO), function(x) x$canon || ((x$X < x$D) == (x$lado == "debajo")), TRUE))`, con un mutante que quite el filtro `lado_ok`.

**Veredicto:** MODIFICAR.

## Lo que comprobé y salió bien

**CORRECCIÓN.** La comprobé con una función de verdad propia (`/home/bootcamp/.cache/fase2c_r5/indep.R`). Lee solo el enunciado, la tabla y las opciones emitidas, no `verdad()`. La muestra fue N = 100: 9 canónicas y 44 de la rama CURSOS.

| Comprobación | Resultado |
|---|---|
| Exactamente una afirmación verdadera | 100/100 |
| Coincide con `exsolution` | 100/100 |
| «La respuesta correcta es la que afirma: «…»» cita esa opción | 100/100 |
| El feedback dice «Verdadero.» o «Falso.» en el orden correcto | 100/100 |
| La lista de totales y «Ganó X, con TG − TP = D» son coherentes con la tabla | 100/100 |

**Canónica (semilla 7)** (`/home/bootcamp/.cache/fase2c_r5/canon.R` y `originales/orig_pregunta_50.png`):
- Las opciones, con U+00A0 pasado a espacio, son iguales a las líneas 71-74 de `ITEM_OFICIAL.md` y al PNG.
- El enunciado es igual a las líneas 38-40, y la tabla, celda a celda (encabezado con «recolectado»).
- Lleva U+00A0 solo en «40 kilos», «20 kilos» y «5 kilos»: 1, 1, 1 y 0 por opción. Se cumple H-2.

**Espacio duro (S1).**
- `ED <- "\u00a0"` está en la línea 24, y el `.Rmd` tiene 0 bytes `C2 A0`.
- En las opciones, la Solution y el feedback emitidos: 0 cifras o `$` separados de «kilos» o «curso(s)», y 0 casos de «El/La» separados de «Bosque/Pradera». Lo comprobé también con espacio fino y con espacio duro estrecho (U+202F).
- En el enunciado, fuera de la tabla: 0.

**V1′.**
- El fuente tiene 20 cierres en el código, 14 de ellos con signo (`kilos.}$` 11, `kilos:}$` 2, `kilos,}$` 1), más 3 en comentarios.
- En lo emitido: 1158 unidades dentro de la fórmula, 730 con el signo dentro y 0 fuera.

**L-6 firmada.** Cada guarda es alcanzable y cada mutante muere por su sonda:
- Los mutantes 23 y 24 mueren en la semilla 7, y el 25 en la 6.
- El 26 muere en la semilla 1, por la guarda del complemento, que va antes de S1.
- El 27 se detecta en 100 casos.
- El 28 sale con exit 3 y 1 pendiente, el 6,7 % en palabras.
- El 29 se detecta.

**Mutantes del 30 al 33.** Los patrones del 30 y del 33 aparecen una sola vez en el fuente (línea 581). El 30 y el 33 mueren por su `stopifnot` en la semilla 1. El 31 da 100 casos, con 0 en S1-Sol. El 32 da 200 casos, con 0 en las otras dos sondas.

**MP1 y MP2** mueren por la razón que declaran:
- MP1: tras el cambio, solo B conserva «promedio». «Quedarse con 'promedio'» da +75,0 pp frente a +25,0 firmados.
- MP2: tras el cambio, solo B tiene dos dígitos. «Más dígitos» da +75,0 pp.

**Cifras firmadas.**
- Las 24 cifras de `FIRMA_P1_CANON` y `FIRMA_P1_SONDAS` coinciden una a una con la tabla de la decisión 2 a.
- `FIRMA_O1` (10 celdas), `FIRMA_S1_PROMEDIO` (0,084) y `FIRMA_LARGO` coinciden con «O-1 corregido».

**Decisión (a).**
- `norm_ed()` actúa solo en la batería (líneas 488 y 511), en los márgenes (340, 498, 515 y 520) y en la canónica (542 y 573).
- S1, S1-Sol y L-6 miran el texto crudo.

**Verificador repetido** (`/home/bootcamp/.cache/fase2c_r5/verificar_r5.txt`): exit 4 en 98 s. La salida es idéntica a `relanzada/verificar_espacio_s1b2.txt`: 37 canales cubiertos, 0 fallos y 0 pendientes. No aparece ninguna regla nueva por encima de +8 pp.

**Diff de 4cffbe8b a 6c9d7705.** Solo cambian las cadenas de los cierres, el bloque de S7 (4 líneas pasan a 2 de comentario), los dos `stopifnot` y algunos comentarios.

**Instrumentos.**
- El de HTML imprime los denominadores por versión y tiene controles positivos reales: e293 da 139/259, 341/347 y 74/55; 4cffbe8b, 132/259; V1, 34/5.
- El de PDF tiene un control sintético (20/20) y el de e293 para la unidad (122).
- Los dos miden lo que dicen, con la salvedad de H4.

**Render y arsenal.** Los formatos HTML, PDF, DOCX, NOPS y Moodle están en `render_6c9d7705/`. El arsenal da 0 errores.

**Residuos BAJOS de la 6b.** Están bien calificados:
- «kilos.» con la letra de la fórmula ocurre en HTML y Moodle, no en PDF.
- La fórmula entera puede pasar a un renglón propio.
- A 320 px, «= 210 kilos.» queda cerca del borde, sin desborde: la fórmula atómica más ancha mide unos 150 px, por debajo de los 270 px de la caja del colegio.

## No auditado

- No volví a renderizar ni a capturar HTML, PDF o Moodle.
- No repetí la invariancia en 1000 semillas: me apoyo en la del coordinador.
- No renderizé el Moodle del colegio, que tampoco renderizó la S1b.
- La diagnosticidad no la medí aparte: solo repetí el verificador, que salió idéntico.

## Veredicto global

**APROBAR_CON_CAMBIOS.** No hay ninguna CORRECCIÓN ni objeciones críticas o altas, así que nada bloquea la FASE 3.

## Próximos pasos

1. **H1:** controles exactos (1158 y 730) con su mutante, y ampliar la clase de separadores de las sondas. Es endurecer y solo toca el verificador.
2. **H3 y H2:** completar la tabla candidata de L-6 con V1 y corregir la redacción del residuo antes del WAIT_USER #3. Proponer al profesor el cierre opcional con `\text{.}`.
3. **H4 y H5:** limpiar el instrumento y el `ok()` de S7.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/verificar_espacio.R:799 | n_dentro >= N, sprintf("S1-Sol fórmula
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/verificar_espacio.R:820 | !anyNA(v_dentro_s) && all(v_dentro_s > 0)
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:262 | " = ", p$delta, "$. Esa resta compara un curso típico de cada "
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:261 | paste0("La cifra ", af$x, " sale de restar los promedios: $",
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/HANDOFF.md:525 | El mecanismo es el mismo, pero en HTML hubo 0 partidas en 100 semillas por ancho (42 casos).
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:591 | stopifnot("L-6: la unidad que cierra una fórmula va dentro de la fórmula" =
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/.claude/CLAUDE.md:1473 | Candidata a cláusula de L-6, pendiente de la firma del profesor
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/.claude/CLAUDE.md:1477 | lleva dentro el signo que la sigue
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/.claude/CLAUDE.md:1332 | La batería §P7 y los márgenes miden con U+00A0 convertido en espacio
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/.claude/CLAUDE.md:1392 | con la guarda «L-6: la unidad que cierra una fórmula va dentro de la
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/salida/ciclo_20261102/s1b/relanzada/cap_s1b2.py:12 | Todas las clases de caracteres van con escapes
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/salida/ciclo_20261102/s1b/relanzada/cap_s1b2.py:37 | (kilos?|cursos?)/g, EL=/
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/verificar_espacio.R:79 | (if (afs$gdis$x > d$D) "encima" else "debajo")
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/verificar_espacio.R:108 | ok(identical(CANON$lado, "encima") && isTRUE(CANON$X > CANON$D),
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
