# FASE 2C: AgenteDetractor sobre la reformulación de GEO-INF-UBI-02 (SCHOICE + CLOZE)

> Reporte copiado tal como lo entregó el detractor (AgenteDetractor, sin `name:`, independiente del
> autor del cambio). El hook `SubagentStop` de citas lo dio por entregado: 9 citas, 0 fallos
> (`~/.claude/hooks/logs/citas-detractor.log`, 2026-10-20 20:53:27). Después de recibirlo se
> corrigieron H1 y H2 (dos comentarios de código); H3 y H4 quedan registrados en el `HANDOFF.md`.

**Fecha**: 2026-10-20 · **Objeto**: el cambio sin commitear en los dos `.Rmd`. No edité ningún archivo del repo; trabajé en el scratchpad.
**Objeciones**: 4 (0 críticas, 0 altas, 1 media, 3 bajas). No hay ninguna de CORRECCIÓN.

## Resumen

- **La clave es verdadera en todas las versiones.** La nueva pregunta tiene una sola lectura en las cuatro ramas y con los seis valores de `b_sing`.
- **«el primero en el orden del acta» es verdadera siempre que `det = TRUE`.**
- **Cuando `det = FALSE`, la pregunta queda indeterminada**, también sin trazado.
- **En la canónica UBI-02 nunca aparece.**
- **La diagnosticidad no cruza ningún umbral.** Reproduje exactamente las cifras del autor.
- **El comentario nuevo tiene dos afirmaciones falsas o fuera de contexto** (H1, H2).
- **Hay una asimetría con el gemelo CLOZE que ya existía antes del cambio** (H3).

## Evidencia ejecutada (método)

- **Barrido propio** (`sweep.R`) del `data_generation` real.
  - Semillas 1..100 en cada ítem (N = 100, regla #23).
  - Más 30 semillas con `is_canonical` forzado a TRUE. Es una enumeración dirigida, no una medición estadística.
  - En cada versión comprueba:
    - que `det` coincide con `info_T && info_A`;
    - que, si `det = TRUE`, el enunciado contiene la frase de asignación por el acta;
    - en el CLOZE: P1, P4 (marca de respondible contra `det`), P5 y P6, y que el `enunciado_p6` contiene la asignación cuando UBI-02 es distractor y no la contiene cuando es clave;
    - que el largo es exactamente `54 + nchar(b_sing)`.
- **Enumeración exacta**: `enum_exacta.R` del autor sobre HEAD, sobre la versión final y sobre una variante con la redacción de 70 caracteres.
- **CLOZE**: `auditoria_propia.R` sobre HEAD y sobre la versión final, con N = 100 y con N = 300. El de 300 es análisis estratificado declarado, el mismo que usó el autor.
- **Detalle de entorno**: con `Rscript --vanilla` el script falla, porque no se lee `.Renviron` y faltan las librerías. Usé `--no-init-file`.

## Dominio A — CORRECCIÓN

**Ninguno.** Esto es lo que verifiqué:

**Las cuatro ramas.** Con `det = info_T && info_A`:
- Cuando `info_A` es TRUE, el enunciado añade siempre «Las franjas se asignan a los <b_plur> en el orden en que aparecen en el acta, empezando por la que da a la carretera».
- Fuera de la canónica, la carretera se declara siempre y va anclada a un lado paralelo a los cortes. Por eso, con trazado, hay una sola franja que da a la carretera y su turno es el primero.
- Esto vale también sin `info_N` (rama `LTA-`): saber cuántas franjas hay no cambia que la primera del acta sea la de la carretera.

**Barrido SCHOICE (100 semillas).**
- UBI-02 es visible en 46 versiones: es clave en `LT-N` (11/11) y en `L--N` (3), y distractor en `-TAN` (24) y `LTA-` (8).
- 0 incoherencias.
- 0 versiones con UBI-02 visible y sin carretera en el enunciado.

**Barrido CLOZE (100 semillas).**
- UBI-02 es visible en 80 versiones:
  - es clave de P1 en `L--N`;
  - aparece en P4 con `sol 0` en `-T-N`, `L--N` y `LT--`, y con `sol 1` en `-TAN` y `LTA-`;
  - es clave de P5 en las ramas sin A;
  - aparece en P6 como clave y como distractor en `-TAN` y `LTA-`.
- 0 incoherencias, incluidas las del enunciado hipotético de P6.

**Canónica.** Con 30 semillas forzadas, en ninguno de los dos ítems aparece UBI-02. La pregunta sobre «la carretera» nunca se muestra donde la carretera no existe.

**Sin trazado (`L--N`, semilla 10 del SCHOICE).**
- El enunciado dice «El lote colinda con una carretera por uno de sus lados» y la clave es UBI-02.
- La pregunta pide un turno que el enunciado no fija. La clave es correcta.
- La Solution presupone una sola «porción que da a la carretera», lo que sin trazado no está definido. Ver H3, que es anterior al cambio.

**«turno».**
- El único orden que aparece en el enunciado es el del acta, que coincide con el orden de asignación. Las dos lecturas posibles («lugar en el acta» y «turno en la asignación») dan «el primero».
- La pregunta ya no depende de saber quién es el beneficiario. La lectura que señaló O1 queda cerrada.
- Sobre el sentido de «turno» en el DLE (orden en que se suceden varias personas) cito de memoria; no lo consulté en esta sesión.

**Todos los lugares donde se imprime UBI-02 en el CLOZE.** Revisé P1, P4, P5, P6 (opciones del banco real; motivo y valores del banco hipotético; el texto solo depende de `b_sing`, así que es idéntico en los dos bancos), la Solution (líneas 1813-1878 y 1893) y el feedback por opción (líneas 1944-1965). Todos componen bien con el valor nuevo.

## Dominio B — Gramática y concordancia

**Ninguno.**
- Los seis valores de `b_sing` (hijo, hermano, asociado, heredero, socio, beneficiario) son masculinos, así que «del <b_sing>» es correcto en todos.
- Salida real de la semilla 4 (LTA-, hermano): `- **"¿Cuál es el turno del hermano cuya porción da a la carretera?"** -> el primero en el orden del acta. El enunciado dice que las franjas se asignan en el orden del acta empezando por la que da a la carretera.`
- «Esta pregunta sí se puede responder (el primero en el orden del acta).» y «Sí se puede responder: el primero en el orden del acta.» se leen bien.

## Dominio C — DIAGNOSTICIDAD

**SCHOICE, enumeración exacta (HEAD contra final).**
- La tabla P(aparece)/P(clave) por código es **idéntica** (diff vacío). UBI-02: 44,945 % / 18,224 %.
- Adversario bayesiano: 57,58 % en los dos.
- H1 exacto por rama: igual en todas, salvo `-TAN` «más larga con margen ≥ 15 %», que pasa de 5,7 % a 6,7 % (la tasa «más larga» sigue en 20,0 %). `LT-N` sigue en 7,1 % y 0,0 %.

**Restricción 1 (molde «¿Cuál es…»).** Verificada: los tres primeros tokens del banco son solo «¿Cuál es el» / «¿Cuál es la» en los dos ítems (2.000 y 2.445 textos en 100 semillas). La redacción de HEAD, «¿Cuál de los…», era la que **rompía** el molde a partir de la segunda palabra; el cambio lo restituye.

**Restricción 3 (largo).** Medido por cálculo: `54 + nchar(b_sing)` en los seis valores, un carácter menos que la redacción vieja.

**CLOZE, `auditoria_propia.R` con N = 300.** APROBADO, 0 hallazgos, todos los estratos dictaminados, igual que HEAD. Reproduje exactamente las cifras del autor:
- P1 `L--N`: el peor token pasa de «los» 45,8 % a «ubicación» 30,5 %.
- P6 global: de «los» 36,7 % a «cortes» 25,0 %.
- P5 `-TAN`, única más larga: de 21,0 % a 28,4 % (n = 81).

Los tokens nuevos («turno», «cuya») solo existen en UBI-02. Su poder predictivo es el mismo que el del código, y la tabla P(clave | aparece) no cambió. No abren canal nuevo. Ver H4 para los movimientos residuales.

## Dominio D — Material didáctico

**Ninguno.**
- C1 en `actividad_estudiante.html`: «¿Qué lugar del acta ocupa el asociado que recibe la franja que da a la carretera?», con respuesta «el primero».
- Pregunta por lo mismo (la posición) y da el mismo valor que el `.Rmd`.
- Que el material nombre el acta y el `.Rmd` no lo haga es coherente con la restricción 2: el caso C1 del material declara el acta.

## Dominio E — El comentario nuevo

Lo que es verdad:
- «Mide un carácter menos»: verificado.
- «(12 caracteres más) … 57,1 % de la rama LT-N (enumeración exacta; antes 7,1 %)»: **reproducido**. La variante de 70 caracteres da en `LT-N` más larga 57,1 % (margen ≥ 15 %: 25,7 %); HEAD da 7,1 %.

Lo que no es verdad: H1 y H2.

## Objeciones

### H1 — MEDIA — Documentación (no es CORRECCIÓN). El comentario del CLOZE cita una rama que el CLOZE no tiene

- **Qué se cuestiona**: el comentario se copió del SCHOICE. En el CLOZE (línea 421) dice que la medición del 57,1 % es «de la rama LT-N … porque allí la clave es UBI-02».
- **Evidencia**: el `COMBOS_INFO` del CLOZE (líneas 235-239) tiene 5 combinaciones: `-T-N`, `-TAN`, `LT--`, `LTA-`, `L--N`. La propia HANDOFF lo confirma: «5 ramas vivas». El barrido de 100 semillas no produjo ninguna versión `LT-N`. La cifra es del SCHOICE.
- **Riesgo**: quien mantenga el CLOZE va a buscar en él una rama y una medición que no existen. Es el patrón de documentación no verificada que prohíbe `documentacion-verificada.md`.
- **Alternativa**: en el CLOZE, «En el SCHOICE gemelo (rama LT-N, que este CLOZE no sortea) la clave era la opción más larga en el 57,1 %…», o dar la medición propia del CLOZE.
- **Veredicto**: MODIFICAR (solo el comentario).

### H2 — BAJA — Documentación. «fuera de info_A el enunciado no la menciona» es falso en el contexto 5

- **Qué se cuestiona**: el comentario dice que, sin `info_A`, el enunciado no menciona el acta. El contexto 5 (herederos) abre con «El acta de reparto de una sucesión indica que el lote…» en las dos versiones: SCHOICE línea 121, CLOZE línea 106.
- **Riesgo**: bajo. La justificación de la restricción 2 sigue valiendo para los otros 6 contextos. En el contexto 5, nombrar el acta en la opción no delataría la clave, porque el acta ya está en el enunciado.
- **Alternativa**: «fuera de info_A el enunciado solo la menciona en el contexto 5 («El acta de reparto…»); en los otros seis, nombrarla en la opción delataría la clave».
- **Veredicto**: MODIFICAR (solo el comentario).

### H3 — BAJA — Solution incompleta, no falsa (no es CORRECCIÓN). Anterior al diff

- **Qué se cuestiona**: el `motivo` y el `contra` de UBI-02 en el SCHOICE no se ramifican por `info_T`. En `L--N`, donde UBI-02 es clave (semilla 10), la Solution dice «cualquiera de los hijos podría recibir esa porción» y «Nada en el enunciado impide que esa porción le toque al primero…». Las dos frases presuponen una única porción que da a la carretera, y sin trazado esa porción no está definida.
- **Comparación con el CLOZE**: el gemelo ya corrigió esto (líneas 426-441; detractor FASE 2C, objeción 3, 2026-08-13).
- **Por qué no es CORRECCIÓN**: ninguna afirmación es falsa, y la sección «Propiedades» sí dice que el enunciado calla «el trazado de los cortes y el orden». El cambio de redacción no lo agrava; al preguntar por el turno, el motivo «no fija ningún orden» encaja mejor que antes.
- **Peso**: según la enumeración exacta, alrededor del 6,7 % de las versiones del SCHOICE tienen UBI-02 como clave fuera de `LT-N` (18,224 % − 11,510 %).
- **Alternativa**: portar al SCHOICE la ramificación `if (!info_T)` del CLOZE, con el mismo texto.
- **Veredicto**: MODIFICAR en un ciclo posterior. No bloquea.

### H4 — BAJA — DIAGNOSTICIDAD. Movimientos residuales por el carácter de menos, todos bajo §P7-A

Lo que se movió:
- SCHOICE `-TAN`, más larga con margen ≥ 15 %: de 5,7 % a 6,7 % (exacto). La tasa «más larga» sigue en 20,0 %, por debajo del azar.
- CLOZE P5 `-TAN`, única más larga: de 21,0 % a 28,4 % (n = 81, sin margen). Exceso ingenuo sobre 25 %: +3,4 pp, menor que +5,3 pp.
- CLOZE P5 `LTA-`, única más larga: de 64,5 % a **66,7 %** (n = 93; 2 versiones). El autor no reportó este número.
  - Es alto, pero ya existía: es la DIAGNOSTICIDAD ALTA que la segunda corrección atacó con `armar_p5(largo_min)`. La guarda de las líneas 1309-1310 acota el margen de la clave frente al distractor más largo.
  - La auditoría solo dispara a ≥ 90 % y dictamina APROBADO tanto en HEAD como en la versión final.
  - **No volví a medir su margen relativo con este cambio.** Si se quiere cerrarlo, hay que medir el margen ≥ 15 % de P5 `LTA-` antes de promover.

Por qué no obliga: según §P7-C la batería está congelada y según §P7-D las pasadas están agotadas. Ninguna cifra supera +8 pp; queda registrado como residuo.

**Alternativa**: ninguna obligatoria. Recomiendo anotar estas cifras en el HANDOFF junto a O-A y O-B.
**Veredicto**: MANTENER.

## Próximos pasos

1. Corregir los dos comentarios (H1 en el CLOZE; H2 en los dos `.Rmd`). Es un cambio de prosa sin efecto en la ejecución.
2. Opcional, en otro ciclo: llevar al SCHOICE la ramificación `info_T` del motivo y el contra de UBI-02 (H3).
3. Antes de promover: medir el margen ≥ 15 % de P5 `LTA-` del CLOZE y anotar en el HANDOFF las cifras de H4.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/informacion-insuficiente-lote-n4/cloze/informacion_insuficiente_lote_geometrico_metrico_argumentacion_n4_cloze_v1.Rmd:421 | rama LT-N (enumeración exacta; antes 7,1 %), porque allí la clave es UBI-02.
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/informacion-insuficiente-lote-n4/cloze/informacion_insuficiente_lote_geometrico_metrico_argumentacion_n4_cloze_v1.Rmd | AUSENTE: list(L = TRUE,  T = TRUE,  A = FALSE, N = TRUE,  w = 12)
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/informacion-insuficiente-lote-n4/informacion_insuficiente_lote_geometrico_metrico_argumentacion_n4_schoice_v1.Rmd:276 | list(L = TRUE,  T = TRUE,  A = FALSE, N = TRUE,  w = 12)
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/informacion-insuficiente-lote-n4/informacion_insuficiente_lote_geometrico_metrico_argumentacion_n4_schoice_v1.Rmd:463-464 | No nombra el acta: fuera de info_A el
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/informacion-insuficiente-lote-n4/informacion_insuficiente_lote_geometrico_metrico_argumentacion_n4_schoice_v1.Rmd:121 | El acta de reparto de una sucesión indica que el lote
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/informacion-insuficiente-lote-n4/cloze/informacion_insuficiente_lote_geometrico_metrico_argumentacion_n4_cloze_v1.Rmd:106 | El acta de reparto de una sucesión indica que el lote
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/informacion-insuficiente-lote-n4/informacion_insuficiente_lote_geometrico_metrico_argumentacion_n4_schoice_v1.Rmd:472 | El enunciado no fija ningún orden de asignación: cualquiera de
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/informacion-insuficiente-lote-n4/cloze/informacion_insuficiente_lote_geometrico_metrico_argumentacion_n4_cloze_v1.Rmd:434 | El enunciado no dice por dónde pasan los cortes, así que ni siquiera
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/informacion-insuficiente-lote-n4/cloze/informacion_insuficiente_lote_geometrico_metrico_argumentacion_n4_cloze_v1.Rmd:1309-1310 | largo_min * nchar(preguntas[[ik]]$texto)) next
FIN_CITAS_DETRACTOR

> Nota posterior: las citas valían contra el archivo en el momento de la entrega (verificado por el
> hook). La corrección de H1 y H2 alargó el comentario una línea en cada `.Rmd`, así que las citas
> posteriores a la línea 421 del CLOZE y a la 464 del SCHOICE quedaron desplazadas en una línea, y
> los fragmentos de H1/H2 sobre el comentario ya no existen: son el texto que se corrigió.

VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
