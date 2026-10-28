# Auditoría detractora FASE 2C, pasada 3 (última de §P7-D): CLOZE `probabilidad-fichas-tabla-doble-entrada-n2`

**Fecha:** 2026-10-28
**Artefacto:** `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd`, md5 `945560602547bcd7bce941c5a1ad7ccc`. Lo comprobé al empezar, y mi script de render tiene un `stopifnot` con ese md5.
**Independencia:** no escribí ni corregí el ejercicio, y no hice las pasadas 1 ni 2.
**Hallazgos:** 2, los dos de severidad MEDIA (uno de proceso y otro de despliegue en Moodle). Ninguno es de CORRECCIÓN y ninguno pide cambiar el `.Rmd`.

## Resumen ejecutivo

**Las seis claves son correctas, ningún gap tiene segunda clave y la Solution solo dice cosas verdaderas.** Lo comprobé de forma distinta a r2 y al verificador:
- Exporté a Moodle 100 versiones nuevas. El RNG global partió de `set.seed(20261028)`, una corriente distinta de las semillas 1–879 que usaron r2 y el verificador.
- Recalculé la verdad leyendo el **texto alternativo de la tabla y el texto visible de cada gap en el XML**, no los objetos del `.Rmd`.
- Resultado: **0 fallos**, en las marcas, en las afirmaciones de la Solution y en la alineación de la retroalimentación.

R3 y R11 se cumplen (los comprobé por diff). V11 se sostiene por un argumento algebraico que cierra el caso, y la única desviación que introduce el sorteo de D es de unos ±0,8 pp. La calificación de MULTIRESPONSE en Moodle es correcta: lo verifiqué en el código fuente de Moodle 4.5.

Los dos hallazgos quedan fuera del `.Rmd`:
- **H1:** el residuo de este ciclo no está declarado en ningún archivo persistente. El HANDOFF no menciona el CLOZE.
- **H2:** la exportación con `MULTICHOICE_V` (F-6) solo existe en `render_cloze.R`. Los exportadores del profesor que hay en la carpeta producirían menús desplegables.

## Lo que verifiqué por mi cuenta

1. **Verdad de punta a punta sobre el XML de Moodle**
   - Archivos: script `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/ca84678d-286e-418b-a9bc-e7a8cd20624b/scratchpad/detractor_r3/gen100.R`, comprobador `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/ca84678d-286e-418b-a9bc-e7a8cd20624b/scratchpad/detractor_r3/chk.py`, salida `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/ca84678d-286e-418b-a9bc-e7a8cd20624b/scratchpad/detractor_r3/moodle100/c100.xml`.
   - Muestra: 100 versiones (10 canónicas y 90 no canónicas; Parte 6 con una verdadera en 48 y con dos en 52), generadas en 86 s.
   - **Marcas y verdad.** En las Partes 1, 5 y 6, marca = verdad, con exactamente una clave en la Parte 1 y en la 5. En la Parte 2 el valor es N; en las Partes 3 y 4, el conteo del evento «sacar …» leído del enunciado.
   - **Solution, apartado «Respuestas».** Coincide parte por parte con las marcas. En la Parte 6, los conjuntos «tienen / no tienen» coinciden con las verdaderas y las falsas.
   - **Cifras de la Solution.**
     - 1380 sumas «tipo: a + b = s fichas»: la aritmética es correcta y s es el conteo real del tipo.
     - 1252 fracciones: todas con denominador N.
     - «Caso específico» y «Solo en la pareja … (n y n)»: coinciden con la clave.
   - **Bloques de distractores.**
     - PROB-UNA (90/90): las dos celdas que cita son las del rótulo que nombra, valen lo mismo y la dimensión libre no está entre las del tipo.
     - PROB-CER (100/100): las cantidades difieren en exactamente una ficha.
     - PROB-CRU (10/10, canónica): cita bien sus celdas sueltas.
   - **Retroalimentación por opción** (última lista de la retroalimentación general): está alineada con el orden de cada gap. «Verdadero»/«Falso» y «Tienen / No tienen la misma probabilidad» corresponden a la verdad, y las cifras coinciden con los conteos.
   - Ninguna «Opción A–D».
   - Todo párrafo, ítem o tramo « / » con `\frac` dice «en cada extracción» o «ficha extraída» (I-7): 0 excepciones.
   - **El comprobador no es vacuo.** Tres mutantes del XML murieron cada uno por su sonda: marca de la Parte 1 movida, +1 en la Parte 2 de la Solution y +1 en una celda de PROB-UNA.

2. **Calificación de MULTIRESPONSE en Moodle** (fuente de nivel 1: `moodle/moodle`, rama `MOODLE_405_STABLE`, vía `gh`).
   - R/exams 2.4.2 escribe `=` en las correctas solo cuando sus porcentajes suman 100 (`make_question_moodle23`, deparse, líneas 320–323).
   - El parser cloze de Moodle (`question/type/multianswer/questiontype.php`, líneas 559–577) normaliza las fracciones positivas de MULTIRESPONSE dividiéndolas por su total.
   - Con dos verdaderas (`=`, `=`, `%-50%`, `%-50%`): marcarlo todo da 0 y marcar una sola correcta da 0,5. Con una verdadera (tres a `%-33.33%`), marcarlo todo da 0.
   - En las variantes sin `_S` (MULTICHOICE_V y MULTIRESPONSE), el parser fija `shuffleanswers = 0`. Por eso la retroalimentación « / » queda alineada con lo que ve el estudiante.

3. **R3 y R11 por diff.**
   - El SCHOICE de SUB tiene md5 `4fa520bb…` y no difiere de HEAD `86ef146b` (el aprobado).
   - El chunk del SCHOICE frente al bloque P1 del CLOZE difiere solo en las 6 líneas de comentario de cabecera, en los renombres a `opciones_p1`, `sol_p1` y `feedback_p1` (y sus usos en `respuesta_correcta` y `txt_frac`) y en el marcador de fin.
   - En la Meta-information solo cambian `exname`, `extype`, `exclozetype`, `exsolution`, `extol` y `Type: CLOZE`. La línea `exshuffle: TRUE` es idéntica.
   - La enumeración de la Parte 1 (10.401.300 combinaciones) es la del verificador del SCHOICE, reejecutado hoy (`sub_verificar_espacio_rerun_2026-10-28.txt`: APROBADO). Se transfiere porque el código es idéntico.

4. **V11 (R8) por álgebra, no solo por muestra.**
   - Cada opción de la Parte 1 equivale a igualar un marginal de una dimensión con un marginal de otra:
     - clave ⟺ n(y1) = n(z1);
     - espejo ⟺ n(y1) = n(z2);
     - P ⟺ n(x1) = n(y2);
     - Q ⟺ n(x1) = n(y1).
   - Las Partes 2 a 6 solo dan los dos marginales de **una** dimensión. Por eso ninguna decide ni descarta una opción de la Parte 1, ni por valor ni por contenido.
   - El único efecto de D viene de excluir la dimensión X cuando empata (33 empates en las 500 no canónicas del verificador, p ≈ 6,6 %):
     - P(D = X) = (1 − p)/3 ≈ 31,1 % y P(Y) = P(Z) ≈ 34,4 %.
     - «Elegir entre las opciones que nombran D» = P(Y)/4 + P(Z)/2 ≈ 25,8 % (+0,8 pp).
     - «Entre las que no la nombran» ≈ 24,2 % (−0,8 pp).
   - Es compatible con lo que mide E3 (24,2 % y 25,8 % con n = 300; error típico ≈ 2,5 pp).
   - La tolerancia de independencia de E3 (3 errores típicos + 2 pp ≈ 10 pp) solo admitiría sesgos de rol que producen como mucho unos +5 pp en esas reglas, por debajo de +8.

5. **Posición de la clave en Moodle**, donde los gaps del cloze no se vuelven a mezclar:
   - Parte 1: 25/21/27/27.
   - Parte 6: marcas por posición 40/44/29/39 de 152, cuando lo esperado son 38.
   - Parte 5: 60/40 con n = 100. Está en el borde de 2σ, pero por construcción es 50 % (`o5` más la mezcla de R/exams), así que no es un canal.

6. **Contrato de mutación (R13).** Cada `stopifnot` de las Partes 2 a 6 tiene su mutante:
   - líneas 702 → M21; 706 → M9 y M22; 780 → M19;
   - 801–811 → M1 a M7; 818–819 → M20 y M23; 847–848 → M8.
   - Las sondas del verificador tienen los suyos: M10 a M13.
   - Las guardas de la Parte 1 tienen M14 a M18 aquí, más los 25/25 del SCHOICE.
   - Los mutantes M5, M10 y M13 invierten la propiedad defendida, no solo el texto de la guarda.

7. **Estado frente a artefactos.**
   - Las vistas previas `final_cloze*.png` (13:36) y el arsenal (13:37:05) son posteriores al `.Rmd` (13:34:32).
   - Los pasos 5 y 6 llevan nota, así que no tienen la firma del auto-sello.
   - La línea 960 dice «puede venir»: el H2 de r2 está aplicado.

## Requisitos del encargo

| Código | Estado | Evidencia |
|---|---|---|
| R3 | Cumple | Diff del punto 3; E1 en 100 semillas más 11 canónicas de cuota, con `.Random.seed` idéntico |
| R5 | Cumple | Las Partes 2 a 6 son eventos de una extracción sobre N; el «no» es complemento en la misma bolsa; no hay probabilidad condicional |
| R6 | Cumple | `Nivel: 2`, `DOK: 2`, D2.3 |
| R8 | Cumple | Punto 4, por álgebra; E3 lo mide |
| R10 | Cumple | Respuestas por contenido; distractores en orden `sort(codigos)`, no en el orden mostrado; 6 secciones de la regla #1; n/N con su glosa (punto 1) |
| R11 | Cumple | Diff de los metadatos (punto 3) |
| R13 | Cumple | Punto 6; 100 por estrato; los estratos con n < 20 se marcan NO CONCLUYENTE; 300/300 versiones únicas |
| CORRECCIÓN | Sin defectos | Puntos 1 y 2 |

## Hallazgos

### H1. El cierre con residuo declarado no tiene dónde quedar: el HANDOFF y el `.claude/CLAUDE.md` local no mencionan el CLOZE

- **Dominio:** testing/proceso. **Severidad:** MEDIA. **Tipo:** proceso.
- **Qué se cuestiona:** ésta es la última pasada, y §P7-D (regla #22) exige cerrar «con residuo declarado».
- **Evidencia:**
  - `HANDOFF.md` se actualizó por última vez el 2026-10-28 («versionado e infraestructura»), antes de abrir el CLOZE (12:10). Sigue diciendo «Estado: 11/11» y tiene 0 menciones de «cloze». El `.claude/CLAUDE.md` local tampoco tiene ninguna.
  - Los residuos de este ciclo solo están en los reportes r1/r2 y en `paso9_diversidad.txt`:
    - **r1 H3:** la Parte 6 se resuelve sin la tabla. El veredicto de cada plantilla no cambia en ninguna versión, y las dos opciones de la Parte 5 presuponen n1 ≠ n2.
    - **r1 H7:** `WARN_DIV_BAJA` en la Parte 5 (22/100). No aparece en `cloze/ejercicio_state.json`.
    - **r1 H8:** sin `expoints`, la pregunta del ítem (Parte 1) vale 1/6 de la nota y los conteos, 3/6.
    - **r1 H5 / punto 5:** con lectura condicional pueden empatar dos eventos si |n1 − n2| = 1.
  - Solo están declarados el DOCX y las opciones en línea del PDF (estado, líneas 53 y 56).
  - `cloze/` está sin versionar (`??` en `git status`). Un agente lanzado en un worktree no lo ve: el mío no lo tiene.
- **Riesgo concreto:** quien retome con la frase disparadora lee «11/11» y no sabe que hay un CLOZE en 6/11 con dos decisiones pendientes del profesor (H3 y H8 de r1). La memoria del proyecto ya registra el caso de un HANDOFF más viejo que el código («El HANDOFF puede estar más viejo que el código»).
- **Alternativa:**
  1. Añadir al HANDOFF una sección «CLOZE (`cloze/`)» con:
     - el estado (6/11);
     - los residuos de arriba con sus cifras;
     - las decisiones pendientes (H3 y H8 de r1);
     - la llamada de exportación (H2).
  2. Cuando el profesor decida, registrar su decisión como fila de la tabla de decisiones del `.claude/CLAUDE.md` local (H-5).
  3. Versionar cuando él lo pida.
- **Veredicto:** MODIFICAR (proceso; no toca el `.Rmd`).

### H2. El ajuste F-6 (gaps de opción única como `MULTICHOICE_V`) solo vive en `render_cloze.R`; los exportadores del profesor darían menús desplegables

- **Dominio:** visual y código R/exams (despliegue en Moodle). **Severidad:** MEDIA. **Tipo:** proceso.
- **Evidencia:**
  - Si las opciones no llevan `\(`, R/exams 2.4.2 exporta los gaps schoice del cloze como `MULTICHOICE`, es decir, como menú desplegable (`make_question_moodle23`, deparse, líneas 296–301). El propio `render_cloze.R` lo dice en su línea 6, y en su línea 26 pasa `cloze = list(cloze_schoice_display = "MULTICHOICE_V")`. Todo lo validado en Moodle (F4, XML, mi punto 1) sale de esa llamada.
  - En la carpeta hay dos exportadores del profesor:
    - `SemilleroMoodle_v2.R` apunta al SCHOICE (línea 8);
    - `SemilleroCloze.R` es una plantilla de otro ejercicio (línea 27) y llama a `exams2moodle` sin argumento `cloze` (líneas 188–196).
  - Adaptar cualquiera de los dos a este CLOZE convierte la Parte 1 (opciones de 66 a más de 100 caracteres) y la Parte 5 en desplegables. Es justo lo que F-6 descartó por legibilidad a 360 px, en un aula que usa celulares.
  - La calificación no cambia: el `eval` por defecto es el mismo.
- **Alternativa:**
  1. Dejar un exportador del CLOZE junto al ejercicio (por ejemplo `cloze/exportar_moodle_cloze.R`, con `exams2moodle(F, n = …, cloze = list(cloze_schoice_display = "MULTICHOICE_V"))`), o anotar esa llamada en la sección del HANDOFF que pide H1.
  2. Proponer al profesor, sin editar sus archivos, la misma línea para su `SemilleroCloze.R`.
  3. Comprobación antes de subirlo: `grep -c '{1:MULTICHOICE:' <xml>` debe dar 0.
- **Veredicto:** MODIFICAR (despliegue; no toca el `.Rmd`).

## Residuos con los que se cierra el ciclo (§P7-D, 3 de 3 pasadas)

Diagnosticidad: no hay ningún canal nuevo por encima de +8 pp.
- El efecto atribuible a D es de unos ±0,8 pp (punto 4).
- Los residuos firmados del SCHOICE se mantienen dentro de su firma (E4 del verificador, con la depuración N = 4000 de r2).

Quedan por declarar (H1):
- H3 de r1 (la Parte 6 se resuelve sin la tabla);
- H7 de r1 (22/100 en la Parte 5);
- H8 de r1 (pesos iguales);
- la lectura condicional con |n1 − n2| = 1 (r1, punto 5);
- el DOCX y las opciones en línea del PDF, que ya figuran en el estado.

H3 y H8 son pedagógicos y no los reabro: los decide el profesor.

## Verificado sin objeción

- Corrección de los seis gaps en 100 versiones exportadas (punto 1), que se suman a las 300 de r2 y las 600 del verificador.
- Calificación de MULTIRESPONSE y alineación de la retroalimentación en Moodle 4.5 (punto 2).
- R3 byte a byte, sus guardas y el SCHOICE de origen igual al aprobado (punto 3).
- V11 por álgebra e independencia de D (punto 4).
- Posición de la clave (punto 5) y contrato de mutación (punto 6).
- Regla #19: no hay letras, y la prosa no sigue el orden mostrado.
- I-7: toda n/N con su glosa.
- I-1, I-2 e I-5 a I-8: heredadas sin cambios.
- Regla #18: `{width=100%}&#8203;`.
- Regla #4: `tabla_<hex>.png` neutral.
- Ortografía y tildes: barrido de las 100 versiones sin palabras sin tilde, sin palabras repetidas y sin glifos prohibidos.
- Afirmaciones lógicas de la Solution: «Leer el "no"…» es verdadera en toda selección posible (con dos verdaderas siempre aparece alguna de `si1_no1`/`si2_no2`). Las frases de «Propiedades» y «Caso específico» también son verdaderas.

## No auditado

- **Auditoría visual 6b** (24 versiones a 360 y 1024 px) **y paso 8** (5 coherencias): los hacen otros agentes y no los dupliqué.
- **Correspondencia entre el PNG y el texto alternativo.** Usé el texto alternativo como fuente de verdad. Los dos salen de la misma matriz `valores` (líneas 422–430 y 680–682 del `.Rmd`), pero no hice OCR del PNG.
- **Importación en el Moodle real de la institución.** Verifiqué contra el código de `MOODLE_405_STABLE`, no contra su versión instalada.
- **Re-medición de E4 y de la enumeración de la Parte 1.** Uso la salida del verificador y su reejecución de hoy.
- **Contenido del DOCX:** su limitación ya está declarada.

## Veredicto global: APROBAR_CON_CAMBIOS

Ningún hallazgo es de CORRECCIÓN ni de severidad crítica o alta, así que nada bloquea la FASE 3. El `.Rmd` no necesita cambios. Como ésta es la última pasada, el ciclo se cierra con el residuo declarado.

**Próximos pasos:**
1. **H1:** sección CLOZE en el HANDOFF con el estado, los residuos y las decisiones pendientes, antes de sellar `detractor_fase2c`.
2. **H2:** fijar la llamada de exportación con `MULTICHOICE_V` junto al ejercicio.
3. **Llevar al profesor** H3 y H8 de r1.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/HANDOFF.md:4 | Última actualización: 2026-10-28 (versionado e infraestructura)
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/HANDOFF.md:14 | ## Estado: 11/11, aprobado para el aula por el profesor el 2026-10-27
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/HANDOFF.md | AUSENTE: CLOZE
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/.claude/CLAUDE.md | AUSENTE: CLOZE
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/salida/paso9_diversidad.txt:8 | WARN_DIV_BAJA: la clave varia poco en p5.
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/ejercicio_state.json | AUSENTE: WARN_DIV_BAJA
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/salida/detractor_fase2c_r1.md:79 | ### H3. Fuga entre partes: la Parte 5 presupone el veredicto de tres de las cinco plantillas de la Parte 6
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/salida/detractor_fase2c_r1.md:117 | ### H7. La diversidad de la Parte 5 está en el techo de los pools
H1 | SIN_ARCHIVO | git status --short del checkout principal lista el directorio cloze del subproyecto como no versionado (??), y el worktree del detractor no lo contiene
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/render_cloze.R:6 | por defecto R/exams los exporta como menú desplegable (MULTICHOICE)
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/render_cloze.R:26 | cloze = list(cloze_schoice_display = "MULTICHOICE_V")
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/SemilleroCloze.R:188 | resultado <- exams2moodle(archivo_examen,
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/SemilleroCloze.R:27 | archivo_examen <- "01-teorema_pitagoras_entrenamiento_completo_cloze_geometrico_metrico_formulacion_ejecucion_n2_cloze_v1.Rmd"
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/SemilleroMoodle_v2.R:8 | archivo_examen <- "probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd"
H2 | SIN_ARCHIVO | exams:::make_question_moodle23 de exams 2.4.2, lineas 296 a 301 del deparse: sin math en las opciones, el display por defecto de los gaps schoice del cloze es MULTICHOICE (desplegable)
R3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:397 | opciones_p1 <- unname(opciones_base[o])
R3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/salida/verificar_espacio_cloze.txt:5 | 100 semillas (9 canónicas) + 11 canónicas de cuota
R5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:692 | el total N y UNA sola dimensión de la misma bolsa (I-4, D2.3)
R5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:745 | Ese número es la cantidad de casos posibles al
R6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:1014 | exextra[Nivel]: 2
R6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:1019 | exextra[DOK]: 2
R6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:1015 | exextra[Descriptor]: D2.3 — Compara la probabilidad de eventos simples
R8 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:802 | V11 por valor: las Partes 2 a 6 no imprimen cifras
R8 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/salida/verificar_espacio_cloze.txt:13 | Empates de la dimensión X (excluida del sorteo en esa versión): 33 de 600 versiones medidas
R10 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:912 | La respuesta correcta es la que afirma: «
R10 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:951 | for (cod in sort(codigos_sel)) {
R11 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:1009 | exextra[Type]: CLOZE
R11 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:1008 | exshuffle: TRUE
R13 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/verificar_espacio_cloze.R:35 | N_EST <- 100L; TOPE <- 4000L
R13 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/salida/verificar_espacio_cloze.txt:106 | 23/23 mutantes cazados por su propia sonda
R13 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/salida/verificar_espacio_cloze.txt:55 | 300 de 300 versiones únicas
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
