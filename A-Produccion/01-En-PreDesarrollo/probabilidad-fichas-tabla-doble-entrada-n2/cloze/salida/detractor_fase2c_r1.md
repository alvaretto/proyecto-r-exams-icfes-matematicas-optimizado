# Auditoría detractora (FASE 2C, primera pasada): CLOZE `probabilidad-fichas-tabla-doble-entrada-n2`

**Fecha:** 2026-10-28
**Artefacto:** `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd`
**Hallazgos:** 8 (0 críticos, 0 altos, 4 medios, 4 bajos). Ninguno es de CORRECCIÓN.

## Resumen ejecutivo

Las claves de los seis gaps son correctas. No hay segunda clave en ningún gap y la marca del XML de Moodle coincide con la verdad en todas las opciones. Lo comprobé por tres vías independientes del verificador del orquestador:
- un analizador propio sobre 400 versiones;
- un parseo propio del XML de Moodle;
- la lectura del código fuente de R/exams 2.4.2.

La Parte 1 es el SCHOICE aprobado. El diff muestra solo el encabezado y los renombres, y las guardas siguen intactas. Las Partes 2 a 6 no dejan ver la clave de la Parte 1.

Los hallazgos son de cuatro tipos:
- **Redacción de la Solution:** jerga «nivel» y «dimensión» (H1), y la frase «esas dos parejas» (H2).
- **Una fuga estructural entre las Partes 5 y 6** (H3), de baja explotabilidad.
- **Un contrato de mutación incompleto en el verificador** (H4).
- **Cuatro observaciones bajas** (H5 a H8).

## Lo que verifiqué por mi cuenta

1. **R3 por diff.** Comparé el chunk del SCHOICE con el bloque P1 del CLOZE: difieren solo en 6 líneas de encabezado y en los renombres `opciones→opciones_p1`, `sol→sol_p1` y `feedback→feedback_p1`, con sus usos en `respuesta_correcta` y `txt_frac`. Se conservan intactas las guardas `P_CANON <= 0.10`, la canónica verbatim, `nchar(pregunta)==123`, `n1+n2<N`, la de clave única, la de PROB-CRU solo en la canónica y la de opciones agregadas.
2. **Verdad independiente** sobre 400 versiones: 100 canónicas y 100 por cada rol de la dimensión sorteada (X, Y, Z), semillas 1 a 879, sin errores de generación. Mi analizador lee el evento del **texto** y cuenta en el cubo `w` con `apply`. Resultado: **0 discrepancias** en P2, P3, P4, P5, P6 y `exsolution`.
3. **XML de Moodle**, con un parseo propio de los 5 `{n:TIPO:…}` de `cloze.xml`. Las marcas de P1, P5 y P6 y las respuestas numéricas son correctas en las 5 preguntas: **0 errores en 65 comprobaciones**. Los tipos son `MULTICHOICE_V`, tres `NUMERICAL`, `MULTICHOICE_V` y `MULTIRESPONSE`.
4. **Código de R/exams 2.4.2:**
   - `read_exercise` completa el questionlist del cloze (10 entradas) con `""` en los gaps numéricos, hasta 13 = longitud del solutionlist. Por eso `ssol = TRUE` y la mezcla de opciones y de feedback queda alineada.
   - `make_question_moodle` escribe `=` en MULTIRESPONSE solo si las fracciones positivas suman 100; es la semántica de reparto en partes iguales.
   - Las marcas `%-50%` (con dos verdaderas) y `%-33.33%` (con una) son coherentes.
5. **Render:**
   - Texto visible completo de `html/cloze2.html` (no canónica, D = color).
   - Página 1 de la canónica en PDF: la tabla coincide celda a celda con I-2 (12/11, 8/15, 16/20, 10/8).
6. **Medidas** (n = 100 por estrato):
   - Clave de P5 = nivel nombrado primero en la pregunta: 49 % (canónica), 52 % (X), 43 % (Y), 52 % (Z). No hay canal posicional.
   - «El primer nivel de `lab` es el mayor»: 58 / 54 / 53 % en las no canónicas.
   - P6 con una sola verdadera (nT = 1): 42–55 % de las versiones.
   - En P6, el veredicto de cada plantilla es invariante en el 100 % de los casos (ver H3).

## Requisitos del encargo

| Código | Estado | Evidencia |
|---|---|---|
| R3 | Cumple | Diff (punto 1); E1 del verificador con 100 semillas más 20 canónicas, `.Random.seed` idéntico |
| R4 | Cumple | 6 partes, `schoice\|num\|num\|num\|schoice\|mchoice`, `##ANSWER1##`…`##ANSWER6##` en orden (líneas 857–887) |
| R5 | Cumple (con H5) | Las Partes 2 a 6 usan N y una sola dimensión. El complemento es un evento simple de la misma bolsa. No hay condicional ni dos bolsas |
| R6 | Cumple | Ninguna parte exige DOK ≥ 3; la P6 (complemento binario) es DOK 2 |
| R7 | Cumple | P5 y P6 son afirmaciones y pares de eventos, no lecturas erróneas; guarda en las líneas 796–797 |
| R8/V11 | Cumple hacia la Parte 1; fuga P5 → P6 (H3) | Ver el análisis tras la tabla |
| R10 | Cumple, con H1 y H2 | Respuesta por parte y por contenido; la P6 se agrupa por veredicto (V6); están las 6 secciones de la regla #1; n/N con «en cada extracción» |
| R11 | Cumple | Los 16 `exextra` son idénticos al SCHOICE salvo `Type: CLOZE`; `exshuffle: TRUE` |
| R12 | Cumple | Ver el análisis tras la tabla |
| R13 | Cumple parcialmente (H4) | N = 100, y los estratos con n < 20 se declaran NO CONCLUYENTE. Faltan mutantes para 5 condiciones |

**R8/V11, análisis.** La clave de la Parte 1 equivale a un empate entre los marginales de dos dimensiones distintas, Y y Z. Las Partes 3 y 4 dan los marginales de una sola dimensión, así que no bastan.

**R12, análisis.**
- El código de la Parte 1 es el mismo byte a byte.
- Las Partes 2 a 6 no imprimen cifras ni tipos de dos características.
- La batería cruzada E3 da como máximo +3,0 pp sobre la base firmada, con un error típico de ≈ 2,7 pp (n = 300): no hay amplificación medible.

## Hallazgos

### H1. Jerga «nivel» y «dimensión» en el texto que ve el estudiante
- **Dominio:** pedagógico y gramática. **Severidad:** MEDIA (no es de CORRECCIÓN).
- **Evidencia:** «nivel(es)» aparece 7 veces en cada uno de los tres HTML renderizados:
  - análisis de las Partes 5 y 6: «uno de dos niveles», «Reescribir el evento con el otro nivel»;
  - Propiedades: «el evento «no es de un nivel» reúne las mismas fichas que el otro nivel»;
  - Estrategia y reflexión del cloze.

  El enunciado nunca define «nivel» ni «dimensión»: habla de color, categoría y figura. «Caso específico» imprime «Para la dimensión de las Partes 2 a 6: En cada extracción…». Son dos problemas: jerga sin definir y mayúscula tras los dos puntos (RAE, DPD, «dos puntos»: tras ellos va minúscula salvo casos tasados).
- **Propuesta:** sustituir los textos genéricos por versiones concretas a partir de `dim_D` y `niv`, como ya hace `regla_complemento`. Por ejemplo, «Si cada ficha es morada o gris, …»; para la categoría, «Cada ficha es de categoría alta o baja». En «Caso específico», poner `tolower` a la primera letra de `frase_p5` o escribir «…las Partes 2 a 6, en cada extracción…». **MODIFICAR.**

### H2. «esas dos parejas son igual de probables»: incoherente con las opciones mostradas
- **Dominio:** semántico (coherencia entre Solution y opciones). **Severidad:** MEDIA (no es de CORRECCIÓN).
- **Evidencia:** en el 42–55 % de las versiones (nT = 1) la Parte 6 muestra **una sola** pareja verdadera. En `cloze2.html`, por ejemplo, solo «Sacar una ficha que no sea morada y sacar una ficha gris». Aun así, la Solution afirma «esas dos parejas son igual de probables». Además, lo que es probable es cada evento, no la pareja. Un estudiante que acertó puede creer que se le escapó una.
- **Propuesta:** nombrar las parejas por contenido y por evento. Por ejemplo, `paste0("Por eso «sacar ", ficha_no(1L), "» y «sacar ", ficha_de(2L), "» son igual de probables, y también «sacar ", ficha_no(2L), "» y «sacar ", ficha_de(1L), "».")`. **MODIFICAR.**

### H3. Fuga entre partes: la Parte 5 presupone el veredicto de tres de las cinco plantillas de la Parte 6
- **Dominio:** semántico Nivel 4 y diversidad (R8, regla #22 §P4-bis y §P7-F). **Severidad:** MEDIA. **Tipo:** DIAGNOSTICIDAD; no obliga, porque no hay exceso medible > +8 pp.
- **Evidencia:**
  - La Parte 5 tiene solo dos opciones, ambas «Es más probable…»: el estudiante sabe que los niveles no empatan, y `dims_ok` lo garantiza.
  - El diseño sacó del pool la pareja directa «nivel 1 / nivel 2».
  - Pero `si1_no1`, `si2_no2` y `no1_no2` son lógicamente la misma comparación para quien domina el complemento.
  - Medido en 400 versiones: esas tres son falsas en el 100 % y `si1_no2`/`si2_no1` son verdaderas en el 100 %.
  - Con nT = 1 (42–55 %), 3 de las 4 opciones de la Parte 6 son de esas plantillas.

  Es estructural: con una sola dimensión binaria, todo par compara n1 con n1 o n1 con n2. La explotabilidad es baja, porque solo ayuda a quien entiende el complemento pero no calculó las Partes 3 y 4.
- **Propuesta:**
  - (a) Declararlo como residuo estructural (§P7-F) en el `.claude/CLAUDE.md` local y en el HANDOFF.
  - (b) Si se busca diagnosticidad, añadir a la Parte 5 la opción «Son igual de probables» y permitir empates de D en una fracción de versiones (hoy X empata en 33 de 600). Exige volver a verificar V11, porque un empate en X relaciona las diferencias de PROB-UNA y PROB-CER.

  Decisión del profesor. **MANTENER con residuo declarado.**

### H4. Contrato de mutación incompleto en las Partes 2 a 6
- **Dominio:** testing (R13). **Severidad:** MEDIA.
- **Evidencia:** 9 de las 14 condiciones de guarda de las Partes 2 a 6 tienen mutante (M1–M9). Las 5 restantes:
  - **Alcanzables sin mutante:**
    - `"pool de la Parte 6: dos parejas iguales y tres distintas"` (l. 771): protege la exclusión V11 de la pareja directa. Si se reintroduce `si1_si2`, la guarda dispara, pero ningún mutante lo prueba. M10 se caza por E2, no por esta guarda.
    - Las de longitud 6 y entero de `exclozetype`/`exsolution` (l. 807–808).
  - **Inalcanzables por construcción:** `"al menos dos dimensiones sin empate"` (l. 702), implicada por |dY| = |dZ| = |espejo| ≠ 0, y `sum(n_niv) == N` (l. 706), implicada por ser la dimensión binaria.
- **Propuesta:** mutantes M19 (`si1_si2` añadido a `parejas_p6`) y M20 (gap eliminado de `exclozetype_str`). Para las inalcanzables, declararlas como asertos estructurales con el motivo, o retirarlas, según el criterio del repo sobre guardas inalcanzables. **MODIFICAR** el verificador.

### H5. «casos posibles en cada extracción» con dos fichas elegidas
- **Dominio:** semántico (I-4, I-7). **Severidad:** BAJA.
- **Evidencia:** la Parte 1 dice «se eligen al azar dos fichas», con lo que la segunda extracción es implícitamente sin reposición. La Parte 2 afirma que N «es la cantidad de casos posibles en cada extracción», y la Solution repite «Los casos posibles en cada extracción son las N fichas».
  - Leído físicamente, en la segunda extracción quedan N − 1 fichas; la afirmación vale solo como probabilidad marginal.
  - I-7 firmó «en cada extracción» para las cifras n/N, no para «casos posibles».
  - Con |n1 − n2| = 1 (0–9 % según el estrato), la lectura condicional de la Parte 5 llega a empatar.
- **Propuesta:** «…casos posibles al extraer una ficha» (I-7 ya admite «una ficha extraída»), ajustando la guarda F-4. Es endurecer, así que puede hacerse sin firma. Si se prefiere no tocarlo, proponerlo al profesor.

### H6. En PDF y HTML las opciones van en línea, separadas por « / » y sin letras
- **Dominio:** visual. **Severidad:** BAJA.
- **Evidencia:** en `preview/pdf_canon-1.png`, las cuatro opciones de la Parte 1 (~100 caracteres cada una, con « y » dentro) forman un solo párrafo; igual ocurre en P5, P6 y en el HTML. Lo produce R/exams: `make_exams_write_pdf` usa `collapse = " / "`. No sirve para papel. Moodle, el formato de aula, sí está bien (MULTICHOICE_V verificado).
- **Propuesta:** declarar el PDF del cloze como no apto para papel, o probar una variante SCHOICE para imprimir.

### H7. La diversidad de la Parte 5 está en el techo de los pools (`WARN_DIV_BAJA`, 22/100)
- **Dominio:** diversidad (regla #22). **Severidad:** BAJA.
- **Evidencia:** 22 = 2 × 11 pares de rótulos (4 de color, 3 de categoría, 4 de figura). El techo ya se alcanza. Es sustantiva: el nivel mayor cambia (53–58 % el primero).
- **Propuesta:** ampliar `CATEGORIAS`, `FIGURAS` y `COLORES` si se quiere quitar el aviso. **MANTENER.**

### H8. Los seis gaps pesan lo mismo en Moodle
- **Dominio:** pedagógico. **Severidad:** BAJA.
- **Evidencia:** todos los gaps llevan `{1:` en el XML y no hay `expoints`. El ítem ICFES (Parte 1) vale 1/6, igual que «¿Cuántas fichas hay en total?».
- **Propuesta:** `expoints`, por ejemplo `3|1|1|1|1|1`, si el profesor quiere que la calificación refleje el ítem. Decisión del profesor.

## Verificado sin objeción

- Corrección de todos los gaps, incluida la marca en Moodle; no hay segunda clave.
- Alineación de la mezcla en el cloze.
- Semántica de MULTIRESPONSE.
- Regla #19 y V6.
- I-1 a I-8 en el CLOZE.
- No amplificación de los residuos firmados (R12).
- Ortografía con tildes: corrector limpio y mi lectura, salvo la mayúscula de H1.
- Arsenal: 0 errores.

**No auditado:**
- DOCX.
- Páginas 2–5 del PDF (la Solution en PDF).
- Vista a 360 px.
- El detalle de las 6 advertencias heurísticas 2E/2F del arsenal.

## Veredicto global: APROBAR_CON_CAMBIOS

Próximos pasos:
1. Corregir H1 y H2 (texto de la Solution) y añadir los mutantes de H4; luego volver a renderizar y correr `verificar_espacio_cloze.R`.
2. Que el profesor decida sobre H3 (declarar el residuo o rediseñar la Parte 5), H5 y H8.
3. Declarar H6 y H7 en el HANDOFF.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:959 | Si cada ficha es de uno de dos niveles, el evento «no es de un nivel» reúne las mismas fichas que el otro
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:968 | Para la dimensión de las Partes 2 a 6
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:818 | frase_p5 <- paste0("En cada extracción, la probabilidad de sacar "
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:842 | «Una ficha que no es de un nivel» es una ficha del otro nivel
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:934 | y esas dos parejas son igual de probables
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:773 | sel6 <- c(idx_T[sample.int(length(idx_T), nT)]
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:759 | no entra en el pool: la Parte 5, con sus dos opciones, ya dice que esos dos eventos
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:766 | si1_no1 = list(ev_si(1L), ev_no(1L)),
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:701 | dims_ok <- DIMS[vapply(DIMS, function(d) n_nivel(d, 1L) != n_nivel(d, 2L), logical(1))]
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:771 | "pool de la Parte 6: dos parejas iguales y tres distintas"
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:702 | stopifnot("al menos dos dimensiones sin empate" = length(dims_ok) >= 2L)
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:807 | stopifnot(all(lengths(strsplit(c(exclozetype_str, exsolution_str, extol_str), "|", fixed = TRUE)) == 6L),
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/verificar_espacio_cloze.R | AUSENTE: dos parejas iguales y tres distintas
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:736 | Ese número es la cantidad de casos posibles en
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:65 | Si se eligen al azar dos fichas de la bolsa, ¿cuáles de las siguientes fichas tienen
H6 | SIN_ARCHIVO | evidencia visual en la imagen cloze/salida/preview/pdf_canon-1.png y en el código de exams 2.4.2 (make_exams_write_pdf usa collapse " / "); no hay texto citable
H7 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/salida/paso9_diversidad.txt:8 | WARN_DIV_BAJA: la clave varia poco en p5
H7 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:253 | CATEGORIAS <- list(c("par", "impar"), c("oro", "plata"), c("alta", "baja"))
H8 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd | AUSENTE: expoints
R3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:397 | opciones_p1 <- unname(opciones_base[o])
R3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:686 | ## ==== FIN DEL BLOQUE P1 ====
R4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:803 | exclozetype_str <- "schoice|num|num|num|schoice|mchoice"
R5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:692 | el total N y UNA sola dimensión de la misma bolsa (I-4, D2.3)
R6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:999 | exextra[Nivel]: 2
R6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:1004 | exextra[DOK]: 2
R7 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:796 | "I-1 y R7: sin vocabulario de error en las opciones"
R10 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:901 | La respuesta correcta es la que afirma: «
R11 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:994 | exextra[Type]: CLOZE
R11 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_cloze_v1.Rmd:993 | exshuffle: TRUE
R12 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/salida/verificar_espacio_cloze.txt:33 | Incremento máximo sobre la base atribuible a D: +3.0 pp
R12 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/salida/verificar_espacio_cloze.txt:48 | compuesta_y_juntas: 68.0 % en las no canónicas, dentro de su firma (64.8 %)
R13 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/verificar_espacio_cloze.R:35 | N_EST <- 100L; TOPE <- 4000L
R13 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/cloze/verificar_espacio_cloze.R:113 | NO CONCLUYENTE: estrato
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS