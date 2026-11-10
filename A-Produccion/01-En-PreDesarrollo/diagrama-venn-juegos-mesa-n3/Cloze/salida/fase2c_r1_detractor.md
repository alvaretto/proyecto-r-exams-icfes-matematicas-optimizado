# Auditoría Detractor, FASE 2C r1: CLOZE hermano de `diagrama-venn-juegos-mesa-n3` (MAT-2026-07-014)

**Fecha**: 2026-11-09 · **Agente**: AgenteDetractor nuevo. No escribí ni corregí nada de lo auditado.
**Objetos**: `.Rmd` md5 `301ceddde131ad70d70cc26b17a30acf` y `verificar_espacio_cloze.R` md5 `8691ac257ae071fb2e5aeccdee2e29ee`, comprobados en disco al empezar; además, la sección «CLOZE hermano» de `.claude/CLAUDE.md`.
**Repo**: no lo modifiqué. Todo lo que ejecuté corrió en el scratchpad.
**Objeciones**: 7 en total. Ninguna es de CORRECCIÓN ni crítica; hay 1 alta, 4 medias y 2 bajas.

## Resumen ejecutivo

No encontré ningún defecto de CORRECCIÓN.

- **Las seis claves son verdaderas** en las 6 ramas y en la canónica:
  - El oráculo del verificador juzga desde el texto: 3686 semillas, sin ningún código.
  - Yo lo repetí por mi cuenta sobre el XML real de Moodle. Calculé las regiones con inclusión-exclusión desde las viñetas del propio XML: 5 de 5 preguntas alineadas en los seis gaps y 20 de 20 afirmaciones de la Parte 6 bien marcadas.
- **La Solution y la retroalimentación son verdaderas**: revisé cada frase contra los datos.
- **La firma de C-3 y C-4 cuadra**: con las tasas firmadas se reproducen las cifras +12,5, +6,8, +5,1 y +2,0 pp.

Los hallazgos son de cobertura del verificador y uno de concordancia gramatical:

- **El más serio (H1, ALTA)**: nada comprueba que las marcas del XML de Moodle coincidan con la verdad. Lo exigen la regla #21 F4 y la I-4.8, que rige la Parte 1. Hoy el XML está bien, pero porque lo medí yo, no porque una sonda lo vigile.
- **Concordancia (H4)**: «las 16 … que quedaron restados» aparece en el 5 % de las variantes.

## Hallazgos

### H1 · ALTA · Ninguna sonda compara las marcas del XML de Moodle con la verdad

**Qué falta.**
- La sección `pesos` lee `salida/moodle/cloze.xml` y solo mira dos cosas: que los pesos sean `1|1|1|1|1|1` y que ningún gap lleve imagen.
- No mira qué opción lleva `=`, ni los valores de NUMERICAL, ni las fracciones de MULTIRESPONSE, ni el feedback, ni la imagen de la Solution.
- Además, el XML que lee es un artefacto de `salida/` que no está atado al md5 del `.Rmd`, así que puede estar viejo.
- La I-4.8 («Comprobar lo que llega al LMS…») rige la Parte 1, y la regla #21 F4 pide verificar marca frente a verdad en el XML de Moodle.
- E1 solo prueba la identidad del bloque trasplantado. El gap 1 de texto, el feedback `fb_p1` y la Solution del CLOZE son nuevos, y E1 no los cubre.

**Lo que medí** (`python3 -I` sobre `cloze.xml` md5 `bd099fbf…`, 5 preguntas, comparando con la verdad calculada desde las viñetas del XML):

| Pregunta del XML | Gap 1 (`=`) | Solution y feedback | NUMERICAL (XML = verdad = feedback) | Parte 6 |
|---|---|---|---|---|
| pregunta 1 (canónica) | `=Diagrama IV` | Dicen «Diagrama IV»; la imagen de la Solution es `diagrama_d_95772775.png`, la del rótulo IV; centro 7, zona doble 3, solo parqués 7 | 14, 19, 18, 122 | 2 `=` verdaderas, 2 `%-50%` falsas |
| pregunta 2 | coincide | coincide | 24, 5, 27, 143 | 1 verdadera, 3 falsas con `%-33.33333%` |
| pregunta 3 | coincide | coincide | 15, 16, 18, 127 | marcas = verdad |
| pregunta 4 | coincide | coincide | 19, 4, 25, 121 | marcas = verdad |
| pregunta 5 | coincide | coincide | 17, 13, 21, 128 | marcas = verdad |

En las cinco: ningún empate en la Parte 6, ningún nombre de archivo no neutral (el grep sale vacío) y ningún gap con imagen.

**Propuesta.** Añadir una sección `moodle` a `SECCIONES`, de modo que el exit 4 la exija:
1. Que haga su propio `exams2moodle` en `tempdir()` desde el `.Rmd` vigente, como ya hace `v6` con el HTML. Familia B, con el N declarado.
2. Que compruebe, desde las viñetas del XML:
   - el `=` del gap 1, el rótulo de «La respuesta correcta es el …», el de «: Correcta.» y la imagen que lleva ese rótulo;
   - los NUMERICAL;
   - que cada marca positiva del MULTIRESPONSE sea una afirmación verdadera.
3. Que tenga mutantes en copias temporales que mueran por esa sonda: `exsol_p3` y `exsol_p4` cruzados, y `sol_p6` invertido con su guarda quitada.
4. Que `pesos` deje de leer el XML de `salida/`, o que verifique de dónde viene.

### H2 · MEDIA · La I-4.7 y la I-5 (papel y 320 px) rigen la Parte 1 pero no tienen guarda en el CLOZE

**Solution (I-4.7).** La I-4.7 dice que todo párrafo nuevo, reescrito o movido da rojo hasta que tenga su comprobación. El CLOZE:
- reescribió párrafos de la Parte 1: «La respuesta correcta es el Diagrama N…», «Revisar solo los totales no basta…» y los prefijos `*Diagrama N.*`;
- añadió los párrafos de las Partes 2 a 6.

`SECCIONES` no tiene ninguna sección `solucion`. La sección `v6` empareja rótulos y grupos, pero no contrasta las cifras de la prosa. Yo revisé el contenido y está bien.

**Papel (I-5).** No hay sección `papel` que vigile las páginas. Lo medí sobre los 4 PDF:
- `pdfimages -list`: los cuatro diagramas están en la página 2 y el de la Solution en la 3.
- `pdftotext -bbox`: en las 4 páginas, también las de la Solution, todas las palabras quedan entre x = 28,3 y 583,9 pt, sobre un ancho de página de 612,3 pt. Son márgenes de 1 cm.
- La página mide 612,28 × 935,43 pt, es decir, 216 × 330 mm.

**320 px.** La 6b se corrió a 360 y 1024 px sobre el md5 anterior (412c61bb) y no se repitió sobre 301ceddd. La I-5 exige 320 y 360 px, y el contenedor de las figuras cambió.

**Propuesta.** Portar `solucion` y `papel` desde `../verificar_espacio.R`, y repetir la 6b a 320 px.

### H3 · MEDIA · La medición del cruce (§P7-G) no tiene ninguna regla aplicable e independiente, ni control, y no mide la canónica

Medí las reglas de `sec_cruce` con N = 100 variantes y 20 canónicas:

| Regla de `sec_cruce` | Qué pasa en realidad |
|---|---|
| `X-mas-numeros-en-comun` y `X-menos-numeros-en-comun` | Aplicables en 0 de 100 variantes y 0 de 20 canónicas: la V11 deja en cero los números en común |
| `X-centro-cercano-al-centro` | Elige lo mismo que «centro mayor» en 100 de 100 y en 20 de 20. Los centros de la Parte 1 son ≤ 12 y el de la otra encuesta es ≥ 13 |
| `X-maximo-cercano-al-mayor-dato` | Coincide con «número mayor» en 100 de 100 y en 20 de 20 |
| `X-suma-lejana-al-total` | Coincide con «suma mínima» en 94 de 100 y en 20 de 20 |

- **La cifra del HANDOFF no es una medición.** El «máximo +0,0 pp» que reporta sale de reglas que nunca aplican, a las que el código asigna 0,25 por defecto.
- **La canónica no se mide**: `muestra_var()` solo toma variantes. Allí `X-centro-cercano` da 50,0 %. Coincide con `MAG-centro-mayor`, que ya está firmada en la canónica con 50,000 %, así que no hay un canal nuevo ni hace falta otra firma.
- **No hay control positivo.**

**Propuesta.**
- Tomar el máximo solo sobre las reglas aplicables.
- Reportar el resultado real: «no hay canal cruzado independiente; los cruces se reducen a reglas ya firmadas, por la separación de escalas 13–24 frente a ≤ 12».
- Medir la canónica con n = 20.
- Añadir un mutante que quite la V11, con su filtro y su guarda: debe volver aplicable la regla de números en común.

### H4 · MEDIA · Concordancia de género en el feedback del gap 4 y en la Solution (regla #7, concordancia dinámica)

En la rama Parte 4 = solo el tercer conjunto, con el grupo «personas» (contexto `dialogo`), el texto dice:

> «… y se suman las 16 de los tres géneros, que quedaron restados dos veces»

Debería decir «restadas». El artículo `art2` se resuelve por rama, pero el participio está fijo.

- **Frecuencia**: 5 de 100 variantes (N = 100). La probabilidad exacta es 1/7 × 1/3 = 4,76 % y es la misma en la canónica; en mis 20 canónicas salieron 0.
- **Ejemplo**: semilla 9.
- **Dónde se ve**: en el feedback general de Moodle (gap 4) y en la Solution, en la viñeta de la Parte 4.

**Propuesta.** Cambiar a `if (fem2) "restadas" else "restados"` y añadir una guarda o un test. Recomiendo hacerlo en esta misma pasada: es una línea.

### H5 · MEDIA · La invariante «sin vocabulario de error» (C-2 y conflicto 4) no tiene prueba de detección propia

- La guarda del `.Rmd` junta dos componentes: «pero no» y el vocabulario de error.
- M13 y M14 desactivan la guarda entera con `TRUE ||` y solo prueban «pero no».
- El oráculo no tiene un componente para el vocabulario de error, y el verificador no contiene el patrón.

**Lo que probé.**
- Hice un mutante en una copia en memoria, que añade «cuidado con el error» a `texto_p6`. Con la semilla 1 muere por la guarda del `.Rmd`, así que la guarda funciona.
- Pero el contrato de mutación (obligación 7 y cobertura mínima) exige declararla `sin_prueba_de_deteccion`, y el reporte no lo hace.
- Además, el patrón no detecta «erróneo» ni «errónea»: `grepl` devuelve FALSE.

**Propuesta.**
- Añadir un mutante nuevo cuya sonda sea `GUARDA_RMD: Partes 2 a 6: sin la frase`.
- Añadir el componente correspondiente al oráculo.
- Añadir `err[oó]ne` al patrón.

### H6 · BAJA · `sec_texto` aborta en vez de contar cuando el oráculo devuelve un código temprano

- **Qué pasa**: si el oráculo devuelve un código temprano (`ESTRUCTURA`, `VINETAS`, `P1_ROTULOS`, `PREGUNTAS` o `P6_FORMA`), `rama` queda en NULL. Entonces `if (is.na(j$rama))` se evalúa sobre un vector vacío, y R aborta con «argumento tiene longitud cero». Lo reproduje.
- **Efecto**: el verificador falla cerrado (R sale con exit 1), pero sin RESUMEN ni diagnóstico. Los mutantes M17 a M22 no pasan por esa línea, así que ninguno lo ve.
- **Propuesta**: `if (is.null(j$rama) || is.na(j$rama)) next`.

### H7 · BAJA · La aceptación se midió con 400 semillas (regla #23)

- **Qué pasa**: el HANDOFF dice «Vuelto a medir en 400 semillas». El estándar es N = 100, y 400 es justo el ejemplo que la regla #23 prohíbe.
- **Riesgo**: bajo. La cifra no decide nada, y el oráculo recorrió 3686 semillas sin ningún aborto de la cascada F1.
- **Propuesta**: medir de nuevo con N = 100, o declarar la excepción (depuración).

## Verificado sin objeción

**(a) Claves.**
- Hice la cuenta yo mismo, desde las viñetas. Pregunta 1 del XML (canónica):
  - Parte 2: 38 − 24 = 14.
  - Parte 3: 33 − 14 = 19.
  - Parte 4: 75 − 24 − 33 = 18.
  - Parte 5: 122.
  - Parte 6: 14 < 24 es V, 18 < 14 es F, 23 > 18 es V y 19 < 10 es F.
- La Parte 1 canónica tiene como clave el diagrama 7, 3, 9, 7, 0, 1, 5.

**(b) Lecturas y redacción.**
- No encontré ninguna lectura alternativa razonable en «solo X y Y», «los/las tres …» ni «al menos uno/una de …».
- La Parte 6 solo compara zonas del mismo tipo.
- Revisé los textos de los contextos `retorica`, `dialogo` y `estudiante`: todo concuerda salvo H4.
- Tildes correctas. Ningún glifo prohibido; solo hay «…» en un comentario de R, en la línea 7.

**(c) Fugas entre partes.**
- En la Parte 6, la palabra «Más/Menos» y el orden de las zonas son independientes del veredicto, porque el intercambio solo depende de la verdad previa, que es Bernoulli(½).
- La Parte 6 no revela ningún valor de las Partes 2 a 5.

**(d) Solution.**
- Todas sus afirmaciones son verdaderas, incluidas «uno de los diagramas los conserva todos…» y la reflexión «a una resta» (p12 = b − centro).
- La prosa va agrupada por mecanismo o por veredicto. La sección v6 sobre el HTML real dio 100 de 100 sin códigos, con la mezcla activa en 95 y en 96 de 100.

**(e) Verificador.**
- Los controles de E1 disparan.
- Revisé uno por uno M7, M11b y M13 a M22: cada mutante muere por su sonda y por la razón correcta, con línea base, en la semilla 1, 3 o 6.
- `decidir_exit` exige todas las secciones, el md5 y la firma.
- En las variantes, la columna «medida» de C-4 es la tasa exacta de `exacto.py`. Vale por la identidad que prueba E1; no es una medición nueva sobre el CLOZE. La canónica sí se midió de nuevo y cuadra con las 12 reglas firmadas.

**(f) XML de Moodle.** Pesos, gaps sin imagen, nombres neutrales y marcas: todo bien (ver H1).

**(g) PDF en oficio.** Los cuatro diagramas van juntos y no hay nada fuera del margen (ver H2).

**Estado.** En `ejercicio_state.json`, los pasos 3 a 6 se sellaron en 0,17 s. Los audité por artefacto: los renders (13:25–13:26) son posteriores al `.Rmd` (13:24), y existen las salidas del hook y las de r2 y r3.

**No auditado.**
- El DOCX, salvo que trae 4 imágenes.
- La vista a 320 px.
- Cómo puntúa Moodle el `=` repetido en MULTIRESPONSE. Esa conversión la hace R/exams 2.4-5 y no depende del ejercicio.

## Próximos pasos
1. H1, antes de la FASE 3, porque una objeción ALTA bloquea.
2. H4, una línea, en esta pasada.
3. H2, H3 y H5.
4. H6 y H7.

Según el HANDOFF, el coordinador guarda este reporte en `salida/fase2c_r1_detractor.md`. Yo no escribí nada en el repo.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R:270 | xml <- file.path(aqui, "salida", "moodle", "cloze.xml")
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R | AUSENTE: generalfeedback
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/.claude/CLAUDE.md:75 | **Comprobar lo que llega al LMS** en el XML real de Moodle
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/.claude/CLAUDE.md:70 | **Ejecutar el chunk `solucion`** en la canónica y en N = 100
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R:598 | SECCIONES <- c("e1", "texto", "pesos", "cruce", "bateria", "mutantes", "v6")
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/HANDOFF.md:47 | No se volvió a correr la 6b completa sobre 301ceddd
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R:304 | c(acierto = if (any(S) && !all(S)) S[k] / sum(S) else 0.25, aplica = any(S) && !all(S))
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R:292 | m <- muestra_var()
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/HANDOFF.md:36 | Cruce §P7-G: máximo +0,0 pp
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_cloze_v1.Rmd:485 | los_tres2, ", que quedaron restados dos veces")
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_cloze_v1.Rmd:368 | art2 <- if (fem2) "las" else "los"
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_cloze_v1.Rmd:458 | "Partes 2 a 6: sin la frase «pero no» ni vocabulario de error (C-2)"
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R:410 | materia = function(e) grepl("pero no", e$texto_p5, fixed = TRUE)),
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R | AUSENTE: confund
H6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R:244 | if (is.na(j$rama)) next
H7 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/HANDOFF.md:25 | Vuelto a medir en 400 semillas
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
