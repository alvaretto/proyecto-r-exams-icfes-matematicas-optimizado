## Auditoría del detractor, FASE 2C r4: CLOZE hermano de `diagrama-venn-juegos-mesa-n3`

**Fecha**: 2026-11-09
**Agente**: detractor nuevo. No he auditado ninguna versión anterior de este ejercicio y no escribí ni corregí nada de lo auditado.
**Repo**: no lo modifiqué. Todo lo que ejecuté escribió en tempdir() o en `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/3530c5eb-2f14-461c-b6c9-761d2d82ab0b/scratchpad/r4det/`.

**Objetos auditados.** Los md5 en disco coinciden con el encargo y con `verificar_r7.md5`:
- `.Rmd`: `889d828e…`
- semillero: `d01bbf0f…`
- verificador: `bf11b0a4…`
- plantilla: `290a3f79…`
- `../solpcielo_cloze.tex`: `83f88427…`

**Objeciones**: 3, ninguna de CORRECCIÓN. Son 3 BAJAS: 2 de COBERTURA y 1 OTRO (documentación y estado).

### Resumen ejecutivo

No encontré ningún defecto de CORRECCIÓN en el PDF impreso con `exshuffle = FALSE`.

**Comprobaciones a mano y repeticiones:**
- **Pregunta 1 de la semilla 1, recalculada a mano** desde la página PNG, contra la hoja de respuestas de mi propio render:
  - Datos 9, 14, 10, 15, 29, 22 y 25. Regiones: s1 5, p12 9, s2 6, ctr 5, p13 10, p23 5, s3 2. Es el Diagrama IV, y la hoja marca (d).
  - La Solution dice «Diagrama IV».
  - Partes 2 a 5: 16, 11, 19 y 124, desde 21/37/27/28/71/62/67. Coinciden con la hoja.
  - Parte 6: F, V, V, F. La hoja marca (b) y (c).
- **Hoja de respuestas = `metainfo$solution`** en las 3 preguntas de la semilla 1: (d), (b) y (b) en la Parte 1, más los números y las marcas de la Parte 6.
- **Medición N = 100, repetida con el verificador final `bf11b0a4`**: cifras idénticas a `salida/t5/medir_n100_pdf.txt`:
  - 100 de 100 gaps en el orden I-IV;
  - 100 de 100 claves en el diagrama correcto;
  - 0 figuras partidas;
  - claves a=25, b=20, c=32, d=23 (χ² = 3,12, 3 gl, p ≈ 0,37).
- **Sección `uso` corrida aparte**: reproduce `uso_aislado.txt` línea por línea. Los 17 mutantes caen como se declara, con 0 `mal_construido`.

Los tres hallazgos no tocan la clave:
- **H1:** un punto ciego de USO_ARCHIVOS.
- **H2:** la obligación 7 del contrato se cumple solo en parte, y hay una captura que no se usa.
- **H3:** tres textos desfasados en documentación y estado.

### Respuestas a las seis preguntas

**1. Semillero.**
- **Solo lo firmado.** Volví a calcular `diff -u` desde la copia r3 (md5 `69194749…`) y sale idéntico byte a byte a `semillero_r3_a_t5.diff`. Tiene 3 hunks: la cabecera, `archivos = 100` y el bloque de `generar_pdf()` con su comentario más `exshuffle = FALSE,`. No hay nada más.
- **Qué funciones aceptan `exshuffle` en exams 2.4.5:**
  - Lo tienen como argumento formal solo `exams2html`, `exams2pandoc`, `exams2particify` y `exams2pdf`. En `exams2pdf` está en la posición 24, con valor por defecto NULL, y viaja como `read = list(exshuffle = exshuffle)`.
  - `exams2moodle`, `exams2canvas` y `exams2qti21` no lo tienen; solo `...`.
  - El semillero no se lo pasa a ninguna otra función, tampoco a la `exams2html` de `prueba_rapida`.
- **Comentario del semillero:**
  - Es verdad que «la Parte 6 sortea el orden de sus afirmaciones»: `pares6` sale de `sample.int` y `v6` se sortea.
  - «allí el estudiante elige el texto»:
    - es cierto en Moodle;
    - el HTML del semillero es un documento estático con soluciones, donde nadie elige;
    - Canvas no lo ejecuta ninguna sección.
  - Es una nota sin severidad.

**2. Sondas.**
- **USO_ARCHIVOS** puede dar verde con el defecto presente (H1).
- **USO_EXSHUFFLE** da rojo con un semillero correcto en tres casos, que medí:
  - `exshuffle = F`;
  - `exsh = FALSE` (R lo acepta como coincidencia parcial, porque el argumento va antes de `...`);
  - `exshuffle = TRUE` explícito en `exams2html`.

  Falla cerrado, así que lo acepto como guarda literal. Su verde con defecto lo cubre USO_ORDEN_PDF, que es de comportamiento. Con 3 preguntas, salir en orden por azar tiene probabilidad (1/24)³.
- **USO_CLAVE_PDF:**
  - **No lee `sol_p1` ni la Solution.** En USO_ESPIA, `sol_p1` está solo para la `materia` de U14. Lee `metainfo$solution`, que es `sol_p1` tal como lo registra R/exams vía `exsolution`, y es justo lo que imprime la hoja (lo comprobé arriba).
  - **El oráculo es propio.** `leer_vinetas` calcula las regiones con sus fórmulas (ctr = b − a, …), no con las `FORMULAS` del `.Rmd`.
  - **Límite declarado.** El paso «archivo → regiones» sale del entorno (`regiones_mezcladas`), no de los píxeles del PNG. Esa parte la cubren E1 (`dibujo_opciones` idéntico al SCHOICE) y la sección `moodle` («imagen de la clave contra el entorno de su semilla: 100 de 100»).
- **Puntos ciegos de la captura:**
  - **`trace()`:** cubre las llamadas sin prefijo, con `exams::`, con `exams:::` y con `do.call`. No ve un envoltorio local que tape a `exams2pdf`, pero el árbol sintáctico vería la llamada interna. Si `exams2pdf` aborta, `res` queda en NULL y la sonda falla cerrado.
  - **Emparejamiento por `fig_id`:** si hay 0 o 2 coincidencias, falla cerrado.
  - **LaTeX:** cualquier cambio de formato del rótulo falla cerrado (U17).
  - **`pdftotext`:** la Solution escribe «Diagrama I:», no la secuencia con «/», así que no hay coincidencia falsa.

**3. U10 a U17.**
- **Obligación 1 (sonda declarada):** cumplen todos.
- **Obligación 2 (`otra` declaradas):** las de U10 y U11 son legítimas:
  - quitar el argumento tumba también, por fuerza, el árbol sintáctico;
  - `exams2moodle` aborta con `exshuffle`, así que «Moodle sin barajar» no se puede alcanzar por esa vía;
  - U13, U15 y U16 aíslan cada componente.
- **Obligación 3 (`materia`):** los 8 mutantes la tienen sobre lo capturado.
  - La de U11 lee el mensaje de error y no depende del idioma.
  - La de U12 hereda el punto ciego de H1.
- **Obligación 4 (patrón único):** `mutar()` lo impone, y hubo 0 mal construidos.
- **Obligación 5 (semillas con tope):** todos mueren en la semilla 1, con tope 5.
- **Obligación 8 (línea base):** en la semilla 1 es la medición principal, y `set.seed` por formato la hace comparable.
- **Obligación 7:** incompleta (H2).

**4. La r7 es evidencia válida.**
- Los mtime de los cuatro archivos son anteriores a su inicio (17:38:15).
- Las otras 11 secciones son idénticas a la r6, línea por línea, salvo los tiempos y la cabecera inicio/fin. Termina con EXIT 4, como la r6.
- El EXIT 3 de `uso_aislado.txt` es intencional: `decidir_exit` da 3 cuando el modo no es `todo`.

**5. Subsección de la decisión.** Las cifras y los literales coinciden con lo medido: `ops <- ops[sample.int(4L)]` en la línea 287, `v6` en la 380, el argumento formal y 100 de 100. Hay un solo choque, con el conflicto 3 (H3 b).

**6. CORRECCIÓN.** Ninguna; la evidencia está en el resumen ejecutivo.
- En la Parte 6, las verdaderas por posición son 47/47/60/44. Por construcción cada posición es verdadera con p = 7/14, porque `v6` es uniforme sobre las 14 filas de `COMB6`. El 60 es ruido de muestreo (z = 2,0 para el máximo de cuatro posiciones), y aparecen los 14 patrones.
- Sin barajar, el orden impreso es el interno, que es uniforme. No se abre ningún canal posicional nuevo.

### Hallazgos

#### H1 · BAJA · COBERTURA · USO_ARCHIVOS da verde con 250 exportaciones si se reasigna `config$archivos`

- `sondas_llamadas_uso` solo mira la asignación `config <- list(...)` (línea 1570).
- `cargar_semillero` evalúa solo las 7 asignaciones de nivel superior (línea 1530).
- **Medido en memoria:** añadí `config$archivos <- 250` (o `config[['archivos']] <- 250`) después de la lista. La sonda devuelve vacío y `cargar_semillero` sigue viendo 100. El semillero real exportaría 250 en HTML, Moodle y Canvas. La `materia` de U12 tiene el mismo punto ciego.
- **Hoy no pasa:** la única asignación es la de la línea 53.
- **Alternativa:** dar rojo ante cualquier asignación de nivel superior cuyo destino empiece por `config`, aparte de la lista.
- **Consecuencia:** residuo declarado, porque el verificador no se cambia.

#### H2 · BAJA · COBERTURA · Obligación 7 incompleta y una captura que nada usa

- **Ramas de falla cerrada sin mutante propio:**
  - USO_CLAVE_PDF tiene 7 ramas y solo U14 y U17 aíslan alguna. Las que no tienen mutante son 5: sin salida de `exams2pdf()`, número de gaps, número de entornos (línea 1609), clave que no es una de cuatro, y entorno o viñetas.
  - USO_ORDEN_PDF: «3 gaps».
  - USO_EXSHUFFLE: «hay una llamada a `exams2pdf`» y el fallo de `parse`.
  - USO_ARCHIVOS: «una sola asignación».
- Todas fallan cerrado, así que no producen verdes falsos.
- **Captura sin uso:** `captura$moodle_orden` (línea 1692) se calcula y ninguna sonda ni `materia` la usa. Choca con la nota de la obligación 2: «si lo calculo, lo uso».
- **Consecuencia:** residuo declarado, de la misma familia que el H3 b de la r3.

#### H3 · BAJA · OTRO · Documentación y estado desfasados respecto de la cuarta pasada

- **(a) Un puntero falso en el verificador.** La línea 1548 dice que la medición de N = 100 «está en el HANDOFF». `HANDOFF.md` (mtime 16:54:53, anterior a la tanda 5) no la contiene; está en `salida/t5/`.
- **(d) Esa medición no está atada al verificador final.** `medir_n100_pdf.txt` no registra el md5 del verificador, y arrancó a las 17:35:49, antes del mtime del verificador (17:37:06). La repetí con `bf11b0a4`: las cifras son idénticas.
- **(b) El conflicto 3 quedó desfasado.** `SUB/.claude/CLAUDE.md`, línea 213, sigue diciendo «Se barajan los textos del gap», sin la excepción del PDF.
- **(c) El estado sellado contradice lo que pasó.** La nota de `detractor_fase2c` en `ejercicio_state.json` (línea 58) afirma «no se cambió nada tras el veredicto» con el semillero `69194749`. No hay ninguna `reaperturas`.
- **Propuesta:**
  - escribir en el HANDOFF la medición, con el md5 `bf11b0a4`; con eso la línea 1548 queda cierta sin tocar el verificador;
  - anotar en el conflicto 3 la excepción del PDF;
  - hacer `workflow-state.sh reopen --desde detractor_fase2c --motivo "cuarta pasada «H1 c · H2 100»"` antes de volver a sellar.

### No auditado

- Canvas: ninguna sección lo ejecuta.
- DOCX.
- La nota del MULTIRESPONSE en Moodle (H5 de la r3).
- Los píxeles de los PNG más allá de la pregunta 1 de la semilla 1.
- El flujo interactivo del semillero (opciones 1 a 5).

### Próximos pasos

1. H3: actualizar el HANDOFF y el conflicto 3, y reabrir el estado con `reopen` antes de volver a sellar.
2. H1 y H2: declararlos como residuos.

CITAS_DETRACTOR
H1 | A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R:1570 | cfg <- Filter(function(x) is.call(x) && identical(x[[1L]], as.name("<-")) && identical(x[[2L]], as.name("config")), as.list(ex))
H1 | A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R:1530 | as.character(x[[2L]]) %in% USO_OBJETOS) eval(x, es)
H1 | A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R:1569 | !all(vapply(sh[!es_pdf], is.null, TRUE))) f <- c(f, "USO_EXSHUFFLE")
H2 | A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R:1692 | if (length(xml) == 1L) captura$moodle_orden <- orden_gap1_xml(xml)
H2 | A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R:1609 | if (length(cap$env) != nq) return(falla("-", sprintf("%d entornos capturados", length(cap$env))))
H2 | .claude/agents/orquestador-cloze.md:1422 | cuyo resultado no condiciona nada es código muerto: si lo calculo, lo uso.
H2 | .claude/agents/orquestador-cloze.md:1449 | Cada componente de una sonda tiene un mutante que la mueve sola
H3 | A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R:1548 | la medición de N = 100 de la tanda 5 está en el HANDOFF.
H3 | A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/HANDOFF.md | AUSENTE: medir_n100
H3 | A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/.claude/CLAUDE.md:213 | Se barajan los textos del gap; los rótulos del enunciado no
H3 | A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/ejercicio_state.json:58 | Sin pasadas restantes: no se cambió nada tras el veredicto
H3 | A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/salida/t5/medir_n100_pdf.txt:2 | md5 .Rmd: 889d828e8129e11b0ec5dceda0cf108b · md5 semillero: d01bbf0f565ea6d651fba05fc795a268
H3 | A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/salida/t5/medir_n100_pdf.txt | AUSENTE: bf11b0a4
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
