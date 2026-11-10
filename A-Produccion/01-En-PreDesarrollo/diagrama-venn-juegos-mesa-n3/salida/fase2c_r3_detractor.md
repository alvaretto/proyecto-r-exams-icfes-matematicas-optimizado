# Auditoría Detractor: `diagrama-venn-juegos-mesa-n3` (FASE 2C, ronda 3)

**Fecha:** 2026-11-06
**Directorio:** `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3`
**Auditado:** `.Rmd` con md5 `f886dfaa45b80a5ee16f9820908049b5` y `verificar_espacio.R` con md5 `6abb80ed0440363b6af00c7320dc3d62`, comprobados al empezar y al terminar, más las invariantes locales. No edité nada del subproyecto.
**Objeciones activas:** 7 (0 de CORRECCIÓN, 0 críticas, 0 altas, 3 medias, 3 bajas, 1 de DIAGNOSTICIDAD para firma)

## Resumen ejecutivo

**No encontré ningún defecto de CORRECCIÓN en este md5.** El `.Rmd` solo cambió en dos frases, una frase literal y el ancho de cinco figuras. El diff contra la copia limpia de r2 es idéntico a `salida/diff_r2_r3_rmd.txt`. H9, H12, H13, H14, H15 y H16 quedan corregidos en lo que pedían.

Los problemas están otra vez en lo que el verificador promete y no prueba:

- **H17, la objeción principal:** nada protege la pregunta. Con «¿cuál diagrama de Venn NO organiza…?» la clave marcada es falsa en todas las versiones, y `moodle`, `cableado` y `solucion` siguen en verde.
- **H18:** `extype: mchoice` pasa la sección `moodle`, aunque cambia el tipo de pregunta y la puntuación.
- **H19 y H20:** quedan huecos menores en I-1 y en la comparación de la Solution.

Ninguno es un defecto vivo, y todos se corrigen sin tocar el `.Rmd`, así que su md5 puede conservarse.

## Los siete puntos del encargo

| # | Punto | Respuesta |
|---|---|---|
| 1 | H9 | Cumple en papel, pantalla, XML de Moodle y tamaño en DOCX. `!important` solo decide el ancho en pantalla: sin él, 207,9 px fijos y sin desborde. Residuo: el DOCX parte las opciones |
| 2 | H12 | X1 a X12 mueren por `SOL_TEXTO`. La comparación prueba que cada párrafo no cambió, no que sea verdadero, y no ve el orden ni los bloques `{=latex}` (H20) |
| 3 | H13 | `exsolution` invertida o fija y `feedback` invertido no pasan. Sí pasan `extype` y `exshuffle` (H18) |
| 4 | H14 | Color y tamaño cubiertos. En `figura` siguen sin comprobarse serie, nivel de gris, ancho, marcas bajo la línea base y posición (H19) |
| 5 | H15, H16 | Verdaderas y legibles con resultado negativo, cero o positivo, y con la zona vacía |
| 6 | Sonda `papel` | Correcta: sus 3 mutantes mueren por su sonda y N = 100 da lo mismo que N = 8. Límites en H21 |
| 7 | Afirmaciones absolutas | Dos imprecisas, ver la tabla de afirmaciones |

## Qué ejecuté

| Prueba | Alcance | Resultado |
|---|---|---|
| `verificar_espacio.R todo`, el real | Todas las secciones | Exit 3. Sus líneas de resultado coinciden con `verificar_espacio_r3.txt`, salvo tiempos. 70 de 70 mutantes mueren por su sonda |
| Papel, N = 100 | Textos más largos, 5 canónicas y azar | 100 PDF con las 4 figuras en la página 2, base de d) 289,8-290,5 mm, holgura mínima 19,5 mm, 0 fallas |
| Papel con `solpcielo_cloze.tex` | Las 8 semillas de la sonda | Misma base, 0 fallas |
| Alineación de semillas | 60 XML (20 canónicas) y 8 PDF | 60/60 con el texto de `correr(k)`, 8/8 con el contexto declarado, 0 `$` crudos |
| Rutas reales de los semilleros | DOCX con `pcielo.tex` y `plain.tex` (2 semillas), Moodle con sus opciones (3 versiones), webquiz | 5,5 cm, 0 fugas de LaTeX, `style` intacto |
| Navegador | Webquiz a 320, 360 y 1024 px, con y sin `!important` | Ver H9 |
| Mutantes propios de I-1 | 7 y control, N = 6 | 5 sobreviven a `figura` (H19) |
| Mutantes propios de la Solution | 7 y control, N = 40 | 7 sobreviven (H20) |
| Mutantes propios del LMS y del texto | 6, sobre 8 versiones | 1 muere, 5 sobreviven (H17, H18) |
| Frases de H15 | 4 000 semillas | Negativo 1 227, cero 195, positivo 789, zona vacía 101; `VN-X-RESTA-UNA` 1 789 |

## Hallazgos heredados

| ID | Clase en r2 | Estado | Comprobación |
|---|---|---|---|
| H9 | MEDIA | Corregido, con residuo BAJA | Ver la sección H9 |
| H10 | DIAGNOSTICIDAD | Abierto, pendiente de firma | Ver la sección H10 |
| H12 | MEDIA | Corregido, residuo en H20 | X1 a X12 mueren |
| H13 | MEDIA | Corregido, residuo en H18 | L1 a L5 mueren; `exsolution: 1000` da `MOODLE_FRACCION` |
| H14 | MEDIA | Corregido, residuo en H19 | M32 y M33 mueren |
| H15 | MEDIA | Corregido | `B − d − c = s3 − z`, y `z ≥ 2`: «se queda corto en z» vale con cualquier signo. `P − b = s1 + p13`, con `p13 > 0` por precondición |
| H16 | BAJA | Corregido | 3 + 3 + 1 = 7, y ya no queda «cuatro intersecciones» |

Las objeciones cerradas en r1 y r2 y la enumeración exacta las doy por buenas sin repetirlas: el diff no toca fórmulas, regiones, cotas ni plantillas. `espacio` volvió a dar 0 en 11 000 000 de versiones.

Texto leído en H15, canónica incluida por su fórmula: «en $13 - 7 - 8 = -2$ el centro quedó restado dos veces… se queda corto en 7. Hay que sumarlo una vez: $-2 + 7 = 5$». Con zona vacía: «$20 - 8 - 14 = -2$… corto en 2… $-2 + 2 = 0$». En 60 XML no quedó ningún `$` crudo.

## H9 — Papel, pantalla, Moodle y DOCX (BAJA, residuo)

| Canal | Resultado | ¿Depende de `!important`? |
|---|---|---|
| PDF con `solpcielo.tex`, 216 × 330 mm | N = 100, 5/5 de `render_r3/pdf` y la canónica: 4 figuras en una página | No |
| `exams2html` (`meas.txt`, `mobile=FALSE`) | 209, 249 y 280 px a 320, 360 y 1024. Los 15 px de barra explican parte de la diferencia con r2 | Sí, sobre 208 px |
| Webquiz (`mobile=TRUE`) | 268, 280 y 280 px, sin desborde | Sí |
| XML de Moodle | `style="width:280px!important;max-width:100%;width:5.5cm"` en 25/25 y, con las opciones del semillero, en 15/15 | El XML lo conserva; el servidor no se probó |
| DOCX | 1 979 999 EMU, es decir 5,5 cm, en `pcielo.tex`, `plain.tex` y `render_r3` | No |

**Si `!important` se pierde:** medido en el navegador, la figura queda en 207,9 px a 320, 360 y 1024 px, sin desborde. El dígito baja a unos 12,7 px, del orden del caso de 209 px. `max-width:100%` sigue actuando.

HTML Purifier elimina `!important` por defecto. Eso solo pesaría si el Moodle limpia el HTML, por ejemplo con `$CFG->forceclean`. No comprobé esa opción en el servidor del colegio.

**Residuo, DOCX:** en carta y con LibreOffice, la ruta real del semillero deja la primera figura en la página 1 y las otras tres en la 2. `render_r3` deja dos y dos. I-5 no dice nada del DOCX y ninguna sonda lo mira.

**Alternativa:** declarar en I-5 que el DOCX no conserva las cuatro opciones juntas, o fijar el tamaño de hoja con un `reference.docx` de oficio si ese canal se imprime.

**Veredicto:** MANTENER, con la declaración.

## H10 — Batería §P7 (DIAGNOSTICIDAD)

Sigue en rojo y sin firma, por eso el exit 3. Las cifras exactas de r2 siguen vigentes, porque el diff no toca los datos. `verificar_espacio.R` sigue imprimiendo las de N = 100, por ejemplo 65,3 % donde el valor exacto es 69,55 %, con un margen de +37,4 pp contra los +33,1 pp impresos. No volví a medir el margen §P7-B.

**Alternativa:** pedir la firma regla por regla con la columna exacta de r2 y cablear ese alcance (§P7-E).

**Veredicto:** MANTENER, con firma humana.

## Objeciones nuevas

### H17 — La pregunta no tiene guarda (MEDIA)

**Qué se cuestiona:** I-1 fija la pregunta e I-2 dice que la instancia es exacta, con `stopifnot` contra literales. Pero `pregunta` no está en el `stopifnot` de I-2, y en `verificar_espacio.R` esa palabra solo aparece en un comentario.

**Por qué:** invariantes I-1 e I-2, y `ITEM_OFICIAL.md`, línea 53, que incluye esa frase en el enunciado verbatim.

**Cifra:**

| Mutante | `moodle` | `cableado` y `solucion` |
|---|---|---|
| «¿cuál diagrama de Venn NO organiza…?» | sobrevive | sobrevive |
| La canónica con otra pregunta | sobrevive | sobrevive |
| Plantilla con «cada estudiante toma un solo taller» | sobrevive | sobrevive |

Corrí las sondas sobre 8 versiones, 2 de ellas canónicas. El primero sería un defecto de CORRECCIÓN en verde: clave falsa y tres opciones correctas.

**Defecto vivo:** ninguno. La línea 193 es literal al impreso y leí las 7 plantillas.

**Alternativa, probada y sin tocar el `.Rmd`:**

```r
PREGUNTA_LIT <- "De acuerdo con lo anterior, ¿cuál diagrama de Venn organiza los datos de manera adecuada?"
# sondas_version
if (!identical(e$pregunta, PREGUNTA_LIT)) f <- c(f, "CABLEADO_PREGUNTA")
# sondas_moodle: pandoc parte líneas, así que hay que normalizar espacios
if (!grepl(PREGUNTA_LIT, gsub("\\s+", " ", qt), fixed = TRUE)) f <- c(f, "MOODLE_PREGUNTA")
```

Sobre el XML real da 10/10, y sobre el mutante 0/3. Para las plantillas, congelar un md5 del bloque `contextos`: cualquier cambio da rojo hasta revisarlo. Añadir los mutantes. El literal de I-2 en el `.Rmd` puede esperar a la próxima vez que se toque.

**Veredicto:** MODIFICAR el verificador.

### H18 — `moodle` no mira el tipo de pregunta (MEDIA)

**Qué se cuestiona:** la sección compara `fraction`, feedback e imágenes, pero no `<single>` ni los metadatos `extype` y `exshuffle`.

**Por qué:** I-4, punto 8: «Comprobar lo que llega al LMS». Regla #4: `exshuffle: FALSE` con opciones gráficas.

**Cifra:**
- `extype: mchoice` sobrevive a `moodle`, `cableado` y `solucion`. Con las opciones por defecto del verificador produce `<single>false</single>` y `fraction="-33.33333"` en los distractores: casillas múltiples y puntos negativos.
- `exshuffle: TRUE` también sobrevive. No altera la clave, pero incumple la regla #4.

**Otros dos puntos:**
- El verificador llama a la exportación con opciones por defecto, y el semillero usa `shuffle = TRUE`, numeración `ABCD` y `rule = "none"`. Comprobé 3 versiones con esas opciones: 3 `fraction="100"`, 9 `fraction="0"`, `single` verdadero y `style` intacto. Por tanto son equivalentes en lo que mide la sonda, salvo que el orden lo mezcla Moodle.
- I-4 punto 8 y la línea 16 del verificador dicen que el chunk `feedback` decide la nota. La nota la decide `exsolution`; el feedback solo llega al estudiante.

**Alternativa:**

```r
if (!grepl("<single>true</single>", x, fixed = TRUE)) f <- c(f, "MOODLE_TIPO")
# sec_cableado
if (sum(lineas == "extype: schoice") != 1L || sum(lineas == "exshuffle: FALSE") != 1L) rojo("cableado", "metadatos")
```

Añadir dos mutantes y corregir la redacción.

**Veredicto:** MODIFICAR el verificador.

### H19 — I-1 en los números: quedan cinco rasgos sin sonda en `figura` (MEDIA)

**Qué se cuestiona:** I-1 dice que nada fuera de los números distingue a una opción. `I1_TAMANO` solo mide el alto de la palabra, `I1_COLOR` solo mide croma, y la máscara de píxeles tapa el resto.

| Mutante solo en la clave | Píxeles distintos | `sondas_figura` | `COPIA_TIKZ` |
|---|---|---|---|
| Gris al 55 % | 5 242 | sobrevive | lo caza |
| Serie `b` en lugar de `bc` | 5 141 | sobrevive | lo caza |
| Estirado horizontal al 125 % | 5 211 | sobrevive | lo caza |
| Subrayado | 250 | sobrevive | lo caza |
| Movido 3 unidades a la derecha | 7 942 | sobrevive | lo caza |

La inclinada y la familia Times sí mueren en `figura`.

**Matiz que baja la severidad:** `COPIA_TIKZ`, en la sección `tinta`, caza los cinco si solo cambia el `.Rmd`. Si se cambian las dos copias, que es la forma legítima de tocar el dibujo, nada los ve. Además, M32 y M33 están declarados como mutantes de `figura`, y esa cobertura no se extiende a estos rasgos.

**Defecto vivo:** ninguno. La línea 66 es un único camino, sin rama para la clave.

**Alternativa, probada:** cinco comprobaciones añadidas tras `I1_COLOR`: una sola fuente con `pdffonts`, ancho por cifra, tinta casi negra, ninguna tinta bajo la línea base y `x0` contra el ancla. Control limpio en N = 30, y los cinco mueren. Código en `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/84d6d937-05ae-40ab-a8ba-f7a9dbf8e4e0/scratchpad/det_r3/copia/t_copia.R`.

**Veredicto:** MODIFICAR el verificador.

### H20 — `SOL_TEXTO` compara conjuntos, no la secuencia (BAJA)

**Qué se cuestiona:** `setdiff` comprueba que cada párrafo esperado exista, no dónde está. El lector de la Solution además se salta los bloques `{=latex}`.

**Por qué:** I-4 punto 7: un párrafo nuevo o reescrito da rojo. En un bloque `{=latex}` no lo da.

**Cifra, N = 40:** sobreviven los 7.

| Mutante | Efecto |
|---|---|
| Un párrafo movido a otra sección | sobrevive |
| La respuesta correcta puesta antes de la Estrategia | sobrevive |
| La reflexión bajo «Caso específico» | sobrevive |
| Los tres diagnósticos invertidos | sobrevive; «también quedó en el centro» pierde su referente |
| «Opcion A» dentro del bloque `{=latex}`, que llega solo al PDF | sobrevive |
| `otro_error` sin sortear | sobrevive |
| `reflexion` sin sortear | sobrevive |

Los dos últimos importan por I-3: lo único que varía la retroalimentación son esos dos sorteos, y nada los exige. La sección `solucion` imprime la distribución, pero no la comprueba.

**Alternativa, probada en los tres mutantes de párrafos con datos; control limpio en N = 60:**

```r
o <- match(esperado_solucion(e), L); h <- match(SOL_ENCABEZADOS, L)
if (anyNA(o) || is.unsorted(o) ||
    !identical(findInterval(o, h), c(1L, 2L, 2L, 3L, 3L, 3L, 5L, 6L, 6L, 6L, 6L))) f <- c(f, "SOL_ORDEN")
```

La reflexión no está en `esperado_solucion` y requiere una línea aparte. Además, exigir que las líneas dentro de `valla` sean el literal del `encabezado`, y que en N = 200 aparezcan los dos errores no ofrecibles y al menos dos reflexiones.

**Veredicto:** MODIFICAR el verificador.

### H21 — Sonda `papel` y secciones en paralelo (BAJA)

**Qué se cuestiona, en tres partes:**

1. **N = 8.** La regla #23 pide 100 en la familia B, o declarar la cifra y la razón. La razón impresa es que cada unidad es un PDF, pero la evidencia marca 5 s para 8 PDF. Yo corrí N = 100 con 0 fallas, y su resultado es igual. Subirlo costaría del orden de un minuto.
2. **Fallo abierto de `mclapply`.** Si un proceso hijo muere, su resultado es `NULL`, y `figura` y `moodle` lo cuentan como versión sin fallas. Probado matando un hijo: `mal: 0 · con falla: 0`. En mis corridas no ocurrió.
3. **Vigilancia mal atribuida.** I-5 dice que la sección `papel` vigila el ancho y el `!important`, pero esa sección solo mide páginas. El literal lo miran `cableado` (línea 163) y `SOL_IMAGEN` (línea 494), sin mutante propio, y nada comprueba en el XML que `!important` preceda a `width:5.5cm`.

**Límites de la sonda:** el límite de 310 mm es correcto, porque `bmargin=20mm`. No cubre `rep()`, que es un límite declarado, ni NOPS ni DOCX. Si `SemilleroUnico_v2.R` se usa tal cual, su PDF activo es `solpcielo_cloze.tex`, con la misma geometría, pero su portada trae la instrucción de CLOZE.

**Alternativa:**
- N = 100, con las versiones de texto más largo incluidas.
- `if (any(vapply(res, is.null, TRUE))) rojo(...)` en `figura`, `moodle` y `papel`.
- En `moodle`, contar el `style` literal, 5 por pregunta.
- Añadir un mutante sin `!important`.
- Atribuir bien la vigilancia en I-5.

**Veredicto:** MODIFICAR el verificador.

## Afirmaciones absolutas revisadas

| Afirmación | Resultado |
|---|---|
| I-5: una pregunta que empieza arriba deja las 4 opciones en una página | Cierta en N = 100 |
| I-5: sin `!important` gana `width:5.5cm` | Cierta: 207,9 px medidos |
| `.Rmd`, línea 355: pandoc descarta el bloque en HTML, Moodle y DOCX | Cierta, con 3 plantillas de DOCX en la prueba: dos dieron 0 fugas y `pcielo_nosol.tex` falló (ver No auditado). El webquiz da 0 fugas |
| I-4 punto 7: un párrafo nuevo da rojo | **Falsa** dentro de `{=latex}` (H20) |
| I-4 punto 8 y línea 16 del verificador: el chunk `feedback` decide la nota | **Imprecisa** (H18) |
| I-5: la sección `papel` vigila el ancho | **Parcial** (H21) |
| Nota de `renderizado`: 5 correctas, 15 incorrectas, 20 nombres neutrales, 25 `style`, 0 `VN-`, 0 `makeatletter` | Cierta en `moodle/venn.xml` |
| Nota de `renderizado`: DOCX de 5,5 cm, 0 `pandocbounded`, 0 pies Figure | Cierta |
| Nota de `renderizado`: NOPS de 5 preguntas, 4 figuras por página, 3 semillas | Cierta en `nops5/` |
| Nota de `arsenal`: 0 errores, 2 advertencias | Cierta; son «tabla de metadatos» y `letra_correcta` |

## Dominios y decisiones

| Dominio | Estado |
|---|---|
| Código R-exams | Sin defecto. Ancho y metadatos correctos; huecos de sonda en H18 |
| Pedagógico | Sin cambios; conflicto de I-3 pendiente |
| Visual | Página canónica y webquiz a 320 px revisados. Las letras quedan al pie izquierdo de cada figura, dentro de su altura |
| Gramática | H15 y H16 correctas; concordancia y tildes sin cambios |
| Coherencia matemática | Sin cambios; `espacio` en 0 |
| ICFES metacognitivo | Sin cambios; `solucion` y `moodle` en verde |
| Testing | H17 a H21 |
| Semántico | Las frases nuevas nombran lo que calculan; sin error |

**Decisiones del profesor:** ninguna esconde un defecto de CORRECCIÓN.
- La condición `twocolumn = TRUE, samepage = TRUE` vive solo en I-5. El semillero no la lleva. Se debe recordar al usarlo.
- I-3 frente a la regla #1 afecta a la variación, y H20 muestra que esa variación no tiene guarda.

## No auditado

- Moodle en servidor real, app móvil y Word.
- `$CFG->forceclean` en el servidor.
- `pcielo_nosol.tex`: con este ejercicio la exportación a DOCX falla con «argumento tiene longitud cero». No lo investigué, y ninguna línea de los semilleros lo usa.
- `validar_multisemilla.R`, paso 9 y paso 10: quedan con `paso9_10_r3.txt`.
- El margen §P7-B y la FASE 2C-bis.
- La comparación de la figura con los recortes: el diff no toca la figura.
- El mutante de pregunta contra el hook y el paso 10.

## Veredicto global

**APROBAR CON CAMBIOS** sobre el md5 `f886dfaa…`. Sin CORRECCIÓN ni objeciones críticas o altas, nada bloquea la FASE 3 por sí mismo. La aprobación sigue dependiendo de la firma de §P7 y de la decisión sobre I-3.

## Próximos pasos

1. Ampliar el verificador con H17, H18, H19, H20 y H21, sin tocar el `.Rmd`. Este reporte sigue cubriendo ese `.Rmd`, no el verificador nuevo.
2. Añadir como mutantes permanentes los que aquí sobrevivieron.
3. Corregir I-4 punto 7, I-4 punto 8 e I-5, y declarar el DOCX.
4. Si el `.Rmd` llega a tocarse, añadir el literal de la pregunta al `stopifnot` y lanzar otro detractor: este reporte no cubre otro md5.
5. Pedir la firma de §P7 con la columna exacta de H10, y mantener el exit 3 hasta entonces.
6. Correr la FASE 2C-bis antes de promover.

Scripts y salidas de esta ronda:
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/84d6d937-05ae-40ab-a8ba-f7a9dbf8e4e0/scratchpad/det_r3/copia/t_fig.R`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/84d6d937-05ae-40ab-a8ba-f7a9dbf8e4e0/scratchpad/det_r3/copia/t_copia.R`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/84d6d937-05ae-40ab-a8ba-f7a9dbf8e4e0/scratchpad/det_r3/copia/t_moodle.R`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/84d6d937-05ae-40ab-a8ba-f7a9dbf8e4e0/scratchpad/det_r3/copia/t_sol.R`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/84d6d937-05ae-40ab-a8ba-f7a9dbf8e4e0/scratchpad/det_r3/copia/t_rng.R`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/84d6d937-05ae-40ab-a8ba-f7a9dbf8e4e0/scratchpad/det_r3/copia/t_sem.R`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/84d6d937-05ae-40ab-a8ba-f7a9dbf8e4e0/scratchpad/det_r3/copia/t_nav.R`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/84d6d937-05ae-40ab-a8ba-f7a9dbf8e4e0/scratchpad/det_r3/copia/t_h15.R`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/84d6d937-05ae-40ab-a8ba-f7a9dbf8e4e0/scratchpad/det_r3/copia/t_fin.R`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/84d6d937-05ae-40ab-a8ba-f7a9dbf8e4e0/scratchpad/det_r3/todo_propio.txt`

Sources:
- [HTML Purifier, `CSS.AllowImportant`](https://raw.githubusercontent.com/ezyang/htmlpurifier/master/library/HTMLPurifier/ConfigSchema/schema/CSS.AllowImportant.txt)
- [Moodle MDL-60940, `$CFG->forceclean`](https://tracker-old.moodle.org/browse/MDL-60940)
- [Moodle MDL-61876, `forceclean` experimental](https://tracker-old.moodle.org/browse/MDL-61876)

CITAS_DETRACTOR
H9 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_schoice_v1.Rmd:344 | {width=5.5cm style="width:280px!important;max-width:100%"}
H9 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/.claude/CLAUDE.md:82 | Por eso cada figura mide 5,5 cm en papel y 280 px en pantalla
H9 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/salida/render_r3/moodle/venn.xml:99 | style="width:280px!important;max-width:100%;width:5.5cm"
H9 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/perifericos/solpcielo.tex:4 | papersize={216mm,330mm},tmargin=15mm,bmargin=20mm
H10 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/salida/verificar_espacio_r3.txt:273 | EST-menos-cifras-del-enunciado       formato        65.3 %  exceso +33.1 pp
H10 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:801 | while (length(m) < 100L) { s <- s + 1L; e <- correr(s); if (e$es_canonica) next
H12 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:518 | if (length(setdiff(esperado_solucion(e), L))
H13 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:603 | if ((fr == 100) != (q == "K")) f <- c(f, "MOODLE_FRACCION")
H14 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:245 | round(l$w$y1 - l$w$y0, 2L))))) != 1L) f <- c(f, "I1_TAMANO")
H14 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:253 | if (croma > 0) f <- c(f, "I1_COLOR")
H15 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_schoice_v1.Rmd:232 | Para llegar a la zona de solo ", t1, " no basta con quitarle al total la intersección con
H15 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_schoice_v1.Rmd:240 | Para llegar a la zona de solo ", t3, " no basta con quitarle al total las dos intersecciones: en
H16 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_schoice_v1.Rmd:400 | los tres totales, las tres intersecciones y el dato «pero no».
H17 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_schoice_v1.Rmd:193 | pregunta <- "De acuerdo con lo anterior, ¿cuál diagrama de Venn organiza los datos de manera adecuada?"
H17 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/.claude/CLAUDE.md:16 | La pregunta sigue siendo «¿cuál diagrama de Venn organiza los datos de manera adecuada?»
H17 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/.claude/CLAUDE.md:28 | Se implementa con su `stopifnot` contra literales.
H17 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/ITEM_OFICIAL.md:53 | De acuerdo con lo anterior, ¿cuál diagrama de Venn organiza los datos de manera adecuada?
H17 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_schoice_v1.Rmd | AUSENTE: "I-2: pregunta"
H17 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R | AUSENTE: identical(e$pregunta
H18 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_schoice_v1.Rmd:411 | extype: schoice
H18 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_schoice_v1.Rmd:413 | exshuffle: FALSE
H18 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R | AUSENTE: <single>true</single>
H18 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R | AUSENTE: exshuffle: FALSE
H18 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:581 | exams2moodle(copia, n = 1L, seed = semilla, name = "q", dir = td)
H18 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/perifericos/SemilleroMoodle_v2.R:109 | mchoice = list(shuffle = TRUE,
H18 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/.claude/CLAUDE.md:74 | `exsolution` y el chunk `feedback` deciden la nota y ninguna otra sección los mira.
H18 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:16 | Es lo que decide la nota: exsolution y el chunk feedback solo se ven aquí.
H19 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/.claude/CLAUDE.md:21 | Trazo, grosor, tamaño de letra y color son idénticos en las cuatro figuras de una versión
H19 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:339 | cuerpo(e2$dibujar_venn)))) f <- c(f, "COPIA_TIKZ")
H19 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_schoice_v1.Rmd:66 | tik <- c(tik, sprintf("\\node[anchor=base west] at (%s,%s) {\\num{%d}};"
H20 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:404 | if (startsWith(l, valla)) { en <- !en; next }
H20 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/.claude/CLAUDE.md:71 | reescrito da rojo hasta que tenga su comprobación
H20 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/.claude/CLAUDE.md:54 | 2 errores no ofrecibles, sorteados por versión
H21 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:561 | [papel] N = un PDF por contexto, más la canónica (regla #23, familia B: cada unidad es un PDF real)
H21 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:621 | mal <- unlist(lapply(res, function(x) if (inherits(x, "try-error")) "ERROR" else x))
H21 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/.claude/CLAUDE.md:84 | `papel` de `verificar_espacio.R`
H21 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:494 | {width=5.5cm style=\"width:280px!important;max-width:100%\"}&#8203;
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
