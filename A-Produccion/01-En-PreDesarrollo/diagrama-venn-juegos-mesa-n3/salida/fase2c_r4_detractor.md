# Auditoría Detractor: `diagrama-venn-juegos-mesa-n3` (FASE 2C, ronda 4)

**Fecha:** 2026-11-06
**Directorio:** `/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3`
**Auditado:** override de `verificar_espacio.R` (md5 `3d58f45501aa004b35acd709b3b6c58a`), sondas H17 a H21, archivos de uso y afirmaciones nuevas. El `.Rmd` (md5 `f886dfaa45b80a5ee16f9820908049b5`) y la batería quedaron fuera de alcance. Los dos md5 coinciden al empezar y al terminar, y no edité nada del subproyecto.
**Objeciones activas:** 10 (0 de CORRECCIÓN del ítem, 0 críticas, 0 altas, 2 medias, 8 bajas).

## Resumen ejecutivo

**El `.Rmd` no necesita cambios, así que la firma sigue vigente.** Ningún hallazgo de esta ronda lo toca.

- **Override:** no hay camino a exit 0. Exit 4 no se alcanza con una regla fuera de lista, una cifra sobre lo firmado (más allá de la tolerancia de 0,001 pp), otro `.Rmd` ni un modo parcial. Lo comprobé en 20 escenarios de punta a punta y 2 modos. Quedan cuatro huecos de cableado de severidad baja (H25).
- **H24, una cifra publicada:** la columna «Exceso exacto» resta un techo de N = 100 a una tasa exacta. Con un techo del mismo tamaño (25,03 %), `LEX-con-repetidos` da +8,39 pp, sobre el corte y fuera de la lista.
- **H22, defecto principal de uso:** el normalizador pasa el escaneo a luminancia. Las marcas naranja, verde claro o amarillas desaparecen: la hoja sale `OK` y `nops_scan()` las lee en blanco, sin aviso.
- **Manchas, escaneo oscuro y giros:** 0 lecturas equivocadas en 95 casos.
- **H17 a H21:** hacen lo que dicen y sus mutantes mueren por su sonda. Quedan dos residuos (H26 y H30).

## Respuesta a los cuatro puntos

| # | Punto | Respuesta |
|---|---|---|
| 1 | Override §P7-E | Resiste los cuatro casos preguntados, sin caminos a exit 0. Objeciones en H24 y H25 |
| 2 | Sondas H17 a H21 | Cumplen, con residuos H26 y H30 |
| 3 | Archivos de uso | El semillero, las dos plantillas y `pdfjam` cumplen. El normalizador tiene H22 y H23 |
| 4 | Afirmaciones nuevas | Verificadas salvo H27, H28 y H29. Lo de Moodle y DOCX queda sin auditar |

## Qué ejecuté

| Prueba | Alcance | Resultado |
|---|---|---|
| `verificar_espacio.R todo`, real, en copia | Todas las secciones | Exit 4, 99 mutantes cazados, 0 no detectados. Veredicto por mutante igual a `salida/verificar_espacio_firma.txt` |
| Escenarios del override | 20, más los modos `bateria` y `xyz` | Tabla siguiente |
| Reglas de R contra `exacto.py` | Las 100 versiones de la batería, 30 reglas | Diferencia máxima 4,4e-16. Las 4 POS solo existen en R |
| Techo nulo exacto | 11 000 000 de versiones, 3 permutaciones de la clave | 25,03 / 25,03 / 25,02 % |
| Normalizador con `nops_scan()` | 111 escaneos: 95 con manchas, trazos, giros u oscuridad, y 16 con tinta de color | H22 y H23 |
| Hoja real del semillero | A4 y oficio directas; oficio, esquina y 216 × 356 mm normalizadas | 5 de 5 con el id `26110600001` y sin error |
| `--pdf` con el PDF de oficio | 5 colocaciones | 5 de 5 leídas bien |
| Marcas finas con escala 1,011 | 20 | 0 diferencias con la lectura directa; todas salieron marcadas, así que no probé el umbral exacto |

Un comando de montaje del arnés fue rechazado por el sistema de permisos, sin motivo visible. Lo rehíce con copias dentro del scratchpad, sin enlaces simbólicos.

## Punto 1: override de punta a punta

El arnés usa el script real con `modo = "todo"` y quita solo las ocho llamadas a secciones. La ejecución de `exacto.py`, `sec_bateria()` y `decidir_exit()` son las reales. `exacto.py` es el real en el control y en el caso del `python3` que falla; en los demás lo sustituye un script que imprime `exacto.txt` con la alteración indicada.

| Escenario | Exit | Razón |
|---|---|---|
| Control con `exacto.py` real | 4 | Cubierto |
| `python3` falla, sin numpy, solo cabecera, falta una regla firmada o no firmada, `nan` | 3 | `FIRMA_SIN_EVALUAR` |
| `50.0.0` en una tasa | 1 | Error de R |
| Regla firmada en 69,555 | 3 | `FIRMA_SOBRE_CIFRA` |
| Regla firmada en 69,554 | 4 | Dentro de `TOL_FIRMA`: 110 versiones de 11 000 000 |
| `LEX-con-repetidos` en 40,300 %, regla nueva al 90 % | 3 | `FIRMA_FUERA_DE_LISTA` |
| `LEX-con-repetidos` en 40,100 % (+7,9 pp) | 4 | Bajo el corte, correcto |
| `.Rmd` con una línea más | 3 | `FIRMA_MD5` |
| `FIRMADA <- FALSE` | 3 | `FIRMA_SIN_FIRMAR` |
| Modo `bateria` / modo `xyz` | 3 | `FIRMA_MODO` |
| `LEX-sin-repetidos` cambiada en R | 3 | Por azar: salió fuera de lista |
| `exacto.py` convertido en un volcado de `exacto.txt` | **4** | H25 |
| `DIV-suma-mult5` retirada de `reglas_p7()` | **4** | H25 |
| Python sale con 1 tras imprimir todo | **4** | H25 |
| Regla repetida, con la cifra firmada primero y 80,000 después | **4** | H25 |

- **Exit 0:** `decidir_exit()` solo devuelve 1, 3 o 4, y la línea 978 es el único `quit`. Ninguna corrida dio 0.
- **`exacto.txt`:** el verificador no lo lee; ejecuta `exacto.py` en cada corrida.
- **Redondeo:** tres decimales con tolerancia 0,001 pp. No abre ningún hueco útil.
- **Regla por regla:** las dos listas van por nombre, sin tope por familia ni por estrato.

## H17 a H21

| ID | Sondas | Comprobación | Estado |
|---|---|---|---|
| H17 | `CABLEADO_PREGUNTA`, `MOODLE_PREGUNTA`, `CONTEXTOS_CONGELADOS` | M34, L6, M35. Las viñetas las protege el `stopifnot` de I-2, porque el código es común | Cumple, residuo H30 |
| H18 | `MOODLE_TIPO`, `CABLEADO_METADATOS` | L7, M36. XML del semillero: `single` verdadero, 1 `fraction="100"`, 3 `fraction="0"`, 5 `style` | Cumple |
| H19 | `I1_FUENTE`, `I1_ANCHO`, `I1_GRIS`, `I1_MARCA`, `I1_POS` | M37 a M41 | Cumple salvo la posición vertical (H26) |
| H20 | `SOL_ORDEN`, `SOL_LATEX`, `SOL_SIN_VARIACION` | S19 a S23 | Cumple |
| H21 | `HIJO_MUERTO`, N = 100 en `papel`, `MOODLE_ESTILO` | M42 y L8. Proceso matado de verdad: 4 `NULL` y `HIJO_MUERTO` | Cumple |

## Objeciones medias

### H22 — El normalizador borra marcas de color (MEDIA, uso)

No es CORRECCIÓN del ítem: la clave y la Solution no cambian.

**Qué se cuestiona:** `Image.open(ruta).convert("L")` en la línea 60, y la frase «No altera las marcas» de la línea 8.

**Por qué:** el código de `exams:::trim_nops_scan()` (exams 2.4-5, Nivel 1) reduce un PNG a color con `pmin(x[, , 1L], x[, , 2L], x[, , 3L])` y corta en 0,7. El normalizador usa luminancia, así que estas tintas pasan el corte de 178 en una vía y no en la otra.

| Tinta | Luminancia | Mínimo de canales |
|---|---|---|
| Naranja | 180 | 40 |
| Verde claro | 189 | 130 |
| Amarilla | 217 | 60 |

**Medido, con colocación en esquina y escaneo en color:**

| Tinta de las X | A4 directo | Todas las X, normalizada | Solo la pregunta 2, normalizada |
|---|---|---|---|
| Negra, azul, roja | `01000 00010 10000` | Igual | Igual (azul y roja) |
| Naranja, verde claro, amarilla | `01000 00010 10000` | `00000 00000 00000` | `01000 00000 10000` |

El normalizador imprime `OK … escala 1.000` y `nops_scan()` no da error ni aviso.

**Lo que limita el daño:**
- `nops_control_calidad.R` lo marca como `sin_marcar` (exit 1 con `--preguntas 3`), pero una respuesta dejada en blanco da lo mismo.
- La hoja pide «bolígrafo azul o negro».
- Si el escáner digitaliza en gris, la marca se pierde antes de llegar al normalizador.

**Alternativa, probada:** 10 de 10 lecturas correctas.

```python
im = Image.open(ruta)
if im.mode in ("RGB", "RGBA", "P"):
    r, g, bl = im.convert("RGB").split()
    im = ImageChops.darker(ImageChops.darker(r, g), bl)   # pmin, como trim_nops_scan()
else:
    im = im.convert("L")
```

Además, conviene sustituir «No altera las marcas» por lo que hace: recorta y remuestrea.

**Veredicto:** MODIFICAR el normalizador antes de la prueba con papel.

### H24 — «Exceso exacto» mezcla una tasa exacta con un techo de N = 100 (DIAGNOSTICIDAD, MEDIA)

No reabro la firma ni propongo el filtro. Es una cifra que el profesor no vio.

**Qué se cuestiona:** `tv` sale de la muestra de N = 100 (línea 954) y decide `sobre8` (línea 878) contra tasas exactas. La columna del HANDOFF y la glosa de `salida/tabla_firma_p7.md` lo presentan como exceso exacto y como el techo que se alcanza por azar.

**Por qué:** el techo corrige la inflación por selección de una muestra finita, y `bateria_eliminacion.R` lo calcula sobre esa muestra. Una tasa exacta no tiene ese ruido. §P7-E pide listar las reglas sobre +8 pp con su cifra exacta.

| Regla | Tasa exacta | Sobre 32,2 % | Sobre 25,03 % |
|---|---|---|---|
| `EST-menos-cifras-del-enunciado` | 69,553 % | +37,4 pp | +44,5 pp |
| `LEX-con-repetidos` | 33,418 % | +1,2 pp | **+8,39 pp** |
| `DIV-suma-par`, `DIV-suma-impar` | 29,545 % | −2,7 pp | +4,5 pp |

**Matices:**
- La vara de §P7-A viene de un corpus finito de 426 ítems, y no comprobé con qué techo se midió. Por eso no afirmo que `LEX-con-repetidos` obligue. Afirmo que +1,2 pp no es su exceso exacto y que «0 fuera de la lista» depende del tamaño del techo.
- Las 8 reglas firmadas no cambian de lado: todas suben 7,2 pp.
- En la canónica, `SIG-tiene-cero` daría +8,3 pp, pero esa instancia es el impreso (H-2).

**Alternativa, sin tocar el `.Rmd`:**
1. Renombrar la columna a «Exceso sobre el techo de N = 100».
2. Añadir esta tabla a «Medido después de la firma» en `salida/tabla_firma_p7.md`.
3. Preguntar al profesor si amplía la firma a `LEX-con-repetidos` con 33,418 %, o si deja escrito que el corte se mide contra el techo de N = 100. Es decisión suya (H-5).

**Veredicto:** MODIFICAR la declaración y MANTENER el código hasta su respuesta.

## Objeciones bajas

### H23 — «OK» con un PNG inservible (BAJA, uso)

En 6 de 6 escaneos con fondo de 175 o menos sale `OK escala 1.208`, exit 0 y un PNG inútil. Después `nops_scan()` da ERROR, así que no es silencioso.

| Resultado en 95 casos sin color | Casos |
|---|---|
| Leídos bien | 19 |
| `OK`, y `nops_scan()` da ERROR | 27 |
| `FALLA` del normalizador | 49 |
| **Lectura equivocada** | **0** |

Incluyen manchas de 1 mm a 3-15 mm de la caja, bandas oscuras, giros de ±0,5° a ±2,8°, hoja boca abajo, hoja cortada, y trazos junto a las cruces y en la zona libre arriba a la izquierda. De los 22 `FALLA` con trazos, 15 son hojas que `nops_scan()` lee bien en A4.

**Alternativa, probada:** `if sum(filas) > 0.35 * ancho * alto: raise ValueError(...)` en `caja_de_tinta`. Da 6 de 6 `FALLA` en los oscuros, y 3 de 3 `OK` en hojas normales. Además, decir en la ayuda qué hacer ante un `FALLA`: recortar a mano o reescanear. **Veredicto:** MODIFICAR.

### H25 — Cuatro caminos a exit 4 que las pruebas puras no ven (BAJA, testing)

| Camino | Medido |
|---|---|
| `exacto.py` sin md5 | Un volcado de `exacto.txt` da exit 4 sin enumerar nada; una regla repetida con cifra alta, también |
| Batería de R sin congelar (§P7-C) | Retirar una regla no firmada da exit 4 |
| `system2` oculta el estado de Python | Exit 4 con Python terminando en 1 |
| `decidir_exit()` no sabe qué secciones corrieron | Mi arnés da exit 4 con 0 de 8 secciones ejecutadas |

Ninguno se alcanza con argumentos: exige editar los archivos. No hay defecto vivo, porque R y Python coinciden en las 30 reglas.

**Alternativa, probada en los tres primeros:** control en 4; volcado, regla retirada y Python fallido pasan a 3.

```r
MD5_EXACTO <- "1d5f279462e10c2b5a8fa4e8954fdc6c"
ruta <- file.path(aqui, "salida", "firma_p7", "exacto.py")
if (!identical(unname(tools::md5sum(ruta)), MD5_EXACTO)) return(numeric(0))
x <- tryCatch(suppressWarnings(system2("python3", c("-I", shQuote(ruta)), stdout = TRUE)), error = function(e) character(0))
if (!is.null(attr(x, "status"))) return(numeric(0))
## en sec_bateria
ev <- length(nom) == 34L && !length(setdiff(nom, names(med_var))) && !length(setdiff(names(med_var), nom)) && !length(setdiff(nom, names(med_can)))
```

Para las secciones, sin probar: registrar cada sección en `corridas` y exigir el conjunto completo en `decidir_exit()`. **Veredicto:** MODIFICAR el verificador.

### H26 — `I1_POS` solo mide la horizontal (BAJA, testing)

I-4 punto 6 promete posición. Moviendo los números solo en la clave, −3 y −1,5 unidades hacia arriba sobreviven hasta la semilla 14; +1,5 y +3 mueren por `I1_MARCA`. `COPIA_TIKZ` los caza si solo cambia el `.Rmd`, no si cambian las dos copias. No hay defecto vivo, porque la línea de dibujo es un único camino.

**Alternativa, probada:** `if (abs((z$y0 + z$y1) / 2 - ay[r] - 2.22) > 0.8) f <- c(f, "I1_POSY")`. Los dos mutantes mueren, el control con N = 30 no da fallas, y la diferencia medida es 2,217-2,218. **Veredicto:** MODIFICAR.

### H27 — numpy no está en el sitio de usuario (BAJA, afirmación falsa)

`python3 -I` importa numpy de `/usr/lib/python3.14/site-packages` y reproduce las 30 tasas de `exacto.txt`. El sitio de usuario no tiene numpy.

**Alternativa:** invocar con `-I`, ya incluido en H25, y corregir la línea 868 y el HANDOFF. **Veredicto:** MODIFICAR.

### H28 — El 0,0 % de margen es propiedad de la escala (BAJA, afirmación)

`lit` es un entero de 0 a 7, así que el margen mínimo posible es 1/7 = 14,3 %. Solo 699 de 9 452 819 versiones bajan de 15 %. El dato casi no puede dar otra cosa. La conclusión es correcta porque §P7-B no exime lo contable, no por esa cifra.

**Alternativa:** añadir esa frase bajo la tabla de márgenes. **Veredicto:** MANTENER, con la aclaración.

### H29 — Evidencia que el commit perdería (BAJA, trazabilidad)

`margenes.py` y `margenes.txt` están ignorados por `**/salida/`, la tabla los cita y el HANDOFF no los lista para `git add -f`. La simulación de escaneo solo vive en el scratchpad de esta sesión.

**Alternativa:** añadirlos a la lista y copiar `a.R`, `leer.R` y la tabla de lecturas a `salida/escaneo_simulado/`. **Veredicto:** MODIFICAR.

### H30 — `MOODLE_PREGUNTA` comprueba contención (BAJA, testing)

Añadí `cat("Marque el diagrama que NO los organiza.\n\n", sep = "")` tras la pregunta: sobrevive a `sondas_moodle()` y el XML trae la frase. Hoy da exit 3 por `RMD_MD5`; importa en el próximo `.Rmd`.

**Alternativa:** comparar la cola tras `</ul>` con `<p>PREGUNTA</p> </p>`. La del XML real coincide con esa cadena (salida leída) y la del mutante no. **Veredicto:** MODIFICAR.

### H31 — Condiciones de uso sin guarda (BAJA, testing)

`solpcielo_una_por_pagina` y `samepage` tienen 0 apariciones en el verificador. El normalizador tiene 0 tests, mientras `nops_control_calidad.R` tiene `tests/testthat/test_nops_control_calidad.R`. Si se quita `samepage` del semillero, nada se pone en rojo.

**Alternativa:** una sección que genere PDF y NOPS de 5 preguntas y exija 4 figuras por página, y una suite del normalizador con los casos de H22 y H23. **Veredicto:** MODIFICAR.

## Archivos de uso que cumplen

- **`solpcielo_una_por_pagina.tex`:** el diff contra `perifericos/solpcielo.tex` es el comentario y `\clearpage`.
- **`pcielo.tex`:** idéntico al de `perifericos/`.
- **`SemilleroUnico_v2.R`:** sus salidas cuadran con el HANDOFF.
  - PDF: p2:4 p3:1 p5:4 p6:1 p8:4 p9:1 p11:4 p12:1 p14:4 p15:1.
  - NOPS: 4 figuras en las páginas 3 a 7; revisé la página 3 a ojo.
- **`pdfjam`:** no altera lo que `nops_scan()` necesita.
  - 7 páginas, 216 × 330 mm, mismas figuras por página y mismo número de fuentes.
  - Caja de tinta de 2016 × 3117 px en A4 y 2015 × 3117 px en oficio, desplazada 36 y 195 px: no hay escala.

## Afirmaciones revisadas

| Afirmación | Resultado |
|---|---|
| Exit 4, 99 mutantes muertos por su sonda | Cierta, repetida |
| Exactas contrastadas con el código real | Cierta en las 30 reglas, con N = 100 |
| Orden A < clave < B < D algebraico | Cierta: K − A = z; B − K = 2a + 2c + 2d − z; D − B = b + z |
| `duplex = FALSE` da 6 páginas | Cierta |
| Multicopia de 3: p2:4 p3:1 p4:3 p5:2 p6:2 p7:3 | Cierta |
| `pcielo_nosol.tex` falla también con `swisscapital.Rmd`, y `pcielo.tex` no | Cierta |
| Directo: A4, centrada y ajustada leídas; esquina y 216 × 356 con error | Cierta, 6 lecturas |
| Normalizador: 8 colocaciones leídas | Cierta para esas colocaciones; no cubre tinta de color (H22) |
| `margenes.txt` | Reproducido línea por línea (H28) |
| «Exceso exacto» | Imprecisa (H24) |
| numpy del sitio de usuario | Falsa (H27) |
| «No altera las marcas» | Falsa con tinta de color (H22) |

## No auditado

- Moodle: `forceclean`, `format_text` y el resultado del purificador. No tengo acceso al servidor.
- DOCX con `--reference-doc`.
- «Figura contra el recorte»: mi medida simple del marco depende del umbral y de la resolución y no reproduce las cifras del HANDOFF, así que no las confirmo ni las objeto.
- Papel, impresora y escáner reales.
- Marcas justo en el umbral de `nops_scan()`.
- La firma en sí: la tomo como dato del encargo, transmitida por el coordinador.
- FASE 2C-bis.

## Veredicto global

**APROBAR CON CAMBIOS.** No hay defecto de CORRECCIÓN del ítem ni objeciones críticas o altas. No pido ningún cambio al `.Rmd`, así que la firma no se anula. Antes de leer un lote NOPS real debe corregirse H22.

## Próximos pasos

1. Corregir H22 y H23 en el normalizador y añadir su suite (H31).
2. Informar al profesor de H24 y registrar su decisión.
3. Aplicar H25, H26, H27 y H30 en `verificar_espacio.R`, actualizar su md5 en el HANDOFF y regenerar la evidencia. Este reporte no cubre esa versión.
4. Resolver H28 y H29 en la documentación.
5. Reintentar la FASE 2C-bis antes de promover.

**Scripts y salidas:**
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/84d6d937-05ae-40ab-a8ba-f7a9dbf8e4e0/scratchpad/det_r4/armar_e2e.py`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/84d6d937-05ae-40ab-a8ba-f7a9dbf8e4e0/scratchpad/det_r4/log/e2e_res.txt`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/84d6d937-05ae-40ab-a8ba-f7a9dbf8e4e0/scratchpad/det_r4/log/real_todo.out`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/84d6d937-05ae-40ab-a8ba-f7a9dbf8e4e0/scratchpad/det_r4/ex/techo.py`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/84d6d937-05ae-40ab-a8ba-f7a9dbf8e4e0/scratchpad/det_r4/ex/cruce.py`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/84d6d937-05ae-40ab-a8ba-f7a9dbf8e4e0/scratchpad/det_r4/esc/gen.py`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/84d6d937-05ae-40ab-a8ba-f7a9dbf8e4e0/scratchpad/det_r4/esc2/gen.py`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/84d6d937-05ae-40ab-a8ba-f7a9dbf8e4e0/scratchpad/det_r4/esc3/norm_fix2.py`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/84d6d937-05ae-40ab-a8ba-f7a9dbf8e4e0/scratchpad/det_r4/parche.py`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/84d6d937-05ae-40ab-a8ba-f7a9dbf8e4e0/scratchpad/det_r4/repo/A-Produccion/01-En-PreDesarrollo/_base/h_y.R`

CITAS_DETRACTOR
H17 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:163 | if (!identical(e$pregunta, PREGUNTA_LIT)) f <- c(f, "CABLEADO_PREGUNTA")
H18 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:648 | if (!grepl("<single>true</single>", x, fixed = TRUE)) f <- c(f, "MOODLE_TIPO")
H19 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:275 | if (length(fu) != 1L) f <- c(f, "I1_FUENTE")
H20 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:560 | if (!all(attr(L, "latex") == SOL_LATEX)) f <- c(f, "SOL_LATEX")
H21 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:149 | sonda_hijos <- function(res) if (any(vapply(res, is.null, TRUE))) "HIJO_MUERTO" else character(0)
H22 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/Auxiliares/exams2nops-escaner/normalizar_escaneos_nops.py:60 | im = Image.open(ruta).convert("L")
H22 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/Auxiliares/exams2nops-escaner/normalizar_escaneos_nops.py:8 | recorta un A4 equivalente. No altera las marcas.
H23 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/Auxiliares/exams2nops-escaner/normalizar_escaneos_nops.py:88 | print(f"OK    {nombre}  escala {k:.3f}")
H23 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/Auxiliares/exams2nops-escaner/normalizar_escaneos_nops.py:14 | un escaneo con manchas en el borde puede recortarse mal.
H23 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/Auxiliares/exams2nops-escaner/normalizar_escaneos_nops.py | AUSENTE: demasiado oscuro
H24 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:954 | tv <- 100 * r$techo_nulo
H24 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:878 | sobre8 <- names(medido)[medido - techo > corte]
H24 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/HANDOFF.md:15 | Exacta (11 000 000)
H24 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/salida/tabla_firma_p7.md:11 | diferencia con el techo que se alcanza por azar: 32,2 % en las variantes
H24 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/salida/tabla_firma_p7.md:27 | con 33,42 % (+1,2 pp), y no entra en la firma.
H24 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/bateria_eliminacion.R:278 | techo_nulo <- mean(maximos_nulos)
H24 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/rules/diversidad-sustantiva.md:20 | al pedir la firma se listan todas las reglas que pasan de +8 pp
H25 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:870 | suppressWarnings(system2("python3", shQuote(file.path(aqui, "salida", "firma_p7", "exacto.py"))
H25 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:953 | ev <- !length(setdiff(nom, names(med_var)))
H25 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:965 | if (modo %in% c("todo", "espacio")) sec_espacio()
H25 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R | AUSENTE: MD5_EXACTO
H25 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R | AUSENTE: attr(x, "status")
H25 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/rules/diversidad-sustantiva.md:18 | La batería se congela al inicio
H26 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:279 | if (abs(z$x0 - ax[r] + g$lsb_numero) > 0.8) f <- c(f, "I1_POS")
H26 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/.claude/CLAUDE.md:69 | posición y que no haya marcas bajo la línea base.
H26 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R | AUSENTE: I1_POSY
H27 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:868 | necesita numpy del sitio de usuario
H27 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/HANDOFF.md:53 | porque necesita numpy del sitio de usuario
H28 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/salida/firma_p7/margenes.py:17 | par = np.abs(m[:, 1:] - m[:, [0]]) / np.maximum(np.maximum(m[:, 1:], m[:, [0]]), 1.0)
H28 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/salida/firma_p7/margenes.txt:1 | margen < 15 % en 0.0 %
H28 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/salida/tabla_firma_p7.md:71 | 0,0 %, en 9 452 819 versiones
H28 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/docs/reglas/diversidad-sustantiva.md:454 | que exime lo imperceptible, no lo contable
H29 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/salida/tabla_firma_p7.md:67 | Script y salida: `salida/firma_p7/margenes.py`
H29 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/HANDOFF.md:72 | `salida/firma_p7/filtro.py`, `salida/firma_p7/filtro.txt`
H29 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/HANDOFF.md:39 | simulado el 2026-11-06 con hoja marcada a mano en imagen
H29 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/HANDOFF.md | AUSENTE: margenes
H30 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R:649 | if (!grepl(PREGUNTA_LIT, gsub("\\s+", " ", qt), fixed = TRUE)) f <- c(f, "MOODLE_PREGUNTA")
H31 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/.claude/CLAUDE.md:89 | Condiciones de uso, en `SemilleroUnico_v2.R`
H31 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R | AUSENTE: solpcielo_una_por_pagina
H31 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/verificar_espacio.R | AUSENTE: samepage
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS