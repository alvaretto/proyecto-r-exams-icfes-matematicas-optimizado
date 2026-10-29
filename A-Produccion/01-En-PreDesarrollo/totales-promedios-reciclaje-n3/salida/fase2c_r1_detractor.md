<!-- Reporte íntegro de la FASE 2C, ronda 1 (AgenteDetractor a196468f93685d6f7, 2026-10-29), recuperado de su transcripción. Citas verificadas por el hook SubagentStop: 20, 0 fallos. -->

# Auditoría del detractor (FASE 2C): `totales-promedios-reciclaje-n3` (MAT-2026-07-050)

**Veredicto: RECHAZAR**, por un solo motivo de CORRECCIÓN (H1), que se arregla con una línea. La clave y los distractores están limpios: en 2000/2000 versiones hay exactamente una afirmación verdadera y es la marcada.

**Fecha:** 2026-10-29.

**Independencia:** no escribí ni corregí el `.Rmd` ni su verificador. Corro como subagente bloqueante, sin `name:`, así que este texto es el canal de entrega.

**Versión auditada:** `.Rmd` con md5 `20201631e1fddf5478d3b2de2fe83b89` y mtime 06:55:25, la misma que cita el arsenal en `ejercicio_state.json`.
- **El archivo cambió mientras yo auditaba.** Mi primera lectura fue de la versión anterior, sin `participaron_txt` y con las líneas corridas en −1.
- Todas las mediciones y las citas de abajo son de la versión vigente.
- Volví a correr `verificar_espacio.R` sobre ella: dio los mismos números y `APROBADO (0 fallos)`. La salida `verificar_espacio_pasada3.txt`, de las 06:50, sigue siendo representativa.

**Objeciones:** 7 (1 crítica de CORRECCIÓN, 2 altas, 2 medias y 2 bajas).

## Resumen

- **H1 (CORRECCIÓN, bloquea).** En el 2,84 % de las versiones no canónicas, la Solution afirma que la cifra de la razón sin sustento «no sale de ningún cálculo con la tabla». En esas versiones esa cifra es la resta de los promedios, la ventaja parcial de G o una suma o producto de datos de la tabla. A veces el mismo texto usa ese número unas líneas antes como resultado de un cálculo.
- **H2 (alta).** Hay una lectura apresurada de «su total fue de X kilos más que el de…» en el 17 % de las versiones: con ella, un distractor queda verdadero y la clave falsa.
- **H3 (alta, diagnosticidad).** Por construcción, que manda L-1, la razón sin sustento siempre nombra a la sede perdedora y la frase «todos sus cursos» siempre a la ganadora. Eso da un atajo de 50 % exacto (+25 pp sobre el azar), que la batería congelada no tiene con qué ver. Lo tiene que decidir el profesor.

## 1. Lo que verifiqué y salió limpio

Ejecuté el chunk `data_generation` real, y también `solucion` y `enunciado`, sobre copias en el scratchpad. Usé un evaluador de verdad propio que parte del **texto mostrado**, no de los campos del generador: sede nombrada, «más/menos cursos», «promedio menor/mayor», razón sin sustento por palabras clave y cifra citada contra *T*_S − *T*_O.

| Comprobación | Resultado |
|---|---|
| Robustez, semillas 1 a 2000 | 0 errores. Canónica en 203/2000 (10,2 %, L-5). Rama CURSOS en el 48,9 % (L-3) |
| Una sola verdadera y es la marcada (lectura gramatical) | 2000/2000. Cuotas de N = 100: CURSOS 100/100, PROMEDIO 100/100. Canónica 1/1 |
| Aritmética de la Solution y de la retroalimentación (todo `$a \times b = c$` y `$a - b = c$`) | 0 errores en 2000 versiones. «La cifra X kilos es el promedio de…»: 0 errores |
| Canónica contra `ITEM_OFICIAL.md`, comparando cadenas y no sumas de control | Enunciado, pregunta, las 4 opciones (con «la sede de secundaria» y «ganó porque») y las 2 filas de la tabla: idénticos |
| Regla #19 y su vecina | 0 letras ni «Opción X» en 2000 Solutions. El análisis recorre los distractores por papel, no por posición |
| Regla #25 | `validar_glifos_latex.R` da OK. El único no-ASCII fuera de las letras españolas es `§`, en comentarios de R |
| Ortografía | El corrector sale limpio, y repasé a mano el texto visible: las tildes están bien |
| Regla #11 | 8 plantillas con 8 tipos de estructura, sin «registró», protagonistas de 2 categorías |
| Regla #1 | 7 errores en el pool, `calcula()` sin azar, precondiciones y cifras comprobadas con `stopifnot` |
| `exextra` | Afirmación, Evidencia, D3.10 y Estándar aparecen literales en `evidencias-mat.json`, `niveles-mat.json` y `estandares-mat-ebc.json`. DOK 2 con Nivel 3 es compatible |
| L-1 a L-5 | Las guardas existen y se ejecutan; el mutante M1 de L-2 muere |

Lo que **no audité**: no rasterizé las páginas del PDF, del DOCX ni del NOPS. Que pdftotext muestre «¾cuál» y «armaciones» parece un artefacto de la extracción (codificación T1), no del render, pero no lo comprobé mirando la página.

## 2. Objeciones

### H1. CRÍTICA (CORRECCIÓN): la Solution afirma algo falso con los números de la versión

**Qué se cuestiona.** En toda versión no canónica, `explica()` de ERR-INVENTA (líneas 294-295) escribe «La cifra de *y* kilos tampoco sale de ningún cálculo con la tabla». Pero `usadas` (línea 400) solo excluye *D*, la cifra del distractor de G, la ventaja parcial de P y los cuatro datos de la tabla. Por eso *y* puede valer δ, la ventaja parcial de G, *a*_G + *a*_P, *n*·δ o *k*·*a*.

**Medición.**
- En 51 de 1797 versiones no canónicas (**2,84 %**), *y* es un cálculo en kilos con la tabla.
- En 22 (1,22 %), *y* es justo δ o la ventaja parcial de G, las cifras que el propio pool define como resultado de calcular (ERR-RESTA y ERR-PARCIAL).
- Por rama: 3,74 % en CURSOS (n = 776) y 2,15 % en PROMEDIO (n = 1021).
- Semillas reproducibles:
  - **109**: PROMEDIO, *a*_G = 38, *a*_P = 27, *y* = 11 = 38 − 27. La misma Solution dice «cada curso recogió, en promedio, 11 kilos más: 3 × 11 = 33 kilos» y luego «La cifra de 11 kilos tampoco sale de ningún cálculo con la tabla».
  - **211**: CURSOS, 12 = 56 − 44.
  - **137**: PROMEDIO, 65 = 5 × 13, la ventaja parcial de G.
  - **449**: CURSOS, 73 = 31 + 42.

**Por qué es CORRECCIÓN.** La regla #9 incluye «Solution falsa», y el encargo pide que la Solution no afirme nada falso con los números de la versión. Aquí la Solution se contradice dentro de su propio texto. No toca la clave.

**Alternativa.**
1. Cambiar la frase por una que la construcción garantiza (*y* ≠ *D*): `paste0(" La cifra de ", af$x, " kilos tampoco es la diferencia entre los totales, que es de ", p$D, " kilos.")`.
2. Para que el diagnóstico no engañe, añadir a `usadas` δ, la ventaja parcial de G, *a*_G + *a*_P, *n*_G·δ, *n*_P·δ, *k*·*a*_G y *k*·*a*_P, con su `stopifnot` y un mutante que lo revierta (L-2.5).

**Veredicto: MODIFICAR.**

### H2. ALTA: «su total fue de X kilos más que el de…» cuando X es el total del ganador

**Qué se cuestiona.** El cierre común de todas las opciones (línea 128), combinado con ERR-TOTAL (*x* = *T*_G, líneas 380-382).

**Por qué no lo clasifico como CORRECCIÓN.** La lectura gramatical, que es la única posible sin coma, es la diferencia: con ella el distractor es falso en 340/340 casos.

**Por qué igual importa.** Una lectura apresurada, «su total fue de 318 kilos, más que el de la sede secundaria», no produce dos claves sino una distinta: deja verdadero el distractor y falsa la clave. Y quien la hace puede haber calculado bien los dos totales, pero la Solution lo diagnosticaría como «confunde el total con la diferencia».

**Fuente (nivel 1).** Haladyna, Downing y Rodriguez (2002), *Applied Measurement in Education* 15(3), https://doi.org/10.1207/S15324818AME1503_5. Entre sus pautas de redacción: asegurar que solo una opción sea correcta y redactar las opciones con claridad (paráfrasis mía).

**Riesgo.**
- Afecta a 340 de 2000 versiones (**17,0 %**). Con N = 100: 23 % en CURSOS y 21 % en PROMEDIO.
- El caso más fuerte combina el molde N con ERR-TOTAL: «…después de juntar lo que recogieron todos sus cursos, su total fue de 318 kilos más que…», con 318 = *T*_G. Ahí la primera mitad de la frase es verdadera por sí sola. Ocurre en el 8,5 % de 2000 versiones (14 % y 11 % con N = 100).

**Alternativa.** Cualquiera de las dos exige volver a correr `verificar_espacio.R`.
- Cierre `paste0("su total superó en ", x, " kilos al de ", O, ".")`. «Superó en 318 kilos» no admite la lectura de total, y el cambio es igual para las cuatro opciones.
- O quitar ERR-TOTAL de `cand_g`. ERR-PARCIAL sigue dando siempre una cifra por encima de *D*, así que el canal 2 sigue siendo factible.

**Veredicto: MODIFICAR.**

### H3. ALTA (diagnosticidad: residuo de más de +8 pp en conflicto con L-1): la razón sin sustento siempre nombra a la perdedora

**Qué se cuestiona.**
- `psin` siempre nombra a P (línea 408).
- Del lado de G, un molde es de perfil y el otro neutro (N, «todos sus cursos»), y cuál de los dos es la clave se sortea con una moneda (línea 367).
- Así, la palabra «todos» aparece en una sola opción, siempre de G, y es la clave la mitad de las veces.
- La razón inventada siempre es de la sede perdedora.

**Por qué (fuente local de nivel 1).** Es el mismo mecanismo que el profesor rechazó al reescribir L-3: quien «ve varias versiones aprende que «la sede de mayor promedio nunca gana»». También es §P4-bis por analogía, y §P7-E y §P7-F de la regla #22. La pauta de Haladyna et al. (2002) contra parejas de opciones que delatan la correcta (paráfrasis mía) apunta a lo mismo.

**Medición** (N = 100 por rama, semillas 5001 en adelante; la canónica, exacta):

| Estrato | S_U: la sede de la razón sin sustento pierde; azar entre las dos opciones de la otra | S_todos: elegir la opción que dice «todos» (solo mira las opciones) | S_U + S1: además se descarta la cifra que está en la tabla |
|---|---|---|---|
| CURSOS | 50,0 % (+25,0 pp) | 44,0 % (+19,0 pp; +11,2 sobre el techo nulo de 32,8 %) | 69,0 % |
| PROMEDIO | 50,0 % (+25,0 pp) | 53,0 % (+28,0 pp; +20,4 sobre el techo de 32,6 %) | 51,5 % |
| Canónica | 50,0 % | 25,0 % (no dice «todos») | 100 % |
| Mezcla (semillas 1 a 100) | 50,0 % | 41,2 % | 61,0 % |

- Por construcción, S_U es 50 % exacto y S_todos es 50 % exacto en las no canónicas. Con 2000 semillas sale 48,5 % en CURSOS y 51,1 % en PROMEDIO.
- §P7-B no aplica: aquí no hay margen de largo; el atajo es la presencia de una palabra o el tipo de razón.
- Detectar la razón sin sustento descarta legítimamente una opción (33,3 %). El salto hasta 50 % sale de la regularidad entre versiones, así que el exceso atribuible al atajo es **+16,7 pp**.
- En la canónica, el 50 % es la estructura del impreso (H-2) y no se puede quitar. Pesa 0,10 × 25 = +2,5 pp.

**Alternativas** (las dos primeras requieren al profesor; regla #24 H-5):
- **(a)** Override firmado según §P7-E, con estas cifras.
- **(b)** Cambiar L-1 en las no canónicas. Con probabilidad 1/3, configuración espejo: G = {clave de perfil, razón sin sustento con cifra falsa} y P = {perfil, «todos» con cifra falsa}. El resto, como ahora, con la moneda del tipo de clave. Entonces la clave es, con probabilidad 1/3 cada uno, el perfil junto a la razón sin sustento, el perfil junto a «todos» o «todos», y nunca la razón sin sustento. Ninguna regla de tipo o de pareja pasa de 33,3 %, que es la eliminación legítima. Hay que comprobarlo enumerando el espacio de diseño (§P7-F) antes de adoptarlo.
- **(c)** Hay una variante que cumple la letra de L-1 sin firma: la clave siempre de perfil y el distractor de G con razón sin sustento. Pero cambia este atajo por otro, «quedarse con 'promedio'» al 50 %, salvo que las razones sin sustento también digan «promedio».

**Veredicto: residuo declarado con su cifra, para que decida el profesor.** Las tres pasadas de §P7-D ya se usaron, así que no obliga a una nueva pasada.

### H4. MEDIA (gramática visible): «participó 1 curso más, que aportan»

**Qué se cuestiona.** `participaron_txt` (línea 19) corrigió el verbo principal, pero no la oración de relativo, que sigue en plural en las líneas 223-224 (ERR-PARCIAL, rama PROMEDIO) y 255 (ERR-PROM). Ejemplo: «…en la sede primaria participó 1 curso más, que aportan $1 \times 53 = 53$ kilos.»

**Fuente.** RAE, *Diccionario panhispánico de dudas*, «concordancia» (https://www.rae.es/dpd/concordancia).

**Frecuencia.** 41,4 % de las no canónicas de CURSOS (todas las de *k* = 1) y 19,2 % de las de PROMEDIO.

**Alternativa.** `if (p$k == 1) " más, que aporta $" else " más, que aportan $"` en las dos líneas.

**Veredicto: MODIFICAR.**

### H5. MEDIA (testing): dos sondas sin guarda en el verificador

1. **S1, «descarta cifras de la tabla» (línea 131), se calcula pero ningún `ok()` la juzga.** Solo S7, el lugar de *D* y H1 tienen umbral. Su valor está justo en el borde:
   - en PROMEDIO, 33,4 % en la salida del verificador (+8,4 pp sobre el azar) y 32,3 % en mi muestra independiente de N = 100 (+7,3 pp);
   - en CURSOS, 28,2 % y 27,5 %.
2. **El residuo §P7-B de PROMEDIO (+10,4 pp) mete entre las reglas de largo a `molde atipico (n tokens)` (líneas 233 a 238).**
   - En la batería esa regla es de la familia «formato», no de largo.
   - Por sí sola marca 42,0 %, es decir +9,4 pp sobre el techo nulo.
   - El verificador solo exige el margen en caracteres de la opción más larga. El margen de esta regla se mediría en palabras, y no está medido: el tramo variable de los moldes difiere hasta en 4 palabras.

Que una sonda no tenga umbral la deja en documentación, no en guarda.

**Alternativa.**
- Poner un `ok()` con umbral de +8 pp a S1 a S6, por rama.
- Medir el margen de `molde atipico` en palabras, o sacarla de `LARGO`.
- Añadir las sondas S_U y S_todos de H3.

### H6. BAJA: `calcula()` sin `stop()` fuera de contexto

ERR-RESTA (línea 200), ERR-PARCIAL, ERR-TOTAL y ERR-INVENTA devuelven su cifra pase lo que pase. La regla `codigo-rmd` #8 pide que `calcula()` se detenga fuera de contexto. El riesgo es bajo, porque la selección filtra antes y hay un `stopifnot` después.

**Alternativa.** Añadir una guarda, por ejemplo `if (p$delta == p$D) stop("calcula(ERR-RESTA): fuera de contexto")`.

### H7. BAJA (visual): encabezados numerados en el PDF

Los `### …` de la Solution salen numerados en el PDF: «0.0.1 Respuesta correcta», «0.0.2 Procedimiento correcto».

**Alternativa.** Usar párrafos en negrita (`**Respuesta correcta**`).

## 3. Veredicto global y próximos pasos

**RECHAZAR**, por H1 (CORRECCIÓN, que es binaria). Nada más bloquea por corrección: la clave, los distractores, la aritmética, la canónica y la regla #19 están limpios.

Pasos, en orden:
1. **Corregir H1** con el cambio de frase en las líneas 294-295 y ampliar `usadas` con su guarda y su mutante.
2. **H2**: cambiar el cierre o quitar ERR-TOTAL.
3. **H4**: dos líneas.
4. **H3 y la alternativa (b) de H2, al profesor**: override firmado según §P7-E o cambio de L-1.
5. **H5** en el verificador.
6. Volver a correr `verificar_espacio.R`, los 4 formatos y la FASE 2C con un detractor nuevo sobre la versión corregida.

Scripts para reproducir las mediciones, en el scratchpad:
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/35e93ef1-04b2-4e1e-8b49-a10ac14fa449/scratchpad/medir.R`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/35e93ef1-04b2-4e1e-8b49-a10ac14fa449/scratchpad/medir2.R`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/35e93ef1-04b2-4e1e-8b49-a10ac14fa449/scratchpad/semillas.R`

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:294-295 | kilos tampoco sale de ningún cálculo con
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:400 | usadas <- c(d$D, fila_g$x, d$parcialP, d$nG, d$nP, d$aG, d$aP)
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:271 | " cada curso recogió, en promedio, ", p$delta, " kilos más: $"
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:128 | cierre <- function(x, O) paste0("su total fue de ", x, " kilos más que el de ", O, ".")
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:380-382 | fuente = c("aG", "aP", "delta", "parcialG", "TG"),
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:408 | psin  = afirmacion(molde_psin, "P", y, "ERR-INVENTA"))
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:367 | tipo_clave <- pick(c("perfil", "neutro")); tipo_gdis <- if (tipo_clave == "perfil") "neutro" else "perfil"
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:140-141 | después de juntar lo que recogieron todos sus cursos
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/.claude/CLAUDE.md:31-32 | dos se lo dan a P: una con la única ventaja que P sí tiene en la tabla
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/.claude/CLAUDE.md:112 | ve varias versiones aprende que «la sede de mayor promedio nunca gana»
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:19 | participaron_txt <- function(x) if (x == 1) "participó 1 curso" else paste("participaron", x, "cursos")
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:223-224 | " más, que aportan $", p$k, " \\times ", p$aP
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:255 | participaron_txt(p$k), " más, que aportan $"
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/verificar_espacio.R:131 | "S1 descarta cifras de la tabla" = m(function(x)
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/verificar_espacio.R:233 | LARGO <- c("la mas larga (chars)", "la mas corta (chars)", "molde atipico (n tokens)",
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/verificar_espacio.R:237 | marg <- vapply(V, function(x) { L <- nchar(x$opciones); k <- x$clave
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/scripts/bateria_referencia_icfes.R:79 | nueva_regla("molde atipico (n tokens)","formato"
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/salida/verificar_espacio_pasada3.txt:74 | molde atipico (n tokens) 42.0%
H6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:199-200 | calcula = function(p) p$delta,
H7 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:505-515 | ### Respuesta correcta
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: RECHAZAR
