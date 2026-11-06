# Auditoría Detractor: rapidez-media-error-paso (SCHOICE v1), FASE 2C, ronda 2

**Fecha**: 2026-11-05
**Artefacto**: md5 `2dfec0c9d8745af9c87bee94fd0126fc`, comprobado antes de empezar y al terminar
**Objeciones**: 6 (0 críticas, 0 altas, 6 medias)

## Resumen ejecutivo

No encontré ningún defecto de CORRECCIÓN: ni clave falsa, ni segunda clave, ni Solution falsa, ni distractor correcto, ni lectura razonable que cambie un veredicto. Las correcciones de la ronda 1 se sostienen y no abrieron un defecto nuevo de esa clase.

El veredicto es APROBAR_CON_CAMBIOS por un solo defecto que ve el estudiante: un doble punto en la Solution de la rama EV-T-FINAL, en el 15,8 % de las versiones. Las otras cinco objeciones no afectan a ninguna versión de hoy. Son controles y declaraciones que faltan: si mañana alguien rompe la prosa de la Solution, nada lo detecta.

| ID | Severidad | Tipo | Afectadas hoy | Reproducción |
|---|---|---|---|---|
| H1 | MEDIA | Gramática | 15,83 % por construcción; 9 de 100 semillas | semillas 8, 15, 20; rama EV-T-FINAL |
| H2 | MEDIA | Testing | 0; la prosa no tiene sonda | mutantes G y H sobreviven a 600 semillas |
| H3 | MEDIA | Testing | 0 de 254 733 conjuntos; falta la guarda | mutantes A y B sobreviven a 600 semillas |
| H4 | MEDIA | Invariante local | 0; L-6 sin guarda propia | el verificador no la nombra |
| H5 | MEDIA | Invariante local | canónica, P = 0,05; sin segunda clave | residuo sin declarar |
| H6 | MEDIA | Testing | 4 canales por encima de +8 pp sin cablear | no mueven el exit del verificador |

## Objeciones

### H1 — Doble punto tras «a. m.» y «p. m.» en la Solution

**Qué se cuestiona**: la línea 262 del `.Rmd` añade un punto tras `t1s`, que ya termina en «m.» (línea 159). El estudiante lee «el tramo empezó a las 2:00 p. m.. El tiempo transcurrido entre las dos lecturas…».

**Por qué** (Nivel 1, RAE): el punto de la abreviatura hace también de punto final y no se duplica. El ejemplo de la propia RAE es «Vendrá a las 4 p. m.».

**Riesgo medido**:
- Por construcción, 0,95 × 1/6 = 15,83 % de las versiones.
- 9 de las semillas 1 a 100 (N = 100) y 922 de 6 000. Semillas 8, 15, 20, 37, 58.
- Confirmado en render real con `exams2html`.
- Es la única ocurrencia: el mismo barrido sobre enunciado, opciones, Solution y veredictos no halló otra.
- Ningún control lo ve. El corrector mira tildes, la guarda de la línea 459 mira contracciones y la auditoría visual no midió ese patrón.

**Alternativa**, probada en memoria sobre 1 500 semillas y la canónica:

```r
" es una hora del reloj y el tramo empezó a las ", t1s, " El tiempo transcurrido entre las dos lecturas es ",
```

Añadir tras la guarda de contracciones la condición `!any(grepl("m\\.\\.", c(error_ok$analisis, diagnostico, feedback)))`, con un mutante que devuelva el punto. Con el texto actual, esa guarda aborta en la semilla 8.

**Veredicto**: MODIFICAR.

### H2 — La prosa de la Solution y los veredictos por opción no tienen sonda

**Qué se cuestiona**: el oráculo lee enunciado, pasos y opciones. No lee `consecuencia`, `feedback` ni la Solution, así que el verificador no los contrasta con nada.

**Por qué** (regla #13 v1.1): los códigos del arsenal comparan valores y no evalúan si un texto es verdadero. Por eso una Solution falsa del 2,84 % pasó una vez con 0 errores.

**Riesgo medido**:
- Hoy no hay defecto. Contrasté desde el texto, sin usar variables del `.Rmd`, el procedimiento correcto, «llega a X en lugar de Y», el caso específico, la clase de cada veredicto y que cada razón falsa se analice. Resultado: 0 problemas en 3 000 semillas, incluidas las semillas 1 a 100.
- Pero dos mutantes sobreviven a 600 semillas, a guardas y oráculo. El mutante G cambia la rapidez de `consecuencia` por `v + 5`. El mutante H hace que el veredicto de la opción correcta nombre otro paso.

**Alternativa**: incorporar esa sonda a `verificar_espacio.R`. La función `chequeo()` de `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0d13b1e6-8d43-4929-9f2b-9b27caa5513b/scratchpad/det/solcheck.R` tiene 15 líneas y mata a G y a H en la semilla 1.

**Veredicto**: MODIFICAR el verificador.

### H3 — Ninguna guarda exige que una razón afirme algo distinto de lo que rechaza

**Qué se cuestiona**: las guardas de las líneas 397 a 403 no cubren esa condición. Hoy se cumple solo porque las precondiciones la evitan, como la de la línea 247.

**Por qué** (regla #13 v1.1, unicidad con mutante; regla #9 v1.5): una propiedad que sostiene la lectura de cada opción no debe depender de una precondición suelta.

**Riesgo medido**: 0 de 254 733 conjuntos de razones hoy. Dos mutantes sobreviven a 600 semillas:
- Mutante A, sin la precondición de EV-D-SIGNO: emite «es −D km y no −D km».
- Mutante B, sin la de EV-C-UNIDAD: emite la misma fracción a los dos lados de «y no».

**Alternativa**, probada: pasa en 1 500 semillas y en la canónica, y mata a A en la semilla 66 y a B en la 10.

```r
stopifnot("ninguna razón afirma lo que rechaza" = all(vapply(ops, function(o) o$afirma != rechaza[[o$rol]], TRUE)))
```

**Veredicto**: MODIFICAR.

### H4 — L-6 no tiene guarda propia

**Qué se cuestiona**: L-6 declara cuatro normas de formato y el verificador no la nombra. Solo dos tienen defensa: U+2212 por la FASE 2O, y la franja por `ORACULO_DATOS`, que la cazó en la semilla 4 con un mutante mío.

**Por qué** (regla #9 v1.5): es el mismo caso que r1-H7, una invariante en prosa sin guarda ejecutable.

**Riesgo medido**: L-6 se cumple hoy. Hay 0 comillas invertidas en los chunks, 0 U+2212 literales y 0 de 300 versiones con un signo fuera de la fórmula. Una regresión solo costaría una falsa alarma de la FASE 2D o un salto de línea feo.

**Alternativa**: dos sondas en `verificar_espacio.R`, `!grepl(intToUtf8(96L), COD, fixed = TRUE)` y `!any(grepl("\\$[.,:;]", textos))`.

**Veredicto**: MODIFICAR, sin urgencia.

### H5 — La lectura de viaje completo de la canónica no está declarada como residuo

**Qué se cuestiona**: la opción A impresa, «100 km y no 80 km», admite esa lectura. L-4 la trata para las versiones generadas, pero «Residuos declarados» no la lista para la canónica, y el oráculo se salta la canónica en la comprobación de L-4 (línea 106).

**Por qué** (regla #13 v1.2): el texto impreso no se reescribe, y su lectura alternativa se declara como residuo siempre que no cree una segunda clave.

**Riesgo medido**: no hay segunda clave. Puestas en su paso, las tres razones falsas dan 100/2 = 50, −20/2 = −10 y 80/(−2) = −40, y ninguna da 40. Solo falta la declaración.

**Alternativa**: una línea en «Residuos declarados», con esas tres cuentas.

**Veredicto**: MODIFICAR.

### H6 — Los canales por encima de +8 pp no mueven el resultado del verificador

**Qué se cuestiona**: el resultado dice «CON RESIDUO DIAGNÓSTICO DECLARADO» y el exit 3 depende solo del veredicto de la batería congelada (línea 469). Los canales X0, X1a, X1b y X1 se imprimen y no se comparan con nada. Si X0 pasara de +11,4 a +30 pp, la salida sería la misma.

**Por qué** (regla #22 §P7-E): aceptar un canal por encima de +8 pp es un override firmado, regla por regla, y su alcance se cablea en el verificador con un mutante que lo saque de «cubierto».

**Riesgo medido**: reproduje las cifras de `.claude/CLAUDE.md` sin diferencias, así que la declaración es exacta. El hueco aparece solo si el profesor firma.

**Alternativa**: si firma, cablear cada canal con su cifra y un mutante. Hasta entonces no hay nada que cambiar.

**Veredicto**: MANTENER hasta la firma; después, MODIFICAR.

## Qué busqué y descarté en CORRECCIÓN

Enumeré los 14 346 estados no canónicos con sus pesos exactos y escribí una sonda nueva por cada lectura. Las cifras van por cada 10 000 versiones no canónicas.

1. **Lecturas del enunciado.** Leí las 9 plantillas. Ninguna ata una hora a «inició» ni admite otra salida.
2. **Razones que reparan.** Ninguna de las 109 021 razones candidatas da la rapidez correcta en su paso.
3. **Un hueco del verificador, cerrado.** Su enumeración pasa el oráculo por un conjunto de razones por estado. Medí los 254 733 conjuntos: 0 duplicados, 0 con el valor correcto, 0 con lo que muestra el paso, 0 con dos signos, 0 con una razón FINAL contra el paso mal.
4. **Razón cierta por coincidencia contra el paso que está mal.** `T-INICIAL` coincide con un tiempo real desde la salida en 14,4 versiones (34 estados; por ejemplo, t1 = 2, t2 = 5, d1 = 60, v = 30, con salida a las 12 m.). Queda fuera porque exige deducir una hora de salida que el ítem nunca pide, y sustituida da D/t1 ≠ v, ya que t1 ≠ dur (en el ejemplo, 45 y no 30). No cumple el criterio `repara()`.
5. **Razón cierta contra un paso correcto.** Con salida a las 12 m. (179,6), `T-FINAL` es literalmente cierta en 37,4, y `D-FINAL` y `T-FINAL` coinciden en la misma versión en 6,5. Tampoco es segunda clave: tendrían que cambiar los dos pasos a la vez y la rapidez no cambia, así que no señalan un error único.
6. **Procedimiento cierto en el viaje completo.** Con salida a las 12 m., el paso de T-FINAL (29,9) y el de D-FINAL (20,0) son ciertos en esa lectura. La clave sigue siendo la única opción que da `v`, porque `repara()` retira la otra.
7. **Razones de signo.** Dan `−v`, pero una distancia o un tiempo negativos son falsos en toda lectura.
8. **Razones del paso 3.** «2 h/80 km» empareja bien cifra y unidad, pero el enunciado pide km/h. «2 km/80 h» cambia la magnitud de las dos cifras. Ninguna da `v` en km/h.
9. **Clave falsa.** En las cinco ramas de distancia o tiempo, lo que muestra el paso 3 nunca es `v`: exigiría `d1 = 0` o `t1 = 0`.
10. **Lectura de versiones.** Leí 26 versiones con dos opciones en el paso de la clave y la Solution de las 6 ramas de error. También revisé el PDF de 2 páginas y los textos contra el recorte impreso.

## Diagnosticidad

La batería congelada da +6,0 pp, `NO_CONCLUYENTE`, igual que `verificar_r6b.txt`. No obliga. La mezcla con la canónica llega a +7,9 pp. Los canales fuera de la batería siguen como están declarados y los decide el profesor: X0 +11,4/+12,5, X1b +10,7/+9,0, X1a +18,7/+17,3 y X1 +36,2/+34,4 pp.

**Trade-off que no objeto.** En EV-D-FINAL y EV-T-FINAL, el 27,8 % de las no canónicas, el paso mal muestra «− 0 km» o «− 0 h». Esa marca lleva al paso de la clave y acierta el 75 % de las veces (1/2 × 1 + 1/2 × 1/2). Pertenece a la misma clase que «h/km» y el negativo, que la cabecera del verificador excluye por ser el constructo, pero esa cabecera no la nombra. Conviene añadirla allí.

## Correcciones de la ronda 1

| Hallazgo | Estado |
|---|---|
| r1-H1 plantilla `noticia` | Corregido; guarda L-7 y M14 |
| r1-H2 viaje completo | Corregido con `repara()`; 0 razones que reparan |
| r1-H3 «a el», «de el» | Corregido; 0 de 6 000; M16 |
| r1-H4 y r1-H5 canales | Declarados con cifras exactas; ver H6 |
| r1-H6 signos que compensan | Corregido con `repara()` |
| r1-H7 guarda de L-2 | Corregido; M13 muere por su sonda |
| r1-H8 falso APROBADO | Corregido: exit 3; queda el alcance de H6 |
| r1-H9 sello sin artefacto | Corregido: `auditoria_visual_r2.md`, con su md5 |

## Invariantes locales

- L-1 se cumple: guardas vivas, y M2 y M9 mueren por su sonda.
- L-2 se cumple, con guarda completa.
- L-3 se cumple: contrasté enunciado, pasos y opciones con el recorte, incluido el −20. Falta H5.
- L-4 se cumple: ver los puntos 2 a 6.
- L-5 se cumple. Las cifras históricas 44,7, 39,2 y +12,4 están en `salida/`.
- L-6 se cumple, sin guarda propia (H4).
- L-7 se cumple: guarda y M14.

## Los 8 dominios

- **Código R-exams**: sin aborto en los 14 346 estados. Queda H3.
- **Pedagógico**: las secciones de la Solution están y las plantillas cumplen la regla #11. A vigilar en el aula, sin datos para objetar: las ramas de signo pueden ser más fáciles que el impreso.
- **Visual**: PDF sin desbordes; sin fugas de LaTeX en DOCX ni en Moodle.
- **Gramática**: H1.
- **Coherencia matemática**: sin hallazgos.
- **ICFES metacognitivo**: literalidad de `exextra` con 0 fallos contra el catálogo canónico. Las 6 advertencias del arsenal son heurísticas genéricas: por ejemplo, los 4 ejercicios hermanos que revisé tampoco llevan `exextra[Tarea]`.
- **Testing**: H2, H3, H4 y H6.
- **Semántico**: sin hallazgos.

## Lo que ejecuté

- `verificar_espacio.R rapido`: oráculo 300 + canónica sin problemas, 16 de 16 mutantes, exit 3.
- **El verificador cambió durante la auditoría**, a las 12:23:42, en el mutante M7. Mi corrida usa ese archivo, md5 `61be5c5f…`, y coincide con `verificar_r8.txt`. `verificar_r7.txt` queda obsoleto: dice 15 de 16.
- Enumeración propia de 14 346 estados y 254 733 conjuntos.
- 8 mutantes propios. Además de G, H, A y B, murieron los dos que recortan `repara()`, y sobrevivieron E (sin el tope `d1 <= 2 v`) y F (`d1 = v`). No objeto estos dos, porque ninguna invariante los exige.

Las 6 000 y 3 000 semillas fueron búsqueda de casos, no medición. Junto a ellas va la cifra con N = 100 (regla #23).

## No auditado

- El DOCX abierto en Word; solo revisé su texto y su XML.
- El HTML a 320 px. El auditor midió 360 y 1024 px con N = 24 y sin razón declarada para ese N (regla #23). Por cálculo no hay riesgo: la fórmula más ancha mide 181 px y la caja de Moodle, 270 px.
- Los escaneos NOPS.
- La afirmación sobre los 7 ítems de 2025-2 en `ITEM_OFICIAL.md`: no los localicé en una búsqueda limitada.
- La vara del corpus oficial para los canales X.

## Veredicto global

**APROBAR_CON_CAMBIOS**. Cero defectos de CORRECCIÓN y cero objeciones críticas o altas. H1 debe corregirse antes de pedir la aprobación del profesor, porque lo ve el estudiante.

## Próximos pasos

1. Corregir H1 con su guarda y su mutante, y volver a renderizar.
2. Añadir las guardas de H3 y H4 y la sonda de H2, y declarar H5.
3. Llevar al profesor los cuatro canales, regla por regla, y aplicar H6 según lo que decida.
4. H1 cambia el md5: este reporte audita `2dfec0c9…`. Si hace falta otra ronda para sellar `detractor_fase2c` lo decide quien coordina, según la regla #9.

## Fuentes externas

- [RAE: ¿Después de un punto de abreviatura se pone otro punto si termina el enunciado?](https://www.rae.es/duda-linguistica/despues-de-un-punto-de-abreviatura-se-pone-otro-punto-si-termina-el-enunciado)
- [RAE: Diccionario panhispánico de dudas, «abreviatura»](https://www.rae.es/dpd/abreviatura)

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:262 | " es una hora del reloj y el tramo empezó a las ", t1s, ". El tiempo transcurrido entre las dos lecturas es ",
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:159 | hora <- function(t) paste0(t, ":00", ED, franja, ED, "m.")
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:8 | parte del TEXTO que lee el estudiante
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R | AUSENTE: e$feedback
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R | AUSENTE: consecuencia
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:438 | Por eso el paso 3, que hace bien su cociente con los valores que recibió, llega a
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:400 | "opciones únicas" = length(unique(opciones)) == 4L,
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:247 | function(p) p$error_cometido != "EV-D-SIGNO", function(p) -D,
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd | AUSENTE: o$afirma != rechaza
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:56 | Ninguna comilla invertida dentro de los chunks
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R | AUSENTE: L-6:
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md | AUSENTE: lectura alternativa
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:106 | if (n == 3L || isTRUE(e$es_canonica)) next
H6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:469 | residuo <- exists("r_all") && !identical(r_all$veredicto, "PASS")
H6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:87 | Canales fuera de la batería congelada
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
