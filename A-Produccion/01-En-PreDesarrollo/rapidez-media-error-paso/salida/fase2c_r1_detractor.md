# Auditoría Detractor: rapidez-media-error-paso (SCHOICE v1), FASE 2C, ronda 1

**Fecha**: 2026-11-05
**Artefacto**: md5 `11c40c25215a0ca282aa7d22255ee448`, comprobado antes de auditar
**Objeciones**: 9 (2 críticas de CORRECCIÓN, 2 altas, 5 medias)

## Resumen ejecutivo

El diseño de fondo resuelve bien la tensión del HANDOFF. La clave varía, con 135 textos de clave distintos en 300 semillas. La canónica es verbatim y el formato no delata la clave dentro de la batería congelada (+5,7 pp).

Rechazo por dos defectos binarios que ningún verificador ve:

- **La plantilla `noticia`.** Admite una lectura natural que cambia el dato de salida. Con esa lectura hay segunda clave o clave falsa en el 3,6 % de las versiones.
- **La lectura de «viaje completo» que la invariante L-4 da por válida.** Crea segunda clave en 22,9 de cada 10 000 versiones no canónicas. La semilla 1, primera del modo `smoke`, es una de ellas. Eso refuta la frase de L-4 de que esa lectura no crea segunda clave contra un paso correcto.

Además, la Solution trae «a el auto de el pueblo» en el 24,2 % de las versiones, incluidas todas las canónicas. Y la batería tiene un punto ciego por encima de +8 pp.

| ID | Severidad | Tipo | Afectadas | Reproducción |
|---|---|---|---|---|
| H1 | CRÍTICA | CORRECCIÓN, semántico | 10,56 % ambiguas; 3,64 % con el veredicto invertido | semillas 9, 22, 33, 112 |
| H2 | CRÍTICA | CORRECCIÓN, matemático | 22,9 de cada 10 000 no canónicas | semillas 1, 367, 683, 1684, 1532, 2585 |
| H3 | ALTA | Gramática | 24,2 %; 25 de 100 semillas; toda canónica | canónica, EV-D-FINAL ofrecida |
| H4 | ALTA | DIAGNOSTICIDAD | +10,8 / +12,3 pp | muestra del verificador y muestra independiente |
| H5 | MEDIA | DIAGNOSTICIDAD | +33 pp compuesta; +4,4 a +4,8 pp sola | 158/300 y 164/300 versiones |
| H6 | MEDIA | Pedagógico | 4,09 % de las no canónicas | semillas 69, 86 |
| H7 | MEDIA | Invariante local | L-2 sin guarda propia | mutante `mult <- 5L`: 265/300 sobreviven |
| H8 | MEDIA | Testing | el «APROBADO» no depende de §P7 | `verificar_r5.txt` |
| H9 | MEDIA | Trazabilidad | sello 6b sin artefacto | `ejercicio_state.json` |

## Objeciones

### H1 — La plantilla noticia ata la primera hora al verbo «inició»

**Qué se cuestiona**: la línea 176 del `.Rmd` produce «ya se había alejado 105 km del peaje donde inició a las 6:00 a. m., y a las 9:00 a. m. se encontraba a 285 km».

**Por qué** (regla #13 v1.1, «Toda lectura razonable cuenta»): la hora queda pegada a la oración de relativo. La lectura más cercana es «el peaje donde inició a las 6:00 a. m.», es decir, que salió a las 6:00. Es la única de las 9 plantillas donde la hora va detrás del verbo que debe anclar. En las otras 8 va antes.

**Riesgo medido**: es 0,95 × 1/9 = 10,56 % por construcción. En 20 000 semillas salió 10,58 %, y en las semillas 1 a 100 salió 21 veces.

- **Segunda clave.** EV-D-FINAL ofrecida: 506 de 20 000 (2,53 %), 5 de las semillas 1 a 100.
  - Semilla 9: con salida a las 6:00, «la distancia recorrida es 285 km y no 180 km» es verdadera, igual que la clave.
  - Semilla 22: se reproduce lo mismo.
- **Clave falsa.** EV-D-FINAL cometido: 222 de 20 000 (1,11 %), 1 de las semillas 1 a 100.
  - Semilla 33: con esa lectura, 160 km en 4 h son 40 km/h y el procedimiento no tiene error.
  - Semilla 112: se reproduce lo mismo.
- La preview del propio hook (informe `20261105-114653-846191`, página 1) salió con esta plantilla. La 2A y el multisemilla no pueden verlo, y el oráculo tampoco: extrae las horas y las distancias del texto y comparte la definición de paso correcto.

**Alternativa**:

```r
noticia = function() paste0("Según informó la concesión vial, a las ", t1s, " ", veh$un,
  " que viajaba con rapidez constante ya se había alejado ", kmt(d1), " ", lugar$del,
  " donde inició, y a las ", t2s, " se encontraba a ", kmt(d2), " de ", lugar$dicho, "."),
```

Añadir una guarda en `data_generation`, con su mutante: `regexpr(t1s, enunciado, fixed = TRUE) < regexpr(kmt(d1), enunciado, fixed = TRUE)`.

**Veredicto**: MODIFICAR.

### H2 — La lectura de viaje completo crea segunda clave contra un paso correcto

**Qué se cuestiona**: L-4 afirma que esa lectura no crea segunda clave contra un paso correcto. La línea 82 del `.Rmd` permite `d1 = 2 v`, sin excluir las coincidencias.

**Por qué** (regla #13 v1.1, regla #9 v1.5): con rapidez constante, la salida queda determinada por los datos. Cuando `d2 = v·t2` o `d2 = v·t1`, una razón «falsa» repara el procedimiento y da exactamente la rapidez correcta. Además, su afirmación es literalmente cierta en la lectura que L-4 admite.

**Riesgo medido**: enumeración exacta de 14 346 estados (fila, d1, error, paso repetido) con todos los conjuntos de razones y sus pesos. El total es 22,9 de cada 10 000 no canónicas. En las semillas 1 a 100 hay 1 caso.

- **T-FINAL cometido + D-FINAL ofrecida: 11,0 de cada 10 000** (semillas 1532, 2585).
  - Semilla 1532: 540 km a las 6:00 p. m. a 90 km/h, salida a las 12 m.
  - El paso 2 «6 h − 0 h = 6 h» es cierto desde la salida, y «la distancia recorrida es 540 km y no 360 km» da 540/6 = 90.
  - La clave señala otra opción.
- **D-FINAL cometido + T-FINAL ofrecida: 6,4 de cada 10 000** (semillas 683, 1684).
  - Semilla 683: «el tiempo transcurrido es 6 h y no 4 h» da 180/6 = 30.
- **D-FINAL cometido + T-INICIAL ofrecida: 5,4 de cada 10 000** (semillas 1, 367).
  - Semilla 1, que sale en `smoke`: salió a las 4:00 a. m., lleva 6 h y 420 km a las 10:00.
  - «6 h y no 4 h» da 420/6 = 70, la rapidez correcta.

El oráculo no puede verlo: su cabecera declara que comparte la definición de paso correcto. Además, una muestra de 300 esperaría 0,7 casos.

**Alternativa**:

```r
cand <- cand[cand != fila$v & cand != fila$D &
             cand != fila$v * fila$t1 & cand + fila$D != fila$v * fila$t1]
```

- Añadir `d1 != v * t1, d2 != v * t1` a la guarda de la línea 92, con su mutante.
- Medido: retira 17 de 797 pares (fila, d1), y toda fila conserva al menos 2 valores de `d1`.
- Reescribir L-4 con la condición exacta.
- La regla #13 pide enumerar un espacio finito: llevar esa enumeración al verificador.

**Veredicto**: MODIFICAR.

### H3 — «a el» y «de el» en la Solution

**Qué se cuestiona**: la línea 219 del `.Rmd`. La canónica emite «100 km es lo que separa a el auto de el pueblo». Lo verifiqué en el texto de la Solution canónica.

**Por qué** (norma de la RAE: las contracciones `al` y `del` son obligatorias; regla #7 para texto visible): el corrector no lo detecta, y su «limpio» tampoco lo prueba.

**Riesgo medido**: 9 687 de 40 000 semillas (24,2 %), 25 de las semillas 1 a 100 y todas las canónicas.

**Alternativa**: añadir un campo `al` a `vehiculos` («al auto», «a la moto») y escribir `" es lo que separa ", veh$al, " ", lugar$del, " a las "`.

**Veredicto**: MODIFICAR.

### H4 — La regla del enunciado borra las horas: canal por encima de +8 pp sin sonda

**Qué se cuestiona**: las reglas EST de `verificar_espacio.R` borran las horas antes de comparar (línea 245).

**Por qué** (regla #22 §P7: «una batería incompleta mide sin sonda»; §P7-G, §P7-E): la clave, `D` o `dur`, nunca es un dato del enunciado por construcción. Las razones FINAL e INICIAL sí lo son, y las de tiempo quedan fuera de la sonda.

**Riesgo medido**, regla «la cifra afirmada no repite un dato del enunciado», con las horas conservadas:

- Muestra del verificador, n = 300 (N = 100 por rol): 39,7 %, +10,8 pp, aplicabilidad 80 %.
- Muestra independiente desde la semilla 200001: 41,1 %, +12,3 pp, aplicabilidad 87 %.
- La regla congelada da 30,2 % con aplicabilidad 50 %.
- La clave sobrevive siempre. No aplica margen §P7-B porque la propiedad es discreta.

Trade-off: la canónica da 50 %, así que es un canal estructural heredado del impreso, no un defecto del rediseño.

**Alternativa**: no pido una cuarta pasada, porque §P7-D está agotado. Añadir la regla, re-medir el histórico (§P7-C) y llevar la cifra al profesor como override firmado regla por regla (§P7-E, H-5).

**Veredicto**: MANTENER el diseño, con firma humana.

### H5 — L-2 no cierra la divisibilidad por sustitución

**Qué se cuestiona**: el filtro de la línea 72 exige `D %% t2` y otra línea `d1` múltiplo de `dur`. No cubre `t1` ni `t1 + t2`.

**Por qué** (L-2: «la divisibilidad delataría la clave»): sustituir una razón en el procedimiento revela qué opciones dan rapidez entera.

**Riesgo medido** en las dos muestras de H4, sin margen §P7-B por ser propiedad discreta:

- X1 (positiva, en km/h y entera): 61,9 / 62,2 %, +33,0 / +33,4 pp.
- X1a (solo positiva en km/h): 45,1 / 45,2 %, +16,2 / +16,3 pp. Esta parte es constructo.
- X1b (solo entera): 33,7 / 33,3 %, +4,8 / +4,4 pp.
- El criterio entero descarta al menos una opción en 158/300 y 164/300 versiones, y nunca la clave.
- La canónica da {A, C}, 50 %.

Sola no obliga. Compuesta añade unos +17 pp a la parte legítima.

**Alternativa**: declararla como residuo y acotar L-2 a lo que muestra el procedimiento. Si se quisiera cerrar, ofrecer EV-T-INICIAL y EV-T-SUMA solo cuando `Dm` sea divisible, pero eso exigiría otra pasada.

**Veredicto**: MANTENER con residuo declarado.

### H6 — Pareja de signos que compensa

**Qué se cuestiona**: la línea 352 limita las razones de signo ofrecidas, pero no el cruce con el signo cometido.

**Por qué** (regla #13, opciones-afirmación): con D-SIGNO cometido y T-SIGNO ofrecida, (−240)/(−3) = 80, justo la rapidez correcta (semilla 69; también 86).

**Riesgo medido**: 3,47 % + 0,62 % = 4,09 % de las no canónicas, por enumeración exacta. No lo clasifico como segunda clave: «el tiempo transcurrido es −3 h» es falsa en toda lectura, y el paso 2 está bien. Pero quien comprueba sustituyendo obtiene la rapidez exacta con una razón falsa, y la Solution no lo explica.

**Alternativa**: precondición `!grepl("SIGNO$", p$error_cometido)` en ambas razones de signo, y re-medir.

**Veredicto**: MODIFICAR, a criterio del profesor.

### H7 — L-2 sin guarda propia

**Qué se cuestiona**: la guarda de la línea 92 lleva `D %% t2` y omite `d1 %% dur`. Tampoco hay mutante que nombre a L-2.

**Por qué** (regla #9 v1.5, invariantes en prosa sin guarda): L-2 dice que la condición vale en todas las versiones.

**Riesgo medido**: el mutante `mult <- 5L`, sobre 300 semillas, deja 265 sobrevivientes. Mueren 35, por otra guarda (`L-1: la rapidez que muestra el paso 3 es entera`).

**Alternativa**: añadir `d1 %% dur == 0L` a la guarda y un mutante con esa sonda.

**Veredicto**: MODIFICAR.

### H8 — El verificador dice APROBADO con la batería NO_CONCLUYENTE

**Qué se cuestiona**: el bloque de batería no toca `errores_total`, así que `RESULTADO: APROBADO` y exit 0 no dependen del veredicto §P7.

**Por qué** (regla #22: `NO_CONCLUYENTE` no es PASS): la salida r5 trae NO_CONCLUYENTE y, en la mezcla con la canónica, `MAG-segundo-afirmado` en +8,1 pp, sin residuo declarado. El margen §P7-B de esa regla es amplio: mediana 60 %, con el 95 % de las versiones ≥ 15 %. Es explotable, pero no estable: en la muestra independiente baja a +6,6 pp, y sube `REL-afirmado-citado-en-otra` a +8,1 pp en las no canónicas. El ruido ronda +8.

**Alternativa**: salida propia para «con residuo» y, en `.claude/CLAUDE.md` local, la lista del residuo regla por regla con sus cifras (§P7-D).

**Veredicto**: MODIFICAR.

### H9 — Sello 6b sin artefacto

**Qué se cuestiona**: la nota del paso `arsenal_post_render` en `ejercicio_state.json`.

**Por qué** (regla #16, «auditar por artefacto»): la cadena solo aparece en ese JSON. No la encontré en el subproyecto ni en la caché del hook, y en `/tmp` no hay reporte del auditor visual. Las observaciones no están enumeradas.

Audité los demás sellos pareados, separados por milisegundos: los renders (desde las 11:46:48) son posteriores al `.Rmd` (11:46:10), y el informe del arsenal existe. No son sellos vacíos.

**Alternativa**: el `.Rmd` va a cambiar, así que hay que repetir `/auditor-visual-html` y guardar su reporte en `salida/`, con las observaciones listadas.

**Veredicto**: MODIFICAR.

## Invariantes locales

- L-1 se cumple: guardas vivas, M2 y M9 mueren por su sonda.
- L-2 se cumple en los datos, pero con guarda parcial (H7) y alcance menor del que declara (H5).
- L-3 se cumple: contrasté enunciado, pasos y opciones con el recorte y con `ITEM_OFICIAL.md`, incluido el −20.
- L-4 se cumple como guarda, pero su justificación es falsa en los casos de H2.
- L-5 se cumple en la batería congelada, con el punto ciego de H4.
- L-6 se cumple: sin comillas invertidas en los chunks y 0 U+2212 literales.

## Los 8 dominios

- **Código R-exams**: sin aborto en 14 346 estados ni en 40 000 semillas. Queda H7.
- **Pedagógico**: las 6 secciones de la Solution están y las 9 plantillas cumplen la regla #11, salvo H1. Queda H6. A vigilar en el aula: las ramas de signo (27,8 % de las no canónicas) pueden ser más fáciles que el impreso. Sin datos no es objeción.
- **Visual**: leí las 2 páginas de la preview del PDF, sin desbordes. El bloque `{=latex}` no se filtra a HTML ni a Moodle.
- **Gramática**: H3. La ortografía de tildes pasa, y `Solution` no usa letras.
- **Coherencia matemática**: H2 y H6.
- **ICFES metacognitivo**: la literalidad de `exextra` da 0 fallos. Las líneas 304, 566 y 592 del Marco sostienen la clasificación como Numérico-variacional en contexto familiar. El cruce con el estándar métrico está declarado y es correcto.
- **Testing**: H7 y H8.
- **Semántico**: H1.

## Lo que ejecuté y pasó

Reproduje `todo` idéntico a `verificar_r5.txt`:

- Oráculo: 300 + canónica, 0 problemas.
- Mutantes: 12 de 12 por su sonda.
- Batería: +5,7 pp, sin cambios.

Sobre una copia en el scratchpad, con el md5 actual:

- `validar_diversidad_sustantiva.R`: PASS, 60 valores de clave.
- `validar_diagnosticidad.R`: PASS, H1 en 10 % y 5 %.
- `validar_stresstest.R`: 100/100. El archivo guardado `stresstest_r4.txt` era de otro md5.
- `validar_multisemilla.R`: 100/100.

Las 40 000 y 20 000 semillas fueron búsqueda de reproducciones, no medición: junto a ellas va la cifra de N = 100 (regla #23).

## No auditado

- El DOCX abierto.
- El HTML a 320 y 360 px.
- Los escaneos NOPS.
- La afirmación sobre los 7 ítems de 2025-2.
- Las cifras históricas de L-5 (+10,3 y +12,4 pp).
- La vara del corpus oficial para las reglas X1 y X2.

## Veredicto global

**RECHAZAR**: H1 y H2 son defectos de CORRECCIÓN y son binarios.

## Próximos pasos

1. Corregir H1, H2 y H3. No consumen presupuesto §P7-D.
2. Corregir H7 y H8, y decidir H6.
3. Volver a medir oráculo, mutantes y batería, porque H2 cambia el espacio.
4. Repetir el render y el auditor visual, guardando su reporte (H9).
5. Llevar H4 y H5 al profesor con sus cifras, regla por regla, para firma o residuo declarado.
6. Lanzar la ronda 2 de FASE 2C sobre el md5 nuevo. Este reporte no sirve para sellarla.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:176 | " que viajaba con rapidez constante ya se había alejado ", kmt(d1), " ", lugar$del, " donde inició a las ",
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:30 | Contra un paso correcto esa lectura no crea segunda clave, y por eso el impreso la usa
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:82 | cand <- seq(mult, 2L * fila$v, by = mult)
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:219 | paste0(M(km(d2)), " es lo que separa a ", veh$el, " de ", lugar$el, " a las ", t2s,
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:245 | conj(!(abs(num1(o)) %in% nums(gsub("[0-9]+:00", "", en)))) }, est = TRUE),
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:72 | espacio$dur != espacio$t1 & espacio$D %% espacio$t2 == 0L & espacio$D <= 360L, ]
H6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:352 | candidatos <- Filter(function(s) sum(grepl("SIGNO$", s)) <= 1L, candidatos)
H7 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:92 | v == round(v), v >= 30, v <= 90, dur >= 2L, d1 > 0L, d2 > d1, d1 != D, t1 != dur, D %% t2 == 0L)
H7 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd | AUSENTE: d1 %% dur == 0L
H8 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:375 | cat("\nRESULTADO:", if (errores_total == 0L) "APROBADO" else "CON ERRORES", "(", errores_total, ")\n")
H8 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/salida/verificar_r5.txt:11 | veredicto NO_CONCLUYENTE
H8 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/salida/verificar_r5.txt:65 | MAG-segundo-afirmado 100.0    33.7   37.0              8.1
H9 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/ejercicio_state.json:47 | 6b: APTO_CON_OBSERVACIONES, N = 24 por texto, 3 por imagen
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: RECHAZAR
