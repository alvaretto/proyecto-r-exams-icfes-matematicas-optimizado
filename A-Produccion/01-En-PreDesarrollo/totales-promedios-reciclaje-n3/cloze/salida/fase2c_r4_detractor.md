<!-- Reporte íntegro del AgenteDetractor de la FASE 2C r4 del CLOZE (ciclo 2026-11-02, relanzada tras el corte eléctrico de las 14:19; .Rmd 27f789dc), copiado de su transcripción por el coordinador el 2026-11-03 (mensaje final: 2026-11-03T19:53:01.246Z). Lo que el detractor dejó en el scratchpad de la sesión, que es tmpfs, se copió a `cloze/salida/fase2c_r4/`; la cita H1 de `correccion_r4b_v2.txt` conserva la ruta original. -->

# Auditoría del detractor, FASE 2C r4 (relanzamiento): CLOZE `totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1`

**Fecha:** 2026-11-03.

**Archivos auditados:**
- `.Rmd`: md5 `27f789dc6327f7d6bda72cded61502cd`. Lo comprobé con `md5sum` y coincide.
- Verificador: md5 `6e76321a22e2026adaa3a4b5f576f070`.
- SCHOICE: md5 `63e2713e6ac0bdbf250f4dc33901b2ec`, el mismo que da `git show 267e0611`.

Soy un detractor nuevo. No había auditado ninguna versión de este CLOZE ni de su SCHOICE, y no usé `~/.cache/fase2c_r4_cloze/`. La auditoría fue solo de lectura: todo lo que generé está en `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/358ab328-e44a-43ce-99ab-b2fa5570d4a9/scratchpad/`.

**Objeciones: 3, todas BAJAS** (0 críticas, 0 altas, 0 medias). **No hay ninguna CORRECCIÓN** en las 6 partes.

## Resumen

- **El re-empalme está bien.** El bloque de las líneas 2-612 es idéntico byte a byte al SCHOICE `63e2713e`, salvo las 4 líneas renombradas. El script comprueba el md5 y el bloque viejo antes de sustituirlo.
- **El verificador reproduce su resultado.** Lo corrí aparte y dio exit 4, con 1409 líneas idénticas a `verificar_espacio_cloze_c1.txt`.
- **Corrección:** mi propio oráculo, que lee el texto que emite R/exams, dio 0 fallos en las 111 versiones de E1.

Las tres objeciones no bloquean nada:
- **H1:** la redacción nueva de H3 en la Solution.
- **H2:** el mutante de texto de MC39 no es aditivo.
- **H3:** el paso 8 no puede reutilizar el oráculo `coherencias_r9`.

## Objeciones

### H1 · BAJA · Precisión de la Solution (regla #13, prosa) y tipografía (Parte 6, H3)

**Qué se cuestiona.** Las líneas 1056-1060 del `.Rmd`: «… habría ganado con un curso más si y solo si su promedio por curso, *a′_P* kilos, es mayor que la diferencia, *D′* kilos; y … habría perdido con un curso menos si y solo si su promedio, *a′_G* kilos, es mayor que esa diferencia.»

**Lo que está bien.** El bicondicional es verdadero:
- En las 111 versiones, con mi función de verdad sobre el texto emitido: el sujeto es el perdedor y el ganador, y las cifras son *a′_P*, *a′_G* y *D′*.
- En todo el espacio, por álgebra:
  - *T′_P* + *a′_P* > *T′_G* ⇔ *a′_P* > *D′*;
  - *T′_G* − *a′_G* < *T′_P* ⇔ *a′_G* > *D′*.
- No hay empates: los excluyen `espacio_paralelo()` (línea 640) y la guarda «Parte 6: hipótesis sin empate» (línea 872).

**El problema.**
- La comparación va en indicativo y lleva las cifras de la versión. En **70 de 111 versiones (63 %)** al menos una de las dos es falsa como enunciado suelto, por ejemplo «su promedio por curso, 30 kilos, es mayor que la diferencia, 40 kilos».
- Es el espejo del H3 de la r3. Allí el subjuntivo se leía como contrafáctico en el caso verdadero; aquí el indicativo se puede leer como una afirmación en el caso falso.
- No cambia ningún veredicto: «Respuestas» y «Parte 6 · Afirmaciones falsas» dan cada marca de forma explícita y verdadera. Por eso no es CORRECCIÓN.
- Se suma el BAJO de la 6b: «diferencia.» queda sola en el último renglón a 360 px en 99 de 100 semillas.

**Alternativa.** Resolver la comparación por rama, con las variables que ya existen:

```r
"Con un curso más, el grado ", per_par, " sumaría ", dp$aP, ED, "kilos, ", if (v_hp) "más" else "menos",
" que la diferencia de ", dp$D, ED, "kilos: ", if (v_hp) "habría ganado" else "no le alcanzaría para ganar", ". ",
"Con un curso menos, el grado ", gan_par, " restaría ", dp$aG, ED, "kilos, ", if (v_hg) "más" else "menos",
" que esa", ED, "diferencia: ", if (v_hg) "habría perdido" else "habría seguido ganando", "."
```

Si se conserva la frase actual, basta con `"esa", ED, "diferencia."` para cerrar el BAJO de la 6b. Cualquiera de los dos cambios toca el `.Rmd`, así que exige reapertura y puede diferirse como se difirió el H3 de la r3.

**Veredicto:** MODIFICAR, opcional.

### H2 · BAJA · Cobertura del verificador: el mutante de texto de MC39 muere también por otra sonda

**Qué se cuestiona.** El mutante de texto de MC39 (líneas 1207-1223): BP_gano gana «… que el que perdió hubiera tenido, conservando su».

**Evidencia.** Calculé los n-gramas con las funciones de la propia batería, `.pal` y `.ngr`: `.pal` quita la puntuación y `.ngr` forma n-gramas de 1 a 3 palabras. Entre los cuatro textos de ventaja, el mutante hace dos cosas:
- Añade 3 n-gramas exclusivos de BP_gano: «tenido conservando», «hubiera tenido conservando» y «tenido conservando su».
- Deja otros 3 como exclusivos de BC_gano: «perdió conservando», «que perdió conservando» y «perdió conservando su».

El exceso de una regla por palabra depende solo de qué textos llevan la palabra (`vocab_p6`: el vector `p` sobre los textos). Así que esos 6 n-gramas tienen la misma cifra que reglas ya firmadas a la mitad del tope. Por ejemplo, «cursos habría perdido», que solo lleva BP_gano, está firmada así en la lista: CURSOS +12,5 (contraria), PROMEDIO +12,5, *k*′ = 1 +8,2 y *k*′ ≥ 2 +8,4 pp (líneas 51, 105, 159 y 213 del `.tsv`). Los 6 n-gramas no están en la lista, así que con el mutante quedarían pendientes por la cláusula «fuera de la lista», que es la de MC36 y MC38.

Esto lo deduje de los conjuntos de textos: no corrí `h2_exacta()` con el mutante.

Además:
- El veredicto (línea 1216) dice `cazado_por_su_sonda` en cuanto `j39` tiene una fila, sin mirar los otros pendientes.
- No hay línea base en la misma medida (`h2_exacta(m39$txt, 1L)`, con una sola Parte 1). El «sube de +12,5» del registro es la cifra firmada sobre 100 Partes 1, no la de la semilla 1 sin mutar.

**Lo que sí prueba.**
- `j39` exige que la regla de la lista quede pendiente con la firma regla por regla y cubierta con la de estrato. Sin la cláusula «o que pase de su cifra», el veredicto sería `cazado_por_otra` y el `ok()` fallaría. El mutante sí prueba que la cláusula existe y actúa.
- La sonda directa (líneas 1201-1206) la aísla sin depender del texto.

**Alternativa** (endurece, así que es autónoma):
- Etiquetar `cazado_por_su_sonda_y_otra`, con el conteo de pendientes fuera de `j39`, como ya hace `juzgar_s1()` en el SCHOICE desde la S2b.
- Calcular una línea base, `h2_exacta(COD_C, 1L)`, y exigir que las filas de `j39` salgan «cubierto» en ella.

### H3 · BAJA · Procedimiento: el paso 8 no puede reutilizar `coherencias_r9` (L-C8)

**Evidencia.** `coherencias_r9/verificar_coherencias.R` compara la Parte 1 y la Solution contra cadenas literales del texto anterior:
- `exp1` (línea 360), que M5 compara con `identical(sl[clave1], exp1)`;
- `fila_p1` (línea 398);
- `ganol1` (línea 403);
- `umbral` (línea 414), con la redacción vieja de H3, «solo si … fuera mayor».

`27f789dc` emite otra cosa:
- en la Parte 1, la unidad y el signo dentro de la fórmula (`\ \text{kilos}$ y`, `\text{kilos:}$`, `\text{kilos.}$`, por L-6);
- en la Parte 6, «si y solo si … es mayor».

El oráculo fallaría a la vista, sin dar un falso verde, así que el objetivo de L-C8 se cumple. Pero no se puede reutilizar. Además escribe `resultados_verificacion.rds` en el directorio de trabajo (línea 610).

**Lo que hay que rehacer (oráculo r10):**
1. Volver a construir las expectativas de la Parte 1 en el formato de L-6: el feedback de las 4 opciones, el procedimiento, «Ganó …» y el caso específico. Y la del párrafo nuevo de H3. Mejor con un lector de las cifras dentro de `$…$` que con plantillas `sprintf`.
2. Leer el texto de la Parte 1 normalizado (U+00A0 como espacio), como ya hace `or_p1(nf = norm_ed)` en E2, y dejar las comprobaciones de L-6 sobre el texto crudo.
3. La condición 2 de L-C8 solo cubre el banco de la Parte 2, que no cambió (la S2 global seguiría en verde). El texto que cambió esta vez es el de la Parte 1 (L-6) y el de la Solution de la Parte 6 (H3). El r10 tiene que juzgarlos, cada uno con su mutación de control: por ejemplo, la redacción vieja de H3 y una unidad fuera de una fórmula de la Parte 1 tienen que morir.
4. Repetir las comprobaciones entre formatos (5 529 en la r9): Moodle (`\(…\)` con `\text{kilos.}`), el PDF también con el pandoc de RStudio, y DOCX.
5. Aplicar el endurecimiento ya registrado en L-C8: volcar el pool sin las marcas de R/exams.
6. Correrlo fuera del repo, por el `saveRDS` de la línea 610.

## Lo que comprobé y salió bien

1. **Re-empalme.**
   - Comparé las líneas 2-612 del CLOZE (con los renombres deshechos) con las 2-612 del SCHOICE: «BLOQUE IDENTICO salvo renombres». Las únicas diferencias crudas están en las líneas 576, 577, 579 y 590.
   - El script `empalmar_bloque_p1.R` comprueba antes de escribir:
     - el md5 del SCHOICE nuevo (línea 14);
     - el viejo, leído de git en `8efdf504` con su md5 (línea 43);
     - las líneas 2-561 del CLOZE, una a una (línea 46);
     - que cada renombre aparezca una sola vez (línea 25);
     - que no queden otros usos de `opciones`, `sol` ni `feedback`, con el analizador de R (líneas 32-36);
     - y es idempotente (línea 37).
   - Las 34 líneas de diferencia fuera del bloque son comentarios, dos U+00A0 literales pasados a escape (mismo valor), L-6 en la prosa de la Parte 1 y H3.
2. **Verificador.**
   - Mi corrida: exit 4, 368 cifras cubiertas, 0 fallos, 0 pendientes. La única diferencia con c1 es la línea de cierre.
   - **Región evaluada.** Los marcadores están en las líneas 471 y 654 de `verificar_espacio.R` (`031d4726…`), así que la región evaluada es la 471-653.
   - **Funciones libres de la región.** Usa `cif`, `sc`, `es_suma`, `filtra` y `atipica`, que se definen antes de la línea 471. Se toman del propio verificador del SCHOICE vía `QUIERO` (líneas 519-522 del CLOZE, con su `stopifnot`), así que es el mismo código. `ok`, `pendiente` y `firmado` se sustituyen a propósito.
   - **Campos.** Los que lee la región (`canon`, `clave`, `cod_gdis`, `gi`, `opciones`, `rol`, `sede_o`, `tabla`) existen en `rec()` y en `rec_p1()`.
   - Las 41 cifras salen iguales a lo firmado. MC41 y MC42 pasan por la misma puerta (`S2D$veredicto_s2d`) y reproducen M44 y M45: 58,8 → 72,2 % y 67,8 → 70,0 %; BE9 de 38,5 a 50,0 % y BE11 de 35,8 a 50,0 %.
   - `rec()` ya no lee `lado_gdis`, y MC40 lo comprueba en la semilla 1001.
   - Mutantes, cada uno cazado por la sonda que declara:

     | Mutante | Resultado |
     |---|---|
     | MP2 | +75,0 pp |
     | MC43 | margen en palabras de 6,5 % a 6,7 % |
     | MC49 | 283/330; fallan exactamente las 47 versiones con nombres de dos palabras |
     | MC44 | una línea con literal |
     | MC45, MC46, MC47 | aditivos: +100 en su sonda, el resto quieto |
     | MC48 | control 600 → 500 |

   - Los controles exactos (6 unidades dentro de la fórmula y 3 con signo por versión) coinciden con la cuenta que hice a mano sobre el código: 3 + 3 y 2 + 1.
3. **Corrección, con mi propio script.**
   - **Muestra.** `xexams()` con `seed = s`, funciones de verdad propias sobre el texto emitido y las tablas leídas del Markdown. Fueron 111 versiones: las semillas 1-100 más 11 canónicas de cuota (122 a 205), 20 canónicas en total. Son las mismas versiones de E1 (111 de 111).
   - **Parte 1:** una sola verdadera, y es la marcada.
   - **Parte 2:** una sola clave. Los distractores «luego entre dos» y «al cuadrado» son falsos con sus dos lecturas cada uno.
   - **Partes 3 a 5:** valores exactos.
   - **Parte 6:** las marcas coinciden con mi función de verdad, con al menos una verdadera y una falsa.
   - **Estructura:** `##ANSWER1##` a `##ANSWER6##` en orden, `clozetype` correcto y `exsolution` igual a la verdad en cada parte, después de la mezcla.
   - **Solution:** «Respuestas» (Partes 1 a 6), el «Ganó …» de la Parte 1 con su frase de rama, «solo una cita la diferencia correcta» (es una sola opción de G con *D*) y H3.
   - **Aritmética:** 2 826 igualdades en la Solution y el feedback, 0 falsas.
   - **Feedback:** «Verdadero.»/«Falso.» alineados con la marca en los 3 gaps de elección (15 entradas de un lado y 15 del otro).
   - **Canónica, 20 de 20:** enunciado, pregunta, encabezados, filas y las 4 opciones iguales al impreso y a `ITEM_OFICIAL.md` (con U+00A0 leído como espacio), y la clave es el contenido de la B. Lo cotejé también contra el recorte.
   - La primera corrida dio 20 fallos de mi propio oráculo, que no reconocía la A del impreso. Corregido el oráculo, 0.
   - **Residuo firmado de «O-1 corregido».** La lectura «total» vuelve verdadero a ERR-TOTAL en 22 de las 91 versiones no canónicas (24,2 %; con N = 91, el intervalo de 95 % es de ±8,8 pp). Es compatible con la exposición exacta firmada, 21,8 % y 20,0 %. No es un hallazgo.
4. **MC39.** Prueba la cláusula «o que pase de su cifra»: la sonda directa la aísla, y la de texto la ejercita, con el matiz de H2.
5. **F-3 y L-8.**
   - F-3: (93,0 − 25) × 3/8 = 25,5 pp. La cuenta es correcta, y RS7 sigue atado a 93,0 % en `FIRMA_S2D`, así que una subida quedaría pendiente. E7 sigue en verde.
   - L-8: está declarada como pendiente de cablear (línea 650 del verificador y el registro de la tanda C1). Queda un detalle de redacción: la decisión 3 dice que «el verificador del CLOZE las comprueba», y para L-8 no puede. La guarda real que existe es indirecta: si cambia el pool de G, se mueven las 13 cifras de «O-1 corregido» o las 41 de la S2d, y E1 lo vigila.
6. **Dominios:**
   - Tildes correctas en el texto nuevo.
   - Regla #19: la Solution no nombra ninguna letra.
   - Regla #20: la guarda `\newcounter{none}` está.
   - Regla #25: sin glifos que rompan pdflatex.
   - Ninguna CRÍTICA ni ALTA en código, pedagogía o semántica.

## No auditado

- No hice render propio de PDF, DOCX, NOPS ni Moodle: para los formatos me apoyo en el arsenal y en la 6b.
- No corrí `h2_exacta()` con el mutante de MC39: lo de H2 está deducido de los conjuntos de textos.
- No miré a ojo las capturas de la 6b.
- No medí las combinaciones de S_U con el barrido o con la lectura (iii), que la S2c declaró sin medir. Por eso el +25,5 pp de F-3 es una cota inferior.

## Veredicto global

**APROBAR_CON_CAMBIOS.** No hay CORRECCIÓN, CRÍTICA ni ALTA. H1 es opcional y cambia el `.Rmd`, así que exige reapertura. H2 toca solo el verificador. H3 es una condición para el próximo paso 8.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd:1058 | "si y solo si su promedio por curso, ", dp$aP, ED, "kilos, es mayor que la diferencia, "
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd:1059 | habría perdido con un curso menos si y solo si su promedio
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/salida/ciclo_20261102/6b_c1/informe_6b_c1_27f789dc_completo.md:54 | «diferencia.» queda sola en el último renglón en 99/100 semillas
H1 | /tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/358ab328-e44a-43ce-99ab-b2fa5570d4a9/scratchpad/correccion_r4b_v2.txt:5 | ver=111 · can=20 · tot_alt=22 · h3_falsa_emb=70
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/verificar_espacio_cloze.R:1208 | que el que perdió hubiera tenido, conservando su
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/verificar_espacio_cloze.R:1210 | HM <- h2_exacta(m39$txt, 1L)
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/verificar_espacio_cloze.R:1216 | else if (nrow(j39)) "cazado_por_su_sonda" else if (any(VM$veredicto == "pendiente")) "cazado_por_otra"
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/bateria_partes_2_a_6.R:149 | .pal <- function(s) tolower(regmatches(s, gregexpr("[A-Za-zÀ-ÿ]+", s))[[1]])
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/firma_p6_palabras_f98f9dda.tsv:51 | P6-V «cursos habría perdido»c
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/salida/coherencias_r9/verificar_coherencias.R:360 | exp1 <- sprintf("Verdadero. %s recogió $%d \\times %d = %d$ kilos y %s
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/salida/coherencias_r9/verificar_coherencias.R:398 | fila_p1 <- c(sprintf("- %s: $%d \\times %d = %d$ kilos."
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/salida/coherencias_r9/verificar_coherencias.R:403 | ganol1 <- sprintf("Ganó %s, con $%d - %d = %d$ kilos más. "
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/salida/coherencias_r9/verificar_coherencias.R:414 | habría ganado con un curso más solo si su promedio por curso, %d kilos, fuera mayor que la diferencia
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/salida/coherencias_r9/verificar_coherencias.R:610 | saveRDS(list(sem = sem, res = res, glob = glob), "resultados_verificacion.rds")
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd:1029 | "\\ \\text{kilos.}$\n", sep = "")
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
