# Auditoría Detractor, FASE 2C ronda 1: `serie-seno-aproximacion-n3` (SCHOICE)

**Veredicto: RECHAZAR.** La clave es correcta en todas las versiones, pero la Solution dice algo falso en el 27,5 % de ellas.

**Fecha:** 2026-11-13 · **Detractor:** independiente (no escribí ni corregí nada de este ejercicio).
**Objeciones:** 8 en total: 1 crítica, 2 altas, 3 medias y 2 bajas.
**Método:** enumeré todo el espacio de diseño ejecutando el propio chunk `data_generation` del `.Rmd`, forzando cada rama y cada terna. Son 501 configuraciones: la canónica más las 6 ramas, con 10, 120 y 120 ternas por valor de n. Además medí la batería oficial (`bateria_eliminacion.R`) con N = 100, semillas 1 a 100. Mis scripts están en `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/7c84a904-6e00-4c09-92a6-8c24851b1e89/scratchpad/detractor_r1/` (`enum.R`, `textos.R`, `bateria.R`, `n100.R`, `alt.R`).

## Resumen

**Lo que está bien:**
- **La clave es la suma parcial correcta en las 501 configuraciones.** Ninguna guarda `stopifnot` falla.
- **No hay segunda clave.** Los distractores son distintos entre sí.
- **Otras afirmaciones de la Solution se cumplen en todas las versiones:**
  - la diferencia exacta de SS-F (1/D) y de SS-N (2/D);
  - la cota de la serie alternante.
- **La canónica reproduce el recorte impreso:** el enunciado, la serie con tres términos y las fracciones 55, 185, 101 y 141 sobre 120. Lo comprobé mirando `orig_pregunta_17.png`.

**Lo que obliga a rechazar:**
- **La Solution afirma un valor falso en el 27,5 % de las versiones (H1).**
- **En la mitad x = −1 del espacio, el texto de cada error contradice el cálculo que se muestra justo debajo (H2).**
- **La batería oficial da BLOQUEA (H3):** +67,5 pp sobre el techo nulo.
  - Las cifras del residuo que declara el autor se midieron con un banco que no es el del `.Rmd`.
  - Hay otros nueve canales por encima de +8 pp que no se declaran.

## Objeciones

### H1. La Solution de SS-F da un valor falso al término omitido (CRÍTICA, CORRECCIÓN)

**Qué se cuestiona.** `descripcion_larga` de SS-F (líneas 143-145): «deja por fuera `$\dfrac{x^{k}}{k!}$`, que con `$x = X$` vale `$frs(ult_num, D)$`». Ahí `ult_num <- (-1)^ult * x^(2*ult+1)` (línea 96) es el término **con su signo de la serie**. En cambio, `ult_tex` (línea 94) muestra el término **sin signo**.

**Evidencia (enumeración exacta, `textos.R`).**
- SS-F aparece en el 36,5 % de las versiones. La afirmación es falsa en el **27,5 %**.
- Ocurre siempre que n es par:
  - en las ramas n = 2, en el 66,7 % de sus versiones;
  - en las ramas n = 4, en el 25 %;
  - con n = 3 y en la canónica, nunca.
- Texto real, rama x = 1, n = 2: «deja por fuera $\dfrac{x^{3}}{3!}$, que con $x = 1$ vale $-\dfrac{1}{6}$». Pero x³/3! con x = 1 vale **+1/6**.
- Texto real, rama x = −1, n = 4: «$\dfrac{x^{7}}{7!}$, que con $x = -1$ vale $\dfrac{1}{5040}$». Su valor real es **−1/5040**.

**Por qué ningún validador lo vio.** La guarda de la línea 239 comprueba que la diferencia con la clave es exactamente 1/D. No comprueba el valor que el texto escribe.

**Riesgo.** Por la regla #9, una Solution falsa es CORRECCIÓN: falla binaria y RECHAZAR.

**Propuesta.**
1. Escribir el término con su signo: `"deja por fuera el término $", if (ult_signo_mas) "+" else "-", ult_tex, "$, que con $x = ..."`. Así `frs(ult_num, D)` pasa a ser cierto en las 6 ramas.
2. O, al revés, escribir `frs(x^(2*ult+1), D)` y dejar el término sin signo.
3. En los dos casos, añadir una guarda que evalúe por separado el término tal como se muestra y lo compare con el valor del texto, con un mutante que la haga caer.

### H2. Con x = −1, el texto del error contradice el cálculo que se muestra (ALTA, coherencia texto–cálculo; roza CORRECCIÓN por la regla #13 v1.1)

**Qué se cuestiona.** `calculo()` (líneas 79-84) muestra los coeficientes **ya sustituidos** (`coef <- sgn(k) * p$x^(2*k+1)`) y se salta el paso con (−1)^k. Las descripciones, en cambio, hablan de los signos de la serie simbólica.

**Evidencia (rama x = −1, n = 3).** Los textos reales son:
- **SS-D:** «escribe un signo más entre todos los términos:», y debajo aparece `$$-1 - \dfrac{1}{3!} - \dfrac{1}{5!} = …$$`, con todos los signos menos.
- **SS-A:** «resta todos los términos después del primero:», y debajo aparece `-1 + \dfrac{1}{2!} + \dfrac{1}{4!}`, con todos los signos más.
- **SS-B, SS-I y SS-N:** pasa lo mismo.

Afecta al **42,2 %** de las versiones: las de x = −1 que muestran alguno de los errores A, B, D, I o N. La lectura razonable de «un cálculo que escribe un signo más entre todos los términos: [cálculo]» es que el cálculo mostrado tiene signos más. Con esa lectura, el texto es falso.

**Propuesta.** Anteponer a cada distractor la línea de sustitución, como ya hace el procedimiento correcto con `sustitucion`. Por ejemplo, para SS-D: `(-1) + \dfrac{(-1)^3}{3!} + \dfrac{(-1)^5}{5!} = -1 - \dfrac{1}{3!} - \dfrac{1}{5!} = …`. Así queda a la vista el signo que describe el texto. Añadir una guarda que compare los signos de esa línea con `sgn(k)` de cada error.

### H3. La batería da BLOQUEA y el residuo declarado no se sostiene como está (ALTA, DIAGNOSTICIDAD)

**a) Las cifras del autor salen de otro banco.**
- `salida/enumeracion_diseno_final.txt` (línea 4) enumera A, B, D, E, F, N, I, H y O para la rama x = 1, n = 3. **Falta SS-G = 61**, que sí está en el `.Rmd` (línea 149).
- Esa enumeración tampoco incluye la canónica (P = 0,10).
- Como 61 es irreducible y termina en 1, el autor **sobrestima** los dos canales de divisibilidad:

| Regla | Autor, agregado / peor rama | Real, agregado / peor rama |
|---|---|---|
| `div_irreducible` | +48,5 / +60,0 pp | +42,6 / +46,3 pp |
| `div_termina1` | +45,0 / +75,0 pp | +33,3 / +56,2 pp |

**b) Batería oficial, N = 100.** Corrí 35 reglas que cubren las seis familias más la relacional y la del estímulo; la léxica se declara no aplicable porque las opciones son fracciones sin palabras.
- Veredicto **BLOQUEA**.
- Máximo 100 % con `est_cerca_sen`. Techo nulo 32,5 %. **Exceso +67,5 pp.**

**c) Canales por encima de +8 pp con enumeración exacta (agregado / peor rama / canónica):**

| Regla | Agregado | Peor rama | Canónica | ¿Declarado? |
|---|---|---|---|---|
| `est_cerca_sen` | +75,0 | +75,0 | +75 | sí |
| `est_cerca_S2` (la más cercana a x − x³/6) | +66,0 | +75,0 | +75 | **no** |
| `est_entre_S2_y_x` | +43,9 | +43,9 | +75 | **no** |
| `div_irreducible` | +42,6 | +46,3 | +75 | sí |
| `div_termina1` | +33,3 | +56,2 | +25 | sí |
| `est_cerca_x` | +23,1 | +25,0 | +75 | **no** |
| `div_termina1o9` | +19,0 | +45,1 | +25 | **no** |
| `fmt_propia` / `est_seno_acotado` | +19,0 / +12,6 | +36,1 / +14,8 | +25 | **no** |
| `div_mult5` | +13,9 | **+75 (n = 2)** | −25 | **no** |
| `div_no_mult3` | +13,4 | +24,1 | +8,3 | **no** |
| `rel_par_cercano` / `rel_media` | +11,2 / +11,2 | +14,6 / +13,9 | +25 / +75 | **no** |
| `mag_2a_menor` | +10,0 | +8,3 | +75 | **no** |

Sobre la posición: con N = 100 y semillas reales, la primera da 28,0 % y la última 22,0 %. Es ruido; `exshuffle: TRUE` hace el resto.

**d) ¿Son estructurales? Solo en parte.**
- **`est_cerca_sen` y `est_cerca_S2` sí lo son.** Toda opción escrita sobre D difiere de la clave en al menos 1/D. El error de la clave es menor que 1/(2n+1)!. Y en una serie alternante, la suma S_n queda entre S_{n−1} y S_{n−2}. Lo medí también con x = 2: `est_cerca_sen` sigue en +75 en las tres n. El ítem impreso lo tiene al 100 %.
- **La divisibilidad no es estructural del tipo de ítem: depende de |x| = 1.** Con |x| = 1, la clave es congruente con ±1 módulo (2n−1)(2n−2). Lo medí con x = 2 y el mismo banco: `div_irreducible` +0,0 y `div_termina1` +0,0 en las tres n, porque todos los numeradores son pares.
  - El precio: x = 2 abre el canal de signo. En x = 2, n = 3, SS-A, SS-G y SS-H son negativos con la clave positiva.
  - También habría que generalizar los textos y guardas de SS-F y SS-N y la cota.
  - Una alternativa más barata es una cascada que fuerce distractores irreducibles: G e I con n = 3; D, G, I y N con n = 4. Solo reduce a la mitad: con un señuelo, +75 pasa a +25 en esa versión.

**Riesgo.** Por la §P7-E, aceptar un canal por encima de +8 pp es un **override humano firmado regla por regla, con sus cifras cableadas en el verificador**. Un comentario de residuo en la cabecera (línea 17) no cumple ese requisito. Además las cifras declaradas son incorrectas y la lista está incompleta.

**Propuesta.**
1. Volver a correr la enumeración sobre el banco final de 10 errores y con la canónica, usando `bateria_eliminacion.R`.
2. Pedir la firma de `est_cerca_sen`, `est_cerca_S2` y `est_entre`, que son estructurales, cada una con su cifra exacta.
3. Para los canales de divisibilidad y de `fmt_propia`, decidir con el profesor entre:
   - medir la rama x = ±2;
   - balancear cuántos distractores tienen |v| > D, igual que ya se balancea el rango de la clave;
   - firmarlos.
4. Valorar quitar la rama n = 2 o ampliar su banco. Tiene solo 5 errores y 10 ternas, `div_mult5` da +75 porque 5 es el único múltiplo de 5, y con un solo factorial (3!) el obstáculo de los factoriales impares queda reducido a un único caso.

### H4. El redondeo de la Comprobación contradice la cota en las versiones con n = 3, incluida la canónica (MEDIA)

**Qué se cuestiona.** `dec_sig()` usa `formatC(v, format = "fg", digits = 2)` (línea 59).
- Con n = 3 la diferencia es 0,0001957 y se imprime «cerca de 0,0002».
- El texto dice que es menor que 1/7! = 0,0001984. Un estudiante que lo compruebe ve 0,0002 > 0,000198.
- Afecta al 40 % de las versiones: la canónica más las ramas n = 3.
- Con n = 4 la diferencia es 0,0000027 frente a 0,0000028: cierto, pero muy justo.

**Propuesta.** Usar tres cifras significativas y escribir también la cota en decimal: «0,000196, menor que 1/5040 ≈ 0,000198».

### H5. La causa de fondo de SS-G no coincide con su cálculo (MEDIA, coherencia nombre–causa–cálculo)

- La causa dice «calcula el factorial sin incluir el propio número» (línea 155).
- El cálculo (línea 150) solo aplica eso a 3! y deja 5! = 120.
- Si el estudiante tuviera esa concepción de forma consistente, tomaría 5! = 4! = 24, y el resultado sería 65/120, que es el valor de SS-H.

**Propuesta.** Presentar SS-G como un desliz puntual: «omite el factor 3 al calcular 3!».

### H6. SS-F en la rama n = 2 (BAJA)

- La línea 294 produce `$$1 = \dfrac{6}{6} = \dfrac{6}{6}$$`, que es redundante.
- La opción «6/6» (= 1) es poco plausible como respuesta escrita: quien usa un solo término escribe 1.

**Propuesta.** Omitir la segunda igualdad cuando l2 = total, o retirar n = 2 (ver H3).

### H7. Faltan pruebas persistentes y guardas sobre los textos (MEDIA, testing)

- El `.Rmd` no tiene `test_that` y no hay suite en `tests/` que mencione el ejercicio.
- Las 13 guardas `stopifnot` comprueban valores, pero no los textos. H1 y H2 pasan todas las guardas.

**Propuesta.** Un verificador que:
- enumere las 501 configuraciones, como hace `enum.R`;
- compruebe la verdad del texto de SS-F y la concordancia entre las palabras de signo y los signos mostrados;
- incluya mutantes que lo hagan caer.

### H8. Decimal negativo con guion de texto en la Comprobación (BAJA)

En la línea 353, `dec(sin(x))` va fuera del modo matemático. El PDF muestra «-0,841471» con guion, mientras el resto de la Solution usa «−» matemático.

**Propuesta.** Meter el número en `$…$`.

## Revisado sin objeciones

- **`exextra`:** `validar_literalidad_exextra.py` da 0 fallos, y los valores coinciden con la ficha MAT-2026-07-017.
- **Nivel y DOK:** DOK 2 con Nivel 3 es coherente, y la Solution nombra los dos obstáculos sin inflar el DOK.
- **Regla #25 (glifos):** OK.
- **Ortografía:** OK.
- **Regla #19 (letras):** la Solution no usa letras. El diagnóstico va en el orden del banco, no en el de las opciones.
- **F1 (sin cuelgue):** solo hay bucles `for` acotados, y no hay `set.seed` con el reloj.
- **Regla #11:** 7 plantillas de contexto, ninguna con «registró».
- **Formatos:** revisé el texto de los 5 PDF, el NOPS y el DOCX; el `<span>` no aparece en DOCX ni en PDF.
- **Fidelidad canónica:** el enunciado, la serie y las cuatro fracciones coinciden con el recorte, y hay guardas literales que lo vigilan.

## Próximos pasos

1. H1 y H2: corregir el texto o la sustitución y añadir sus guardas con mutante. Esto bloquea.
2. H3: medir de nuevo sobre el banco real y la canónica, firmar los canales estructurales y decidir los de divisibilidad, `fmt_propia` y n = 2.
3. H4, H5, H6, H7 y H8.
4. Ronda 2 de la FASE 2C con un detractor nuevo.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:96 | ult_num  <- (-1)^ult * x^(2 * ult + 1)
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:144 | "$, que con $x = ", x, "$ vale $", frs(ult_num, D),
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:126 | usa los factoriales correctos, pero escribe un signo más entre todos los términos
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:81 | coef <- sgn(k) * p$x^(2 * k + 1)
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/salida/enumeracion_diseno_final.txt:4 | aplicables: A=55[in] B=185 D=141 E=104[in] F=100[in] N=99[in] I=139 H=65[in] O=108[in]
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:149 | "SS-G" = error_serie("SS-G", "Calcula mal 3!"
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:17 | Residuo de diagnosticidad declarado (no se persigue, regla #22 §P7-D/F)
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:59 | formatC(v, format = "fg", digits = 2)
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:354 | ". La diferencia, cerca de ", dec_sig(dif),
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:155 | calcula el factorial sin incluir el propio número.
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:150 | function(k) ifelse(k == 1, 2, factorial(2 * k + 1))
H6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:294 | eq_display(paste0(cc$l1, " = ", cc$l2, " = ", frs(cc$total, D)))
H7 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd | AUSENTE: test_that
H7 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:239 | "Solution SS-F: difiere de la clave en exactamente 1/D"
H8 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:353 | ")$ es aproximadamente ", dec(sin(x)), ", y $",
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: RECHAZAR