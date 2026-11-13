# Auditoría Detractor, FASE 2C ronda 2: `serie-seno-aproximacion-n3` (SCHOICE)

**Fecha:** 2026-11-13 · **Artefacto:** md5 `faaed5306a95cc7519e627d38e4c8ef9` (verificado) · **Detractor:** independiente y nuevo. No escribí ni corregí nada de este ejercicio y no audité la ronda 1.

**Veredicto: RECHAZAR.** No hay ningún problema de clave: es correcta en las 208 configuraciones del espacio y no hay segunda clave. Rechazo por dos motivos. Primero, una frase de la Comprobación es falsa en su lectura literal en el 50 % de las versiones (H1). Segundo, la batería del autor no tiene sonda para los canales que crea la trampa de la calculadora, así que el residuo que se iba a presentar al profesor se queda corto en unos 50 pp (H2, H3).

**Objeciones:** 8 en total: 3 altas, 3 medias y 2 bajas.

**Método.** Enumeré el espacio de forma exacta ejecutando el chunk `data_generation` real. Reemplacé `sample.int` en el entorno de evaluación y forcé las cuatro llamadas que deciden la versión: x, n, el reparto `b` y el par `cand`.
- Salen 208 configuraciones cuyos pesos de muestreo suman 1. Ninguna guarda falla.
- Medí la batería oficial (`bateria_eliminacion.R`) de dos maneras: con enumeración exacta pesada, por rama y en agregado, y con N = 100 (semillas 1 a 100, las mismas que el autor).
- Usé las 31 reglas del autor más 10 mías.
- Hice dos renders PDF de control y medí una variante de diseño.

Los scripts están en `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/7c84a904-6e00-4c09-92a6-8c24851b1e89/scratchpad/detractor_r2/` (`enum.R`, `bat.R`, `plaus.R`, `variante.R`, `r12.R`, `r_neg.R`).

## 1. Corrección (enumeración exacta, 208 configuraciones)

| Comprobación | Resultado |
|---|---|
| La clave es la suma parcial exacta, recalculada aparte | 208/208 |
| Segunda clave u opciones duplicadas | 0 |
| SS-C = `round(sen(x)·D)`, estrictamente la fracción k/D más cercana (\|SS-C/D − sen x\| < 0,5/D) y distinta de la clave | 208/208 |
| Cota de la serie alternante: \|S_n − sen x\| < \|t_{n+1}\| | 208/208 |
| La diferencia impresa es menor que la cota impresa | 8/8 ramas: 0,024 < 0,0254 · 0,00136 < 0,00141 · 0,384 < 0,434 · 0,05 < 0,0542 |
| Texto y valor de SS-F y SS-N; signos de A, B, D, I y N frente al cálculo mostrado | Coherentes. Las guardas de las líneas 101-102 y 250-265 no fallan en ninguna configuración |
| Explicación de la calculadora (Comprobación y diagnóstico SS-C) | Verdadera en las 8 ramas: sen(x) y la trampa coinciden a 3 o 4 decimales |
| **«a partir del segundo término, cada término es menor que el anterior»** | **Falsa en su lectura literal con \|x\| = 3: 50 % del peso (H1)** |

## 2. Estado de los hallazgos de la ronda 1

| R1 | Estado |
|---|---|
| H1 (valor de SS-F) | **Resuelto.** Usa `ult_tex_signo`, tiene guarda en la línea 101 y el mutante M1 la hace caer. |
| H2 (signos del texto frente al cálculo) | **Resuelto.** `l0` muestra los signos de la fórmula, con la guarda `dice` y el mutante M2. |
| H3 (batería y residuo) | **Parcial.** Se volvió a medir sobre el banco real y con todas las reglas (§P7-C cumplido), pero la batería sigue sin sonda para la trampa (ver H2 de este reporte) y no hay override cableado. |
| H4 (redondeo de la Comprobación) | **Resuelto.** Tres cifras, guarda en la línea 331 y mutante M3. |
| H5 (causa de SS-G) | **Resuelto.** La causa ahora dice «desliz de cálculo en un solo término». |
| H6 (rama n = 2) | **Resuelto** por D3. |
| H7 (pruebas persistentes) | **Parcial.** Hay 4 mutantes, pero sobre 600 semillas y no sobre la enumeración, y nada en `tests/`. La guarda de «a partir del segundo término» comprueba otra proposición (H1). |
| H8 (guion en lugar de signo menos) | **Resuelto** con `decm`. |

## Objeciones

### H1. La Comprobación afirma algo falso en el 50 % de las versiones (ALTA, CORRECCIÓN por la regla #13 v1.1, «toda lectura razonable cuenta»)

**Qué se cuestiona.** Las líneas 379-381 dicen: «es menor que el valor absoluto del primer término que se dejó por fuera …: a partir del segundo término, cada término es menor que el anterior en valor absoluto».

**Evidencia.** Leída literalmente, la frase incluye que el 2.º término es menor que el 1.º. Con x = ±3, en las ramas que suman el 50 % del peso, |t₂| = 27/6 = 4,5 y |t₁| = 3. El estudiante lo ve en la misma Solution, porque el Procedimiento muestra `3 − 27/6 + …`.

La guarda de las líneas 262-263, `all(diff(abs(terminos[-1])) < 0)`, compara desde t₃ < t₂. Comprueba la lectura del autor, no lo que dice el texto. La conclusión (la cota) es verdadera en el 100 % de las versiones; por eso la califico ALTA y no CRÍTICA. Aun así, la regla #9 trata una Solution falsa como CORRECCIÓN, y eso es binario.

**Propuesta.**
- Reescribir: «a partir del primer término que se dejó por fuera, cada término es menor que el anterior en valor absoluto y los signos se alternan, así que el error nunca supera ese término». Esta es además la condición que usa la cota de Leibniz.
- Guarda: términos k = n…n+5 estrictamente decrecientes en valor absoluto. Se cumple, porque x²/((2k+2)(2k+3)) ≤ 9/72 cuando k ≥ 3.
- Mutante: llevar la guarda a `terminos` completo. Debe caer con x = ±3.

### H2. La batería del autor no tiene sonda para la trampa: el exceso real es +67,3 pp, no +17,9 pp (ALTA, DIAGNOSTICIDAD; residuo estructural de D1 sin firma)

**Evidencia, N = 100 (semillas 1 a 100, `evaluar_bateria`, 41 reglas).**
- BLOQUEA. El máximo es 100 % con `est_2a_cerca_sen` (la segunda opción más cercana a sen(x)).
- Techo nulo 32,7 % (p95 37,0 %). **Exceso +67,3 pp**; exceso atómico +75,0 pp.
- `salida/p9_bateria_p7.txt` da +17,9 pp, porque su única regla sobre la trampa es `est_no_cerca_sen` (`d != min(d)`), que reparte el acierto entre 3 opciones.

**Enumeración exacta (agregado / peor rama / mejor rama, en pp sobre 25 %):**

| Regla | Agregado | Peor | Mejor | Por qué es estructural bajo D1 |
|---|---|---|---|---|
| `est_2a_cerca_sen` | **+75,0** | +75,0 | +75,0 | La trampa es siempre la más cercana; la clave queda segunda en las 208 configuraciones |
| `rel_par_lejos_sen` (del par más próximo, la más lejana de sen x) | **+75,0** | +75,0 | +75,0 | El par {clave, trampa} es siempre el más próximo |
| `rel_par_mas_cercano` | +25,0 | +25,0 | +25,0 | Mismo motivo |
| `rel_par_mayor_abs` / `rel_par_menor_abs` | +25,0 | +75 (n = 3 / n = 4) | −25 | Con n impar la suma alternante se pasa de sen x; con n par se queda corta |
| `est_cerca_S2` | +25,0 | +75 (n = 4) | −25 | Orden de las sumas parciales: S₂ < S₄ < sen x |

**En palabras del aula, para el profesor.**
- Quien usa la calculadora y sabe que la opción más parecida a la calculadora es la trampa acierta siempre eligiendo la segunda más parecida.
- Como la trampa está en el 100 % de las versiones, en Moodle esto se aprende después de ver dos o tres versiones.

**Riesgo.** La línea 17 del `.Rmd` dice que los canales se declararon «regla por regla … con las cifras de salida/p9_bateria_p7.txt». Esas cifras no incluyen los tres canales mayores. Además `verificar_bateria_p7.R` no tiene alcance firmado cableado («rojo cubierto»): sale con BLOQUEA (exit 1). La §P7-E exige firma regla por regla con su cifra exacta y un mutante que saque la versión de «cubierto».

**Propuesta.** No hay rediseño dentro de D1. Cualquier distractor más cercano a sen(x) que la clave tendría que salir de un error real, y ninguno del banco lo hace. Por eso propongo:
1. Congelar la batería ampliada con estas reglas.
2. Pedir la firma de cada regla que pasa de +8 pp, con las cifras de esta tabla y de H3 y H4.
3. Cablear ese alcance en el verificador.

### H3. D2 no se cumple con la regla compuesta «del par más próximo, la que se simplifica» (ALTA, DIAGNOSTICIDAD; conflicto entre D1 y D2 que debe decidir el profesor)

**Evidencia.**
- `rel_par_reducible` da **+62,5 pp en agregado y +75 en 6 de las 8 ramas**; en x = ±3, n = 4 da +25.
- La clave se simplifica en las 8 ramas: 112/120, 4576/5040, 63/120, 459/5040.
- La trampa es irreducible en 6 de las 8: 109/120, 4583/5040 y 17/120. Solo 711/5040 se simplifica.
- La regla no necesita calculadora: «las dos opciones parecidas, y de ellas la que se simplifica».
- Las reglas aisladas sí cumplen D2: `div_irreducible` −19,8, `div_reducible` +6,2 y `div_termina_en_1` −5,2 (N = 100: tasas 5,2 %, 31,6 % y 19,8 %).

**Por qué es estructural.** El par lo fija D1, los numeradores los fijan x y n, y las fracciones van sin simplificar. Ningún cambio en la elección de los dos distractores del banco lo toca.

**Propuesta.** Contárselo al profesor tal cual: la regla «la que se simplifica» por sí sola no delata la clave, pero combinada con «las dos opciones parecidas» la delata en el 75 % de las versiones. Las salidas son firmarla o revisar el conjunto de x. Las dos son decisiones suyas.

### H4. Canales de magnitud y signo que dependen del diseño, y distractores muy fuera de rango (MEDIA, DIAGNOSTICIDAD y plausibilidad)

**Evidencia (enumeración exacta).**
- **Rango:** 93,6 % de las versiones tienen al menos un distractor con |v| > 1. En el 25 % hay dos, de modo que en [−1, 1] solo quedan la clave y la trampa. En el 32,8 % hay alguno con |v| > 10; el máximo es −1359936/5040 ≈ −269,8 (SS-E, x = ±3, n = 4).
- **Cascada:** la de la línea 224 no se cumple en 1/3 de las ramas x = ±2, n = 3 y x = ±3 (25 % del peso total).
- **Reglas por encima de +8 pp en agregado (agregado / peor rama):**
  - `est_rango_seno` +12,0 / +13,9;
  - `est_rango_no_cerca_sen` +36,4 / +41,7;
  - `est_rango_reducible` +29,6 / +41,7;
  - `est_rango_mayor_abs` +25,0 / +75;
  - `est_rango_menor_abs` +13,5 / +75;
  - `est_entre_S2_y_x` +16,8 / +25;
  - `est_cerca_x` +15,0 / +55;
  - `est_signo_x` +9,5 / +11,1.

**Variante medida (V1: sortear `b` solo entre repartos con un distractor en rango; vector completo en `variante.R`).**
- Baja `est_rango_no_cerca_sen` a +23,6 y `est_rango_reducible` a +19,6.
- Abre `rel_cerca_media`: de +6,4 a +18,9.
- Sube `est_cerca_x` de +15,0 a +20,0, con peor rama de +55 a +65, y la peor rama de `est_signo_x` pasa a +16,7.
- El máximo se queda en +75.

Es decir, desplaza el canal y no lo cierra. **No la recomiendo.**

**Propuesta.** Incluir estas reglas, con sus cifras, en la lista de firma de H2. Para la plausibilidad, decirle al profesor que con x = 3 y cuatro términos algunos distractores valen cientos. Un estudiante que sabe que |sen x| ≤ 1 los descarta sin hacer ningún cálculo.

### H5. Las ecuaciones de la Solution se salen del margen en el PDF (MEDIA, visual, reglas #6 y #26)

**Evidencia.** Con `exams2pdf`, semilla 213 (x = −3, n = 4, SS-A y SS-E), el log de pdflatex da `Overfull \hbox (28.05pt)` y `(48.05pt)`. Lo comprobé a ojo en la página 2: las dos cadenas pasan del margen derecho. La semilla 12 (x = 3, sin paréntesis) no desborda.

La causa está en la línea 314: las cuatro formas `l0 = l1 = l2 = total` van en un solo `$$…$$`, y `overflow-x` solo protege el HTML.

**Propuesta.** En los distractores, partir la cadena en dos displays: `l0 = l1` y `= l2 = total`. Luego volver a medir los Overfull sobre la enumeración, ramas x = −3 y x = −2 con n = 4, usando la plantilla de oficio.

### H6. Documentación desactualizada respecto al diseño r2 (MEDIA, documentación)

- **`ITEM_OFICIAL.md`, § «Regla de uso» (líneas 113-118).** Sigue describiendo el diseño anterior: canónica con probabilidad 0,10, x entre 1 y −1, n entre dos, tres y cuatro, un «banco de diez errores» sin SS-C y `enumerar_diseno.R`. Es el hallazgo de documentación que anticipó el coordinador.
- **`HANDOFF.md`.** Cita el md5 `eafc5840…` y dice que «La ronda 2 no se ha hecho».
- **`salida/render_4_formatos.R`.** Sigue llamando «canónica» a `html_canonica1.html` (semilla con `runif < 0,10`), que ya no lo es.
- **Línea 17 del `.Rmd`.** Afirma una declaración que no se ha hecho con las cifras correctas (H2).
- **Para declarar en el paso 9.** El `WARN_DIV_BAJA` (8 claves) es consecuencia directa de D1 y D3: con el mismo x y el mismo n, todos los estudiantes tienen la misma fracción correcta.

**Propuesta.** Reescribir la «Regla de uso» y el HANDOFF con el diseño r2: 8 ramas, SS-C siempre presente, política P2 y sin instancia impresa. Renombrar la salida del render.

### H7. Restos de código del diseño anterior (BAJA, código)

- `es_canonica <- FALSE` (línea 25) no se usa en el `.Rmd`; solo lo lee el verificador externo.
- El campo `ficha` (líneas 107-108 y 115) ya no lo lee nadie.

**Propuesta.** Quitarlos, o dejar `es_canonica` con un comentario que diga que lo consume `verificar_bateria_p7.R`.

### H8. Redacción (BAJA, gramática y precisión)

- «hayan hecho un cálculo que calcula …» (línea 302 combinada con las líneas 118 y 168) aparece con SS-C, SS-G y SS-O. Propuesta: «hayan hecho un cálculo en el que calculan …», o empezar `descripcion_corta` con «toma».
- El feedback de SS-C (línea 124), «Es el valor del seno … escrito con denominador D», es exacto solo de forma aproximada. Propuesta: «Es la fracción con denominador D más cercana al valor del seno que da la calculadora».

## Revisado sin objeciones

- **F1:** no hay `while` ni `repeat`; `combn` es finito y no hay `set.seed`. La stresstest da 100/100 con un máximo de 0,30 s.
- **Validaciones automáticas:** regla #19 (FASE 2J OK), glifos (2O OK), ortografía (2D OK) y multisemilla 100/100 (2G), en mi propio render.
- **Literalidad de `exextra`:** 0 fallos (`p10_literalidad.txt`).
- **Nivel y DOK:** Nivel 3 con DOK 2. La Solution nombra los dos obstáculos sin inflar el DOK.
- **Enunciado:** coincide con el del ítem salvo x y n, y la serie se muestra con tantos términos como se usan.
- **SS-C:** `precondicion` y `calcula()` son puras.
- **Plausibilidad pedagógica de SS-C:** «elegir la opción que coincide con la calculadora» es una conducta natural, y la Solution la explica sin afirmaciones falsas.

## Próximos pasos

1. **H1:** reescribir la frase, con su guarda y su mutante. Es lo que obliga a RECHAZAR.
2. **H2 a H4:** presentar al profesor, en lenguaje llano, la lista completa de reglas por encima de +8 pp con su cifra exacta (agregado y peor rama). Pedir la firma regla por regla y la decisión sobre el conflicto entre D1 y D2. Cablear el alcance firmado en `verificar_bateria_p7.R`.
3. **H5 a H8:** corregir.
4. **FASE 2C ronda 3** con un detractor nuevo.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:380 | segundo término, cada término es menor que el anterior en valor absoluto
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:262 | Solution: desde el segundo término, cada término es menor que el anterior
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:263 | all(diff(abs(terminos[-1])) < 0)
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/salida/verificar_bateria_p7.R:45 | nueva_regla("est_no_cerca_sen", "magnitud"
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/salida/verificar_bateria_p7.R | AUSENTE: rojo cubierto
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:17 | canales por encima de +8 pp declarados al profesor regla por regla
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/salida/enumeracion_diseno_r2.txt:1 | (0.9083) red=FALSE
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:104 | num_trampa <- round(sin(x) * D)
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:224 | cand <- which(b_par == b & rango1)
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:226 | if (!cascada_rango_cumplida) cand <- which(b_par == b)
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:314 | eq_display(paste(unique(c(cc$l0, cc$l1, cc$l2, frs(cc$total, D))), collapse = " = "))
H6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/ITEM_OFICIAL.md:113 | (probabilidad 0,10) reproduce el enunciado de la plana
H6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/ITEM_OFICIAL.md:115 | el número de términos entre dos, tres y cuatro
H6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/HANDOFF.md:4 | La ronda 2 no se ha hecho.
H6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/HANDOFF.md:8 | eafc5840e02095345458ea1347238a51
H6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/salida/render_4_formatos.R:6 | name = "html_canonica", seed = s_canon
H7 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:25 | es_canonica <- FALSE
H7 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:108 | list(codigo = codigo, nombre = nombre, ficha = ficha,
H8 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:302 | hayan hecho un cálculo que ", e$descripcion_corta
H8 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:168 | calcula $3!$ como $2
H8 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:124 | Es el valor del seno que da la calculadora, escrito con denominador
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: RECHAZAR