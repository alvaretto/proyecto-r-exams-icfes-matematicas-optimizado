# Auditoría del detractor, FASE 2C, ronda 3: `serie-seno-aproximacion-n3` (SCHOICE)

**Fecha:** 2026-11-13. **Artefacto:** md5 `c6468f300d38b1c079cf2539edb61e09` (comprobado). **Detractor:** independiente y nuevo. No escribí ni corregí nada de este ejercicio y no audité las rondas 1 ni 2.

**Veredicto: APROBAR_CON_CAMBIOS.** No encontré ningún fallo de corrección en las 208 configuraciones del espacio, enumeradas de forma exacta: la clave es siempre correcta, no hay segunda clave y SS-C es lo que dice ser. Todas las afirmaciones numéricas de la Solution son verdaderas, también la nueva frase de la Comprobación. El PDF ya no se sale del margen.

Hay que corregir tres cosas:
- La lista de reglas que se va a llevar a la firma del profesor está incompleta: faltan 11 de 26 reglas por encima de +8 pp (H1).
- La decisión 2 del profesor solo se cumple si se mide con la medida diluida (H2).
- Quedan dos frases imprecisas sobre la trampa de la calculadora (H3).

**Objeciones:** 5 (0 críticas, 0 altas, 3 medias, 2 bajas).

## Método

- **Enumeración exacta.** Ejecuté el chunk `data_generation` real con `sample.int` sustituido, forzando las cuatro llamadas que deciden la versión (x, n, el reparto `b` y el par `cand`). Salen 208 configuraciones, con pesos que suman 1, y ninguna guarda falla.
- **Comprobación independiente.** Recalculé el valor de cada opción a partir de su mecanismo, sin usar el banco (clave y SS-A … SS-O). También ejecuté el chunk `solucion` en las 208 configuraciones.
- **Batería exacta.** Usé las 40 reglas del autor más 6 reglas nuevas, con la semántica del helper (tasa sobre las versiones en que la regla aplica). Doy también la cifra diluida, que es la que usó la ronda 2.
- **PDF.** Rendericé las 208 configuraciones en un solo `exams2pdf` con la plantilla de oficio `perifericos/solpcielo.tex` (346 páginas) y con `plain` (485 páginas), y leí el log de pdflatex. Revisé a ojo las páginas 301-302 de la versión x = −3, n = 4 con SS-A y SS-E (numeradores de 7 cifras).
- **Scripts y salidas:** `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/7c84a904-6e00-4c09-92a6-8c24851b1e89/scratchpad/detractor_r3/` (`enum3.R`, `bat3.R`, `pdf3.R`, `cfg3.rds`, `textos.rds` y `pdf/`).

## 1. Corrección (208 configuraciones, sin fallos)

| Comprobación | Resultado |
|---|---|
| Valor de cada opción = su mecanismo, recalculado aparte | 208/208 |
| Una sola opción igual a la suma parcial; opciones duplicadas | 1 en todas; 0 duplicadas |
| SS-C = `round(sen(x)·D)`, estrictamente la fracción k/N! más cercana y distinta de la clave | 208/208 |
| Nueva frase de la Comprobación, «a partir del último término usado…» | Verdadera en las dos lecturas: la que excluye la comparación del último término usado con el penúltimo y la que la incluye |
| Cota de la serie alternante | 208/208 |
| Diferencia impresa menor que la cota impresa | 0,024 < 0,0254 · 0,00136 < 0,00141 · 0,384 < 0,434 · 0,05 < 0,0542 |
| «No son iguales» (sen(x) y la clave a 4 decimales) | Distintos en las 8 ramas |
| Diferencias que dicen los textos de SS-F y SS-N | Correctas en las 56 configuraciones de cada una |
| Signos de SS-A, SS-B, SS-D, SS-I y SS-N frente al cálculo mostrado | Coherentes (guardas sin fallo) |
| Explicación de la calculadora (decimales de sen(x) y de la trampa) | Correcta; véase H3 sobre la redacción |
| Textos con NA, Inf o notación científica | 0 |

**Decisiones del profesor implementadas:**
- x ∈ {2, 3, −2, −3}, sin la instancia impresa.
- SS-C presente en el 100 % de las versiones y definida como se pidió.
- n ∈ {3, 4}.
- 150 versiones únicas de 150. Ojo: esto mide el envoltorio; claves distintas hay 8, y está declarado.
- La decisión 2 no está implementada del todo: véase H2.

## 2. Estado de los hallazgos de la ronda 2

| Hallazgo de la ronda 2 | Estado |
|---|---|
| 1 (frase de la Comprobación) | **Resuelto.** Frase nueva, guarda en las líneas 264-265 y mutante 5 cazado. La guarda cubre solo una de las dos lecturas (H5, BAJA). |
| 2 a 4 (canales para la firma) | **Pendiente de firma**, como estaba previsto. Pero la lista que se iba a llevar al profesor está incompleta y mezcla dos medidas (H1, H2). |
| 5 (Overfull en el PDF) | **Resuelto.** Oficio: **0** `Overfull \hbox` en las 208 configuraciones, incluidas las 27 con x = −3, n = 4. Con `plain` hay 1, de 1,2 pt, en un párrafo de contexto (partición de palabras), no en una ecuación. A ojo, las páginas 301-302 están dentro del margen. |
| 6 (documentación) | **Resuelto en lo esencial.** «Regla de uso», HANDOFF y nombre del render al día. Quedan dos residuos (H4). |
| 7 (código muerto) | **Resuelto.** `es_canonica` y `ficha` llevan un comentario que dice quién los lee. |
| 8 (redacción) | **Resuelto a medias.** «hayan calculado … y buscado» y el feedback de SS-C están bien, pero la misma imprecisión sigue en otros dos sitios (H3). |

Arsenal post-render: 0 errores sobre este md5 (informe de las 09:50:42, posterior al `.Rmd` de las 09:50:12). El corrector ortográfico no encuentra nada. Literalidad de `exextra`: sin fallos. Los pasos 3 a 6 se sellaron en lote (con 60 ms de diferencia), pero cada artefacto es posterior al md5 vigente, así que el sello se sostiene.

## Objeciones

### H1. La lista para la firma está incompleta: hay 26 reglas por encima de +8 pp y la lista vigente tiene 15 (MEDIA, diagnosticidad)

**Qué se cuestiona.** El HANDOFF (línea 37) remite a `p9_bateria_p7.txt` (N = 100) y a los hallazgos 2 a 4 de la ronda 2. Esas fuentes tienen dos problemas:
- **Medida.** Las de N = 100 tienen error de muestreo. Las de la ronda 2 están diluidas: cuentan como 25 % las versiones en que la regla no aplica. La regla #22 pide firmar cada regla con su cifra exacta.
- **Cobertura.** No hay ninguna sonda del mecanismo que de verdad separa la clave de la trampa.

**Evidencia** (enumeración exacta, semántica del helper; «diluida» es la medida de la ronda 2):

| Regla | Aplica | Exceso | Diluida | Peor rama | ¿En la lista vigente? |
|---|---|---|---|---|---|
| `est_2a_cerca_sen` · `rel_par_lejos_sen` | 100 % | +75,0 | +75,0 | +75 | Sí |
| `rel_par_reducible` | 100 % | +62,5 | +62,5 | +75 | Sí |
| **`rel_par_mult_x`** (del par más próximo, la de numerador múltiplo de x) | 100 % | **+62,5** | +62,5 | +75 | **No (nueva)** |
| `est_rango_no_cerca_sen` · `est_rango_reducible` | 100 / 98,8 % | +36,4 / +29,9 | igual | +41,7 | Sí |
| `rel_par_mas_cercano`, `rel_par_mayor_abs`, `rel_par_menor_abs`, `est_rango_mayor_abs`, cercanía a x − x³/6 | 100 % | +25,0 | +25,0 | +75 (salvo la primera, +25) | Sí |
| «entre x − x³/6 y x» | 97,6 % | +17,2 | +16,8 | +25 | Sí |
| `est_signo_x` | 60,2 % | **+15,8** | +9,5 | +16,7 | Sí, pero con +9,5 |
| `est_cerca_x` · `est_rango_menor_abs` | 100 % | +15,0 / +13,5 | igual | +55 / +75 | Sí |
| **`est_abs_menor_x`** | 82 % | +13,4 | +11,0 | +15,5 | **No (nueva)** |
| `est_rango_seno` | 93,6 % | +12,8 | +12,0 | +14,6 | Sí |
| **`div_no_mult5`** | 36,9 % | **+12,7** | +4,7 | +13,2 | **No** (N = 100 daba +5,3) |
| **`mag_2a_menor` · `mag_2a_mayor` · `pos_centrales`** | 100 % | +8,3 | +8,3 | +8,3 | **No** |
| **`mag_menos_cifras`** | 82,5 % | +8,3 | +6,9 | +25 | **No** |
| **`div_reducible` · `est_no_cerca_sen`** | 75 / 100 % | +8,3 | +6,2 / +8,3 | +8,3 | **No** |
| **`div_mult_x` · `sig_mayoritario`** | 75 / 33 % | +8,3 | +6,2 / +2,8 | +8,3 | **No (nuevas)** |

**Lo que ninguna cifra vigente deja ver.** Cada término de la serie lleva x, así que la clave y los 10 errores del banco tienen siempre un numerador múltiplo de x. La trampa no lo es en el 75 % de las versiones (109, 4583 y 17; solo 711 lo es). `rel_par_mult_x` tiene exactamente el mismo perfil por rama que `rel_par_reducible` (75/75/75/25…): es el mecanismo de fondo de esa regla. Y, a diferencia de «la que se simplifica», es un razonamiento matemático natural que un buen estudiante puede usar sin calculadora.

**Por qué `pos_centrales` y `mag_2a_*` salen de la decisión 1.** La trampa queda siempre del mismo lado de la clave (n impar: la serie se pasa del seno; n par: se queda corta). Por eso la clave nunca es la opción extrema de ese lado. Su rango sale 1/6, 1/3, 1/3, 1/6, y descartar las dos extremas da +8,3.

**Propuesta.**
1. Congelar la batería con las 4 reglas nuevas (regla #22: si se amplía, se vuelve a medir todo).
2. Sacar **una sola tabla** de firma de la enumeración exacta, con la semántica del helper, la aplicabilidad y la peor rama.
3. Presentar `rel_par_mult_x` en palabras de aula: «la opción de la calculadora casi nunca es múltiplo del número; las demás sí».
4. Después de la firma, cablear el alcance en `verificar_bateria_p7.R`.

### H2. La decisión 2 («que las reglas de divisibilidad aisladas no señalen la clave») solo se cumple con la medida diluida (MEDIA, decisión del profesor no implementada del todo)

**Evidencia.** `ITEM_OFICIAL.md:134` afirma que «la que se simplifica» queda por debajo de +8 pp. Con la semántica del helper:
- `div_reducible` da +8,3 (aplica en el 75 %).
- `div_mult_x` da +8,3.
- `div_no_mult5` da +12,7, solo en las versiones con D = 120: la clave nunca es múltiplo de 5, y los errores que quitan o alteran el término de 5! sí lo son.

Diluidas, las tres quedan en +6,2, +6,2 y +4,7. Por eso la afirmación es verdadera con una medida y falsa con la otra. `div_no_mult5` es poco plausible como estrategia de un estudiante. `div_mult_x`, en cambio, es plausible (H1).

**Propuesta.**
- Corregir la línea 134 con las tres cifras y su medida.
- Llevar al profesor la elección: firmar estas reglas o cambiar la trampa. Una opción a enumerar **antes** de proponerla es la fracción con numerador múltiplo de x más cercana a sen(x): 110/120, 4582/5040, 18/120 y 711/5040. Ninguna coincide con la clave, pero no la medí. Además cambia la letra de la decisión 1 y ya no reproduce exactamente lo que hace el estudiante con la calculadora. Es decisión del profesor.

### H3. La Solution dice que la opción de la calculadora «es el valor del seno» con dos decimales distintos a la vista (MEDIA, precisión de la Solution, regla #13 v1.1)

**Evidencia.** Hay dos frases:
- `descripcion_larga` de SS-C (línea 122): «Ese es el valor del seno, no el de la serie…».
- El análisis (línea 392): «la otra es el valor del seno que da la calculadora».

En las ramas con n = 3 (50 % del peso), la frase anterior muestra «sen(2) ≈ 0,9093, y 109/120 ≈ 0,9083» y «sen(3) ≈ 0,1411, y 17/120 ≈ 0,1417». Leída al pie de la letra, la afirmación queda contradicha en la misma línea.

La ronda 2 corrigió esta misma imprecisión en el feedback (línea 126), pero no en estos dos sitios. No la califico de CORRECCIÓN: la frase anterior define la opción como «la fracción … más cercana a ese valor», y el contraste que se quiere hacer es entre dos cantidades, el seno y la serie. Aun así, es una lectura razonable que la regla #13 v1.1 obliga a cerrar.

**Propuesta.**
- Línea 122: «Esa fracción aproxima el valor del seno; no es el valor de la serie con sus primeros … términos, que es lo que pide la pregunta.»
- Línea 392: «la otra es la fracción más cercana al valor del seno que da la calculadora».

### H4. Dos residuos de documentación (BAJA)

- **«Rango de la clave balanceado».** Lo dicen `ITEM_OFICIAL.md:124` y el `.Rmd`, línea 13. El rango medido es 1/6, 1/3, 1/3, 1/6, y eso es lo que produce el +8,3 de `pos_centrales` (H1). Propuesta: «sorteado entre los repartos factibles; la clave nunca es la extrema del lado de la trampa».
- **Fuentes que no coinciden.** El `.Rmd` (línea 18) remite las cifras exactas a `enumeracion_diseno_r2.txt`. El HANDOFF (línea 37) las remite al reporte de la ronda 2. Ninguna de las dos tiene la lista completa. Propuesta: que los dos remitan a la tabla única de H1.

### H5. La guarda de la Comprobación cubre una sola lectura de la frase (BAJA, testing)

**Evidencia.** La guarda de la línea 265 empieza en `k = n − 1`. Cubre la lectura «cada término posterior al último usado es menor que el anterior». No cubre la lectura que también compara el último término usado con el penúltimo. Hoy las dos lecturas son verdaderas en todo el espacio (lo comprobé). Pero con n = 2 y x = ±3, la segunda lectura sería falsa (4,5 > 3) y la guarda seguiría pasando. Es una invariante sin guarda.

**Propuesta.** Usar `(n - 2L):(n + 5L)`, y añadir un mutante que permita n = 2 con x = 3 y que tenga que caer.

## Revisado sin objeciones

- Enunciado idéntico al impreso salvo x y n.
- Siete plantillas narrativas.
- Concordancia «dos/tres términos».
- Solution sin letras y sin enumerar las opciones por orden (FASE 2J correcta).
- Sin glifos problemáticos.
- Nivel 3 con DOK 2 y los obstáculos nombrados.
- Funciones puras y sin bucles sin cota.
- Exactitud de `3^9/9! = 19683/362880 ≈ 0,0542`.
- Propiedades del concepto, verdaderas en el espacio (y la cota también en general, por el resto de Lagrange).

## Próximos pasos

1. **H3 y H5:** dos cambios de texto y una guarda, con su mutante. No hacen falta otra pasada de diagnosticidad ni otra ronda de esta FASE 2C.
2. **H1 y H2:** tabla única de firma y la decisión del profesor sobre la divisibilidad. Corregir `ITEM_OFICIAL.md:134`.
3. **H4:** documentación.
4. Después, sellar los pasos 7 a 10 en orden. La pausa 3 sigue siendo del profesor.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/HANDOFF.md:37 | Cifras en `salida/p9_bateria_p7.txt` (N = 100) y en el
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/salida/verificar_bateria_p7.R | AUSENTE: rel_par_mult_x
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/ITEM_OFICIAL.md:134 | «la irreducible» y «la que se simplifica» quedan por debajo de +8 pp
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:122 | Ese es el valor del seno, no el de la serie con sus primeros
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:392 | la otra es el valor del seno que da la calculadora
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/ITEM_OFICIAL.md:124 | con el rango de la clave balanceado
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:13 | con el RANGO DE LA CLAVE balanceado
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:18 | salida/enumeracion_diseno_r2.txt (exacta)
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:265 | all(diff(abs(vapply((n - 1L):(n + 5L)
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:384 | último término usado, cada término es menor que el anterior en valor absoluto
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS