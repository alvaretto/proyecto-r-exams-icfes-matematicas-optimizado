# Auditoría del detractor, FASE 2C, ronda 4: `serie-seno-aproximacion-n3` (SCHOICE)

**Fecha:** 2026-11-13 · **md5 auditado:** `b310061eaac8404f72cd8a18fc56c54a` (comprobado) · **Hallazgos:** 6 (0 críticos, 0 altos, 3 medios, 3 bajos)

## Resumen

**La clave es correcta en todas las versiones.** Enumeré las 88 configuraciones ejecutando el chunk real y las contrasté con un oráculo independiente:
- La clave es correcta en las 88 configuraciones y no hay segunda clave.
- SS-C es la fracción k/D más cercana a sen(x).
- El tope de 4 se cumple en todas: el valor máximo es 3,6254.
- Toda igualdad y toda cifra de la Solution es verdadera en las 88.

Quedan tres hallazgos medios, ninguno de corrección:
- **H1:** el feedback de la clave desborda el ancho real de Moodle a 320 px.
- **H2:** la lista para la firma deja fuera reglas por encima de +8 pp, entre ellas la de «clave en un extremo».
- **H3:** una frase de SS-A es falsa en su lectura literal.

## 1. Corrección

**Método.** El script `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/7c84a904-6e00-4c09-92a6-8c24851b1e89/scratchpad/detractor_r4/enum.R` fuerza las cuatro decisiones del chunk `data_generation`. Da 88 configuraciones y los pesos suman 1. Después ejecuta el chunk `solucion` y lee su salida:
- Convierte cada `$$…$$` a una expresión numérica y la evalúa.
- Comprueba con fórmulas propias cada uno de los 10 errores del banco.

Además corrí 3000 semillas reales: 0 errores, las 88 combinaciones aparecen y ninguna queda fuera del enumerado.

**Resultado: 0 fallos.**
- **Clave:** exactamente una opción vale S_n, y es la marcada en `sol`.
- **Distractores:** cada uno vale lo que dice su error.
- **SS-C:** es `round(sen(x)·D)`, a menos de 0,5/D de sen(x).
- **Banco:** siempre quedan al menos 2 distractores, con |valor| ≤ 3,6254.
- **Cadenas de igualdades:** las 4 ecuaciones del Procedimiento, y cada «= …» de cada diagnóstico, valen la opción correspondiente. La partición en una igualdad por ecuación no rompió ninguna cadena.
- **Comprobación:**
  - Los decimales de sen(x) y de S_n son correctos a 4 cifras, incluido «$-$0,9093» para x negativo.
  - La diferencia está bien redondeada a 3 cifras significativas.
  - La cota |x|^(2n+1)/(2n+1)! está bien escrita.
  - La diferencia es menor que la cota, tanto en valor exacto como con las cifras impresas.
  - Los términos decrecen desde el último usado, comprobado hasta k = 12.
- **Prosa de SS-F y SS-N:** el término omitido, su valor y la diferencia «exactamente» (o el doble) son correctos.
- **Control positivo:** quité el factor 2 en la diferencia que se imprime en la explicación de SS-N. El comprobador lo marcó en 34 configuraciones.

## 2. Tope, tabla de atajos, alcance y posición

- **Tope.** Reproduzco las cuentas de `enumeracion_tope_r4.txt`. El tercer menor |valor| elegible es 3,525 (SS-N con x = ±3, n = 3), así que 4 es el tope **entero** más estricto, no el más estricto en absoluto (H5).
  - Con x = ±3 y n = 3, los tres elegibles (SS-F, SS-H, SS-N) quedan todos del mismo lado de la clave.
- **Clave en un extremo (exacto, por rama):**

  | Rama | Clave en un extremo |
  |---|---|
  | x = ±3, n = 3 | **100 %** (la mayor con x = 3, la menor con x = −3) |
  | x = ±3, n = 4 | 50 % |
  | x = ±2, n = 3 y n = 4 | 33,3 % |
  | Global | 54,2 % (rango de la clave 27,1 / 22,9 / 22,9 / 27,1 %) |

  La posición en pantalla sí se mueve: `ord` y `exshuffle: TRUE` la barajan.
- **Verificador.** Lo volví a correr: su salida es idéntica byte a byte a `p9_bateria_p7.txt`. Da 21 reglas por encima de +8 pp, «ROJO_SIN_CUBRIR … pendiente de firma del profesor», con exit 1. Nunca dice PASS ni «cubierto».
- **Mutantes del alcance:**
  - `MUTANTE_QUITAR=est_2a_cerca_sen` da «FUERA DEL ALCANCE: est_2a_cerca_sen (75 pp)».
  - `MUTANTE_TOPE=div_mult_x` da «POR ENCIMA DE SU TOPE: div_mult_x (8.3 pp > 7.3)».
  - Los dos funcionan, pero solo se distinguen por el mensaje (H6).
- **Tabla de 21 reglas.** Está bien medida y completa respecto a la batería congelada de 44 reglas. Pero hay reglas fuera de esa batería que pasan de +8 pp (H2).

## 3. Visual

- **PDF:** 100 versiones en un solo documento con `perifericos/solpcielo.tex` (oficio, 189 páginas): **0 `Overfull \hbox`** (render real, N = 100).
- **HTML:** 100 versiones (semillas 1 a 100), medidas con el script del autor:

  | Ancho de contenido | Resultado |
  |---|---|
  | 344 px | 0 desbordes |
  | 310 px (360 px reales en Moodle) | 0 desbordes |
  | 304 px (lo que midió el autor como «320») | 0 desbordes |
  | 270 px (320 px reales en Moodle) | desborde: ver H1 |

## 4. Código, ortografía y documentación

- **Código:** no hay `repeat` ni `while`; todos los `for` tienen cota y los sorteos usan `sample.int` y `pick`.
- **Ortografía:** el corrector no encuentra errores (recordatorio: su «limpio» no prueba que haya tildes).
- **YAML de Knit:** inocuo para R/exams.
  - Con `exams2html` el enunciado y la Solution salen sin rastro de `output:` y el nombre del ejercicio es correcto.
  - El PDF de 100 versiones compila.
  - Consecuencias de proceso: H4.
- **Estado del ejercicio:** los pasos 3 a 6 se sellaron en 190 ms (10:47:23,695 a 10:47:23,885). Auditado por artefacto, los respaldan: el render es de 10:43, posterior a la modificación del `.Rmd` (10:39), y el informe del arsenal es de 10:44:58 sobre este md5. `detractor_fase2c` sigue abierto.

## Hallazgos

### H1. El feedback de la clave desborda la caja real de Moodle a 320 px (MEDIA, visual)

- **Qué pasa.** El feedback de la clave (líneas 336-337 del `.Rmd`) es una ecuación en línea con cuatro fracciones `\dfrac` y el resultado. No está dentro de un contenedor con desplazamiento horizontal.
- **Por qué la medición del autor no lo vio.** `medir_desborde_360_320.py` usa una caja de 320 px con 8 px de relleno, o sea 304 px de contenido. La caja real de `.qtext` en el Moodle del colegio es la pantalla menos 50 px: 270 px a 320 y 310 a 360 (referencia medida del proyecto, `ref_moodle_icfes_ancho_qtext.md`).
- **Medición a 270 px** (`detractor_r4/desb/diag_286.html`), sobre 100 versiones:
  - En 15, esa ecuación rebasa la caja: 14 px con `-3 + \dfrac{27}{6} - \dfrac{243}{120} + \dfrac{2187}{5040} = -\dfrac{459}{5040}` y 6 px con x = −2 y n = 4.
  - 8 versiones desbordan la página.
  - Aparte, en 14 versiones alguna ecuación de la Solution necesita desplazamiento (16 ecuaciones, máximo 13 px). Esas están dentro de su contenedor con desplazamiento, que es la salida prevista en la familia 3.
- **Propuesta:**
  - Dejar el feedback de la clave sin la cadena: «Correcta. Usa los primeros N términos, con los signos alternados y los factoriales impares; el resultado es $frs$.»
  - Medir de nuevo con cajas de 286 y 326 px (contenido de 270 y 310 px).
  - Corregir el ancho en el script de medición.

### H2. Hay reglas por encima de +8 pp fuera de la lista para la firma (MEDIA, diagnosticidad)

La regla 22, apartado E, exige listar, regla por regla y con su cifra exacta, toda regla por encima de +8 pp. Medí tres reglas de la batería congelada sobre las 88 configuraciones (`detractor_r4/extra.R`):

| Regla (en palabras de aula) | Exceso | Peor rama |
|---|---|---|
| «si x es positivo, la mayor; si es negativo, la menor» | **+8,3 pp**, aplica al 100 % | **+75 pp** (x = ±3, n = 3) |
| «de las opciones con numerador múltiplo de x, la más cercana a sen(x)» | **+50,0 pp** | — |
| «la opción más cercana a la de la calculadora» | **+75,0 pp** | — |

- La primera es la versión ejecutable de la «clave en un extremo» que `ITEM_OFICIAL.md` solo describe en prosa (línea 128).
- **Atajo de la calculadora.** En la decisión 1 el profesor dijo «No aceptarlo». Con este diseño la calculadora sigue acertando el 100 %: se descarta la opción más cercana a sen(x) y se elige la segunda (`est_2a_cerca_sen`, +75 pp). Está en la lista, pero la petición de firma tiene que decirlo así, en lenguaje llano.
- **Propuesta:**
  - Presentar las tres reglas al profesor junto con las 21, cada una con su cifra.
  - Si entran en la batería, volver a medir todo el histórico, como pide la regla 22, apartado C.

### H3. Dos textos de los distractores: una frase falsa en su lectura literal y una frase con dos dos puntos (MEDIA, precisión de la Solution, regla #13 v1.1)

- **SS-A.** La explicación (línea 147) dice que el estudiante «escribe un signo menos delante de todos los términos». El cálculo que se muestra justo encima («2 − 8/2! − 32/4!») y la descripción corta («resta todos los términos después del primero») lo contradicen: el primer término no lleva signo menos. Ocurre en las ramas x = ±2, que es donde SS-A cumple el tope.
  - **Propuesta:** «escribe un signo menos delante de todos los términos que siguen a $x$».
- **SS-I.** La descripción corta (línea 199) lleva dos puntos y la plantilla del diagnóstico añade otros (línea 329). Queda «…empezando al revés: suma el segundo término y resta el tercero:».
  - **Propuesta:** «alterna los signos al revés (suma el segundo término y resta el tercero)».

### H4. El YAML de Knit no afecta a R/exams, pero deja dos riesgos de proceso (BAJA)

- **Salidas de Knit en la carpeta.** `.html`, `.pdf` y `.docx` del Knit de RStudio (10:28 a 10:30) están en la raíz del subproyecto. Son anteriores a la versión actual del `.Rmd`, no son salidas de R/exams y la carpeta entera está sin seguimiento. Un `git add .` las metería en el repositorio.
- **Cambio de md5.** Quitar el YAML cambia el md5 `b310061e…`, que está anclado en `ejercicio_state.json` y en las salidas del paso 9.
- **Propuesta:** con el visto bueno del profesor, quitar el YAML y borrar esas salidas antes del paso 11, y volver a correr los validadores que anotan el md5. Otra opción es declarar el md5 del cuerpo.

### H5. La documentación tiene tres desajustes (BAJA)

- **«El tope más estricto».** La cabecera del `.Rmd` (línea 24) y `ITEM_OFICIAL.md` dicen que 4 es «el tope más estricto». Es el entero más estricto entre los 7 candidatos probados; el mínimo exacto es 3,525.
- **Referencias viejas en el `.Rmd`.** Las líneas 26-28 remiten a la tabla «del H1 de fase2c_r3» (26 reglas) y dicen «sin alcance firmado cableado». Hoy son 21 reglas y el alcance está cableado, pendiente de firma.
- **`HANDOFF.md` desactualizado.** Tiene el md5 `735ae17e…`, habla de 26 reglas y de la coma en MathML. Además, la cabecera de la tabla de `enumeracion_tope_r4.txt` dice «|v|», pero los valores llevan signo.
- **Propuesta:** corregir los tres puntos al reescribir el `HANDOFF.md`.

### H6. Ningún mutante prueba el camino de «ROJO_CUBIERTO» (BAJA, testing)

- **Qué pasa.** Con la firma vacía (`verificar_bateria_p7.R:27`), la línea base ya sale con exit 1. Los mutantes del alcance se distinguen solo por el mensaje, y nada prueba que, con una firma, el verificador dé exit 3 y que el mutante lo devuelva a exit 1.
- **Propuesta:** un tercer mutante que rellene una firma ficticia y compruebe la secuencia: exit 3 sin mutar y exit 1 con el mutante de quitar o el de tope.

## No auditado o no repetido

- `verificar_mutantes_textos.R` (mutantes 1 a 7): leí su salida (los 7 cazados, 0 fallos de base en 600 semillas), pero no lo volví a correr.
- No volví a generar DOCX ni NOPS.
- No hice la FASE 2C-bis.

## Próximos pasos

1. H1: feedback de la clave sin la cadena, y nueva medición a 270 y 310 px de contenido.
2. H3: las dos reescrituras.
3. H2: añadir las tres reglas a la lista para la firma, con la explicación de la calculadora en lenguaje llano.
4. H4 a H6 al cerrar la ronda.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:337 | ck$l1, " = ", frs(num_clave, D), "$."))
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/salida/medir_desborde_360_320.py:12 | box-sizing:border-box;padding:8px;margin:0 0 40px 0
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/salida/verificar_bateria_p7.R:31 | regla = c("est_2a_cerca_sen", "rel_par_lejos_sen", "rel_par_mult_x"
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/salida/reglas_p7.R | AUSENTE: if (e$x > 0) o == max(o)
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/ITEM_OFICIAL.md:128 | los tres elegibles quedan del mismo lado de la clave
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:147 | delante de todos los términos, aunque en la serie los signos se alternan.
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:144 | y resta todos los términos después del primero
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:199 | alterna los signos empezando al revés: suma el segundo término y resta el tercero
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:3 | word_document: default
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:24 | es el tope más estricto que deja al menos 3 errores elegibles
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/serie_seno_aproximacion_numerico_variacional_formulacion_ejecucion_n3_schoice_v1.Rmd:27 | la tabla exacta del H1 de salida/fase2c_r3_detractor.md
H6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/serie-seno-aproximacion-n3/salida/verificar_bateria_p7.R:27 | FIRMA <- list(fecha = "", texto = "")
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS