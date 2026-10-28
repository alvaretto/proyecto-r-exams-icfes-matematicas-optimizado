# Auditoría del detractor, FASE 2C: `probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd`

**Fecha:** 2026-10-27
**Artefacto auditado:** md5 `5dae44c0231aec926b8348641d98f14d`, comprobado en disco. Es la versión de hoy.
**Modo:** solo lectura. Lo que ejecuté vive en `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0569ece5-0795-4f06-b915-ded47d71f2a8/scratchpad/detractor/`.
**Objeciones:** 8. Una de CORRECCIÓN, una ALTA, cuatro MEDIA y dos BAJA.

## Resumen

**Lo que está bien:**
- La clave es correcta y única en la canónica y en todo el espacio no canónico.
- Los tres distractores son falsos en todas las ramas.
- En las 2.698 versiones no canónicas que barrí, todo lo que afirma la Solution es verdadero.

**El defecto que obliga a rechazar está en la Solution de la instancia canónica.** Es justo la versión que reproduce el ítem oficial. En los bloques de las opciones C y D atribuye el error a «no sumar las celdas de cada tipo» o a «dejar fuera parte de los datos». Pero en esas dos opciones cada tipo es una sola celda, y la propia Solution lo dice unas líneas más arriba («8 fichas», «11 fichas»).

El código ya lo sabía. El comentario de la línea 172 declara que esos textos genéricos son falsos en C y D, y la corrección de la reapertura 1 quedó a medias. Por la regla #9 (la Solution afirma algo matemáticamente falso) es una CORRECCIÓN: binaria, **RECHAZAR**. El arreglo es pequeño: un par de campos de `TXT_CANON`, sin tocar la clave ni el generador.

**Sobre los dos overrides:**
- **Flujo B:** medí el estado actual con la misma rúbrica y da 96,8. El override firmado no lo cubre, así que hace falta una firma nueva.
- **Canónica:** el override está redactado por cifra (+25 pp) y no cubre el +75 pp de la regla relacional. El HANDOFF ya lo escala correctamente.

**Sobre el verificador:** volví a ejecutar `verificar_espacio.R` y la salida es idéntica a la guardada; modela bien `construir_cubo()`. Pero publica en la mezcla una cifra de posición canónica de +8,6 pp, por encima del corte de +8, que es un artefacto de una sola semilla.

## Lo que verifiqué sin objeción (evidencia ejecutada)

- **Re-ejecución de `verificar_espacio.R`:** exit 0. Comparada con `salida/r1/verificar_espacio.txt`, el diff solo cambia la línea del tiempo.
  - E1: 12.993.248 combinaciones, 0 violaciones.
  - E2: 600 cubos generados, todos dentro del espacio enumerado.
  - E3: la canónica coincide con `ITEM_OFICIAL.md`.
  - E5: los 9 mutantes los caza su propia sonda.
- **Barrido propio con conteos independientes** (indexé `w` yo mismo, sin pasar por `celdas_tipo()`), sobre 3.000 semillas, de las que 2.698 son no canónicas:
  - 0 versiones con dos claves.
  - PROB-UNA, 1.772 casos: el rótulo que nombra `explica_una()` corresponde a dos celdas realmente iguales y al número que se muestra. 0 fallos.
  - PROB-CRU, 1.814 casos: las celdas cruzadas son realmente iguales. 0 fallos.
- **Lectura completa de la Solution** en la semilla 7 (canónica) y en las semillas 1 y 2 (no canónicas). Sumas, fracciones y feedback correctos. No hay letras (regla #19). Los distractores se recorren por `sort(codigos_sel)`, no en el orden de las opciones, así que no hay Incidente Q.
- **Tabla canónica:** la generé desde el `.Rmd` y la comparé a ojo con `orig_pregunta_32.png`.
  - Celdas 12|11, 8|15, 16|20, 10|8.
  - Celdas combinadas «Figura geométrica», «Blanca» y «Negra» presentes.
  - Encabezados en negrita y cuerpo en tipo normal.
  - Enunciado y opciones verbatim.
  - La rúbrica no encuentra valores incorrectos.
- **Suma de control de I-2:** 28217 y 306 coinciden con el enunciado de `ITEM_OFICIAL.md`, no solo con la constante del `.Rmd`.
- **Glifos:** `validar_glifos_latex.R` da OK.
- **Ortografía:** el corrector sale limpio. Como tiene un punto ciego conocido, leí además a mano los textos visibles de tres semillas y no falta ninguna tilde.
- **Regla #18:** la imagen lleva `{width=85%}`.
- **Regla #11:** ocho plantillas, ocho estructuras, sin «registró».

## Hallazgos

| ID | Severidad | Tipo | Resumen |
|---|---|---|---|
| H1 | CORRECCIÓN | Solution falsa | Bloques de C y D en la canónica: «sumar» y «dejar fuera» sobre tipos que son una sola celda |
| H2 | ALTA | Proceso (relajar requiere humano) | El 96,8 del Flujo B no está cubierto por el override del 98,3 |
| H3 | MEDIA | Verificador | La cifra de posición de la canónica sale de una sola semilla: +8,6 pp espurio |
| H4 | MEDIA | Diagnosticidad / override | La regla relacional da +75 pp en la canónica; el override solo firma +25 pp |
| H5 | MEDIA | Testing / hook | El ERROR de la 2D es falso positivo, pero deja al hook sin 2G ni 2H en cada render |
| H6 | MEDIA | Guarda de I-2 | La suma de control del enunciado no ve permutaciones de palabras |
| H7 | BAJA | Guarda | La guarda de `calcula()` es tautológica; la de precondición no tiene mutante |
| H8 | BAJA | Metadatos | `TipoMetacognicion` describe la Solution, no la tarea del estudiante |

### H1. La Solution canónica afirma algo falso sobre C y D (CORRECCIÓN)

**Qué se cuestiona.** Esto es lo que se ve hoy en la Solution de la semilla 7. La frase también está en el render entregado `salida/r1/html/v71.html`.

- **Bloque de D (PROB-CRU).** Entre los campos se contradice a sí mismo:
  - *Lectura que lleva a elegirla:* «Compara las celdas que se cruzan en la tabla en lugar de sumar las celdas de cada tipo.» Es el texto genérico de la línea 159; `TXT_CANON` no lo sobrescribe.
  - *Por qué es un error:* «Elegir una pareja por la posición de sus celdas en la tabla no es comparar…» (línea 181).
  - *Causa raíz:* «Elige por la posición de las celdas en la tabla, sin leer ni sumar sus valores.» (línea 183).
- **Bloque de C (PROB-UNA).**
  - *Por qué es un error:* «Mirar solo una categoría deja fuera parte de los datos…» (línea 177).
  - Explicación de la rama canónica: «Al leer solo las celdas de una categoría se ven valores cercanos, 11 y 10» (línea 118). Pero 11 es de categoría par y 10 de categoría impar.

**Por qué es falso.** El *Procedimiento correcto* de esa misma Solution (línea 667) dice que cada tipo de C y D es una sola celda: «una ficha blanca de categoría impar con un círculo»: 8 fichas, y así con los demás. Entonces:

- En D no hay celdas que sumar dentro de cada tipo.
- Comparar las celdas cruzadas da 8 frente a 20, que lleva a **descartar** D, no a elegirla.
- En C, leer las celdas 11 y 10 es exactamente el cálculo correcto: no deja ningún dato fuera.

El autor lo dejó escrito en la línea 172: «Los textos genéricos describen tipos de dos celdas y en C y D serían falsos». La reapertura 1 corrigió la causa raíz de PROB-CRU, pero dejó la lectura genérica y escribió una descripción larga nueva para PROB-UNA con la cláusula falsa.

**Fuente.** Texto íntegro de la regla #9, tabla de tipos de defecto: CORRECCIÓN es «la Solution afirma algo matemáticamente falso». Es binaria («cualquier instancia lo activa») y lleva a RECHAZAR. El encargo pide además que cada afirmación de la Solution sea verdadera en cada rama.

**Riesgo concreto.** Afecta a la canónica: alrededor del 10 % de las versiones (80 de 688 en E2) y, sobre todo, la que se discutirá en clase porque es el ítem oficial. A quien eligió D se le dice que debió sumar celdas que no existen, y eso contradice el procedimiento que se le acaba de enseñar.

**Alternativa.**

Lo que se corrige sin firma, porque es texto propio y no de la ficha, y endurecer es autónomo:

```r
TXT_CANON[["PROB-CRU"]]$descripcion_corta <- paste(
  "Ve que las dos fichas ocupan celdas que se cruzan en la tabla y elige la pareja por esa",
  "posición, sin comparar sus valores.")
TXT_CANON[["PROB-UNA"]]$descripcion_larga <- paste(
  "Cada ficha de esta opción es una sola celda de la tabla y no hay nada que sumar; pero dos",
  "valores cercanos no son iguales: con el mismo total de fichas, 11 y 10 dan probabilidades distintas.")
```

Hay también una guarda mecánica barata. Si todos los tipos de un distractor son de una sola celda, sus campos de Solution no pueden contener `sum` ni «deja fuera»:

```r
stopifnot(!any(grepl("sum|deja fuera", campo(e, "descripcion_corta"))))
```

Además conviene un mutante que la demuestre: devolver el campo genérico.

Lo que es frase de la ficha (I-6 lo ancla) y conviene **proponer** al profesor en WAIT_USER #3:
- El nombre de PROB-CRU, «Compara celdas cruzadas sin sumar». Propuesta: el verbo de la ficha, «Toma las celdas que se cruzan en la tabla».
- La explicación de la rama canónica de `explica_cru`, «…sin sumar las categorías…».
- La de `explica_una`, «solo las celdas de una categoría». Propuesta: «Cada ficha de esta opción es una sola celda: 11 y 10. Están cerca, pero no son iguales.»

Tal como están escritas son, como mínimo, ambiguas.

**Veredicto:** MODIFICAR. Esto es corrección, no diagnosticidad, así que no gasta pasadas de §P7-D.

### H2. El Flujo B en 96,8 no está cubierto por el override del 98,3 (ALTA)

**Evidencia ejecutada.** Generé la tabla canónica desde el `.Rmd` actual (semilla 7, con `knitr.in.progress`) y le pasé `graficador/python/evaluar_similitud_python.py`:

- **Total: 96,8.** Colores 19,3; posiciones 20; valores 20; proporciones 15; estilos 12,5; elementos 10.
- Error de masa de texto: 12,7 %.

El dato del encargo queda confirmado.

**Análisis.** La fila firmada en `.claude/CLAUDE.md:120` acepta «el 98,3 % de Python, que es interpolado» con el ajuste de peso, y aclara que la relajación está «limitada a la lectura de la rúbrica de similitud». Es decir, el profesor relajó *cómo se lee* la rúbrica (interpolada en vez de literal), no el umbral de 98 de la regla #3. Aceptó una cifra que pasaba ese umbral con esa lectura. El 96,8 no lo pasa con ninguna de las dos lecturas.

Que el profesor ordenara el ajuste no equivale a firmar su resultado: el resultado se midió después. Relajar nunca es autónomo (regla #24), y la regla #3 manda «informar la mejor similitud y ofrecer opciones».

La curva medida en el HANDOFF muestra que ningún peso «normal» llega a 98: 0,25 da 94,2, 0,45 da 96,8 y 0,60 da 97,3, y este último ya parece seminegrita.

Además, `ejercicio_state.json:27` sigue registrando `"similitud_python": 98.3` con `flujo_b` sellado. Ese registro ya no describe el artefacto, y `reopen` no puede reabrir `flujo_b`.

**Para la decisión del profesor:** el ajuste **aumenta** la fidelidad tipográfica al impreso. `ITEM_OFICIAL.md` dice que «Par», «Impar» y los números van en tipo normal. La rúbrica, en cambio, penaliza la masa de tinta que el escaneo engorda. Las opciones son:
- (a) firmar el 96,8 con esa razón; o
- (b) volver a la negrita del 98,3, menos fiel al impreso.

**Respuesta a la pregunta del encargo:** el override tal como está redactado **no** cubre el estado actual. Hace falta una firma nueva, y al firmar hay que anotar el 96,8 en la tabla de decisiones local y en las notas del JSON. El HANDOFF (línea 83) ya lo escala correctamente.

**Veredicto:** MODIFICAR el registro y pedir la firma. Bloquea el paso 11, no la clave.

### H3. La posición de la canónica se mide sobre una sola semilla (MEDIA)

**Qué se cuestiona.** `verificar_espacio.R` evalúa la canónica solo con `canon[1]`, que es la semilla 7 (línea 240), y calcula `p_acierto` sobre ese único orden interno (línea 272). En la semilla 7 la clave cae en la posición interna 4. De ahí sale:
- `ultima_posicion` al 100 % en la canónica (+75 pp).
- En la mezcla, **33,6 % (+8,6 pp)**, por encima del corte de +8 pp que obliga.

**Evidencia ejecutada.** En las 44 semillas canónicas entre 1 y 400, la posición interna de la clave se reparte 11 / 15 / 12 / 6 entre las posiciones 1 a 4. La última posición sale el 13,6 % de las veces, no el 100 %. Lo esperado es 25 %, lo que deja la mezcla en torno a +1,1 pp.

El propio archivo reconoce el problema (línea 210 de la salida: las reglas de posición miden el orden interno y R/exams y Moodle vuelven a mezclar), pero imprime igual la cifra en la mezcla. El HANDOFF no la menciona. Un lector en WAIT_USER #3 puede tomar ese +8,6 como un canal que obliga.

**Alternativa.** Para las reglas que dependen del orden, promediar `p_acierto` sobre **todas** las semillas de `canon`, o excluir la familia de posición de la fila canónica con la razón declarada. Las reglas que no dependen del orden, como la relacional, quedan como están.

**Veredicto:** MODIFICAR el verificador.

### H4. El override de la canónica está escrito por cifra y no cubre el +75 pp (MEDIA)

**Qué se cuestiona.** El texto firmado cubre «+25 pp» (`.claude/CLAUDE.md:91` y `:108`). La regla relacional congelada `cruzada_en_su_molde` elige solo B en la canónica: 100 %, es decir +75 pp. Esta cifra sí es real, porque no depende del orden. En la mezcla queda en **+7,1 pp**: zona gris entre +5,3 y +8 de §P7-A, que no obliga.

No lo cubre la cifra firmada. El HANDOFF lo declara y lo escala correctamente (línea 54).

**Análisis para el profesor.** En la canónica verbatim, A y B son las dos únicas opciones agregadas y miden lo mismo (66 caracteres). Cualquier regla que separe A de B da por fuerza 0 % o 100 % en esa única instancia. Una tasa de una sola instancia no se compara con la vara de §P7-A, que es un promedio sobre 426 ítems. Lo que se mide de verdad es la mezcla: la no canónica tiene exceso de −0,6 pp, y en la mezcla real el máximo es +7,1 pp (o +2,5 pp sin contar la relacional).

**Alternativa.** Redactar el override por **alcance**, no por cifra. Por ejemplo: «residuo de la instancia canónica verbatim, cualquier regla de la batería congelada, mientras la mezcla real quede ≤ +8 pp». Así no hay que volver a firmar cada vez que la batería encuentre otro separador A/B.

**Veredicto:** MANTENER el diseño y que el profesor amplíe la redacción.

### H5. La 2D es un falso positivo, pero deja al hook sin 2G ni 2H (MEDIA)

**Punto (i): confirmado, es un falso positivo de la herramienta.** Emulé el código de la FASE 2D. El regex de la línea 152 del arsenal no cruza una comilla invertida, y las líneas 532 y 536 del `.Rmd` tienen comillas invertidas dentro del código Python.

- **Original:** el texto «limpio» conserva el chunk `data_generation` entero (19.822 caracteres). La palabra `codigo` solo aparece como identificador (`function(p, codigo)`, `list(codigo = "PROB-SIM"…`). Las dos únicas cadenas con `codigo` son concatenaciones de variables, no texto visible.
- **Control**, con esas dos comillas cambiadas: 0 marcas.

**Dos consecuencias que el HANDOFF no dice:**
1. En el control, el texto limpio mide **82 caracteres**. Como todo el texto visible lo emite R, la 2D no revisa nada de este ejercicio ni siquiera cuando pasa.
2. Mientras dure el ERROR, el hook se salta 2G y 2H en **cada** render futuro. Una regresión de multisemilla pasaría sin aviso.

**Alternativa.**
- En las dos líneas Python, cambiar las comillas invertidas por `«»` o comillas simples. No cambia nada funcional y el hook vuelve a correr completo.
- Aparte, reportar el regex del arsenal (por ejemplo `(?s)```\{r.*?\n```` sin barrera de comilla). Esto queda fuera del subproyecto.

### H6. La guarda del enunciado (I-2) no ve permutaciones (MEDIA)

**Qué se cuestiona.** La línea 341 compara la suma de puntos de código y la longitud del enunciado.

**Evidencia ejecutada.** «…fichas negras y fichas blancas…» (colores intercambiados) da 28217 / 306 y pasa. «impar o par» también pasa. Son los errores de transcripción más probables para este ítem. La comparación `identical()` de E3 los atrapa, pero solo cuando alguien corre el verificador.

**Alternativa.** Una suma ponderada por posición contra un literal independiente de `CANON_ENUNCIADO`:

```r
u <- utf8ToInt(enunciado)
sum(as.numeric(seq_along(u)) * u) == 4314626
```

El valor sale de mi corrida sobre el texto de `ITEM_OFICIAL.md`; hay que recalcularlo al implantar. Añadir el mutante M10: intercambiar blancas y negras.

### H7. Guarda tautológica de `calcula()` (BAJA)

**Qué se cuestiona.** La línea 350 compara `e$calcula(params)`, que devuelve `txt_opcion(ordenar_par(p$pares[[codigo]]), p$lab)` (línea 79), con `opciones_base`. Esta es la misma función aplicada al mismo par, y `ordenar_par` es idempotente porque los dos tipos de cada par siempre difieren en el nivel de cada dimensión que nombran. **No puede fallar.**

Además, `calcula()` solo busca el par que se asignó en `data_generation`; no deriva el error. La Capa C del Nivel 4 se cumple por construcción.

La guarda de precondición, en el mismo `stopifnot`, es inalcanzable en el espacio actual (E1 da precondición = 0). Como guarda de regresión es legítima, pero ningún mutante demuestra que pueda dispararse.

**Alternativa.** Quitar la guarda de identidad o declararla como consistencia. Añadir un mutante de precondición, por ejemplo `e <- d` → `e <- d + 1L` en la rama de PROB-UNA.

### H8. `TipoMetacognicion: analisis_error` (BAJA)

**Punto 3: no contradice I-1.** I-1 regula las opciones, no el metadato. Pero la etiqueta describe la Solution, no la tarea del estudiante. El estudiante juzga cuál de cuatro afirmaciones de equiprobabilidad es verdadera, y el feedback de cada opción es literalmente «Verdadero.» o «Falso.» (línea 362).

De los valores permitidos (texto íntegro de la regla #1, línea 672), `evaluacion_afirmacion` describe la tarea. El riesgo es que un filtro del banco por `analisis_error` mezcle este ítem con otros cuyo enunciado sí presenta un error.

**Alternativa.** Usar `evaluacion_afirmacion`, o dejar `analisis_error` y declarar en el `.claude/CLAUDE.md` local que la etiqueta se refiere a la Solution. No bloquea; decide el profesor.

## Respuestas a los puntos del encargo

**1. El `.Rmd`.**
- **Clave:** correcta en la canónica (26 = 26) y única en todo el espacio (E1, más mi barrido).
- **Solution:** verdadera en las no canónicas, en los seis pares ordenados de errores. Falsa en la canónica (H1).
- **Distractores:** falsos en todas las ramas.
- **Regla #19, Incidente Q y glifos:** sin problemas.

Las guardas, una por una:

| Guarda | ¿Puede fallar? |
|---|---|
| I-1 (vocabulario de error en las opciones) | Sí, la caza el mutante M5 |
| I-2: opciones | Sí: compara el generador contra literales |
| I-2: celdas | Sí, M6 |
| I-2: enunciado | Sí, pero es ciega a permutaciones (H6) |
| I-5 (una sola clave) | Sí, M1 y M2 |
| I-6 (pool ≥ 4) | Sí, M8 |
| I-7: celdas ≥ 1 | Sí, M4 |
| I-7: ningún par complementario | Hoy es estructuralmente inalcanzable (cada par cubre 4 de 8 celdas y las otras 4 suman al menos 12); sirve de regresión, M3 |
| I-8 (toda opción agregada) | Sí, M9 |
| Precondición | Inalcanzable hoy y sin mutante (H7) |
| Identidad de `calcula()` | Tautológica (H7) |
| I-3 e I-4 | Sin guarda, y así está declarado |

**2a. Override de la canónica.** Ver H4. La cifra de posición de la mezcla es un artefacto (H3).

**2b. Override del Flujo B.** No cubre el 96,8. Hace falta firma nueva (H2).

**3. Etiqueta de metacognición.** Ver H8.

**4. Afirmaciones del ciclo que comprobé:**
- **(i)** Confirmado: la 2D es un falso positivo, con las dos consecuencias de H5.
- **(ii)** El texto genérico de PROB-UNA es verdadero: 1.772 casos, 0 fallos; «mismo rótulo» es exacto. En `TXT_CANON`:
  - PROB-UNA: la lectura y la causa raíz son verdaderas; la descripción larga y la explicación canónica tienen la cláusula falsa (H1).
  - PROB-CRU: la descripción larga y la causa raíz son verdaderas; la lectura y el nombre siguen siendo los genéricos y son falsos (H1).
- **(iii)** El verificador replica una por una las ramas de `construir_cubo()`: las líneas 106-127 del verificador frente a las 205-237 del `.Rmd`. Las anclas de texto lo atan al generador, los 600 cubos de E2 pertenecen al espacio, mi re-ejecución da una salida idéntica y los 9 mutantes los caza su propia sonda. Sus límites están declarados, más el de H3.

**5. Severidades.**
- H1 es CORRECCIÓN y lleva a RECHAZAR.
- La diagnosticidad no obliga: la no canónica da −0,6 pp (PASS) y la mezcla real queda como máximo en +7,1 pp.

**Dominios.**
- **Cubiertos:** código R/exams, coherencia matemática, semántico, pedagógico, ICFES metacognitivo, testing y gramática.
- **Visual, solo en parte:** tabla canónica frente al original, más la rúbrica.
- **No auditados:** los PDF, DOCX y NOPS renderizados, y la vista a 360 px. La auditoría visual previa ya dejó un hallazgo MEDIO ahí.

## Próximos pasos

1. Corregir H1 en `TXT_CANON`, añadir la guarda y su mutante, y llevar al profesor la redacción de las frases de la ficha. En la misma pasada, quitar las comillas invertidas de H5 y cambiar la suma de control de H6. Para eso, `workflow-state.sh reopen <dir> --desde generacion_rmd` con su motivo; después re-renderizar, volver a correr `verificar_espacio.R` y pasar una FASE 2C nueva con otro detractor.
2. Corregir H3 en el verificador.
3. Llevar a WAIT_USER #3:
   - la firma del 96,8 (H2);
   - el override de la canónica redactado por alcance (H4);
   - la etiqueta de metacognición (H8);
   - `{width=100%}` para la tabla.

Mis scripts y salidas, en `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0569ece5-0795-4f06-b915-ded47d71f2a8/scratchpad/detractor/`:
- `a1.R`: lee la Solution en la canónica y en no canónicas, la posición interna de la clave en las 44 canónicas y las permutaciones del enunciado.
- `a2.R`: emula la 2D y su control.
- `a3.R`: barrido de 3.000 semillas.
- `canon_actual.png`: tabla canónica generada desde el `.Rmd` actual.
- `comparacion_python_detractor.png`: comparación de la rúbrica.
- `verif_rerun.txt`: re-ejecución del verificador.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:159 | Compara las celdas que se cruzan en la tabla en lugar de sumar las celdas de cada tipo.
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:172 | Los textos genéricos describen tipos de dos celdas y en C y D serían falsos.
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:181 | Elegir una pareja por la posición de sus celdas en la tabla no es comparar:
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:183 | Elige por la posición de las celdas en la tabla, sin leer ni sumar sus valores.
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:177 | Mirar solo una categoría deja fuera parte de los datos, y dos valores cercanos no son
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:118 | Al leer solo las celdas de una categoría se ven valores cercanos,
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:667 | Cada tipo de ficha nombrado en las opciones corresponde a una o dos celdas de la tabla; cuando son dos, hay que sumarlas
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/.claude/CLAUDE.md:120 | Umbral del Flujo B: se acepta el 98,3 % de Python, que es interpolado
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/.claude/CLAUDE.md:120 | Relajación firmada (H-5), limitada a la lectura de la rúbrica de similitud.
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/ejercicio_state.json:27 | "similitud_python": 98.3,
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/HANDOFF.md:83 | Si el override del Flujo B (98,3 %) cubre el 96,8 % que deja el ajuste de peso.
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/verificar_espacio.R:240 | set.seed(canon[1]); envc <- new.env(); eval(EXPR, envir = envc)
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/verificar_espacio.R:272 | pc  <- vapply(REGLAS, function(rg) p_acierto(rg, opc, kc), 0)
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/salida/r1/verificar_espacio.txt:210 | Las reglas de posición miden el orden INTERNO: R/exams (exshuffle: TRUE) y Moodle vuelven a mezclar.
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/.claude/CLAUDE.md:91 | «elegir entre las dos cortas» acierta el 50 %: **+25 pp** sobre el 25 % del azar.
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/.claude/CLAUDE.md:108 | +25 pp se mide **aparte** y se declara como residuo de la instancia canónica
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/HANDOFF.md:54 | **Nuevo, declarado (§P7-C, no obliga en este ciclo):** la regla relacional
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/SOURCES/scripts_validacion/arsenal_validacion_completa.R:152 | contenido_texto_limpio <- gsub("```\\{r[^`]*```", "", contenido_texto_limpio, perl = TRUE)
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:532 | debe ser una matriz 4 x 2; llegó con forma %s
H6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:341 | sum(utf8ToInt(enunciado)) == 28217L, nchar(enunciado) == 306L, N == 100)
H7 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:350 | identical(e$calcula(params), opciones_base[[e$codigo]])
H7 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:79 | txt_opcion(ordenar_par(p$pares[[codigo]]), p$lab)
H8 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:738 | exextra[TipoMetacognicion]: analisis_error
H8 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/probabilidad-fichas-tabla-doble-entrada-n2/probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd:362 | paste0("Verdadero. Cada tipo tiene ", cuentas_o[, 1]
H8 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/docs/reglas/ejercicios-metacognitivos.md:672 | exextra[TipoMetacognicion]: [analisis_error|evaluacion_afirmacion|comparacion_procedimientos]
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: RECHAZAR