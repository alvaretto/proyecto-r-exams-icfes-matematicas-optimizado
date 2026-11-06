# Auditoría Detractor: rapidez-media-error-paso (SCHOICE v1), FASE 2C, ronda 4

**Fecha**: 2026-11-05
**Artefactos**: `.Rmd` md5 `904af1ca1271d97c54123218c9db03c0` y `verificar_espacio.R` md5 `35cf38e910b91c8e51b9ab89226747c0`, comprobados al empezar y al terminar. No toqué el subproyecto: corrí todo en copias del scratchpad, porque el modo `exacto` reescribe `salida/firma_o2_medida.csv`.
**Tabla de firma**: cambió mientras auditaba (mtime 17:20:50). Audité la versión md5 `daf0b3309c80b1004668caf44a01d76b`. Esa versión ya trae corregida la glosa de `RE-con-fraccion`. La leí primero con «su fracción», que daría +9,21 y no +14,95, así que ya no es hallazgo.
**Objeciones**: 7 (0 críticas, 3 altas, 4 medias)

## Resumen ejecutivo

**CORRECCIÓN: 0 defectos**, en las cuatro ramas y en la canónica, con enumerador y función de verdad propios sobre 9 564 estados y 131 586 conjuntos. Ni `EV-D-PROM` ni `EV-T-CUENTA` admiten una lectura razonable que las vuelva verdaderas.

**Alcance de O-2: cumplido.** Las condiciones del `.Rmd` son las de `modelo.py`. Reproduje las 34 filas de la tabla a 4 decimales, sin `metricas()`.

**Tres ALTAS, y ninguna pide tocar el `.Rmd`:**

1. **H1.** La enumeración no lee lo que el generador sortea. Tres mutantes del reparto dejan la tabla O-2 idéntica byte a byte.
2. **H2.** La glosa de `NM` no dice lo que mide el código: leída al pie de la letra da −1,31 pp, no +10,62.
3. **H3.** Hay reglas sin cálculo por encima de +8 pp fuera de la tabla, varias de 2 cláusulas. `EV-T-CUENTA` se descarta a ojo, y la clave nunca sube el número en los pasos 1 y 2.

## Tabla de hallazgos

| ID | Severidad | Tipo | Hallazgo | Reproducción |
|---|---|---|---|---|
| H1 | ALTA | Verificador, §P7-E | la enumeración no ata `pick` ni `elegidos` | 3 mutantes: tabla O-2 idéntica a r11 |
| H2 | ALTA | Tabla de firma | glosa de `NM` falsa | 27,49 % contra 39,42 % |
| H3 | ALTA | Diagnosticidad | reglas > +8 pp sin fila, 6 623 conjunciones | enumeración exacta; control permutado 25,47 % |
| H4 | MEDIA | Testing | sondas de los 4 mutantes re-enumerados no discriminan | O2-I y O2-R, misma lista |
| H5 | MEDIA | Testing | el 1b salta las razones nuevas | 11 535 de 76 354 |
| H6 | MEDIA | L-9 | la guarda repite la línea de arriba y no hay oráculo | mutante C: 35/35 |
| H7 | MEDIA | Declaraciones | punteros viejos en `.claude/CLAUDE.md` | líneas 34, 51 y 83 |

## 1. CORRECCIÓN

**Enumeración propia** (Python, escrito desde el `.Rmd`). Los 131 586 conjuntos tienen peso > 0, y hubo 0 casos de cada una de estas fallas:

- más de una verdadera o clave falsa;
- razón falsa que dé `v`;
- opciones duplicadas;
- razón que afirme lo que rechaza;
- FINAL contra su paso;
- dos razones de signo;
- un paso ausente;
- L-9 incumplida.

**Control positivo**: sin las razones nuevas, el mismo código da `E3` 100,00 %, `M3` 100,00 %, `EST` sin rótulos 50,95 % y `entre` 56,19 %, las cifras de la r3.

**Coincidencias de las razones nuevas con otras magnitudes:**

| Pregunta | Resultado |
|---|---|
| ¿`dur + 1` es el tiempo de todo el viaje, `d2 / v`? | No. Exigiría `d1 = v`, que `cand` excluye. 0 casos |
| ¿`dur + 1` coincide con `t1`, `t2`, `Tm`, `Vm` o `t1 + t2`? | 0 casos |
| ¿`dur + 1`, sustituida, da `v`? | Sí en 57 estados, `EV-D-SUMA` con `2 d1 = v`. `repara()` la retira: 0 ofrecidas |
| ¿El promedio coincide con `D`, `Dm`, `d1`, `d2` o `v`? | 0 casos: precondición, y `prom = d1 + v dur / 2 > v` |
| ¿El promedio, sustituido, da `v`? | 0 casos |
| ¿El promedio coincide con `Vm`? | Sí, en el 5,85 % de las versiones con `EV-D-PROM`: `dur = 2` y `EV-D-SUMA`. El veredicto no cambia, solo la cifra ya no es nueva |
| ¿El promedio es la mitad de lo que rechaza? | Sí, en el 11,51 %: toda `EV-D-SUMA` con PROM. El veredicto no cambia |
| ¿El promedio coincide con `v t1`? | 0,81 %. Sin efecto: el procedimiento nunca muestra `t1` como tiempo |

**Lecturas razonables.** Contar las dos puntas no es una lectura de «el tiempo que transcurrió»: el paso 2 ya lo mide como resta y las nueve plantillas solo dan dos instantes. «Rapidez media» explica por qué alguien promediaría, que es la `causa_raiz`, pero no hace verdadera la razón. Ninguna plantilla habla de promedios de posición ni de horas contadas.

**Prosa.** Leí seis versiones no canónicas con esas razones, en las cuatro ramas, más la canónica en `smoke`.

- Las `dur + 1` marcas y los `dur` intervalos son ciertos de 2 a 5: las horas son enteras y no cruzan el mediodía.
- `prom` siempre queda entre las dos lecturas.
- Feedback y Solution son coherentes con la rama. No hallé texto falso.

## 2. Alcance de O-2

| Comprobación | Resultado |
|---|---|
| Condiciones de `EV-D-PROM` y `EV-T-CUENTA` frente a `modelo.py` | Idénticas: entera y distinta de `D`, `Dm`, `d1`, `d2`; distinta de `t1`, `t2`, `Tm`, `dur` |
| Orden de la obligación | Después del filtro de signo (línea 385) y antes de los pesos (392-397) |
| `RANGO_META`, `P_CANON`, L-2, pesos, otras razones | Sin cambios en el diff contra `rmd_md5_25a924d4.Rmd.txt` |
| Fuera de la lista de O-2 | Solo la guarda L-8 de operandos, H6 de la r3: no mueve el reparto |
| Variante sin obligación | Reproduzco +16,39 pp de `EST` sin rótulos y 32,93 % de versiones con razón nueva, las de `c1.json` |

El 8,09 % de las versiones no admite ninguna razón nueva, y ahí sigue la propiedad de la r3.

## 3. L-8 y L-9

- **L-8: sólida.** La guarda ve resultados y operandos, y el oráculo ve el texto. M17a1, a2, b, c, d y e mueren por lo que declaran. No hallé forma de mostrar un negativo sin que muera nada: un negativo solo en el texto del paso 3 lo caza `ORACULO_L8`.
- **L-9: la guarda puede disparar (M25, semilla 2), pero no detecta lo que dice.** Ver H6.

## 4. Verificador

| Hallazgo de la r3 | Estado |
|---|---|
| H3 de la r3 | Resuelto: la mezcla se calcula para todo `MEDIBLES`. `X1a` da +5,07 y +5,88 |
| H4 de la r3 | Resuelto a medias: amarre del `switch`, ancla de `cand`, errores de regla y exit. Queda H1 |
| H5 de la r3 | Resuelto: mutante `X1b` con 0 abortos, 131 586 conjuntos |
| H7 de la r3 | Resuelto para cifras y tramos. M26 a M29 mueren |
| H8 de la r3 | Resuelto en la cabecera. Los residuos están pendientes, y ya lo declaraste |
| H9 de la r3 | Resuelto: M3b, M21b, M17a1 y M17a2 |

**Exit.** `decidir_exit` da 4 solo con `todo` y `FIRMADA`, y con `FIRMADA` en FALSE da 3. Lo comprobé por lectura y con V4. No probé `FIRMADA` TRUE en una corrida real.

**Mutantes del reparto.** Cada uno se corrió en una copia, con el verificador intacto:

| Mutante del `.Rmd` | Efecto real, exacto en mi modelo | `exacto` | `rapido` |
|---|---|---|---|
| A: línea 397, `prob = pesos / sum(pesos) + 1 / length(pesos)` | `MAG-segundo-afirmado` −1,58 pp, `NM` −0,80 pp | exit 3, 34 listadas, 0 diferencias, tabla idéntica | exit 3, 35/35 |
| B: línea 32, `pick` con `prob = rev(seq_along(x))` | `U` baja a 27,88 %; `EST` sin rótulos sube a 42,58 %, +13,78 pp, y el verificador dice +4,53 | Igual | exit 1, de rebote: M6 no halla su caso en 600 semillas |
| C: tras la guarda L-9, con P = 1/2 cambia CUENTA por SUMA | versiones con razón nueva: de 91,91 % a 69,39 % | Igual | exit 3, 35/35, oráculo 0 de 300 |

**Reproducción propia**, de las 34 filas. Ejemplos, no canónicas / mezcla:

| Regla | No canónicas | Mezcla |
|---|---|---|
| `U` | 42,5205 % | 45,3945 % |
| `NM` | 39,4186 % | 39,1144 % |
| `X1b` | 44,3134 % | 43,3477 % |
| `R30-90` | 59,3862 % | 58,9169 % |
| `M3` | 90,6842 % | 91,1500 % |

## 5. Tabla de firma

- **Líneas citadas:** todas correctas.
- **34 filas:** 16 + 18 confirmadas, dentro de las 74 reglas de `MEDIBLES`. No es «toda combinación» en absoluto: ver H3.
- **«Exige calcular»:** defendible en todas. `E3` compara numerador y denominador, y las de sustitución dividen.
- **Glosas:** de las 18 reglas, 17 dicen lo que mide su línea. Falla `NM`: ver H2.

## 6. Reglas sin cálculo

**Lectura como estudiante.** La marca señala el paso (`M0` +46,20, ya en tabla). Cuando ese paso se repite, quedan dos opciones.

- Ahí `5 h y no 4 h` se descarta mirando las dos cifras de la propia opción.
- En `EV-T-FINAL`, si `3 h` y `4 h` conviven, gana la menor.
- `EV-D-PROM` es la única razón nueva que solo se descarta calculando, aunque sea una media.

**Búsqueda exacta con pesos reales**, sin marca, de 1 a 3 cláusulas, con los 29 rasgos más 17 propios:

- 96 568 conjunciones evaluadas; **6 623 llegan a +8 pp**.
- **Control de equivalencia:** los 29 rasgos reproducen las 18 líneas de `busq_c2.txt`.
- **Control permutado:** máximo 25,47 %.
- **Control positivo:** sección 1.
- **Reglas propias solas, por debajo de +8:** `dif1_menor` +3,91, «descarta `N + 1` contra `N`» +0,10. La paridad de `d1 + d2` da 25,00 %, porque sola no aplica.

Las cifras por regla están en H3.

## Objeciones

### H1 — La enumeración no está atada a lo que el generador sortea

**Qué se cuestiona**: `enumerar()` reparte el peso con `pst` escrito a mano y luego usa `e$pesos` sobre `e$candidatos`. Nunca lee `pick`, ni la línea que sortea `elegidos`, ni lo que pasa después. La cabecera dice «ATADO al generador real».

**Por qué** (Nivel 1, regla #22 §P7-E; memoria `feedback_invariante_sin_guardia`): «un mutante debe sacarlo de “cubierto”». Aquí cambia el reparto y nada sale de «listada». `comparar_modelo` tampoco lo ve, porque `modelo.py` comparte la misma suposición uniforme.

**Riesgo medido**: tabla de los tres mutantes.

- La tabla O-2 queda idéntica byte a byte a r11.
- Con B, la cifra real de una regla no listada pasa de +8 pp y el verificador sigue diciendo +4,53.
- Por inferencia, sin haberlo corrido: con `FIRMADA` en TRUE, A y C saldrían con exit 4 en `todo`, porque `enumerar` y `mutantes_o2` tampoco leen `elegidos`.
- **Las cifras del md5 actual son correctas.** Las líneas 32 y 397 son las esperadas. El riesgo es lo que ampare la firma después.

**Alternativa**:

1. Cablear `RMD_MD5` y que `decidir_exit` dé 3 si `tools::md5sum(RMD)` difiere. O-1 ya decía que sus cifras valen solo para un md5, y esto cubre A, B y C.
2. Añadir V5 y V6: una copia con un byte cambiado debe dar exit 3.
3. Corregir «ATADO» en la cabecera.

No cambia el md5 del `.Rmd`.

**Veredicto**: MODIFICAR el verificador.

### H2 — La glosa de `NM` no dice lo que mide el código

**Qué se cuestiona**: la tabla dice «la primera cifra de lo que muestra su paso». El código compara con `rn`, que es el **resultado** en los pasos 1 y 2 y el numerador solo en el paso 3.

**Por qué** (Nivel 1, O-2: «su glosa contrastada con esa línea»; memoria `feedback_glosar_variables_desde_el_codigo`): es el mismo tipo de error que tumbó O-1.

**Riesgo medido**: leída literalmente, con `d2` y `t2` como primera cifra, `NM` da 27,49 %, −1,31 pp, y no haría falta firmarla. El código da 39,42 %, +10,62 pp.

**Alternativa**: «Descarta las opciones cuya cifra afirmada supera lo que rechazan: el resultado del paso en los pasos 1 y 2, y el numerador de la fracción que muestra el paso en el paso 3. Empatar no descarta.»

**Veredicto**: MODIFICAR la tabla, sin volver a medir.

### H3 — Reglas sin cálculo por encima de +8 pp, sin fila

**Qué se cuestiona**: la tabla dice contener toda combinación con exceso >= +8, pero solo dentro de `MEDIBLES`. O-2 exige listar «una por una» toda regla que pase de +8.

**Por qué** (Nivel 1, regla #22 §P7-E): firma regla por regla, y un tope por familia no es alcance firmado.

**Causa estructural 1.** En los pasos 1 y 2 la clave afirma menos que lo que rechaza en el 100 % de los casos: tras L-8, todo error cometido infla el número. `EV-T-CUENTA` sube en el 81,61 %. Por eso L-9 no compite con `NM`: `NM+frac` era +27,30 antes de O-2 y es +27,28 ahora.

**Causa estructural 2.** `EV-T-CUENTA` sale en el 52,63 % de las versiones, y en el 46,85 % se lee como `N + 1 y no N`.

**Riesgo medido**, exacto. Exceso sobre 28,8 %, no canónicas / mezcla:

| Regla propuesta | Qué hace | Exacta | Exceso | Mezcla | Exceso | Canónica | Calcula |
|---|---|---|---|---|---|---|---|
| `NM+frac` | `NM`, sin fracciones menores que 1 | 56,08 | +27,28 | 54,94 | +26,14 | 33,3 | no |
| `NM+pasos` | `NM`, sin cifras que aparezcan en los pasos | 54,70 | +25,90 | 53,22 | +24,42 | 25,0 | no |
| `NM+kmh` | `NM`, sin fracciones h/km | 53,88 | +25,08 | 52,86 | +24,06 | 33,3 | no |
| `NM+X0` | `NM`, sin cifras del enunciado | 50,30 | +21,50 | 50,28 | +21,48 | 50,0 | no |
| `NM+X0+frac` | las tres condiciones | 64,45 | +35,65 | 63,73 | +34,93 | 50,0 | no |
| `NM+kmh+frac` | las tres condiciones | 64,26 | +35,46 | 62,71 | +33,91 | 33,3 | no |
| `NM+pos+frac` | las tres condiciones | 63,17 | +34,37 | 65,01 | +36,21 | 100 | no |
| `EST-1` | `EST` sin rótulos, sin `N + 1 y no N` | 44,55 | +15,75 | 43,57 | +14,77 | 25,0 | no |
| `EST+-1` | además sin negativas | 46,26 | +17,46 | 45,20 | +16,40 | 25,0 | no |
| `EST+-1-par` | y sin la mayor de dos cifras del mismo paso que difieren en 1 | 49,87 | +21,07 | 48,62 | +19,82 | 25,0 | no |
| `E3-1` | `E3`, sin `N + 1 y no N` | 73,73 | +44,93 | 75,05 | +46,25 | 100 | no |
| `E3-1-par` | y sin la mayor del par | 78,05 | +49,25 | 79,14 | +50,34 | 100 | no |
| `M3-par` | `M3`, sin la mayor del par | 95,44 | +66,64 | 95,67 | +66,87 | 100 | no |
| `entre-media` | `entre`, sin la media de las lecturas | 47,97 | +19,17 | 46,82 | +18,02 | 25,0 | sí, una media |
| `E3-1-par-media` | además sin esa media | 98,47 | +69,67 | 98,54 | +69,74 | 100 | sí, una media |

**Margen §P7-B.** Son propiedades contables, así que no las exime. Las comparaciones tienen margen: en `N + 1 y no N` es `1/dur`, de 20 a 50 %, y la clave se aparta de lo que rechaza un 15 % o más en el 92,78 % de los casos.

**Lectura del conjunto.** `EST` sin rótulos pasa de +22,15 a +4,53 con la obligación, pero descartando `N + 1 y no N` vuelve a +15,75, casi los +16,39 de no tener obligación. Contra quien no calcula, la defensa real de L-9 es `EV-D-PROM`.

**Alternativa**: no toco el diseño, porque O-2 lo prohíbe.

1. Añadir estas reglas a `EXTRA`, medirlas en el verificador y llevarlas a `FIRMA_O2` y a la tabla.
2. Cambiar «TODA combinación» por «toda combinación de las reglas de `MEDIBLES`».
3. Llevar al profesor las 6 623: no se pueden listar una por una, y §P7-E no admite tope por familia. Debe decidir qué se firma y qué se declara como no agotado.

**Veredicto**: decisión del profesor; `.Rmd` sin cambios.

### H4 — Las sondas de los 4 mutantes re-enumerados no discriminan

**Qué se cuestiona**: las sondas se fijaron después de medir, y el criterio es `setequal`: una foto de lo que salió.

**Por qué** (Nivel 1, §P7-E; contrato de mutación del repo): una sonda debe probar la causa, no solo que algo se movió.

**Riesgo medido**: O2-I, que quita razones, y O2-R, que fuerza el rango, tienen **la misma lista de 15 reglas**. Con tolerancia 1e-6, casi cualquier cambio de reparto saca casi todo. Prueban que `clasificar_o2` reacciona, y eso ya lo prueban los 72 mutantes en copia. No prueban causa, dirección ni magnitud. Además, ninguno re-enumera L-9: los +16,39 de `.claude/CLAUDE.md`:90 no están atados al R.

**Alternativa**: una regla causal por mutante, con cifra predicha por el modelo Python **antes** de correr el R, a la centésima. El primero ya tiene su predicción escrita: mutante sin obligación ni guarda, `ns` 311 091 y `EST-sin-rotulos` +16,39.

**Veredicto**: REEMPLAZAR las sondas.

### H5 — El 1b no revisa las razones nuevas

**Qué se cuestiona**: el `switch` no tiene `EV-D-PROM` ni `EV-T-CUENTA`. Caen en `NA` y siguen de largo, aunque la cabecera promete revisar toda razón candidata.

**Por qué** (Nivel 1, regla #13): la enumeración declara una cobertura que no da.

**Riesgo medido**: hoy, 0 defectos. De las 76 354 «razones revisadas», 11 535 son nuevas y se saltan. `EV-T-CUENTA` necesita `repara()` en 57 estados. Lo cubren `Fl[6]` en `exacto` y el oráculo, si sale sorteada.

**Alternativa**: añadir `"EV-D-PROM" = (2L * d1 + f$D) %/% 2L` y `"EV-T-CUENTA" = f$dur + 1L`.

**Veredicto**: MODIFICAR.

### H6 — La guarda L-9 repite la línea de arriba y no hay oráculo

**Qué se cuestiona**: la guarda comprueba `elegidos` justo después de sortear sobre `candidatos`, y `ops` se arma después. El oráculo no mira L-9.

**Por qué** (Nivel 1, memoria `feedback_invariante_sin_guardia`): sin mutante que muera al revertirla, es documentación. M25 solo prueba la línea de al lado.

**Riesgo medido**: mutante C, con razones nuevas en 69,39 % y no en 91,91 %: exit 3, 35/35, tabla idéntica. Si se quita la razón antes de `con_nueva`, sí lo ve `exacto` por diferencia con el modelo, así que el hueco es lo que corre después de la guarda.

**Alternativa**, sin tocar el `.Rmd`: en `oraculo()`, usar `e$ops`, y si `length(e$con_nueva) >= 1L` y ninguna opción es razón nueva, devolver `ORACULO_L9`. Añadir el mutante C como M25b.

**Veredicto**: MODIFICAR el verificador.

### H7 — Punteros viejos en `.claude/CLAUDE.md`

**Qué se cuestiona**:

- L-5 dice que lo vigente está en O-1, que quedó sin efecto.
- L-4 nombra M3, hoy M3b.
- L-8 nombra M17a, hoy M17a1 y M17a2.

Además de los residuos que ya declaraste, faltan `M0` y `RE`, como pide O-2.

**Por qué** (Nivel 1, regla #9 v1.5): las invariantes locales están en el alcance del detractor.

**Riesgo medido**: bajo en sustancia, pero el profesor firma contra este archivo.

**Alternativa**: corregir las tres líneas al actualizar los residuos.

**Veredicto**: MODIFICAR.

## Invariantes y dominios

**Invariantes, una por una:**

- L-1, L-2, L-3, L-7 y L-8: se cumplen. L-3 se comprobó en el diff y en `smoke`.
- L-4: se cumple, 0 razones que reparen.
- L-5: cifras históricas, con el puntero de H7.
- L-6: se cumple según las sondas, que no volví a mutar.
- L-9: se cumple en todo el espacio, con la guarda débil de H6.

**Dominios:**

- **Código R-exams**: 0 abortos.
- **Pedagógico**: las dos razones son concepciones reales y la Solution tiene sus secciones. El Nivel 3 sigue sin evidencia de aula.
- **Visual**: no auditado aquí, va en paralelo.
- **Gramática**: sin hallazgos en lo leído.
- **Coherencia matemática**: sin hallazgos.
- **ICFES metacognitivo**: `exextra` sin cambios en el diff.
- **Testing**: H1, H4, H5 y H6.
- **Semántico**: sin hallazgos de CORRECCIÓN.
- **Override**: H1, H2 y H3.

**Restos inocuos, sin hallazgo:** la entrada «como tramo» de `frases()`, pisada en la línea 201; `smoke` sale con 0 aunque la cabecera diga que no ocurre; `ofrecible` sigue sin uso.

## Lo que ejecuté

- Enumerador propio, función de verdad, reproducción de la tabla, búsqueda exacta, control permutado y control positivo.
- Tres mutantes del `.Rmd`, cada uno en `exacto` y `rapido`, en copias. La muestra de `rapido` es N = 100 por rol, la estándar.
- `verificar_citas_detractor.py` sobre este bloque: 35 citas, 0 problemas.

Scripts y salidas:

- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0d13b1e6-8d43-4929-9f2b-9b27caa5513b/scratchpad/det4/gen.py`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0d13b1e6-8d43-4929-9f2b-9b27caa5513b/scratchpad/det4/rasgos.py`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0d13b1e6-8d43-4929-9f2b-9b27caa5513b/scratchpad/det4/busca.out`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0d13b1e6-8d43-4929-9f2b-9b27caa5513b/scratchpad/det4/perm.out`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0d13b1e6-8d43-4929-9f2b-9b27caa5513b/scratchpad/det4/compuestas.py`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0d13b1e6-8d43-4929-9f2b-9b27caa5513b/scratchpad/det4/mutA.exacto.out`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0d13b1e6-8d43-4929-9f2b-9b27caa5513b/scratchpad/det4/mutB.rapido.out`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0d13b1e6-8d43-4929-9f2b-9b27caa5513b/scratchpad/det4/mutC.rapido.out`

## No auditado

- Auditoría visual, pasos 8, 9 y 10, DOCX, escaneos NOPS y `/detractor-hetero`.
- `todo`, `enumerar` y `mutantes_o2` sobre los mutantes. Lo que digo de ellos es inferencia del código.
- `FIRMADA` TRUE en corrida real.
- Conjunciones de más de 3 cláusulas fuera de las que listo, y la vara del corpus oficial para las reglas de H3.

## Veredicto global

**APROBAR_CON_CAMBIOS.** Hay cero defectos de CORRECCIÓN, así que no corresponde RECHAZAR. Las tres ALTAS bloquean la firma del paso 11, no el `.Rmd`.

## Próximos pasos

1. Corregir la glosa de `NM` (H2) y el alcance de la frase «TODA combinación».
2. Medir en el verificador las reglas de H3 y llevarlas al profesor, junto con las 6 623 conjunciones. No se reajusta nada.
3. Cablear el md5 del `.Rmd` (H1), el oráculo de L-9 (H6), el `switch` del 1b (H5) y las sondas (H4). Todo es del verificador.
4. Actualizar `.claude/CLAUDE.md` (H7) y correr los pasos 8, 9 y 10.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:609 | w <- e$pesos / sum(e$pesos)
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:595 | pst <- 1 / nrow(esp) / length(cand) / 3 / length(errs[[rol]])
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:31 | ATADO al generador real
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:397 | elegidos <- candidatos[[sample.int(length(candidatos), 1L, prob = pesos)]]
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:32 | pick  <- function(x) x[sample.int(length(x), 1L)]
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:135 | sus cifras valen solo para el md5
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/salida/tabla_firma_o2.md:19 | Descarta las opciones cuya primera cifra afirmada supera la primera cifra de lo que muestra su paso
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:530 | rn <- vapply(pso, function(p) c_paso(est, p), 0)
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:510 | resultado de los pasos 1 y 2; numerador del paso 3
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/salida/tabla_firma_o2.md:7 | Contiene TODA combinación regla-ámbito de exceso >= +8: 34 filas
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:125 | final se lista una por una
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:499 | MEDIBLES <- c(nr, XN, "Q", EXTRA)
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:556 | sc_log(n_ <= rn)
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:338 | paste0(" y no $", rechaza[[rol]], "$"))
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:299 | function(p) p$dur + 1L),
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:106 | dist = pick(c("EV-D-SUMA", "EV-D-FINAL")),
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:90 | queda en +16,39 pp, y con ella en +4,53 pp
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:1016 | Las sondas se declararon DESPUÉS de medir cada mutante
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:1045 | setequal(sale, m$sonda)
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:1025 | list(id = "O2-I", sonda = c("X1b", "X1", "R30-90", "R20-120", "R10-150", "R20-120+ent", "U", "NM", "X0+", "E2", "E3", "M2", "M3", "MAG-segundo-afirmado", "E1"),
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:1029 | list(id = "O2-R", sonda = c("X1b", "X1", "R30-90", "R20-120", "R10-150", "R20-120+ent", "U", "NM", "X0+", "E2", "E3", "M2", "M3", "MAG-segundo-afirmado", "E1"),
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:21 | Exige que ninguna razón repare el procedimiento (L-4)
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:358 | x <- switch(cd, "EV-D-FINAL" = d1 + f$D, "EV-D-SUMA" = 2L * d1 + f$D, "EV-D-SIGNO" = -f$D, "EV-D-INICIAL" = d1,
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:359 | "EV-T-INICIAL" = f$t1, NA)
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:360 | if (is.na(x)) { if (cd == "EV-C-CIFRAS"
H6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:398-399 | length(con_nueva) == 0L || any(elegidos %in% c("EV-D-PROM", "EV-T-CUENTA")))
H6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:400 | clave <- switch(rol_clave,
H6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R | AUSENTE: ORACULO_L9
H6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:90-91 | mutante que quite la obligación
H7 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:51 | Lo vigente está en O-1.
H7 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:34 | Guarda L-4, mutante M3.
H7 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:83 | Mutantes M17a (reintroduce el sorteo)
L-8 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:77 | El procedimiento nunca muestra un negativo
L-8 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:128-130 | min(paso_dist$a, paso_dist$b, paso_tiem$a, paso_tiem$b) >= 0L)
L-9 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:86 | La clave no es la única cifra nueva y creíble
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
