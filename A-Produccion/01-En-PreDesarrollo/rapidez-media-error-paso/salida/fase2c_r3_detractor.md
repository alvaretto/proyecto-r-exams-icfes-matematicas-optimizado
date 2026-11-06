# Auditoría Detractor: rapidez-media-error-paso (SCHOICE v1), FASE 2C, ronda 3

**Fecha**: 2026-11-05
**Artefactos**: `.Rmd` md5 `25a924d4f407a4ea1f43644a564d5db3` y `verificar_espacio.R` md5 `41198f5ffb5b87fc64073b78e249f4a4`, comprobados al empezar y al terminar. No toqué nada del subproyecto.
**Objeciones**: 10 (0 críticas, 2 altas, 8 medias)

## Resumen ejecutivo

**CORRECCIÓN: 0 defectos.** Con función de verdad propia, sobre los 197 349 conjuntos de razones (incluidos los 507 de peso 0): 0 con más de una verdadera, 0 razones falsas que den `v` y 0 claves que no la den. Tampoco hay duplicadas ni negativos en el procedimiento. La canónica sigue siendo el impreso.

**O-1 tiene dos problemas que vuelven al profesor**, por su propio texto («se para y se lleva al profesor»):

1. **Una de las 9 cifras firmadas no mide lo que dice la tabla (H1).** `EST-afirmado-nuevo` trata el rótulo «Paso 2.» como una cifra vista en el estímulo. Sin ese artefacto da 50,95 %, +22,15 pp, y no 37,21 %, +8,41 pp. Con esa corrección, el verificador real sale con exit 3.
2. **Hay reglas sin cálculo por encima de +8 pp, sin firma ni batería (H2).** La más parecida a `Q` es `U`, «elige la opción en la que solo cambian las unidades»: +13,72 pp, y acierta el 100 % en `EV-C-UNIDAD`.

**El argumento que decidió la variante D no se sostiene tal como se escribió.** Decía que la peor regla sin cálculo bajaba de +17,03 a +14,49 pp. Medida sin artefacto, sube de +17,03 a +22,15 pp. No concluyo que D sea peor: `Q` acertaba el 100 % en el 27,8 % de las versiones. Concluyo que el profesor firmó con una cifra equivocada.

## Tabla de hallazgos

| ID | Severidad | Tipo | Hallazgo | Reproducción |
|---|---|---|---|---|
| H1 | ALTA | O-1 | `EST-afirmado-nuevo` cuenta los rótulos: +22,15 pp, no +8,41 | copia del verificador: exit 3 |
| H2 | ALTA | Diagnosticidad | reglas sin cálculo > +8 pp sin firma | enumeración exacta |
| H3 | MEDIA | O-1 | `X1a` en la mezcla da +8,04 pp y no se clasifica | 36,8385 % |
| H4 | MEDIA | O-1 | exit 4 con un generador cuyo reparto cambió | `.Rmd` mutado: exit 4, tabla idéntica |
| H5 | MEDIA | Testing | O1-E no prueba lo que dice | 58,64 % sin mutar |
| H6 | MEDIA | L-8 | un negativo en un operando no lo mata nada | 3 mutantes, 400/400 |
| H7 | MEDIA | Testing | la prosa de análisis no tiene sonda | 2 mutantes, 300/300 |
| H8 | MEDIA | Constructo | cabecera desactualizada; «− 0» no es constructo pleno | M0 = 75,00 % |
| H9 | MEDIA | Testing | M3, M21 y M17a prueban menos de lo que declaran | M3b |
| H10 | MEDIA | Evidencia | la nota visual remite a un registro inexistente | `v07` aparece 0 veces en `HANDOFF.md` |

## 1. CORRECCIÓN sobre la versión nueva

**Enumeración propia.** Reescribí el generador en Python desde el `.Rmd`, sin usar el R del verificador. Da 9 564 estados, 197 349 conjuntos y 196 842 de peso > 0, igual que `verificar_r9.txt`.

**Control positivo.** Con las ramas de signo, el mismo código reproduce las nueve cifras de `HANDOFF.md`, entre ellas `Q` 45,83 %, `X0` 40,55 %, `X1a` 46,43 %, `R20-120` 65,47 % y `R30-90` 76,71 %. También reproduce los 14 346 estados. Es decir, el enumerador sí dispara cuando la regla existe.

**Lecturas alternativas**, por cada 10 000 versiones no canónicas. Ninguna crea una segunda clave:

| Lectura | Peso | Por qué no cambia el veredicto |
|---|---|---|
| El paso de `EV-D-FINAL` es literalmente cierto en el viaje completo | 1 666,7 | El paso 2 mide el intervalo, y la Solution lo dice. Ninguna razón de tiempo da `v` |
| El paso de `EV-T-FINAL` es cierto con salida a las 12 | 59,9 | `repara()` retira la razón de distancia que lo completaría |
| Una razón de tiempo es literalmente la duración total, sin reparar | 69,4 | Sustituida no da `v` |

**Clave falsa:** el paso 3 nunca muestra `v` fuera de la rama del cociente (0 estados).

**Solution:** leí la versión completa de `EV-D-FINAL`, `EV-T-FINAL`, `EV-C-UNIDAD` y `EV-D-SUMA`, y la canónica en imagen a 320 px. Contrasté cada `descripcion_larga` del pool con el código. No hallé texto falso.

**Restos del diseño anterior:**

| Dónde | Qué queda | Efecto |
|---|---|---|
| `verificar_espacio.R`:27 | nombra «un negativo», que ya no existe, y no nombra «+» | H8 |
| `.Rmd`:218, 224 y 344 | `ofrecible`: ninguna entrada lo pone en FALSE | código muerto, inocuo |
| `verificar_espacio.R`:146 | `\\(?` para paréntesis que ya no salen | inocuo |
| `FMT-parentesis`, `FMT-sin-parentesis` | sin aplicabilidad | declarado en la línea 154 de `.claude/CLAUDE.md`; batería congelada |
| `Q`, `verificar_espacio.R`:431 | no puede disparar sin negativos | declarado; queda como vigía de L-8 |
| Canónica | la guarda dice «en el orden impreso», pero `exshuffle: TRUE` reordena al renderizar | inocuo: `v07` sale con el paso 3 primero, y ni la Solution ni el feedback dependen de la letra |

## 2. Auditoría de O-1

**Reproducción propia, sin `clasificar_o1`:**

| Regla | Exacta | Exceso |
|---|---|---|
| `X0` | 43,29 % | +14,49 |
| `EST-afirmado-nuevo` (con rótulos) | 37,21 % | +8,41 |
| `X1b` | 41,22 % | +12,42 |
| `X1` | 57,83 % | +29,03 |
| `R10-150` | 47,38 % | +18,58 |
| `R20-120` | 58,95 % | +30,15 |
| `R20-120+ent` | 70,05 % | +41,25 |
| `R30-90` | 72,12 % | +43,32 |
| `MAG-segundo-afirmado`, en no canónicas | 34,39 % | con la canónica al 100 %, mezcla 37,67 %: +8,87 |

Fuera de la firma, `X1a` da 36,15 % (+7,35) y `Q` 25,00 %. Repetí también el modo `exacto` real con este md5: da lo mismo que `verificar_r9.txt`, exit 4. Hacía falta porque ese archivo no registra el md5.

**Lo que preguntaste:**

| Comprobación | Resultado |
|---|---|
| ¿La corrección se salió del alcance? | No hallé nada. `RANGO_META` y L-2 siguen: con esos valores, mi modelo reproduce a la centésima las cifras de antes y las de la variante D, y hay 97 filas. **Límite:** el `.Rmd` md5 `2dfec0c9…` no está en git, ni en el scratchpad, ni en la caché del hook, así que no hay diff. Un cambio que no mueva distribución ni guardas no se distinguiría |
| ¿`FIRMA_O1` es literal? | Sí: las 9 cifras coinciden con `HANDOFF.md`:90-91 y con la tabla de O-1 |
| ¿Sin tope por familia ni estrato? | Sí, fila a fila |
| ¿Una regla nueva > +8 pp queda cubierta? | Dentro de la batería, no, en ningún ámbito. **Fuera sí se escapa:** ver H3 y H2 |
| ¿Exit 4 solo si todo se evaluó? | **No:** ver H4 |
| ¿Mutantes en copia (12)? | Sólidos para lo que tocan: mueven una fila +0,02 pp y comprueban que nada más cambie. Solo prueban `clasificar_o1`. Ninguno prueba el exit, ni una regla nueva en el ámbito mezcla, ni la rama `no_reproduce` en copia. Mi corrida real con exit 3 cubre el primer hueco |
| ¿Mutantes sobre el generador (4)? | O1-S, O1-I y O1-R tienen 0 abortos y prueban el cableado, aunque mueven varias reglas a la vez. O1-E no: ver H5 |

## 3. L-8

- **La guarda y el oráculo** ven los resultados `Dm`, `Tm` y `Vm`, no los operandos. Por eso sí hay otra forma de mostrar un negativo: ver H6.
- **M17b y M17c** prueban lo que dicen: con la segunda componente apagada, el negativo solo lo ve el oráculo.
- **M17a** muere por L-8, pero sin L-8 muere igual por L-1 en la semilla 2: «exactamente un paso está mal». No reintroduce el negativo, porque la entrada SIGNO ya no está en el `switch`. Solo prueba el orden de las guardas.

## 4. Reglas que aciertan sin calcular

Exactas, con los pesos reales, sobre no canónicas; mezcla con `P_CANON` = 0,05; exceso sobre 28,8 %. **Ninguna exige calcular.** Una regla nueva no cambia la vara del ciclo (§P7-C): se declara y la decide el profesor.

**Sin mirar la marca del paso** (ramas: `EV-C-UNIDAD` / `EV-D-FINAL` / `EV-D-SUMA` / `EV-T-FINAL`):

| Regla | Qué hace quien la usa | Exacta | Exceso | Mezcla | Canónica | Por rama |
|---|---|---|---|---|---|---|
| `EST` sin rótulos | descarta la cifra ya vista en enunciado o pasos | 50,95 | +22,15 | 49,65 | 25 | 10,7 / 70,0 / 75,6 / 69,4 |
| `RE` | elige la que rechaza un dato del enunciado | 50,00 | +21,20 | 48,75 | 25 | 25 / 75 / 25 / 75 |
| `U` | elige la que solo cambia las unidades | 42,52 | +13,72 | 45,39 | 100 | 100 / 13,4 / 12,5 / 14,6 |
| `NM` | descarta la que afirma más de lo que rechaza | 39,22 | +10,42 | 38,93 | 33,3 | 47,9 / 36,4 / 33,4 / 34,9 |
| `X0+` | `X0` y además sin negativas | 51,96 | +23,16 | 54,36 | 100 | 53,0 / 49,1 / 41,0 / 57,9 |
| `EST+` | `EST` sin rótulos y sin negativas | 54,01 | +25,21 | 52,56 | 25 | 10,7 / 74,6 / 83,9 / 72,1 |
| `E1` | positiva, no repite dato, no supera el dato mayor; el paso 3 no se toca | 55,56 | +26,76 | 57,78 | 100 | 75,0 / 45,8 / 45,8 / 45,8 |
| `E2` | `E1` y sin fracciones h/km | 82,48 | +53,68 | 83,36 | 100 | 87,5 / 80,3 / 81,2 / 79,2 |
| `E3` | `E2` y sin fracción menor que 1 | 100,00 | +71,20 | 100,00 | 100 | 100 en las cuatro |

**Con la marca del paso:**

| Regla | Qué hace quien la usa | Exacta | Exceso | Mezcla | Canónica | Por rama |
|---|---|---|---|---|---|---|
| `M0` | elige en el paso con la marca | 75,00 | +46,20 | 76,25 | 100 | 75 en las cuatro |
| `MU` | marca y, en h/km, `U` | 83,33 | +54,53 | 84,17 | 100 | 100 / 75 / 75 / 75 |
| `M2` | marca, positiva y no repite dato | 85,56 | +56,76 | 86,28 | 100 | 75,0 / 88,7 / 100 / 87,3 |
| marca, no repite dato, fracción no menor que 1 | | 89,97 | +61,17 | no medida | no medida | no medida |
| `M3` | marca, `E1` y fracción km/h mayor que 1 | 100,00 | +71,20 | 100,00 | 100 | 100 en las cuatro |

**Margen §P7-B:** en todas, la propiedad se comprueba en el texto, no se percibe a ojo. §P7-B exime lo imperceptible, no lo contable, así que no las exime. La única comparación de magnitud es la cota de `E1`: la suma supera al dato mayor por 15 % o más en el 77,3 % de las razones de distancia y en el 100 % de las de tiempo.

**`E3` y `M3` no son un accidente, son estructurales.** Las 8 razones falsas de distancia y tiempo son siempre un negativo, un dato repetido o una suma que excede el dato mayor. Las del cociente siempre van en h/km o dan una fracción menor que 1. La clave se puede hallar sin obtener nunca `D` ni `dur`. Son conjunciones de 4 o 5 cláusulas que ningún estudiante enunciaría entera, pero cada cláusula es natural.

**Contra el impreso.** La canónica cae al 100 % en `U`, `X0+`, `E1`, `E3` y `M0`, así que en esas reglas el ítem generado no es peor que el original. Las únicas de esta tabla que el impreso no tiene son `EST` y `RE` (25 %), y son las que L-8 hizo crecer:

| Regla | Con ramas de signo | Variante D |
|---|---|---|
| `EST` sin rótulos | +5,27 | **+22,15** |
| `EST+` | +7,16 | **+25,21** |
| `RE` | +10,09 | **+21,20** |
| `X0+` | +20,78 | +23,16 |
| `U` | +12,20 | +13,72 |
| `E1` | +26,76 | +26,76 |
| `M0` | +46,20 | +46,20 |

**Búsqueda automática.** Probé 20 875 conjunciones de 1 a 3 rasgos sin marca:

- La mejor es «no repite dato, no afirma más de lo que rechaza, sin h/km»: 72,82 %, +44,02 pp.
- La mejor de un solo rasgo es «no aparece en los pasos»: 47,62 %, +18,82 pp.
- Con la clave permutada, el máximo cae a 25,27 %, así que el resultado no viene de la selección.
- `X0` y `U` salen con la misma cifra que en mis otros cálculos.
- **Límite del control:** en el diseño anterior, el rasgo `opuesto` da 39,60 % y no quedó entre los 6 primeros, porque también dispara con razones falsas. El control de `Q` lo da la enumeración, no esta búsqueda.

**La regla `F`** («descarta el paso que afirma el dato final») nunca elimina la clave, por las precondiciones de L-4. Da 32,79 %, +3,99 pp, en zona gris.

## 5. Atención de la ronda 2

| Hallazgo r2 | Estado actual |
|---|---|
| r2-H1 doble punto | Corregido en `.Rmd`:264, guarda en la línea 463, M18 |
| r2-H2 prosa sin sonda | `chequeo()` incorporado y cumple lo pedido. Queda H7 |
| r2-H3 afirma lo que rechaza | Guarda en `.Rmd`:400, 0 casos, M21 y M22 |
| r2-H4 L-6 | Dos sondas, M23 y M24, y 0 comillas invertidas en los chunks |
| r2-H5 canónica | Declarado en `.claude/CLAUDE.md`:43-46; las cuentas 50, −10 y −40 son exactas |
| r2-H6 canales | Cableados en `FIRMA_O1`. Quedan H3 y H4 |
| Cabecera «− 0» | A medias: H8 |

**¿M21 equivale al mutante A?** En lo que prueba, sí: la guarda está viva. En lo que muta, no: A quitaba una precondición y M21 cambia `calcula`, y solo toca el rol tiempo. Sí hay análogos literales de A: sin la precondición de `EV-D-SUMA` muere en la semilla 35, y sin la de `EV-T-FINAL` en la 45, ambos por esa guarda.

**¿Apagar la guarda debilita M3 y M7?**
- **M7, no.** Muere en la semilla 34 con el paso 1 mal, un estado que «afirma lo que rechaza» no ve.
- **M3, sí.** Muere en la semilla 13 con `EV-D-FINAL`, un estado que la otra guarda también cubre. No prueba que L-4 aporte algo propio. Ver H9.

## Objeciones

### H1 — La cifra firmada de `EST-afirmado-nuevo` mide los rótulos «Paso n»

**Qué se cuestiona**: `nums()` extrae todo grupo de dígitos del estímulo, y el estímulo incluye «Paso 1.», «Paso 2.» y «Paso 3.». Por eso una clave de 2 h o 3 h cuenta como vista aunque no aparezca en ningún dato. En la semilla 2 la clave «2 h» no sale en el enunciado ni en los pasos, y la regla la descarta.

**Por qué** (Nivel 1, regla #22 §P7-E): la firma es regla por regla y sobre la cifra exacta. La glosa firmada dice «la cifra afirmada no aparece en el enunciado ni en los pasos», y esa lectura da otra cifra. Memoria `feedback_glosar_variables_desde_el_codigo`.

**Riesgo medido**:
- Exacta, 50,95 % (+22,15 pp); mezcla, 49,65 % (+20,85 pp). La firma dice +8,41.
- La rama `EV-T-FINAL` pasa de 28,2 % a 69,4 %. Muestra del generador real, N = 100 por rol: rol tiempo, 27,0 % contra 72,2 %.
- Copia del verificador con solo ese cambio: `EST-afirmado-nuevo` queda `pendiente_de_firma`, 8 cubiertas, 2 pendientes, **exit 3**.
- En el diseño anterior daba +5,27, y `HANDOFF.md`:83 comparaba +17,03 contra +14,49. La peor regla de una cláusula sin cálculo pasa de +17,03 a +22,15.
- Falsa también la frase de O-1 que llama a `X1b` «la única que entrena algo falso»: `EST` premia elegir la cifra no vista.

**Alternativa**: la batería sigue congelada (§P7-C), y esa regla no se toca.
1. Añadir una vigilada `EST-sin-rotulos`, con `nums(gsub("Paso [0-9]\\.", "", e))`, en los dos ámbitos.
2. Corregir la glosa de O-1.
3. Llevar al profesor +22,15 y +20,85 junto con la comparación corregida de variantes.

**Veredicto**: MODIFICAR O-1 y volver a firmar.

### H2 — Reglas sin cálculo por encima de +8 pp, sin firma

**Qué se cuestiona**: `U`, `RE`, `NM`, `X0+`, `EST+` y las de la sección 4. `U` es el análogo directo de `Q`: la opción que solo cambia las unidades acierta siempre en `EV-C-UNIDAD`. Se queda en 42,52 % porque `EV-C-UNIDAD` también se ofrece como razón falsa, que es la defensa de L-5.

**Por qué** (Nivel 1, regla #22 §P7-E): si aparece otro canal por encima de +8 pp, el override no lo cubre y vuelve al humano. §P7-C impide cambiar la vara, no declarar la cifra.

**Riesgo medido**: el de la tabla. `X1a` y las R premian comprobar; estas no comprueban nada.

**Alternativa**: declarar en O-1, regla por regla, al menos `EST` sin rótulos, `RE`, `U`, `NM`, `X0+` y `EST+`, y que el profesor decida. Si no acepta `EST`, la vía de raíz sería una razón falsa positiva, nueva y dentro de la cota: rompería `EST+`, `E1` y `E3`. **No la medí, no está autorizada por O-1 y sería otra variante.** No propongo tocar `RANGO_META`.

**Veredicto**: decisión del profesor. Sin cambio en el `.Rmd` mientras él no lo autorice.

### H3 — `X1a` pasa de +8 pp en la mezcla y no se clasifica

**Qué se cuestiona**: `clasificar_o1` evalúa `X1a` y `Q` solo en no canónicas. La mezcla solo se calcula para las 51 reglas de la batería.

**Por qué** (Nivel 1, §P7-E): `MAG-segundo-afirmado` se firmó justo en la mezcla, porque ese ámbito es lo que recibe el estudiante.

**Riesgo medido**: `X1a` da 36,1458 % y 50 % en la canónica. La mezcla da 36,8385 %, **+8,04 pp**. El verificador la declara «a 0,65 pp de +8» y sale con exit 4. También `R10-150` en la mezcla da +18,71 sobre +18,58 firmado, y eso no está declarado (`X0` sí).

**Alternativa**: calcular la mezcla para todo `MET`, añadir `X1a` y `Q` en los dos ámbitos, y llevar `X1a` al profesor.

**Veredicto**: MODIFICAR.

### H4 — Exit 4 con cosas sin evaluar

**Qué se cuestiona**: la enumeración no mide el generador, mide una copia de sus reglas. `ERRS`, `MULT`, el filtro de `cand` y `pst` están escritos en el verificador, y `error_cometido` se pisa con `.ST$err`.

**Por qué** (Nivel 1, regla #22 §P7-E y memoria `feedback_invariante_sin_guardia`): lo que clasifica el alcance debe quedar atado a lo que se emite.

**Riesgo medido**:
- Copia del `.Rmd` con `dist = "EV-D-SUMA",`, de modo que `EV-D-FINAL` nunca sale: modo `exacto` con exit **4**, 9 cubiertas, tabla O-1 idéntica, y la salida sigue listando la rama `EV-D-FINAL` con 16,7 %.
- En mi script, ese mutante pasa también guardas y oráculo en 100 de 100 semillas. No corrí el modo `todo` sobre él.
- El modo `exacto` solo también sale con exit 4 sin haber corrido oráculo ni mutantes. Lo comprobé.
- Una regla que lance un error puntúa 25 % en silencio (`verificar_espacio.R`:415). Las 42 reglas no firmadas podrían fallar sin que nada lo diga.

**Alternativa**:
1. Evaluar los brazos del `switch` real con `pick <- identity` y exigir `identical(..., ERRS)`.
2. Cambiar el ancla por `stopifnot(.ST$d1 %in% cand); d1 <- .ST$d1`.
3. Contar los errores capturados, y que cualquiera dé exit 1.
4. Reservar el exit 4 para `todo`.

**Veredicto**: MODIFICAR.

### H5 — O1-E no prueba lo que dice

**Qué se cuestiona**: aborta 6 241 de 9 564 estados y mide sobre el 24,43 % del peso. Se le acepta como «cazado» aunque saca 22 reglas a la vez.

**Por qué** (Nivel 1, §P7-E): el mutante debe sacar de «cubierto» esa regla por su causa.

**Riesgo medido**: el generador **sin mutar**, restringido a esos mismos estados, ya da `X1b` 58,64 % y `EST` 58,99 %. Basta la selección de estados, sin el filtro. Lo único que prueba es que `clasificar_o1` reacciona si toda la distribución cambia, y eso ya lo prueba O3 mejor.

**Alternativa**, medida en mi modelo: no abortar, sino multiplicar `pesos` por 0,3 por cada razón entera del conjunto. Con 0 abortos, `X1b` da 45,19 % (+16,39 pp), `X0` 43,36 % y `EST` 34,87 %.

**Veredicto**: REEMPLAZAR O1-E.

### H6 — L-8 no ve un negativo en un operando

**Qué se cuestiona**: la guarda mira `min(Dm, Tm, Vm)` y el oráculo mira `c1, c2, z3`. La regex del oráculo sigue aceptando operandos negativos.

**Por qué** (Nivel 1, regla #9 v1.5): L-8 dice «nunca muestra un negativo», y su guarda solo cubre el resultado.

**Riesgo medido**: hoy, 0 estados. Tres mutantes sobreviven 400 de 400 semillas a guardas y oráculo. El primero muestra «140 km − −20 km = 160 km» y el segundo «−140 km − −560 km = 420 km». Un negativo solo en el texto del paso 3 sí muere, por `ORACULO_ARITMETICA`.

**Alternativa**: añadir a la guarda `min(paso_dist$a, paso_dist$b, paso_tiem$a, paso_tiem$b) >= 0`, al oráculo `any(c(a1, b1, a2, b2, x3, y3) < 0)`, y un mutante M17d. No cambia ninguna cifra firmada, porque solo es guarda.

**Veredicto**: MODIFICAR.

### H7 — La prosa de análisis y de cada razón no tiene sonda

**Qué se cuestiona**: `chequeo()` solo exige que cada razón falsa esté citada, no que su análisis sea cierto.

**Por qué** (Nivel 1, regla #13 v1.1): los códigos comparan valores, no evalúan si un texto es verdadero.

**Riesgo medido**: hoy, 0. Leí esos textos contra el código. Dos mutantes sobreviven 300 de 300: uno cambia la cifra de `EV-D-INICIAL` por `d2`, y el otro pone `tramo_d` en el análisis de `EV-T-FINAL`, de modo que la Solution llama tiempo a una distancia.

**Alternativa**: exigir en `chequeo()`, por código presente, su frase con las cifras del enunciado, por ejemplo `sprintf("%d km es lo que", d1)`, `sprintf("%d h es una hora del reloj", t2)` y la frase del tramo en cada `analisis`.

**Veredicto**: MODIFICAR el verificador.

### H8 — La cabecera de constructo está desactualizada y «− 0» no es constructo pleno

**Qué se cuestiona**: la línea 27 sigue nombrando «un negativo» y **no nombra «+»**, aunque se dé por declarada. La línea 28 afirma que esas lecturas son el constructo.

**Por qué** (Nivel 1, L-1): «revisar solo las cuentas nunca basta». La pregunta del ítem tiene dos mitades, en qué paso y por qué.

**Mi posición, marca por marca:**
- **`h/km`: sí.** Exige contrastar con «en km/h». El impreso se resuelve así al 100 %.
- **`+`: sí.** Exige saber que dos posiciones desde un mismo punto no se suman.
- **`− 0`: parcial.** Basta ver que se resta algo que no hace nada, sin leer el enunciado. `RE` lo detecta desde las opciones. Está en el 50 % de las no canónicas y el impreso no lo tiene.

**Riesgo medido**: el 100 % de las no canónicas lleva marca, y `M0` da 75,00 % (+46,20 pp). Con paso no repetido (P = 1/2), el «por qué» no se evalúa: la marca sola acierta siempre. Trade-off legítimo: el impreso da 100 %, así que la variante es más exigente. Por eso no pido cambiar el diseño.

**Alternativa**: reescribir la cabecera con las tres marcas, separar `− 0` como constructo parcial y declarar `M0` y `RE` con cifra en los residuos.

**Veredicto**: MANTENER el diseño; MODIFICAR la declaración.

### H9 — M3, M21 y M17a prueban menos de lo que declaran

**Qué se cuestiona**: M3 muere en un estado que la otra guarda también cubre. M21 no es la mutación de A. M17a muere igual sin L-8.

**Por qué** (Nivel 1, contrato de mutación del repo y memoria `feedback_invariante_sin_guardia`): cada mutante debe probar la aportación de su guarda.

**Riesgo medido**: M3b ofrece `EV-D-FINAL` solo contra `EV-D-SUMA`. Con la otra guarda encendida, muere en la semilla 35 por L-4. Sin L-4 sobrevive 800 de 800: **L-4 es la única defensa en ese estado**, y M3 no lo mostraba.

**Alternativa**:
1. Sustituir M3 por M3b, sin apagar nada.
2. Añadir a M21 el mutante sin la precondición de `EV-D-SUMA`.
3. Hacer que M17a añada también la entrada al `switch`.

**Veredicto**: MODIFICAR.

### H10 — La auditoría visual remite a un registro que no existe

**Qué se cuestiona**: la nota dice que el coordinador revisó las canónicas por imagen, «ver `HANDOFF.md`», pero ese archivo es de las 13:58, anterior a la auditoría, y no menciona ninguna. El auditor reconoce 8 + 4 versiones contra 16 + 16.

**Por qué** (Nivel 1, regla #16): se audita por artefacto, no por flag. Regla #23: declarar el N junto al resultado, y aquí está declarado.

**Riesgo medido**: bajo en sustancia. La nota acierta: las canónicas de las semillas 1 a 100 son 7, 27, 47, 54 y 66, y las ramas dan 18, 16, 26 y 40. Miré `v07` a 320 px: 0 defectos, ancho 304 px, coherente con los 305 declarados. Las capturas no están en `salida/`. Fuera de esos dos puntos, falta el registro.

**Alternativa**: asentar qué versiones vio el coordinador antes de sellar, o completar las 16 + 16 con las 5 canónicas.

**Veredicto**: MODIFICAR el registro.

## Invariantes locales

| Invariante | Estado |
|---|---|
| L-1 | Se cumple: 0 estados con cuenta mala ni con más de un paso mal |
| L-2 | Se cumple: 97 filas, guarda viva |
| L-3 | Se cumple en texto. Contrasté enunciado, pasos y opciones contra el recorte; el orden en el render lo sortea `exshuffle` |
| L-4 | Se cumple: 0 razones que reparen, en 197 349 conjuntos. Canónica declarada |
| L-5 | Las cifras históricas están rotuladas como tales. Lo vigente queda afectado por H1 y H2 |
| L-6 | Se cumple, con sondas |
| L-7 | Se cumple: leí las 9 plantillas en el fuente |
| L-8 | Se cumple hoy, con guarda incompleta (H6) |

## Los 8 dominios y O-1

- **Código R-exams**: sin abortos en 9 564 estados. Queda `ofrecible` muerto.
- **Pedagógico**: las secciones de la Solution están y las plantillas cumplen la regla #11. Sin datos de aula: el Nivel 3 sigue sin evidencia.
- **Visual**: PDF y HTML sin defectos en lo que vi.
- **Gramática**: sin hallazgos en las 5 versiones leídas.
- **Coherencia matemática**: sin hallazgos.
- **ICFES metacognitivo**: los `exextra` coinciden con `ITEM_OFICIAL.md` en un cotejo a ojo. No corrí el validador de literalidad en esta ronda.
- **Testing**: H3 a H7 y H9.
- **Semántico**: sin hallazgos de CORRECCIÓN; lo pendiente es diagnosticidad.
- **Override e invariantes locales**: H1 a H5.

## Lo que ejecuté

- `verificar_espacio.R exacto` sobre el md5 auditado: exit 4, mismas cifras que r9.
- Enumerador propio en Python, sobre el diseño actual y el anterior.
- Dos copias del verificador en corrida exacta real: con la regla sin rótulos, exit 3; con el `.Rmd` mutado, exit 4.
- 11 mutantes propios, más M3 con su guarda encendida.
- 5 versiones leídas completas y la búsqueda de 20 875 conjunciones.

Scripts y salidas:

- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0d13b1e6-8d43-4929-9f2b-9b27caa5513b/scratchpad/det3/modelo.py`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0d13b1e6-8d43-4929-9f2b-9b27caa5513b/scratchpad/det3/reglas.py`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0d13b1e6-8d43-4929-9f2b-9b27caa5513b/scratchpad/det3/busqueda.txt`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0d13b1e6-8d43-4929-9f2b-9b27caa5513b/scratchpad/det3/exacto_est.txt`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0d13b1e6-8d43-4929-9f2b-9b27caa5513b/scratchpad/det3/exacto_peso.txt`
- `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/0d13b1e6-8d43-4929-9f2b-9b27caa5513b/scratchpad/det3/mut2.R`

Las 400 y 800 semillas son búsqueda de supervivientes, no medición. Las cifras de muestra van con N = 100 por rol (regla #23).

## No auditado

- **DOCX en Word.** Solo lo convertí con LibreOffice y miré la página 1 de una versión: 37 fórmulas OMML, 0 `$`, 0 barras invertidas.
- **Escaneos NOPS: nada.** Del PDF solo comprobé que tiene 4 páginas A4; la página que abrí, la 2, está en blanco. La página de la pregunta no la miré.
- **Modos `mutantes`, `mutantes_o1` y `todo` del verificador:** no los repetí; usé `verificar_r9.txt`.
- **Pasos 8, 9 y 10 sobre este md5.** Los archivos en `salida/` son del md5 anterior.
- **`/detractor-hetero`.**
- **La vara del corpus oficial** para las reglas de la sección 4.

## Veredicto global

**APROBAR_CON_CAMBIOS.** Hay cero defectos de CORRECCIÓN, así que no corresponde RECHAZAR. Las dos ALTAS son de diagnosticidad y de alcance firmado, y por el texto de O-1 bloquean la FASE 3 hasta que decida el profesor.

## Próximos pasos

1. Llevar al profesor H1, H2 y H3: la cifra corregida +22,15, la comparación con +17,03, las reglas nuevas con su cifra y `X1a` +8,04. Él decide si firma o abre otra variante. No se reajusta nada antes.
2. Corregir en el verificador H3, H4, H5, H7 y H9, y en el `.Rmd` la guarda de H6. Solo H6 cambia el md5 del `.Rmd`, sin mover ninguna cifra.
3. Corregir la cabecera y las declaraciones (H8), y el registro visual (H10).
4. Si el `.Rmd` cambia, otro detractor sobre el md5 nuevo, según la regla #9.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:296 | function(x) as.numeric(regmatches(x, gregexpr("[0-9]+", x))[[1]])
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:375 | L("EST-afirmado-nuevo", "lexico", function(o, e) conj(!(abs(num1(o)) %in% nums(e))), est = TRUE),
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:474 | ee <- ver(paste(e$enunciado, e$paso_1, e$paso_2, e$paso_3))
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:204 | paso_2 <- paste0("**Paso 2.** Se calcula el tiempo que transcurrió: "
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:115 | la cifra afirmada no aparece en el enunciado ni en los pasos
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/HANDOFF.md:83 | la peor regla sin cálculo del diseño actual es +17,03 pp; en D es
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:508 | firmado = c(14.49, 8.41, 12.42, 29.03, 18.58, 30.15, 41.25, 43.32, 8.87)
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:126 | Una regla nueva por encima de +8 pp **no** queda cubierta.
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:127 | cifras, o alguna regla pasa de su alcance, se para y se lleva al profesor: no se reajusta nada.
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:58 | también se ofrece como razón falsa contra un paso 3 correcto
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:297 | function(p) p$rol_clave != "coc", function(p) c(p$Dm, p$Tm),
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/docs/reglas/diversidad-sustantiva.md:460 | o aparece otro canal por encima de +8 pp, el override no lo cubre y vuelve al humano
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/docs/reglas/diversidad-sustantiva.md:454 | que exime lo imperceptible, no lo contable
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:526 | data.frame(regla = c("X1a", "Q"), ambito = "nocanon", stringsAsFactors = FALSE))
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:515 | mezcla <- (1 - P_CANON) * tot[nr] + P_CANON * canon[nr]
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:157 | +7,35 pp, a 0,65 pp de +8
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/salida/verificar_r9.txt:129 | X1a  36.15   7.35       50
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:443 | error_cometido <- .ST$err
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:410 | ERRS <- list(dist = c("EV-D-SUMA", "EV-D-FINAL"), tiem = "EV-T-FINAL", coc = "EV-C-UNIDAD")
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:458 | m0 <- MULT[f$dur - 1L]; cand <- seq(m0, 2L * f$v, by = m0)
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:460 | pst <- 1 / nrow(esp) / length(cand) / 3 / length(errs[[rol]])
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:415 | error = function(e) rep(FALSE, 4L))
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:854 | O1_NOREP > 0L) 3L else 4L)
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:105 | dist = pick(c("EV-D-SUMA", "EV-D-FINAL")),
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:818 | list(id = "O1-E", sonda = "X1b",
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:830 | "cazado_por_su_sonda_y_otra"
H5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/salida/verificar_r9.txt:284 | abortos 6241 (peso que sobrevive 0.2443)
H6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:128 | !endsWith(error_cometido, "SIGNO") && min(Dm, Tm, Vm) > 0)
H6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:156 | if (any(c(c1, c2, z3) <= 0)) return(list(problema = "ORACULO_L8: un paso muestra un negativo"))
H6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:144 | (-?[0-9]+) km ([-+]) (-?[0-9]+) km = (-?[0-9]+) km$
H6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:77 | L-8 · El procedimiento nunca muestra un negativo
H7 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:123 | p <- c(p, "FALSA no analizada")
H7 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:254 | paste0(M(km(d1)), " es lo que ", veh$el, " llevaba recorrido a las ", t1s,
H7 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:264 | " El tiempo transcurrido entre las dos lecturas es ",
H8 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:27 | paso que muestra un negativo
H8 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:28 | Eso ES el constructo que se evalúa: localizar el error.
H8 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/rapidez_media_error_paso_numerico_variacional_argumentacion_n3_schoice_v1.Rmd:111 | rol_repetido <- sample(ROLES, 1L, prob = ifelse(ROLES == rol_clave, 1 / 2, 1 / 4))
H8 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:135 | el paso 2 conserva un solo error cometido
H9 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:725 | sin_guarda_afirma(mutar('function(p) p$rol_clave != "dist", function(p) d2,', 'function(p) TRUE, function(p) d2,'))
H9 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:768 | list(id = "M21", sonda = "ninguna razón afirma lo que rechaza", cod = mutar('function(p) TRUE, function(p) t1),', 'function(p) TRUE, function(p) p$Tm),')
H9 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/verificar_espacio.R:755 | list(id = "M17a", sonda = "L-8: el procedimiento nunca muestra un negativo"
H10 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/salida/auditoria_visual_r3.md:1 | Por imagen las revisó el coordinador, ver
H10 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/HANDOFF.md | AUSENTE: v07, v27
H10 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/salida/auditoria_visual_r3.md:19 | Incumplo el mínimo pedido de 16 versiones por imagen y ancho.
L-1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:10 | En ninguna versión el error es de aritmética
L-2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:16 | Las condiciones de divisibilidad valen para todas las versiones
L-3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:23 | La canónica es el impreso, verbatim
L-4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:29 | Ninguna razón falsa repara el procedimiento
L-5 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:48 | El formato no delata la clave
L-6 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:61 | Formato del fuente
L-7 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:72 | La primera hora ancla la primera distancia
L-8 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/rapidez-media-error-paso/.claude/CLAUDE.md:77 | El procedimiento nunca muestra un negativo
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
