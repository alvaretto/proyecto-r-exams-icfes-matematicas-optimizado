<!-- Reporte íntegro del AgenteDetractor de la FASE 2C r6 (tanda S2, .Rmd 63e2713e), copiado de su transcripción por el coordinador el 2026-11-03 (mensaje final: 2026-11-03T13:51:07.871Z). -->

Ninguna objeción es de CORRECCIÓN, pero hay una alta. La resta de los dos promedios de la tabla delata a la sede ganadora y deja ver la clave sin calcular ningún total, y el verificador da ese canal por cubierto.

Mi veredicto es APROBAR_CON_CAMBIOS: hay 4 objeciones (1 alta y 3 bajas).

# FASE 2C r6: auditoría del SCHOICE `totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1`, ciclo 2026-11-02, tanda S2a

**Fecha:** 2026-11-03.

**Objeto.** Comprobé con `md5sum`, antes de empezar, que los tres archivos coinciden con lo encargado:
- `.Rmd`: `63e2713e6ac0bdbf250f4dc33901b2ec`.
- `verificar_espacio.R`: `bdd0daf06ba933a1eff873770a755d59`.
- `cap_s1b2.py`: `1d0117ed4fe86504eec07fa8f5a28c49`.

**Independencia.** Soy un detractor nuevo: no audité ninguna versión anterior y no escribí el artefacto. Trabajé solo en lectura: no creé ni modifiqué nada dentro del repo. Mis scripts y salidas están en `/home/bootcamp/.cache/fase2c_r6/`: `oraculo_r6.R` y `oraculo_r6.txt`, `canal_resta.R` y `canal_resta.txt`.

## Resumen

**CORRECCIÓN limpia.** Mi función de verdad lee solo lo que emite R/exams, ya mezclado. En las 100 versiones de E1:
- hay una sola afirmación verdadera, y coincide con `exsolution`;
- la Solution y el feedback son verdaderos;
- las 9 canónicas reproducen el impreso.

**La S2a hace lo que dice:**
- el punto de ERR-RESTA va dentro de la fórmula;
- las sondas y la guarda del signo están generalizadas;
- los controles son exactos;
- los mutantes M34 a M41 funcionan, y también la guarda de H5 con M40;
- `cap_s1b2.py` es equivalente al anterior.

Busqué signos fuera de una fórmula en el texto emitido y en HTML, Moodle, TeX y DOCX, y no hay ninguno.

**Lo nuevo es H1 (ALTA, DIAGNOSTICIDAD).** Hay un canal que el verificador no mide: la cifra que es la resta de los dos promedios de la tabla. Esa cifra delata a la sede que gana y, con ella, a su distractor.
- Con «S_U», el atajo llega al 78,5 % en la rama PROMEDIO, por encima de la cifra más alta firmada en esa rama (74,5 %).
- Con las tres pistas firmadas, llega al 94,5 % en CURSOS y al 98,0 % en PROMEDIO.

Por «O-1 corregido» § Alcance, vuelve al profesor. No lo introdujo la S2a.

H2 a H4 son endurecimientos del verificador, de la guarda del `.Rmd`, del registro y del instrumento.

## Hallazgos

### H1. Canal sin medir: la resta de los promedios de la tabla delata a la sede que gana y a su distractor (ALTA, DIAGNOSTICIDAD; no es CORRECCIÓN)

**Qué se cuestiona.**
- ERR-RESTA va siempre del lado de G (`lado = "G"`, línea 257) y cita δ = |a_G − a_P| (línea 263).
- En las no canónicas, ninguna otra opción puede citar δ:
  - la cifra de P es n_P·δ, con n_P ≥ 3, o k·a_P ≥ 20 > 15 ≥ δ;
  - las otras cifras de G valen 20 o más;
  - la razón sin sustento excluye `calculos_tabla()`, que incluye δ;
  - el canal 8 impide que dos opciones repitan cifra.
- Por tanto, quien resta los dos promedios de la tabla y encuentra esa cifra en una opción sabe dos cosas sin calcular ningún total: que esa sede ganó y que la clave es la otra afirmación de esa sede.
- Es la misma clase de atajo que S1, que está firmada en O-1. En el lado de G, las tres pistas firmadas cubren ERR-DATO (S1), ERR-PARCIAL (suma) y ERR-TOTAL (atípica). ERR-RESTA era el único código sin pista.

**Por qué el verificador no lo ve.**
- R_dif busca una cifra que sea diferencia de otras dos cifras de las opciones (`outer(v[-i], v[-i], "-")`, línea 307), no de los promedios de la tabla.
- En E4 y en la batería, la tabla solo se usa como pertenencia (`%in% x$tabla`, líneas 288, 289, 310, 319, 323 y 389) o en el canal 3 (línea 385).
- `ESTIMULO_SC` (línea 497) declara que los canales de la tabla se miden en E4.
- Resultado: el verificador sale con exit 4 y declara todo dentro de lo firmado.

**Evidencia** (`/home/bootcamp/.cache/fase2c_r6/canal_resta.R`).
- Usé los estratos del verificador, con N = 100: las semillas 1 a 100 (E1) y, desde la 1001, las 100 primeras no canónicas de cada rama (E2).
- Como control, mi reimplementación de «S_U + S1 + suma + atípica» da 77,0 / 81,5 / 74,5 / 100,0 %. Son las cifras del verificador (`verificar_espacio_s2a.txt`, líneas 60 y 89) y las firmadas.

| Regla (acierto) | E1, mezcla | CURSOS | PROMEDIO | Canónica (exacta) |
|---|---|---|---|---|
| «resta» sola: gana la sede cuya opción cita la resta de los promedios; elige la otra de esa sede | 54,2 % (+29,2 pp) | 58,8 % (+33,8 pp) | 67,8 % (+42,8 pp) | 0,0 % (−25,0 pp) |
| S_U (control; firmado: 50,0 %) | 50,0 % | 50,0 % | 50,0 % | 50,0 % |
| S_U + resta | 71,0 % | 72,5 % | **78,5 %** | 50,0 % |
| S_U + S1 + suma + atípica (control; firmado: 81,5 / 74,5 %) | 77,0 % | 81,5 % | 74,5 % | 100,0 % |
| S_U + S1 + suma + atípica + resta | **93,0 %** | **94,5 %** | **98,0 %** | 100,0 % |

- La regla aplica en el 45 % de las versiones de CURSOS y en el 57 % de las de PROMEDIO. En las 144 no canónicas en que aplica, la opción que cita la resta es de la sede que gana: 144 de 144.
- En la canónica la regla es antipredictiva: la C del impreso cita 5 = 45 − 40 y nombra a la sede que pierde. Su peso en la mezcla es de 0,10 × (−25,0) = −2,5 pp.
- La regla es discreta (igualdad exacta), así que no tiene margen en el sentido de §P7-B, como las de O-1.
- El propio verificador deja como pendiente toda regla que pase de +8 pp sobre el 25 % (R_dif y «Atípica», líneas 447 a 450). Con ese criterio, «resta» quedaría PENDIENTE en las dos ramas.
- **No lo introdujo la S2a.** El sorteo de códigos no ha cambiado desde `e293d5c8`: la invariancia de objetos, opciones, clave y rama está registrada en S1, S1b y S2a. Por esa invariancia, sin medirlo aparte, el canal rige igual en la versión aprobada para el aula y en la Parte 1 del CLOZE (E1, F-2).

**Riesgo concreto.**
- En un banco de práctica con versiones al azar (la razón de ser de L-3), quien aprendió el patrón responde sin calcular ningún total en el 93-98 % de las versiones.
- La cifra firmada en «O-1 corregido» era del 77,6 % en la mezcla, y con ella se firmó también F-3.
- El verificador lo reporta como cubierto.

**Alternativa.** §P7-D está agotado, así que el rediseño no puede ser autónomo (H-5).
1. Cablear en E4 «resta», «S_U + resta» y «S_U + S1 + suma + atípica + resta», por rama y con la canónica exacta, con el criterio de R_dif.
   - El verificador pasaría a exit 3 hasta la firma.
   - El mutante, al estilo de M19: ERR-RESTA siempre que esté disponible, y la regla tiene que salir de lo firmado.
2. Llevarlo al profesor en el WAIT_USER #3, regla por regla (§P7-E), con estas cifras.
   - Si prefiere cerrarlo, hay que medir antes las opciones (§P7-F).
   - Una sería que la resta de los promedios salga también del lado de P, como en la C del impreso («tuvo mayor promedio con δ kilos de más»), para que citar δ deje de nombrar al ganador.
   - Eso toca el `.Rmd` y gastaría una pasada que ya no queda: decide él.
3. Corregir `ESTIMULO_SC`, o añadir una regla del estímulo que calcule con la tabla (§P7-G).

**Veredicto:** MODIFICAR el verificador y el registro. El diseño lo decide el profesor. Bloquea la FASE 3 hasta su firma o su decisión.

### H2. Los mutantes de sonda de la S2a no aíslan su sonda: `juzgar_s1()` ignora los controles al juzgarlos (BAJA, cobertura del verificador)

**Qué se cuestiona.** El criterio de `juzgar_s1()` no es el mismo para los dos tipos de mutante:
- **Mutante de control** (línea 913): si falla otro control o alguna sonda, el veredicto es «cazado_por_otra».
- **Mutante de sonda** (línea 911): solo mira las otras sondas y no mira los controles.

Los siete mutantes de sonda de la S2a también hacen fallar los controles exactos, y aun así salen como «cazado_por_su_sonda» (`verificar_espacio_s2a.txt`, líneas 242 a 248):
- el mutante 38 baja «unidad dentro» a 958 y «unidad con el signo dentro» a 530: los dos fallan;
- los mutantes 39a, 39b y 39c, lo mismo: 958 y 530;
- los mutantes 41a, 41b y 41c dejan «unidad dentro» en 1158 y bajan «unidad con el signo dentro» a 530, que falla.

El mutante 32 hace caer el control del signo de 730 a 530 por construcción, y su comprobación no lo mira.

**Consecuencia.**
- Estos mutantes prueban que las sondas ampliadas son sensibles: si se quedaran ciegas, saldrían «no_detectado».
- No prueban que la clase ampliada sea necesaria. Con los controles exactos, estos mismos defectos dejarían el verificador en FALLA también con la clase anterior (958 ≠ 1158 y 530 ≠ 730).
- Lo único que aporta la clase ampliada es ver defectos **aditivos**: una fórmula nueva, sin `\ \text{kilos}$`, seguida de un separador y un signo o la unidad. Es la clase de defecto de H2 de la r5, y ningún mutante la prueba.
- La fila 2 de la tabla candidata presenta M32, M38 y M41a a M41c como «la sonda».

**Evidencia.** Hice un mutante aditivo, D2: una frase nueva antes de «Propiedades del concepto», `Comprobación: $D$`, seguida de U+2009 y un punto. En las semillas 1 a 100:
- S1-Sol signo lo detecta en 100 casos;
- los controles no se mueven: 1158 / 730 / 42;
- S1 y S1-Sol fórmula dan 0;
- la sonda de la S1b, que exigía `\text{kilos}`, da 0.

**Alternativa** (es endurecer y solo toca el verificador):
1. Añadir dos mutantes aditivos, con los tres controles exactos y la clase anterior en 0:
   - M42 para S1-Sol signo, como D2, con U+2009, U+202F y U+2060;
   - M43 para S1-Sol fórmula: `$D$`, U+2060 y «kilos».
2. Hacer simétrico `juzgar_s1()`: si falla algún control, el veredicto pasa a ser «cazado_por_su_sonda_y_control».

### H3. La guarda de V1 (línea 594) sigue siendo una lista negra, y la fila 1 de la candidata no lo dice (BAJA, guarda del `.Rmd` y registro)

**Qué se cuestiona.**
- La guarda de la línea 595 usa `\$[ ED]+(kilos?|cursos?)\b`: solo ve el espacio normal y U+00A0.
- La de la línea 602 admite hasta tres caracteres cualesquiera que no sean letra ASCII, cifra, `$` ni salto de línea.
- La fila 1 de la candidata atribuye «cualquier separador que no sea letra ni cifra» a la sonda, lo que es correcto. Pero no dice que la guarda es más estrecha.
- La fila 2 dice «cualquier separador» de la guarda de la línea 601. En realidad son tres caracteres como máximo: á, é y ñ cuentan como separadores, y cuatro o más ya no se ven.

**Evidencia.**
- D3: el patrón de la línea 595 da FALSE sobre `= 20$` + U+2009 + `kilos y`; la clase de la línea 602 da TRUE.
- D1: en memoria, cambié el feedback de la clave a `"$", intToUtf8(0x2009), "kilos: ganó por $"`. En las semillas 1 a 100:
  - el `data_generation` no aborta ninguna vez: no lo ve ni la guarda de la línea 594 ni la de la 601;
  - S1-Sol fórmula lo ve en 100 casos, y los controles bajan a 1058 / 630.
- Es decir, el defecto solo se caza cuando alguien corre el verificador, no al renderizar.

**Alternativa.**
- (a) Endurecer: usar en la línea 595 la misma clase, `"\\$[^A-Za-z0-9$\\n]{0,3}(kilos?|cursos?)\\b"`, con D1 como mutante. D1 tiene que morir por «L-6: la unidad que cierra…». Cambia el `.Rmd`, pero no el texto emitido.
- (b) Sin tocar el `.Rmd`, escribirlo en la tabla:
  - en la fila 1, que la guarda solo ve el espacio normal y U+00A0, y que el resto lo cubre la sonda;
  - en la fila 2, la clase exacta en lugar de «cualquier separador».
- Como la candidata se firma en el WAIT_USER #3, al menos (b).

### H4. `cap_s1b2.py` documenta `python3 -I`, y con `-I` no se puede ejecutar en este entorno (BAJA, instrumento)

**Evidencia.**
- La línea 16 dice «Uso: python3 -I cap_s1b2.py …», y la línea 19 hace `import websockets`.
- `websockets` está solo en el site de usuario (`~/.local/lib/python3.14/site-packages`), y `python3 -I -c "import websockets"` da `ModuleNotFoundError`.
- La 6b de la S2a declara la desviación en la línea 9 de su informe.
- La S2a editó este archivo por H4 de la r5 y dejó esa línea.

**Riesgo.** La próxima corrida repite la desviación, o alguien lo ejecuta sin `-I` desde dentro del repo.

**Alternativa.** Documentar `python3 -E -P cap_s1b2.py …`, que conserva el site de usuario y no mete el directorio del script ni el cwd en `sys.path`, con un assert de que el cwd está fuera del repo. La otra opción es un venv fuera del repo con `websockets`, ejecutado con `-I`.

## Lo que comprobé y salió bien

**1. CORRECCIÓN, sobre `63e2713e`** (`oraculo_r6.R`).

Método:
- Usé `xexams()` + `read_exercise()`, es decir, lo que queda tras la mezcla de R/exams, con las semillas 1 a 100 (= E1). Son 9 canónicas, 44 versiones de CURSOS y 56 de PROMEDIO.
- La función de verdad lee solo la tabla emitida, los nombres del enunciado y las opciones.

Resultado: 0 fallos.
- **Clave.**
  - Una sola afirmación verdadera en 100 de 100, igual a `exsolution` tras la mezcla en 100 de 100.
  - El reparto 2 + 2 se cumple, y solo una de las de G cita D.
- **Solution.**
  - «La respuesta correcta es la que afirma…» cita la verdadera. Todos los productos y todas las restas son correctos.
  - Son verdaderos la lista de totales, «Ganó …», la frase de la rama y «Las dos afirmaciones…». Los tres análisis son de las tres falsas.
  - Las cifras de cada análisis son correctas: ERR-DATO, ERR-RESTA (δ, máximo, mínimo, cursos y totales), ERR-TOTAL, «Es cierto que…», «olvida que…» y la cifra de ERR-INVENTA.
  - También son correctas las 191 frases «… no ganó: recogió …» y el «Caso específico».
  - No nombra ninguna letra.
- **Feedback.** El veredicto de cada opción es el correcto, y también su contenido: gdis; padv, con la ventaja real de P; psin y la clave.
- **Canónica** (9 versiones):
  - las opciones son iguales a las del impreso (U+00A0 pasado a espacio), y el enunciado también;
  - la tabla es 5/40 y 4/45, con el encabezado «recolectado»;
  - la clave es la B.
  Miré `orig_pregunta_50.png`.

**2. Signo tras una fórmula** (punto 1 del foco). Mi detector empareja los `$` de cada línea y admite cualquier número de separadores. No depende del regex de la S2a.

| Texto medido | Signos fuera | Unidades fuera | Signos dentro sin unidad |
|---|---|---|---|
| `63e2713e`, semillas 1-100 | 0 | 0 | 42 |
| `63e2713e`, semillas 1-1000 | 0 | 0 | 425 |
| `6c9d7705`, semillas 1-100 | 42 (= S1-Sol signo, 42) | 0 | 0 |

**Falsos positivos de la clase ampliada.** Ninguna apertura de `$` casa con ella, con los dos md5 y con 100 y 1000 semillas. Para que pasara haría falta una fórmula que empiece con un signo tras hasta tres caracteres que no sean letra ni cifra, como `$.5$`, y aquí no hay ninguna.

**Formatos:**

| Formato | Archivos | Cierres de fórmula | Signo fuera | Unidad fuera |
|---|---|---|---|---|
| HTML de medición | 100 | 1306 `</math>` | 0 | 0 |
| HTML oficial | 3 | 38 `</math>` | 0 | 0 |
| Moodle | 100 preguntas | 1307 `\)` (40 son `\text{.}\)`) | 0 | 0 |
| TeX | 101 | 1306 `\)` | 0 | 0 |
| DOCX | 3 | 38 `oMath` | 0 | no medido |

**3. El diff del `.Rmd`.** Solo cambian:
- 3 líneas de comentario;
- el sitio de ERR-RESTA (línea 265);
- la guarda de la línea 601 (mensaje y clase).

En la tabla firmada de L-6 no cambia nada: el diff del CLAUDE.md local solo añade líneas (127 inserciones y 0 borrados).

**4. El verificador** (punto 2 del foco).
- **Controles exactos.** Mi propia emisión reproduce 1158 / 730 / 42, y 11 575 / 7298 / 425 en 1000 semillas: son deterministas. Cada sitio sale al menos 7 veces en E1, así que un solo sitio cambiado mueve su control. Distractor de G en E1:
  - CURSOS: ERR-DATO 12, más 9 canónicas; ERR-PARCIAL 7; ERR-RESTA 7; ERR-TOTAL 9.
  - PROMEDIO: ERR-PARCIAL 8; ERR-RESTA 35; ERR-TOTAL 13.
- **`textos_sol()`.**
  - Toma el `data_generation` del mismo texto que la Solution, y para el `.Rmd` sin mutar es el mismo EXPR de siempre.
  - Los mutantes 27 (100), 31 (100 / 0) y 32 (200 / 0) dan lo mismo que en la r5: ninguno de los anteriores cambia de veredicto.
- **Mutantes 34, 35 y 36.** Cada uno quita un solo sitio, porque `mut_lin()` exige que el patrón aparezca una vez, y solo se mueve su control:
  - 34: 1158 → 1151, las 7 versiones de CURSOS con ERR-PARCIAL;
  - 35: 730 → 721, las 9 canónicas;
  - 36: 42 → 0.
- **El mutante 37.** Su patrón aparece una vez y muere en la semilla 1 por la guarda de la línea 601. La clase de la S1b exigía `\text{kilos}` y no lo veía. El 33 muere con el mensaje nuevo.
- **Mutantes 38, 39a-c y 41a-c.** Los caza la sonda declarada, con las otras sondas en 0 y la clase anterior en 0, con el matiz de H2. Los separadores se construyen con `intToUtf8()`, y la guarda de literales del verificador pasa.
- **La guarda de H5 y el mutante 40.** `lado_cumple` no es trivial:
  - en el `.Rmd` sin mutar, 0 de 291 versiones quedan fuera de su lado;
  - con el mutante 40, 32 de 100 quedan fuera de su lado, sin ningún error.
- **`lado = NA` en la canónica no rompe nada.** Solo lo leen `lado_cumple`, que la descarta antes por ser canónica, y los recuentos de VL.

**5. `cap_s1b2.py`** (punto 3 del foco).
- El JS con los escapes deshechos es idéntico al anterior: 4810 caracteres, con 10 U+00A0, 3 «»», 3 «”» y 3 «…», que ahora van como escapes.
- Como el JS es una cadena cruda, JavaScript recibe el escape.
- El assert comprueba los cuatro espacios especiales en el JS y en el propio archivo.

**6. La tabla candidata** (punto 4 del foco). Cada guarda, sonda, control y mutante citado existe y hace lo que dice su fila (líneas 594 y 601; CTRL_DENTRO, CTRL_SIGNO y CTRL_SIN_U; mutantes 30 a 41), con las precisiones de H2 y H3.

**7. La 6b** (punto 6 del foco). El cierre no depende de la geometría:
- el punto está dentro del `<math>`, que Chromium maqueta como una caja atómica, y MathJax no parte entre la cifra y un `\text{}` que la sigue;
- la evidencia para la fórmula al final del renglón es la de V1′ (de 34/5 a 0 a 360/320 px), que es el mismo mecanismo;
- los 42 casos a mitad de renglón de la S2a no pueden probarlo, como declara la propia 6b.

## No auditado

- No volví a correr el verificador entero; lo repitió el coordinador, con 297 líneas idénticas. Leí su código y su salida, y reproduje con mi código los controles, las sondas y las celdas de O-1 que necesitaba.
- No rendericé ni capturé a 360 y 320 px, ni en la caja del Moodle del colegio. Tampoco repetí la medición del PDF por coordenadas: solo la estructura del TeX.
- H1 no lo medí sobre `e293d5c8` ni en el CLOZE: allí lo doy por la invariancia registrada.
- Fuera de H1, no medí la diagnosticidad, porque §P7-D está agotado.

## Veredicto global

**APROBAR_CON_CAMBIOS.** No hay ninguna CORRECCIÓN. Por la regla #9, H1 (ALTA) bloquea la FASE 3 hasta que el profesor la firme o la decida. H2 a H4 son endurecimientos y registro.

## Próximos pasos

1. **H1:**
   - cablear «resta» y sus combinaciones en E4, con su mutante (exit 3 hasta la firma);
   - llevarlas al profesor regla por regla en el WAIT_USER #3;
   - corregir `ESTIMULO_SC`.
2. **H3:** antes de la firma, corregir el texto de las dos filas de la candidata. Si se quiere, ampliar la guarda de la línea 595.
3. **H2:** añadir M42 y M43, aditivos, y hacer simétrico `juzgar_s1()`.
4. **H4:** corregir la línea de uso de `cap_s1b2.py`.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:257 | codigo = "ERR-RESTA", lado = "G"
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:263 | stop("calcula(ERR-RESTA): fuera de contexto"); p$delta
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/verificar_espacio.R:307 | outer(v[-i], v[-i], "-") == v[i]
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/verificar_espacio.R:497 | los canales del enunciado y de la tabla se miden aparte, en las sondas de E4
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/.claude/CLAUDE.md:228 | Si una medición posterior supera cualquiera de estas cifras, o
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/.claude/CLAUDE.md:206 | S_U + las tres pistas
H1 | /home/bootcamp/.cache/fase2c_r6/canal_resta.txt:8 | S_U + S1 + suma + atípica + resta
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/verificar_espacio.R:911 | else if (any(s[names(s) != sonda] > 0)) "cazado_por_otra"
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/verificar_espacio.R:913 | any(cmal[names(cmal) != sonda])) "cazado_por_otra"
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/salida/ciclo_20261102/s2a/verificar_espacio_s2a.txt:243 | controles: unidad dentro 958 (falla), unidad con el signo dentro 530 (falla)
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/.claude/CLAUDE.md:1589 | M41a, M41b y M41c (la sonda: la lista de totales lo saca
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:595 | paste0("\\$[ ", ED, "]+(kilos?
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:602 | grepl("\\$[^A-Za-z0-9$\\n]{0,3}[.,:;!?)
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/.claude/CLAUDE.md:1588 | cualquier separador que no sea letra ni cifra; control exacto: 1158 unidades dentro de la fórmula en E1
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/salida/ciclo_20261102/s1b/relanzada/cap_s1b2.py:16 | Uso: python3 -I cap_s1b2.py
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/salida/ciclo_20261102/s1b/relanzada/cap_s1b2.py:19 | import websockets
H4 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/salida/ciclo_20261102/s2a/6b/informe_6b_63e2713e_completo.md:9 | que está en el site de usuario
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
