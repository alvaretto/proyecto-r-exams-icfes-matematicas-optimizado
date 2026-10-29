<!-- Reporte íntegro de la FASE 2C, ronda 3 (AgenteDetractor nuevo, sin name:, 2026-10-29), extraído de su transcripción por el orquestador. Versión auditada: md5 cd94b59adfdea7b9c92dc074ce0a1435. -->

# Auditoría del detractor, FASE 2C ronda 3: `totales-promedios-reciclaje-n3` (MAT-2026-07-050)

**Veredicto: APROBAR_CON_CAMBIOS.** La corrección está limpia. Hay una objeción ALTA de diagnosticidad que va al profesor junto con el WAIT_USER #3, sin pasada nueva (§P7-D agotado). Hay además una MEDIA sobre cómo se declaran las cifras y una BAJA de redacción.

- **Fecha:** 2026-10-29.
- **Versión auditada:** md5 `cd94b59adfdea7b9c92dc074ce0a1435`. Lo comprobé al empezar, dentro de cada script (`stopifnot`) y al terminar: no cambió.
- **Independencia:** no escribí ni corregí este ejercicio y no soy el detractor de la ronda 1 ni el de la ronda 2. Corro como subagente bloqueante, sin `name:`.
- **Objeciones:** 3 (0 críticas, 1 alta, 1 media, 1 baja). Ninguna es de CORRECCIÓN.

## Resumen

- **Corrección: limpia.**
  - Mi oráculo lee el texto y la tabla que se muestran. Lo pasé por 1 677 264 textos de opción (todo el espacio, con 12 variantes de nombre y fila) y por 7700 versiones reales: hay una sola afirmación verdadera, y es la marcada.
  - La Solution y el feedback no afirman nada falso en ninguna versión, incluidas 795 canónicas.
  - La canónica es idéntica al impreso, cadena por cadena.
- **Las cuatro decisiones del profesor están implementadas y comprobadas:**
  - el cierre nuevo;
  - ERR-TOTAL de vuelta en el pool;
  - la guarda de lecturas y la estructural;
  - la H4 de la ronda 2.
- **Testing.** M16, M17 y M18 llegan a la parte del código que mutan y mueren por su propia sonda; los volví a correr con un arnés propio. No se relajó ninguna guarda del verificador. Sale con exit 3, no con exit 0.
- **Lo nuevo (H1).** ERR-TOTAL abre la pista «descartar la cifra atípica»:
  - En **todas** las versiones con ERR-TOTAL, *T*_G es al menos el doble de cada una de las otras tres cifras.
  - Junto con S_U decide el par de G en el 100 % de esas versiones.
  - Con las tres pistas superficiales, el atajo de O-1 acierta el **77,3 %** en la mezcla sin calcular ningún total. El verificador no tiene esa sonda.

## A. Corrección: lo que verifiqué y salió limpio

| Comprobación | Método | Resultado |
|---|---|---|
| Una sola verdadera, y es la clave (todo el espacio) | Oráculo con 14 plantillas escritas a mano desde la lectura del texto, no desde `MOLDES`. CURSOS: 2134 combinaciones × 12 (conjunto, fila de G) × moldes × todas las cifras = 1 135 200 textos. PROMEDIO: 542 064 textos | 0 textos sin plantilla; 0 claves falsas; 0 textos de G con cifra falsa que salgan verdaderos; 0 textos de P verdaderos |
| Lo mismo en el chunk real | 7700 semillas (1-1000, 90001-90100, 91001-91600, 100001-106000): 795 canónicas, 3076 CURSOS y 3829 PROMEDIO. Opciones leídas de `opciones` y tabla leída de `tabla_md` | 0 fallos |
| Solution y feedback | Unas 25 comprobaciones por versión: la aritmética de cada `$a \times b = c$` y `$a - b = c$`; quién ganó y su perfil; «solo una cita la diferencia correcta»; cada `explica()` contra la tabla; que el código ERR coincida con el contenido de la afirmación citada; «tampoco es la diferencia»; filas del procedimiento; caso específico; Verdadero/Falso del feedback contra `sol`; cifras del feedback; material del enunciado | **0 afirmaciones falsas en 7700 versiones** (795 canónicas); 0 letras en la Solution |
| Canónica contra `ITEM_OFICIAL.md` | Cadenas literales: enunciado (l. 38-40), pregunta (l. 44), encabezado (l. 48), celdas (l. 50-51), opciones (l. 71-74), clave = contenido de B | 795/795 idénticas; también en `salida/r3/realista/tex_7/exercise1.tex` |
| Moodle (`salida/r3/moodle`) | Parseo del XML | 3 preguntas, cada una con una sola respuesta de fracción 100 y feedback «Verdadero»; `shuffleanswers` false (mezcla R/exams); sin letras |
| Punto 1 (cierre) | Las 4 plantillas casan con el 100 % de las opciones no canónicas | Implementado |
| Punto 3 (H4 de r2) | Líneas 310 y 314: «Sobre lo recogido, la tabla da solo…» | Implementado |

**Qué lecturas admite el cierre nuevo, en particular con *x* = *T*_G.**

- En «su total fue más alto que el de *O* en *x* kilos», la cifra solo aparece como complemento diferencial del comparativo («en *x* kilos»).
- Es la misma construcción que la regla #13 v1.1 pone como modelo de frase con una sola lectura («superó en N kilos a»; texto íntegro, l. 236-237).
- A diferencia del cierre de la ronda 2, ninguna subcláusula presenta *x* como el total: no hay «fue de *x* kilos» ni «recogió *x* kilos».
- Leer «en 318 kilos» como «su total fue 318» obliga a reinterpretar la preposición. Ese desliz no es una segunda lectura de la frase: es el error de ERR-TOTAL (confundir el total con la diferencia).
- **Mi juicio:** no es una lectura razonable en el sentido de la regla, así que no hay defecto de CORRECCIÓN.

**Exposición, para que el profesor la conozca.** El verificador la da en configuraciones; esta es la cifra por versión.

- Si un estudiante toma la cifra como el total, en 533 de 3076 versiones CURSOS (17,3 %) y en 802 de 3829 PROMEDIO (20,9 %) encuentra una sola «verdadera» que no es la clave: siempre ERR-TOTAL.
- En el resto de las versiones, con esa lectura no queda ninguna verdadera.
- En la canónica, esa lectura deja la clave falsa y ninguna verdadera. Es la rareza verbatim aceptada en L-1.

## B. Testing

- **M16, M17 y M18, corridos con mi arnés** (sus definiciones de mutación, mi bucle de semillas): los tres llegan al entorno y mueren por su propia sonda, en las semillas 1, 3 y 5.
- **Defensa en profundidad:** M17 sin la guarda de lecturas lo caza también la guarda estructural, en la semilla 3. En las versiones no canónicas la protección real es la guarda estructural; la de lecturas solo actúa en la canónica, con la excepción limitada a la clave.
- **Ninguna guarda retirada:** comparé las líneas OK/FALLA de `verificar_espacio_ronda2_tras_fase2c.txt` y `_ronda3.txt` y solo hay altas.
- **La guarda H2 de la ronda 2 no se quitó en silencio:** se sustituyó en el `.Rmd` por decisión del profesor (HANDOFF, § Próximo paso 6).
- **Exit:** 3, `PENDIENTE DE FIRMA (5)`. No sale con exit 0 sobre residuos sin firmar. Hay dos veredictos en el borde que dependen del bloque de semillas: ver H2.

## C. Diagnosticidad: cifras para el WAIT_USER #3

- **Muestras.** La oficial es N = 100 por rama (semillas 91001 en adelante, no canónicas). Añadí una **muestra de depuración declarada** (semillas 100001-106000): CURSOS n = 2422 y PROMEDIO n = 2972, con un error típico de unos 0,8-0,9 pp en una tasa del 30 %.
- **Canónica:** sus 24 órdenes, enumerados.
- **Mezcla:** ponderada 0,4 / 0,5 / 0,1, como en L-3 y L-5.
- **Rasgos:** leídos del texto y de la tabla mostrados.

| Sonda (tasa de acierto, %) | CURSOS N=100 | PROMEDIO N=100 | Canónica | CURSOS dep. | PROMEDIO dep. | Mezcla dep. |
|---|---|---|---|---|---|---|
| S_U (O-1) | 50,0 | 50,0 | 50 | 50,0 | 50,0 | 50,0 |
| S1 (descarta la cifra de la tabla) | 27,1 | 33,0 | 50 | 27,7 | **32,0** | 32,1 |
| R_dif | 29,3 | 29,2 | 25 | 30,0 | 30,4 | 29,7 |
| S_U + S1 | 66,0 | 54,5 | 100 | 67,9 | 51,4 | 62,8 |
| S_U + suma | 58,5 | 60,5 | 50 | 59,8 | 60,3 | 59,1 |
| S_U + S1 + suma (las dos pistas) | 66,5 | 65,0 | 100 | 72,3 | 61,7 | 69,8 |
| **S_U + atípica (nueva)** | 57,5 | 60,0 | 100 | 56,2 | **60,1** | 62,6 |
| **S_U + S1 + suma + atípica** | 74,0 | 75,0 | 100 | **78,4** | **71,9** | **77,3** |
| Atípica sola (descartarla) | 26,2 | 27,2 | 33 | 26,0 | 27,3 | 27,4 |

**Qué supera +8 pp y qué cubre O-1:**

- **S_U:** 50,0 % exacto, +16,7 pp atribuibles (frente a 33,3 %). Está **dentro** de O-1.
- **Sondas solas, todas por debajo de +8 pp:**
  - S1: +2,7 pp en CURSOS y **+7,0 pp** en PROMEDIO con la muestra de depuración. El +8,4 que da el verificador es una tirada (H2).
  - R_dif: +5,0 / +5,4 pp.
  - La atípica sola: +1,0 / +2,3 pp.
- **Combinaciones, comparadas con el 50 % de la premisa (tachada):** ninguna está firmada.
  - S_U + suma: +9,8 / +10,3 pp.
  - Las dos pistas: +22,3 / +11,7 pp.
  - S_U + atípica: +6,2 / **+10,1 pp**.
  - Las tres pistas: +28,4 / +21,9 pp.
  - Todas quedan **fuera del alcance de O-1** y vuelven al profesor (O-1 § Alcance).
  - S_U + S1 en PROMEDIO (+1,4 pp) queda dentro.
- **Batería §P7, mezcla:** +1,4 pp con mis N = 100 (el verificador da +0,3). PASS.
- **Batería §P7, por rama:** «la más larga (chars)» acierta el 36,9 % en CURSOS y el 35,4 % en PROMEDIO con la muestra de depuración, unos +11 pp sobre el azar **en las dos ramas**. Su margen máximo es 2,7 % y 2,0 % (mediana 1,9 %). Según §P7-B es inexplotable y no es defecto.
- **Canónica:** 100 % en «la que MÁS comparte»; pesa 0,10 × 75 = **+7,5 pp** en la mezcla (lo reproduje).
- **Cuotas del distractor de G en la muestra de depuración:**

| Rama | ERR-DATO | ERR-RESTA | ERR-PARCIAL | ERR-TOTAL |
|---|---|---|---|---|
| CURSOS | 23,1 % | 41,5 % | 18,7 % | 16,8 % |
| PROMEDIO | 3,3 % | 52,1 % | 22,9 % | 21,7 % |

## D. Otros dominios

- **Código R/exams:**
  - Metadatos completos; `exshuffle: TRUE` con opciones de texto; `exsolution` dinámico.
  - Está la guarda `\newcounter{none}`; la tabla mide como máximo 81 de 85 caracteres.
  - Glifos y ortografía: OK y limpio.
  - Los 5 valores largos de `exextra` aparecen literales en `ITEM_OFICIAL.md`.
  - 271/300 versiones únicas; diversidad sustantiva PASS.
- **Pedagógico:**
  - La Solution tiene sus 6 partes y el reparto 2 + 2 se cumple.
  - Regla #11: 8 plantillas de 8 tipos y ninguna con «registró».
- **Visual:** revisé la página 1 de la semilla 3, una versión con ERR-TOTAL (`salida/r3/png/s3-1.png`). La tabla está centrada y cabe en el margen, las opciones se leen bien y los encabezados de la Solution van en negrita, sin numerar.
- **ICFES:** Nivel 3 con DOK 2 es coherente (la recíproca es falsa).
- **Semántico:** el código del error y la afirmación coinciden en las 7700 versiones.
- **Arsenal:** 0 errores; sus avisos («Tarea», `letra_correcta`, patrón «Es posible que…») son heurísticos y no aplican.

## Objeciones

### H1 — ALTA · DIAGNOSTICIDAD (va al profesor; no pide pasada nueva): con ERR-TOTAL de vuelta, la cifra falsa de G es atípica y delata al distractor

- **Qué se cuestiona.** `cand_g` incluye *T*_G (l. 409-411). *T*_G = *n*_G · *a*_G siempre es mucho mayor que *D*, que *n*_P · δ y que la cifra sin sustento.
- **Evidencia medida** (depuración n = 2422 / 2972):
  - En el 100 % de las versiones con ERR-TOTAL (16,8 % / 21,7 %), *T*_G es al menos el doble de cada una de las otras tres cifras. La clave es la cifra atípica solo en el 4,5 % / 1,4 %.
  - S_U + «descartar la atípica» acierta el 99,9-100 % en esas versiones y el 47-49 % en el resto. En total: 56,2 % / 60,1 %; con N = 100, 57,5 / 60,0.
  - Sumada a las dos pistas que el verificador sí mide, el atajo llega al 78,4 % / 71,9 % y al **77,3 % en la mezcla**. El paquete actual informa como máximo un 71 %.
  - **Ejemplo** (semilla 3): las cifras son 3, 12 (clave), 52 y 192 = 6 × 32. A simple vista, 192 es la atípica.
- **Fuentes:**
  - Regla #22 P5: un distractor atípico es un canal de fuga.
  - §P7-E: hace falta al menos una regla relacional; la de la cifra atípica lo es.
  - O-1 § Alcance: «si aparece otro canal por encima de +8 pp, vuelve al profesor».
- **Alternativas:**
  - (a) **Sin tocar el `.Rmd`:** añadir a `verificar_espacio.R` las sondas «S_U + atípica» y «S_U + S1 + suma + atípica» como PENDIENTE, y llevar estas cifras al WAIT_USER #3, sea con O-1 corregido o con un O-2.
  - (b) **Si el profesor quiere reducirla** (sería una 4.ª pasada, con su firma): bajar el peso de ERR-TOTAL en el sorteo de `cod_gdis`. El canal aporta más o menos la mitad de su cuota. Pero eso sube la cuota de ERR-PARCIAL (R_dif/suma) o de ERR-DATO (S1): es el ciclo de desplazamiento de canal.

### H2 — MEDIA · TESTING: dos veredictos del verificador en el borde dependen del bloque de semillas

- **S1 en PROMEDIO.** El verificador da 33,4 % (+8,4 pp) y lo deja pendiente. Mis N = 100 dan 33,0 % (+8,0 pp). La muestra de depuración da **32,0 % (+7,0 pp)**. La mezcla ponderada del verificador pasa por 0,02 pp: 33,0 %, +8,0.
- **Largo.** El verificador da PASS en CURSOS (+0,7 pp) y declara el residuo §P7-B «solo en PROMEDIO» (+10,4 pp).
  - Mis N = 100 dan lo contrario: CURSOS +10,2 pp BLOQUEA («la más larga» 43 %) y PROMEDIO +3,9 pp NO_CONCLUYENTE.
  - La muestra de depuración lo explica: el canal existe en las **dos** ramas, con unos +11 pp y un margen de 2,7 % / 2,0 %.
- **Lo mismo con la cuota de ERR-TOTAL en CURSOS:** el verificador mide 9 %; mis N = 100, 20 %; la muestra de depuración, 16,8 %.
- **Fuentes:** regla #23 (estratos; una muestra de depuración se declara junto a la de N = 100), §P7-B y la H3 de la ronda 2.
- **Alternativa:**
  - Declarar el residuo de largo para las dos ramas, con su margen.
  - Dar S1 en PROMEDIO como «+8,0 a +8,4 pp con N = 100; +7,0 pp con la muestra de depuración de 2972».
  - En el verificador, aplicar el test de margen de §P7-B a cada rama aunque su muestra de N = 100 no supere +8 pp.

### H3 — BAJA · PEDAGÓGICO (Solution)

- **Qué se cuestiona.** La causa raíz de ERR-TOTAL dice «calcula un total, pero no compara». Pero la afirmación que la ilustra sí compara («su total fue más alto que el de *O*»): lo que falta es restar.
- **Alternativa.** «Se detiene a mitad del procedimiento: calcula un total, pero no le resta el del otro grupo.»
- **Cuándo.** Solo si se abre otro ciclo: cualquier edición del `.Rmd` exige una FASE 2C nueva.

## No auditado

- DOCX y NOPS rasterizados.
- HTML a 360 px (FASE 2M).
- Del PDF solo vi la página 1 de la semilla 3.
- No volví a correr el verificador completo: solo sus mutantes M16-M18 y la batería §P7 congelada sobre mis semillas.

## Scripts y salidas

Están en `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/35e93ef1-04b2-4e1e-8b49-a10ac14fa449/scratchpad/detractor_r3/`:

- Scripts: `comun.R`, `gen.R`, `correccion.R`, `diag.R`, `extra.R` y `mut.R`.
- Salidas: `correccion.out` y `diag.out`.

CITAS_DETRACTOR
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:410 | fuente = c("aG", "aP", "delta", "parcialG", "TG"),
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/verificar_espacio.R:228 | "S_U + S1 (par de G sin la cifra de la tabla)"
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/verificar_espacio.R | AUSENTE: descarta la cifra atípica
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/.claude/CLAUDE.md:185 | si aparece otro canal por encima de
H1 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/.claude/rules/diversidad-sustantiva.md:13 | Distractor posicional outlier (giro 180°, longitud única)
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/salida/verificar_espacio_ronda3.txt:114 | S1 descarta cifras de la tabla, rama PROMEDIO: 33.4 % (+8.4 pp; cota del diseño +8,3 pp)
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/salida/verificar_espacio_ronda3.txt:119 | S1 descarta cifras de la tabla, mezcla ponderada: 33.0 % (+8.0 pp; obliga > +8)
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/salida/verificar_espacio_ronda3.txt:137 | máx 33.5 % (sede de la opción más larga (relacional)), techo nulo 32.8 %, exceso +0.7 pp -> PASS
H2 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/salida/verificar_espacio_ronda3.txt:143 | RESIDUO DECLARADO §P7-B (rama PROMEDIO): exceso +10.4 pp por reglas de largo
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:252 | causa_raiz = "Se detiene a mitad del procedimiento: calcula un total, pero no compara.",
H3 | /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd:138 | paste0("su total fue más alto que el de ", O, " en ", x, " kilos.")
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
