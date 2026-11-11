# HANDOFF — vueltas-llanta-perimetro-n4 (MAT-2026-07-028)

SCHOICE nuevo del ítem 12 de Evaluar para Avanzar 2023, Matemáticas 9.°, cuadernillo 1 (I_1461453), que el simulacro SAI
de grado 10 imprimió como pregunta 28 con una errata en la opción C. Fuente verbatim, errata, clave, Guía y clasificación:
[`ITEM_OFICIAL.md`](ITEM_OFICIAL.md) (no se edita). Sin `.claude/` local.

## Estado (2026-11-10): 11 de 11, aprobado para el aula y firmado

- **11, aprobación:** el profesor respondió **«Aprobar y firmar»** en la pausa 3 (ver «Decisiones del profesor»). El
  coordinador selló el paso con los md5 comprobados en disco. La firma y lo que leyó están al final de
  `salida/tabla_firma_p7.md`.
- `.Rmd` md5 `85773027d040e2ce4b45eda03afa362c`. Versiones anteriores, fuera del repo para que el test de nomenclatura no
  las recorra: r3 `6a51d76a…` (`/home/bootcamp/.cache/retomar-sesion/vueltas_rmd_r3_6a51d76.Rmd`) y r2 `4389766…`
  (`/home/bootcamp/.cache/retomar-sesion/vueltas_rmd_r2_4389766.Rmd`).
- **Reapertura #3** con la CLI (`reopen --desde generacion_rmd`), con el motivo y el reporte de la r3 en `reaperturas`.
- Pasos 3 a 6 re-ejecutados y sellados sobre el md5 de arriba, cada uno con su nota.
- **7, FASE 2C r4:** `salida/fase2c_r4_detractor.md`, **APROBAR_CON_CAMBIOS**, de un detractor nuevo lanzado sin `name:`.
  Citas verificadas por el orquestador: 31, 0 problemas, exit 0. Al cierre, el coordinador vio 10 de 31 citas sin
  coincidencia: todas apuntan a `salida/tabla_firma_p7.md` y a este HANDOFF, que se editaron después de la entrega (19:55
  y 19:56) para atender H1 y H3. El hook las había verificado al entregar (`~/.claude/hooks/logs/citas-detractor.log`,
  19:52:16: 31 citas, 0 fallos, ENTREGA VERIFICADA).
  - Sin hallazgos de CORRECCIÓN (oráculo propio: 1248 combinaciones forzadas por el chunk real, 48 canónicas y 1500
    semillas, 0 problemas). Sin CRÍTICAS ni ALTAS.
  - H1 (MEDIA: tres explicaciones de la tabla de firma) y H3 (BAJA: precisión de la tabla) aplicados a
    `salida/tabla_firma_p7.md`, sin cambiar el md5.
  - Pendientes, BAJAS y opcionales según el detractor. Se atienden en el próximo cambio del `.Rmd`, porque cambian el md5:
    - **H2:** los comentarios de las líneas 7 y 125-127 del `.Rmd` describen algo que el código no hace. En `numero_izq`,
      VL-F empieza por una fracción, así que la forma sí distingue la clave. La línea 7 da la clave como K/V × d, que solo
      vale en la canónica y en `fraccion_izq`.
    - **H4:** el crédito 1/4 de la guarda no se ejecuta con cinco atajos. Hay que declararlo `sin_prueba_de_deteccion` en
      el verificador o retirarlo.
  - Sellado.
- **8, 9 y 10:** ejecutados sobre el md5 de arriba (evidencia abajo). Sellados en orden 7, 8, 9 y 10 después del veredicto, cada uno con su nota.

## Decisiones del profesor (literales)

- **2026-11-10, pausa 1** (transmitida por el coordinador): **«Sin figura» · «Mezclar con errores nuevos»**.
  - Flujo B no requerido, sellado con `--requerido false`.
  - Banco de 6 errores. De la Guía: `VL-A` 100/d, `VL-B` d × P/100, `VL-D` 100/P. Nuevos: `VL-E` d/P, `VL-F` (100/P) ÷ d,
    `VL-G` (100/P) × 100 × d.
  - Canónica con probabilidad 0,10 y los tres errores de la Guía.
- **2026-11-10, tras la FASE 2C r1:** **«Aceptarla»** (la regla del estímulo «contiene la relación K/V y otra operación»:
  se mide y se firma con su cifra en la pausa 3; no se persigue).
- **2026-11-10, tras la FASE 2C r2:** **«Corregirla en la última pasada»** (hecha en la r3 y revertida en la escritura
  por la decisión siguiente).
- **2026-11-10, tras la FASE 2C r3:** **«Volver a la de la ronda 2»** (transmitida por el coordinador).
  - Pregunta que leyó: «Pregunta de la llanta: ya no quedan pasadas de corrección y hay que elegir cómo se escribe la
    respuesta correcta en las versiones nuevas. Cualquiera de las dos opciones cambia el archivo y necesita otro revisor
    antes de que usted lo apruebe para el aula. ¿Con cuál escritura se queda?».
  - Antes de la pregunta se le explicó: que la ronda 3 bajó la pista de «la fórmula más ancha» de 67 a 20 de cada 100 y
    separó las opciones en el celular; que en 9 de cada 10 versiones la respuesta correcta quedó escrita como
    «150 ÷ P/100», distinta del cuadernillo, de modo que quien modela bien el problema tiene que transformar su fórmula
    (dividir entre una fracción); y que esa escritura deja siete pistas de 43 a 50 de cada 100 en el conjunto, una de 82
    en una cuarta parte de las versiones.
  - Opciones que vio: «Volver a la de la ronda 2» (la respuesta correcta se ve como la escribe quien modela el problema,
    100/P × 150 o 100 × 150/P; vuelve la pista de «la fórmula más ancha», 67 de cada 100 versiones nuevas, que firma en
    la pausa 3 y puede observar en clase; se conservan el arreglo del celular y los demás arreglos) y «Quedarse con la
    escritura nueva», que descartó.
  - **Corrección de la cifra:** la tasa exacta de «la más ancha» en las versiones nuevas es **60,0 de cada 100**, no 67.
    El 67,5 % salía de una muestra de 100 versiones. Se le dice en la pausa 3.
  - **Segunda corrección (coordinador, al cierre):** de las «siete pistas de 43 a 50 de cada 100» que se le atribuyeron a la
    escritura de la r3, seis estaban en ese rango; la séptima, «contiene ×», daba 16,5 en el conjunto y 59,3 dentro de
    `fraccion_nv` (`salida/fase2c_r3_detractor.md`, H2). No cambia la comparación: esa escritura sacaba además la clave de
    la forma en que la escribe quien modela el problema. Se le dijo al profesor al cerrar.
- **2026-11-10, pausa 3 (WAIT_USER #3), hacia las 20:10:** **«Aprobar y firmar»** (transmitida por el coordinador).
  Aprueba llevarlo al aula y firma las 36 reglas de `salida/tabla_firma_p7.md` tal como están, con la cifra corregida de
  «la más ancha» (60 de cada 100 versiones nuevas) a la vista. Pregunta, opciones y lo que se le mostró: al final de esa
  tabla. Condición: si en el aula los estudiantes eligen «la más ancha» u otro atajo de superficie, se anota como evidencia.
- **Nivel 4:** es el de la ficha. Va con DOK 2, que la regla #1 permite.

## Ronda 4: qué se hizo (§P7-D agotado: no es una cuarta pasada)

- **Restauración.** El `.Rmd` se reconstruyó desde la copia exacta de la r2 con `construir.py` (copia permanente en
  `salida/r4_construir.py`). De la r3
  se conservaron solo los arreglos que no son de escritura:
  - el feedback de VL-F reescrito (falso positivo de «cuanto» en el corrector);
  - el `span` de separación vertical entre opciones (HTML y Moodle);
  - el modelo de ancho del render de TeX (`ancho_tex`) y el crédito 1/4 cuando un atajo no señala ninguna opción;
  - el comentario sin «MAS».
  Salieron las formas `cociente`, `cociente_b` y `fraccion_nv`, con sus textos (`TXT_CLAVE`, `PROC_FORMA`, `B_CORTA`,
  `G_CORTA`, `G_LARGA`) y sus marcas de guarda (`contiene_div`, `letra_arriba`). Vuelven las formas `fraccion_izq`,
  `numero_izq` y `fraccion_unica` con la tabla de pesos de la r2 (`salida/pesos_lp.txt`).
- **Guarda «diagnosticidad: tabla de pesos».** Cinco atajos (centralidad, medoide, tres datos, la que más comparte y la
  más ancha en TeX), con topes por (nivel, atajo): la cifra del diseño más 0,5 pp en cada forma y más 1 pp en el agregado.
  El verificador comprueba celda por celda que la instancia de referencia coincide con la enumeración exacta (diferencia
  5,6e-16): atiende la H4.1 de la r3. El `stopifnot` de `all(PESOS > 0)` tiene nombre y mutante (`G02b`): atiende la H4.2.
- **Verificador r4** (`salida/verificar_vueltas.R`; el de la r3 queda en `salida/verificar_vueltas_r3.R`). Mide por
  **enumeración exacta** las 1248 combinaciones de las versiones nuevas y la canónica en sus 24 órdenes (H1 de la r3).
  Familias: B (batería congelada con la anchura y la estrechez en el render), V1 (detractor r1), V2 («contiene ÷» y marcas
  únicas) y V3 (reglas de conjunto del detractor r3, H2, en `salida/detractor_r3/reglas_r3.R`). El techo nulo es el de
  N = 100, el menor de dos tandas de semillas. Controles: pertenencia de 800 versiones reales a la enumeración, frecuencias
  de las celdas (chi-cuadrado p = 0,755) y estabilidad entre las dos tandas (|z| máximo 1,62 en 616 tasas).
- **Alcance:** `salida/alcance_r4.R`, 36 reglas con su tasa exacta. **Tabla de firma:** `salida/tabla_firma_p7.md`.
- **Glosa de la cabecera** del `.Rmd`: dice las cifras exactas (H2 de la r3).

## Evidencia (`salida/`, sobre el md5 de arriba)

- **Render** (`render_r4/`): HTML 4, PDF 5 (con `tex/`), DOCX 2, NOPS 2 y Moodle 10 (`seed` 1:10).
  - `\pandocbounded` 0 veces fuera del preámbulo; `LTcaptype` solo en el preámbulo de la plantilla (no hay tablas).
  - El `span` no aparece en PDF, NOPS ni DOCX; en el XML de Moodle hay 40, y 4 por HTML.
  - «101/120» en 0 salidas. PDF v1 revisado a ojo, en sus 2 páginas.
- **Arsenal** (`hook_post_render_r4.json`): 0 errores y las 6 advertencias de siempre. FASE 2G 100/100 y 2H sin
  anomalías. El hook se corrió a mano con la ruta literal del `.Rmd`: la sesión arrancó en la subcarpeta y los hooks del
  repo no se cargan, y su regex exige la ruta literal dentro de `exams2*()`.
- **6b** (`paso6b_auditoria_visual_r4.md`): **APTO_VISUAL**, N = 24, a 320, 360 y 1024 px. Hueco mínimo entre opciones
  de 9,6 px a 320 y a 360 px.
- **8** (`paso8_coherencias_r4.md`): **OK_CON_OBSERVACIONES**, sin fallas. Moodle cuadra en 10 de 10 preguntas.
- **9:**
  - `paso9_verificador.txt`: **ROJO_CUBIERTO**, exit 3, 0 fallos, 36 rojos cubiertos, 0 sin alcance.
    - Oráculo de texto 802/802 y oráculo algebraico 1249/1249.
    - Modelo de ancho contra pdflatex en 1182 cadenas (error 0,0045 pt), con su mutante A1.
    - 20 mutantes de guarda cazados por su sonda (G01-G20, con el G02b nuevo y sin G17) y una guarda en `sin_prueba_de_deteccion` (la clave canónica en la C
      oficial, implicada por las opciones de la Guía y el oráculo).
    - 5 mutantes de texto por atajo (`T-*`), cazados por su sonda en los cuatro niveles.
    - 20 pares (nivel, atajo) por mutación del entorno, todos `cazado_por_su_sonda`.
    - Mutantes del alcance V1 a V3 cazados.
  - `paso9_diversidad.txt`: PASS (63 valores de la clave en 100 versiones).
  - `paso9_diagnosticidad.txt`: PASS. H1 0 % (margen 8 %), H2 3 %, H3 58 % de 19 versiones y H3b 67 % («dfrac times p»).
    H3b exacta: 63,0 % de todas las versiones (70,0 % de las nuevas); avisa desde el 90 %.
  - `paso9_stresstest.txt`: PASS, 100/100.
  - `paso9_multisemilla.txt`: 100/100.
  - `paso9_unicas.txt`: 268 versiones únicas de 300 y 138 claves distintas.
- **10** (`paso10_literalidad.txt`): `TOTAL fallos: 0`, con el catálogo canónico montado. Nivel 4 con DOK 2.

## Vector completo, tasas exactas (`paso9_verificador.txt`; % de acierto con el atajo; azar 25 %)

| Atajo | Nuevas | `fraccion_izq` | `numero_izq` | `fraccion_unica` | Canónica |
|---|---|---|---|---|---|
| Centralidad | 32,5 | 50,0 | 50,0 | 25,0 | 0,0 |
| Medoide | 32,5 | 50,0 | 50,0 | 25,0 | 0,0 |
| La que más comparte | 31,0 | 29,2 | 29,2 | 31,8 | 50,0 |
| Usa los tres datos | 33,5 | 37,5 | 37,5 | 31,8 | 50,0 |
| **La más ancha (TeX)** | **60,0** | 25,0 | 25,0 | **75,0** | 50,0 |
| Divide entre una fracción | 25,0 | 25,0 | 25,0 | 25,0 | 25,0 |
| Empieza por la distancia | 32,5 | 25,0 | **75,0** | 25,0 | 0,0 |
| Letra arriba de una fracción | 21,3 | 12,5 | 12,5 | 25,0 | 0,0 |
| Contiene × | **42,3** | **66,7** | **66,7** | 31,8 | 50,0 |
| Un solo operador | 36,3 | 41,7 | 41,7 | 33,9 | 50,0 |
| No contiene K/V | 19,8 | 0,0 | 0,0 | 28,3 | 0,0 |
| Gemelas | 25,0 | 29,2 | 25,0 | 24,1 | 0,0 |
| Contiene ÷ | 17,5 | 0,0 | 0,0 | 25,0 | 25,0 |
| Estímulo K/V y otra operación | 30,0 | 41,7 | 41,7 | 25,0 | 100,0 |

Fuera del vector pedido, también en rojo: «letra abajo (conjunto)» 56,5 % en las nuevas y 67,3 % en `fraccion_unica`.

## Archivos versionados (commit del 2026-11-10)

`.gitignore` ignora `**/salida/`, así que la evidencia entra con `git add -f`, por nombre. Con estos archivos el
verificador corre desde git (además usa `.claude/scripts/bateria_eliminacion.R` y `bateria_referencia_icfes.R`):

- **Ejercicio:** el `.Rmd`, `ITEM_OFICIAL.md`, este `HANDOFF.md`, `ejercicio_state.json` y `Originales/` (la plana
  `pagina_009.jpg` y el recorte `orig_pregunta_28.png`, por LFS).
- **Verificador y firma:** `salida/verificar_vueltas.R`, `salida/alcance_r4.R`, `salida/tabla_firma_p7.md`,
  `salida/r4_exacto.csv`, `salida/r4_proponer.txt`, `salida/r4_construir.py` y `salida/detractor_r3/` (`reglas_r3.R`, que
  el verificador carga; `exacto_r3.R`, `oraculo_r3.R` y sus salidas).
- **FASE 2C:** `salida/fase2c_r1_detractor.md` a `salida/fase2c_r4_detractor.md`.
- **Pasos 6 a 10 de la r4:** `salida/hook_post_render_r4.json`, `salida/paso6b_auditoria_visual_r4.md`,
  `salida/paso8_coherencias_r4.md`, `salida/paso9_verificador.txt`, `salida/paso9_diversidad.txt`,
  `salida/paso9_diagnosticidad.txt`, `salida/paso9_stresstest.txt`, `salida/paso9_unicas.R`, `salida/paso9_unicas.txt`,
  `salida/paso9_md5.txt` y `salida/paso10_literalidad.txt`.
- **Tabla de pesos (de la r2, la que usa el `.Rmd`):** `salida/pesos_celdas.R`, `salida/pesos_celdas.csv`,
  `salida/pesos_lp.py` y `salida/pesos_lp.txt`.

Quedan fuera, en disco: los renders (`render_r2/` a `render_r4/`, `r2/`, `r3/`, `html/`, `pdf/`…), las capturas de la 6b
(unos 90 MB), la medición de la r3 (`p3_*`), los verificadores de la r2 y la r3 y `paso9_multisemilla.txt` (696 KB; su
resultado, 100/100, está arriba).

## Referencias cruzadas

- Errata de la C: regla #24 H-2, aclaración del 2026-11-10 (`.claude/docs/reglas/hermes-imagenes-icfes.md`).
- Convergencia, ancho y constructo de este ciclo: regla #22 §P7-F, cuarto caso (`.claude/docs/reglas/diversidad-sustantiva.md`).
- Citas de la r4 que fallan al cierre: regla #9 v1.8, «Sin commit» (`.claude/docs/reglas/detractor-obligatorio.md`).
- Índice: `.claude/docs/INDICE_LECCIONES.md` §8 y el changelog del índice, v3.41.1.

## 2026-11-11: Knit de RStudio y dos avisos de consola (sin cambio en el ejercicio)

El profesor reportó dos mensajes al renderizar. Ninguno salía del `.Rmd`, y el md5 firmado no cambió.

- **Knit alteró el `.Rmd` y se restauró.** El archivo en disco tenía una cabecera YAML de cinco líneas
  (`output: pdf_document / html_document`) y un espacio menos en el comentario de la línea `ED <- " "`: md5
  `372168be…` (Error 43, regla #26). Se restauró con `git checkout` desde `efe34781`, con el visto bueno del profesor. El
  md5 vuelve a ser `85773027d040e2ce4b45eda03afa362c`, `git status` no muestra cambios en el archivo y los sellos siguen
  valiendo. No se guardó copia de la versión alterada.
- **«The following object is masked _by_ ‘.GlobalEnv’: fmt».** Sale en el `library(exams)` de la línea 2 de
  `SemilleroUnico_v2.R` cuando RStudio restaura el `.RData` de la raíz del repo, que traía una función `fmt` de otra
  sesión; `exams` 2.4.5 exporta una con el mismo nombre. El `.Rmd` no usa `fmt`. Reproducido con `R --no-save` desde la
  raíz; con `Rscript --no-restore` no aparece. El `.RData` se borró a petición del profesor (ya estaba en `.gitignore`).
  RStudio lo recrea al cerrar mientras «Save workspace to .RData on exit» no esté en «Never».
- **«[WARNING] Deprecated: --mathjax».** Es de pandoc 3.11 por una opción que pasa `exams` 2.4.5. Medido etapa por etapa
  en una copia fuera del repo: lo emiten `exams2moodle()` y el cuestionario interactivo (`webquiz_autocontenido.R`, que
  termina en `WEBQUIZ_OK`); `exams2pandoc()` a DOCX no. `exams2html()` emite el gemelo `--mathml`. Todas terminan con
  exit 0 y no cambia la salida: no se corrige en el `.Rmd`, en el semillero ni en `perifericos/`. `exams2pdf()` con
  `solpcielo` y `exams2nops()` no se midieron por separado.
- **Render de comprobación** en sesión limpia, sobre el md5 firmado: `exams2html()` y `exams2pdf()` con `n = 1`, exit 0,
  sin el mensaje de `fmt` y con las fórmulas del HTML en MathML.
- **En disco, sin versionar:** el profesor ejecutó `SemilleroUnico_v2.R` a las 04:59 y dejó en `salida/` el PDF, el DOCX,
  el XML de Moodle, el NOPS y el cuestionario interactivo. Los semilleros y las plantillas `.tex` de la carpeta siguen sin
  seguimiento de git.

## Próximo paso

Aprobado para el aula (11 de 11). Lo que falta:

- **Git:** el ejercicio y su evidencia están versionados y en el remoto (`efe34781`, rama
  `feat/plantillas-oficiales-rexams`; lista en «Archivos versionados»). Siguen sin seguimiento los semilleros y las
  plantillas `.tex` de la carpeta. `.gitignore` ignora `**/salida/`: evidencia nueva entra con `git add -f`, por nombre.
  Antes de un commit que toque el `.Rmd` o el verificador, la suite completa (`R_TESTS_FULL=1`).
- **Render:** con `SemilleroUnico_v2.R`, nunca con el botón Knit (cambia el md5 firmado; ver la sección del 2026-11-11).
- **Aula:** llevarlo a clase y anotar si los estudiantes usan «la más ancha» u otro atajo de la tabla.
- **Antes de promover:** evidencia de aula (Nivel 3) y FASE 2C-bis (`/detractor-hetero`).
- **Si se toca el `.Rmd`:** atender H2 y H4 de la r4. La firma queda sin efecto, y hacen falta otro verificador, otra tabla
  y otro detractor.
