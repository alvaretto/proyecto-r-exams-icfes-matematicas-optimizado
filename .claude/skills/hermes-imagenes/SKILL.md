---
name: hermes-imagenes
description: |
  Triaje y gate de fidelidad para figuras de cuadernillos ICFES escaneados.
  Úsala ANTES de reproducir cualquier figura de un ítem original: decide qué
  es realmente la figura (la descripción textual SOBRE-clasifica), si el ítem
  es fidelidad-crítico (trampa deliberada), y con qué rama verificar que la
  reproducción no alteró rótulos, cifras, posiciones ni conteos.
  El sistema se AUTOENTRENA: registra cada caso, destila lecciones de sus
  aciertos/errores y endurece su propia política sin intervención de Claude.
version: 1.9.0
model_recommendation: sonnet
platforms: [linux]
metadata:
  hermes:
    tags: [icfes, imagenes, image-gen, pve, education, autonomous]
    category: productivity
    related_skills: [analizar-imagen-grafica, generar-codigo-tikz, comparar-similitud-visual]
---

> ⚠️ **COPIA CONGELADA — importada a este repositorio el 2026-08-15.**
> Fuente original: `Todo-Pajaro/…/Mejorando-Imagenes/motor-hermes/skill-hermes/SKILL.md` (v1.9.0).
> El **motor ejecutable NO se forkea aquí**: vive en `$MOTOR_HERMES` de Todo-Pajaro y es su fuente
> única (dos copias divergentes del mismo gate es el modo de fallo del invariante I-10).
>
> **Qué rige en el repositorio ICFES R/exams.** Aquí las figuras son **vectoriales y dinámicas**
> (TikZ/Python/R, regla #3), porque deben regenerarse por versión con parámetros aleatorios
> (regla #22) — es decir, en el vocabulario de este documento estamos **siempre en la rama
> `tipo:"datos"` → vector**, y el generativo NO produce nuestras figuras. Lo que sí es obligatorio
> son sus dos gates: el **triaje** (§Protocolo pre-generación, lecciones 1-5) y la **fidelidad por
> tipo** (lecciones 10-13). Las secciones de generación, routing de generadores y manifest quedan
> como referencia del motor de Todo-Pajaro.
>
> Contrato operativo resumido y cableado al workflow: `.claude/rules/hermes-imagenes-icfes.md`
> (regla #24).

# Mejora de imágenes ICFES (harness autónomo)

Redibuja figuras de cuadernillos ICFES escaneados como ilustración editorial
flat-vector, con un **gate PVE de fidelidad** que evita alterar rótulos, cifras,
posiciones o conteos. El motor de todo es el script `harness_mejora.py`; tú
(el cerebro) sólo lo LANZAS y reportas el resultado — el script ya pinea
`gpt-5.5` con visión para juzgar cada salida.

## Cuándo NO usar generativo

**ACTUALIZACIÓN 2026-07-04 (test decisivo):** con el generador correcto
(`nano-banana-pro`) + los datos EXACTOS en el prompt (verbatim de la transcripción)
+ el gate PVE, las **tablas y gráficas de datos SÍ se reproducen con fidelidad total**
(validado: 12/12 tablas exactas número-por-número en una pasada). El flujo autónomo
`auto_prep.py` genera datos vía **extracción por prompt** sobre la página completa y
fuerza el modelo (aunque `tipo:"datos"`). PERO con TRES guardas NO negociables:
(a) los datos van VERBATIM en el prompt; (b) verificación humana/visual contra el
ESCANEO antes de insertar; (c) **excluir preguntas-trampa** (lección 5) donde una
inconsistencia deliberada ES la respuesta. Sin esas guardas, o para geometría compleja
/ scatter con puntos exactos que no convergen en el gate → siguen a vector.

## Protocolo pre-generación (gate visual + anti-errores) — LECCIONES

Antes de generar CUALQUIER figura, aplica este protocolo (lecciones de incidentes reales 2026-07-04 y 2026-07-05):

1. **Gate visual obligatorio — la descripción textual SOBRE-clasifica.** Un marcador
   `[FIGURA: …]` puede describir algo que NO es una ilustración. **MIRA el recorte en el
   JPG antes de decidir.** Solo van a generativo las **ilustraciones cualitativas** (escenas,
   objetos, montajes físicos sin cifras críticas). Van a VECTOR (no generativo, `tipo:"datos"`):
   tablas, gráficas con cifras, **física rotulada** (vectores de fuerza, T1/T2/T3, dimensiones
   A/L, posiciones numeradas) y **química** (estructuras moleculares).
   · Incidente: Q143 "tres recipientes sobre estufas" parecía ilustración; al ver el JPG era
   una TABLA de solubilidad. Q053 "alambres" tenía dimensiones A=1/2 cm², 20/10 cm
   (answer-critical) → vector.

2. **Ancla = número IMPRESO en el JPG, NO el mapeo.** `mapeo_jpg_pregunta.json` tiene desfase
   acumulado. Para localizar la página de la pregunta N, confía en el número impreso en la
   esquina del JPG, no en el mapeo. · Incidente: la página que el mapeo daba como "Q143"
   imprimía "146".

3. **Marcadores espurios / mis-atribuidos.** Un `[FIGURA: …]` puede ser un FANTASMA filtrado
   del enunciado u opción de OTRA pregunta. Si al ver el JPG la página no tiene esa figura
   (p.ej. es una tabla), el marcador es espurio → NO generes; repórtalo para limpieza.
   · Incidente: el `[FIGURA: estufas]` de Q143 era un duplicado del de Q141.

4. **`contexto` obligatorio en scans de baja resolución.** El juez PVE alucina siluetas
   ambiguas (leyó una trompeta como violín en Q179) y rechaza generaciones fieles. El
   enunciado en `contexto` ancla al juez (prioridad sobre el scan) y evita el falso negativo.
   · El `contexto`/datos DEBEN salir VERBATIM de la transcripción, CON tildes — retiparlos sin
   tildes hace que el juez rechace las tildes correctas de la generada (falso negativo).

5. **⛔ La TRAMPA / error deliberado ES la pregunta — NUNCA "corregir".** Muchos ítems ICFES
   incluyen inconsistencias DELIBERADAS en el estímulo (una gráfica cuyas barras NO coinciden
   con su tabla, una tabla con total errado, una etiqueta mal escrita) y la pregunta pide al
   estudiante DETECTARLAS. El generativo tiende a "arreglar" la inconsistencia y **destruye el
   ítem**. Reproduce SIEMPRE la figura tal cual, INCLUIDOS sus errores/inconsistencias; jamás
   normalices ni hagas coincidir dos representaciones. · Incidente Q067: la gráfica del original
   discrepa de su tabla A PROPÓSITO (clave C = "la gráfica presenta MAL la cantidad"); la generada
   la "corrigió" para cuadrar con la tabla → habría hecho verdadera la opción B. **`auto_prep`
   ahora EXCLUYE a humano las preguntas cuyo enunciado interroga la representación** (keywords:
   "cuál es el error", "misma información", "no coincide", "presenta mal", "inconsistente",
   **"¿es correcta la gráfica/tabla?"** — patrón `CORRIGE_REP`, añadido tras q107 dona 2026-07-05:
   una dona mal diseñada a propósito vs su tabla; clave B); marca `TRAMPA_REQUIERE_HUMANO`.

6. **Verificar contra el ESCANEO, no solo la descripción (fidelidad de datos).** Cuando la
   descripción `[FIGURA:]` viene truncada o la figura es multi-representación (tabla+gráfica,
   `riesgo:"MULTI"`), el modelo puede inventar cifras plausibles o cortar la imagen. Verifica cada
   generada contra el JPG original antes de insertar. · Incidentes 2026-07-04: Q067 gráfica
   alterada; Q075 imagen cortada (tabla se salió del lienzo); Q081 pregunta multi-tabla (solo se
   generó Tabla 1). En cambio, 12 tablas de datos puras salieron 100% exactas en una sola pasada.

7. **Screening ampliado: "inconsistente" y validación de afirmación (2026-07-05).** El detector
   de trampas (lección 5) tenía dos huecos, descubiertos en el lote de gráficas de barras de
   Mat-2026-1: (a) NO capturaba la palabra **"inconsistente"** → Q058 ("¿por qué la gráfica es
   INCONSISTENTE con la afirmación del gerente?", donde la caída deliberada de octubre ES la
   respuesta) pasó como OK. Ya corregido: `inconsistent*` añadido a `TRAMPA`. (b) Las preguntas
   de **validación de afirmación** ("X afirma que… ¿es verdadera la afirmación?", "esta afirmación
   es [correcta/incorrecta]") son fidelidad-crítico igual que las trampas: la respuesta depende de
   leer la gráfica al pie de la letra y el generativo puede alterar un valor y romper la clave.
   `auto_prep` las marca ahora **`VALIDACION`** → se generan PERO exigen verificación humana contra
   el escaneo, **NUNCA auto-insertar**. · En Mat-2026-1: Q012/Q166 se verificaron fieles (número por
   número) e insertaron; Q009 se EXCLUYÓ (un valor diario dudoso + la descripción `[FIGURA:]` de la
   cruda estaba errada). **Regla de oro**: el gate real de datos es la verificación visual humana
   contra el JPG, no el juez PVE ni la descripción de la transcripción (que puede venir mal
   transcrita — el generativo llega a leer los valores impresos del scan mejor que la descripción).

8. **Crops de referencia como JPEG liviano — evita HTTP 413.** El gateway Nous rechaza data-URIs
   grandes con **HTTP 413** ("Payload Too Large"): un crop PNG de página a 1509 px (~3.5 MB) bloqueó
   Q017. El crop es solo REFERENCIA visual para el generador → guárdalo como **JPEG q85 con downscale
   a ≤1400 px de ancho** (~450 KB). `auto_prep` ya lo hace por defecto.

9. **Triaje de tipo/riesgo VISUAL — `clasificar_visual.py` (2026-07-05, pieza de autonomía).** El
   paso menos automatizado era el TRIAJE: mirar cada crop para decidir si la figura es
   auto-generable, verificable o va a humano (la descripción `[FIGURA:]` SOBRE-clasifica: "gráfica
   de líneas" capturaba geometría, esquemas-en-opción, ilustraciones+fórmulas). Ahora el cerebro
   gpt-5.5 (visión) MIRA el crop y devuelve `DICTAMEN={tipo, riesgo, features_criticos, razon}`.
   Tipos→riesgo: tabla/barras_simple/linea_simple/ilustracion→**AUTO**; linea_multiserie (el gen
   INTERCAMBIA estilos sólida/punteada/discontinua)/dispersion/poligonal_features→**VERIFICAR**;
   geometria/fisica_rotulada/quimica/figura_en_opciones→**HUMANO**. La decisión final = `combinar()`
   de este riesgo visual con el textual de `auto_prep.clasificar_riesgo` (la MÁS conservadora;
   TRAMPA textual → HUMANO). **Validado 9/9** contra el triaje manual en el lote de líneas de
   Mat-2026-1. Flujo autónomo: `auto_prep → clasificar_visual --items → filtrar AUTO/VERIFICAR/HUMANO`.

10. **Gate AUTOMÁTICO de fidelidad de datos — `gate_fidelidad.py` (2026-07-05, ÚLTIMO ESLABÓN).**
    Reemplaza la verificación MANUAL número-por-número que hacía Claude. Patrón PVE: la PERCEPCIÓN
    la hace gpt-5.5 (visión), la DECISIÓN la hace Python (comparación determinista). **NO uses OCR
    determinista** (pytesseract): falla en gráficas — no lee los números SOBRE las barras (texto
    blanco/color/disperso), sólo marcas de eje. Estrategia **por tipo, 4 ramas** (del clasificador):
    · **Numérica** (barras_simple = cifras rotuladas sobre barras): gpt-5.5 extrae los datos de la
    GENERADA + de la DESCRIPCIÓN [FIGURA:] + (si divergen) del ESCANEO → comparación de 3 fuentes en
    Python. gen==desc→FIEL; gen≠desc y gen==scan→FIEL_SCAN (la descripción venía mal transcrita);
    si no→DUDOSO. · **Celda-a-celda** (tabla_datos → ver lección 11). · **Inventario de geometría**
    (geometria/fisica_rotulada → ver lección 13). · **Visual** (linea/curva/
    poligonal/dispersión = valores leídos del grid, sin cifras rotuladas): `checklist_visual`
    compara GENERADA vs ESCANEO con preguntas dirigidas
    (forma/pendiente, cortes de eje, tramos constantes, intersecciones, posiciones relativas,
    estilos de línea por serie, tildes) → {ok, fallas}. Un feature cualitativo flaggeado degrada
    FIEL→DUDOSO (captura los estilos de línea intercambiados de q099, invisibles a los números).
    **Validado 5/5** contra el juicio manual (q017/q051/q024 FIEL·auto_insertable; q009/q099 DUDOSO)
    **con 0 falsos positivos** (nunca marcó FIEL algo que debía ir a humano — propiedad de seguridad).
    Sólo `auto_insertable ∈ {FIEL, FIEL_SCAN}` se inserta sin humano; DUDOSO → revisión. n=5 es
    prueba-de-concepto representativa (cubre numérico-fiel, numérico-dudoso, visual-fiel,
    visual-dudoso-estilos), NO garantía estadística. **Pipeline autónomo COMPLETO**: `auto_prep →
    clasificar_visual → single_shot (AUTO/VERIFICAR) → gate_fidelidad → insertar (FIEL/FIEL_SCAN) →
    render`. El único paso ya no-manual restante es orquestarlos en un solo `--auto`.

11. **Gate de TABLAS = checklist CELDA-A-CELDA, no multiset numérico (2026-07-05, 6ª tanda).**
    El multiset de cifras planas funciona en barras (números sueltos y discretos) pero da **falsos
    negativos sistemáticos en tablas**: aplana la estructura y no sabe leer ecuaciones (`n!`, `C(n,r)`),
    intervalos (`15≤p<20`), unidades ("minutos"), texto ni comas decimales (`3,5`) — el revisor
    confirmaba FIEL y el gate mandaba a humano (q060/q143/q144/q147). Fix: `tabla_datos` usa
    `gate_fidelidad.checklist_tabla`, que compara la GENERADA vs el ESCANEO **celda por celda** (misma
    fila/columna/encabezado), contando como falla cualquier cifra, símbolo, fórmula, intervalo, unidad,
    palabra, o fila/columna añadida/perdida/reordenada — y una celda **ilegible en el escaneo = falla**
    (falla-seguro a humano). Es el análogo del `checklist_visual` de curvas, para la rejilla. **Validado
    7/7** (`validar_gate_tablas.py`, sin costo Nous): rescata los 4 falsos negativos y sigue atrapando
    alteraciones reales (`q137 n!!≠n!`, `q015 3≠4` en barras). **Bonus de rigor**: destapó que
    `auto_prep` recortaba mal las tablas ANCHAS (q140: crop cortó la 8ª columna → el gate no la podía
    certificar → DUDOSO correcto, la generada sí era fiel).

12. **Crop al borde del CONTENIDO, no a un % fijo (2026-07-05, 7ª tanda).** El recorte de `auto_prep`
    pasó de `0.86·W` fijo (cortaba tablas de 7-8 columnas) a `borde_derecho_contenido(im,W,H)`: detecta
    el borde derecho de la tinta **negra/gris** (oscuro Y poco saturado, `V<128 & S<60`) y AMPLÍA el
    crop hasta ahí, excluyendo la decoración de COLOR saturado de las esquinas ICFES (turquesa/rosa/
    azul, que llegaba al 99% y ensuciaba la detección). Nunca reduce por debajo del 86%; tope 98%.
    Resuelve q140 (crop 86%→93.6%, ahora incluye "Oscar 26"; gate → FIEL). Requiere numpy (sin él cae
    al 86% histórico). Regla operativa: si una tabla ancha aún va a humano por "columna ilegible",
    regenera su crop con el `auto_prep` actual **antes** de sospechar del gate.

13. **Gate de GEOMETRÍA = inventario BIDIRECCIONAL de rótulos, no checklist de forma (2026-07-05,
    8ª tanda).** El experimento de geometría probó que la geometría con datos ROTULADOS SÍ es
    automatizable (el estudiante usa los números escritos, no mide con transportador → un ángulo
    aproximado no afecta la respuesta), PERO el `checklist_visual` (lección 10, pensado para curvas)
    daba **FIEL a una generada que había AGREGADO un rótulo espurio**: q090 (pirámide 3×3×5) inventó
    una etiqueta "A" en el vértice, copiada de la opción A vecina del examen — el checklist de forma
    no enumera rótulos, así que no la veía. Fix: `geometria`/`fisica_rotulada` usan
    `gate_fidelidad.checklist_geometria`, que fuerza al juez a ENUMERAR bidireccionalmente todas las
    etiquetas (letras de vértice, medidas rotuladas, ángulos, fórmulas) de la GENERADA y del ORIGINAL,
    y aplica reglas DURAS en Python (asimetría de seguridad — sólo FIEL si todo limpio): (a) cualquier
    etiqueta en la generada AUSENTE del original = elemento agregado = DUDOSO (atrapa la "A"); (b)
    cualquier medida rotulada distinta gen-vs-scan = DUDOSO; (c) `original_tiene_figura=false` = **crop
    sin figura** = DUDOSO inmediato (q038: enunciado partido en 2 páginas → el crop cayó en la página
    de opciones → la generada se FABRICA desde el texto, sin original que verificar). Ignora estilo
    2D/3D y ángulos aproximados (las medidas escritas mandan → q157 rampa 160/80/α sigue FIEL).
    **Validado 3/3** (`validar_gate_geometria.py`, sin costo Nous): q090→DUDOSO (detecta "A"),
    q038→DUDOSO (crop-sin-figura), q157→FIEL (no-regresión). **Robustez (una corrida dio 2/3):** el
    fallo-seguro por *no-parseo* del juez degradaba una geometría FIEL a DUDOSO por azar → se añadió
    un parser de JSON **balanceado** (`_extraer_json`, no depende del prefijo `CHECK=`, ignora prosa
    con llaves; 7/7 tests) + **reintento** (2) ante no-parseo, y `faltantes` pasó a **advertencia**
    (no degrada; el juez marca faltantes espurios). La seguridad no se toca: un no-parseo residual va
    a DUDOSO, nunca a FIEL. El veto "toda geometría a humano" se reduce así a: trampa (redibujo limpio
    puede borrar el error deliberado), y las que el inventario marque DUDOSO. `clasificar_visual`
    mantiene el piso HUMANO en `geometria`: esta rama sólo actúa cuando el operador FUERZA la
    generación de geometría rotulada (override consciente para un lote).

## Autoentrenamiento — el meta-loop (2026-07-05, capacidad de autooptimización)

**Ya no dependes de que Claude te entrene.** Antes, cada lección (1–10 de arriba) la
razonaba y escribía un humano/Claude a mano tras cada tanda. Ahora el sistema aprende
de su propia experiencia y endurece su motor solo. Piezas en
`…/motor-hermes/entrenamiento/` (ver `README-autoentrenamiento.md`):

- **Telemetría**: `auto.py` registra CADA caso que procesas en `casos.jsonl` (tipo,
  riesgo textual/visual, decisión, veredicto del gate, resultado). Es tu memoria de
  experiencia; no la borres.
- **Feedback**: cuando reportes un caso como DUDOSO/A-HUMANO y un revisor lo juzgue,
  se anota en `feedback.jsonl` (`python3 registro.py --feedback <SUB> <NUM> <VEREDICTO>
  "<nota>"`; VEREDICTO ∈ CONFIRMA_FIEL | RECHAZAR | TRAMPA_NO_DETECTADA | ERA_FIEL). Es
  tu única señal supervisada, y solo sobre los casos que tú mismo marcaste dudosos.
- **Autoentrenar**: corre `python3 ciclo.py` (destila lecciones + aplica las
  auto-aplicables + reporta las que van a humano). Hazlo tras acumular feedback o
  cerrar una tanda grande — no en cada figura.

**Regla de oro del autoentrenamiento — ASIMETRÍA DE SEGURIDAD**: solo te está
permitido ENDURECER de forma autónoma (añadir una trampa nueva, subir el piso de
riesgo de un tipo que demostró ser frágil). RELAJAR (bajar un riesgo, auto-generar un
tipo antes vetado) **nunca** es autónomo: se queda como lección candidata para que un
humano la apruebe. Un falso positivo cuesta trabajo; un falso negativo corrompe un
ítem. La política aprendida vive en `entrenamiento/politica_aprendida.json` (datos, no
código) y `auto_prep`/`clasificar_visual` ya la leen al arrancar. Verifica el
mecanismo con `python3 test_meta_loop.py` (debe dar "TODOS LOS CHEQUEOS PASARON").

## Cómo ejecutar

El harness vive en el repo Todo-Pajaro:

```
HARNESS=/home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Mejorando-Imagenes/motor-hermes/harness_mejora.py
```

Corre así (puede tardar minutos por ítem → lánzalo en BACKGROUND y sondea el
archivo de resultados; NO lo dejes en foreground, el terminal se corta a ~180 s):

```bash
nohup python3 "$HARNESS" --manifest /ruta/items.json --max-iter 3 \
  > /ruta/items.log 2>&1 &
# luego sondea:
tail -n 40 /ruta/items.log
cat /ruta/items.json.resultados.json
```

Flags: `--dry-run` (valida manifest+routing sin generar), `--only <id>`
(procesa un solo ítem), `--max-iter N` (reintentos, default 3).

## Formato del manifest (`items.json`)

```json
{"items": [
  {
    "id": "q085",
    "crop": "/ruta/recorte.png",           // o: "jpg": "/orig.jpg", "coords": [x0,y0,x1,y1]
    "prompt": "/ruta/prompt_qNNN.txt",       // prompt flat-vector afinado del ítem
    "out": "/ruta/imagenes-mejoradas/q085.png",
    "tipo": "objeto",                        // ilustracion | texto_denso | objeto | datos
    "expected_labels": ["Madera", "Cemento"], // opcional; refuerza el chequeo de texto
    "contexto": "enunciado oficial del ítem"  // ★ ancla de verdad de terreno del juez PVE
  }
]}
```

**★ Incluye SIEMPRE `contexto`** (el enunciado oficial) cuando el recorte sea un
escaneo de baja resolución: el juez-visión puede alucinar el contenido del scan y
rechazar generaciones fieles (incidente Q179: leyó una trompeta como violín y no
convergió en 3 iteraciones). El `contexto` tiene prioridad sobre el scan ambiguo.

## Routing por tipo (qué generador elige el harness)

- `texto_denso` / `ilustracion` → `fal-ai/nano-banana-pro` (mejor texto ES + tildes + estética).
- `objeto` → `fal-ai/gpt-image-2` (objetos / mecánica física simple).
- `datos` → NO generativo (skip → vector).

## Qué reportar al usuario

Lee `items.json.resultados.json` y resume por ítem el `estado`:
`ACEPTADO` (con `iter_aceptada`) · `NO_CONVERGE` (→ revisión humana / vector) ·
`SKIP_DATOS_VECTOR` · `ERROR`. Para los `ACEPTADO`, el PNG final quedó en su
`out`. Con `--insertar` (+ bloque `insert` por ítem: num, transcripcion, ficha,
imagenes_dir, alt, area_prefix) el harness **inserta** la figura en la
transcripción + ficha y re-renderiza automáticamente (idempotente; patrón
Naturales por defecto). Sin `--insertar`, la inserción queda como paso aparte.

## Detalle técnico

Arquitectura y verificación del plano de datos:
`…/Mejorando-Imagenes/motor-hermes/PIEZA1-cerebro-reparado.md` y
`README-harness.md`. El cerebro NO manipula bytes de imagen (la referencia va
como data-URI vía `encode_file` dentro del shim `generar_imagen.py`, porque el
gateway de Nous no acepta rutas locales).
