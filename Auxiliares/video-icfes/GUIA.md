# Videos explicativos de ejercicios ICFES — guía de uso

Sale del video de `vueltas-llanta-perimetro-n4` (noviembre de 2026). Produce, de un ejercicio ya
aprobado, un video narrado de 3 a 4 minutos en horizontal (1920×1080) y vertical (1080×1920), cada uno
con subtítulos y con una copia sin subtítulos para editar aparte. Se reproduce sin internet.

## Pedir un video

En Claude Code, abierto en la raíz del repo:

    /video-icfes A-Produccion/01-En-PreDesarrollo/<ejercicio>

El proceso se detiene cuatro veces a esperar al profesor:

| Parada | Qué revisa el profesor |
|---|---|
| 1. Guion | El texto que dirá la voz (`guion.py`) |
| 2. Plan de escenas | Qué se dibuja en cada escena (`plan_escenas.md`) |
| 3. Primera versión | Los dos MP4: verlos y **oírlos** |
| 4. Versión final | Los MP4 después de las correcciones del revisor adversario |

## Dónde queda cada cosa

    Auxiliares/video-icfes/video_icfes/     programas comunes (no se copian a cada ejercicio)
    Auxiliares/video-icfes/plantilla/       guion.py y escenas.py de partida
    <ejercicio>/salida/video/               guion.py, escenas.py, plan_escenas.md, estado_video.json
        NOMBRE_h.mp4, NOMBRE_v.mp4          los videos con subtítulos
        NOMBRE_h_sin_subtitulos.mp4, …      para editar aparte
        revision_h/, revision_v/            fotogramas (uno cada 2 s), narración con tiempos, medidas de audio
        copias/                             el MP4 anterior de cada regeneración
        comun_congelado/                    los programas comunes con los que se aprobó (tras la parada 4)

`salida/` está fuera de git.

## A mano (sin el comando)

    CAD="env PYTHONPATH=Auxiliares/video-icfes $HOME/.venvs/manim/bin/python -m video_icfes"
    $CAD.estado <carpeta>                       en qué parada va
    $CAD.cadena <carpeta> --etapa revisar       guion, voz y palabras ancla (~2 min)
    $CAD.cadena <carpeta> --etapa borrador      video mudo a baja resolución (~8 min los dos formatos)
    $CAD.cadena <carpeta>                       todo (~25 min los dos formatos; --formato h para uno solo)
    $CAD.cadena <carpeta> --etapa congelar      tras la aprobación final

La voz se genera con edge-tts y necesita internet **solo al generar**; si el guion no cambia, se reutiliza.

## Lo que la cadena detiene sola

1. La carpeta no pertenece a un ejercicio con `ITEM_OFICIAL.md`, aprobación del profesor y `.Rmd` con el md5 firmado.
2. El guion tiene cifras, símbolos, o nombra una opción por letra o posición; o un descarte no cierra con 1 s de pausa.
3. Una palabra que ancla una animación (`self.en("palabra", n)`) o un efecto de sonido (`MUSICA["efectos"]`) no la dice la voz. El mensaje nombra la escena y la palabra.
4. Faltan las aprobaciones de las paradas 1 y 2 para dibujar, o la 4 para congelar.
5. El audio final no está en −15 ± 0,5 LUFS, su pico pasa de −0,5 dBFS, o entre 1000 y 3000 Hz la música tapa la voz en el 10 % peor de los momentos.
6. Algún `.Rmd` del ejercicio cambió durante la cadena.

Además avisa (sin detener) de las animaciones que llegan más de 0,6 s tarde a su palabra y de las letras sueltas sin coma.

## Lo que la cadena NO puede comprobar

- Cómo suena. Ningún modelo de Claude oye audio. `audio_medidas.md` trae mediciones y una transcripción
  automática que cambia de una corrida a otra: lo que marque es un tramo para que el profesor lo oiga.
- El volumen de la música: es `VOL_MUSICA` en `guion.py` y solo lo cambia el profesor.
- Que los dibujos estén bien compuestos: hay que mirar los fotogramas de los dos formatos.
- Que la explicación sea correcta y se entienda: para eso están los revisores (abajo) y las paradas.

## Los revisores (informan; no bloquean ni corrigen)

Cuatro agentes distintos de quien escribió el video. Cada uno recibe un **expediente** con datos medidos, que arma
`python -m video_icfes.revisores preparar <carpeta> <revisor>`, y deja sus hallazgos en
`<carpeta>/revisores/<revisor>/hallazgos.json`. Sus instrucciones están en `Auxiliares/video-icfes/revisores/`.

| Revisor | Cuándo | Qué recibe | Qué busca |
|---|---|---|---|
| `guion` | antes de la parada 1 | el guion frase por frase, el ítem oficial, la Solution | frases con dos lecturas, pasos sin justificar, matemática contra el ítem |
| `oido` | antes de la parada 1 (voz) y de la 4 (mezcla) | pausas en cada signo, frases donde la pausa más corta cae antes de «y/o/pero», palabras que la voz pega, lo que oyó el transcriptor | lo que entendería quien solo oye |
| `pantalla` (uno por formato) | antes de la parada 4 | hojas con un fotograma cada 2 s, uno por palabra ancla, y las 6 anclas tras las que la pantalla menos cambia (antes, durante y después) | texto ilegible, pantalla vacía, resaltes que no se notan, pantalla y voz que no coinciden |
| `herramienta` | al cierre | las correcciones del profesor (`revisores correccion`), los hallazgos de los otros y los avisos de la cadena | qué se le escapó a la herramienta y cómo evitarlo: lecciones para un revisor o comprobaciones nuevas, cada una con su prueba |

- `revisores comprobar` rechaza un hallazgo cuya cita no esté literal en el lugar que dice (guion, expediente, ítem,
  `escenas.py`) o cuyo fotograma no exista. Máximo 10 hallazgos por revisor.
- `revisores resumen` junta todo en `revisores/informe_revisores.md`, del más grave al menos grave.
- Las propuestas del revisor `herramienta` solo entran con `revisores registrar <carpeta> P1 P3`, cuando el profesor
  las acepta. Quedan en `revisores/lecciones.json`: las de tipo instrucción aparecen solas en el expediente del
  revisor en el siguiente video; las mecánicas quedan «pendiente de programar» y pasan a «programada» cuando
  existe la comprobación y su prueba (un mutante que la hace fallar).
- Registradas el 2026-11-12 (video de la llanta): L-001 y L-002, programadas (`montar.comprobar_signos` lee el `.ass`
  ya escrito y `guion_lint` exige ¿? y ¡! emparejados; las dos probadas con su mutante); L-003 a L-005, instrucciones
  para los revisores de guion y oído.
- Dos `animate` en una misma `play` sobre un grupo y sobre uno de sus hijos se anulan (se ve solo el del grupo):
  usar `generate_target()` + `MoveToTarget` (medido: el borde amarillo de la tarjeta leída no aparecía).
- Un revisor puede equivocarse o quedarse corto en la gravedad: los hallazgos de gravedad alta se contrastan con el
  código o el fotograma antes de mostrarlos.

## Escribir las escenas

- `class Video(VideoBase)` con un método por escena: `s1_…` a `s9_…`, cada uno empieza con `self.ini("sN")` y termina con `self.fin()`.
- Zonas en vez de coordenadas: `self.ubicar(mob, "figura")`, `self.repartir(mobs, "figura")` (fila en horizontal, rejilla en vertical), `self.apilar`, `self.titulo`, `self.pie`. Zonas: `titulo`, `figura`, `a`, `b` (lado a lado en horizontal, una sobre otra en vertical), `pie`, `todo`. Si no basta: `hv(valor_h, valor_v)`.
- Textos nunca por debajo de tamaño 32 ni más oscuros que `GRIS_CLARO`.
- Piezas: `piezas/opciones.py` (tarjetas sin letras; probada en un video completo), `piezas/estadistica.py`, `piezas/geometria.py`, `piezas/probabilidad.py` (probadas solo con escenas de muestra: `piezas/muestra.py`).

## Estado de lo probado (2026-11-12)

Prueba de aceptación: el video de `vueltas-llanta-perimetro-n4`, regenerado en `salida/video_auto/`.

| Prueba | Resultado |
|---|---|
| Horizontal, sin intervención manual | 104 de 104 fotogramas idénticos al video hecho a mano; −15,1 LUFS; pico −0,7 dBFS; voz sobre música en 1–3 kHz, percentil 10 = 4,1 dB |
| Vertical | Se genera con el mismo audio. Composición revisada en 14 de 104 fotogramas: nada fuera del cuadro ni solapado; en las escenas 3, 4 y 5 el dibujo queda pequeño y sobra media pantalla |
| Mutante: «Solo» → «Únicamente» en el guion | La cadena se detuvo nombrando «solo» (vez 2), en música y en escenas |
| md5 del `.Rmd` | Igual antes y después |
| Aviso de atrasos | Encontró dos animaciones tardías en la escena 6 del video hecho a mano; corregidas con `self.falta()` |

Calibración de los revisores (2026-11-12), con dos versiones de la llanta que tenían defectos que el profesor encontró ese día:

| Defecto conocido | Revisor | Resultado |
|---|---|---|
| Unidades mezcladas sin explicar (escena 9) | guion | Lo encontró a la primera (gravedad media) |
| «va en» oído como «van» | oido | Lo encontró a la primera y propuso «se mide en» |
| «en centímetros y la distancia» mal agrupado | oido | No lo vio a la primera; sí tras añadir al expediente las frases con la pausa más corta antes de «y» (lo calificó de baja) |
| Media pantalla vacía en vertical | pantalla (v) | Lo encontró a la primera |
| Resalte del 100 que dura un instante | pantalla (h) | No lo vio a la primera (el fotograma era posterior al destello); sí tras añadir las 6 anclas con menos cambio y sus tres fotogramas |
| Subtítulos sin signos | ninguno | Sin calibrar: el montaje ya los pone siempre; ningún revisor lee el archivo de subtítulos |

El revisor `herramienta` se probó una vez: hizo el balance de 3 correcciones del profesor y propuso 5 mejoras; ninguna
está registrada ni programada. Cada revisor se probó con un solo ejercicio y una o dos corridas: no se sabe cuánto
varían sus hallazgos de una corrida a otra.

Sin probar: nadie ha oído los videos de prueba; las piezas de estadística, geometría y probabilidad solo
tienen escenas de muestra estáticas (sin animación, sin video completo); el comando `/video-icfes` no se ha
recorrido de principio a fin con un ejercicio nuevo; `--etapa congelar` no se ha ejecutado.
