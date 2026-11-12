Produce el video explicativo narrado de un ejercicio ICFES ya aprobado, en horizontal y en vertical, con cuatro paradas en las que el profesor aprueba. Guía completa: `Auxiliares/video-icfes/GUIA.md`.

## Input recibido

$ARGUMENTS

## Instrucciones para Claude

Si `$ARGUMENTS` está vacío, responde SOLO con: `Uso: /video-icfes <carpeta del ejercicio>` (por ejemplo `A-Produccion/01-En-PreDesarrollo/vueltas-llanta-perimetro-n4`) y termina.

Lee primero `Auxiliares/video-icfes/GUIA.md`. Todo el trabajo técnico lo hace la cadena común; tú escribes el guion y las escenas del ejercicio y llevas al profesor de parada en parada.

```
CAD="env PYTHONPATH=Auxiliares/video-icfes $HOME/.venvs/manim/bin/python -m video_icfes"
V=<carpeta del ejercicio>/salida/video
$CAD.estado $V                      # en qué parada va
$CAD.cadena $V --etapa revisar      # guion + voz + palabras ancla (~2 min)
$CAD.cadena $V --etapa borrador     # video mudo a baja resolución + fotogramas (~8 min)
$CAD.cadena $V                      # todo, los dos formatos (~25 min): en segundo plano
$CAD.revisores preparar $V guion    # arma el expediente de un revisor e imprime su encargo (también: oido, pantalla --formato h|v, herramienta)
$CAD.revisores resumen $V           # junta los hallazgos en $V/revisores/informe_revisores.md
$CAD.revisores correccion $V "<lo que pidió cambiar el profesor>"
```

### Reglas que no se negocian
1. Solo ejercicios con `ITEM_OFICIAL.md` y aprobación del profesor (la cadena lo comprueba). No se edita ni se teje ningún `.Rmd`, `.tex`, semillero ni `ejercicio_state.json`. Sin commits salvo que el profesor los pida.
2. Las opciones se muestran sin letras A-D y la respuesta nunca se identifica por letra ni posición, ni en la voz ni en pantalla. Una errata del ítem oficial no se muestra ni se dice.
3. La clave y el error de cada distractor salen de `ITEM_OFICIAL.md` y de la Solution del `.Rmd` aprobado; no se inventan.
4. `VOL_MUSICA` (en `guion.py`) solo lo cambia el profesor, oyendo. Ningún modelo de Claude oye audio: el sonido se juzga por `revision_<formato>/audio_medidas.md`, y lo que ahí salga distinto en la transcripción es un tramo para que el profesor lo oiga, no un defecto confirmado. No regenerar más de dos veces persiguiendo una palabra sin que él la oiga.
5. No se borran intermedios (`media_*`, `copias/`, fotogramas) sin que el profesor lo pida. No se instala nada.
6. Subagentes en Sonnet y SIN `name:`. Preguntas al profesor en lenguaje llano, con la consecuencia de cada opción.
7. **Revisores.** Cada revisor es un agente `general-purpose` en Sonnet, SIN `name:`, que no escribió el video. Su prompt es, tal cual, el encargo que imprime `revisores preparar` (queda en `$V/revisores/<revisor>/encargo.txt`). Los revisores informan: nunca bloquean ni corrigen. Antes de mostrar sus hallazgos, comprueba que `revisores comprobar` dio 0 para cada uno y contrasta con el código o el fotograma los de gravedad alta: un revisor puede equivocarse. Si uno no entrega `hallazgos.json`, relánzalo una vez; si tampoco, dilo al profesor sin sustituirlo por tu propia revisión.
8. **Cada cambio que pida el profesor se anota** con `revisores correccion`, con sus palabras, antes de aplicarlo. De ahí aprende la herramienta.
9. Una aprobación se registra con `$CAD.estado $V aprobar <parada> "<lo que dijo>"` solo después de que el profesor la dé en la conversación.

### Recorrido
Corre `$CAD.estado $V` y continúa desde la primera parada pendiente.

**A. Preparar (si `$V` no existe).** Copia `Auxiliares/video-icfes/plantilla/guion.py` y `escenas.py` a `$V/`. Lee `ITEM_OFICIAL.md`, la Solution del `.Rmd` y mira la imagen del ítem en `Originales/` si la hay.

**B. Guion → PARADA 1.** Escribe `guion.py` con las nueve escenas de la plantilla (gancho con el resultado, enunciado y opciones, concepto, dos casos, generalización, construcción de la respuesta, comprobación con un número, descarte de cada distractor por su error, cierre con transferencia y reto); objetivo 3 a 4 minutos. Corre `--etapa revisar` y corrige errores y avisos. Lanza el revisor «guion» y, con la voz ya hecha, el revisor «oido» (`preparar $V guion` y `preparar $V oido`), los dos a la vez. Muéstrale al profesor el texto completo, escena por escena, seguido de los hallazgos de los dos revisores en lenguaje llano, y espera su aprobación.

**C. Plan de escenas → PARADA 2.** Escribe `$V/plan_escenas.md`: por escena, qué se ve, en qué palabra aparece cada cosa, qué piezas de `video_icfes/piezas/` usa y cómo se acomoda en vertical. Muéstraselo y espera.

**D. Escenas y primera versión → PARADA 3.** Escribe `escenas.py` (zonas, no coordenadas fijas; datos en constantes). Itera con `--etapa borrador` mirando los fotogramas de `borrador_h/` y `borrador_v/` hasta que no haya solapes ni nada fuera del cuadro. Declara en `MUSICA["efectos"]` del guion los efectos atados a palabras. Lanza la cadena completa en segundo plano. Al terminar: lee `informe_cadena.json` y los dos `audio_medidas.md`, y mira los fotogramas de `revision_h/` y `revision_v/` (di cuántos miraste de cuántos). Entrégale al profesor las rutas de los dos MP4 y los tramos que conviene oír. Espera.

**E. Revisores y versión final → PARADA 4.** Con el video montado, lanza a la vez tres revisores: «oido» (ahora con la transcripción de la mezcla), «pantalla» horizontal y «pantalla» vertical (`preparar $V oido`, `preparar $V pantalla --formato h`, `preparar $V pantalla --formato v`). Cuando entreguen, corre `revisores resumen $V` y presenta al profesor los hallazgos del más grave al menos grave. Aplica los que elija (máximo 5 por tanda), anotando cada uno con `revisores correccion`, y regenera. Si el profesor usa un modelo externo que sí oye (Gemini), sus afirmaciones también se contrastan con el código: inventa detalles y confunde texto de pantalla con voz. Entrega y espera la aprobación final.

**F. Cierre.** Tras la parada 4: `$CAD.cadena $V --etapa congelar`. Después lanza el revisor «herramienta» (`preparar $V herramienta`): compara lo que corrigió el profesor con lo que vieron los revisores y propone lecciones. Muéstrale las propuestas al profesor; las que acepte se registran con `$CAD.revisores registrar $V P1 P3`. Las de tipo instrucción entran solas en el expediente de los revisores del próximo video; las mecánicas quedan «pendiente de programar» y se programan con su prueba cuando el profesor lo pida.

### Reporte en cada entrega
Qué se verificó y qué no: fotogramas vistos de cuántos, por formato; compuertas de audio; qué revisores pasaron, cuántos hallazgos dejó cada uno y cuáles se aplicaron; que nadie ha oído el audio salvo el profesor; md5 del `.Rmd` (lo imprime la cadena).
