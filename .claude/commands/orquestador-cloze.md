Lanza el agente `orquestador-cloze`: pipeline end-to-end del workflow ICFES CLOZE (11 pasos, 3 pausas humanas obligatorias). Gemelo de `/orquestador-schoice`, adaptado a ejercicios CLOZE (Progressive Disclosure mín. 4 partes, exclozetype multi-gap, ##ANSWERi## en orden). Wrapper que delega al agente vía `Task(subagent_type="orquestador-cloze", ...)`.

## Input recibido

$ARGUMENTS

## Instrucciones para Claude

Si `$ARGUMENTS` está vacío, responde SOLO con este mensaje y termina:

````
Uso: /orquestador-cloze <input>

Donde <input> es UNO de:

1) JSON completo (recomendado para producción):
   /orquestador-cloze {"ruta_destino":"A-Produccion/01-En-PreDesarrollo/mi-ejercicio","nombre_ejercicio":"mediana_metacognitivo_argumentacion_n3_cloze_v1","entrada":"<ruta-imagen-o-texto>","modo":"ejecutar"}

2) Forma corta (texto libre): describe destino + entrada y yo construyo el JSON antes de lanzar:
   /orquestador-cloze mediana n3 desde imagenes/p23.png en 01-En-PreDesarrollo/mediana-cloze-v1

3) Modo dry-run (audita el plan sin ejecutar):
   /orquestador-cloze {"ruta_destino":"...","nombre_ejercicio":"...","entrada":"...","modo":"dry-run"}

Schema del JSON (ver `.claude/agents/orquestador-cloze.md`):
- ruta_destino:    debe estar bajo A-Produccion/01-En-PreDesarrollo/ o /02-En-Desarrollo/
- nombre_ejercicio: <tema>_metacognitivo_<competencia>_n<2|3|4>_cloze_v<N>
- entrada:         ruta a imagen ICFES original | texto del enunciado
- modo:            "ejecutar" | "dry-run"
- opciones_extra:  { patron_progressive_disclosure, n_partes, max_reintentos_por_fase, auto_seleccionar_grafico } (opcional)
````

Si `$ARGUMENTS` contiene contenido:

1. **Detecta el formato:**
   - Si empieza con `{` y parsea como JSON → usar tal cual.
   - Si es texto libre → construye un JSON mínimo razonable a partir de él (pide confirmación al usuario en una sola línea antes de lanzar si hay ambigüedad sobre `ruta_destino` o `nombre_ejercicio`).

2. **Valida pre-flight ligero antes de delegar** (no reemplaza los pre-flight del agente, solo evita lanzamientos obviamente rotos):
   - `ruta_destino` está bajo `A-Produccion/01-En-PreDesarrollo/` o `02-En-Desarrollo/`. Si está bajo `03-En-Produccion/` o `Ejemplos-Funcionales-Rmd/` → **rechaza** y muestra la regla violada.
   - `nombre_ejercicio` matchea `^[a-z0-9_]+_metacognitivo_[a-z]+_n[234]_cloze_v[0-9]+$` (warning, no bloqueo, si no coincide).
   - `modo` ∈ {"ejecutar","dry-run"}; si falta, default `"ejecutar"`.
   - `n_partes` (si viene en `opciones_extra`) debe ser ≥ 4. Si es < 4 → warning: el agente lo subirá a 4 (regla `ejercicios-metacognitivos.md` § CLOZE).

3. **Lanza el agente** con un único `Task` call (foreground, NO background — el orquestador necesita interactuar con el usuario en los 3 `WAIT_USER`):

   ```
   Task(
     subagent_type = "orquestador-cloze",
     description = "Orquestador CLOZE: <nombre_ejercicio>",
     prompt = <JSON serializado del input validado>
   )
   ```

4. **Cuando el agente retorne**, presenta al usuario:
   - El `exit_status` reportado por el agente.
   - Resumen de pasos completados (lista de los 11 con ✅/⬜).
   - Estado de las validaciones CLOZE V1-V5 (conteo gaps, orden ##ANSWERi##, exsolution/extol por gap, mínimo 4 partes, gráficas-opción fuera del gap — Incidente G).
   - Estado de NOPS: si fue `N/A (esperado, gaps num/string)`, acláralo — NO es un fallo.
   - Si quedó pendiente un `WAIT_USER`, indica cuál y qué decisión necesita tomar el usuario para reanudar (ej: "Reanuda con `/orquestador-cloze {...,\"modo\":\"ejecutar\"}` después de responder Flujo B").
   - Ruta del `.Rmd` final si llegó al paso 11.

5. **No dupliques trabajo**: el agente ya hace sus propios pre-flight checks, validaciones V1-V5 y manejo de errores. Tu job aquí es: parsear input, validar lo mínimo, delegar, reportar.

## Salvaguardas que aplica el agente

Este wrapper es delgado a propósito: las defensas viven en `.claude/agents/orquestador-cloze.md` (pre-flight checks + incidentes A-J). Resumen para quien lea este comando sin abrir el agente:

- **Regla #22 — Diversidad sustantiva** ([`.claude/rules/diversidad-sustantiva.md`](../rules/diversidad-sustantiva.md)): el paso 9 ejecuta `.claude/scripts/validar_diversidad_sustantiva.R --n 40`; `ERR_DIV_COSMETICA` (respuesta correcta invariante en cualquier parte/gap) es **BLOQUEANTE** (exit 1).
- **Error 23** (etiquetas solapadas en diagramas dinámicos, caso extremo de parámetros) y **Error 24** (predictibilidad posicional/orientacional + distractor extremo por construcción algebraica) — ver [`.claude/docs/patrones-errores-conocidos.md`](../docs/patrones-errores-conocidos.md).
- **Reglas #18, #19, #20**: imágenes Markdown con `{width=...}` (anti `\pandocbounded`), Solution letter-independent en TODAS las sub-partes schoice (nunca `r letra_correcta_pN`/"Opción A-D"), guard `\newcounter{none}` en tablas Markdown.
- **Incidente G (CLOZE, [`.claude/rules/graficos-como-opciones.md`](../rules/graficos-como-opciones.md))**: las gráficas-opción NUNCA van dentro del gap `MULTICHOICE`/`MULTIRESPONSE` — Moodle no renderiza `<img>` ahí. Van ROTULADAS (I, II, III, IV) en el ENUNCIADO de la parte, con las opciones del gap como texto ("Gráfica I"…). Verificado en V5 (paso 10) sobre el XML de Moodle.

## Notas

- El agente tiene `maxTurns: 65` y `model: opus`. Una corrida completa es costosa.
- Para auditar antes de gastar tokens: usar `modo: "dry-run"` la primera vez.
- El agente soporta **reanudación**: si `ejercicio_state.json` existe en `ruta_destino`, retoma desde el primer paso pendiente. No hace falta reiniciar.
- **CLOZE vs SCHOICE**: usa este orquestador cuando el ejercicio requiere múltiples niveles cognitivos en secuencia (Progressive Disclosure ≥ 4 partes, gaps mixtos num/schoice/mchoice). Para una sola pregunta de selección única, usa `/orquestador-schoice`.
- **NOPS**: para CLOZE con gaps `num` o `string`, `exams2nops()` retorna N/A por diseño (la hoja escaneable solo soporta selección). El agente NO trata esto como error.
- **Gráficas-opción (Incidente G / V5)**: si una sub-parte ofrece gráficas como opciones, en CLOZE NO pueden ir dentro del gap (Moodle no renderiza `<img>` en un gap → "no se ven los gráficos en el Paso N"). El agente las pone ROTULADAS (I, II, III…) en el enunciado de la parte y deja las opciones del gap como texto ("Gráfica I"…). Esto es distinto del SCHOICE puro, donde las imágenes-opción sí funcionan. El agente valida en V5 que ningún gap del XML de Moodle contenga imágenes.
