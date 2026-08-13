Lanza el agente `orquestador-schoice`: pipeline end-to-end del workflow ICFES SCHOICE (11 pasos, 3 pausas humanas obligatorias). Wrapper que delega al agente vía `Task(subagent_type="orquestador-schoice", ...)`.

## Input recibido

$ARGUMENTS

## Instrucciones para Claude

Si `$ARGUMENTS` está vacío, responde SOLO con este mensaje y termina:

````
Uso: /orquestador-schoice <input>

Donde <input> es UNO de:

1) JSON completo (recomendado para producción):
   /orquestador-schoice {"ruta_destino":"A-Produccion/01-En-PreDesarrollo/mi-ejercicio","nombre_ejercicio":"mediana_aleatorio_argumentacion_n3_schoice_v1","entrada":"<ruta-imagen-o-texto>","modo":"ejecutar"}

2) Forma corta (texto libre): describe destino + entrada y yo construyo el JSON antes de lanzar:
   /orquestador-schoice mediana n3 desde imagenes/p23.png en 01-En-PreDesarrollo/mediana-v1

3) Modo dry-run (audita el plan sin ejecutar):
   /orquestador-schoice {"ruta_destino":"...","nombre_ejercicio":"...","entrada":"...","modo":"dry-run"}

Schema del JSON (ver `.claude/agents/orquestador-schoice.md`):
- ruta_destino:    debe estar bajo A-Produccion/01-En-PreDesarrollo/ o /02-En-Desarrollo/
- nombre_ejercicio: <tema>_<componente>_<competencia>_n<2|3|4>_schoice_v<N>  (componente: geometrico_metrico|numerico_variacional|aleatorio · competencia: interpretacion_representacion|formulacion_ejecucion|argumentacion)
- entrada:         ruta a imagen ICFES original | texto del enunciado
- modo:            "ejecutar" | "dry-run"
- opciones_extra:  { patron_metacognitivo, max_reintentos_por_fase, auto_seleccionar_grafico } (opcional)
````

Si `$ARGUMENTS` contiene contenido:

1. **Detecta el formato:**
   - Si empieza con `{` y parsea como JSON → usar tal cual.
   - Si es texto libre → construye un JSON mínimo razonable a partir de él (pide confirmación al usuario en una sola línea antes de lanzar si hay ambigüedad sobre `ruta_destino` o `nombre_ejercicio`).

2. **Valida pre-flight ligero antes de delegar** (no reemplaza los pre-flight del agente, solo evita lanzamientos obviamente rotos):
   - `ruta_destino` está bajo `A-Produccion/01-En-PreDesarrollo/` o `02-En-Desarrollo/`. Si está bajo `03-En-Produccion/` o `Ejemplos-Funcionales-Rmd/` → **rechaza** y muestra la regla violada.
   - `nombre_ejercicio` matchea `^[a-z0-9_]+_(geometrico_metrico|numerico_variacional|aleatorio)_(interpretacion_representacion|formulacion_ejecucion|argumentacion)_n[1-4]_schoice(_neg)?_v[0-9]+$` (**bloqueo**: la nomenclatura es obligatoria — ver `.claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md`).
     Fuente única: `.claude/docs/NOMENCLATURA_ARCHIVOS_RMD.md`
   - `modo` ∈ {"ejecutar","dry-run"}; si falta, default `"ejecutar"`.

3. **Lanza el agente** con un único `Task` call (foreground, NO background — el orquestador necesita interactuar con el usuario en los 3 `WAIT_USER`):

   ```
   Task(
     subagent_type = "orquestador-schoice",
     description = "Orquestador SCHOICE: <nombre_ejercicio>",
     prompt = <JSON serializado del input validado>
   )
   ```

4. **Cuando el agente retorne**, presenta al usuario:
   - El `exit_status` reportado por el agente.
   - Resumen de pasos completados (lista de los 11 con ✅/⬜).
   - Si quedó pendiente un `WAIT_USER`, indica cuál y qué decisión necesita tomar el usuario para reanudar (ej: "Reanuda con `/orquestador-schoice {...,\"modo\":\"ejecutar\"}` después de responder Flujo B").
   - Ruta del `.Rmd` final si llegó al paso 11.

5. **No dupliques trabajo**: el agente ya hace sus propios pre-flight checks, validaciones y manejo de errores. Tu job aquí es: parsear input, validar lo mínimo, delegar, reportar.

## Salvaguardas que aplica el agente

Este wrapper es delgado a propósito: las defensas viven en `.claude/agents/orquestador-schoice.md` (pre-flight checks + incidentes A-L). Resumen para quien lea este comando sin abrir el agente:

- **Regla #22 — Diversidad sustantiva** ([`.claude/rules/diversidad-sustantiva.md`](../rules/diversidad-sustantiva.md)): el paso 9 ejecuta `.claude/scripts/validar_diversidad_sustantiva.R --n 100`; `ERR_DIV_COSMETICA` (respuesta correcta invariante) es **BLOQUEANTE** (exit 1).
- **Error 23** (etiquetas solapadas en diagramas dinámicos, caso extremo de parámetros) y **Error 24** (predictibilidad posicional/orientacional + distractor extremo por construcción algebraica) — ver [`.claude/docs/patrones-errores-conocidos.md`](../docs/patrones-errores-conocidos.md).
- **Reglas #18, #19, #20**: imágenes Markdown con `{width=...}` (anti `\pandocbounded`), Solution letter-independent (nunca `r letra_correcta`/"Opción A-D"), guard `\newcounter{none}` en tablas Markdown.
- **Incidentes I-L (2026-07-28)**: (I) nunca reseedear el RNG con `set.seed(Sys.time()/proc.time())` dentro de `data_generation` — pisa el argumento `seed` del llamador y hace irreproducible cualquier fallo multi-semilla; (J) umbrales de legibilidad en opciones gráficas van en CASCADA (`c(0.40,0.35,0.30,0.25)`, helper `seleccionar_combinacion_con_cascada()` — Familia 6 en [`.claude/scripts/snippets_familias_rmd.R`](../scripts/snippets_familias_rmd.R)), nunca un umbral único con `stopifnot`; (K) si las opciones gráficas muestran su valor numérico, incluir distractores que CONSERVEN ese valor y difieran solo en la dimensión evaluada (dirección/orientación), para que el rótulo no resuelva el ítem por sí solo; (L) ecuaciones `$$...$$` dentro de listas numeradas deben ir indentadas, no a columna 0 (rompe la enumeración en PDF).

## Notas

- El agente tiene `maxTurns: 60` y `model: opus`. Una corrida completa es costosa.
- Para auditar antes de gastar tokens: usar `modo: "dry-run"` la primera vez.
- El agente soporta **reanudación**: si `ejercicio_state.json` existe en `ruta_destino`, retoma desde el primer paso pendiente. No hace falta reiniciar.
