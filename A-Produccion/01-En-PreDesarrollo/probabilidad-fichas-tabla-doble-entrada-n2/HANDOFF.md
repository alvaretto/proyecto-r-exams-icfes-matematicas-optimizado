# HANDOFF — `probabilidad-fichas-tabla-doble-entrada-n2`

> Archivo de retorno (convención del repo). Frase disparadora: «Continúa con
> probabilidad-fichas-tabla-doble-entrada-n2». Última actualización: 2026-10-27 ~23:30. Si este archivo
> y `ejercicio_state.json` discrepan, manda el JSON y sus notas.

## Qué es

SCHOICE `probabilidad_fichas_tabla_doble_entrada_aleatorio_formulacion_ejecucion_n2_schoice_v1.Rmd`
(md5 final `4fa520bb…`), de la pregunta impresa 32 del simulacro SAI «Nivelación 6» de grado 10
(Sesión 2). Ítem verbatim y alineación: [`ITEM_OFICIAL.md`](ITEM_OFICIAL.md). Invariantes I-1…I-8,
guardias y **todas las firmas del profesor**: [`.claude/CLAUDE.md`](.claude/CLAUDE.md).

## Estado: 11/11, aprobado para el aula por el profesor el 2026-10-27 (~23:35)

No está en producción: la promoción espera la evidencia de aula. Sin `git add` ni commit hasta que el
profesor lo pida.

Evidencia de la versión final en `salida/r6/`:

- `verificar_espacio.txt`: APROBADO.
  - E1 enumera 10.401.300 combinaciones sin abortos, también con conjuntos vacíos.
  - El canal de repetición vale 1/3 exacto.
  - E4c mide las reglas que ven la tabla contra sus firmas.
  - 25/25 mutantes cazados por su sonda.
- `arsenal_hook.txt`: 0 errores.
  - 2G 100/100 y 2H PASA.
  - La advertencia 2N es un falso positivo del hook (ver abajo).
- Renders de los 4 formatos más Moodle; vistas previas y captura a 360 px.
- `paso9_*.txt`: diversidad PASS; diagnosticidad PASS; 271/300 versiones únicas.
- `paso10_cotejo.txt`: metadatos iguales a la ficha.
- `detractor_fase2c_r6.md`: APROBAR_CON_CAMBIOS, sin CORRECCIÓN. Paso 8: CUMPLE_CON_OBSERVACIONES.

## Historia del 2026-10-27: seis detractores, seis rondas (`salida/r1` … `r6`)

| Ronda | md5 | Detractor | Qué cambió después |
|---|---|---|---|
| r1 | `5dae44c0` | RECHAZAR | Solution canónica falsa sobre C y D (sumas en celdas sueltas) |
| r2 | `52bb35e4` | APROBAR_CON_CAMBIOS | Decisiones del profesor: 96,8 %, C → PROB-CER, residuo canónico por alcance, ancho 100 %, `evaluacion_afirmacion` |
| r3 | `632e09ea` | APROBAR_CON_CAMBIOS | «En cada extracción» junto a cada n/N; guardas léxicas y estructurales |
| r4 | `bc4b370b` | APROBAR_CON_CAMBIOS (ALTA) | Canal de estímulo «descarta la que repite» al 66 % → **PROB-CRU solo en la canónica**; quedó en 1/3 (firmado) |
| r5 | `afc260eb` | APROBAR_CON_CAMBIOS (2 ALTA) | Segundo desplazamiento → regla de parada; **pasada 2 numérica** (clave en 3..24) y firma de la compuesta (1/2) |
| r6 | `4fa520bb` | APROBAR_CON_CAMBIOS (ALTA) | Cadenas firma + «celdas más juntas» → **firmadas por alcance** (cierre de §P7-D con residuo; 2 de 3 pasadas usadas) |

## Residuos firmados (diagnosticidad; ninguno afecta la corrección)

Tasas en las no canónicas; el ítem oficial va entre paréntesis.

| Regla | Tasa | Ítem oficial |
|---|---|---|
| «Descarta la pareja que repite una celda» | 1/3 (+8,3 pp) | 1/3 |
| Compuesta «repite + su gemela de molde» | 1/2 (+25 pp) | 0 % |
| «Celdas más juntas», sola | 32,6 % (+7,6 pp) | 0 % |
| Cadena «repite» + «celdas más juntas» | 47,6 % | 0 % |
| Cadena compuesta + «celdas más juntas» | 64,8 % | 0 % |
| Residuo canónico (batería de opciones) | firmado por alcance con P_CANON ≤ 0,10 | — |

Todos exigen aprender comparando versiones que la pareja que repite nunca es la clave. **La evidencia
de aula dirá si alguien los explota.**

## Pendientes declarados (no bloquean)

- **E4c con tasas exactas** (H2 del detractor r6): ponderar la enumeración de E1 con las probabilidades
  del generador. Hoy E4c compara la muestra N = 100 por estrato contra la firma con 3 errores típicos.
- **Numeración «0.0.x» de los encabezados de la Solution en el PDF** (H4 r6): es la convención del repo
  (6 de 10 ejemplos funcionales). `### … {-}` la quitaría, pero hay que comprobarlo en el PDF y supone
  otra ronda.
- **Defecto del hook, fuera del subproyecto:** la FASE 2N de `post-exams2-validation.sh` da falsos
  «sin aleatorización». El hook usa `set -o pipefail` y `echo "$DG_BLOCK" | grep -q` recibe SIGPIPE.
  Arreglo: `grep -qE … <<< "$DG_BLOCK"`. El hook está protegido: se cambia solo con permiso.

## El hook post-render

`post-exams2-validation.sh` no muestra su salida en la sesión. Invocación manual (timeout de Bash
600000 ms):

```bash
RMD=<ruta absoluta del .Rmd>; D=$(dirname "$RMD")
python3 - "$RMD" "$D" <<'PY' | bash .claude/hooks/post-exams2-validation.sh
import json, sys
rmd, d = sys.argv[1], sys.argv[2]
print(json.dumps({"tool_input": {"command": f"Rscript -e 'exams::exams2html(\"{rmd}\")'"}, "cwd": d}))
PY
```

Hay tres cosas que tener presentes:

- El hook sella por su cuenta los pasos 5 y 6 sin nota: hay que añadirla después con `complete`.
- No hay que poner comillas invertidas dentro del chunk, ni siquiera en comentarios: la 2D da un falso
  positivo y se salta 2G y 2H.
- Para revisar los nombres de figura en el XML de Moodle hay que usar `[A-Za-z0-9_.-]+\.png`. Con
  `[a-z_]+` se corta un sufijo hexadecimal que termine en letras.

## Pendiente después de la aprobación

- Evidencia de aula antes de promover: 11/11 significa «aprobado para el aula», no producción.
- `git add` solo cuando el profesor lo pida (incluye `verificar_espacio.R` y `salida/r*/` si se quieren
  versionar). `Sesion-02.pdf` no se versiona (`.gitignore:469`); los recortes PNG sí, por LFS.
