#!/usr/bin/env bash
# paso-pipeline.sh — envuelve un paso del pipeline para que ENTREGUE POR ARCHIVO.
#
# Por qué existe: la norma del canal («todo hijo entrega por archivo») no sirve si depende de que
# cada agente se acuerde de cumplirla. Este envoltorio la vuelve MECÁNICA: corre el comando, mide
# el resultado real (exit code, duración, salida) y sella el reporte en el buzón. Si el paso falla,
# el reporte queda sellado igual — un paso que revienta sin dejar rastro es justo lo que hay que
# evitar.
#
# Motivo medido (2026-09-11): el despertar del carril del padre al ceder turno falla
# (`embedded tool authority is no longer active`, 4/4 intentos agotados). El canal de mensajes no
# es fiable; el archivo en disco sí.
#
# Uso:
#   paso-pipeline.sh <runId> <nombre-paso> -- <comando> [args...]
#
# Ej.:
#   REPORTES_DIR=/tmp/reportes-agente tools/paso-pipeline.sh \
#     fase1-validar-glifos "Validar glifos LaTeX" -- \
#     Rscript .claude/scripts/validar_glifos_latex.R <ruta.Rmd>
#
# Variables:
#   REPORTES_DIR     buzón (por defecto /tmp/reportes-agente)
#   PASO_LOGS_DIR    dónde guardar la salida cruda (por defecto el buzón, subdir logs/)
#   PASO_ARTEFACTOS  rutas separadas por ':' que el paso debe producir (se comprueban)

set -uo pipefail

AQUI="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CANAL="$AQUI/reporte-agente.sh"
[ -x "$CANAL" ] || { echo "FALLA: no encuentro el canal en $CANAL" >&2; exit 2; }

RUNID="${1:-}"; NOMBRE="${2:-}"; shift 2 2>/dev/null || true
[ "${1:-}" = "--" ] && shift
CMD=("$@")

if [ -z "$RUNID" ] || [ -z "$NOMBRE" ] || [ ${#CMD[@]} -eq 0 ]; then
  sed -n '12,26p' "$0" | sed 's/^# \{0,1\}//'; exit 2
fi

BUZON="${REPORTES_DIR:-/tmp/reportes-agente}"
LOGS="${PASO_LOGS_DIR:-$BUZON/logs}"
mkdir -p "$LOGS"
LOG="$LOGS/$RUNID.log"

"$CANAL" init "$RUNID" >/dev/null

echo "── paso: $NOMBRE"
echo "   comando: ${CMD[*]}"

T0=$(date +%s)
"${CMD[@]}" > "$LOG" 2>&1
EXIT=$?
T1=$(date +%s)
DUR=$((T1-T0))

ESTADO="ok"; [ "$EXIT" -ne 0 ] && ESTADO="fallo"

# Artefactos declarados: si el paso dice que produce algo y no está, es fallo aunque el
# comando haya salido con 0. Un verde que no dejó el entregable es un falso verde.
ART_LIST="$LOG"
FALTAN=""
if [ -n "${PASO_ARTEFACTOS:-}" ]; then
  IFS=':' read -r -a _arts <<< "$PASO_ARTEFACTOS"
  for a in "${_arts[@]}"; do
    [ -z "$a" ] && continue
    ART_LIST="$ART_LIST
$a"
    [ -e "$a" ] || FALTAN="$FALTAN $a"
  done
  if [ -n "$FALTAN" ] && [ "$ESTADO" = "ok" ]; then
    ESTADO="fallo"
    echo "   AVISO: exit 0 pero falta el entregable:$FALTAN" >&2
  fi
fi

TAIL="$(tail -c 1200 "$LOG")"
RESUMEN="$NOMBRE: exit=$EXIT en ${DUR}s"
[ -n "$FALTAN" ] && RESUMEN="$RESUMEN (entregable ausente:$FALTAN)"

# El JSON se construye con python, NO con plantilla de shell: un comando con comillas
# rompía el JSON, el reporte no se sellaba y el envoltorio aun así decía «ok».
export _P_ESTADO="$ESTADO" _P_RESUMEN="$RESUMEN" _P_EXIT="$EXIT" _P_DUR="$DUR" \
       _P_CMD="${CMD[*]}" _P_LOG="$LOG" _P_TAIL="$TAIL" _P_ART="$ART_LIST"
python3 - <<'PY' | "$CANAL" done "$RUNID"
import json, os
d = {
    "estado": os.environ["_P_ESTADO"],
    "resumen": os.environ["_P_RESUMEN"],
    "evidencia": {
        "exit_code": int(os.environ["_P_EXIT"]),
        "duracion_s": int(os.environ["_P_DUR"]),
        "comando": os.environ["_P_CMD"],
        "log": os.environ["_P_LOG"],
        "salida_tail": os.environ["_P_TAIL"],
    },
    "artefactos": [a for a in os.environ["_P_ART"].split("\n") if a],
}
print(json.dumps(d, ensure_ascii=False))
PY
SELLO=$?

# Si el reporte no se selló, el paso NO puede declararse bueno: el entregable es el reporte.
if [ "$SELLO" -ne 0 ]; then
  echo "   FALLA: el reporte no se pudo sellar (exit $SELLO). El paso queda SIN entregable." >&2
  exit 3
fi

echo "   veredicto: $ESTADO (exit=$EXIT, ${DUR}s)"
echo "   reporte  : $BUZON/$RUNID.json"
[ "$ESTADO" = "ok" ] || exit 1
