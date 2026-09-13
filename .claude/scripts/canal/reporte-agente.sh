#!/usr/bin/env bash
# reporte-agente.sh — canal de reportes por ARCHIVO entre subagente y padre.
#
# Motivo (medido, 2026-09-11): el despertar del carril del padre al ceder turno
# falla ("embedded tool authority is no longer active", 4/4 intentos agotados),
# asi que los reportes que dependen del canal de mensajes se pierden.
# El archivo en disco SI sobrevive: se verifico con la sonda d47b1fbd.
#
# Contrato:
#   init  <runId>            crea el buzon
#   done  <runId>            lee JSON por stdin, valida, escribe atomico
#   read  <runId>            imprime el reporte (exit 2 si falta)
#   list                     lista reportes terminados
#   pending                  lista runIds iniciados sin reporte
#
# Buzon por defecto: $REPORTES_DIR o /tmp/reportes-agente
# Salidas: 0 ok | 2 no encontrado | 3 JSON invalido | 4 faltan claves

set -uo pipefail

DIR="${REPORTES_DIR:-/tmp/reportes-agente}"
CMD="${1:-}"; RUNID="${2:-}"

_uso() { sed -n '2,20p' "$0" | sed 's/^# \{0,1\}//'; exit 1; }
[ -z "$CMD" ] && _uso

_path() { echo "$DIR/$1.json"; }

case "$CMD" in
  init)
    [ -z "$RUNID" ] && _uso
    mkdir -p "$DIR"
    printf '{"runId":"%s","iniciadoEn":"%s"}\n' "$RUNID" "$(date -Is)" > "$DIR/$RUNID.meta"
    echo "buzon listo: $(realpath "$DIR")"
    ;;

  done)
    [ -z "$RUNID" ] && _uso
    mkdir -p "$DIR"
    JSON="$(cat)"
    # Validar JSON y claves obligatorias
    printf '%s' "$JSON" | python3 -c '
import json,sys
try:
    d=json.load(sys.stdin)
except Exception as e:
    sys.stderr.write("JSON invalido: %s\n"%e); sys.exit(3)
req=["estado","resumen"]
falta=[k for k in req if k not in d]
if falta:
    sys.stderr.write("faltan claves obligatorias: %s\n"%",".join(falta)); sys.exit(4)
if d["estado"] not in ("ok","fallo","bloqueado"):
    sys.stderr.write("estado invalido: %s\n"%d["estado"]); sys.exit(4)
' || exit $?
    # Escritura atomica: tmp + mv (nunca un reporte a medias)
    TMP="$DIR/.$RUNID.tmp.$$"
    printf '%s' "$JSON" | python3 -c '
import json,sys,datetime
d=json.load(sys.stdin)
d["_recibidoEn"]=datetime.datetime.now().astimezone().isoformat()
d["_runId"]=sys.argv[1]
json.dump(d,sys.stdout,ensure_ascii=False,indent=2)
' "$RUNID" > "$TMP" || { rm -f "$TMP"; exit 3; }
    mv -f "$TMP" "$( _path "$RUNID" )"
    echo "reporte sellado: $( _path "$RUNID" )"
    ;;

  read)
    [ -z "$RUNID" ] && _uso
    P="$( _path "$RUNID" )"
    [ -f "$P" ] || { echo "SIN REPORTE: $P" >&2; exit 2; }
    cat "$P"
    ;;

  list)
    mkdir -p "$DIR"
    for f in "$DIR"/*.json; do [ -e "$f" ] || continue; basename "$f" .json; done
    ;;

  pending)
    mkdir -p "$DIR"
    for m in "$DIR"/*.meta; do
      [ -e "$m" ] || continue
      id="$(basename "$m" .meta)"
      [ -f "$DIR/$id.json" ] || echo "$id"
    done
    ;;

  *) _uso ;;
esac
