#!/usr/bin/env bash
# =============================================================================
# detractor-hetero.sh — Segundo detractor HETEROGÉNEO (familia de modelo distinta
# de Anthropic) para un .Rmd o un directorio de ejercicio. FASE 2C-bis.
#
#   bash .claude/scripts/detractor-hetero.sh <ruta.Rmd|directorio> [opciones]
#
# Opciones:
#   --motor auto|deepseek|ollama-cloud|codex|local   (defecto: auto = cascada)
#   --modelo <id>        id de modelo del motor elegido (defecto por motor, ver abajo)
#   --salida <archivo>   reporte (defecto: <dir del objetivo>/detractor-hetero-<motor>-<fecha>.md)
#   --max-turnos <N>     turnos de herramientas del agente (defecto: 40)
#   --sin-codex          en modo auto, no intentar Codex
#
# Cascada en modo auto (se detiene en el primer motor que ENTREGA, es decir, cuya
# última línea es el marcador VEREDICTO_DETRACTOR:):
#   1. deepseek      api.deepseek.com/anthropic  — clave de ~/.credenciales [deepseek]
#                    o $DEEPSEEK_API_KEY. Modelo: $DETRACTOR2_DEEPSEEK_MODEL o deepseek-v4-flash
#   2. ollama-cloud  proxy local Ollama (localhost:11434) hacia ollama.com, cuenta `ollama signin`.
#                    Modelo: $DETRACTOR2_OLLAMA_MODEL o glm-5.3:cloud
#   3. codex         Codex CLI (suscripción ChatGPT). Modelo: $DETRACTOR2_CODEX_MODEL o gpt-5.6-terra
#   4. local         qwen3:14b en Ollama local, UNA sola pasada sin herramientas (lento: CPU)
#
# El prompt de sistema es el cuerpo de .claude/agents/agente-detractor.md (sin frontmatter),
# así que los 8 dominios, el formato de objeción y el contrato de entrega son los mismos que
# en FASE 2C. El motor NUNCA escribe: solo Read/Glob/Grep/Rscript de solo lectura.
#
# Códigos de salida: 0 reporte entregado con marcador · 2 objetivo inválido · 3 ningún
# motor entregó (todos saturados/sin saldo o sin marcador). Las claves nunca se imprimen.
# Regla asociada: .claude/rules/detractor-obligatorio.md § "Segundo detractor heterogéneo".
# =============================================================================
set -uo pipefail

AQUI="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "$AQUI/../.." && pwd)"
AGENTE="$REPO/.claude/agents/agente-detractor.md"
MARCADOR='^VEREDICTO_DETRACTOR: (APROBAR|APROBAR_CON_CAMBIOS|RECHAZAR)[[:space:]]*$'
SATURADO='usage limit|rate.?limit|\b429\b|\b402\b|insufficient|balance|quota|credit|billing|overloaded|subscription or extra usage|limit reached|not_found_error|model.*not found'

MOTOR="auto"; MODELO=""; SALIDA=""; MAX_TURNOS=40; SIN_CODEX=0; OBJETIVO=""
while [ $# -gt 0 ]; do
  case "$1" in
    --motor) MOTOR="$2"; shift 2 ;;
    --modelo) MODELO="$2"; shift 2 ;;
    --salida) SALIDA="$2"; shift 2 ;;
    --max-turnos) MAX_TURNOS="$2"; shift 2 ;;
    --sin-codex) SIN_CODEX=1; shift ;;
    -h|--help) sed -n '2,32p' "$0"; exit 0 ;;
    *) if [ -z "$OBJETIVO" ]; then OBJETIVO="$1"; else echo "argumento inesperado: $1" >&2; exit 2; fi; shift ;;
  esac
done

[ -n "$OBJETIVO" ] || { echo "uso: $0 <ruta.Rmd|directorio> [--motor ...]" >&2; exit 2; }
[ -e "$OBJETIVO" ] || { echo "no existe: $OBJETIVO" >&2; exit 2; }
[ -f "$AGENTE" ] || { echo "falta $AGENTE" >&2; exit 2; }
OBJETIVO="$(cd "$(dirname "$OBJETIVO")" && pwd)/$(basename "$OBJETIVO")"
if [ -d "$OBJETIVO" ]; then DIR_OBJ="$OBJETIVO"; else DIR_OBJ="$(dirname "$OBJETIVO")"; fi
FECHA="$(date +%Y%m%d-%H%M%S)"

# Prompt de sistema = cuerpo del agente sin frontmatter YAML.
SISTEMA="$(awk 'BEGIN{fm=0} NR==1 && /^---$/ {fm=1; next} fm==1 && /^---$/ {fm=2; next} fm!=1 {print}' "$AGENTE")"
SISTEMA="$SISTEMA

## Instrucciones de esta invocación (detractor heterogéneo, FASE 2C-bis)
- Eres el SEGUNDO detractor, de una familia de modelos distinta a la del primero. Tu valor está en
  encontrar lo que el primero no vio: razona sobre el SIGNIFICADO de enunciado, opciones y Solution
  (unicidad de la clave, distractores que resultan correctos en alguna rama, afirmaciones falsas).
- Trabajas en modo Auditoría sobre el objetivo indicado. Lee el .Rmd completo. Puedes ejecutar
  Rscript de solo lectura (por ejemplo .claude/scripts/validar_coherencia_matematica.R) pero NO
  puedes escribir ni editar nada.
- Clasifica cada hallazgo como CORRECCIÓN (binario, bloqueante) o DIAGNOSTICIDAD (gradual, sólo
  obliga por encima de +8 pp frente a la vara oficial) según la regla detractor-obligatorio.md.
- Escribe el reporte en español, con el formato «Revisión Detractor» de la regla. Tu texto final
  ES el reporte y su ÚLTIMA línea debe ser exactamente una de:
  VEREDICTO_DETRACTOR: APROBAR
  VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
  VEREDICTO_DETRACTOR: RECHAZAR"

PROMPT="Auditoría detractor (FASE 2C-bis, heterogénea) sobre: $OBJETIVO
Repositorio: $REPO
Aplica los 8 dominios de .claude/rules/detractor-obligatorio.md. Emite el reporte completo y cierra con el marcador VEREDICTO_DETRACTOR:."

leer_clave_deepseek() {
  if [ -n "${DEEPSEEK_API_KEY:-}" ]; then printf '%s' "$DEEPSEEK_API_KEY"; return; fi
  awk '/^\[deepseek\]/{f=1;next} /^\[/{f=0} f&&/^api_key/{sub(/^api_key[ ]*=[ ]*/,""); gsub(/[ \r\n]/,""); print; exit}' "$HOME/.credenciales" 2>/dev/null
}

# claude -p contra un endpoint compatible con la API Anthropic Messages.
# $1 base_url  $2 token  $3 modelo  $4 archivo de salida
correr_claude() {
  local base="$1" token="$2" modelo="$3" out="$4"
  ( cd "$REPO" && env \
      ANTHROPIC_BASE_URL="$base" ANTHROPIC_AUTH_TOKEN="$token" ANTHROPIC_API_KEY="" \
      ANTHROPIC_DEFAULT_HAIKU_MODEL="$modelo" ANTHROPIC_SMALL_FAST_MODEL="$modelo" \
      ANTHROPIC_DEFAULT_SONNET_MODEL="$modelo" ANTHROPIC_DEFAULT_OPUS_MODEL="$modelo" \
      CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC=1 \
      CLAUDE_CODE_MAX_OUTPUT_TOKENS="${DETRACTOR2_MAX_OUTPUT_TOKENS:-16000}" \
    claude -p "$PROMPT" \
      --model "$modelo" \
      --append-system-prompt "$SISTEMA" \
      --allowedTools "Read" "Glob" "Grep" "Bash(Rscript *)" "Bash(ls *)" "Bash(wc *)" "Bash(head *)" "WebFetch" \
      --disallowedTools "Write" "Edit" "NotebookEdit" "Agent" "Task" \
      --permission-mode dontAsk \
      --max-turns "$MAX_TURNOS" \
      --no-session-persistence \
      --settings "{\"env\":{\"CLAUDE_CODE_MAX_OUTPUT_TOKENS\":\"${DETRACTOR2_MAX_OUTPUT_TOKENS:-16000}\"}}" \
      --output-format text ) > "$out" 2>&1
}

motor_deepseek() {
  local out="$1" clave modelo
  clave="$(leer_clave_deepseek)"
  [ -n "$clave" ] || { echo "deepseek: sin clave (~/.credenciales [deepseek] o DEEPSEEK_API_KEY)" >&2; return 1; }
  modelo="${MODELO:-${DETRACTOR2_DEEPSEEK_MODEL:-deepseek-v4-flash}}"
  MODELO_USADO="$modelo"
  correr_claude "https://api.deepseek.com/anthropic" "$clave" "$modelo" "$out"
}

motor_ollama_cloud() {
  local out="$1" modelo
  modelo="${MODELO:-${DETRACTOR2_OLLAMA_MODEL:-glm-5.3:cloud}}"
  MODELO_USADO="$modelo"
  curl -s -m 5 http://localhost:11434/api/version >/dev/null 2>&1 || { echo "ollama-cloud: Ollama local no responde en :11434" >&2; return 1; }
  ollama show "$modelo" >/dev/null 2>&1 || ollama pull "$modelo" >/dev/null 2>&1 || { echo "ollama-cloud: no se pudo obtener el manifiesto $modelo" >&2; return 1; }
  correr_claude "http://localhost:11434" "ollama" "$modelo" "$out"
}

motor_codex() {
  local out="$1" modelo
  modelo="${MODELO:-${DETRACTOR2_CODEX_MODEL:-gpt-5.6-terra}}"
  MODELO_USADO="$modelo"
  command -v codex >/dev/null 2>&1 || { echo "codex: CLI no instalado" >&2; return 1; }
  ( cd "$REPO" && codex exec --skip-git-repo-check --sandbox read-only -m "$modelo" \
      --output-last-message "$out.last" "$SISTEMA

$PROMPT" ) > "$out.log" 2>&1
  if [ -s "$out.last" ]; then cat "$out.last" > "$out"; else cat "$out.log" > "$out"; fi
  rm -f "$out.last"
}

motor_local() {
  local out="$1" modelo contenido cuerpo
  modelo="${MODELO:-${DETRACTOR2_LOCAL_MODEL:-qwen3:14b}}"
  MODELO_USADO="$modelo"
  curl -s -m 5 http://localhost:11434/api/version >/dev/null 2>&1 || { echo "local: Ollama local no responde en :11434" >&2; return 1; }
  if [ -d "$OBJETIVO" ]; then contenido="$(find "$OBJETIVO" -maxdepth 2 -name '*.Rmd' -exec sh -c 'echo "===== $1 ====="; cat "$1"' _ {} \;)"
  else contenido="$(cat "$OBJETIVO")"; fi
  cuerpo="$(python3 - "$SISTEMA" "$PROMPT

Contenido del objetivo (no tienes herramientas: audita SOLO con este texto):

$contenido" "$modelo" <<'EOF'
import json, sys
print(json.dumps({"model": sys.argv[3], "max_tokens": 12000, "system": sys.argv[1],
                  "messages": [{"role": "user", "content": sys.argv[2]}]}))
EOF
)"
  curl -s -m 3600 http://localhost:11434/v1/messages -H 'content-type: application/json' \
       -H 'x-api-key: ollama' -H 'anthropic-version: 2023-06-01' -d "$cuerpo" \
    | python3 -c 'import sys,json; d=json.load(sys.stdin); print("".join(b.get("text","") for b in d.get("content",[]) if b.get("type")=="text") or json.dumps(d))' > "$out" 2>&1
}

entregado() { tail -n 3 "$1" | grep -Eq "$MARCADOR"; }
saturado()  { grep -Eiq "$SATURADO" "$1"; }

ejecutar_motor() {
  local motor="$1" out="$2"
  case "$motor" in
    deepseek)     motor_deepseek "$out" ;;
    ollama-cloud) motor_ollama_cloud "$out" ;;
    codex)        motor_codex "$out" ;;
    local)        motor_local "$out" ;;
    *) echo "motor desconocido: $motor" >&2; return 1 ;;
  esac
}

if [ "$MOTOR" = "auto" ]; then
  CASCADA="deepseek ollama-cloud codex local"
  [ "$SIN_CODEX" = 1 ] && CASCADA="deepseek ollama-cloud local"
else
  CASCADA="$MOTOR"
fi

TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
INTENTOS=""
for motor in $CASCADA; do
  out="$TMP/$motor.txt"; MODELO_USADO="?"
  inicio=$(date +%s)
  echo ">> motor $motor …" >&2
  if ejecutar_motor "$motor" "$out"; then :; fi
  dur=$(( $(date +%s) - inicio ))
  if [ -s "$out" ] && entregado "$out"; then
    [ -n "$SALIDA" ] || SALIDA="$DIR_OBJ/detractor-hetero-$motor-$FECHA.md"
    {
      echo "# Revisión Detractor heterogénea (FASE 2C-bis)"
      echo
      echo "- **Objetivo**: $OBJETIVO"
      echo "- **Motor / modelo**: $motor / $MODELO_USADO"
      echo "- **Fecha**: $(date '+%Y-%m-%d %H:%M:%S') · **Duración**: ${dur}s · **Intentos previos**: ${INTENTOS:-ninguno}"
      echo "- **Familia distinta de Anthropic**: sí — complemento del AgenteDetractor de FASE 2C, no lo sustituye"
      echo
      echo "---"
      echo
      cat "$out"
    } > "$SALIDA"
    echo "REPORTE: $SALIDA" >&2
    echo "$SALIDA"
    exit 0
  fi
  motivo="sin marcador"
  [ -s "$out" ] && saturado "$out" && motivo="saturado/sin saldo"
  [ -s "$out" ] || motivo="sin salida"
  echo "   $motor ($MODELO_USADO): $motivo tras ${dur}s" >&2
  INTENTOS="${INTENTOS:+$INTENTOS, }$motor=$motivo"
  # Conservar la salida fallida para diagnóstico (nunca contiene claves).
  cp "$out" "$DIR_OBJ/.detractor-hetero-$motor-$FECHA.fallido.log" 2>/dev/null || true
done

echo "NINGÚN motor entregó reporte con marcador: $INTENTOS" >&2
exit 3
