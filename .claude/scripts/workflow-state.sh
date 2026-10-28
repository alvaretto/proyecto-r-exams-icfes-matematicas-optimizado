#!/usr/bin/env bash
# workflow-state.sh — CLI para gestión de estado del workflow de ejercicios ICFES
# Versión: 2.0 — Fecha: 2026-03-21
# Usa python3 para manipulación JSON (jq no disponible en el entorno)

set -uo pipefail

# ---------------------------------------------------------------------------
# Constantes
# ---------------------------------------------------------------------------
STATE_FILE="ejercicio_state.json"
SCHEMA_VERSION="2.0"

PASOS_ORDEN=(
    analisis_icfes
    flujo_b
    generacion_rmd
    retroalimentacion
    renderizado_4_formatos
    arsenal_post_render
    detractor_fase2c
    coherencias_5
    validar_diversidad
    validar_icfes
    aprobacion_usuario
)

declare -A PASO_COMANDO=(
    [analisis_icfes]="/analizar-icfes"
    [flujo_b]="Determinar si requiere gráficos"
    [generacion_rmd]="/generar-schoice o /generar-cloze"
    [retroalimentacion]="/skill-retroalimentacion"
    [renderizado_4_formatos]="exams2html/pdf/docx/nops"
    [arsenal_post_render]="(hook post-render; sellar a mano con --nota)"
    [detractor_fase2c]="/adversario archivo.Rmd"
    [coherencias_5]="Documentar 5 coherencias"
    [validar_diversidad]="/validar-diversidad"
    [validar_icfes]="/validar-icfes"
    [aprobacion_usuario]="Pedir aprobación al usuario"
)

# ---------------------------------------------------------------------------
# Utilidades
# ---------------------------------------------------------------------------
die() {
    echo "ERROR: $*" >&2
    exit 1
}

now_iso() {
    python3 -c "import datetime; print(datetime.datetime.now().astimezone().isoformat())"
}

resolve_dir() {
    local dir="$1"
    if [[ ! -d "$dir" ]]; then
        die "El directorio no existe: $dir"
    fi
    cd "$dir" && pwd
}

state_path() {
    local dir
    dir=$(resolve_dir "$1")
    echo "${dir}/${STATE_FILE}"
}

require_state() {
    local sf="$1"
    if [[ ! -f "$sf" ]]; then
        die "No se encontró $STATE_FILE en el directorio. Ejecute: workflow-state.sh init <dir>"
    fi
}

# ---------------------------------------------------------------------------
# Comando: init
# ---------------------------------------------------------------------------
cmd_init() {
    local dir=""
    local tipo=""
    local nombre=""

    # Parsear argumentos
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --tipo)
                shift
                tipo="${1:-}"
                ;;
            --nombre)
                shift
                nombre="${1:-}"
                ;;
            *)
                if [[ -z "$dir" ]]; then
                    dir="$1"
                fi
                ;;
        esac
        shift
    done

    [[ -z "$dir" ]] && die "Uso: init <dir> --tipo schoice|cloze [--nombre N]"
    [[ -z "$tipo" ]] && die "Argumento --tipo requerido: schoice o cloze"
    [[ "$tipo" != "schoice" && "$tipo" != "cloze" ]] && die "--tipo debe ser 'schoice' o 'cloze'"

    local abs_dir
    abs_dir=$(resolve_dir "$dir")
    local sf="${abs_dir}/${STATE_FILE}"

    if [[ -f "$sf" ]]; then
        echo "ADVERTENCIA: Ya existe $sf" >&2
        echo "¿Sobreescribir? (s/N): " >&2
        read -r resp
        [[ "$resp" != "s" && "$resp" != "S" ]] && { echo "Cancelado."; exit 0; }
    fi

    # Nombre por defecto: nombre del directorio
    if [[ -z "$nombre" ]]; then
        nombre=$(basename "$abs_dir")
    fi

    local ts
    ts=$(now_iso)

    python3 - "$sf" "$nombre" "$tipo" "$ts" "$SCHEMA_VERSION" <<'PYEOF'
import json, sys

sf     = sys.argv[1]
nombre = sys.argv[2]
tipo   = sys.argv[3]
ts     = sys.argv[4]
ver    = sys.argv[5]

state = {
    "version": ver,
    "ejercicio": nombre,
    "tipo": tipo,
    "timestamp_inicio": ts,
    "timestamp_ultima_actualizacion": ts,
    "pasos": {
        "analisis_icfes": {
            "completado": False,
            "timestamp": None,
            "resultado": None
        },
        "flujo_b": {
            "requerido": None,
            "completado": False,
            "timestamp": None
        },
        "generacion_rmd": {
            "completado": False,
            "timestamp": None,
            "archivo": None
        },
        "retroalimentacion": {
            "completado": False,
            "timestamp": None
        },
        "renderizado_4_formatos": {
            "completado": False,
            "timestamp": None
        },
        "arsenal_post_render": {
            "completado": False,
            "timestamp": None
        },
        "detractor_fase2c": {
            "completado": False,
            "timestamp": None,
            "veredicto": None
        },
        "coherencias_5": {
            "completado": False,
            "timestamp": None
        },
        "validar_diversidad": {
            "completado": False,
            "timestamp": None,
            "versiones_unicas": None
        },
        "validar_icfes": {
            "completado": False,
            "timestamp": None
        },
        "aprobacion_usuario": {
            "completado": False,
            "timestamp": None
        }
    }
}

with open(sf, 'w', encoding='utf-8') as fh:
    json.dump(state, fh, indent=2, ensure_ascii=False)
    fh.write('\n')

print(f"Estado creado: {sf}")
PYEOF
}

# ---------------------------------------------------------------------------
# Comando: complete
# ---------------------------------------------------------------------------
cmd_complete() {
    local dir=""
    local paso=""
    local -a extras=()

    # Parsear posicionales y --key value
    local positional=0
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --*)
                local key="${1#--}"
                shift
                local val="${1:-}"
                extras+=("$key" "$val")
                ;;
            *)
                if [[ $positional -eq 0 ]]; then
                    dir="$1"
                    positional=1
                elif [[ $positional -eq 1 ]]; then
                    paso="$1"
                    positional=2
                fi
                ;;
        esac
        shift
    done

    [[ -z "$dir" || -z "$paso" ]] && die "Uso: complete <dir> <paso> [--key value ...]"

    local sf
    sf=$(state_path "$dir")
    require_state "$sf"

    local ts
    ts=$(now_iso)

    # Pasar extras como JSON array de pares
    local extras_json
    extras_json=$(python3 -c "
import json, sys
args = sys.argv[1:]
pairs = []
i = 0
while i < len(args):
    pairs.append([args[i], args[i+1]])
    i += 2
print(json.dumps(pairs))
" "${extras[@]+"${extras[@]}"}")

    python3 - "$sf" "$paso" "$ts" "$extras_json" <<'PYEOF'
import json, sys

sf         = sys.argv[1]
paso       = sys.argv[2]
ts         = sys.argv[3]
extras_raw = sys.argv[4]

extras = json.loads(extras_raw)  # lista de pares [key, val_str]
LENGUAJES = ("tikz", "python", "r")

with open(sf, 'r', encoding='utf-8') as fh:
    state = json.load(fh)

if paso not in state["pasos"]:
    print(f"ERROR: Paso desconocido '{paso}'", file=sys.stderr)
    print(f"Pasos válidos: {', '.join(state['pasos'].keys())}", file=sys.stderr)
    sys.exit(1)

# Parsear extras: intentar como JSON, sino guardar como string
parsed = []
for key, val_str in extras:
    try:
        val = json.loads(val_str)
    except (json.JSONDecodeError, ValueError):
        val = val_str
    parsed.append((key, val))

info = state["pasos"][paso]

def guardar():
    with open(sf, 'w', encoding='utf-8') as fh:
        json.dump(state, fh, indent=2, ensure_ascii=False)
        fh.write('\n')

# flujo_b: registrar la decisión (WAIT_USER #1) NO es terminar el Flujo B.
# Con requerido=true sólo se sella cuando el usuario eligió lenguaje
# (WAIT_USER #2, --lenguaje tikz|python|r); antes, el gate del .Rmd debe
# seguir cerrado. Sellar en la decisión abría el gate sin gráfico elegido.
if paso == "flujo_b":
    nuevos = dict(parsed)
    req = nuevos.get("requerido", info.get("requerido"))
    lenguaje = nuevos.get("lenguaje", info.get("lenguaje"))
    if "lenguaje" in nuevos and lenguaje not in LENGUAJES:
        print(f"ERROR: --lenguaje debe ser uno de: {', '.join(LENGUAJES)} "
              f"(recibido: {lenguaje!r})", file=sys.stderr)
        sys.exit(2)
    if req is None:
        print("ERROR: flujo_b sin decidir. Preguntar al usuario y registrar "
              "--requerido true|false.", file=sys.stderr)
        sys.exit(3)
    if req is True and lenguaje not in LENGUAJES:
        for key, val in parsed:
            info[key] = val
        info["completado"] = False
        state["timestamp_ultima_actualizacion"] = ts
        guardar()
        print("⏸  Decisión registrada: flujo_b.requerido = true. El paso sigue "
              "PENDIENTE (el gate del .Rmd sigue cerrado).")
        print("   Tras generar TikZ→Python→R y la elección del usuario "
              "(WAIT_USER #2), sellar con:")
        print("   workflow-state.sh complete <dir> flujo_b --lenguaje tikz|python|r")
        sys.exit(0)

info["completado"] = True
info["timestamp"]  = ts
state["timestamp_ultima_actualizacion"] = ts
for key, val in parsed:
    info[key] = val

guardar()

print(f"✅ Paso '{paso}' marcado como completado.")
PYEOF
}

# ---------------------------------------------------------------------------
# Comando: check
# ---------------------------------------------------------------------------
cmd_check() {
    local dir="${1:-}"
    local paso="${2:-}"
    [[ -z "$dir" || -z "$paso" ]] && die "Uso: check <dir> <paso>"

    local sf
    sf=$(state_path "$dir")
    require_state "$sf"

    python3 - "$sf" "$paso" <<'PYEOF'
import json, sys

sf   = sys.argv[1]
paso = sys.argv[2]

with open(sf, 'r', encoding='utf-8') as fh:
    state = json.load(fh)

if paso not in state["pasos"]:
    print(f"ERROR: Paso desconocido '{paso}'", file=sys.stderr)
    sys.exit(2)

completado = state["pasos"][paso].get("completado", False)
sys.exit(0 if completado else 1)
PYEOF
}

# ---------------------------------------------------------------------------
# Comando: status
# ---------------------------------------------------------------------------
cmd_status() {
    local dir="${1:-}"
    [[ -z "$dir" ]] && die "Uso: status <dir>"

    local sf
    sf=$(state_path "$dir")
    require_state "$sf"

    python3 - "$sf" <<'PYEOF'
import json, sys

sf = sys.argv[1]

with open(sf, 'r', encoding='utf-8') as fh:
    state = json.load(fh)

print(f"")
print(f"  Ejercicio : {state.get('ejercicio', '?')}")
print(f"  Tipo      : {state.get('tipo', '?')}")
print(f"  Inicio    : {state.get('timestamp_inicio', '?')[:19]}")
print(f"  Actualiz. : {state.get('timestamp_ultima_actualizacion', '?')[:19]}")
print(f"")
print(f"  {'#':<3} {'Paso':<30} {'Estado'}")
print(f"  {'-'*3} {'-'*30} {'-'*20}")

ORDEN = [
    "analisis_icfes",
    "flujo_b",
    "generacion_rmd",
    "retroalimentacion",
    "renderizado_4_formatos",
    "arsenal_post_render",
    "detractor_fase2c",
    "coherencias_5",
    "validar_diversidad",
    "validar_icfes",
    "aprobacion_usuario",
]

for i, paso in enumerate(ORDEN, 1):
    info = state["pasos"].get(paso, {})
    completado = info.get("completado", False)
    icono = "✅" if completado else "⬜"

    # Información extra relevante
    extra = ""
    if paso == "flujo_b":
        req = info.get("requerido")
        if req is None:
            extra = "(requerido: por determinar)"
        elif req is True:
            extra = "(requerido: sí)"
        else:
            extra = "(requerido: no)"
    elif paso == "generacion_rmd":
        arch = info.get("archivo")
        if arch:
            extra = f"({arch})"
    elif paso == "detractor_fase2c":
        verd = info.get("veredicto")
        if verd:
            extra = f"(veredicto: {verd})"
    elif paso == "validar_diversidad":
        vu = info.get("versiones_unicas")
        if vu is not None:
            extra = f"({vu} versiones únicas)"

    label = paso.replace("_", " ")
    print(f"  {i:<3} {icono} {label:<28} {extra}")

total = sum(1 for p in ORDEN if state["pasos"].get(p, {}).get("completado", False))
print(f"")
print(f"  Progreso: {total}/{len(ORDEN)} pasos completados")

reaperturas = state.get("reaperturas") or []
if reaperturas:
    ult = reaperturas[-1]
    ult_ts = str(ult.get("timestamp") or "?")[:19]
    ult_motivo = " ".join(str(ult.get("motivo") or "?").split())
    print(f"  🔁 Reaperturas: {len(reaperturas)} (última: {ult_ts} "
          f"desde {ult.get('desde', '?')} — {ult_motivo})")

# Aviso (2026-10-28): firma del auto-sello del hook post-render hasta la v6.2. Ese hook sellaba
# renderizado_4_formatos y arsenal_post_render uno tras otro (el 6 unos 40-80 ms después del 5) sobre el
# ejercicio_state.json del cwd de la sesión, sin nota: esos dos sellos no prueban que alguien
# renderizara los formatos ni revisara el informe. Con los dos completados, el 6 de 40 a 80 ms después
# del 5 (en ese orden) y ninguno con más claves que completado y timestamp, se avisa. Solo informa: ni
# el estado ni los demás comandos (check, next, complete) ni el gate cambian por esto.
from datetime import datetime, timedelta

def firma_autosello(pasos):
    p5 = pasos.get("renderizado_4_formatos") or {}
    p6 = pasos.get("arsenal_post_render") or {}
    if p5.get("completado") is not True or p6.get("completado") is not True:
        return False
    if (set(p5) | set(p6)) - {"completado", "timestamp"}:
        return False
    try:
        t5 = datetime.fromisoformat(p5["timestamp"])
        t6 = datetime.fromisoformat(p6["timestamp"])
        delta_us = (t6 - t5) // timedelta(microseconds=1)   # enteros: sin error de coma flotante en los bordes
    except (KeyError, TypeError, ValueError):
        return False
    return 40_000 <= delta_us <= 80_000

if firma_autosello(state["pasos"]):
    print("  ⚠ pasos 5 y 6: firma del auto-sello del hook post-render (v6.2 o anterior), sin nota: "
          "re-verificar por artefacto antes de confiar en ellos")
print(f"")
PYEOF
}

# ---------------------------------------------------------------------------
# Comando: next
# ---------------------------------------------------------------------------
cmd_next() {
    local dir="${1:-}"
    [[ -z "$dir" ]] && die "Uso: next <dir>"

    local sf
    sf=$(state_path "$dir")
    require_state "$sf"

    # Obtener siguiente paso y su mensaje
    python3 - "$sf" <<'PYEOF'
import json, sys

sf = sys.argv[1]

with open(sf, 'r', encoding='utf-8') as fh:
    state = json.load(fh)

ORDEN = [
    "analisis_icfes",
    "flujo_b",
    "generacion_rmd",
    "retroalimentacion",
    "renderizado_4_formatos",
    "arsenal_post_render",
    "detractor_fase2c",
    "coherencias_5",
    "validar_diversidad",
    "validar_icfes",
    "aprobacion_usuario",
]

COMANDOS = {
    "analisis_icfes":       "/analizar-icfes",
    "flujo_b":              "Determinar si requiere gráficos",
    "generacion_rmd":       "/generar-schoice o /generar-cloze",
    "retroalimentacion":    "/skill-retroalimentacion",
    "renderizado_4_formatos": "exams2html/pdf/docx/nops",
    "arsenal_post_render":  "(hook post-render; sellar a mano con --nota)",
    "detractor_fase2c":     "/adversario archivo.Rmd",
    "coherencias_5":        "Documentar 5 coherencias",
    "validar_diversidad":   "/validar-diversidad",
    "validar_icfes":        "/validar-icfes",
    "aprobacion_usuario":   "Pedir aprobación al usuario",
}

pasos = state["pasos"]

for paso in ORDEN:
    info = pasos.get(paso, {})
    completado = info.get("completado", False)

    if completado:
        continue

    # Caso especial flujo_b
    if paso == "flujo_b":
        req = info.get("requerido")
        if req is None:
            print(f"⬜ Siguiente: flujo_b")
            print(f"   Acción   : Preguntar al usuario si el ejercicio requiere gráficos")
            print(f"   Luego    : workflow-state.sh complete <dir> flujo_b --requerido true|false")
            sys.exit(0)
        elif req is True and not completado:
            print(f"⬜ Siguiente: flujo_b")
            print(f"   Acción   : Completar el Flujo B (Graficador Experto)")
            print(f"   Comando  : /auto-refinar-grafico")
            print(f"   Luego    : workflow-state.sh complete <dir> flujo_b --lenguaje tikz|python|r")
            sys.exit(0)
        # req is False → skip (no requiere gráficos), continuar al siguiente
        continue

    print(f"⬜ Siguiente: {paso.replace('_', ' ')}")
    print(f"   Comando  : {COMANDOS.get(paso, '?')}")
    print(f"   Marcar   : workflow-state.sh complete <dir> {paso}")
    sys.exit(0)

print("✅ COMPLETO — Todos los pasos del workflow están finalizados.")
PYEOF
}

# ---------------------------------------------------------------------------
# Comando: reopen
# ---------------------------------------------------------------------------
# Reabre un ejercicio sellado para corregirlo. Des-sella --desde y todo paso
# posterior que estuviera completado (completado=false, timestamp=null) y deja
# en reaperturas[] el motivo y lo que esos pasos tenían (estado_previo): no se
# pierde nada y `status` no muestra los datos de la versión anterior.
# Sólo desde generacion_rmd en adelante: reabrir el análisis o el Flujo B es
# rehacer el ejercicio, no corregirlo.
cmd_reopen() {
    local dir=""
    local desde=""
    local motivo=""
    local reporte=""

    while [[ $# -gt 0 ]]; do
        case "$1" in
            --desde)
                shift
                desde="${1:-}"
                ;;
            --motivo)
                shift
                motivo="${1:-}"
                ;;
            --reporte)
                shift
                reporte="${1:-}"
                ;;
            --*)
                echo "ERROR: Opción desconocida para reopen: $1" >&2
                exit 2
                ;;
            *)
                if [[ -z "$dir" ]]; then
                    dir="$1"
                else
                    echo "ERROR: Argumento inesperado para reopen: $1" >&2
                    exit 2
                fi
                ;;
        esac
        shift
    done

    if [[ -z "$dir" ]]; then
        echo "Uso: reopen <dir> --desde <paso> --motivo \"<texto>\" [--reporte <ruta>]" >&2
        exit 2
    fi
    if [[ -z "$desde" ]]; then
        echo "ERROR: --desde es obligatorio: paso desde el que se reabre (generacion_rmd ... aprobacion_usuario)" >&2
        exit 2
    fi

    local sf
    sf=$(state_path "$dir")
    require_state "$sf"

    local ts
    ts=$(now_iso)

    python3 - "$sf" "$desde" "$motivo" "$reporte" "$ts" "${PASOS_ORDEN[@]}" <<'PYEOF'
import json, sys

sf      = sys.argv[1]
desde   = sys.argv[2]
motivo  = sys.argv[3].strip()
reporte = sys.argv[4].strip() or None
ts      = sys.argv[5]
ORDEN   = sys.argv[6:]  # orden canónico del workflow (PASOS_ORDEN)

# Sólo se reabre desde la generación del .Rmd en adelante.
INICIO     = ORDEN.index("generacion_rmd")
REABRIBLES = ORDEN[INICIO:]

# Un paso des-sellado queda con la forma que le da `init` (la que exige el
# schema): completado=false y timestamp=null, más estas claves. `archivo` se
# conserva (el .Rmd es el mismo, se corrige en el sitio); veredicto y
# versiones_unicas vuelven a null para que `status` no muestre los de la
# versión anterior. Todo lo demás que tuviera el paso va a estado_previo.
CONSERVA = {"generacion_rmd": ("archivo",)}
REINICIA = {"detractor_fase2c": ("veredicto",),
            "validar_diversidad": ("versiones_unicas",)}

if desde in ORDEN[:INICIO]:
    print(f"ERROR: No se puede reabrir desde '{desde}': reabrir el análisis o el "
          f"Flujo B no es una corrección, es rehacer el ejercicio.", file=sys.stderr)
    print(f"Para corregir, reabra desde '{REABRIBLES[0]}' o un paso posterior.",
          file=sys.stderr)
    sys.exit(2)
if desde not in REABRIBLES:
    print(f"ERROR: Paso desconocido '{desde}'", file=sys.stderr)
    print(f"Pasos desde los que se puede reabrir: {', '.join(REABRIBLES)}",
          file=sys.stderr)
    sys.exit(2)
if not motivo:
    print("ERROR: --motivo es obligatorio y no puede estar vacío: queda "
          "registrado en 'reaperturas' del estado.", file=sys.stderr)
    sys.exit(2)

with open(sf, 'r', encoding='utf-8') as fh:
    state = json.load(fh)

pasos = state["pasos"]
desellados = []
estado_previo = {}
for paso in REABRIBLES[REABRIBLES.index(desde):]:
    info = pasos.get(paso)
    if not isinstance(info, dict) or not info.get("completado", False):
        continue
    previo = {"timestamp": info.get("timestamp")}
    previo.update({k: v for k, v in info.items()
                   if k not in ("completado", "timestamp")})
    estado_previo[paso] = previo
    nuevo = {"completado": False, "timestamp": None}
    for k in CONSERVA.get(paso, ()):
        nuevo[k] = info.get(k)
    for k in REINICIA.get(paso, ()):
        nuevo[k] = None
    pasos[paso] = nuevo
    desellados.append(paso)

if not desellados:
    print(f"ERROR: nada que reabrir: ningún paso desde '{desde}' está completado "
          f"(el estado no se modificó).", file=sys.stderr)
    sys.exit(1)

reaperturas = state.get("reaperturas") or []
reaperturas.append({
    "timestamp": ts,
    "desde": desde,
    "motivo": motivo,
    "reporte": reporte,
    "pasos_desellados": desellados,
    "estado_previo": estado_previo,
})
state["reaperturas"] = reaperturas
state["timestamp_ultima_actualizacion"] = ts

# Serializar ANTES de abrir el archivo para escribir: si el motivo o el reporte
# no son UTF-8 válido, el estado queda intacto en vez de truncado.
try:
    datos = (json.dumps(state, indent=2, ensure_ascii=False) + '\n').encode('utf-8')
except UnicodeEncodeError:
    print("ERROR: --motivo o --reporte no son UTF-8 válido (el estado no se "
          "modificó).", file=sys.stderr)
    sys.exit(2)
with open(sf, 'wb') as fh:
    fh.write(datos)

print(f"🔁 Reapertura #{len(reaperturas)}: {len(desellados)} paso(s) des-sellado(s) "
      f"desde '{desde}': {', '.join(desellados)}")
PYEOF
}

# ---------------------------------------------------------------------------
# Comando: help
# ---------------------------------------------------------------------------
cmd_help() {
    cat <<'HELP'

workflow-state.sh — Gestión de estado del workflow de ejercicios ICFES

USO:
  workflow-state.sh <comando> [argumentos]

COMANDOS:

  init <dir> --tipo schoice|cloze [--nombre N]
      Crea ejercicio_state.json en <dir>.
      --tipo   : Tipo de ejercicio (obligatorio)
      --nombre : Nombre del ejercicio (por defecto: nombre del directorio)

  complete <dir> <paso> [--key value ...]
      Marca un paso como completado con timestamp automático.
      Se pueden agregar datos extra con --key value.
      Ejemplo: workflow-state.sh complete ./mi-ejercicio flujo_b --requerido false
      Ejemplo: workflow-state.sh complete ./mi-ejercicio validar_diversidad --versiones_unicas 287

  check <dir> <paso>
      Verifica si un paso está completado.
      Exit 0 = completado, Exit 1 = pendiente.
      Útil en scripts: workflow-state.sh check ./dir analisis_icfes && echo "OK"

  status <dir>
      Muestra todos los pasos con íconos ✅/⬜ y datos extras relevantes.
      Avisa (⚠) cuando los pasos 5 y 6 llevan la firma del auto-sello del hook post-render
      (v6.2 o anterior): los dos completados, el 6 de 40 a 80 ms después del 5 y ninguno con
      más claves que completado y timestamp, es decir, sin nota. Solo informa.

  next <dir>
      Muestra el siguiente paso pendiente con el comando sugerido.

  reopen <dir> --desde <paso> --motivo "<texto>" [--reporte <ruta>]
      Reabre un ejercicio ya sellado para corregirlo. Des-sella, en orden, todos
      los pasos completados desde <paso> hasta aprobacion_usuario (completado=false,
      timestamp=null) y guarda lo que tenían (veredicto, versiones únicas, ...) en
      la lista 'reaperturas' del estado, junto con el motivo: no se pierde nada.
      --desde   : generacion_rmd ... aprobacion_usuario. analisis_icfes y flujo_b
                  no se reabren: hacerlo es rehacer el ejercicio, no corregirlo.
      --motivo  : OBLIGATORIO (no puede ir vacío). Por qué se reabre.
      --reporte : ruta del reporte que motiva la reapertura (opcional).
      Exit 0 = reabierto; 1 = nada que reabrir (el archivo no cambia);
      2 = argumentos inválidos (el archivo no cambia).
      Ejemplo: workflow-state.sh reopen ./mi-ejercicio --desde validar_icfes --motivo "El detractor halló un distractor correcto" --reporte DETRACTOR.md

  help
      Muestra este mensaje.

PASOS DEL WORKFLOW (en orden):
  1.  analisis_icfes         → /analizar-icfes
  2.  flujo_b                → Determinar si requiere gráficos
  3.  generacion_rmd         → /generar-schoice o /generar-cloze
  4.  retroalimentacion      → /skill-retroalimentacion
  5.  renderizado_4_formatos → exams2html/pdf/docx/nops
  6.  arsenal_post_render    → hook post-render (corre solo; el paso se sella a mano, con --nota)
  7.  detractor_fase2c       → /adversario archivo.Rmd
  8.  coherencias_5          → Documentar 5 coherencias
  9.  validar_diversidad     → /validar-diversidad
  10. validar_icfes          → /validar-icfes
  11. aprobacion_usuario     → Pedir aprobación al usuario

CAMPOS ESPECIALES POR PASO:
  flujo_b           : --requerido true|false  (decisión, WAIT_USER #1)
                      --lenguaje tikz|python|r (elección, WAIT_USER #2)
                      Con requerido=true el paso SOLO se sella con --lenguaje;
                      sin él se registra la decisión y queda pendiente.
  generacion_rmd    : --archivo nombre_archivo.Rmd
  detractor_fase2c  : --veredicto APROBAR|"APROBAR CON CAMBIOS"|RECHAZAR
  validar_diversidad: --versiones_unicas N

EJEMPLO COMPLETO:
  workflow-state.sh init ./mi-ejercicio --tipo schoice --nombre "mediana_metacognitivo_v1"
  workflow-state.sh status ./mi-ejercicio
  workflow-state.sh complete ./mi-ejercicio analisis_icfes --resultado "Clasificado N2/Interpretación"
  workflow-state.sh complete ./mi-ejercicio flujo_b --requerido false
  workflow-state.sh next ./mi-ejercicio
  workflow-state.sh complete ./mi-ejercicio generacion_rmd --archivo "mediana_metacognitivo_v1.Rmd"

HELP
}

# ---------------------------------------------------------------------------
# Dispatcher principal
# ---------------------------------------------------------------------------
main() {
    local cmd="${1:-help}"
    shift || true

    case "$cmd" in
        init)     cmd_init "$@" ;;
        complete) cmd_complete "$@" ;;
        check)    cmd_check "$@" ;;
        status)   cmd_status "$@" ;;
        next)     cmd_next "$@" ;;
        reopen)   cmd_reopen "$@" ;;
        help|--help|-h) cmd_help ;;
        *)
            echo "ERROR: Comando desconocido: '$cmd'" >&2
            echo "Ejecute: workflow-state.sh help" >&2
            exit 1
            ;;
    esac
}

main "$@"
