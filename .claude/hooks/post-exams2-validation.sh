#!/bin/bash
# =============================================================================
# post-exams2-validation.sh (v6.2: el informe llega al modelo como JSON; 2I-2L con ruta relativa)
# Hook PostToolUse: ARSENAL COMPLETO DE VALIDACIONES POST-RENDERIZADO
#
# Corre después de cada comando Bash terminado con éxito cuyo texto renderiza un .Rmd literal
# (`exams2*("archivo.Rmd")`). Si el comando falla, Claude Code dispara PostToolUseFailure y este
# hook no corre. Fases:
#
# FASE 2A: Coherencia Matemática (.R script)
# FASE 2B: Preview Visual (PDF → PNG)
# FASE 2C: Opciones Únicas (gráficos diferentes)
# FASE 2D: Ortografía Española (tildes)
# FASE 2E: Metadatos ICFES (6 dimensiones)
# FASE 2F: Estructura Metacognitiva (Solution completa)
# FASE 2G: Multi-semilla rápida (100 semillas, Nivel 5; regla #23)
# FASE 2H: Stress Test Visual (10 semillas, renderizado real + PNGs)
# FASE 2I: Detección \pandocbounded en .tex y `![]()` sin width en .Rmd (Errores 16-17)
# FASE 2J: Letter-independence en Solution (Error 19, regla #19)
# FASE 2K: Guard contador 'none' para tablas (Error 21, regla #20)
# FASE 2L: Gráficas-opción en gap CLOZE (V5, Incidente G, regla graficos-como-opciones)
# FASE 2M: Auditoría visual HTML masiva (recordatorio — workflow)
# FASE 2N: Detección ESTÁTICA de diversidad cosmética (regla #22 — WARN_DIV_ESTATICA)
# FASE 2O: Glifos Unicode que rompen pdflatex (ERR_GLIFO_LATEX)
#
# 2G y 2H solo corren si las fases anteriores no dieron errores. El informe llega al modelo como JSON
# (sección «ENTREGA DEL INFORME AL MODELO») y queda completo en ~/.cache/post-exams2-validation/.
# =============================================================================

set -o pipefail

# Leer JSON de stdin
INPUT=$(cat)

# Extraer el comando ejecutado usando Python
COMMAND=$(echo "$INPUT" | python3 -c "
import sys, json
try:
    data = json.load(sys.stdin)
    cmd = data.get('tool_input', {}).get('command', '')
    print(cmd)
except:
    pass
" 2>/dev/null)

# Si no hay comando, salir silenciosamente
if [ -z "$COMMAND" ]; then
  exit 0
fi

# Solo proceder si el comando contiene exams2.
# Here-string, no `echo | grep -q`: con `set -o pipefail`, en un comando de más de ~30 KB echo
# recibía SIGPIPE y el hook entero se saltaba sin avisar (con 37 KB, 19 de 20 corridas;
# 2026-10-28, tests/testthat/test_post_exams2_pipefail.R).
if ! grep -q 'exams2' <<< "$COMMAND"; then
  exit 0
fi

# Extraer directorio de trabajo
CWD=$(echo "$INPUT" | python3 -c "
import sys, json
try:
    data = json.load(sys.stdin)
    print(data.get('cwd', ''))
except:
    pass
" 2>/dev/null)

if [ -z "$CWD" ]; then
  CWD="$(pwd)"
fi

# Extraer nombre del archivo .Rmd del comando
RMD_FILE=$(echo "$COMMAND" | grep -oP 'exams2\w+\(\s*"[^"]*\.Rmd"' | head -1 | grep -oP '"[^"]*\.Rmd"' | tr -d '"')

if [ -z "$RMD_FILE" ]; then
  RMD_FILE=$(echo "$COMMAND" | grep -oP "exams2\w+\(\s*'[^']*\.Rmd'" | head -1 | grep -oP "'[^']*\.Rmd'" | tr -d "'")
fi

if [ -z "$RMD_FILE" ]; then
  exit 0
fi

# Determinar ruta del proyecto
PROJECT_DIR="${CLAUDE_PROJECT_DIR:-$(git -C "$CWD" rev-parse --show-toplevel 2>/dev/null)}"
if [ -z "$PROJECT_DIR" ]; then
  exit 0
fi

# =============================================================================
# ENTREGA DEL INFORME AL MODELO (v6.2)
# =============================================================================
# Con exit 0, Claude Code manda el texto plano de un PostToolUse al log de depuración, no al modelo
# (https://code.claude.com/docs/en/hooks.md#exit-code-0): hasta la v6.1 este informe no lo leía
# nadie (verificado el 2026-10-28). Desde la v6.2 el hook escribe en stdout un único objeto JSON:
#   - hookSpecificOutput.additionalContext: el informe sin marcos o, si no cabe, sus líneas clave.
#     Claude Code corta ese campo en 10 000 caracteres y del exceso solo muestra una vista previa de
#     2 000, así que el hook se queda en 9 000.
#   - decision "block" + reason, si hay errores: Claude Code pone la razón junto al resultado.
#   - systemMessage: una línea para el usuario.
# El informe completo queda en $POST_EXAMS2_INFORME_DIR (por defecto ~/.cache/post-exams2-validation/).
# Test: tests/testthat/test_post_exams2_informe.R.
emitir_json() {
  # emitir_json aviso <texto>
  # emitir_json informe <ruta_del_informe> <nombre> <errores> <advertencias>
  python3 - "$@" <<'PY'
import json, re, sys

LIMITE = 9000


def largo(texto):
    # Unidades UTF-16, como mide una cadena de JavaScript.
    return len(texto.encode("utf-16-le")) // 2


if sys.argv[1] == "aviso":
    texto = sys.argv[2]
    print(json.dumps({"systemMessage": texto,
                      "hookSpecificOutput": {"hookEventName": "PostToolUse",
                                             "additionalContext": texto}},
                     ensure_ascii=False))
    sys.exit(0)

ruta, nombre = sys.argv[2], sys.argv[3]
errores, advertencias = int(sys.argv[4] or 0), int(sys.argv[5] or 0)
try:
    with open(ruta, encoding="utf-8", errors="replace") as f:
        crudo = f.read()
except OSError:
    crudo = ""

# Fuera colores, líneas que solo son marco y los bordes de las cajas.
crudo = re.sub(r"\x1b\[[0-9;]*[A-Za-z]", "", crudo)
lineas = []
for linea in crudo.splitlines():
    if re.fullmatch(r"[\s╔╗╚╝╠╣═║┌┐└┘─│]*", linea):
        if lineas and lineas[-1] != "":
            lineas.append("")
        continue
    linea = re.sub(r"^\s*[║│]\s*", "", linea)
    lineas.append(re.sub(r"\s*[║│]\s*$", "", linea).rstrip())
while lineas and lineas[-1] == "":
    lineas.pop()

cabecera = (f"Arsenal post-render (post-exams2-validation.sh v6.2) sobre {nombre}.Rmd: "
            f"{errores} error(es), {advertencias} advertencia(s). Informe completo: {ruta}")
cuerpo = "\n".join(lineas).strip()
if largo(cabecera) + largo(cuerpo) + 2 > LIMITE:
    # Resumen: de las fases, los encabezados y las líneas con marca (más dos de detalle tras cada
    # ❌); el bloque final (resumen, órdenes y previews), entero.
    corte = max([i for i, l in enumerate(lineas) if "RESUMEN FINAL" in l], default=len(lineas))
    marca = re.compile(r"FASE 2|❌|⚠|ERR_|WARN_|ERROR")
    sel, detalle = [], 0
    for linea in lineas[:corte]:
        if marca.search(linea):
            sel.append(linea)
            detalle = 2 if "❌" in linea else 0
        elif detalle and linea.strip():
            sel.append(linea)
            detalle -= 1
    cabecera += ("\n(Resumido para caber en el contexto: de cada fase, el encabezado y las líneas con "
                 "❌, ⚠️, ERR_, WARN_ o ERROR; el resto está en el informe completo.)")
    final = "\n".join(lineas[corte:])
    nota = "[… recortado: ver el informe completo]"
    libre = LIMITE - largo(cabecera) - largo(final) - largo(nota) - 6
    tomadas, usado = [], 0
    for linea in sel:
        if usado + largo(linea) + 1 > libre:
            tomadas.append(nota)
            break
        tomadas.append(linea)
        usado += largo(linea) + 1
    cuerpo = "\n".join(tomadas) + "\n\n" + final
contexto = cabecera + "\n\n" + cuerpo
if largo(contexto) > LIMITE:
    contexto = contexto[:LIMITE - 50] + "\n[… recortado]"

salida = {
    "systemMessage": (f"Arsenal post-render de {nombre}.Rmd: {errores} error(es), "
                      f"{advertencias} advertencia(s). Informe: {ruta}"),
    "hookSpecificOutput": {"hookEventName": "PostToolUse", "additionalContext": contexto},
}
if errores > 0:
    salida["decision"] = "block"
    salida["reason"] = (f"El arsenal post-render encontró {errores} error(es) en {nombre}.Rmd. "
                        "Corregirlos y volver a renderizar antes de continuar: el detalle está en el "
                        f"contexto adicional de este hook y el informe completo en {ruta}.")
print(json.dumps(salida, ensure_ascii=False))
PY
}

# Construir ruta completa del .Rmd
if [[ "$RMD_FILE" = /* ]]; then
  RMD_PATH="$RMD_FILE"
else
  RMD_PATH="$CWD/$RMD_FILE"
fi

if [ ! -f "$RMD_PATH" ]; then
  # Hasta la v6.1 salía callado: un render con el .Rmd fuera del cwd de la sesión (por ejemplo, tras
  # un `cd` dentro del mismo comando) dejaba el arsenal sin correr y nadie se enteraba.
  emitir_json aviso "post-exams2-validation.sh: el comando renderiza «$RMD_FILE», pero ese archivo no existe visto desde el cwd de la sesión ($CWD). El arsenal post-render NO corrió; para que corra, renderizar con la ruta absoluta del .Rmd."
  exit 0
fi

RMD_BASENAME=$(basename "$RMD_FILE" .Rmd)

# Las fases 2I-2L comprueban `[ -f "$RMD_FILE" ]` con la ruta tal como vino en el comando. Si el
# proceso del hook no está en el cwd de la sesión, una ruta relativa no aparece y esas fases callan:
# con el mismo .Rmd, 0 errores con ruta relativa y 2 con absoluta (2026-10-28).
cd "$CWD" 2>/dev/null || true

# Desde aquí todo lo que escribe el arsenal (stdout y stderr, también el de los Rscript) va al
# informe. Al salir, por la vía que sea, el trap restaura los descriptores y entrega el JSON.
INFORME_DIR="${POST_EXAMS2_INFORME_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/post-exams2-validation}"
INFORME="$INFORME_DIR/${RMD_BASENAME}.txt"
if ! { mkdir -p "$INFORME_DIR" && : > "$INFORME"; } 2>/dev/null; then
  INFORME=$(mktemp "${TMPDIR:-/tmp}/post-exams2-XXXXXX.txt")
fi
exec 3>&1 4>&2 1>"$INFORME" 2>&1
trap 'exec 1>&3 2>&4; emitir_json informe "$INFORME" "$RMD_BASENAME" "${ERRORES_TOTALES:-0}" "${ADVERTENCIAS_TOTALES:-0}"' EXIT

# =============================================================================
# ENCABEZADO DEL ARSENAL
# =============================================================================

echo ""
echo "╔═══════════════════════════════════════════════════════════════╗"
echo "║  ARSENAL COMPLETO DE VALIDACIONES POST-RENDERIZADO            ║"
echo "║  Activado automáticamente - NO HAY FORMA DE SALTARLO          ║"
echo "╠═══════════════════════════════════════════════════════════════╣"
echo "║  Archivo: $(printf '%-52s' "$RMD_BASENAME.Rmd")║"
echo "╚═══════════════════════════════════════════════════════════════╝"
echo ""

ERRORES_TOTALES=0
ADVERTENCIAS_TOTALES=0

# =============================================================================
# FASE 2A: VALIDACIÓN DE COHERENCIA MATEMÁTICA
# =============================================================================

SCRIPT_MATH="$PROJECT_DIR/.claude/scripts/validar_coherencia_matematica.R"

echo "┌───────────────────────────────────────────────────────────────┐"
echo "│ FASE 2A: Coherencia Matemática                                │"
echo "└───────────────────────────────────────────────────────────────┘"

if [ -f "$SCRIPT_MATH" ]; then
  MATH_OUTPUT=$(cd "$CWD" && Rscript "$SCRIPT_MATH" "$RMD_FILE" 2>&1)
  MATH_EXIT=$?

  if [ $MATH_EXIT -eq 0 ]; then
    echo "  ✓ APROBADO"
    echo "$MATH_OUTPUT" | grep -E "✓|APROBADO" | head -5
  else
    echo "  ❌ ERRORES DETECTADOS"
    echo "$MATH_OUTPUT" | tail -10
    ERRORES_TOTALES=$((ERRORES_TOTALES + 1))
  fi
else
  echo "  ⚠️  Script no encontrado: validar_coherencia_matematica.R"
  ADVERTENCIAS_TOTALES=$((ADVERTENCIAS_TOTALES + 1))
fi

echo ""

# =============================================================================
# FASE 2B: GENERACIÓN DE PREVIEW VISUAL
# =============================================================================

echo "┌───────────────────────────────────────────────────────────────┐"
echo "│ FASE 2B: Preview Visual (PDF → PNG)                           │"
echo "└───────────────────────────────────────────────────────────────┘"

# Buscar PDF generado
PDF_DIR=$(echo "$COMMAND" | grep -oP 'dir\s*=\s*"[^"]*"' | head -1 | grep -oP '"[^"]*"' | tr -d '"')
if [ -z "$PDF_DIR" ]; then
  PDF_DIR=$(echo "$COMMAND" | grep -oP "dir\s*=\s*'[^']*'" | head -1 | grep -oP "'[^']*'" | tr -d "'")
fi

PDF_FOUND=""
SEARCH_DIRS=()

if [ -n "$PDF_DIR" ]; then
  if [[ "$PDF_DIR" = /* ]]; then
    SEARCH_DIRS+=("$PDF_DIR")
  else
    SEARCH_DIRS+=("$CWD/$PDF_DIR")
  fi
fi

SEARCH_DIRS+=("$CWD/output_pdf" "$CWD/output_pdf_test" "$CWD/output" "$CWD")

for DIR in "${SEARCH_DIRS[@]}"; do
  if [ -d "$DIR" ]; then
    CANDIDATE=$(find "$DIR" -maxdepth 1 -name "*.pdf" -type f -mmin -5 2>/dev/null | head -1)
    if [ -n "$CANDIDATE" ] && [ -f "$CANDIDATE" ]; then
      PDF_FOUND="$CANDIDATE"
      break
    fi
  fi
done

if [ -n "$PDF_FOUND" ]; then
  PREVIEW_PNG="$CWD/preview_${RMD_BASENAME}.png"

  if command -v magick &>/dev/null; then
    magick -density 150 "$PDF_FOUND" -quality 90 "$PREVIEW_PNG" 2>/dev/null
    MAGICK_EXIT=$?

    if [ $MAGICK_EXIT -eq 0 ]; then
      NUM_PAGES=$(ls -1 "${CWD}/preview_${RMD_BASENAME}"*.png 2>/dev/null | wc -l)
      echo "  ✓ Preview generado: $NUM_PAGES página(s)"
      ls -1 "${CWD}/preview_${RMD_BASENAME}"*.png 2>/dev/null | while read f; do
        echo "    → $(basename "$f")"
      done
    else
      echo "  ❌ Error al convertir PDF a PNG"
      ERRORES_TOTALES=$((ERRORES_TOTALES + 1))
    fi
  else
    echo "  ⚠️  magick no instalado - preview manual requerido"
    ADVERTENCIAS_TOTALES=$((ADVERTENCIAS_TOTALES + 1))
  fi
else
  echo "  ⚠️  PDF no encontrado - ejecutar exams2pdf() primero"
  ADVERTENCIAS_TOTALES=$((ADVERTENCIAS_TOTALES + 1))
fi

echo ""

# =============================================================================
# FASES 2C-2F: ARSENAL COMPLETO DE VALIDACIÓN
# =============================================================================

SCRIPT_ARSENAL="$PROJECT_DIR/.claude/scripts/arsenal_validacion_completa.R"

if [ -f "$SCRIPT_ARSENAL" ]; then
  echo "┌───────────────────────────────────────────────────────────────┐"
  echo "│ FASES 2C-2F: Arsenal Completo                                 │"
  echo "└───────────────────────────────────────────────────────────────┘"

  ARSENAL_OUTPUT=$(cd "$CWD" && Rscript "$SCRIPT_ARSENAL" "$RMD_FILE" 2>&1)
  ARSENAL_EXIT=$?

  # Mostrar salida filtrada
  echo "$ARSENAL_OUTPUT" | grep -E "FASE|✓|❌|⚠️|ERROR|ADVERTENCIA|Total|VALIDACIÓN"

  # Preferir el conteo reportado por el script; si no lo reporta, fallback a +1 por exit!=0
  ARSENAL_ERRORES=$(echo "$ARSENAL_OUTPUT" | grep "Total ERRORES:" | grep -oP '\d+' | tail -1)
  ARSENAL_ADVERTENCIAS=$(echo "$ARSENAL_OUTPUT" | grep "Total ADVERTENCIAS:" | grep -oP '\d+' | tail -1)

  if [ -n "$ARSENAL_ERRORES" ]; then
    ERRORES_TOTALES=$((ERRORES_TOTALES + ARSENAL_ERRORES))
  elif [ $ARSENAL_EXIT -ne 0 ]; then
    ERRORES_TOTALES=$((ERRORES_TOTALES + 1))
  fi

  if [ -n "$ARSENAL_ADVERTENCIAS" ]; then
    ADVERTENCIAS_TOTALES=$((ADVERTENCIAS_TOTALES + ARSENAL_ADVERTENCIAS))
  fi
else
  echo "  ⚠️  Script arsenal no encontrado"
  ADVERTENCIAS_TOTALES=$((ADVERTENCIAS_TOTALES + 1))
fi

echo ""

# =============================================================================
# FASE 2G: VALIDACIÓN MULTI-SEMILLA RÁPIDA (Nivel 5)
# =============================================================================

SCRIPT_MULTISEMILLA="$PROJECT_DIR/.claude/scripts/validar_multisemilla.R"

if [ -f "$SCRIPT_MULTISEMILLA" ] && [ $ERRORES_TOTALES -eq 0 ]; then
  echo "┌───────────────────────────────────────────────────────────────┐"
  echo "│ FASE 2G: Multi-semilla (100 semillas, Nivel 5) — regla #23    │"
  echo "└───────────────────────────────────────────────────────────────┘"

  # N=100 = muestra estandar unica (.claude/rules/muestra-estandar-validacion.md).
  # El timeout del hook en settings.json subio a 300 s para que quepa: medido
  # 61 s en el ejercicio con el motor mas pesado del repo.
  MULTISEED_OUTPUT=$(cd "$CWD" && Rscript "$SCRIPT_MULTISEMILLA" "$RMD_FILE" --n 100 2>&1)
  MULTISEED_EXIT=$?

  echo "$MULTISEED_OUTPUT" | grep -E "Semillas|Fallos|Tasa|RESULTADO|ERR_ANS|ERR_SEM"

  if [ $MULTISEED_EXIT -ne 0 ]; then
    ERRORES_TOTALES=$((ERRORES_TOTALES + 1))
    echo "  ❌ Multi-semilla: FALLOS detectados en alguna(s) semilla(s)"
  else
    echo "  ✓ Multi-semilla: 100/100 semillas aprobadas"
  fi
elif [ $ERRORES_TOTALES -gt 0 ]; then
  echo "  ⚠️  Multi-semilla omitida (hay errores previos que resolver primero)"
fi

echo ""

# =============================================================================
# FASE 2H: STRESS TEST VISUAL MULTI-SEMILLA
# =============================================================================

SCRIPT_STRESS_TEST="$PROJECT_DIR/SOURCES/scripts_validacion/stress_test_visual.R"

if [ -f "$SCRIPT_STRESS_TEST" ] && [ $ERRORES_TOTALES -eq 0 ]; then
  echo "┌───────────────────────────────────────────────────────────────┐"
  echo "│ FASE 2H: Stress Test Visual (10 semillas, renderizado + PNGs) │"
  echo "└───────────────────────────────────────────────────────────────┘"

  STRESS_OUTPUT_DIR="$CWD/stress_test_output"
  STRESS_OUTPUT=$(cd "$CWD" && Rscript "$SCRIPT_STRESS_TEST" "$RMD_FILE" --n 10 --output-dir "$STRESS_OUTPUT_DIR" 2>&1)
  STRESS_EXIT=$?

  echo "$STRESS_OUTPUT" | grep -E "Semillas|Anomalías|VEREDICTO|PASA|FALLA|ADVERTENCIA|sospechosas"

  if [ $STRESS_EXIT -ne 0 ]; then
    ERRORES_TOTALES=$((ERRORES_TOTALES + 1))
    echo "  ❌ Stress Test Visual: ANOMALÍAS detectadas"
    echo ""
    echo "  Claude DEBE:"
    echo "  1. Read(\"$STRESS_OUTPUT_DIR/reporte.json\") para ver anomalías"
    echo "  2. Read() CADA PNG en $STRESS_OUTPUT_DIR/pngs/ de semillas sospechosas"
    echo "  3. Verificar visualmente los problemas reportados"
  else
    echo "  ✓ Stress Test Visual: Sin anomalías críticas"
  fi
elif [ $ERRORES_TOTALES -gt 0 ]; then
  echo "  ⚠️  Stress Test Visual omitido (hay errores previos que resolver primero)"
fi

echo ""

# =============================================================================
# FASE 2I: ANTI-PANDOCBOUNDED + COHERENCIA SOLUTION (Errores 16-17, regla #18)
# =============================================================================

echo "┌───────────────────────────────────────────────────────────────┐"
echo "│ FASE 2I: Anti-\\pandocbounded + coherencia Solution (E16-E17)  │"
echo "└───────────────────────────────────────────────────────────────┘"

PANDOC_BUG_FOUND=0
SOL_BUG_FOUND=0

# 2I.1 — Markdown sin width en el .Rmd (estático)
if [ -f "$RMD_FILE" ]; then
  # Patrón: !\[...\](...\.png) sin {width=...} inmediatamente después
  IMG_HITS=$(grep -nE '!\[[^]]*\]\([^)]+\.(png|jpe?g|svg|pdf)\)' "$RMD_FILE" | \
             grep -vE '!\[[^]]*\]\([^)]+\.(png|jpe?g|svg|pdf)\)\{[^}]*width' || true)
  # Excluir comentarios R puros
  IMG_HITS=$(echo "$IMG_HITS" | grep -vE '^[0-9]+:\s*#' || true)
  if [ -n "$IMG_HITS" ]; then
    PANDOC_BUG_FOUND=1
    ERRORES_TOTALES=$((ERRORES_TOTALES + 1))
    echo "  ❌ ERROR 16: Markdown sin atributo {width=...} (causará \\pandocbounded en PDF)"
    echo "$IMG_HITS" | sed 's/^/     /'
    echo ""
    echo "     Fix: reemplazar \`![](file.png)\` por \`cat(\"![](file.png){width=80%}\\\\n\")\` en bloque R"
    echo "     Ver .claude/rules/markdown-imagenes-pdf.md"
  fi
fi

# 2I.2 — \pandocbounded en .tex generados durante este render
#
# ⚠️ HAY QUE DISTINGUIR **DEFINICIÓN** DE **USO**. Desde R/exams 2.4-1 TODAS las
#    plantillas LaTeX del paquete traen la definición no-op
#        \providecommand{\pandocbounded}[1]{#1}
#    (exams/tex/{plain,plain8,exam,form,solution,plain-highlight}.tex).
#    Un `grep pandocbounded` a secas casa esa DEFINICIÓN y marcaba ERROR 16 en
#    CUALQUIER .tex, tuviera o no imágenes -> gate que siempre falla, que se
#    aprende a ignorar (misma patología que la FASE 2G en falso rojo).
#    El fallo real (`Undefined control sequence`) exige USO **sin** definición,
#    que es lo que ocurre con una plantilla propia o con exams < 2.4-1.
#
#    Las dos firmas se declaran como variables para que el test de regresión
#    las extraiga del propio hook y no derive una copia paralela.
PB_USO_RE='\\pandocbounded[[:space:]]*\{'
PB_DEF_RE='\\(providecommand|newcommand|renewcommand|def)[[:space:]]*\{?\\pandocbounded'

PB_USOS=""
PB_DEFINIDO=0
TEX_FILES=$(find "$CWD" -maxdepth 4 -name "*.tex" -newer "$RMD_FILE" 2>/dev/null)
if [ -n "$TEX_FILES" ]; then
  for tex in $TEX_FILES; do
    if grep -qE "$PB_USO_RE" "$tex" 2>/dev/null; then
      PB_USOS="${PB_USOS} $(basename "$tex")"
    fi
    # El preámbulo vive en plain*.tex y el uso en exercise*.tex: la definición
    # se busca en TODO el conjunto del render, no fichero a fichero.
    if grep -qE "$PB_DEF_RE" "$tex" 2>/dev/null; then
      PB_DEFINIDO=1
    fi
  done
fi

if [ -n "$PB_USOS" ] && [ $PB_DEFINIDO -eq 0 ]; then
  PANDOC_BUG_FOUND=1
  ERRORES_TOTALES=$((ERRORES_TOTALES + 1))
  echo "  ❌ ERROR 16: uso de \\pandocbounded{...} SIN definición en el preámbulo"
  echo "     .tex con uso:${PB_USOS}"
  echo "     Fix: dar {width=...} a la imagen (regla #18), o usar una plantilla"
  echo "          que defina el macro. Ver .claude/rules/markdown-imagenes-pdf.md"
elif [ -n "$PB_USOS" ]; then
  echo "  ✓ \\pandocbounded en uso pero DEFINIDO en el preámbulo (R/exams >= 2.4-1): no bloquea"
fi

# 2I.3 — exshuffle: TRUE + Solution con letra explícita (estático)
if [ -f "$RMD_FILE" ]; then
  HAS_EXSHUFFLE_TRUE=$(grep -E '^[[:space:]]*exshuffle[[:space:]]*:[[:space:]]*TRUE' "$RMD_FILE" | head -1)
  HAS_LETRA_REF=$(grep -nE '`r[[:space:]]+letra_correcta[[:space:]]*`|Opci[oó]n[[:space:]]+\*\*[A-D]\*\*|Opci[oó]n[[:space:]]+[A-D][[:space:]]*\(?CORRECTA' "$RMD_FILE" | head -3)

  if [ -n "$HAS_EXSHUFFLE_TRUE" ] && [ -n "$HAS_LETRA_REF" ]; then
    SOL_BUG_FOUND=1
    ERRORES_TOTALES=$((ERRORES_TOTALES + 1))
    echo "  ❌ ERROR 17: exshuffle:TRUE combinado con Solution que referencia letra explícita"
    echo "     exshuffle: $HAS_EXSHUFFLE_TRUE"
    echo "     Solution refs:"
    echo "$HAS_LETRA_REF" | sed 's/^/       /'
    echo ""
    echo "     Fix: cambiar a \`exshuffle: FALSE\` + mezcla interna con sample()"
    echo "     Ver .claude/rules/codigo-rmd.md regla #6 + graficos-como-opciones.md"
  fi
fi

if [ $PANDOC_BUG_FOUND -eq 0 ] && [ $SOL_BUG_FOUND -eq 0 ]; then
  echo "  ✓ FASE 2I: sin patrones de Error 16/17 detectados"
fi

echo ""

# =============================================================================
# FASE 2J: LETTER-INDEPENDENCE EN SOLUTION (Error 19, regla #19)
# =============================================================================
# Defensa permanente: Solution NUNCA debe referenciar letra/posicion de opcion.
# Aplica INDEPENDIENTEMENTE del valor de exshuffle (Moodle puede re-shufflear).

echo "┌───────────────────────────────────────────────────────────────┐"
echo "│ FASE 2J: Letter-independence en Solution (E19, regla #19)     │"
echo "└───────────────────────────────────────────────────────────────┘"

LETRA_BUG_FOUND=0

if [ -f "$RMD_FILE" ]; then
  # Extraer SOLO la seccion Solution (entre 'Solution' y 'Meta-information')
  SOL_BLOCK=$(awk '
    /^Solution[[:space:]]*$/ { in_sol = 1; next }
    /^Meta-information[[:space:]]*$/ { in_sol = 0 }
    in_sol == 1 { print NR ":" $0 }
  ' "$RMD_FILE")

  if [ -n "$SOL_BLOCK" ]; then
    # P1+P2: backtick-r letra_correcta o letras[...]
    P1_HITS=$(echo "$SOL_BLOCK" | grep -E '`r[[:space:]]+(letra_correcta|letras\[)' || true)
    if [ -n "$P1_HITS" ]; then
      LETRA_BUG_FOUND=1
      ERRORES_TOTALES=$((ERRORES_TOTALES + 1))
      echo "  ❌ ERR_SOL_LETRA_R: Solution interpola letra_correcta o letras[...]"
      echo "$P1_HITS" | sed 's/^/     /'
    fi

    # P3: cat("**Opcion ", l, ...) — emision dinamica con variable de letra
    P3_HITS=$(echo "$SOL_BLOCK" | grep -E 'cat\([^)]*"\*?\*?Opci[oó]n[[:space:]]+",[[:space:]]*l[[:space:],)]' || true)
    if [ -n "$P3_HITS" ]; then
      LETRA_BUG_FOUND=1
      ERRORES_TOTALES=$((ERRORES_TOTALES + 1))
      echo "  ❌ ERR_SOL_LETRA_CAT: chunk R emite 'Opción ' con variable de letra"
      echo "$P3_HITS" | sed 's/^/     /'
    fi

    # P4: literal "Opcion A/B/C/D" en Markdown (no dentro de backticks de codigo)
    # Filtrar lineas que parecen ser parte de codigo R (cat con error$codigo, etc)
    P4_HITS=$(echo "$SOL_BLOCK" | grep -E '(^|[^`])(\*\*)?Opci[oó]n[[:space:]]+[A-D]\b' || true)
    # Excluir matches dentro de strings de codigo R que construyen prosa generica
    P4_HITS=$(echo "$P4_HITS" | grep -vE 'descripcion_corta|descripcion_larga|err\$' || true)
    if [ -n "$P4_HITS" ]; then
      LETRA_BUG_FOUND=1
      ERRORES_TOTALES=$((ERRORES_TOTALES + 1))
      echo "  ❌ ERR_SOL_LETRA_LITERAL: Solution menciona 'Opción [A-D]' literal"
      echo "$P4_HITS" | sed 's/^/     /'
    fi

    if [ $LETRA_BUG_FOUND -gt 0 ]; then
      echo ""
      echo "     Fix obligatorio (regla #19, sin excepciones):"
      echo "       1. Quitar 'Opción \`r letra_correcta\`' de headers de Solution"
      echo "       2. Identificar la opción correcta por contenido o codigo de error"
      echo "          (ej: cita de descripcion_corta + codigo GRAF-ARG-NN)"
      echo "       3. En for loop de distractores, NO emitir 'Opcion <letra>'"
      echo "          Reemplazar por '<codigo> — <nombre>'"
      echo ""
      echo "     Ver .claude/rules/solution-letter-independence.md"
      echo "     Razon: Moodle (y otros LMS) puede re-shufflear las opciones"
      echo "            INDEPENDIENTEMENTE de exshuffle:FALSE de R-exams, rompiendo"
      echo "            la coherencia letra <-> contenido para el estudiante."
    else
      echo "  ✓ FASE 2J: Solution es letter-independent (regla #19)"
    fi
  else
    echo "  ⊘ FASE 2J: no se encontro seccion Solution en el .Rmd"
  fi
fi

echo ""

# =============================================================================
# FASE 2K: GUARD CONTADOR 'none' PARA TABLAS (Error 21, regla #20)
# =============================================================================
# pandoc >=3.7 (RStudio bundlea 3.8.3) envuelve longtable con \def\LTcaptype{none}.
# La plantilla LaTeX de R-exams no define el contador 'none' -> exams2pdf/nops
# fallan con "No counter 'none' defined". Todo .Rmd con tabla markdown DEBE incluir
# el guard \newcounter{none} al inicio de Question.

echo "┌───────────────────────────────────────────────────────────────┐"
echo "│ FASE 2K: Guard contador 'none' para tablas (E21, regla #20)   │"
echo "└───────────────────────────────────────────────────────────────┘"

if [ -f "$RMD_FILE" ]; then
  TIENE_TABLA=$(grep -nE 'kable\(|format[[:space:]]*=[[:space:]]*"markdown"|cat\("\|' "$RMD_FILE" || true)
  TIENE_GUARD=$(grep -nE 'newcounter\{none\}|@ifundefined\{c@none\}' "$RMD_FILE" || true)
  if [ -n "$TIENE_TABLA" ] && [ -z "$TIENE_GUARD" ]; then
    ERRORES_TOTALES=$((ERRORES_TOTALES + 1))
    echo "  ❌ ERR_TABLA_NONE: .Rmd con tabla markdown SIN guard del contador 'none'"
    echo "     pandoc >=3.7 (RStudio usa 3.8.3) -> exams2pdf/nops fallan con"
    echo "     'No counter \"none\" defined' (invisible con el pandoc 3.6 de terminal)."
    echo ""
    echo "     Fix obligatorio (regla #20): al INICIO de la seccion Question, un"
    echo "     bloque raw LaTeX (se ignora en HTML/DOCX):"
    echo '       ```{=latex}'
    echo '       \makeatletter\@ifundefined{c@none}{\newcounter{none}}{}\makeatother'
    echo '       ```'
    echo "     Ver .claude/rules/markdown-tablas-pandoc.md"
  elif [ -n "$TIENE_TABLA" ]; then
    echo "  ✓ FASE 2K: tabla markdown con guard del contador 'none' presente"
  else
    echo "  ⊘ FASE 2K: el .Rmd no usa tablas markdown (guard no requerido)"
  fi
fi

echo ""

# =============================================================================
# FASE 2L: GRÁFICAS-OPCIÓN EN GAP CLOZE (V5, Incidente G, regla graficos-como-opciones)
# =============================================================================
# En Moodle un gap CLOZE ({1:MULTICHOICE:...}) renderiza sus opciones como texto
# plano y descarta el HTML -> las <img> de las opciones desaparecen ("no se ven
# los graficos en el Paso N"). Las graficas-opcion de un CLOZE DEBEN ir rotuladas
# en el ENUNCIADO + opciones de TEXTO. Deteccion estatica: imagen markdown dentro
# del Answerlist del ENUNCIADO (las imagenes legitimas del enunciado van ANTES del
# header "Answerlist", por lo que no son falso positivo). Distinto del SCHOICE puro.

echo "┌───────────────────────────────────────────────────────────────┐"
echo "│ FASE 2L: Gráficas-opción en gap CLOZE (V5, Incidente G)       │"
echo "└───────────────────────────────────────────────────────────────┘"

if [ -f "$RMD_FILE" ]; then
  ES_CLOZE=$(grep -nE '^extype:[[:space:]]*cloze|^exclozetype:' "$RMD_FILE" || true)
  if [ -n "$ES_CLOZE" ]; then
    # Answerlist del ENUNCIADO = primer bloque Answerlist, antes de la seccion Solution.
    ENUNCIADO_AL=$(awk '
      /^Solution[[:space:]]*$/ { in_al=0; next }
      /^Answerlist[[:space:]]*$/ && !seen { in_al=1; seen=1; next }
      in_al { print }
    ' "$RMD_FILE")
    # Imagen markdown ![](...) dentro de las opciones del gap (directa o via cat()).
    IMG_EN_AL=$(echo "$ENUNCIADO_AL" | grep -nE '!\[[^]]*\]\(' || true)
    if [ -n "$IMG_EN_AL" ]; then
      ERRORES_TOTALES=$((ERRORES_TOTALES + 1))
      echo "  ❌ ERR_CLOZE_V5: el Answerlist del enunciado (opciones del gap) contiene"
      echo "     imágenes markdown. En Moodle un gap CLOZE NO renderiza <img> -> el"
      echo "     estudiante 'no ve los gráficos en el Paso N'."
      echo "$IMG_EN_AL" | sed 's/^/     /'
      echo ""
      echo "     Fix obligatorio (Incidente G, regla graficos-como-opciones.md):"
      echo "       1. Mover las gráficas ROTULADAS (I, II, III…) al ENUNCIADO de la"
      echo "          parte (chunk results='asis' con ![](file.png){width=...})."
      echo "       2. En el Answerlist del enunciado, opciones de TEXTO: '* Gráfica I'…"
      echo "       3. En Solution citar el rótulo por contenido (regla #19), no la letra."
      echo "     (Distinto del SCHOICE puro: ahí las imágenes-opción sí funcionan.)"
    else
      echo "  ✓ FASE 2L: ninguna gráfica-opción dentro del gap CLOZE (V5 OK)"
    fi
  else
    echo "  ⊘ FASE 2L: el .Rmd no es CLOZE (V5 no aplica)"
  fi
fi

echo ""

# =============================================================================
# FASE 2M: AUDITORÍA VISUAL HTML MASIVA (recordatorio — ejecuta el workflow)
# =============================================================================
# NO corre el render masivo aquí (respeta el timeout del hook). La auditoría
# visual de varias decenas de HTMLs (móvil 360px + desktop 1024px) es un paso
# del workflow, automatizado por orquestador-{schoice,cloze} y
# revisar-{schoice,cloze}. Sirve para SCHOICE y CLOZE (el agente auto-detecta).
if [ -n "$RMD_FILE" ] && [ "$ERRORES_TOTALES" -eq 0 ]; then
  echo "┌───────────────────────────────────────────────────────────────┐"
  echo "│ FASE 2M: Auditoría visual HTML multi-semilla (schoice/cloze)  │"
  echo "└───────────────────────────────────────────────────────────────┘"
  echo "  Paso del workflow — ejecutar el auditor visual masivo:"
  echo "    /auditor-visual-html \"$RMD_FILE\" 24"
  echo "  (o agente subagent_type=\"auditor-visual-html\"). Renderiza N"
  echo "  versiones, captura móvil 360px + desktop 1024px y detecta fugas de"
  echo "  markup, math sin renderizar, desbordes/responsividad y anomalías."
  echo ""
fi

# =============================================================================
# FASE 2N: DETECCIÓN ESTÁTICA DE DIVERSIDAD COSMÉTICA (regla #22)
# =============================================================================
# Detección ESTÁTICA (solo grep — no ejecuta validar_diversidad_sustantiva.R).
# La validación dinámica exhaustiva vive en el orquestador (paso 9).
# Esta fase emite WARN_DIV_ESTATICA (NO bloqueante) cuando detecta señales
# estáticas de diversidad cosmética para recordar al desarrollador.
if [ -n "$RMD_FILE" ]; then
  echo "┌───────────────────────────────────────────────────────────────┐"
  echo "│ FASE 2N: Detección estática diversidad cosmética (regla #22) │"
  echo "└───────────────────────────────────────────────────────────────┘"
  # Extraer bloque data_generation (entre el primer ```{r data_generation y el cierre ```)
  DG_BLOCK=$(awk '/^```\{r[ ,].*data_generation/,/^```\s*$/' "$RMD_FILE" 2>/dev/null || true)
  if [ -z "$DG_BLOCK" ]; then
    # Fallback: primer bloque R del archivo
    DG_BLOCK=$(awk '/^```\{r/,/^```\s*$/' "$RMD_FILE" 2>/dev/null | head -80 || true)
  fi
  DIV_WARN=0
  # grep lee el bloque por here-string, no por `echo | grep -q`: con `set -o pipefail`, grep -q
  # sale en la primera coincidencia, echo recibe SIGPIPE en bloques de más de ~30 KB y la tubería
  # falla. Eso daba «sin aleatorización» falso y callaba la señal 1 (2026-10-27;
  # tests/testthat/test_post_exams2_pipefail.R).
  # Señal 1: file.copy( usado para PNGs que se referencian en el answerlist
  if grep -qE 'file\.copy\(' <<< "$DG_BLOCK"; then
    if grep -qE 'file\.copy\(.*\.png|\.png.*file\.copy' <<< "$DG_BLOCK"; then
      echo "  WARN_DIV_ESTATICA: se usa file.copy() para PNGs en data_generation."
      echo "    -> Los PNGs estáticos producen contenido idéntico en todas las semillas."
      echo "    -> Si son opciones gráficas, deben generarse dinámicamente (ggplot2/TikZ/reticulate)."
      echo "    -> Ejecutar: Rscript .claude/scripts/validar_diversidad_sustantiva.R \"$RMD_FILE\" --n 100"
      DIV_WARN=1
    fi
  fi
  # Señal 2: ausencia total de funciones de aleatorización en data_generation
  if ! grep -qE 'sample\(|runif\(|rnorm\(|rbinom\(|rpois\(|rexp\(|rgeom\(|rhyper\(|runif\(|pick_int\(|safe_sample\(' <<< "$DG_BLOCK"; then
    echo "  WARN_DIV_ESTATICA: no se detecta ninguna función de aleatorización en data_generation."
    echo "    -> Sin sample()/runif()/rnorm() la respuesta correcta puede ser invariante entre versiones."
    echo "    -> Ejecutar: Rscript .claude/scripts/validar_diversidad_sustantiva.R \"$RMD_FILE\" --n 100"
    DIV_WARN=1
  fi
  if [ "$DIV_WARN" -eq 0 ]; then
    echo "  ✓ FASE 2N: señales estáticas OK (file.copy PNG no detectado; aleatorización presente)."
    echo "    Nota: verificación sustantiva COMPLETA solo via validar_diversidad_sustantiva.R (orquestador paso 9)."
  fi
  ADVERTENCIAS_TOTALES=$((ADVERTENCIAS_TOTALES + DIV_WARN))
  echo ""
fi

# =============================================================================
# FASE 2O: GLIFOS UNICODE QUE ROMPEN pdflatex (ERR_GLIFO_LATEX)
# =============================================================================
# Un `✓` (U+2713) LITERAL en texto visible impide compilar el PDF:
#   ! LaTeX Error: Unicode character ✓ (U+2713) not set up for use with LaTeX
# El defecto es INVISIBLE en HTML (no pasa por LaTeX) y ningun validador del
# arsenal miraba los caracteres del fuente: por eso sobrevivio meses en
# ejercicios de 03-En-Produccion.
#
# DOS SEVERIDADES, CALIBRADAS midiendo los 63 .Rmd del repo que tenian glifos
# rompedores (render del original vs. del mismo archivo con los glifos
# sustituidos, para no atribuir al glifo un fallo ajeno):
#
#   glifo en texto MARKDOWN  -> ERR_GLIFO_LATEX  : 16/16 concluyentes rotos (100 %)
#   glifo en CODIGO R        -> WARN_GLIFO_LATEX : 1/24 rotos (96 % falsos positivos)
#   glifo en COMENTARIO R    -> informativo      : medido inocuo (compila)
#
# Bloquear tambien en codigo R habria marcado 23 archivos que SI compilan: la
# cadena puede no emitirse nunca. Por eso avisa pero no suma error.
#
# POR QUE BLOQUEAR EN MARKDOWN NO PUEDE "ROMPER UN RENDER": este hook corre
# DESPUES del render, no puede impedirlo, solo marcar el resultado. Su valor es
# cazar el caso en que solo se hizo exams2html() -- que NO falla -- y el defecto
# quedaria latente. Que es exactamente lo que paso.
#
# Detector unico compartido con tests/testthat/test_glifos_latex.R.

# OJO: se usa $RMD_PATH (ABSOLUTA), no $RMD_FILE (relativa al cwd del comando).
# El hook no hace cd al cwd, así que `-f "$RMD_FILE"` es falso siempre que el
# comando use ruta relativa — la fase entera quedaría muda sin que nada avisara.
if [ -f "$RMD_PATH" ]; then
  echo "┌───────────────────────────────────────────────────────────────┐"
  echo "│ FASE 2O: Glifos que rompen pdflatex (ERR_GLIFO_LATEX)         │"
  echo "└───────────────────────────────────────────────────────────────┘"

  DETECTOR_GLIFOS="$PROJECT_DIR/.claude/scripts/validar_glifos_latex.R"
  if [ -f "$DETECTOR_GLIFOS" ]; then
    GLIFOS_OUT=$(Rscript --vanilla "$DETECTOR_GLIFOS" "$RMD_PATH" 2>&1)
    GLIFOS_EXIT=$?
    if [ $GLIFOS_EXIT -eq 1 ]; then
      ERRORES_TOTALES=$((ERRORES_TOTALES + 1))
      echo "  ❌ ERR_GLIFO_LATEX (bloqueante) — glifo en texto Markdown visible"
      echo "$GLIFOS_OUT" | grep -v '^$' | sed 's/^/     /'
      echo "     Ver .claude/rules/glifos-latex-prohibidos.md"
    elif [ $GLIFOS_EXIT -eq 0 ]; then
      # Here-string por la misma razón que en la 2N: con una salida de más de ~30 KB, `echo | grep -q`
      # fallaba por SIGPIPE y la 2O decía «sin glifos» (2026-10-28).
      if grep -q "WARN_GLIFO_LATEX" <<< "$GLIFOS_OUT"; then
        ADVERTENCIAS_TOTALES=$((ADVERTENCIAS_TOTALES + 1))
        echo "$GLIFOS_OUT" | grep -v '^$' | sed 's/^/  ⚠️   /'
      elif grep -q "INFO_GLIFO_COMENTARIO" <<< "$GLIFOS_OUT"; then
        echo "$GLIFOS_OUT" | grep INFO_GLIFO_COMENTARIO | sed 's/^/  ⊘ /'
        echo "     No suma nada: los comentarios R no llegan a LaTeX (medido)."
      else
        echo "  ✓ FASE 2O: sin glifos que rompan pdflatex"
      fi
    else
      echo "  ⊘ FASE 2O: el detector no pudo analizar el archivo (exit $GLIFOS_EXIT)"
    fi
  else
    echo "  ⊘ FASE 2O: detector no encontrado en $DETECTOR_GLIFOS"
  fi
  echo ""
fi

# =============================================================================
# RESUMEN FINAL Y ACCIONES OBLIGATORIAS
# =============================================================================

echo "╔═══════════════════════════════════════════════════════════════╗"
echo "║  RESUMEN FINAL DEL ARSENAL                                    ║"
echo "╠═══════════════════════════════════════════════════════════════╣"
printf "║  Errores totales:      %-37s║\n" "$ERRORES_TOTALES"
printf "║  Advertencias totales: %-37s║\n" "$ADVERTENCIAS_TOTALES"
echo "╚═══════════════════════════════════════════════════════════════╝"
echo ""

if [ $ERRORES_TOTALES -gt 0 ]; then
  echo "╔═══════════════════════════════════════════════════════════════╗"
  echo "║  ❌ VALIDACIÓN FALLIDA - CORRECCIÓN OBLIGATORIA               ║"
  echo "╠═══════════════════════════════════════════════════════════════╣"
  echo "║  Claude DEBE:                                                 ║"
  echo "║  1. Corregir TODOS los errores reportados arriba              ║"
  echo "║  2. Volver a renderizar                                       ║"
  echo "║  3. El arsenal se ejecutará automáticamente de nuevo          ║"
  echo "║                                                               ║"
  echo "║  PROHIBIDO: Continuar sin resolver errores                    ║"
  echo "║  PROHIBIDO: Marcar como 'completado' con errores              ║"
  echo "╚═══════════════════════════════════════════════════════════════╝"
elif [ $ADVERTENCIAS_TOTALES -gt 0 ]; then
  echo "╔═══════════════════════════════════════════════════════════════╗"
  echo "║  ⚠️  VALIDACIÓN CON ADVERTENCIAS                              ║"
  echo "╠═══════════════════════════════════════════════════════════════╣"
  echo "║  Claude DEBE:                                                 ║"
  echo "║  1. Revisar TODAS las advertencias                            ║"
  echo "║  2. Ejecutar Read() sobre CADA preview PNG generado           ║"
  echo "║  3. Verificar las 5 coherencias VISUALMENTE                   ║"
  echo "║  4. Documentar hallazgos con checklist                        ║"
  echo "║  5. Solicitar aprobación del usuario                          ║"
  echo "╚═══════════════════════════════════════════════════════════════╝"
else
  echo "╔═══════════════════════════════════════════════════════════════╗"
  echo "║  ✓ ARSENAL COMPLETO APROBADO                                  ║"
  echo "╠═══════════════════════════════════════════════════════════════╣"
  echo "║  Claude DEBE aún:                                             ║"
  echo "║  1. Ejecutar Read() sobre CADA preview PNG                    ║"
  echo "║  2. Verificar las 5 coherencias VISUALMENTE                   ║"
  echo "║  3. Documentar con checklist                                  ║"
  echo "║  4. Solicitar aprobación del usuario                          ║"
  echo "║                                                               ║"
  echo "║  PROHIBIDO: Decir 'todo correcto' sin mostrar imágenes        ║"
  echo "╚═══════════════════════════════════════════════════════════════╝"
fi

echo ""
echo "───────────────────────────────────────────────────────────────"
echo "Previews a inspeccionar:"
ls -1 "${CWD}/preview_${RMD_BASENAME}"*.png 2>/dev/null || echo "  (ninguno generado)"
echo "───────────────────────────────────────────────────────────────"
echo ""

# =============================================================================
# NUDGE: SIGUIENTE PASO DEL WORKFLOW (si existe ejercicio_state.json)
# =============================================================================
STATE_FILE="${CWD}/ejercicio_state.json"
if [ -f "$STATE_FILE" ]; then
  # Auto-marcar renderizado y arsenal como completados
  WORKFLOW_SCRIPT="$PROJECT_DIR/.claude/scripts/workflow-state.sh"
  if [ -f "$WORKFLOW_SCRIPT" ] && [ $ERRORES_TOTALES -eq 0 ]; then
    bash "$WORKFLOW_SCRIPT" complete "$CWD" renderizado_4_formatos 2>/dev/null
    bash "$WORKFLOW_SCRIPT" complete "$CWD" arsenal_post_render 2>/dev/null
  fi

  NEXT_STEP=$(bash "$WORKFLOW_SCRIPT" next "$CWD" 2>/dev/null)
  if [ -n "$NEXT_STEP" ]; then
    echo "╔═══════════════════════════════════════════════════════════════╗"
    echo "║  SIGUIENTE PASO OBLIGATORIO DEL WORKFLOW                     ║"
    echo "╠═══════════════════════════════════════════════════════════════╣"
    printf "║  %-60s║\n" "$NEXT_STEP"
    echo "╚═══════════════════════════════════════════════════════════════╝"
    echo ""
  fi
fi

exit 0
