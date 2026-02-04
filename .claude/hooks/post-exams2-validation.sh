#!/bin/bash
# =============================================================================
# post-exams2-validation.sh (v4.1)
# Hook PostToolUse para Bash: detecta exams2* y lanza validación automática
#
# FASE 2A: Validación de coherencia matemática (.R script)
# FASE 2B: Generación automática de preview visual (PDF → PNG)
#
# Se activa DESPUÉS de cada comando Bash exitoso.
# Si el comando contiene exams2*, extrae el archivo .Rmd y ejecuta:
#   1. validar_coherencia_matematica.R (coherencia matemática)
#   2. Conversión PDF → PNG para inspección visual obligatoria
#
# Entrada: JSON en stdin con tool_input.command, cwd, etc.
# Salida: Reporte de validación + ruta de preview PNG
#
# NOTA: Usa Python para parsear JSON (jq no disponible en este sistema)
# =============================================================================

# Leer JSON de stdin
INPUT=$(cat)

# Extraer el comando ejecutado usando Python (jq no disponible)
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

# Solo proceder si el comando contiene exams2
if ! echo "$COMMAND" | grep -q 'exams2'; then
  exit 0
fi

# Extraer directorio de trabajo usando Python
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
# Patrones comunes: exams2html("archivo.Rmd", ...) o exams2pdf("archivo.Rmd", ...)
RMD_FILE=$(echo "$COMMAND" | grep -oP 'exams2\w+\(\s*"[^"]*\.Rmd"' | head -1 | grep -oP '"[^"]*\.Rmd"' | tr -d '"')

if [ -z "$RMD_FILE" ]; then
  # Intentar con comillas simples
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

SCRIPT="$PROJECT_DIR/.claude/scripts/validar_coherencia_matematica.R"

if [ ! -f "$SCRIPT" ]; then
  exit 0
fi

# Construir ruta completa del .Rmd
if [[ "$RMD_FILE" = /* ]]; then
  RMD_PATH="$RMD_FILE"
else
  RMD_PATH="$CWD/$RMD_FILE"
fi

if [ ! -f "$RMD_PATH" ]; then
  exit 0
fi

# =====================================================================
# FASE 2A: VALIDACIÓN DE COHERENCIA MATEMÁTICA
# =====================================================================

VALIDATION_OUTPUT=$(cd "$CWD" && Rscript "$SCRIPT" "$RMD_FILE" 2>&1)
MATH_EXIT_CODE=$?

echo "═══════════════════════════════════════════════════════════════"
echo "  HOOK POST-EXAMS2: VALIDACIÓN AUTOMÁTICA"
echo "═══════════════════════════════════════════════════════════════"
echo ""

if [ $MATH_EXIT_CODE -eq 0 ]; then
  echo "FASE 2A — Coherencia matemática: APROBADO"
  echo "$VALIDATION_OUTPUT" | tail -5
else
  echo "FASE 2A — Coherencia matemática: ERRORES DETECTADOS"
  echo "$VALIDATION_OUTPUT"
  echo ""
  echo "ACCIÓN REQUERIDA: Corregir errores matemáticos antes de continuar."
  echo "═══════════════════════════════════════════════════════════════"
  exit 0
fi

# =====================================================================
# FASE 2B: GENERACIÓN DE PREVIEW VISUAL (PDF → PNG)
# =====================================================================

echo ""
echo "───────────────────────────────────────────────────────────────"
echo "FASE 2B — Validación visual automática"
echo "───────────────────────────────────────────────────────────────"

# Extraer directorio de salida PDF del comando (parámetro dir = "...")
PDF_DIR=$(echo "$COMMAND" | grep -oP 'dir\s*=\s*"[^"]*"' | head -1 | grep -oP '"[^"]*"' | tr -d '"')

if [ -z "$PDF_DIR" ]; then
  PDF_DIR=$(echo "$COMMAND" | grep -oP "dir\s*=\s*'[^']*'" | head -1 | grep -oP "'[^']*'" | tr -d "'")
fi

# Buscar PDFs en ubicaciones conocidas (orden de prioridad)
PDF_FOUND=""
SEARCH_DIRS=()

# 1. Directorio explícito del comando
if [ -n "$PDF_DIR" ]; then
  if [[ "$PDF_DIR" = /* ]]; then
    SEARCH_DIRS+=("$PDF_DIR")
  else
    SEARCH_DIRS+=("$CWD/$PDF_DIR")
  fi
fi

# 2. Directorios estándar de exams2pdf
SEARCH_DIRS+=("$CWD/output_pdf" "$CWD/output" "$CWD")

# Buscar el PDF más reciente
for DIR in "${SEARCH_DIRS[@]}"; do
  if [ -d "$DIR" ]; then
    CANDIDATE=$(find "$DIR" -maxdepth 1 -name "*.pdf" -type f 2>/dev/null | head -1)
    if [ -n "$CANDIDATE" ] && [ -f "$CANDIDATE" ]; then
      PDF_FOUND="$CANDIDATE"
      break
    fi
  fi
done

if [ -z "$PDF_FOUND" ]; then
  echo "No se encontró PDF generado."
  echo ""
  echo "ACCIÓN OBLIGATORIA para Claude:"
  echo "  1. Ejecutar exams2pdf() para generar el PDF"
  echo "  2. El hook generará automáticamente el preview PNG"
  echo "  3. Verificar las 5 coherencias visualmente"
  echo "═══════════════════════════════════════════════════════════════"
  exit 0
fi

# Generar nombre del preview basado en el .Rmd
RMD_BASENAME=$(basename "$RMD_FILE" .Rmd)
PREVIEW_PNG="$CWD/preview_${RMD_BASENAME}.png"

# Convertir PDF → PNG con magick
if command -v magick &>/dev/null; then
  magick -density 150 "$PDF_FOUND" -quality 90 "$PREVIEW_PNG" 2>/dev/null
  MAGICK_EXIT=$?

  if [ $MAGICK_EXIT -eq 0 ]; then
    # Listar PNGs generados
    PREVIEW_FILES=$(ls -1 "${CWD}/preview_${RMD_BASENAME}"*.png 2>/dev/null)
    NUM_PREVIEWS=$(echo "$PREVIEW_FILES" | wc -l)

    echo "Preview generado: $NUM_PREVIEWS página(s)"
    echo ""
    echo "$PREVIEW_FILES"
    echo ""
    echo "═══════════════════════════════════════════════════════════════"
    echo "OBLIGATORIO — Claude DEBE ejecutar AHORA:"
    echo ""
    echo "  1. Read() cada archivo PNG listado arriba"
    echo "  2. Verificar las 5 coherencias VISUALMENTE:"
    echo "     - Semántica: Tildes, gramática, redacción"
    echo "     - Visual-Texto: Valores coinciden con enunciado"
    echo "     - Matemática: Fórmulas y cálculos correctos"
    echo "     - Código: Elementos dinámicos funcionan"
    echo "     - General: Legible, estilo ICFES, opciones visibles"
    echo "  3. Documentar hallazgos con checklist"
    echo "  4. Solicitar aprobación del usuario"
    echo ""
    echo "  PROHIBIDO continuar sin inspección visual"
    echo "  PROHIBIDO decir 'se generó correctamente' sin mostrar imagen"
    echo "═══════════════════════════════════════════════════════════════"
  else
    echo "Error al convertir PDF a PNG con magick"
    echo "  PDF: $PDF_FOUND"
    echo "═══════════════════════════════════════════════════════════════"
  fi
else
  echo "magick no está instalado en el sistema"
  echo ""
  echo "ACCIÓN OBLIGATORIA para Claude:"
  echo "  1. Convertir manualmente: magick -density 150 '$PDF_FOUND' -quality 90 preview.png"
  echo "  2. Read(preview.png) para inspección visual"
  echo "  3. Verificar las 5 coherencias"
  echo "═══════════════════════════════════════════════════════════════"
fi

exit 0
