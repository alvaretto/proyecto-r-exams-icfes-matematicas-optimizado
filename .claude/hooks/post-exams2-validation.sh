#!/bin/bash
# =============================================================================
# post-exams2-validation.sh (v6.0)
# Hook PostToolUse: ARSENAL COMPLETO DE VALIDACIONES POST-RENDERIZADO
#
# Se activa AUTOMÁTICAMENTE después de CADA comando Bash que contenga exams2*
# Ejecuta TODAS las validaciones SIN EXCEPCIÓN:
#
# FASE 2A: Coherencia Matemática (.R script)
# FASE 2B: Preview Visual (PDF → PNG)
# FASE 2C: Opciones Únicas (gráficos diferentes)
# FASE 2D: Ortografía Española (tildes)
# FASE 2E: Metadatos ICFES (6 dimensiones)
# FASE 2F: Estructura Metacognitiva (Solution completa)
# FASE 2G: Multi-semilla rápida (20 semillas, Nivel 5)
# FASE 2H: Stress Test Visual (10 semillas, renderizado real + PNGs)
#
# GARANTÍA: Toda renderización activa TODAS las fases
# PERMANENTE: No hay forma de saltarse estas validaciones
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

# Solo proceder si el comando contiene exams2
if ! echo "$COMMAND" | grep -q 'exams2'; then
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

# Construir ruta completa del .Rmd
if [[ "$RMD_FILE" = /* ]]; then
  RMD_PATH="$RMD_FILE"
else
  RMD_PATH="$CWD/$RMD_FILE"
fi

if [ ! -f "$RMD_PATH" ]; then
  exit 0
fi

RMD_BASENAME=$(basename "$RMD_FILE" .Rmd)

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

  if [ $ARSENAL_EXIT -ne 0 ]; then
    ERRORES_TOTALES=$((ERRORES_TOTALES + 1))
  fi

  # Extraer conteos del arsenal
  ARSENAL_ERRORES=$(echo "$ARSENAL_OUTPUT" | grep "Total ERRORES:" | grep -oP '\d+' | tail -1)
  ARSENAL_ADVERTENCIAS=$(echo "$ARSENAL_OUTPUT" | grep "Total ADVERTENCIAS:" | grep -oP '\d+' | tail -1)

  if [ -n "$ARSENAL_ERRORES" ]; then
    ERRORES_TOTALES=$((ERRORES_TOTALES + ARSENAL_ERRORES))
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
  echo "│ FASE 2G: Multi-semilla rápida (20 semillas, Nivel 5)          │"
  echo "└───────────────────────────────────────────────────────────────┘"

  MULTISEED_OUTPUT=$(cd "$CWD" && Rscript "$SCRIPT_MULTISEMILLA" "$RMD_FILE" --n 20 2>&1)
  MULTISEED_EXIT=$?

  echo "$MULTISEED_OUTPUT" | grep -E "Semillas|Fallos|Tasa|RESULTADO|ERR_ANS|ERR_SEM"

  if [ $MULTISEED_EXIT -ne 0 ]; then
    ERRORES_TOTALES=$((ERRORES_TOTALES + 1))
    echo "  ❌ Multi-semilla: FALLOS detectados en alguna(s) semilla(s)"
  else
    echo "  ✓ Multi-semilla: 20/20 semillas aprobadas"
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
