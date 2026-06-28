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
# FASE 2I: Detección \pandocbounded en .tex y `![]()` sin width en .Rmd (Errores 16-17)
# FASE 2J: Letter-independence en Solution (Error 19, regla #19)
# FASE 2K: Guard contador 'none' para tablas (Error 21, regla #20)
# FASE 2L: Gráficas-opción en gap CLOZE (V5, Incidente G, regla graficos-como-opciones)
# FASE 2M: Auditoría visual HTML masiva (recordatorio — workflow)
# FASE 2N: Detección ESTÁTICA de diversidad cosmética (regla #22 — WARN_DIV_ESTATICA)
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
TEX_FILES=$(find "$CWD" -maxdepth 4 -name "*.tex" -newer "$RMD_FILE" 2>/dev/null)
if [ -n "$TEX_FILES" ]; then
  for tex in $TEX_FILES; do
    if grep -l 'pandocbounded' "$tex" >/dev/null 2>&1; then
      PANDOC_BUG_FOUND=1
      ERRORES_TOTALES=$((ERRORES_TOTALES + 1))
      echo "  ❌ ERROR 16: \\pandocbounded encontrado en $(basename "$tex")"
    fi
  done
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
  # Señal 1: file.copy( usado para PNGs que se referencian en el answerlist
  if echo "$DG_BLOCK" | grep -qE 'file\.copy\('; then
    if echo "$DG_BLOCK" | grep -qE 'file\.copy\(.*\.png|\.png.*file\.copy'; then
      echo "  WARN_DIV_ESTATICA: se usa file.copy() para PNGs en data_generation."
      echo "    -> Los PNGs estáticos producen contenido idéntico en todas las semillas."
      echo "    -> Si son opciones gráficas, deben generarse dinámicamente (ggplot2/TikZ/reticulate)."
      echo "    -> Ejecutar: Rscript .claude/scripts/validar_diversidad_sustantiva.R \"$RMD_FILE\" --n 40"
      DIV_WARN=1
    fi
  fi
  # Señal 2: ausencia total de funciones de aleatorización en data_generation
  if ! echo "$DG_BLOCK" | grep -qE 'sample\(|runif\(|rnorm\(|rbinom\(|rpois\(|rexp\(|rgeom\(|rhyper\(|runif\(|pick_int\(|safe_sample\('; then
    echo "  WARN_DIV_ESTATICA: no se detecta ninguna función de aleatorización en data_generation."
    echo "    -> Sin sample()/runif()/rnorm() la respuesta correcta puede ser invariante entre versiones."
    echo "    -> Ejecutar: Rscript .claude/scripts/validar_diversidad_sustantiva.R \"$RMD_FILE\" --n 40"
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
