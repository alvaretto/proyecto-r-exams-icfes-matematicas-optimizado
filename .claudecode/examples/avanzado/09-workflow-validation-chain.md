# Workflow Avanzado: Cadena de Validación Automática (Chaining)

**Nivel**: Avanzado  
**Tipo**: Agent Chaining - Workflow Completo  
**Propósito**: Validar y corregir archivos .Rmd automáticamente hasta renderizado exitoso

---

## Definición del Workflow

```yaml
# .claudecode/workflows/validation_chain.yml
name: "Cadena de Validación Automática ICFES"
description: "Workflow completo: validar → corregir → renderizar → iterar hasta éxito"
version: "1.0.0"

steps:
  - name: "Validación Inicial"
    agent: "validator_icfes"
    type: "validation"
    inputs:
      file_path: "{{input_file}}"
    outputs:
      validation_report: "validation_report.json"
  
  - name: "Corrección Automática"
    agent: "auto_fixer_icfes"
    type: "fix"
    condition: "validation_report.errors > 0"
    inputs:
      file_path: "{{input_file}}"
      validation_report: "{{validation_report}}"
    outputs:
      fixed_file: "{{input_file}}.fixed"
      fix_report: "fix_report.json"
  
  - name: "Renderizado de Prueba"
    skill: "render_validator"
    type: "execution"
    inputs:
      rmd_file: "{{fixed_file|default:input_file}}"
      output_dir: "/tmp/rexams_validation_{{timestamp}}"
    outputs:
      render_status: "render_status.json"
      error_log: "error_log.txt"
  
  - name: "Análisis de Errores"
    skill: "error_analyzer"
    type: "analysis"
    condition: "render_status.status == 'FAILED'"
    inputs:
      error_log_path: "{{error_log}}"
      rmd_file_path: "{{fixed_file|default:input_file}}"
    outputs:
      error_analysis: "error_analysis.json"
  
  - name: "Corrección Dirigida"
    agent: "auto_fixer_icfes"
    type: "fix"
    condition: "error_analysis.error_type != 'UNKNOWN'"
    inputs:
      file_path: "{{fixed_file|default:input_file}}"
      error_analysis: "{{error_analysis}}"
      fix_mode: "targeted"
    outputs:
      fixed_file: "{{fixed_file}}.fixed2"
      fix_report: "fix_report2.json"
  
  - name: "Validación Final"
    skill: "render_validator"
    type: "execution"
    condition: "fixed_file exists"
    inputs:
      rmd_file: "{{fixed_file}}.fixed2"
      output_dir: "/tmp/rexams_validation_final_{{timestamp}}"
    outputs:
      final_status: "final_status.json"

max_iterations: 5
on_complete: "generate_summary_report"
on_failure: "save_debug_info"
```

---

## Implementación del Workflow

```bash
#!/bin/bash
# .claudecode/workflows/validation_chain.sh
# Ejecutor principal de la cadena de validación

set -euo pipefail

# Configuración
RMD_FILE="${1:-}"
MAX_ITERATIONS=5
ITERATION=0
CURRENT_FILE="$RMD_FILE"
WORK_DIR="/tmp/rexams_validation_workflow_$$"
VALIDATION_REPORT=""
FIX_REPORT=""
ERROR_ANALYSIS=""

# Colores
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Crear directorio de trabajo
mkdir -p "$WORK_DIR"

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "🔗 CADENA DE VALIDACIÓN AUTOMÁTICA ICFES"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Archivo: $RMD_FILE"
echo "Directorio de trabajo: $WORK_DIR"
echo ""

# Función de limpieza
cleanup() {
    if [ -f "${CURRENT_FILE}.backup" ]; then
        rm -f "${CURRENT_FILE}.backup"
    fi
}

trap cleanup EXIT

# PASO 1: Validación Inicial
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}PASO 1: Validación Inicial${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"

VALIDATION_REPORT="${WORK_DIR}/validation_report.json"

# Ejecutar validador (simulado - integrar con agente real)
if bash .claudecode/examples/basico/01-pre-commit-yaml-validator.sh "$CURRENT_FILE" > "${WORK_DIR}/validation_stdout.txt" 2>&1; then
    echo -e "${GREEN}✓ Validación inicial pasada${NC}"
    VALIDATION_STATUS="PASSED"
else
    echo -e "${YELLOW}⚠ Validación inicial encontró problemas${NC}"
    VALIDATION_STATUS="FAILED"
fi

# Generar reporte JSON simulado
cat > "$VALIDATION_REPORT" <<EOF
{
  "status": "$VALIDATION_STATUS",
  "errors": 0,
  "warnings": $(grep -c "⚠" "${WORK_DIR}/validation_stdout.txt" || echo 0),
  "file": "$CURRENT_FILE",
  "timestamp": "$(date -Iseconds)"
}
EOF

# PASO 2: Corrección Automática (si hay errores)
if [ "$VALIDATION_STATUS" = "FAILED" ]; then
    echo ""
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}PASO 2: Corrección Automática${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    
    # Crear backup
    cp "$CURRENT_FILE" "${CURRENT_FILE}.backup"
    
    # Aquí se integraría con el agente corrector automático
    # Por ahora, simulación:
    echo -e "${YELLOW}⚠ Ejecutando corrección automática...${NC}"
    echo -e "${BLUE}💡 En producción, aquí se ejecutaría el agente auto_fixer_icfes${NC}"
    
    FIXED_FILE="${CURRENT_FILE}.fixed"
    cp "$CURRENT_FILE" "$FIXED_FILE"
    CURRENT_FILE="$FIXED_FILE"
fi

# BUCLE PRINCIPAL: Validar renderizado iterativamente
ITERATION=0
RENDER_SUCCESS=false

while [ $ITERATION -lt $MAX_ITERATIONS ] && [ "$RENDER_SUCCESS" = false ]; do
    ITERATION=$((ITERATION + 1))
    
    echo ""
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}PASO 3.$ITERATION: Renderizado de Prueba (Iteración $ITERATION/$MAX_ITERATIONS)${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    
    RENDER_DIR="${WORK_DIR}/render_${ITERATION}"
    mkdir -p "$RENDER_DIR"
    
    # Ejecutar renderizado
    if bash .claudecode/examples/avanzado/07-skill-render-validator.sh "$CURRENT_FILE" "$RENDER_DIR" > "${RENDER_DIR}/render_output.txt" 2>&1; then
        echo -e "${GREEN}✅ Renderizado exitoso en iteración $ITERATION${NC}"
        RENDER_SUCCESS=true
        break
    else
        echo -e "${RED}❌ Renderizado falló en iteración $ITERATION${NC}"
        
        # PASO 4: Análisis de Errores
        echo ""
        echo -e "${BLUE}PASO 4.$ITERATION: Análisis de Errores${NC}"
        
        ERROR_LOG="${RENDER_DIR}/render_errors.txt"
        ERROR_ANALYSIS="${RENDER_DIR}/error_analysis.json"
        
        if [ -f "$ERROR_LOG" ]; then
            # Usar skill de análisis de errores (simulado)
            # En producción, esto llamaría al skill error_analyzer
            echo -e "${YELLOW}⚠ Analizando errores...${NC}"
            
            # Extraer tipo de error básico del log
            if grep -qi "latex.*error" "$ERROR_LOG"; then
                ERROR_TYPE="LATEX_ERROR"
            elif grep -qi "package.*not found" "$ERROR_LOG"; then
                ERROR_TYPE="PACKAGE_MISSING"
            elif grep -qi "syntax.*error" "$ERROR_LOG"; then
                ERROR_TYPE="R_SYNTAX"
            else
                ERROR_TYPE="UNKNOWN"
            fi
            
            cat > "$ERROR_ANALYSIS" <<EOF
{
  "error_type": "$ERROR_TYPE",
  "error_message": "$(head -1 "$ERROR_LOG" | cut -c1-200)",
  "location": {
    "file": "$CURRENT_FILE",
    "line_number": null
  },
  "severity": "ERROR",
  "suggested_fix": []
}
EOF
            
            echo -e "${BLUE}📋 Tipo de error identificado: $ERROR_TYPE${NC}"
        fi
        
        # PASO 5: Corrección Dirigida (solo si no es última iteración)
        if [ $ITERATION -lt $MAX_ITERATIONS ] && [ -f "$ERROR_ANALYSIS" ]; then
            echo ""
            echo -e "${BLUE}PASO 5.$ITERATION: Corrección Dirigida${NC}"
            
            # Aquí se integraría corrección dirigida basada en error_analysis
            echo -e "${YELLOW}⚠ Aplicando correcciones dirigidas...${NC}"
            echo -e "${BLUE}💡 En producción, aquí se ejecutaría corrección específica según error_type${NC}"
            
            # Crear nueva versión corregida
            NEW_FIXED="${CURRENT_FILE}.iter${ITERATION}"
            cp "$CURRENT_FILE" "$NEW_FIXED"
            CURRENT_FILE="$NEW_FIXED"
        fi
    fi
done

# RESUMEN FINAL
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📊 RESUMEN DE VALIDACIÓN"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if [ "$RENDER_SUCCESS" = true ]; then
    echo -e "${GREEN}✅ ÉXITO: Archivo renderizado correctamente${NC}"
    echo "Archivo final: $CURRENT_FILE"
    echo "Iteraciones necesarias: $ITERATION"
    exit 0
else
    echo -e "${RED}❌ FALLO: No se pudo renderizar después de $MAX_ITERATIONS iteraciones${NC}"
    echo "Archivo con errores: $CURRENT_FILE"
    echo "Logs disponibles en: $WORK_DIR"
    exit 1
fi

```

---

## Uso del Workflow

### Ejecución Manual
```bash
# Ejecutar cadena completa de validación
bash .claudecode/workflows/validation_chain.sh ejercicio.Rmd
```

### Integración con Git Pre-commit Hook
```bash
#!/bin/bash
# .git/hooks/pre-commit

# Ejecutar cadena de validación en archivos .Rmd staged
for file in $(git diff --cached --name-only --diff-filter=ACM | grep '\.Rmd$'); do
    if ! bash .claudecode/workflows/validation_chain.sh "$file"; then
        echo "❌ Validación falló para $file"
        exit 1
    fi
done
```

### Integración con CI/CD
```yaml
# .github/workflows/validate-rmd.yml (GitHub Actions)
name: Validate R-Exams Files

on:
  pull_request:
    paths:
      - '**/*.Rmd'

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      
      - name: Setup R
        uses: r-lib/actions/setup-r@v1
        with:
          r-version: '4.2'
      
      - name: Install R dependencies
        run: |
          Rscript -e "install.packages(c('exams', 'rmarkdown'))"
      
      - name: Run validation chain
        run: |
          for file in $(git diff --name-only origin/main...HEAD | grep '\.Rmd$'); do
            bash .claudecode/workflows/validation_chain.sh "$file"
          done
```

---

## Características del Workflow

1. **Validación Inicial**: Detecta errores obvios antes de renderizar
2. **Corrección Automática**: Aplica correcciones conocidas automáticamente
3. **Renderizado Iterativo**: Intenta renderizar y captura errores
4. **Análisis Inteligente**: Analiza errores y clasifica por tipo
5. **Corrección Dirigida**: Aplica correcciones específicas según tipo de error
6. **Límite de Iteraciones**: Evita bucles infinitos
7. **Reportes Detallados**: Genera logs y reportes en cada paso

---

## Extensión del Workflow

Para agregar nuevos tipos de corrección automática:

1. Agregar patrón de error en `error_analyzer.py`
2. Agregar lógica de corrección en `auto_fixer_icfes`
3. El workflow automáticamente usará la nueva corrección
