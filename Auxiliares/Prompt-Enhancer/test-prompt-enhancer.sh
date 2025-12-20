#!/bin/bash

# ============================================================================
# TEST PROMPT ENHANCER - Script de Prueba
# ============================================================================
# Descripción: Prueba el funcionamiento del Prompt Enhancer desde diferentes
#              ubicaciones y con diferentes opciones
# ============================================================================

set -euo pipefail

# Colores
readonly GREEN='\033[0;32m'
readonly RED='\033[0;31m'
readonly YELLOW='\033[1;33m'
readonly CYAN='\033[0;36m'
readonly NC='\033[0m'

# Ruta al script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROMPT_ENHANCER="$SCRIPT_DIR/prompt-enhancer.sh"

echo -e "${CYAN}╔════════════════════════════════════════════════════════════════╗${NC}"
echo -e "${CYAN}║         TEST PROMPT ENHANCER - Pruebas Funcionales            ║${NC}"
echo -e "${CYAN}╚════════════════════════════════════════════════════════════════╝${NC}\n"

# Contador de pruebas
total_tests=0
passed_tests=0
failed_tests=0

# ============================================================================
# FUNCIÓN: Ejecutar prueba
# ============================================================================
run_test() {
    local test_name="$1"
    local test_command="$2"

    total_tests=$((total_tests + 1))
    echo -e "${YELLOW}Prueba $total_tests: $test_name${NC}"

    if eval "$test_command" >/dev/null 2>&1; then
        echo -e "${GREEN}✓ PASÓ${NC}\n"
        passed_tests=$((passed_tests + 1))
        return 0
    else
        echo -e "${RED}✗ FALLÓ${NC}"
        echo -e "${YELLOW}Comando: $test_command${NC}\n"
        failed_tests=$((failed_tests + 1))
        return 1
    fi
}

# ============================================================================
# PRUEBAS
# ============================================================================

echo -e "${CYAN}=== PRUEBAS BÁSICAS ===${NC}\n"

# Prueba 1: Script ejecutable
run_test "Script es ejecutable" "test -x '$PROMPT_ENHANCER'"

# Prueba 2: Ayuda funciona
run_test "Opción --help funciona" "'$PROMPT_ENHANCER' --help"

# Prueba 3: Prompt directo
run_test "Prompt directo funciona" "'$PROMPT_ENHANCER' 'Prueba básica'"

echo -e "${CYAN}=== PRUEBAS DE CONTEXTO ===${NC}\n"

# Prueba 4: Detecta raíz del proyecto
run_test "Detecta raíz del proyecto" "'$PROMPT_ENHANCER' 'Test' 2>&1 | grep -q 'Raíz del proyecto encontrada'"

# Prueba 5: Incluye reglas de .augment
run_test "Incluye reglas de .augment/rules/" "'$PROMPT_ENHANCER' 'Test' 2>&1 | grep -q 'REGLAS GENERALES DEL PROYECTO'"

# Prueba 6: Incluye documentación de .claude
run_test "Incluye documentación de .claude/" "'$PROMPT_ENHANCER' 'Test' 2>&1 | grep -q 'DOCUMENTACIÓN TÉCNICA'"

# Prueba 7: Incluye guía de estilo de .claudedoc
run_test "Incluye guía de estilo de .claudedoc/" "'$PROMPT_ENHANCER' 'Test' 2>&1 | grep -q 'GUÍA DE ESTILO ICFES'"

# Prueba 8: Lista skills disponibles
run_test "Lista skills disponibles" "'$PROMPT_ENHANCER' 'Test' 2>&1 | grep -q 'Skills Disponibles'"

# Prueba 9: Lista comandos disponibles
run_test "Lista comandos disponibles" "'$PROMPT_ENHANCER' 'Test' 2>&1 | grep -q 'Comandos Disponibles'"

# Prueba 10: Lista ejemplos funcionales
run_test "Lista ejemplos funcionales" "'$PROMPT_ENHANCER' 'Test' 2>&1 | grep -q 'EJEMPLOS FUNCIONALES DISPONIBLES'"

echo -e "${CYAN}=== PRUEBAS DE OPCIONES ===${NC}\n"

# Prueba 11: Guardar en archivo
run_test "Opción -o (guardar en archivo)" "'$PROMPT_ENHANCER' 'Test' -o /tmp/test_prompt.txt && test -f /tmp/test_prompt.txt && rm /tmp/test_prompt.txt"

# Prueba 12: Leer desde archivo
run_test "Opción -f (leer desde archivo)" "echo 'Test desde archivo' > /tmp/test_input.txt && '$PROMPT_ENHANCER' -f /tmp/test_input.txt > /dev/null && rm /tmp/test_input.txt"

echo -e "${CYAN}=== PRUEBAS DE PORTABILIDAD ===${NC}\n"

# Prueba 13: Funciona desde subcarpeta
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
run_test "Funciona desde subcarpeta" "cd '$PROJECT_ROOT/A-Produccion' && '$PROMPT_ENHANCER' 'Test desde subcarpeta' 2>&1 | grep -q 'A-Produccion'"

# Prueba 14: Detecta contexto de producción
run_test "Detecta contexto de producción" "cd '$PROJECT_ROOT/A-Produccion/En-Produccion' && '$PROMPT_ENHANCER' 'Test' 2>&1 | grep -q 'produccion'"

# Prueba 15: Detecta contexto de desarrollo
run_test "Detecta contexto de desarrollo" "cd '$PROJECT_ROOT/A-Produccion/En-Desarrollo' && '$PROMPT_ENHANCER' 'Test' 2>&1 | grep -q 'desarrollo'"

# ============================================================================
# RESUMEN
# ============================================================================

echo -e "${CYAN}╔════════════════════════════════════════════════════════════════╗${NC}"
echo -e "${CYAN}║                    RESUMEN DE PRUEBAS                          ║${NC}"
echo -e "${CYAN}╚════════════════════════════════════════════════════════════════╝${NC}\n"

echo -e "Total de pruebas: ${CYAN}$total_tests${NC}"
echo -e "Pruebas exitosas: ${GREEN}$passed_tests${NC}"
echo -e "Pruebas fallidas: ${RED}$failed_tests${NC}\n"

if [[ $failed_tests -eq 0 ]]; then
    echo -e "${GREEN}╔════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║  ✓ TODAS LAS PRUEBAS PASARON - SISTEMA COMPLETAMENTE FUNCIONAL║${NC}"
    echo -e "${GREEN}╚════════════════════════════════════════════════════════════════╝${NC}\n"
    exit 0
else
    echo -e "${RED}╔════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${RED}║  ✗ ALGUNAS PRUEBAS FALLARON - REVISAR CONFIGURACIÓN           ║${NC}"
    echo -e "${RED}╚════════════════════════════════════════════════════════════════╝${NC}\n"
    exit 1
fi

