#!/bin/bash

# ============================================================================
# VERIFICAR ALIAS - Script de Verificación de Alias Prompt Enhancer
# ============================================================================
# Descripción: Verifica que los alias del Prompt Enhancer estén configurados
#              correctamente en bash y zsh
# ============================================================================

# Colores
readonly GREEN='\033[0;32m'
readonly RED='\033[0;31m'
readonly YELLOW='\033[1;33m'
readonly CYAN='\033[0;36m'
readonly NC='\033[0m'

echo -e "${CYAN}╔════════════════════════════════════════════════════════════════╗${NC}"
echo -e "${CYAN}║     VERIFICACIÓN DE ALIAS - PROMPT ENHANCER                   ║${NC}"
echo -e "${CYAN}╚════════════════════════════════════════════════════════════════╝${NC}\n"

# ============================================================================
# VERIFICAR CONFIGURACIÓN EN ARCHIVOS
# ============================================================================

echo -e "${YELLOW}=== VERIFICACIÓN DE ARCHIVOS DE CONFIGURACIÓN ===${NC}\n"

# Verificar ~/.bashrc
if grep -q "ICFES_PROJECT_ROOT" ~/.bashrc 2>/dev/null; then
    echo -e "${GREEN}✓ ~/.bashrc configurado correctamente${NC}"
    echo -e "${CYAN}  Alias encontrados:${NC}"
    grep "alias.*prompt-enhancer\|alias.*pe" ~/.bashrc | sed 's/^/    /'
else
    echo -e "${RED}✗ ~/.bashrc NO configurado${NC}"
fi

echo ""

# Verificar ~/.zshrc
if [ -f ~/.zshrc ]; then
    if grep -q "ICFES_PROJECT_ROOT" ~/.zshrc 2>/dev/null; then
        echo -e "${GREEN}✓ ~/.zshrc configurado correctamente${NC}"
        echo -e "${CYAN}  Alias encontrados:${NC}"
        grep "alias.*prompt-enhancer\|alias.*pe" ~/.zshrc | sed 's/^/    /'
    else
        echo -e "${RED}✗ ~/.zshrc NO configurado${NC}"
    fi
else
    echo -e "${YELLOW}⚠ ~/.zshrc no existe (normal si no usas zsh)${NC}"
fi

echo ""

# ============================================================================
# INSTRUCCIONES PARA ACTIVAR LOS ALIAS
# ============================================================================

echo -e "${CYAN}╔════════════════════════════════════════════════════════════════╗${NC}"
echo -e "${CYAN}║              CÓMO ACTIVAR LOS ALIAS                            ║${NC}"
echo -e "${CYAN}╚════════════════════════════════════════════════════════════════╝${NC}\n"

echo -e "${YELLOW}Para que los alias estén disponibles en tu terminal actual:${NC}\n"

# Detectar shell actual
CURRENT_SHELL=$(basename "$SHELL")

if [ "$CURRENT_SHELL" = "bash" ]; then
    echo -e "${CYAN}Ejecuta en tu terminal:${NC}"
    echo -e "  ${GREEN}source ~/.bashrc${NC}\n"
elif [ "$CURRENT_SHELL" = "zsh" ]; then
    echo -e "${CYAN}Ejecuta en tu terminal:${NC}"
    echo -e "  ${GREEN}source ~/.zshrc${NC}\n"
else
    echo -e "${CYAN}Ejecuta en tu terminal (según tu shell):${NC}"
    echo -e "  ${GREEN}source ~/.bashrc${NC}  (para bash)"
    echo -e "  ${GREEN}source ~/.zshrc${NC}   (para zsh)\n"
fi

echo -e "${YELLOW}O simplemente abre una nueva terminal.${NC}\n"

# ============================================================================
# ALIAS DISPONIBLES
# ============================================================================

echo -e "${CYAN}╔════════════════════════════════════════════════════════════════╗${NC}"
echo -e "${CYAN}║                 ALIAS DISPONIBLES                              ║${NC}"
echo -e "${CYAN}╚════════════════════════════════════════════════════════════════╝${NC}\n"

echo -e "${GREEN}prompt-enhancer${NC} - Comando completo"
echo -e "  Uso: ${CYAN}prompt-enhancer \"Tu prompt aquí\"${NC}\n"

echo -e "${GREEN}pe${NC} - Versión corta (recomendado)"
echo -e "  Uso: ${CYAN}pe \"Tu prompt aquí\"${NC}\n"

echo -e "${GREEN}pec${NC} - Con copia al portapapeles"
echo -e "  Uso: ${CYAN}pec \"Tu prompt aquí\"${NC}"
echo -e "  (Copia automáticamente el resultado al portapapeles)\n"

echo -e "${GREEN}pei${NC} - Modo interactivo"
echo -e "  Uso: ${CYAN}pei${NC}"
echo -e "  (Te pedirá el prompt de forma interactiva)\n"

# ============================================================================
# EJEMPLOS DE USO
# ============================================================================

echo -e "${CYAN}╔════════════════════════════════════════════════════════════════╗${NC}"
echo -e "${CYAN}║                    EJEMPLOS DE USO                             ║${NC}"
echo -e "${CYAN}╚════════════════════════════════════════════════════════════════╝${NC}\n"

echo -e "${YELLOW}1. Uso básico:${NC}"
echo -e "   ${CYAN}pe \"Genera un ejercicio de estadística nivel 3\"${NC}\n"

echo -e "${YELLOW}2. Con copia al portapapeles:${NC}"
echo -e "   ${CYAN}pec \"Corrige este error en el archivo .Rmd\"${NC}\n"

echo -e "${YELLOW}3. Modo interactivo:${NC}"
echo -e "   ${CYAN}pei${NC}"
echo -e "   (Te pedirá el prompt)\n"

echo -e "${YELLOW}4. Desde cualquier ubicación:${NC}"
echo -e "   ${CYAN}cd ~/Documentos${NC}"
echo -e "   ${CYAN}pe \"Analiza este ejercicio\"${NC}\n"

# ============================================================================
# VERIFICACIÓN FINAL
# ============================================================================

echo -e "${CYAN}╔════════════════════════════════════════════════════════════════╗${NC}"
echo -e "${CYAN}║                  VERIFICACIÓN FINAL                            ║${NC}"
echo -e "${CYAN}╚════════════════════════════════════════════════════════════════╝${NC}\n"

echo -e "${YELLOW}Para verificar que todo funciona:${NC}\n"
echo -e "1. ${CYAN}source ~/.bashrc${NC} (o abre una nueva terminal)"
echo -e "2. ${CYAN}pe \"Test\"${NC}"
echo -e "3. Deberías ver el prompt mejorado con todo el contexto del proyecto\n"

echo -e "${GREEN}✓ Configuración completada${NC}\n"

