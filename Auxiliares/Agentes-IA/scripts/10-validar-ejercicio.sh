#!/bin/bash

# ============================================================================
# Script de Validación Automática de Ejercicios R-exams ICFES
# ============================================================================
# Uso: ./10-validar-ejercicio.sh archivo.Rmd
# Descripción: Valida estructura, sintaxis y metadatos de archivos .Rmd
# ============================================================================

ARCHIVO=$1
PROYECTO_ROOT="/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Contadores
ERRORES=0
ADVERTENCIAS=0
EXITOS=0

echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║  VALIDADOR DE EJERCICIOS R-EXAMS ICFES                     ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
echo ""
echo -e "${YELLOW}📄 Archivo: ${NC}$ARCHIVO"
echo -e "${YELLOW}📅 Fecha: ${NC}$(date '+%Y-%m-%d %H:%M:%S')"
echo ""
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"

# ============================================================================
# 1. VERIFICAR EXISTENCIA DEL ARCHIVO
# ============================================================================
echo ""
echo -e "${BLUE}[1/8]${NC} Verificando existencia del archivo..."

if [ ! -f "$ARCHIVO" ]; then
    echo -e "${RED}❌ ERROR: Archivo no encontrado${NC}"
    exit 1
else
    echo -e "${GREEN}✅ Archivo encontrado${NC}"
    ((EXITOS++))
fi

# ============================================================================
# 2. VERIFICAR ESTRUCTURA YAML
# ============================================================================
echo ""
echo -e "${BLUE}[2/8]${NC} Verificando encabezado YAML..."

if grep -q "^---$" "$ARCHIVO"; then
    echo -e "${GREEN}✅ Encabezado YAML presente${NC}"
    ((EXITOS++))
    
    # Verificar campos de output
    if grep -q "output:" "$ARCHIVO"; then
        echo -e "${GREEN}  ✅ Campo 'output' presente${NC}"
        ((EXITOS++))
    else
        echo -e "${RED}  ❌ Falta campo 'output'${NC}"
        ((ERRORES++))
    fi
else
    echo -e "${RED}❌ Falta encabezado YAML${NC}"
    ((ERRORES++))
fi

# ============================================================================
# 3. VERIFICAR METADATOS ICFES
# ============================================================================
echo ""
echo -e "${BLUE}[3/8]${NC} Verificando metadatos ICFES..."

if grep -q "icfes:" "$ARCHIVO"; then
    echo -e "${GREEN}✅ Sección de metadatos ICFES presente${NC}"
    ((EXITOS++))
    
    # Verificar campos específicos
    if grep -q "competencia:" "$ARCHIVO"; then
        echo -e "${GREEN}  ✅ Competencia definida${NC}"
        ((EXITOS++))
    else
        echo -e "${RED}  ❌ Falta competencia${NC}"
        ((ERRORES++))
    fi
    
    if grep -q "nivel_dificultad:" "$ARCHIVO"; then
        echo -e "${GREEN}  ✅ Nivel de dificultad definido${NC}"
        ((EXITOS++))
    else
        echo -e "${RED}  ❌ Falta nivel de dificultad${NC}"
        ((ERRORES++))
    fi
    
    if grep -q "componente:" "$ARCHIVO"; then
        echo -e "${GREEN}  ✅ Componente definido${NC}"
        ((EXITOS++))
    else
        echo -e "${RED}  ❌ Falta componente${NC}"
        ((ERRORES++))
    fi
    
    if grep -q "contexto:" "$ARCHIVO"; then
        echo -e "${GREEN}  ✅ Contexto definido${NC}"
        ((EXITOS++))
    else
        echo -e "${YELLOW}  ⚠️  Falta contexto (recomendado)${NC}"
        ((ADVERTENCIAS++))
    fi
else
    echo -e "${RED}❌ Faltan metadatos ICFES${NC}"
    ((ERRORES++))
fi

# ============================================================================
# 4. VERIFICAR CHUNKS OBLIGATORIOS
# ============================================================================
echo ""
echo -e "${BLUE}[4/8]${NC} Verificando chunks obligatorios..."

if grep -q '```{r setup' "$ARCHIVO" || grep -q '```{r inicio' "$ARCHIVO"; then
    echo -e "${GREEN}✅ Chunk de configuración inicial presente${NC}"
    ((EXITOS++))
else
    echo -e "${RED}❌ Falta chunk de configuración inicial (setup/inicio)${NC}"
    ((ERRORES++))
fi

if grep -q "generar_datos" "$ARCHIVO"; then
    echo -e "${GREEN}✅ Función generar_datos() presente${NC}"
    ((EXITOS++))
else
    echo -e "${RED}❌ Falta función generar_datos()${NC}"
    ((ERRORES++))
fi

if grep -q "version_diversity_test" "$ARCHIVO" || grep -q "test_that" "$ARCHIVO"; then
    echo -e "${GREEN}✅ Test de diversidad presente${NC}"
    ((EXITOS++))
else
    echo -e "${YELLOW}⚠️  Falta test de diversidad (recomendado)${NC}"
    ((ADVERTENCIAS++))
fi

# ============================================================================
# 5. VERIFICAR SECCIONES R-EXAMS
# ============================================================================
echo ""
echo -e "${BLUE}[5/8]${NC} Verificando secciones R-exams..."

if grep -q "^Question$" "$ARCHIVO"; then
    echo -e "${GREEN}✅ Sección Question presente${NC}"
    ((EXITOS++))
else
    echo -e "${RED}❌ Falta sección Question${NC}"
    ((ERRORES++))
fi

if grep -q "^Solution$" "$ARCHIVO"; then
    echo -e "${GREEN}✅ Sección Solution presente${NC}"
    ((EXITOS++))
else
    echo -e "${RED}❌ Falta sección Solution${NC}"
    ((ERRORES++))
fi

if grep -q "^Meta-information$" "$ARCHIVO"; then
    echo -e "${GREEN}✅ Sección Meta-information presente${NC}"
    ((EXITOS++))
else
    echo -e "${RED}❌ Falta sección Meta-information${NC}"
    ((ERRORES++))
fi

# Verificar Answerlist
ANSWERLIST_COUNT=$(grep -c "^Answerlist$" "$ARCHIVO")
if [ "$ANSWERLIST_COUNT" -ge 2 ]; then
    echo -e "${GREEN}✅ Answerlist presente en Question y Solution${NC}"
    ((EXITOS++))
elif [ "$ANSWERLIST_COUNT" -eq 1 ]; then
    echo -e "${YELLOW}⚠️  Solo una sección Answerlist encontrada${NC}"
    ((ADVERTENCIAS++))
else
    echo -e "${RED}❌ Falta sección Answerlist${NC}"
    ((ERRORES++))
fi

# ============================================================================
# 6. VERIFICAR META-INFORMACIÓN
# ============================================================================
echo ""
echo -e "${BLUE}[6/8]${NC} Verificando meta-información..."

if grep -q "exname:" "$ARCHIVO"; then
    echo -e "${GREEN}✅ exname definido${NC}"
    ((EXITOS++))
else
    echo -e "${RED}❌ Falta exname${NC}"
    ((ERRORES++))
fi

if grep -q "extype:" "$ARCHIVO"; then
    EXTYPE=$(grep "extype:" "$ARCHIVO" | head -1 | awk '{print $2}')
    echo -e "${GREEN}✅ extype definido: ${NC}$EXTYPE"
    ((EXITOS++))
else
    echo -e "${RED}❌ Falta extype${NC}"
    ((ERRORES++))
fi

if grep -q "exsolution:" "$ARCHIVO"; then
    echo -e "${GREEN}✅ exsolution definido${NC}"
    ((EXITOS++))
else
    echo -e "${RED}❌ Falta exsolution${NC}"
    ((ERRORES++))
fi

if grep -q "exshuffle:" "$ARCHIVO"; then
    echo -e "${GREEN}✅ exshuffle definido${NC}"
    ((EXITOS++))
else
    echo -e "${YELLOW}⚠️  exshuffle no definido (recomendado: TRUE)${NC}"
    ((ADVERTENCIAS++))
fi

# ============================================================================
# 7. VERIFICAR CONFIGURACIONES TÉCNICAS
# ============================================================================
echo ""
echo -e "${BLUE}[7/8]${NC} Verificando configuraciones técnicas..."

# Verificar configuración de locale
if grep -q "Sys.setlocale" "$ARCHIVO"; then
    echo -e "${GREEN}✅ Configuración de locale presente${NC}"
    ((EXITOS++))
else
    echo -e "${YELLOW}⚠️  Falta configuración de locale (recomendado)${NC}"
    ((ADVERTENCIAS++))
fi

# Verificar configuración de OutDec
if grep -q 'options(OutDec = ".")' "$ARCHIVO"; then
    echo -e "${GREEN}✅ Configuración de OutDec presente${NC}"
    ((EXITOS++))
else
    echo -e "${YELLOW}⚠️  Falta configuración de OutDec (recomendado)${NC}"
    ((ADVERTENCIAS++))
fi

# Verificar set.seed aleatorio
if grep -q "set.seed(sample" "$ARCHIVO"; then
    echo -e "${GREEN}✅ set.seed aleatorio presente${NC}"
    ((EXITOS++))
elif grep -q "set.seed([0-9]" "$ARCHIVO"; then
    echo -e "${RED}❌ set.seed fijo detectado (debe ser aleatorio)${NC}"
    ((ERRORES++))
else
    echo -e "${YELLOW}⚠️  No se detectó set.seed${NC}"
    ((ADVERTENCIAS++))
fi

# ============================================================================
# 8. VERIFICAR SINTAXIS BÁSICA
# ============================================================================
echo ""
echo -e "${BLUE}[8/8]${NC} Verificando sintaxis básica..."

# Verificar balance de chunks
CHUNK_OPEN=$(grep -c '```{r' "$ARCHIVO")
CHUNK_CLOSE=$(grep -c '^```$' "$ARCHIVO")

if [ "$CHUNK_OPEN" -eq "$CHUNK_CLOSE" ]; then
    echo -e "${GREEN}✅ Chunks balanceados (${CHUNK_OPEN} abiertos, ${CHUNK_CLOSE} cerrados)${NC}"
    ((EXITOS++))
else
    echo -e "${RED}❌ Chunks desbalanceados (${CHUNK_OPEN} abiertos, ${CHUNK_CLOSE} cerrados)${NC}"
    ((ERRORES++))
fi

# Verificar caracteres Unicode problemáticos
if grep -P '[^\x00-\x7F]' "$ARCHIVO" | grep -v "^#" | grep -v "Question" | grep -v "Solution" > /dev/null; then
    echo -e "${YELLOW}⚠️  Posibles caracteres Unicode detectados (usar LaTeX en su lugar)${NC}"
    ((ADVERTENCIAS++))
else
    echo -e "${GREEN}✅ No se detectaron caracteres Unicode problemáticos${NC}"
    ((EXITOS++))
fi

# ============================================================================
# RESUMEN FINAL
# ============================================================================
echo ""
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║  RESUMEN DE VALIDACIÓN                                     ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
echo ""
echo -e "${GREEN}✅ Éxitos:        ${NC}$EXITOS"
echo -e "${YELLOW}⚠️  Advertencias:  ${NC}$ADVERTENCIAS"
echo -e "${RED}❌ Errores:       ${NC}$ERRORES"
echo ""

# Calcular puntuación
TOTAL=$((EXITOS + ADVERTENCIAS + ERRORES))
if [ "$TOTAL" -gt 0 ]; then
    PUNTUACION=$((EXITOS * 100 / TOTAL))
else
    PUNTUACION=0
fi

echo -e "${BLUE}📊 Puntuación:    ${NC}$PUNTUACION%"
echo ""

# Determinar estado general
if [ "$ERRORES" -eq 0 ]; then
    if [ "$ADVERTENCIAS" -eq 0 ]; then
        echo -e "${GREEN}🎉 ESTADO: EXCELENTE - Archivo completamente validado${NC}"
        EXIT_CODE=0
    else
        echo -e "${YELLOW}✓ ESTADO: BUENO - Archivo validado con advertencias menores${NC}"
        EXIT_CODE=0
    fi
else
    if [ "$ERRORES" -le 3 ]; then
        echo -e "${YELLOW}⚠ ESTADO: REQUIERE CORRECCIONES MENORES${NC}"
        EXIT_CODE=1
    else
        echo -e "${RED}✗ ESTADO: REQUIERE CORRECCIONES IMPORTANTES${NC}"
        EXIT_CODE=2
    fi
fi

echo ""
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""

# Recomendaciones
if [ "$ERRORES" -gt 0 ]; then
    echo -e "${YELLOW}💡 RECOMENDACIONES:${NC}"
    echo ""
    echo "1. Consultar ejemplos funcionales en:"
    echo "   ${PROYECTO_ROOT}/Auxiliares/Ejemplos-Funcionales-Rmd/"
    echo ""
    echo "2. Revisar biblioteca de soluciones:"
    echo "   ${PROYECTO_ROOT}/Auxiliares/BIBLIOTECA_Soluciones_Errores_Comunes.md"
    echo ""
    echo "3. Usar checklist de validación:"
    echo "   ${PROYECTO_ROOT}/Auxiliares/CHECKLIST_Validacion_Archivos_Rmd.md"
    echo ""
fi

exit $EXIT_CODE

