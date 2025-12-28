#!/bin/bash
# Script de rollback para la integración del Graficador-Experto
# Fecha: 2025-12-27
# Uso: ./rollback_integracion_graficador.sh

set -e

REPO_ROOT="/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams"
BACKUP_DIR="$REPO_ROOT/.claude/backups"

echo "========================================"
echo "ROLLBACK: Integración Graficador-Experto"
echo "========================================"

# Buscar el backup más reciente
LATEST_BACKUP=$(ls -td "$BACKUP_DIR"/pre_integracion_graficador_* 2>/dev/null | head -1)

if [ -z "$LATEST_BACKUP" ]; then
    echo "ERROR: No se encontró backup de la integración del Graficador-Experto"
    echo "Buscando en: $BACKUP_DIR/pre_integracion_graficador_*"
    exit 1
fi

echo "Backup encontrado: $LATEST_BACKUP"
echo ""

# Confirmar rollback
read -p "¿Deseas restaurar desde este backup? (s/n): " confirm
if [ "$confirm" != "s" ]; then
    echo "Rollback cancelado."
    exit 0
fi

echo ""
echo "Iniciando rollback..."

# 1. Restaurar archivos principales
echo "[1/5] Restaurando Mermaid_Chart.txt..."
if [ -f "$LATEST_BACKUP/Mermaid_Chart.txt" ]; then
    cp "$LATEST_BACKUP/Mermaid_Chart.txt" "$REPO_ROOT/.claude/"
    echo "      ✓ Mermaid_Chart.txt restaurado"
fi

echo "[2/5] Restaurando settings.json..."
if [ -f "$LATEST_BACKUP/settings.json" ]; then
    cp "$LATEST_BACKUP/settings.json" "$REPO_ROOT/.claude/"
    echo "      ✓ settings.json restaurado"
fi

echo "[3/5] Restaurando settings.local.json..."
if [ -f "$LATEST_BACKUP/settings.local.json" ]; then
    cp "$LATEST_BACKUP/settings.local.json" "$REPO_ROOT/.claude/"
    echo "      ✓ settings.local.json restaurado"
fi

# 2. Restaurar directorios
echo "[4/5] Restaurando directorio skills/..."
if [ -d "$LATEST_BACKUP/skills" ]; then
    rm -rf "$REPO_ROOT/.claude/skills"
    cp -r "$LATEST_BACKUP/skills" "$REPO_ROOT/.claude/"
    echo "      ✓ skills/ restaurado"
fi

echo "[5/5] Restaurando directorio hooks/..."
if [ -d "$LATEST_BACKUP/hooks" ]; then
    rm -rf "$REPO_ROOT/.claude/hooks"
    cp -r "$LATEST_BACKUP/hooks" "$REPO_ROOT/.claude/"
    echo "      ✓ hooks/ restaurado"
fi

# 3. Eliminar archivos nuevos creados por la integración
echo ""
echo "Limpiando archivos de integración..."

# Skills nuevos
if [ -d "$REPO_ROOT/.claude/skills/detectar-imagen-matematica" ]; then
    rm -rf "$REPO_ROOT/.claude/skills/detectar-imagen-matematica"
    echo "      ✓ Eliminado: skills/detectar-imagen-matematica/"
fi

if [ -d "$REPO_ROOT/.claude/skills/generar-codigo-triple" ]; then
    rm -rf "$REPO_ROOT/.claude/skills/generar-codigo-triple"
    echo "      ✓ Eliminado: skills/generar-codigo-triple/"
fi

# Hook nuevo
if [ -f "$REPO_ROOT/.claude/hooks/post-imagen-matematica-detectada.md" ]; then
    rm "$REPO_ROOT/.claude/hooks/post-imagen-matematica-detectada.md"
    echo "      ✓ Eliminado: hooks/post-imagen-matematica-detectada.md"
fi

echo ""
echo "========================================"
echo "ROLLBACK COMPLETADO EXITOSAMENTE"
echo "========================================"
echo ""
echo "El sistema ha sido restaurado al estado anterior a la integración."
echo "Backup utilizado: $LATEST_BACKUP"
echo ""
echo "Verificar que el workflow funciona correctamente:"
echo "  1. Probar /analizar-icfes"
echo "  2. Probar /generar-schoice"
echo "  3. Verificar renderizado exams2*"
echo ""
