#!/bin/bash
# Hook BLOQUEANTE: Rechaza commits con errores ortográficos
# Instalación: cp .claude/hooks/pre-commit-ortografia.sh .git/hooks/pre-commit && chmod +x .git/hooks/pre-commit

RMD_FILES=$(git diff --cached --name-only --diff-filter=ACM | grep '\.Rmd$')
[ -z "$RMD_FILES" ] && exit 0

SCRIPT=".claude/scripts/corregir_ortografia_espanol.R"
[ ! -f "$SCRIPT" ] && exit 0

ERRORES=0
for f in $RMD_FILES; do
    [ -f "$f" ] && Rscript "$SCRIPT" "$f" 2>/dev/null | grep -q "ERRORES" && ERRORES=1
done

if [ $ERRORES -eq 1 ]; then
    echo "❌ COMMIT RECHAZADO: Errores ortográficos detectados"
    echo "   Rscript $SCRIPT <archivo> --fix"
    exit 1
fi
exit 0
