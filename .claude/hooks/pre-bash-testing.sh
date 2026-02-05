#!/bin/bash
# Pre-bash testing hook - Simplified v2
# Solo bloquea git commit/push sin tests previos
# Lee JSON de stdin para extraer el comando

# Leer stdin con timeout para evitar bloqueo
INPUT=$(timeout 2 cat 2>/dev/null || true)

# Si no hay input, permitir ejecución
if [ -z "$INPUT" ]; then
  exit 0
fi

# Extraer comando del JSON
COMMAND=$(echo "$INPUT" | python3 -c "
import sys, json
try:
    data = json.load(sys.stdin)
    print(data.get('tool_input', {}).get('command', ''))
except:
    print('')
" 2>/dev/null || echo "")

# Si no se pudo extraer comando, permitir ejecución
if [ -z "$COMMAND" ]; then
  exit 0
fi

# Solo interceptar git commit y git push
case "$COMMAND" in
  *"git commit"*)
    if [[ "$COMMAND" != *"--amend"* ]]; then
      echo "Pre-commit: ejecutando tests..."
      cd "${CLAUDE_PROJECT_DIR:-.}" && Rscript tests/run_all_tests.R 2>&1 || exit 2
    fi
    ;;
  *"git push"*)
    echo "Pre-push: ejecutando tests..."
    cd "${CLAUDE_PROJECT_DIR:-.}" && Rscript tests/run_all_tests.R 2>&1 || exit 2
    ;;
esac

exit 0
