# Setup de Hooks de Ontología

## Máquina Cinnamon (La Tebaida) — post-commit

El hook `.git/hooks/post-commit` ya está configurado y registra ejercicios nuevos en Fuseki tras cada commit.

## Máquina Plasma (externa) — post-checkout

El hook `.git/hooks/post-checkout` debe copiarse manualmente a Plasma después de clonar/actualizar el repositorio:

```bash
# Ejecutar en Plasma, desde el directorio del proyecto:
# El hook ya debería estar en el repo si se hace git pull — pero hooks no se sincronizan con git
# Copiar manualmente:
cat > .git/hooks/post-checkout << 'EOF'
#!/bin/bash
# .git/hooks/post-checkout
# Repuebla Fuseki en Plasma cuando los archivos de ontología cambian tras git pull.
# Solo actúa en cambios de rama (IS_BRANCH_CHANGE=1), no en checkouts de archivos.
#
# Argumentos:
#   $1 = PREV_HEAD  (SHA anterior)
#   $2 = NEW_HEAD   (SHA nuevo)
#   $3 = IS_BRANCH_CHANGE  (1=rama, 0=archivo)

PREV_HEAD=$1
NEW_HEAD=$2
IS_BRANCH_CHANGE=$3

# Solo actuar en cambios de rama (git pull, git checkout <branch>)
[ "$IS_BRANCH_CHANGE" = "0" ] && exit 0

REPO_ROOT=$(git rev-parse --show-toplevel)

# Verificar si los archivos de ontología o ejercicios cambiaron
if git diff --name-only "$PREV_HEAD" "$NEW_HEAD" 2>/dev/null \
    | grep -qE "ontologia/(matematicas-icfes|conceptos)\.ttl|A-Produccion/03-En-Produccion/.*\.Rmd$"; then
  echo "[ontologia] Cambios detectados — repoblando Fuseki..."
  cd "$REPO_ROOT" && Rscript core/poblar_ontologia.R || \
    echo "[ontologia] Error al repoblar. Verifique que Fuseki esté activo."
fi

exit 0
EOF
chmod +x .git/hooks/post-checkout
```

## Verificar Fuseki en Plasma

```bash
systemctl --user status fuseki.service
curl -s http://localhost:3030/$/ping
```

Si no está instalado, seguir los mismos pasos que en Cinnamon (Task 2 del plan).

## Notas Importantes

- **Git hooks NO se sincronizan automáticamente**: El archivo `.git/hooks/post-checkout` no se rastrea con git. Debe copiarse manualmente a cada máquina.
- **Solo actúa en cambios de rama**: El hook ignora checkouts de archivos individuales (`git checkout file.txt`) y solo se ejecuta en `git pull` y `git checkout <branch>`.
- **Requisito previo**: Fuseki debe estar activo en la máquina para que el hook funcione. Si no está instalado:
  - Instalar Apache Fuseki (Task 2)
  - Configurar `fuseki.service` como unidad de usuario
  - Asegurar que el script `core/poblar_ontologia.R` está disponible
