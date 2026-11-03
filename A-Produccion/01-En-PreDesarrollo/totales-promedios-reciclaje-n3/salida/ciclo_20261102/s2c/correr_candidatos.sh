#!/usr/bin/env bash
# Corre verificar_s2c.R para cada candidato, en paralelo. Uso: bash correr_candidatos.sh ID1 ID2 ...
cd "$(dirname "$0")"
RMD="$(cd ../../.. && pwd)/totales_promedios_reciclaje_aleatorio_argumentacion_n3_schoice_v1.Rmd"
source ./configs_s2c.sh
for id in "$@"; do
  vars="${!id}"
  (
    t0=$(date +%s)
    if [ "$id" = "B0" ]; then
      env S2C_ID=B0 S2C_DIR="$PWD" Rscript --no-init-file verificar_s2c.R "$RMD" > "salida_${id}.txt" 2>&1
    else
      env $vars S2C_ID="$id" S2C_CAND="$PWD/candidatos_s2c.R" S2C_DIR="$PWD" Rscript --no-init-file verificar_s2c.R "$RMD" > "salida_${id}.txt" 2>&1
    fi
    ec=$?; t1=$(date +%s)
    echo "exit=$ec segundos=$((t1 - t0))" >> "salida_${id}.txt"
  ) &
done
wait
for id in "$@"; do printf '%s: %s\n' "$id" "$(tail -1 "salida_${id}.txt")"; done
