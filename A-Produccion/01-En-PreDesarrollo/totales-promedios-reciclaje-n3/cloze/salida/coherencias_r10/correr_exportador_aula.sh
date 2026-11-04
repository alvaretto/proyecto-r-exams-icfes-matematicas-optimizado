#!/bin/bash
# Corre el exportador REAL (exportar_moodle_cloze.R, 30 versiones) con una copia fuera del repo (el script exige estar en cloze/salida) y trae el XML a coherencias_r10/moodle_aula.
set -u
R10="$(cd "$(dirname "$0")" && pwd)"; W="$HOME/.cache/c2_cloze/exp"; SUBC="$R10/../.."
mkdir -p "$W/cloze/salida"; command cp -f "$SUBC/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd" "$SUBC/exportar_moodle_cloze.R" "$SUBC/verificar_gaps_xml.py" "$W/cloze/"
md5sum "$W/cloze/totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"
cd "$W/cloze/salida" && rm -rf moodle_aula && TMPDIR="$HOME/.cache/c2_cloze/oraculo/tmp" Rscript --no-init-file ../exportar_moodle_cloze.R 30 > "$R10/exportador_aula.log" 2>&1; echo "exit=$?" >> "$R10/exportador_aula.log"
mkdir -p "$R10/moodle_aula"; command cp -f "$W/cloze/salida/moodle_aula/"* "$R10/moodle_aula/"
