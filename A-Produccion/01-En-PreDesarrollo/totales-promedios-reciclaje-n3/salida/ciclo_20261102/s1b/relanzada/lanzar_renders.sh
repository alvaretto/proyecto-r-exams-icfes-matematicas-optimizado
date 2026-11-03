#!/bin/bash
# Tanda S1b relanzada: renders pareados (html, pdf, moodle) de las cinco versiones, en paralelo.
S1B="$1"; W="$2"; RL="$S1B/relanzada"
declare -A MD5=([e293]=e293d5c8 [base]=4cffbe8b [v1]=5cf5b12f [v2]=0ba64a97 [v1p]=2a5e59a1)
for v in e293 base v1 v2 v1p; do
  ( Rscript --no-init-file "$RL/render_versiones.R" "$W/$v/ej.Rmd" "$S1B/render_${MD5[$v]}" html,pdf,moodle > "$RL/log_render_$v.txt" 2>&1; echo "exit=$?" >> "$RL/log_render_$v.txt" ) &
done
wait
echo "TODOS_LOS_RENDERS_TERMINADOS $(date -Is)" > "$RL/renders.fin"
