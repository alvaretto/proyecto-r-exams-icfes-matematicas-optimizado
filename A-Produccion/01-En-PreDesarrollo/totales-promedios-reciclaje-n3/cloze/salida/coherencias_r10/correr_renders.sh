#!/bin/bash
# Renders de la r10 (cwd fuera del repo; salidas en coherencias_r10/). Cada paso deja su línea exit= en su log.
R10="$(cd "$(dirname "$0")" && pwd)"; cd "$HOME/.cache/c2_cloze/oraculo" || exit 1; export TMPDIR="$PWD/tmp"
for f in html pdf moodle docx nops; do Rscript --no-init-file "$R10/render_r10.R" $f > "$R10/render_$f.log" 2>&1; echo "exit=$?" >> "$R10/render_$f.log"; done
bash "$R10/correr_exportador_aula.sh"
echo FIN > "$R10/render_fin.log"
