#!/usr/bin/env bash
# Uso: compilar.sh <archivo_sin_extension>   (en graficador/tikz/)
# pdflatex -> PDF; magick -density 600 -> PNG (fondo blanco, sin alfa, 8 bits, RGB).
set -euo pipefail
cd "$(dirname "$0")/.."
b="$1"
log="$(mktemp)"; trap 'rm -f "$log"' EXIT
pdflatex -interaction=nonstopmode -halt-on-error "$b.tex" > "$log" 2>&1 || { echo "FALLO pdflatex"; grep -n -E "^!|Error" "$log" | head; exit 1; }
grep -E "Warning|Overfull" "$log" | grep -v -E "First Aid" | head -5 || true
magick -density 600 "$b.pdf" -background white -alpha remove -alpha off -depth 8 "PNG24:$b.png"
rm -f "$b.aux" "$b.log"
identify "$b.png"
