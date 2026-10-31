#!/usr/bin/env python3
"""montaje_p2_r8.py — recorta la ventana de la Parte 2 (enunciado + opciones) de shots/sNN_box320.png para las 24 semillas y las junta, 8 por imagen."""
import sys
from PIL import Image, ImageDraw
SEEDS = [7,3,2,1,4,5,6]+list(range(8,25))
mode, y0, y1, per, w = sys.argv[1], int(sys.argv[2]), int(sys.argv[3]), int(sys.argv[4]), int(sys.argv[5])
tag = sys.argv[6]
ims = []
for s in sorted(SEEDS):
    im = Image.open(f"shots/s{s:02d}_{mode}.png").convert("RGB"); H = im.size[1]
    c = im.crop((0, min(y0, H-10), w, min(y1, H))); d = ImageDraw.Draw(c); d.rectangle((0, 0, 70, 16), fill=(255, 255, 160)); d.text((3, 2), f"s{s:02d} y{y0}", fill=(0, 0, 0))
    ims.append(c)
cols = 4; n = 0
for i in range(0, len(ims), per):
    grp = ims[i:i+per]; rows = (len(grp)+cols-1)//cols
    out = Image.new("RGB", (cols*(w+6), rows*(y1-y0+6)), (150, 160, 190))
    for j, c in enumerate(grp): out.paste(c, ((j % cols)*(w+6), (j//cols)*(y1-y0+6)))
    fn = f"montaje_{tag}_{n}.png"; out.save(fn); print(fn, out.size); n += 1
