#!/usr/bin/env python3
"""Coordinador, 2026-11-03: recuento propio de los cortes «habría ⏎ X» del párrafo «Parte 6.», para reproducir H1 y H2 de la
FASE 2C r6. Lee los renglones que midió la 6b de la C1e (~/.cache/c1e_6b/out.json). No usa contar_h2.py, agg.py ni el script del
detractor. n = e16842c1; r = 32243d0a. «gen» es el criterio general; «res», las dos frases resueltas."""
import json, collections, pathlib
d = json.load(open(pathlib.Path.home() / '.cache/c1e_6b/out.json', encoding='utf-8'))
NB = chr(0xA0)
lineas = lambda r: [s.replace(NB, ' ').strip() for s in (r['m'].get('p6') or {}).get('lines') or []]
cnt, den = collections.defaultdict(collections.Counter), collections.defaultdict(collections.Counter)
pag, pags, ej = collections.Counter(), collections.Counter(), {}
for r in d:
    k = (r['v'], r['vw']); L = lineas(r); assert L
    pal = [(w, i) for i, l in enumerate(L) for w in l.split(' ') if w]; txt = [w for w, _ in pal]
    ini = next(j for j in range(len(txt) - 3) if txt[j:j + 4] == ['Con', 'un', 'curso', 'más,'])
    corte = False
    for j in range(len(pal) - 1):
        a, ia = pal[j]; b, ib = pal[j + 1]
        if a == 'habría':
            z = 'res' if j >= ini else 'gen'; den[k][(z, b)] += 1
            if ia != ib:
                cnt[k][(z, b)] += 1; corte = corte or z == 'res'
    pag[k] += corte; pags[k] += 1
    if r.get('seed') == 1: ej[k] = L
for k in sorted(pags, key=lambda z: (z[0], -z[1])):
    print('==', k, 'páginas', pags[k], '· con «habría ⏎ X» en las frases resueltas:', pag[k])
    for key in sorted(den[k]): print('   ', key[0], 'habría ⏎', key[1], f"{cnt[k][key]}/{den[k][key]}")
print('--- semilla 1, renglones del criterio')
for k, L in sorted(ej.items(), key=lambda z: (z[0][0], -z[0][1])):
    print(k, [l for l in L if 'promedio' in l or 'menor, no' in l or 'mayor' in l])
