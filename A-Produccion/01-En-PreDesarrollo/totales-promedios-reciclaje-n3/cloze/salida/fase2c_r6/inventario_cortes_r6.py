#!/usr/bin/env python3
"""FASE 2C r6. Inventario propio de cortes de renglón del párrafo «Parte 6.» sobre el out.json de la 6b C1e. No usa contar_h2.py ni agg.py."""
import json, collections, re
d = json.load(open('/home/bootcamp/.cache/c1e_6b/out.json', encoding='utf-8'))
NB = chr(0xA0)
print('páginas', len(d), 'claves', sorted({(r['v'], r['vw']) for r in d}))
FRASE = "Por eso, para saber si el grado que perdió habría ganado con un curso más, o si el que ganó habría perdido con uno menos, basta comparar el promedio de cada uno con la diferencia entre los totales: si el promedio es mayor, sí; si es menor, no."
hab = collections.defaultdict(collections.Counter); den = collections.Counter(); den_tot = collections.Counter()
frase_cortes = collections.defaultdict(collections.Counter)
quien_h = collections.defaultdict(collections.Counter)
for r in d:
    k = (r['v'], r['vw'])
    lin = [s.replace(NB, ' ').strip() for s in (r['m'].get('p6') or {}).get('lines') or []]
    if not lin:
        print('SIN LINEAS', k, r.get('seed')); continue
    tx = ' '.join(lin); den_tot[k] += 1
    # tokens con índice de renglón
    tok = [(w, i) for i, l in enumerate(lin) for w in l.split(' ') if w]
    for j in range(len(tok) - 1):
        a, ia = tok[j]; b, ib = tok[j + 1]
        if a == 'habría':
            den[(k, b)] += 1
            if ia != ib:
                hab[k][b] += 1
    # fin de renglón con 'el', 'si', 'la', 'de', 'que', 'el grado' etc: palabras funcionales al final (solo informativo)
    if FRASE in tx:
        i0 = tx.index(FRASE); pre = len(tx[:i0].split(' ')) - 1 if i0 else 0
        words = FRASE.split(' ')
        for j, w in enumerate(words[:-1]):
            if tok[pre + j][1] != tok[pre + j + 1][1]:
                frase_cortes[k][f"{words[j]} ⏎ {words[j+1]}"] += 1
for k in sorted(den_tot, key=lambda z: (z[0], -z[1])):
    print('\n==', k, 'páginas', den_tot[k])
    print('  «habría ⏎ X»:', {x: f"{hab[k][x]}/{den[(k, x)]}" for x in sorted({b for (kk, b) in den if kk == k})})
    if frase_cortes[k]:
        print('  cortes dentro de la frase nueva:', dict(frase_cortes[k]))
