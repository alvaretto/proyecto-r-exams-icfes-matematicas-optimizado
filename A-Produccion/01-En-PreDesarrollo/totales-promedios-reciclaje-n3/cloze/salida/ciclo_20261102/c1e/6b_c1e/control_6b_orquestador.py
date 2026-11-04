#!/usr/bin/env python3
"""control_6b_orquestador.py — tanda C1e: repite sobre out.json, con código propio, las frecuencias de H2 y las métricas del párrafo que
reporta el auditor, sin usar contar_h2.py ni agg.py. Renglones unidos con espacio y U+00A0 normalizado; un corte «x ⏎ y» se busca por
pares de renglones consecutivos. Además compara los renglones del párrafo de la referencia r con los del NUEVO de la C1d (mismo md5,
out.json no conservado: se comparan las cifras, no los renglones). Uso: python3 -I control_6b_orquestador.py <out.json>"""
import collections, json, sys
NB = chr(0xA0)
d = json.load(open(sys.argv[1], encoding="utf-8"))
T = collections.defaultdict(lambda: collections.Counter())
for r in d:
    k = (r["v"], r["vw"]); t = T[k]; m = r["m"]; p = m["p6"]
    L = [x.replace(NB, " ").strip() for x in p["lines"]]; tx = " ".join(L)
    t["pag"] += 1; t["reng"] += len(L); t["H"] += m["H"]; t["w1"] += p["lastWords"] == 1; t["u1"] += p["lastTok"] == 1
    t["ovf"] += bool(p["ovf"]); t["split"] += p["tokSplit"]; t["kcut"] += p["kcut"]; t["kn"] += p["kn"]
    par = list(zip(L, L[1:]))
    cut = lambda x, y: any(a.split(" ")[-len(x.split(" ")):] == x.split(" ") and b.startswith(y) for a, b in par)
    for nom, frase, x, y in (("A", "habría ganado.", "habría", "ganado."), ("B", "habría seguido ganando.", "habría", "seguido "),
                             ("C", "no le habría alcanzado", "no le", "habría alcanzado"), ("D", "habría sumado", "habría", "sumado"),
                             ("G", "habría ganado con", "habría", "ganado con"), ("H", "habría perdido con", "habría", "perdido con")):
        if frase in tx:
            t[nom + "_d"] += 1
            t[nom + "_n"] += cut(x, y)
    t["AB"] += cut("habría", "ganado.") or cut("habría", "seguido ")
for k in sorted(T, key=lambda z: (z[0], -z[1])):
    t = T[k]
    print(f"{k[0]} {k[1]}: páginas {t['pag']} · renglones media {t['reng']/t['pag']:.2f} · suma H {t['H']} · última 1 palabra {t['w1']} · última 1 unidad {t['u1']} · "
          f"cifra/kilos {t['kcut']}/{t['kn']} · desborde {t['ovf']} · partidas {t['split']} · A {t['A_n']}/{t['A_d']} · B {t['B_n']}/{t['B_d']} · A o B {t['AB']} · "
          f"C {t['C_n']}/{t['C_d']} · D {t['D_n']}/{t['D_d']} · G {t['G_n']}/{t['G_d']} · H {t['H_n']}/{t['H_d']}")
