"""Margen §P7-B de las reglas firmadas que no lo tenían medido (2026-11-06).
Usa las funciones de exacto.py. Margen: distancia relativa entre la clave y la opción eliminada más próxima,
en la métrica de la regla, solo cuando la regla favorece a la clave. EST-primer-dato y EST-total no tienen
gradación: comprueban si un número está o no, así que el margen no aplica."""
import numpy as np, collections
src = open('exacto.py', encoding='utf-8').read().split("suma = collections.OrderedDict(); n = 0")[0]
exec(src)
ac = collections.Counter()
for ctr in range(2, 13):
    for p12 in range(2, 13):
        if p12 == ctr: continue
        O, E = opciones(ctr, p12); R, M = reglas(O, E)
        for nom, met in (('EST-menos-cifras-del-enunciado', 'lit'), ('REL-no-extrema-en-coincidencias', 'conv')):
            S = R[nom]; m = M[met].astype(float); k = S.sum(1)
            f = S[:, 0] & (k < 4)
            with np.errstate(all='ignore'):
                par = np.abs(m[:, 1:] - m[:, [0]]) / np.maximum(np.maximum(m[:, 1:], m[:, [0]]), 1.0)
            rel = np.where(S[:, 1:], np.inf, par).min(1)     # rival eliminado más próximo, par a par
            ac[nom + '_n'] += int(f.sum()); ac[nom + '_lt15'] += int((rel[f] < .15).sum()); ac[nom + '_sum'] += float(rel[f].sum())
for nom in ('EST-menos-cifras-del-enunciado', 'REL-no-extrema-en-coincidencias'):
    n = ac[nom + '_n']
    print(f"{nom}: favorece a la clave en {n} versiones; margen < 15 % en {100*ac[nom+'_lt15']/n:.1f} %; margen medio {100*ac[nom+'_sum']/n:.1f} %")
