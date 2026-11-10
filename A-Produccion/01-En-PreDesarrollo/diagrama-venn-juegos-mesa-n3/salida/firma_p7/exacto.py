import numpy as np, itertools, time, collections
t0 = time.time()
# Enumeración independiente del espacio del .Rmd (líneas 109-113): 11 × 10 × 10^5 = 11 000 000
d5 = np.array(list(itertools.product(range(10), repeat=5)), dtype=np.int32)  # s1, s2, p13, p23, s3
def opciones(ctr, p12):
    piso = max(0, p12 - ctr); piso3 = max(0, ctr - p12)
    n = len(d5)
    s1 = ctr + d5[:,0]; s2 = ctr + d5[:,1]; p13 = piso + d5[:,2]; p23 = piso + d5[:,3]; s3 = piso3 + d5[:,4]
    c_ = np.full(n, ctr, np.int32); p_ = np.full(n, p12, np.int32)
    a = p_; b = p_ + c_; c = p23 + c_; d = p13 + c_
    P = s1 + p_ + p13 + c_; M = s2 + p_ + p23 + c_; B = s3 + p13 + p23 + c_
    z = b - a
    K = np.stack([P-a-d, a, M-a-c, z, d-z, c-z, B-d-c+z], 1)
    A = np.stack([P-b-d, b, M-b-c, a, d-a, c-a, B-d-c+a], 1)
    Bm = np.stack([P, a, M, z, d-z, c-z, B], 1)
    D = np.stack([P, b, M, a, d, c, B], 1)
    O = np.stack([K, A, Bm, D], 1)      # N × 4 × 7
    E = np.stack([a, b, c, d, P, M, B], 1)  # N × 7
    return O, E
def reglas(O, E):
    sm = O.sum(2); gr = O.max(2); ct = O[:,:,3]
    dosc = (O >= 10).sum(2)
    nch = (1 + (O >= 10) + (O >= 100)).sum(2) + 6
    so = np.sort(O, 2); dist = 1 + (so[:,:,1:] != so[:,:,:-1]).sum(2); rep = 7 - dist
    eq = (O[:,:,None,:] == O[:,None,:,:]).sum(3)  # N×4×4
    conv = eq.sum(2) - 7
    lit = (O[:,:,:,None] == E[:,None,None,:]).any(3).sum(2)
    rk2 = lambda m: (m == np.sort(m,1)[:,[1]]) if False else None
    def rmin(m):  # rank(ties.method='min')
        return (m[:,:,None] > m[:,None,:]).sum(2) + 1
    R = collections.OrderedDict()
    R['MAG-suma-max'] = sm == sm.max(1, keepdims=True)
    R['MAG-suma-min'] = sm == sm.min(1, keepdims=True)
    R['MAG-suma-segunda-menor'] = rmin(sm) == 2
    R['MAG-suma-segunda-mayor'] = rmin(-sm) == 2
    R['MAG-suma-no-extrema'] = (sm != sm.max(1,keepdims=True)) & (sm != sm.min(1,keepdims=True))
    R['MAG-mayor-numero'] = gr == gr.max(1,keepdims=True)
    R['MAG-menor-maximo'] = gr == gr.min(1,keepdims=True)
    R['MAG-sin-el-mayor-numero'] = gr != gr.max(1,keepdims=True)
    R['MAG-centro-mayor'] = ct == ct.max(1,keepdims=True)
    R['MAG-centro-menor'] = ct == ct.min(1,keepdims=True)
    R['DIV-suma-par'] = sm % 2 == 0
    R['DIV-suma-impar'] = sm % 2 == 1
    R['DIV-suma-mult5'] = sm % 5 == 0
    R['DIV-centro-par'] = ct % 2 == 0
    R['DIV-centro-impar'] = ct % 2 == 1
    R['SIG-tiene-cero'] = (O == 0).sum(2) > 0
    R['SIG-sin-cero'] = (O == 0).sum(2) == 0
    R['FOR-mas-dos-cifras'] = dosc == dosc.max(1,keepdims=True)
    R['FOR-menos-dos-cifras'] = dosc == dosc.min(1,keepdims=True)
    R['FOR-cadena-larga'] = nch == nch.max(1,keepdims=True)
    R['FOR-cadena-corta'] = nch == nch.min(1,keepdims=True)
    R['LEX-con-repetidos'] = rep > 0
    R['LEX-sin-repetidos'] = rep == 0
    R['REL-mas-coincidencias'] = conv == conv.max(1,keepdims=True)
    R['REL-menos-coincidencias'] = conv == conv.min(1,keepdims=True)
    R['REL-no-extrema-en-coincidencias'] = (conv != conv.max(1,keepdims=True)) & (conv != conv.min(1,keepdims=True))
    R['EST-mas-cifras-del-enunciado'] = lit == lit.max(1,keepdims=True)
    R['EST-menos-cifras-del-enunciado'] = lit == lit.min(1,keepdims=True)
    R['EST-total-en-zona-de-un-conjunto'] = (O[:,:,[0,2,6],None] == E[:,None,None,4:7]).any(3).sum(2) == 0
    R['EST-primer-dato-en-la-zona-doble'] = O[:,:,1] == E[:,[0]]
    return R, dict(sm=sm, gr=gr, lit=lit, conv=conv)
def puntaje(S):
    k = S.sum(1)
    return np.where((k == 0) | (k == 4), 0.25, S[:,0] / np.maximum(k, 1))
suma = collections.OrderedDict(); n = 0
m_acum = collections.defaultdict(list)
filtros = {'lit(K)>=lit(A)': lambda M: M['lit'][:,0] >= M['lit'][:,1],
           'lit(K)>=2': lambda M: M['lit'][:,0] >= 2,
           'lit(K)>=3': lambda M: M['lit'][:,0] >= 3}
f_s = {f: collections.OrderedDict() for f in filtros}; f_n = dict.fromkeys(filtros, 0)
lit_k = collections.Counter(); unico = 0; empate = 0
for ctr in range(2, 13):
    for p12 in range(2, 13):
        if p12 == ctr: continue
        O, E = opciones(ctr, p12)
        assert (O >= 0).all() and (O <= 99).all()
        R, M = reglas(O, E)
        n += len(O)
        for nom, S in R.items():
            suma[nom] = suma.get(nom, 0.0) + puntaje(S).sum()
        # márgenes: clave contra su rival más próximo en la métrica de la regla (§P7-B)
        sm = M['sm'].astype(float); dif = np.abs(sm[:,[1,2,3]] - sm[:,[0]]); rel = dif / np.maximum(sm[:,[1,2,3]], sm[:,[0]])
        m_acum['suma'].append(np.percentile(rel.min(1), [0,10,50,90]))
        m_acum['suma_lt15'].append((rel.min(1) < .15).mean())
        gr = M['gr'].astype(float)
        S = R['MAG-menor-maximo']; f = S[:,0] & (S.sum(1) < 4)
        otros = gr[:,1:]; riv = np.where(S[:,1:], np.inf, otros).min(1)
        rg = (riv[f] - gr[f,0]) / riv[f]
        if f.any(): m_acum['gr_menor'].append((rg < .15).sum()); m_acum['gr_menor_n'].append(f.sum())
        S = R['MAG-sin-el-mayor-numero']; f = S[:,0]
        rg2 = (gr[f].max(1) - gr[f,0]) / gr[f].max(1)
        if f.any(): m_acum['gr_sin'].append((rg2 < .15).sum()); m_acum['gr_sin_n'].append(f.sum())
        lit = M['lit']
        for v, c in zip(*np.unique(lit[:,0], return_counts=True)): lit_k[int(v)] += int(c)
        S = R['EST-menos-cifras-del-enunciado']; k = S.sum(1)
        unico += int(((k == 1) & S[:,0]).sum()); empate += int(((k > 1) & (k < 4) & S[:,0]).sum())
        for f, fn in filtros.items():
            sel = fn(M); f_n[f] += int(sel.sum())
            if sel.any():
                for nom, S in R.items():
                    f_s[f][nom] = f_s[f].get(nom, 0.0) + puntaje(S[sel]).sum()
print('versiones', n, 'seg', round(time.time()-t0))
print('--- tasas exactas (independientes) ---')
tn = 32.2
for nom, s in sorted(suma.items(), key=lambda kv: -kv[1]):
    r = 100*s/n
    print(f'{nom:36s} {r:7.3f} %  exceso {r-tn:+.1f} pp')
print('--- EST-menos-cifras: clave con cuántas cifras del enunciado ---')
for v in sorted(lit_k): print(v, f'{100*lit_k[v]/n:.2f} %')
print('clave sola en el mínimo', f'{100*unico/n:.2f} %', '| clave empatada en el mínimo', f'{100*empate/n:.2f} %')
print('--- margen ---')
print('suma, clave vs rival más próximo: versiones con margen < 15 %:', f"{100*np.mean(m_acum['suma_lt15']):.1f} %", 'pct 0/10/50/90 medios', np.round(100*np.mean(m_acum['suma'],0),1))
print('MAG-menor-maximo, margen < 15 % cuando favorece a la clave:', f"{100*sum(m_acum['gr_menor'])/sum(m_acum['gr_menor_n']):.1f} %")
print('MAG-sin-el-mayor-numero, margen < 15 %:', f"{100*sum(m_acum['gr_sin'])/sum(m_acum['gr_sin_n']):.1f} %")
print('--- filtros hipotéticos ---')
for f in filtros:
    print(f, 'quedan', f'{100*f_n[f]/n:.2f} %', 'de las versiones')
    top = sorted(f_s[f].items(), key=lambda kv: -kv[1])[:9]
    print('   ', ' · '.join(f'{nom} {100*s/f_n[f]:.1f}' for nom, s in top))
