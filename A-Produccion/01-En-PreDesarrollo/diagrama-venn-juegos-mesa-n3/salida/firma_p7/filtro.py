import numpy as np, runpy, collections, sys, io, contextlib
src = open('exacto.py', encoding='utf-8').read().split("suma = collections.OrderedDict(); n = 0")[0]
exec(src)
n = 0; nf = 0
c = collections.Counter()
for ctr in range(2, 13):
    for p12 in range(2, 13):
        if p12 == ctr: continue
        O, E = opciones(ctr, p12)
        lit = (O[:,:,:,None] == E[:,None,None,:]).any(3).sum(2)
        sel = lit[:,0] >= lit[:,1]
        K = O[:,0,:]; Ks = K[sel]
        n += len(K); nf += int(sel.sum())
        c['cero_todo'] += int((K == 0).any(1).sum()); c['cero_fil'] += int((Ks == 0).any(1).sum())
        c['p13_0_todo'] += int((K[:,4] == 0).sum()); c['p13_0_fil'] += int((Ks[:,4] == 0).sum())
        c['p23_0_todo'] += int((K[:,5] == 0).sum()); c['p23_0_fil'] += int((Ks[:,5] == 0).sum())
        c['s3_0_todo'] += int((K[:,6] == 0).sum()); c['s3_0_fil'] += int((Ks[:,6] == 0).sum())
        c['claves_uni_fil'] += 0
        c['ctr_sum_todo'] += int(K[:,3].sum()); c['ctr_sum_fil'] += int(Ks[:,3].sum())
        # ¿el filtro cabe en cada par (centro, a)? aceptación mínima por pareja
        c['min_acept'] = min(c.get('min_acept', 10**9), int(sel.sum()))
print('versiones que quedan', nf, f'{100*nf/n:.2f} %')
for k in ['cero', 'p13_0', 'p23_0', 's3_0']:
    print(k, f"todo {100*c[k+'_todo']/n:.2f} %  filtrado {100*c[k+'_fil']/nf:.2f} %")
print('centro medio', round(c['ctr_sum_todo']/n, 2), round(c['ctr_sum_fil']/nf, 2), '| aceptación mínima en una pareja (centro, a):', f"{100*c['min_acept']/100000:.1f} %")
