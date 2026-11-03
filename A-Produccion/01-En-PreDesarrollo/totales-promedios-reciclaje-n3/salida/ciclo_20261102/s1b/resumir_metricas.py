"""S1b: resumen pareado de las métricas HTML (100 semillas x 2 anchos)."""
import sys,json,collections
M=json.load(open(sys.argv[1])); vers=sys.argv[2].split(','); base=sys.argv[3] if len(sys.argv)>3 else None
idx={(r['v'],r['seed'],r['vw']):r for r in M}
print('errores de captura:',sum('error' in r for r in M),' iw:',sorted({r['m']['iw'] for r in M if 'm' in r}))
for vw in (360,320):
  print(f'--- {vw} px')
  for v in vers:
    R=[idx[(v,s,vw)]['m'] for s in range(1,101)]
    c=collections.Counter(); n=collections.Counter()
    for m in R:
      for p in m['pairs']:
        k=p['k']+('-tabla' if p.get('intd') else '')
        n[k]+=1; c[k]+=bool(p['sp'])
    fu=c['formula-unidad']+c['formula-unidad-dentro']; nfu=n['formula-unidad']+n['formula-unidad-dentro']
    sp=sum(bool(m['spills']) for m in R); to=sum(any(t['outside'] for t in m['tables']) for m in R)
    mr=sum(p.get('rects',1)>1 for m in R for p in m['pairs'] if p['k']=='formula-unidad-dentro')
    nw=sum(m.get('nowrap',0) for m in R)
    print(f"{v:5s} formula+unidad partidas {fu:3d}/{nfu} (fuera {c['formula-unidad']}/{n['formula-unidad']}, dentro {c['formula-unidad-dentro']}/{n['formula-unidad-dentro']}, math en >1 caja {mr})"
          f" | cifra-unidad texto {c['cifra-unidad']}/{n['cifra-unidad']} | El/La {c['El-Bosque']}/{n['El-Bosque']} | desbordes {sp} | tabla fuera {to} | nowrap {nw}")
    if base and v!=base:
      dH=[m['H']-idx[(base,s,vw)]['m']['H'] for s,m in zip(range(1,101),R)]
      print(f"       alto vs {base}: más alto en {sum(d>0 for d in dH)}, más bajo en {sum(d<0 for d in dH)}, igual en {sum(d==0 for d in dH)}; máx {max(dH)} px, mín {min(dH)} px")
