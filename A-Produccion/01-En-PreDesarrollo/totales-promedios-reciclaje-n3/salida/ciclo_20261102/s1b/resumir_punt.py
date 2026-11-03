"""S1b: vector completo por versión y ancho: fórmula+unidad partidas y signo (.,:;) huérfano tras <math> o <span>."""
import sys,json,collections
M=json.load(open(sys.argv[1])); vers=sys.argv[2].split(','); idx={(r['v'],r['seed'],r['vw']):r for r in M}
print('errores de captura:',sum('error' in r for r in M))
for vw in (360,320):
  print(f'--- {vw} px (N = 100 semillas)')
  for v in vers:
    R=[idx[(v,s,vw)]['m'] for s in range(1,101)]
    fu=sum(p['sp'] for m in R for p in m['pairs'] if p['k'].startswith('formula-unidad')); nfu=sum(1 for m in R for p in m['pairs'] if p['k'].startswith('formula-unidad'))
    pu=[p for m in R for p in m.get('punt',[])]; ph=sum(p['sp'] for p in pu); sem=sum(any(p['sp'] for p in m.get('punt',[])) for m in R)
    porc=collections.Counter(p['t'] for p in pu if p['sp'])
    print(f"{v:6s} fórmula+unidad partidas {fu:3d}/{nfu} | signo huérfano tras la fórmula {ph:3d}/{len(pu)} en {sem} semillas {dict(porc)} | desbordes {sum(bool(m['spills']) for m in R)} | cifra-unidad {sum(p['sp'] for m in R for p in m['pairs'] if p['k']=='cifra-unidad')}")
