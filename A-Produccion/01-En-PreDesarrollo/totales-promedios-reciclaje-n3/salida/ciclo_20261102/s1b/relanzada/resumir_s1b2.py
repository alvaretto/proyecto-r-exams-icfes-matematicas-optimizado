"""Tanda S1b relanzada: vector completo por versión y ancho, sobre metricas de cap_s1b2.py (N = 100 semillas).
Uso: python3 -I resumir_s1b2.py <metricas.json> <v1,v2,...> [referencia_para_alturas]"""
import sys, json, collections
M = json.load(open(sys.argv[1])); vers = sys.argv[2].split(','); ref = sys.argv[3] if len(sys.argv) > 3 else None
idx = {(r['v'], r['seed'], r['vw']): r for r in M}
print('errores de captura:', sum('error' in r for r in M), '· anchos de ventana medidos (innerWidth):', sorted({r['m']['iw'] for r in M if 'm' in r}))
for vw in sorted({r['vw'] for r in M}, reverse=True):
    print(f'--- {vw} px (N = 100 semillas)')
    for v in vers:
        R = [idx[(v, s, vw)]['m'] for s in range(1, 101)]
        fu = [p for m in R for p in m['pairs'] if p['k'].startswith('formula-unidad')]
        ph = [p for m in R for p in m['punt'] if p['fm']]
        pa = [p for m in R for p in m['punt'] if not p['fm']]
        es = [p for m in R for p in m['esp']]
        cu = [p for m in R for p in m['pairs'] if p['k'] == 'cifra-unidad']
        el = [p for m in R for p in m['pairs'] if p['k'] == 'El-Bosque']
        sem_ph = [s for s, m in zip(range(1, 101), R) if any(p['sp'] and p['fm'] for p in m['punt'])]
        porc = dict(collections.Counter(p['t'] for p in ph if p['sp']))
        print(f"{v:5s} fórmula+unidad partidas {sum(p['sp'] for p in fu):3d}/{len(fu)}"
              f" | signo huérfano tras una fórmula {sum(p['sp'] for p in ph):3d}/{len(ph)} {porc} en {len(sem_ph)} semillas {sem_ph[:15]}"
              f" | tras otro elemento {sum(p['sp'] for p in pa)}/{len(pa)}"
              f" | renglón con espacio inicial tras una fórmula {sum(p['sp'] for p in es):3d}/{len(es)}"
              f" | cifra-unidad {sum(p['sp'] for p in cu)}/{len(cu)} | El/La {sum(p['sp'] for p in el)}/{len(el)}"
              f" | fórmula partida por dentro {sum(m['mathSplit'] for m in R)}"
              f" | desbordes {sum(bool(m['spills']) for m in R)} | tabla fuera de su caja {sum(t['outside'] for m in R for t in m['tables'])}"
              f" (tablas {sum(len(m['tables']) for m in R)})")
        if ref and v != ref:
            H = [m['H'] for m in R]; HR = [idx[(ref, s, vw)]['m']['H'] for s in range(1, 101)]
            d = [a - b for a, b in zip(H, HR)]
            print(f"      alto frente a {ref}: más alto en {sum(x > 0 for x in d)}, más bajo en {sum(x < 0 for x in d)}, máx ±{max(abs(x) for x in d)} px")
