import json,collections,sys,re
res=json.load(open(sys.argv[1])); out={}
for r in res:
    k=f"{r['v']}_{r['vw']}"; m=r['m']; d=out.setdefault(k,collections.Counter()); L=out.setdefault(k+"_p6",collections.defaultdict(list))
    d['semillas']+=1; d['H_sum']+=m['H']
    if m['docSW']>m['iw'] or m['bodySW']>m['bodyCW']: d['desborde_pag']+=1
    d['spills']+=len(m['spills']); d['tablas_fuera']+=sum(t['outside'] for t in m['tables']); d['tablas_scroll']+=sum(t['needsScroll'] for t in m['tables'])
    d['mathSplit']+=m['mathSplit']
    for p in m['pairs']:
        kk=p['k']+('(tabla)' if p['intd'] else ''); d[f"{p['reg']}|{kk}|n"]+=1; d[f"{p['reg']}|{kk}|sp"]+=int(p['sp'])
    for p in m['esp']: d[f"{p['reg']}|espacio-inicial|n"]+=1; d[f"{p['reg']}|espacio-inicial|sp"]+=int(p['sp'])
    for p in m['punt']:
        kk='signo-tras-formula' if p['fm'] else 'signo-tras-otro'; d[f"{p['reg']}|{kk}|n"]+=1; d[f"{p['reg']}|{kk}|sp"]+=int(p['sp'])
    p6=m['p6']
    if p6.get('missing'): d['p6_missing']+=1; continue
    L['n'].append(p6['n']); d['p6_lastWords1']+=int(p6['lastWords']==1); d['p6_lastTok1']+=int(p6['lastTok']==1)
    d['p6_kn']+=p6['kn']; d['p6_kcut']+=p6['kcut']; d['p6_ovf']+=int(p6['ovf']); d['p6_tokSplit']+=p6['tokSplit']; d['p6_nb']+=p6['nb']
    L['lastWords'].append(p6['lastWords']); L['lastText'].append((r['seed'],p6['lastText']))
fin={}
for k,v in out.items():
    if k.endswith('_p6'):
        w=v['lastWords']; n=v['n']
        fin[k]={'renglones_media':sum(n)/len(n),'renglones_min':min(n),'renglones_max':max(n),'hist_renglones':dict(collections.Counter(n)),'hist_palabras_ultimo':dict(sorted(collections.Counter(w).items())),'ejemplos_ultimo':v['lastText'][:3]}
    else: fin[k]=dict(sorted(v.items()))
json.dump(fin,open(sys.argv[2],'w'),indent=1,ensure_ascii=False)
for k,v in fin.items():
    print("==",k); print(json.dumps(v,ensure_ascii=False))
