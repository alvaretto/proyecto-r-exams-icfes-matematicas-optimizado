#!/usr/bin/env python3
# Enumerador propio de la r4, escrito leyendo el .Rmd md5 904af1ca. No importa modelo.py ni usa metricas().
from fractions import Fraction as Fr
from itertools import combinations, product
from collections import defaultdict, Counter
ROLES=['dist','tiem','coc']
META={'dist':(3,3,2,0),'coc':(2,3,2,0),'tiem':(0,0,1,3)}
MULT={2:10,3:15,4:20,5:10}
ERRS={'dist':['EV-D-SUMA','EV-D-FINAL'],'tiem':['EV-T-FINAL'],'coc':['EV-C-UNIDAD']}
NUE=('EV-D-PROM','EV-T-CUENTA')
def espacio():
    F=[]
    for v in range(30,95,5):
        for dur in range(2,6):
            for t1 in range(2,11):
                t2=t1+dur; D=v*dur
                if not((t1>=6 and t2<=11) or (t1<=5 and t2<=9)): continue
                if dur==t1 or D%t2 or D>360: continue
                F.append((t1,dur,v))
    return F
def pool(s,o2=True):
    """s: dict del estado. Devuelve (cod,rol,val) en el orden del .Rmd."""
    d1,d2,t1,t2,D,dur,Dm,Tm,rol,err=[s[k] for k in 'd1 d2 t1 t2 D dur Dm Tm rol err'.split()]
    P=[]
    if rol!='dist': P.append(('EV-D-FINAL','dist',d2))
    if err!='EV-D-SUMA': P.append(('EV-D-SUMA','dist',d1+d2))
    P.append(('EV-D-SIGNO','dist',-D))
    P.append(('EV-D-INICIAL','dist',d1))
    if o2 and (d1+d2)%2==0:
        pr=(d1+d2)//2
        if pr not in (D,Dm,d1,d2): P.append(('EV-D-PROM','dist',pr))
    if rol!='tiem': P.append(('EV-T-FINAL','tiem',t2))
    P.append(('EV-T-SUMA','tiem',t1+t2))
    P.append(('EV-T-SIGNO','tiem',-dur))
    P.append(('EV-T-INICIAL','tiem',t1))
    if o2 and (dur+1) not in (t1,t2,Tm,dur): P.append(('EV-T-CUENTA','tiem',dur+1))
    P.append(('EV-C-INV','coc',(Tm,Dm,'h/km')))
    P.append(('EV-C-CIFRAS','coc',(Tm,Dm,'km/h')))
    if rol!='coc': P.append(('EV-C-UNIDAD','coc',(Dm,Tm,'h/km')))
    return P
def estados(oblig=True,o2=True):
    F=espacio()
    for (t1,dur,v) in F:
        t2=t1+dur; D=v*dur; m=MULT[dur]
        cand=[x for x in range(m,2*v+1,m) if x!=v and x!=D]
        for d1 in cand:
            d2=d1+D
            for rol in ROLES:
                for err in ERRS[rol]:
                    Dm={'EV-D-SUMA':d1+d2,'EV-D-FINAL':d2}.get(err,D)
                    Tm=t2 if err=='EV-T-FINAL' else dur
                    for rp in ROLES:
                        ps=Fr(1,len(F))*Fr(1,len(cand))*Fr(1,3)*Fr(1,len(ERRS[rol]))*(Fr(1,2) if rp==rol else Fr(1,4))
                        s=dict(t1=t1,t2=t2,dur=dur,v=v,D=D,d1=d1,d2=d2,rol=rol,err=err,rp=rp,Dm=Dm,Tm=Tm,ps=ps)
                        P0=pool(s,o2)
                        def repara(c):
                            if c[1]=='dist': return c[2]==v*Tm
                            if c[1]=='tiem': return Dm==v*c[2]
                            return False
                        s['repara_filtradas']=[c[0] for c in P0 if repara(c)]
                        P=[c for c in P0 if not repara(c)]
                        por={r:[c for c in P if c[1]==r] for r in ROLES}
                        n={r:1 for r in ROLES}; n[rp]+=1; n[rol]-=1
                        C=[sum(x,()) for x in product(*[list(combinations(por[r],n[r])) for r in ROLES])]
                        C=[x for x in C if sum(c[0].endswith('SIGNO') for c in x)<=1]
                        s['admite_nueva']=any(any(c[0] in NUE for c in x) for x in C)
                        if oblig and s['admite_nueva']:
                            C=[x for x in C if any(c[0] in NUE for c in x)]
                        pk={'dist':D,'tiem':dur,'coc':D}[rol]
                        vv=lambda c:(c[2][0] if c[1]=='coc' else c[2])
                        rg=[1+sum(vv(c)>pk for c in x) for x in C]
                        cnt=Counter(rg)
                        w=[Fr(META[rol][r-1],cnt[r]) for r in rg]
                        tw=sum(w)
                        if tw==0: w=[Fr(1)]*len(C); tw=Fr(len(C))
                        yield s,C,[x/tw for x in w]
def rech(s,paso):
    if paso==1: return (s['Dm'],'km')
    if paso==2: return (s['Tm'],'h')
    return (s['D'],s['dur'],'h/km') if s['err']=='EV-C-UNIDAD' else (s['Dm'],s['Tm'],'km/h')
def opciones(s,C):
    """clave primero."""
    pn={'dist':1,'tiem':2,'coc':3}
    o=[]
    r=s['rol']
    if r=='coc': o.append(dict(cod='CLAVE',paso=3,num=s['D'],den=s['dur'],u='km/h'))
    else: o.append(dict(cod='CLAVE',paso=pn[r],num=s['D'] if r=='dist' else s['dur'],den=None,u=None))
    for c,rr,val in C:
        if rr=='coc': o.append(dict(cod=c,paso=3,num=val[0],den=val[1],u=val[2]))
        else: o.append(dict(cod=c,paso=pn[rr],num=val,den=None,u=None))
    for x in o: x['r']=rech(s,x['paso'])
    return o
def verdad(s,x):
    # función de verdad propia: el paso señalado está mal y lo afirmado es el valor correcto
    ok1=(s['err'] not in ('EV-D-SUMA','EV-D-FINAL')); ok2=(s['err']!='EV-T-FINAL'); ok3=(s['err']!='EV-C-UNIDAD')
    if x['paso']==1: return (not ok1) and x['num']==s['D']
    if x['paso']==2: return (not ok2) and x['num']==s['dur']
    return (not ok3) and x['u']=='km/h' and x['num']==s['D'] and x['den']==s['dur']
def sust(s,x):
    if x['paso']==1: return Fr(x['num'],s['Tm']),'km/h'
    if x['paso']==2: return Fr(s['Dm'],x['num']),'km/h'
    return Fr(x['num'],x['den']),x['u']
