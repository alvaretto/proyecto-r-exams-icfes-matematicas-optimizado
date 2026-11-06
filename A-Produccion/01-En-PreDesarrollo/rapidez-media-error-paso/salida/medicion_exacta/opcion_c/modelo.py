#!/usr/bin/env python3
# Enumerador independiente: NO usa el R del verificador ni clasificar_o1. Reescribe el generador desde el .Rmd.
import sys, statistics, os
VAR=os.environ.get('VAR','base')
NUEVAS=('EV-D-PROM','EV-T-CUENTA')
from fractions import Fraction as Fr
from itertools import combinations, product
from collections import defaultdict

ROLES=['dist','tiem','coc']; META={'dist':[3,3,2,0],'coc':[2,3,2,0],'tiem':[0,0,1,3]}
MULT={2:10,3:15,4:20,5:10}

def filas():
    o=[]
    for t1 in range(2,11):
        for dur in range(2,6):
            for v in range(30,95,5):
                t2=t1+dur; D=v*dur
                if ((t1>=6 and t2<=11) or (t1<=5 and t2<=9)) and dur!=t1 and D%t2==0 and D<=360: o.append((t1,dur,v,t2,D))
    return o

def pool(st,signo,sin_prec_final=False):
    t1,t2,d1,d2,D,dur,v,rol,err,Dm,Tm=st
    P=[]
    if rol!='dist' or sin_prec_final: P.append(('EV-D-FINAL','dist',d2))
    if err!='EV-D-SUMA': P.append(('EV-D-SUMA','dist',d1+d2))
    if not(signo and err=='EV-D-SIGNO'): P.append(('EV-D-SIGNO','dist',-D))
    P.append(('EV-D-INICIAL','dist',d1))
    if rol!='tiem' or sin_prec_final: P.append(('EV-T-FINAL','tiem',t2))
    P.append(('EV-T-SUMA','tiem',t1+t2))
    if not(signo and err=='EV-T-SIGNO'): P.append(('EV-T-SIGNO','tiem',-dur))
    P.append(('EV-T-INICIAL','tiem',t1))
    P.append(('EV-C-INV','coc',(Tm,Dm,'h')))
    P.append(('EV-C-CIFRAS','coc',(Tm,Dm,'km')))
    if rol!='coc': P.append(('EV-C-UNIDAD','coc',(Dm,Tm,'h')))
    if VAR!='base':
        if (d1+d2)%2==0:
            x=(d1+d2)//2
            if x not in (D,Dm,d1,d2): P.append(('EV-D-PROM','dist',x))
        x=dur+1
        if x not in (t1,t2,Tm,dur): P.append(('EV-T-CUENTA','tiem',x))
    return P

def estados(signo=False,sin_prec_final=False):
    F=filas(); errs={'dist':['EV-D-SUMA','EV-D-FINAL']+(['EV-D-SIGNO'] if signo else []),
                     'tiem':['EV-T-FINAL']+(['EV-T-SIGNO'] if signo else []),'coc':['EV-C-UNIDAD']}
    for (t1,dur,v,t2,D) in F:
        m=MULT[dur]; cand=[x for x in range(m,2*v+1,m) if x!=v and x!=D]
        for d1 in cand:
            d2=d1+D
            for rol in ROLES:
                for err in errs[rol]:
                    Dm={'EV-D-SUMA':d1+d2,'EV-D-FINAL':d2,'EV-D-SIGNO':-D}.get(err,D)
                    Tm={'EV-T-FINAL':t2,'EV-T-SIGNO':-dur}.get(err,dur)
                    st=(t1,t2,d1,d2,D,dur,v,rol,err,Dm,Tm)
                    for rp in ROLES:
                        ps=Fr(1,len(F))*Fr(1,len(cand))*Fr(1,3)*Fr(1,len(errs[rol]))*(Fr(1,2) if rp==rol else Fr(1,4))
                        P=pool(st,signo,sin_prec_final)
                        def repara(c):
                            _,r,val=c
                            if r=='dist': return val==v*Tm
                            if r=='tiem': return Dm==v*val
                            return False
                        apl=[c for c in P if not repara(c)]
                        por={r:[c for c in apl if c[1]==r] for r in ROLES}
                        n={r:1 for r in ROLES}; n[rp]+=1; n[rol]-=1
                        combs=[list(combinations(por[r],n[r])) for r in ROLES]
                        C=[sum(x,()) for x in product(*combs)]
                        C=[s for s in C if sum(c[0].endswith('SIGNO') for c in s)<=1]
                        if VAR=='c2':
                            C2=[s for s in C if any(c[0] in NUEVAS for c in s)]
                            if C2: C=C2
                        if not C: yield ('ABORTA',st,rp,ps); continue
                        pk={'dist':D,'tiem':dur,'coc':D}[rol]
                        val1=lambda c:(c[2][0] if c[1]=='coc' else c[2])
                        rg=[1+sum(val1(c)>pk for c in s) for s in C]
                        cnt=defaultdict(int)
                        for r in rg: cnt[r]+=1
                        w=[Fr(META[rol][r-1],cnt[r]) for r in rg]
                        tw=sum(w)
                        if tw==0: w=[Fr(1)]*len(C); tw=Fr(len(C))
                        yield ('OK',st,rp,ps,C,[x/tw for x in w])

def opciones(st,s):
    """Lista de opciones con la clave primero."""
    t1,t2,d1,d2,D,dur,v,rol,err,Dm,Tm=st
    rech={'dist':(Dm,'km'),'tiem':(Tm,'h'),'coc':(D,dur,'h') if err=='EV-C-UNIDAD' else (Dm,Tm,'km')}
    ops=[]
    if rol=='coc': ops.append(dict(cod='CLAVE',paso=3,num=D,den=dur,u='km',rech=rech['coc']))
    else: ops.append(dict(cod='CLAVE',paso=1 if rol=='dist' else 2,num=D if rol=='dist' else dur,rech=rech[rol]))
    for c,r,val in s:
        if r=='coc': ops.append(dict(cod=c,paso=3,num=val[0],den=val[1],u=val[2],rech=rech['coc']))
        else: ops.append(dict(cod=c,paso=1 if r=='dist' else 2,num=val,rech=rech[r]))
    return ops

def sc(S):
    k=sum(S)
    if k==0 or k==len(S): return Fr(1,4)
    return Fr(1,k) if S[0] else Fr(0)

def nums_en(st): t1,t2,d1,d2,*_=st; return {t1,t2,d1,d2,0}
def pasos(st):
    t1,t2,d1,d2,D,dur,v,rol,err,Dm,Tm=st
    p1={'EV-D-SUMA':(d2,d1,d1+d2),'EV-D-FINAL':(d2,0,d2),'EV-D-SIGNO':(d1,d2,D)}.get(err,(d2,d1,D))
    p2={'EV-T-FINAL':(t2,0,t2),'EV-T-SIGNO':(t1,t2,dur)}.get(err,(t2,t1,dur))
    p3=(D,dur,v) if err=='EV-C-UNIDAD' else (abs(Dm),abs(Tm),abs(Fr(Dm,Tm)))
    return p1,p2,p3

def sustv(o,st):
    Dm,Tm=st[9],st[10]
    if o['paso']==1: return Fr(o['num'],Tm),True
    if o['paso']==2: return Fr(Dm,o['num']),True
    return Fr(o['num'],o['den']),o['u']=='km'

# ---- reglas firmadas (reimplementadas) -------------------------------------------------
def x0(o,st): return [abs(x['num']) not in nums_en(st) for x in o]
def conj(S): return S if any(S) and not all(S) else [False]*len(S)
def est_nuevo(o,st,rot=True):
    p=pasos(st); N=set(nums_en(st))|set(p[0])|set(p[1])|set(int(x) if x==int(x) else -1 for x in p[2])
    if rot: N|={1,2,3}
    return [abs(x['num']) not in N for x in o]
def mag2(o,st):
    x=[y['num'] for y in o]; r=[1+sum(b>a for b in x) for a in x]; return [q==2 for q in r]
def xa(o,st): return [sustv(x,st)[0]>0 and sustv(x,st)[1] for x in o]
def xb(o,st): return [sustv(x,st)[0].denominator==1 for x in o]
def R(lo,hi):
    return lambda o,st:[sustv(x,st)[1] and lo<=sustv(x,st)[0]<=hi for x in o]
def q(o,st): return [isinstance(x['rech'][0],int) and x['rech'][0]<0 and x['num']==-x['rech'][0] for x in o] if True else None
