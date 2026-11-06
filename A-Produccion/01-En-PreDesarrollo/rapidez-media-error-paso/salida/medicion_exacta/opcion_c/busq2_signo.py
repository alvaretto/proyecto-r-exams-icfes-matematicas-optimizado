import sys,time
sys.path.insert(0,'.')
from modelo import *
import numpy as np, itertools
NOM=['p1','p2','p3','pos','en_enun','en_pasos','rech_en_enun','mayor_rech','menor_rech','igual_rech','U','paso_rep','paso_con_neg','paso_con_final','sobre_cota','frac_hkm','max_op','min_op','mult5','mult10','rech_mult5','rech_par','citada_otra','par','opuesto','frac_menor1','entre','menor_mismo_paso','mayor_mismo_paso']
MARCA={'EV-C-UNIDAD':3,'EV-D-SUMA':1,'EV-D-FINAL':1,'EV-T-FINAL':2,'EV-D-SIGNO':1,'EV-T-SIGNO':2}
def bits(o,st):
    t1,t2,d1,d2=st[:4]; en=nums_en(st); p=pasos(st); pp=set(p[0])|set(p[1])|{int(x) for x in p[2] if x==int(x)}
    ns=[x['num'] for x in o]; mx=max(ns); mn=min(ns)
    from collections import Counter
    cp=Counter(x['paso'] for x in o)
    neg={x['paso'] for x in o if x['num']<0}; fin={x['paso'] for x in o if (x['paso']==1 and x['num']==d2) or (x['paso']==2 and x['num']==t2)}
    res=[]
    for x in o:
        n=x['num']; r=x['rech'][0]; f=x['paso']==3
        L=[x['paso']==1,x['paso']==2,f,n>0,abs(n) in en,abs(n) in pp,r in en,n>r,n<r,n==r,f and (n,x['den'])==tuple(x['rech'][:2]),cp[x['paso']]==2,x['paso'] in neg,x['paso'] in fin,
           (not f) and n>(d2 if x['paso']==1 else t2),f and x['u']=='h',n==mx,n==mn,n%5==0,n%10==0,r%5==0,r%2==0,
           any(abs(n) in ([abs(y['num'])]+([y['den']] if 'den' in y else [])) for y in o if y is not x),n%2==0,n==-r,f and n<x['den'],(not f) and ((d1<n<d2) if x['paso']==1 else (t1<n<t2)),(not f) and cp[x['paso']]==2 and n==min(y['num'] for y in o if y['paso']==x['paso']),(not f) and cp[x['paso']]==2 and n==max(y['num'] for y in o if y['paso']==x['paso'])]
        res.append(sum(1<<i for i,b in enumerate(L) if b))
    return res,[x['paso']==MARCA[st[8]] for x in o]
def construir(signo):
    A=[];W=[];M=[]
    for e in estados(signo):
        _,st,rp,ps,C,w=e
        for s,wi in zip(C,w):
            if wi==0: continue
            o=opciones(st,s); b,m=bits(o,st); A.append(b); W.append(float(ps*wi)); M.append(m)
    return np.array(A,dtype=np.uint32),np.array(W),np.array(M,dtype=bool)
def lits(A):
    L={}
    for i,n in enumerate(NOM):
        b=((A>>i)&1).astype(bool); L[n]=b; L['no_'+n]=~b
    return L
def punt(S,W,kp=None):
    k=S.sum(1); h=S[:,0] if kp is None else S[np.arange(len(S)),kp]
    with np.errstate(divide='ignore',invalid='ignore'):
        v=np.where((k==0)|(k==4),.25,h/k)
    return float((W*v).sum()/W.sum())
def buscar(L,W,prof,base=None,kp=None,excl=()):
    nom=[n for n in L if n.replace('no_','') not in excl]; out=[]
    for r in range(1,prof+1):
        for c in itertools.combinations(nom,r):
            S=L[c[0]] if base is None else base&L[c[0]]
            for x in c[1:]: S=S&L[x]
            out.append((punt(S,W,kp),c))
    out.sort(key=lambda t:-t[0]); return out
t=time.time()
A,W,M=construir(True); L=lits(A); print('nuevo',A.shape,time.time()-t,flush=True)
ex=('opuesto',)
r=buscar(L,W,2,excl=ex)
print('N combos',len(r))
print('--- TOP 1 literal')
for v,c in [x for x in r if len(x[1])==1][:9]: print('%6.2f %+6.2f  %s'%(100*v,100*v-28.8,' & '.join(c)))
print('--- TOP 2 literales')
for v,c in [x for x in r if len(x[1])==2][:9]: print('%6.2f %+6.2f  %s'%(100*v,100*v-28.8,' & '.join(c)))
print('tiempo',round(time.time()-t))
