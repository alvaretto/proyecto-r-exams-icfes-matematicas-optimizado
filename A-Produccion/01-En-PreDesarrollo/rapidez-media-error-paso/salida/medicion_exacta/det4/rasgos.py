import sys
sys.path.insert(0,'.')
from gen import *
import numpy as np, itertools, time, pickle
from collections import Counter
NOM29=['p1','p2','p3','pos','en_enun','en_pasos','rech_en_enun','mayor_rech','menor_rech','igual_rech','U','paso_rep','paso_con_neg','paso_con_final','sobre_cota','frac_hkm','max_op','min_op','mult5','mult10','rech_mult5','rech_par','citada_otra','par','opuesto','frac_menor1','entre','menor_mismo_paso','mayor_mismo_paso']
EXTRA=['dif1_mismo_paso','dif1_menor','dif1_mayor','dif1_rech','paso_dif1_rech','media_lect','media_vis','paso_con_media','mitad_rech','paso_mitad_rech','par_suma','hay_dif1_rech','hay_media','ninguna_nueva','en_pasos3','cifra_nueva_paso','dif1_con_visible']
NOM=NOM29+EXTRA
def nums_en(s): return {s['t1'],s['t2'],s['d1'],s['d2'],0}
def visibles(s):
    p1=(s['d2'],0,s['d2']) if s['err']=='EV-D-FINAL' else ((s['d2'],s['d1'],s['d1']+s['d2']) if s['err']=='EV-D-SUMA' else (s['d2'],s['d1'],s['D']))
    p2=(s['t2'],0,s['t2']) if s['err']=='EV-T-FINAL' else (s['t2'],s['t1'],s['dur'])
    p3=(s['D'],s['dur'],s['v']) if s['err']=='EV-C-UNIDAD' else (s['Dm'],s['Tm'],s['Dm']//s['Tm'])
    return p1,p2,p3
def bits(s,o):
    d1,d2,t1,t2=s['d1'],s['d2'],s['t1'],s['t2']
    E=nums_en(s); P=visibles(s); pp=set(P[0])|set(P[1])|set(P[2])
    ns=[x['num'] for x in o]; mx=max(ns); mn=min(ns)
    cp=Counter(x['paso'] for x in o)
    neg={x['paso'] for x in o if x['num']<0}
    fin={x['paso'] for x in o if (x['paso']==1 and x['num']==d2) or (x['paso']==2 and x['num']==t2)}
    vis=sorted((E|pp)-{0})
    med=set()
    for a,b in itertools.combinations(vis,2):
        if (a+b)%2==0: med.add((a+b)//2)
    # opción por opción
    res=[]
    dif1rech=[(x['paso']<3 and x['num']==x['r'][0]+1) for x in o]
    mediaL=[(x['paso']==1 and x['num']*2==d1+d2) for x in o]
    mitad=[(x['paso']<3 and x['num']*2==x['r'][0]) for x in o]
    nueva=any(x['cod'] in NUE for x in o)
    for i,x in enumerate(o):
        n=x['num']; r=x['r'][0]; f=x['paso']==3
        mismos=[y['num'] for y in o if y is not x and y['paso']==x['paso'] and y['paso']<3]
        dif=[m for m in mismos if abs(m-n)==1]
        L=[x['paso']==1,x['paso']==2,f,n>0,abs(n) in E,abs(n) in pp,r in E,n>r,n<r,n==r,
           f and (n,x['den'])==tuple(x['r'][:2]),cp[x['paso']]==2,x['paso'] in neg,x['paso'] in fin,
           (not f) and n>(d2 if x['paso']==1 else t2),f and x['u']=='h/km',n==mx,n==mn,n%5==0,n%10==0,r%5==0,r%2==0,
           any(abs(n) in ([abs(y['num'])]+([y['den']] if y['den'] else [])) for y in o if y is not x),n%2==0,n==-r,f and n<x['den'],
           (not f) and ((d1<n<d2) if x['paso']==1 else (t1<n<t2)),
           (not f) and cp[x['paso']]==2 and n==min(y['num'] for y in o if y['paso']==x['paso']),
           (not f) and cp[x['paso']]==2 and n==max(y['num'] for y in o if y['paso']==x['paso'])]
        L+= [bool(dif), any(n<m for m in dif), any(n>m for m in dif),
             dif1rech[i], any(dif1rech[j] and o[j]['paso']==x['paso'] for j in range(4)),
             mediaL[i], (n>0 and n in med), any(mediaL[j] and o[j]['paso']==x['paso'] for j in range(4)),
             mitad[i], any(mitad[j] and o[j]['paso']==x['paso'] for j in range(4)),
             (d1+d2)%2==0, any(dif1rech), any(mediaL), not nueva,
             abs(n) in set(P[2]), abs(n) not in (set(P[0] if x['paso']==1 else P[1] if x['paso']==2 else P[2])),
             any(abs(abs(n)-q)==1 for q in vis)]
        res.append(sum(1<<k for k,b in enumerate(L) if b))
    return res
def marca(s): return {'EV-D-SUMA':1,'EV-D-FINAL':1,'EV-T-FINAL':2,'EV-C-UNIDAD':3}[s['err']]
CAN=dict(t1=8,t2=10,dur=2,v=40,D=80,d1=20,d2=100,rol='coc',err='EV-C-UNIDAD',rp='dist',Dm=80,Tm=2)
OC=[dict(cod='CLAVE',paso=3,num=80,den=2,u='km/h'),dict(cod='A',paso=1,num=100,den=None,u=None),dict(cod='B',paso=2,num=-2,den=None,u=None),dict(cod='D',paso=1,num=-20,den=None,u=None)]
for y in OC: y['r']=rech(CAN,y['paso'])
def construir(oblig=True,o2=True):
    A=[];W=[];M=[];Z=[]
    for s,C,w in estados(oblig,o2):
        for x,wi in zip(C,w):
            if wi==0: continue
            o=opciones(s,x)
            A.append(bits(s,o)); W.append(float(s['ps']*wi)); M.append([y['paso']==marca(s) for y in o]); Z.append((s['err'],s['rp']))
    return np.array(A,dtype=np.uint64),np.array(W),np.array(M,dtype=bool),Z
def lits(A,nom=NOM):
    L={}
    for i,n in enumerate(nom):
        b=((A>>np.uint64(i))&np.uint64(1)).astype(bool); L[n]=b; L['no_'+n]=~b
    return L
def punt(S,W,kp=None):
    k=S.sum(1); h=S[:,0] if kp is None else S[np.arange(len(S)),kp]
    with np.errstate(divide='ignore',invalid='ignore'):
        v=np.where((k==0)|(k==4),.25,h/k)
    return float((W*v).sum()/W.sum())
