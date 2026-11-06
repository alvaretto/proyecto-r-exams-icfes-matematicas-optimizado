import os,sys,json
from collections import defaultdict
from modelo import *
VAR=os.environ.get('VAR','base'); SIGNO=os.environ.get('SIGNO','0')=='1'
MARCA={'EV-C-UNIDAD':3,'EV-D-SUMA':1,'EV-D-FINAL':1,'EV-T-FINAL':2,'EV-D-SIGNO':1,'EV-T-SIGNO':2}
def dm(st,o): return st[3] if o['paso']==1 else st[1]
def scf(S):
    k=sum(1 for x in S if x); n=len(S)
    if k==0 or k==n: return .25
    return 1/k if S[0] else 0.
def r_U(o,st): return [x['paso']==3 and (x['num'],x['den'])==tuple(x['rech'][:2]) for x in o]
def r_RE(o,st): return [x['paso']<3 and x['rech'][0] in nums_en(st) for x in o]
def r_NM(o,st): return [x['num']<=x['rech'][0] for x in o]
def r_H(o,st): return [not(x['paso']==3 and x['u']=='h') for x in o]
def r_E1(o,st): return [x['paso']==3 or (x['num']>0 and x['num'] not in nums_en(st) and x['num']<=dm(st,x)) for x in o]
def r_E2(o,st): return [a and b for a,b in zip(r_E1(o,st),r_H(o,st))]
def r_E3(o,st): return [a and not(x['paso']==3 and x['num']<x['den']) for a,x in zip(r_E2(o,st),o)]
def r_X0P(o,st): return [x['num']>0 and x['num'] not in nums_en(st) for x in o]
def r_ESTsr(o,st): return est_nuevo(o,st,rot=False)
def r_ESTc(o,st): return est_nuevo(o,st,rot=True)
def r_ESTP(o,st): return [x['num']>0 and y for x,y in zip(o,est_nuevo(o,st,rot=False))]
def m0(o,st): return [x['paso']==MARCA[st[8]] for x in o]
def r_M2(o,st): return [a and x['num']>0 and x['num'] not in nums_en(st) for a,x in zip(m0(o,st),o)]
def r_M3(o,st):
    S=[]
    for a,x in zip(m0(o,st),o):
        if not a: S.append(False); continue
        if x['paso']==3: S.append(x['u']=='km' and x['num']>x['den'])
        else: S.append(x['num']>0 and x['num'] not in nums_en(st) and x['num']<=dm(st,x))
    return S
def r_X1(o,st): return [a and b for a,b in zip(xa(o,st),xb(o,st))]
R10,R20,R30=R(10,150),R(20,120),R(30,90)
def r_R20e(o,st): return [a and b for a,b in zip(R20(o,st),xb(o,st))]
def r_Q(o,st): return [isinstance(x['rech'][0],int) and x['rech'][0]<0 and x['num']==-x['rech'][0] for x in o]
REG=[('Q',r_Q),('EST sin rotulos',r_ESTsr),('EST con rotulos',r_ESTc),('RE',r_RE),('U',r_U),('NM',r_NM),('X0',x0),('X0+',r_X0P),('EST+',r_ESTP),
     ('E1',r_E1),('E2',r_E2),('E3',r_E3),('M0',m0),('M2',r_M2),('M3',r_M3),
     ('X1a',xa),('X1b',xb),('X1',r_X1),('R10-150',R10),('R20-120',R20),('R20-120+ent',r_R20e),('R30-90',R30),('MAG-segundo',mag2)]
tot=defaultdict(float); ram=defaultdict(lambda:defaultdict(float)); pr=defaultdict(float); ab=0.; n=0; ns=0; nueva=0.; dosclaves=0; noreal=0
for e in estados(SIGNO):
    if e[0]=='ABORTA': ab+=float(e[3]); continue
    _,st,rp,ps,C,w=e; pr[st[8]]+=float(ps); n+=1
    v=st[6]
    for s,wi in zip(C,w):
        if wi==0: continue
        ns+=1; pw=float(ps*wi); o=opciones(st,s)
        if any(c[0] in NUEVAS for c in s): nueva+=pw
        # corrección: ninguna razón falsa, sustituida, da v con km/h; la clave sí
        sv=[sustv(x,st) for x in o]
        if not(sv[0][0]==v and sv[0][1]): noreal+=1
        if any(y[0]==v and y[1] for y in sv[1:]): dosclaves+=1
        if len({(x['paso'],x['num'],x.get('den'),x.get('u')) for x in o})<4: dosclaves+=1
        for k,f in REG:
            q_=pw*scf(f(o,st)); tot[k]+=q_; ram[k][st[8]]+=q_
T=sum(pr.values())
json.dump(dict(VAR=VAR,SIGNO=SIGNO,n=n,ns=ns,ab=ab,nueva=nueva/T,dos=dosclaves,noreal=noreal,tot={k:100*v/T for k,v in tot.items()},ram={k:{b:100*x/pr[b] for b,x in d.items()} for k,d in ram.items()}),open(sys.argv[1],'w'))
print(VAR,SIGNO,'estados',n,'conjuntos',ns,'abortos',round(ab,6),'peso con razón nueva',round(100*nueva/T,1),'dos claves/duplicadas',dosclaves,'clave no da v',noreal)
