import sys
sys.path.insert(0,'.')
from rasgos import *
A,W,M,Z=construir(True,True); L=lits(A)
Ac=np.array([bits(CAN,OC)],dtype=np.uint64); Lc=lits(Ac); Wc=np.array([1.])
Mc=np.array([[True,False,False,False]])
Z=np.array(['%s|%s'%z for z in Z])
def comp(L,M):
    E1=L['p3']|(L['pos']&L['no_en_enun']&L['no_sobre_cota'])
    E2=E1&L['no_frac_hkm']; E3=E2&L['no_frac_menor1']
    EST=L['no_en_enun']&L['no_en_pasos']
    d={}
    d['E3 (referencia)']=E3
    d['M3 (referencia)']=M&E3
    d['EST sin rotulos (referencia)']=EST
    d['EST+ (referencia)']=EST&L['pos']
    d['A1 E3 y no dif1_rech']=E3&L['no_dif1_rech']
    d['A2 E3 y no dif1_mayor']=E3&L['no_dif1_mayor']
    d['A3 E3, no dif1_rech, no dif1_mayor']=E3&L['no_dif1_rech']&L['no_dif1_mayor']
    d['A4 E3 y no paso_dif1_rech']=E3&L['no_paso_dif1_rech']
    d['A5 E3, no paso_dif1_rech, no dif1_mayor']=E3&L['no_paso_dif1_rech']&L['no_dif1_mayor']
    d['B1 EST y no dif1_rech']=EST&L['no_dif1_rech']
    d['B2 EST+, no dif1_rech']=EST&L['pos']&L['no_dif1_rech']
    d['B3 EST+, no dif1_rech, no dif1_mayor']=EST&L['pos']&L['no_dif1_rech']&L['no_dif1_mayor']
    d['B4 EST+, no paso_dif1_rech, no dif1_mayor']=EST&L['pos']&L['no_paso_dif1_rech']&L['no_dif1_mayor']
    d['C1 NM y no dif1_mayor']=L['no_mayor_rech']&L['no_dif1_mayor']
    d['C2 NM, no frac<1']=L['no_mayor_rech']&L['no_frac_menor1']
    d['C3 NM, no frac<1, no h/km']=L['no_mayor_rech']&L['no_frac_menor1']&L['no_frac_hkm']
    d['C4 NM, no h/km, no en_enun']=L['no_mayor_rech']&L['no_frac_hkm']&L['no_en_enun']
    d['D1 solo dif1_menor']=L['dif1_menor']
    d['D2 no dif1_rech (solo descarta CUENTA)']=L['no_dif1_rech']
    d['D3 no paso_dif1_rech']=L['no_paso_dif1_rech']
    d['F1 M3 y no dif1_mayor']=M&E3&L['no_dif1_mayor']
    d['F2 M0 y no dif1_mayor']=M&L['no_dif1_mayor']
    d['G1 A3 y no media_lect (calcula)']=E3&L['no_dif1_rech']&L['no_dif1_mayor']&L['no_media_lect']
    d['G2 B3 y no media_lect (calcula)']=EST&L['pos']&L['no_dif1_rech']&L['no_dif1_mayor']&L['no_media_lect']
    return d
d=comp(L,M); dc=comp(Lc,Mc)
ramas=sorted(set(z.split('|')[0] for z in Z))
print('%-46s %7s %7s %7s %7s %5s  %s'%('regla','exacta','exc','mezcla','exc','canon','por rama '+' / '.join(r[3:] for r in ramas)))
for k,S in d.items():
    v=100*punt(S,W); c=100*punt(dc[k],Wc); m=.95*v+.05*c
    pr=[]
    for r in ramas:
        i=np.array([z.startswith(r) for z in Z]); pr.append('%.1f'%(100*punt(S[i],W[i])))
    print('%-46s %7.2f %+7.2f %7.2f %+7.2f %5.1f  %s'%(k,v,v-28.8,m,m-28.8,c,' / '.join(pr)))
# aplicabilidad de los rasgos nuevos
for n in ('dif1_rech','dif1_mismo_paso','dif1_menor','media_lect','mitad_rech','hay_dif1_rech','hay_media','ninguna_nueva'):
    b=L[n]; print(n,'peso con alguna opción',round(100*float((W*(b.any(1))).sum()/W.sum()),2),'; clave la tiene',round(100*float((W*(b[:,0])).sum()/W.sum()),2))
# control con clave permutada para estas compuestas
rng=np.random.default_rng(7); kp=rng.integers(0,4,len(A))
print('control clave permutada, máximo de las compuestas sin marca:',round(max(100*punt(S,W,kp) for k,S in d.items() if not k.startswith(('F','M3'))),2))
