"""S1b: PDF (pdftotext de 100 ejercicios, semillas 1..100) y XML de Moodle (n = 100, set.seed(1)) de cada variante."""
import sys,re,subprocess,json
SCR=sys.argv[1]; out={}
U=re.compile(r'^\s*(kilos?|cursos?)\b')
for v in ['ref','base','v1','v2']:
    pdf=f'{SCR}/pdf/{v}/ej1.pdf'; txt=pdf[:-3]+'txt'; subprocess.run(['pdftotext',pdf,txt],check=True)
    t=open(txt,encoding='utf-8').read().split('\n')
    tot=form=cif=0
    for i,l in enumerate(t):
        if U.match(l):
            tot+=1; p=t[i-1].rstrip() if i else ''
            if re.search(r'=\s*[0-9]+$',p): form+=1
            elif re.search(r'[0-9]$',p): cif+=1
    texto='\n'.join(t)
    pags=int(re.search(r'Pages:\s+(\d+)',subprocess.run(['pdfinfo',pdf],capture_output=True,text=True).stdout).group(1))
    r=dict(paginas=pags, renglones_que_empiezan_por_unidad=tot, tras_formula=form, tras_cifra=cif,
           formula_unidad_juntas=len(re.findall(r'=\s*[0-9]+\s(kilos|cursos?)\b',texto)))
    x=open(f'{SCR}/moodle/{v}/ej.xml',encoding='utf-8').read() if v!='ref' else ''
    if x:
        r['moodle']=dict(math_inline_mathjax=x.count('<span class="math inline">\\('), mathml=x.count('<math'),
            cierre_fuera_con_ED=len(re.findall(r'\\\)</span>\u00a0(kilos|cursos?)',x)),
            cierre_fuera_espacio=len(re.findall(r'\\\)</span> (kilos|cursos?)',x)),
            unidad_dentro=len(re.findall(r'\\ \\text\{(kilos|cursos?)\}\\\)',x)),
            span_nowrap=x.count('style="white-space:nowrap"')+x.count("style='white-space:nowrap'"),
            nowrap_con_formula=len(re.findall(r'<span style="white-space:nowrap"><span class="math inline">\\\(.*?\\\)</span>\u00a0(kilos|cursos?)</span>',x)),
            preguntas=x.count('<question type="multichoice"'))
    out[v]=r; print(v, json.dumps(r,ensure_ascii=False))
json.dump(out,open(sys.argv[2],'w'),ensure_ascii=False,indent=1)
