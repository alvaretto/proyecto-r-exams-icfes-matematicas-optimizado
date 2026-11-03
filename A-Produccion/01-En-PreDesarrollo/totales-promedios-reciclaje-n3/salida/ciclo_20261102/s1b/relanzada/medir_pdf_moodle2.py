"""Tanda S1b relanzada: PDF (pdftotext de 100 ejercicios, semillas 1..100) y XML de Moodle (n = 100, set.seed(1)).
Amplía medir_pdf_moodle.py de la primera S1b: renglones que empiezan por un signo (todos los signos ante los que el
texto no admite corte), errores de compilación en el .log, \\pandocbounded en el .tex, y en Moodle el signo que sigue
al cierre de una fórmula que termina en su unidad.
Uso: python3 -I medir_pdf_moodle2.py <salida.json> v=<dir_render> [v=<dir_render> ...]"""
import sys, re, subprocess, json, glob, os
SIG = '.,:;!?)\\]\u00bb\u201d\u2026'
U = re.compile(r'^\s*(kilos?|cursos?)\b')
S = re.compile(r'^\s*[' + SIG + ']')
out = {}
for arg in sys.argv[2:]:
    v, d = arg.split('=', 1)
    pdf = f'{d}/pdf/ej1.pdf'; txt = pdf[:-3] + 'txt'
    r = {}
    if os.path.exists(pdf):
        subprocess.run(['pdftotext', pdf, txt], check=True)
        t = open(txt, encoding='utf-8').read().split('\n')
        tot = form = cif = 0; sig_tot = 0; sig_tras = []
        for i, l in enumerate(t):
            p = t[i - 1].rstrip() if i else ''
            if U.match(l):
                tot += 1
                if re.search(r'=\s*[0-9]+$', p): form += 1
                elif re.search(r'[0-9]$', p): cif += 1
            if S.match(l):
                sig_tot += 1; sig_tras.append((p[-30:], l[:20]))
        texto = '\n'.join(t)
        pags = int(re.search(r'Pages:\s+(\d+)', subprocess.run(['pdfinfo', pdf], capture_output=True, text=True).stdout).group(1))
        tex = ''.join(open(x, encoding='utf-8', errors='replace').read() for x in glob.glob(f'{d}/pdf_tex/*.tex'))
        logs = ''.join(open(x, encoding='utf-8', errors='replace').read() for x in glob.glob(f'{d}/pdf_tex/*.log'))
        r.update(paginas=pags, renglones_que_empiezan_por_unidad=tot, por_unidad_tras_formula=form, por_unidad_tras_cifra=cif,
                 renglones_que_empiezan_por_signo=sig_tot, ejemplos_signo=sig_tras[:5],
                 formula_unidad_juntas=len(re.findall(r'=\s*[0-9]+\s(kilos|cursos?)\b', texto)),
                 tex_unidad_y_signo_dentro=len(re.findall(r'\\text\{(kilos?|cursos?)[' + SIG + r']\}', tex)),
                 tex_signo_tras_cierre_con_unidad=len(re.findall(r'\\text\{(kilos?|cursos?)\}\\\)[' + SIG + ']', tex)),
                 tex_pandocbounded=tex.count('\\pandocbounded'),
                 log_errores_latex=len(re.findall(r'^! ', logs, re.M)), log_undefined=logs.count('Undefined control sequence'),
                 log_encontrado=bool(logs))
    xml = f'{d}/moodle/ej.xml'
    if os.path.exists(xml):
        x = open(xml, encoding='utf-8').read()
        r['moodle'] = dict(
            preguntas=x.count('<question type="multichoice"'), math_inline_mathjax=x.count('<span class="math inline">\\('),
            mathml=x.count('<math'),
            unidad_dentro=len(re.findall(r'\\text\{(kilos?|cursos?)[' + SIG + r']?\}\\\)', x)),
            unidad_y_signo_dentro=len(re.findall(r'\\text\{(kilos?|cursos?)[' + SIG + r']\}\\\)', x)),
            signo_tras_cierre_con_unidad=len(re.findall(r'\\text\{(kilos?|cursos?)\}\\\)</span>[' + SIG + ']', x)),
            unidad_fuera_tras_cierre=len(re.findall(r'\\\)</span>[ \u00a0](kilos?|cursos?)\b', x)),
            signo_tras_unidad_fuera=len(re.findall(r'\\\)</span>[ \u00a0](kilos?|cursos?)(</span>)?[' + SIG + ']', x)),
            signo_tras_cualquier_cierre=len(re.findall(r'\\\)</span>[' + SIG + ']', x)),
            span_nowrap=x.count('white-space:nowrap'))
    out[v] = r; print(v, json.dumps(r, ensure_ascii=False))
json.dump(out, open(sys.argv[1], 'w'), ensure_ascii=False, indent=1)
