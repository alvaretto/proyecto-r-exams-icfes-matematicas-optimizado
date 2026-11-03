"""Tanda S1b relanzada: renglones FÍSICOS del PDF (pdftotext -bbox, coordenadas de cada palabra). Un signo o una unidad
«empieza renglón» si ninguna palabra queda a su izquierda en la misma línea de base (tolerancia 2 pt). El flujo de
pdftotext sin coordenadas separa la negrita del punto que la sigue («el mayor **total**.» sale como «el mayor» / «.»)
y daba 62 renglones con signo inicial en las cinco versiones, también en e293d5c8: era la extracción, no el PDF.
v2 del detector: una palabra empieza renglón si ninguna palabra a su izquierda se solapa con ella en vertical (más
de la mitad de la altura menor). La v1 agrupaba por yMax con 2 pt de tolerancia y separaba la negrita Type 3 («total»,
yMax 683,9) de su renglón (yMax 679,7): contaba 38 «.» iniciales que en el PDF van tras «total», en el mismo renglón.
Uso: python3 -I pdf_renglones_bbox.py <pdf> [<pdf> ...]"""
import sys, re, subprocess, html
SIG = '.,:;!?)]»”…'
for pdf in sys.argv[1:]:
    x = subprocess.run(['pdftotext', '-bbox', pdf, '-'], capture_output=True, text=True).stdout
    sig_ini = []; uni_ini = []; uni_tras_formula = 0; n_sig = n_uni = 0
    for pg, page in enumerate(re.findall(r'<page[^>]*>(.*?)</page>', x, re.S), 1):
        W = [(float(a), float(b), float(c), float(d), html.unescape(t)) for a, b, c, d, t in
             re.findall(r'<word xMin="([\d.]+)" yMin="([\d.]+)" xMax="([\d.]+)" yMax="([\d.]+)">(.*?)</word>', page)]
        solapa = lambda a, b: min(a[3], b[3]) - max(a[1], b[1]) > 0.5 * min(a[3] - a[1], b[3] - b[1])
        def izquierda(w): return [v for v in W if v is not w and v[2] <= w[0] + 1.0 and solapa(v, w)]
        def renglon_previo(w):
            arriba = [v for v in W if v[3] <= w[1] + 1.0 and not solapa(v, w)]
            if not arriba: return ''
            y = max(v[3] for v in arriba); fila = sorted([v for v in arriba if solapa(v, max(arriba, key=lambda v: v[3]))])
            return ' '.join(v[4] for v in fila)
        for w in W:
            prim = w[4]
            if prim[:1] in SIG and prim[:1] and not izquierda(w):
                sig_ini.append((pg, renglon_previo(w)[-30:], prim[:12]))
            if re.match(r'(kilos?|cursos?)\b', prim) and not izquierda(w):
                prev = renglon_previo(w); uni_ini.append((pg, prev[-30:], prim))
                if re.search(r'=\s*[0-9]+$', prev): uni_tras_formula += 1
        n_sig += sum(1 for w in W if w[4][:1] in SIG)
        n_uni += sum(1 for w in W if re.match(r'(kilos?|cursos?)\b', w[4]))
    print(f"{pdf.split('/')[-3]}: renglones físicos que empiezan por un signo {len(sig_ini)} (palabras con signo inicial: {n_sig})"
          f" | que empiezan por la unidad {len(uni_ini)}, tras una fórmula {uni_tras_formula}"
          f" | ejemplos signo {sig_ini[:3]} | ejemplos unidad {uni_ini[:2]}")
