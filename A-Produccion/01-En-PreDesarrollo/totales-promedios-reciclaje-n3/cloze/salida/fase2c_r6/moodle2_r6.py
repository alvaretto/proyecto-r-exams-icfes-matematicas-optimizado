import re, html, os, collections, importlib.util, io, contextlib, sys
B = os.path.expanduser('~/.cache/fase2c_r6_cloze/')
s = importlib.util.spec_from_file_location('o', B + 'oraculo_r6.py'); o = importlib.util.module_from_spec(s)
with contextlib.redirect_stdout(io.StringIO()): s.loader.exec_module(o)
limpia = lambda c: o.nb(html.unescape(re.sub(r'<[^>]+>', '', c))).strip()
def corre(ruta):
    X = open(ruta, encoding='utf-8').read(); qs = re.findall(r'<question type="cloze">(.*?)</question>', X, re.S)
    f = collections.Counter(); pref = collections.Counter(); ej = None; pen = collections.Counter()
    for q in qs:
        body = re.search(r'<questiontext[^>]*>\s*<text><!\[CDATA\[(.*?)\]\]></text>', q, re.S).group(1)
        gen = re.search(r'<generalfeedback[^>]*>\s*<text><!\[CDATA\[(.*?)\]\]></text>', q, re.S)
        tabs = [[[limpia(c) for c in re.findall(r'<t[dh][^>]*>(.*?)</t[dh]>', r, re.S)] for r in re.findall(r'<tr.*?</tr>', t, re.S)] for t in re.findall(r'<table.*?</table>', body, re.S)]
        T1, T2 = o.tabla(tabs[0]), o.tabla(tabs[1])
        gaps = re.findall(r'\{(\d+):(MULTICHOICE_V|MULTIRESPONSE|NUMERICAL):(.*?)\}', body, re.S)
        if len(gaps) != 6: f['gaps'] += 1; continue
        def alts(g):
            out = []
            for a in g.split('~'):
                m = re.match(r'^(=|%-?[\d.]+%)?(.*?)(?:#.*)?$', a, re.S); out.append((m[1] or '', limpia(m[2])))
            return out
        A1, A2, A6 = alts(gaps[0][2]), alts(gaps[1][2]), alts(gaps[5][2])
        for nom, A, fn, T in (('p1', A1, o.p1_verdad, T1), ('p2', A2, o.p2_verdad, T2), ('p6', A6, o.p6_verdad, T2)):
            v = [fn(t, T) for _, t in A]
            cor = [p == '=' or (p.startswith('%') and float(p.strip('%')) > 0) for p, _ in A]
            if None in v: f[nom + '_no_juzgado'] += 1
            if v != cor: f[nom + '_marca'] += 1
            if nom == 'p6':
                k = (sum(cor), tuple(sorted(set(p for (p, _), c in zip(A, cor) if c))), tuple(sorted(set(p for (p, _), c in zip(A, cor) if not c))))
                pref[k] += 1
                if ej is None and sum(cor) == 2: ej = gaps[5][2]
                ## nota con la regla real de Moodle (multichoice_multi): suma de fracciones acotada a [0, 1]
                w = [1.0 if p == '=' else float(p.strip('%')) / 100 if p else 0.0 for p, _ in A]
                un = max(w); pen[(sum(cor), 'una sola verdadera marcada, nota del gap', round(min(max(un, 0), 1), 3))] += 1
        txt = limpia(body); g, p, nG, aG, nP, aP = o.gp(T2); TG, TP = nG * aG, nP * aP
        for k, gi in ((3, 2), (4, 3)):
            gr = re.search(rf'Parte {k}\.\s*¿Cuántos kilos de \S+ recogió en total el grado (\S+)\?', txt).group(1)
            if int(float(re.search(r'=?(-?[\d.]+):', gaps[gi][2])[1])) != T2[gr][0] * T2[gr][1]: f['num'] += 1
        if int(float(re.search(r'=?(-?[\d.]+):', gaps[4][2])[1])) != TG - TP: f['num5'] += 1
        gt = limpia(gen.group(1)) if gen else ''
        f['frase_en_retro'] += o.FRASE in gt
    print(os.path.basename(ruta), 'preguntas', len(qs), dict(f)); print('  por n.º de verdaderas (prefijo de las verdaderas, de las falsas):', dict(pref)); print('  nota con una sola verdadera marcada:', dict(pen)); print('  ej (2 verdaderas):', (ej or '')[:420])
corre(B + 'moodle/m100.xml')
corre('/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/cloze/salida/ciclo_20261102/c1e/render_e16842c1/moodle/cloze.xml')
