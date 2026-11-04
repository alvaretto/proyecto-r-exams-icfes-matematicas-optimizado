import re, os, itertools, collections
B = os.path.expanduser('~/.cache/fase2c_r6_cloze/')
X = open(B + 'moodle/m100.xml', encoding='utf-8').read(); qs = re.findall(r'<question type="cloze">(.*?)</question>', X, re.S)
def nota(sel, w): return min(max(sum(w[i] for i in sel), 0), 1)
res = collections.defaultdict(list)
for q in qs:
    body = re.search(r'<questiontext[^>]*>\s*<text><!\[CDATA\[(.*?)\]\]></text>', q, re.S).group(1)
    g = re.findall(r'\{1:MULTIRESPONSE:(.*?)\}', body, re.S)[0]; w = []; esp = False
    for a in g.split('~'):
        p = re.match(r'^(=|%-?[\d.]+%)?', a)[1] or ''
        if p == '=': w.append(1.0)
        elif p: w.append(float(p.strip('%')) / 100); esp = True
        else: w.append(0.0)
    tot = sum(x for x in w if x > 0); w = [x / tot if x > 0 else (x if esp else -1 / tot) for x in w]
    T = [i for i, x in enumerate(w) if x > 0]; n = len(T)
    res[(n, 'clave completa')].append(nota(T, w)); res[(n, 'una sola verdadera')].append(nota(T[:1], w)); res[(n, 'marcar las cuatro')].append(nota(range(4), w))
for k in sorted(res): print(k, 'nota', sorted(set(round(x, 3) for x in res[k])), 'n =', len(res[k]))
