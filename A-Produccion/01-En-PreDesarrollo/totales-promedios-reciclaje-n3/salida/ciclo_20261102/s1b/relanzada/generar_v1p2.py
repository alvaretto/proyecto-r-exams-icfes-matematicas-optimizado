r"""Tanda S1b relanzada (2026-11-03): V1' sobre V1 (.Rmd 5cf5b12f). El signo que sigue a la unidad de una fórmula
entra en la fórmula:  "\\ \\text{kilos}$."  ->  "\\ \\text{kilos.}$"   (signos . , : ; ! ? ) ] » ” …).

Corrige el generador de la primera S1b (generar_v1p.py), que solo veía el signo pegado al «$» y contaba también el
comentario de cabecera. Clasifica cada «\\ \\text{unidad}$» del fuente:
  - comentario      : línea de comentario R (no se toca);
  - pegado          : el signo va en el mismo literal, justo tras el «$»;
  - otra_cadena     : el literal termina en «$» y el SIGUIENTE literal del paste0/cat empieza por el signo;
  - no_literal      : el literal termina en «$» y lo sigue una expresión (variable, llamada): no se puede decidir en
                      el fuente; se declara y lo resuelve la prueba sobre el texto emitido;
  - fin_llamada     : el literal termina en «$» y cierra la llamada;
  - sin_signo       : lo sigue texto sin signo (« y », « que compensan», « a favor de »…).
Uso: python3 -I generar_v1p2.py <V1.Rmd> <salida.Rmd>   (sin salida: solo clasifica)"""
import re, sys, collections
SIG = '.,:;!?)]»”…'
src = sys.argv[1]
s = open(src, encoding='utf-8').read()
OCC = re.compile(r'\\\\ \\\\text\{(kilos|cursos?)\}\$')
# Inicio de cada línea de comentario R (# tras espacios): las ocurrencias ahí no se tocan.
com = []
off = 0
for ln in s.split('\n'):
    if ln.lstrip().startswith('#'):
        com.append((off, off + len(ln)))
    off += len(ln) + 1
en_com = lambda p: any(a <= p < b for a, b in com)
clases = collections.OrderedDict()
cambios = []          # (inicio, fin, reemplazo)
for m in OCC.finditer(s):
    i, j = m.start(), m.end()
    lin = s.count('\n', 0, i) + 1
    if en_com(i):
        clases.setdefault('comentario', []).append(lin); continue
    sig = s[j] if j < len(s) else ''
    if sig in SIG and sig:
        clases.setdefault('pegado', []).append(lin)
        cambios.append((i, j + 1, '\\\\ \\\\text{' + m.group(1) + sig + '}$'))
        continue
    if sig == '"':
        m2 = re.compile(r'"(\s*,\s*)"([^"]?)').match(s, j)
        if m2 and m2.group(2) and m2.group(2) in SIG:
            clases.setdefault('otra_cadena', []).append(lin)
            # el signo sale del literal siguiente y entra en la fórmula
            cambios.append((i, m2.end(), '\\\\ \\\\text{' + m.group(1) + m2.group(2) + '}$"' + m2.group(1) + '"'))
            continue
        if m2:
            clases.setdefault('sin_signo', []).append(lin); continue
        if re.compile(r'"\s*\)').match(s, j):
            clases.setdefault('fin_llamada', []).append(lin); continue
        clases.setdefault('no_literal', []).append(lin); continue
    clases.setdefault('sin_signo', []).append(lin)
tot = sum(len(v) for v in clases.values())
print(f'«\\\\ \\\\text{{unidad}}$» en el fuente: {tot}')
for k, v in clases.items():
    print(f'  {k:12s} {len(v):2d}  líneas {v}')
if len(sys.argv) > 2:
    out = s
    for a, b, t in sorted(cambios, key=lambda x: -x[0]):
        out = out[:a] + t + out[b:]
    assert ' ' not in out and '⁠' not in out
    open(sys.argv[2], 'w', encoding='utf-8').write(out)
    print('sitios cambiados:', len(cambios), '->', sys.argv[2])
