r"""Tanda S1b: genera V1 y V2 a partir del .Rmd 4cffbe8b, solo en los 20 cierres de fórmula «"$", ED, "kilos».
V1: la unidad entra en la fórmula:  ... = T\ \text{kilos}$   (se quita el ED de ese sitio).
V2: <span style='white-space:nowrap'> desde el $ que abre la fórmula hasta la unidad (el ED se conserva)."""
import re, sys
src, modo, dst = sys.argv[1], sys.argv[2], sys.argv[3]
s = open(src, encoding='utf-8').read()
CIERRE = re.compile(r'"\$"(\s*),(\s*)ED(\s*),(\s*)"(kilos|cursos?)')
def dolares_en_cadenas(s):
    """Posiciones de '$' dentro de literales "..." de los chunks R (fuera de comentarios)."""
    pos, lines, off, en_chunk = [], s.split('\n'), 0, False
    for ln in lines:
        if ln.startswith('```{r'): en_chunk = True; off += len(ln) + 1; continue
        if ln.startswith('```'): en_chunk = False; off += len(ln) + 1; continue
        if en_chunk:
            i, dentro = 0, False
            while i < len(ln):
                c = ln[i]
                if dentro:
                    if c == '\\': i += 2; continue
                    if c == '"': dentro = False
                    elif c == '$': pos.append(off + i)
                else:
                    if c == '#': break
                    if c == '"': dentro = True
                i += 1
        off += len(ln) + 1
    return pos
ms = list(CIERRE.finditer(s)); assert len(ms) == 20, len(ms)
if modo == 'v1':
    out = CIERRE.sub(lambda m: '"\\\\ \\\\text{' + m.group(5) + '}$', s)
elif modo == 'v2':
    D = dolares_en_cadenas(s); ins = []
    for m in ms:
        cierre = m.start() + 1                      # el $ del literal "$"
        assert cierre in D, cierre
        abre = max(p for p in D if p < cierre)      # el $ que abre la fórmula
        ins.append((abre, "<span style='white-space:nowrap'>"))
        # corrección (2.ª medición de V2): si el $ que abre está en la rama else de `if (...) "...$" else "...$"`,
        # la rama if también abre la fórmula y lleva su propio span.
        ini = s.rfind('"', 0, abre)
        m_else = re.search(r'"\s*else\s*$', s[:ini])
        if m_else:
            fin_if = m_else.start()
            prev = [q for q in D if q < fin_if]
            if prev and prev[-1] == fin_if - 1:
                ins.append((prev[-1], "<span style='white-space:nowrap'>"))
        ins.append((m.end(), "</span>"))
    out = s
    for p, t in sorted(ins, key=lambda x: -x[0]): out = out[:p] + t + out[p:]
else: sys.exit('modo')
assert ' ' not in out and '⁠' not in out
open(dst, 'w', encoding='utf-8').write(out)
print(modo, 'sitios:', len(ms))
