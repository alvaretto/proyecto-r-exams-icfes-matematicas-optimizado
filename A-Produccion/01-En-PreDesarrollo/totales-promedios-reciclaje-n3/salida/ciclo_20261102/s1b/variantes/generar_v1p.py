r"""Tanda S1b, medición informativa V1' (no aplicada): sobre V1, el signo que sigue a la unidad entra también en la fórmula:
   "\\ \\text{kilos}$."  ->  "\\ \\text{kilos.}$"   (también «:» y «,»)."""
import re, sys
s = open(sys.argv[1], encoding='utf-8').read()
P = re.compile(r'\\\\ \\\\text\{(kilos|cursos?)\}\$([.,:;])')
print("sitios con signo pegado:", len(P.findall(s)), "de", s.count('\\\\ \\\\text{kilos}$'))
s = P.sub(lambda m: '\\\\ \\\\text{' + m.group(1) + m.group(2) + '}$', s)
open(sys.argv[2], 'w', encoding='utf-8').write(s)
