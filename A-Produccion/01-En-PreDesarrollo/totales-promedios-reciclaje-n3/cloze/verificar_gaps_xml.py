"""Cuenta las etiquetas HTML que quedan DENTRO de los gaps de un XML de Moodle (Incidente G: un gap de cloze
muestra texto plano, y cualquier etiqueta queda a la vista o desaparece).

Tras html.unescape, busca los gaps con  \\{[0-9]*:(?:MULTICHOICE|MULTIRESPONSE|NUMERICAL|SHORTANSWER)[^{}]*\\}
y cuenta dentro de ellos  </?[a-zA-Z][^>]*> . Debe dar 0. Control positivo: un gap sintético con <b> debe contar 1.

Uso: python3 -I verificar_gaps_xml.py <archivo.xml> [...]
"""
import html
import re
import sys

GAP = re.compile(r'\{[0-9]*:(?:MULTICHOICE|MULTIRESPONSE|NUMERICAL|SHORTANSWER)[^{}]*\}')
ETQ = re.compile(r'</?[a-zA-Z][^>]*>')


def contar(texto):
    t = html.unescape(texto)
    gaps = GAP.findall(t)
    return len(gaps), sum(len(ETQ.findall(g)) for g in gaps)


assert contar('<p>a {1:MULTICHOICE_V:%100%<b>x</b>~%0%y}</p>') == (1, 2), 'control positivo'
assert contar('{1:NUMERICAL:=5:0}') == (1, 0), 'control negativo'
total_g = total_e = 0
for f in sys.argv[1:]:
    g, e = contar(open(f, encoding='utf-8').read())
    total_g += g
    total_e += e
    print(f'{f}: gaps {g}, etiquetas dentro de gaps: {e}')
print(f'TOTAL: gaps {total_g}, etiquetas dentro de gaps: {total_e} (control positivo y negativo OK)')
