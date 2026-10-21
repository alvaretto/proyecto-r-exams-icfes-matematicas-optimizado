#!/usr/bin/env python3
"""Comprueba verificacion_diferida.xml (cuestionario de verificación diferida, H4) y su tabla en la guía docente.

  python3 -I verificar_cuestionario.py [XML] [GUIA]      # exit 0 si todo está bien

Por pregunta: una sola opción con fracción 100; la clave no es la opción más larga; sin «$» (el MathJax del
sitio lo leería como fórmula); sin nombrar letras (las opciones se barajan). Y la tabla de la sección 10b de
guia_docente.html tiene las mismas preguntas, en el mismo orden y con la misma clave que el XML.
Lo corre el nivel 0 de mod_aulaviva (Proyectos-Varios/Moodle/plugins/aulaviva/tests/local/verificar_local.sh).
"""
import html
import pathlib
import re
import sys
import xml.etree.ElementTree as ET

AQUI = pathlib.Path(__file__).resolve().parent
fallos = 0


def ok(cond, msg):
    global fallos
    print(('  OK    ' if cond else '  FALLA ') + msg)
    fallos += 0 if cond else 1


def texto(nodo):
    return re.sub(r'\s+', ' ', html.unescape(re.sub(r'<[^>]+>', '', nodo.findtext('text') or ''))).strip()


def main():
    xml = pathlib.Path(sys.argv[1]) if len(sys.argv) > 1 else AQUI / 'verificacion_diferida.xml'
    guia = pathlib.Path(sys.argv[2]) if len(sys.argv) > 2 else AQUI / 'guia_docente.html'
    preguntas = []
    for q in ET.parse(xml).getroot().findall("question[@type='multichoice']"):
        nombre = texto(q.find('name'))
        ops = [(texto(a), float(a.get('fraction'))) for a in q.findall('answer')]
        claves = [t for t, f in ops if f == 100]
        todo = ' '.join([texto(q.find('questiontext')), texto(q.find('generalfeedback'))]
                        + [t for t, _ in ops] + [texto(a.find('feedback')) for a in q.findall('answer')])
        ok(len(claves) == 1, f'{nombre}: una sola clave ({len(claves)})')
        if claves:
            largos = [len(t) for t, _ in ops]
            ok(len(claves[0]) < max(largos), f'{nombre}: la clave no es la opción más larga ({len(claves[0])} de {largos})')
        ok('$' not in todo, f'{nombre}: sin «$»')
        ok(not re.search(r'\b(opción|letra) [A-D]\b|\b[A-D]\)', todo), f'{nombre}: sin nombrar letras')
        preguntas.append((nombre, claves[0] if claves else None))
    ok(len(preguntas) == 6, f'seis preguntas de selección única ({len(preguntas)})')

    g = guia.read_text(encoding='utf-8')
    m = re.search(r'<h2 id="verificacion">.*?<tbody>(.*?)</tbody>', g, re.S)
    ok(m is not None, 'la guía docente tiene la tabla de la sección 10b')
    if m:
        filas = re.findall(r'<tr><td>(.*?)</td><td>.*?</td><td><strong>(.*?)</strong></td>', m.group(1), re.S)
        filas = [(html.unescape(re.sub(r'<[^>]+>', '', a)).strip(), html.unescape(re.sub(r'<[^>]+>', '', b)).strip())
                 for a, b in filas]
        ok(filas == preguntas, 'la tabla 10b de la guía tiene las mismas preguntas y claves que el XML, en el mismo orden')
    print('CUESTIONARIO OK' if not fallos else f'CUESTIONARIO CON {fallos} FALLAS')
    return 1 if fallos else 0


if __name__ == '__main__':
    sys.exit(main())
