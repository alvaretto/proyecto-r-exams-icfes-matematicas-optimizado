#!/usr/bin/env python3
"""formatos_parrafo_c1e.py — tanda C1e: la frase nueva y la vieja en la salida real de los cuatro formatos (HTML, PDF, DOCX, Moodle).
Por archivo cuenta la frase nueva (exigida una vez por versión) y la vieja (exigida 0). La frase vieja es la de 32243d0a. En el PDF se quitan los guiones de fin de renglón antes de contar. Uso: python3 -I formatos_parrafo_c1e.py <render>"""
import glob
import html
import os
import re
import subprocess
import sys
import zipfile

B = sys.argv[1]
NUEVA = "Por eso, para saber si el grado que perdió habría ganado con un curso más, o si el que ganó habría perdido con uno menos, basta comparar el promedio de cada uno con la diferencia entre los totales: si el promedio es mayor, sí; si es menor, no."
VIEJA = "basta comparar el promedio de ese grado"


def plano(t):
    t = html.unescape(html.unescape(re.sub(r"<[^>]+>", " ", t)))
    return re.sub(r"\s+", " ", t.replace(chr(0xA0), " "))


def tex(p):
    return re.sub(r"\s+", " ", re.sub(r"-\n", "", subprocess.run(["pdftotext", p, "-"], capture_output=True, text=True).stdout).replace(chr(0xA0), " "))


def docx(p):
    z = zipfile.ZipFile(p).read("word/document.xml").decode("utf-8")
    z = re.sub(r"</w:t>\s*<w:t[^>]*>", "", z)
    return plano(re.sub(r"</w:r>", "", z))


fuentes = [("HTML", glob.glob(B + "/html/*.html"), lambda p: plano(open(p, encoding="utf-8").read())),
           ("PDF", glob.glob(B + "/pdf/*.pdf"), tex),
           ("DOCX", glob.glob(B + "/docx/*.docx"), docx),
           ("Moodle", glob.glob(B + "/moodle/*.xml"), lambda p: plano(open(p, encoding="utf-8").read()))]
for nom, fs, lee in fuentes:
    for f in sorted(fs):
        t = lee(f)
        print(f"{nom:7s}{os.path.basename(f):22s} frase nueva {t.count(NUEVA)} · frase vieja {t.count(VIEJA)}")
