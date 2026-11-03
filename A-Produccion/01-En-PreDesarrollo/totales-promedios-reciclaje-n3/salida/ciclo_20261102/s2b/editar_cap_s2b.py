#!/usr/bin/env python3
"""Tanda S2b (2026-11-03), H4 de la FASE 2C r6: cap_s1b2.py documenta python3 -E -P y comprueba que el directorio de
trabajo está fuera del repo antes de importar websockets. Cada ancla aparece una vez. Copia previa:
cap_s1b2_antes_s2b.py.txt. Uso: python3 -I editar_cap_s2b.py
"""
import hashlib, pathlib, shutil

SUB = pathlib.Path("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/"
                   "01-En-PreDesarrollo/totales-promedios-reciclaje-n3")
CAP = SUB / "salida/ciclo_20261102/s1b/relanzada/cap_s1b2.py"
OUT = SUB / "salida/ciclo_20261102/s2b"
raw = CAP.read_bytes()
assert hashlib.md5(raw).hexdigest() == "1d0117ed4fe86504eec07fa8f5a28c49", "cap_s1b2.py no es el de la S2a"
shutil.copyfile(CAP, OUT / "cap_s1b2_antes_s2b.py.txt")
src = raw.decode("utf-8")


def rep(old, new):
    global src
    n = src.count(old)
    assert n == 1, f"ancla {old[:70]!r} aparece {n} veces"
    src = src.replace(old, new)


rep("Uso: python3 -I cap_s1b2.py ", "Uso: python3 -E -P cap_s1b2.py ")
rep("     <dir_html>/sNNN/ej1.html, semillas 1..100. El perfil de Chromium va FUERA del repo.",
    "     <dir_html>/sNNN/ej1.html, semillas 1..100. El perfil de Chromium va FUERA del repo.\n"
    "Tanda S2b (H4 de la FASE 2C r6): -E -P y no -I. websockets está en el site de usuario, que -I deja fuera; -P no mete\n"
    "  en sys.path el directorio del script ni el de trabajo, y -E ignora las variables PYTHON*. El directorio de trabajo\n"
    "  va FUERA del repo: lo comprueba un assert antes de importar websockets.")
rep("import asyncio, json, os, sys, subprocess, time, base64, urllib.request\nimport websockets\n",
    "import asyncio, json, os, sys, subprocess, time, base64, urllib.request\n"
    "MARCA_REPO = \"/RepositorioMatematicasICFES_R_Exams/\"\n"
    "assert MARCA_REPO not in os.path.realpath(os.getcwd()) + \"/\", \"el directorio de trabajo va fuera del repo (H4 de la r6)\"\n"
    "import websockets\n")
ESP = [chr(c) for c in (0xa0, 0x2009, 0x202f, 0x2060)]
assert not any(c in src for c in ESP), "cap_s1b2.py lleva un espacio especial literal"
CAP.write_bytes(src.encode("utf-8"))
print("md5 nuevo:", hashlib.md5(src.encode("utf-8")).hexdigest())
