#!/usr/bin/env python3
"""Paso 10 (tanda C2; copia de literalidad_exextra_r9.py de la tanda 9b): literalidad de los exextra del CLOZE contra el catálogo canónico (solo lectura).
Uso: cd cloze/salida && python3 -I ciclo_20261102/c2/literalidad_exextra_c2.py"""
import hashlib, json, re, unicodedata
RMD = "../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"
ITEM = "../../ITEM_OFICIAL.md"
C = "/home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Matematicas/catalogos-oficiales-mat/"
CATS = ["niveles-mat.json", "evidencias-mat.json", "estandares-mat-ebc.json", "estructura-prueba-mat.json"]
md5 = lambda p: hashlib.md5(open(p, "rb").read()).hexdigest()
print("md5 del .Rmd:", md5(RMD))
for c in CATS: print("catálogo", c, md5(C + c))
txt = open(RMD, encoding="utf-8").read()
meta = txt[txt.index("\nMeta-information"):]
ex = dict(re.findall(r"^exextra\[(\w+)\]: (.*)$", meta, flags=re.M))
item = open(ITEM, encoding="utf-8").read()
ficha = dict((k.strip(), v.strip()) for k, v in re.findall(r"^\| ([^|]+?) \| ([^|]+?) \|$", item, flags=re.M))
def strings(o, p=""):
    if isinstance(o, str): yield p, o
    elif isinstance(o, dict):
        for k, v in o.items(): yield from strings(v, f"{p}/{k}")
    elif isinstance(o, list):
        for i, v in enumerate(o): yield from strings(v, f"{p}[{i}]")
todo = [(c, p, s) for c in CATS for p, s in strings(json.load(open(C + c, encoding="utf-8")))]
def buscar(t):
    return [(c, p) for c, p, s in todo if s == t]
def nfc(s): return unicodedata.normalize("NFC", s)
casos = [
 ("Competencia", ex["Competencia"], ficha.get("Competencia")),
 ("Componente", ex["Componente"], ficha.get("Componente")),
 ("Afirmacion", ex["Afirmacion"], ficha.get("Afirmación")),
 ("Evidencia", ex["Evidencia"], ficha.get("Evidencia")),
 ("Descriptor (texto tras ' — ')", ex["Descriptor"].split(" — ", 1)[1], ficha.get("Descriptor", " — ").split(" — ", 1)[1]),
 ("Estandar (texto antes de ' (')", ex["Estandar"].rsplit(" (", 1)[0], ficha.get("Estándar asociado", " (").rsplit(" (", 1)[0]),
]
fallos = 0
for nombre, v, f in casos:
    hits = buscar(v)
    ok_cat = len(hits) > 0; ok_ficha = (v == (f or "").rsplit(" (", 1)[0] if nombre.startswith("Estandar") else v == f) or v == f
    print(f"\n[{'OK   ' if ok_cat and ok_ficha else 'FALLA'}] {nombre}: {v!r}")
    print("   = ficha (ITEM_OFICIAL.md):", ok_ficha, "· NFC:", v == nfc(v))
    for c, p in hits[:4]: print("   literal en", c, p)
    if not hits: print("   SIN coincidencia literal en el catálogo")
    fallos += not (ok_cat and ok_ficha)
# campos enteros de la ficha que no van al catálogo
for k_rmd, k_ficha in [("Descriptor", "Descriptor"), ("Estandar", "Estándar asociado"), ("Nivel", "Nivel"), ("GradoSugerido", "Grado sugerido"),
                       ("Contenido", "Contenido"), ("TemaEspecifico", "Tema Específico")]:
    ok = ex.get(k_rmd) == ficha.get(k_ficha); fallos += not ok
    print(f"\n[{'OK   ' if ok else 'FALLA'}] {k_rmd} = ficha «{k_ficha}» carácter a carácter: {ex.get(k_rmd)!r}")
# D3.10: posición en el nivel 3 del catálogo
d = json.load(open(C + "niveles-mat.json", encoding="utf-8"))
loc = [p for c, p, s in todo if c == "niveles-mat.json" and s == casos[4][1]]
print("\nD3.10 en niveles-mat.json:", loc)
# Estándar: pensamiento y grado
loc = [p for c, p, s in todo if c == "estandares-mat-ebc.json" and s == casos[5][1]]
print("Estándar en estandares-mat-ebc.json:", loc)
print(f"\nDOK = {ex['DOK']} · Nivel = {ex['Nivel']} · Bloom = {ex['Bloom']}: DOK >= 3 => Nivel >= 3:",
      (int(ex['DOK']) < 3) or int(ex['Nivel']) >= 3, "· Bloom = Evaluar => Nivel >= 3:", ex['Bloom'] != 'Evaluar' or int(ex['Nivel']) >= 3)
print(f"\nTOTAL fallos: {fallos}")
