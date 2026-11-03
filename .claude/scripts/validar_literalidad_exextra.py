#!/usr/bin/env python3
"""Literalidad de los `exextra` oficiales de un .Rmd contra el catálogo canónico ICFES.

SOLO MATEMÁTICAS (catálogos `catalogos-oficiales-mat`). No sirve para otras áreas.

Uso:
    python3 -I validar_literalidad_exextra.py <ruta.Rmd> [--item ITEM_OFICIAL.md] [--catalogos DIR]

Catálogo: --catalogos, o la variable de entorno CATALOGOS_MAT, o el directorio por defecto
de Todo-Pajaro (niveles-mat.json, evidencias-mat.json, estandares-mat-ebc.json,
estructura-prueba-mat.json). Solo lectura.

Campos obligatorios: Competencia, Componente, Afirmacion, Evidencia. Se comprueban si
están: Descriptor (texto tras ' — ') y Estandar (texto antes de ' ('). Cada valor debe
aparecer como cadena IGUAL (en NFC) en algún catálogo; con --item, además igual al campo de
la ficha (tabla `| Campo | Valor |`).

Salida: md5 del .Rmd, md5 de cada catálogo y una línea por campo ([OK], [FALLA],
[NO DECLARADO]).
Códigos de salida: 0 todo OK; 1 algún campo FALLA o falta un obligatorio;
2 NO VERIFICABLE (sin catálogos, JSON ilegible, .Rmd sin Meta-information...): nunca un PASS.
"""
import argparse
import hashlib
import json
import os
import re
import sys
import unicodedata

DEFECTO = ("/home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/"
           "Matematicas/catalogos-oficiales-mat/")
CATS = ["niveles-mat.json", "evidencias-mat.json", "estandares-mat-ebc.json",
        "estructura-prueba-mat.json"]
# (clave exextra, etiqueta, nombres posibles en la ficha, obligatorio, transformación)
CAMPOS = [
    ("Competencia", "Competencia", ["Competencia"], True, None),
    ("Componente", "Componente", ["Componente"], True, None),
    ("Afirmacion", "Afirmacion", ["Afirmación", "Afirmacion"], True, None),
    ("Evidencia", "Evidencia", ["Evidencia"], True, None),
    ("Descriptor", "Descriptor (texto tras ' — ')", ["Descriptor"], False, "descriptor"),
    ("Estandar", "Estandar (texto antes de ' (')", ["Estándar asociado", "Estandar asociado",
                                                    "Estándar", "Estandar"], False, "estandar"),
]


def nfc(s):
    return unicodedata.normalize("NFC", s)


def plegar(s):
    """Sin tildes, sin mayúsculas y con los espacios colapsados: para distinguir FORMATO de contenido."""
    s = "".join(c for c in unicodedata.normalize("NFKD", s) if not unicodedata.combining(c))
    return " ".join(s.lower().split())


def md5(p):
    with open(p, "rb") as f:
        return hashlib.md5(f.read()).hexdigest()


def no_verificable(razon):
    print("NO VERIFICABLE: " + razon)
    sys.exit(2)


def strings(o, p=""):
    if isinstance(o, str):
        yield p, o
    elif isinstance(o, dict):
        for k, v in o.items():
            yield from strings(v, f"{p}/{k}")
    elif isinstance(o, list):
        for i, v in enumerate(o):
            yield from strings(v, f"{p}[{i}]")


def transformar(v, modo):
    """Devuelve la parte del valor que va al catálogo.

    Descriptor «D3.10 — texto» → «texto»; Estándar «texto (código)» → «texto». Sin el separador se
    compara el valor entero: un Estándar escrito sin su código entre paréntesis puede ser literal
    (medido el 2026-11-02 en `tasa-caminata-velocidad-maxima-n4/cloze_v2`, cuyo Estándar está tal cual
    en estandares-mat-ebc.json y salía FALLA solo por no llevar « ( »). Un Descriptor reducido a su
    código («D3.10») no es texto del catálogo y falla por SIN coincidencia literal.
    """
    if modo == "descriptor":
        return v.split(" — ", 1)[1] if " — " in v else v
    if modo == "estandar":
        return v.rsplit(" (", 1)[0] if " (" in v else v
    return v


def main():
    ap = argparse.ArgumentParser(add_help=True)
    ap.add_argument("rmd")
    ap.add_argument("--item")
    ap.add_argument("--catalogos")
    a = ap.parse_args()

    cdir = a.catalogos or os.environ.get("CATALOGOS_MAT") or DEFECTO
    if not os.path.isdir(cdir):
        no_verificable(f"no existe el directorio de catálogos: {cdir}")
    if not os.path.isfile(a.rmd):
        no_verificable(f"no existe el .Rmd: {a.rmd}")
    todo = []
    md5s = []
    for c in CATS:
        p = os.path.join(cdir, c)
        if not os.path.isfile(p):
            no_verificable(f"falta el catálogo {p}")
        try:
            with open(p, encoding="utf-8") as f:
                datos = json.load(f)
        except (ValueError, OSError) as e:
            no_verificable(f"catálogo ilegible {p}: {e}")
        md5s.append((c, md5(p)))
        todo += [(c, ru, nfc(s)) for ru, s in strings(datos)]

    with open(a.rmd, encoding="utf-8") as f:
        txt = f.read()
    m = re.search(r"^Meta-information\s*$", txt, flags=re.M)
    if not m:
        no_verificable("el .Rmd no tiene sección Meta-information")
    ex = dict(re.findall(r"^exextra\[(\w+)\]:[ \t]*(.*?)[ \t]*$", txt[m.end():], flags=re.M))

    ficha = None
    if a.item:
        if not os.path.isfile(a.item):
            no_verificable(f"no existe la ficha: {a.item}")
        with open(a.item, encoding="utf-8") as f:
            ficha = {nfc(k.strip()): nfc(v.strip()) for k, v in
                     re.findall(r"^\|\s*([^|]+?)\s*\|\s*([^|]+?)\s*\|\s*$", f.read(), flags=re.M)}

    print("md5 del .Rmd:", md5(a.rmd))
    for c, h in md5s:
        print("catálogo", c, h)
    print()

    fallos = 0
    for clave, etq, nombres_ficha, obligatorio, modo in CAMPOS:
        if clave not in ex or not ex[clave]:
            if obligatorio:
                print(f"[NO DECLARADO] {etq}: falta exextra[{clave}]")
                fallos += 1
            continue
        crudo = nfc(ex[clave])
        v = transformar(crudo, modo)
        hits = [(c, ru) for c, ru, s in todo if s == v]
        motivos = []
        if not hits:
            # Regla #7 íntegra, § 2b: una diferencia solo de formato (tildes, mayúsculas) se reporta como
            # FORMATO, no como contenido distinto. Sigue siendo FALLA: el valor tiene que ser literal.
            parecidos = sorted({s for c, ru, s in todo if plegar(s) == plegar(v)})
            if parecidos:
                motivos.append("FORMATO (solo tildes o mayúsculas): el literal del catálogo es "
                               + " | ".join(repr(p) for p in parecidos[:2]))
            else:
                motivos.append("SIN coincidencia literal en el catálogo")
        if ficha is not None:
            f = next((ficha[n] for n in nombres_ficha if nfc(n) in ficha), None)
            if f is None:
                motivos.append("la ficha no tiene el campo " + "/".join(nombres_ficha))
            else:
                fv = transformar(f, modo)
                if fv != v:
                    motivos.append(f"distinto de la ficha: {fv!r}")
        if motivos:
            fallos += 1
            print(f"[FALLA] {etq}: {v!r}")
            for mo in motivos:
                print("   " + mo)
        else:
            print(f"[OK] {etq}: {v!r}")
        for c, ru in hits[:4]:
            print(f"   literal en {c} {ru}")
    print(f"\nTOTAL fallos: {fallos}")
    sys.exit(1 if fallos else 0)


if __name__ == "__main__":
    main()
