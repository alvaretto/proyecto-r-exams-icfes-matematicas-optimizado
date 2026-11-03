"""Tanda S2a (2026-11-03), H4 de la FASE 2C r5: cap_s1b2.py llevaba 10 U+00A0 literales en el JS aunque su línea 12
decía que todas las clases iban con escapes. Este script:
1. pasa a escape (\\u00a0, \\u00bb, \\u201d, \\u2026) los U+00A0 y los signos no ASCII de las clases del JS;
2. añade un assert que comprueba que ni el JS ni el propio fuente llevan U+00A0, U+2009, U+202F o U+2060 literales;
3. deja la descripción conforme al código.
Las barras invertidas se construyen con chr(92): este fuente no lleva escapes de Unicode que una herramienta pueda
convertir en el carácter. Uso: python3 -I editar_cap_s2a.py <cap_s1b2.py> <copia_de_respaldo>"""
import sys, shutil, hashlib
f, copia = sys.argv[1], sys.argv[2]
shutil.copyfile(f, copia)
t = open(f, encoding="utf-8").read()
antes = hashlib.md5(t.encode("utf-8")).hexdigest()
BS = chr(92)
i = t.index('JS = r"""'); j = t.index('"""', i + len('JS = r"""')) + 3
js = t[i:j]
cuenta = {c: js.count(chr(c)) for c in (0xA0, 0xBB, 0x201D, 0x2026)}
print("en el JS, antes:", {f"U+{c:04X}": n for c, n in cuenta.items()})
assert cuenta[0xA0] == 10, "se esperaban 10 U+00A0 literales en el JS (H4 de la r5)"
for c in (0xA0, 0xBB, 0x201D, 0x2026):
    js = js.replace(chr(c), BS + "u" + f"{c:04x}")
t = t[:i] + js + t[j:]
# Descripción de la línea 12
vieja = "Todas las clases de caracteres van con escapes (" + BS + BS + "u00a0…), sin caracteres literales.\n"
assert t.count(vieja) == 1, "no encuentro la línea 12"
nueva = ("Tanda S2a (H4 de la FASE 2C r5): en el JS, los U+00A0 y los signos no ASCII de las clases van con su escape\n"
         "  (" + BS + BS + "u00a0, " + BS + BS + "u00bb, " + BS + BS + "u201d, " + BS + BS + "u2026). Hasta la S2a el JS llevaba 10 U+00A0 literales, aunque esta\n"
         "  línea decía que no. Un assert comprueba que ni el JS ni este archivo llevan U+00A0, U+2009, U+202F o U+2060\n"
         "  literales. Los comentarios del JS conservan sus tildes.\n")
t = t.replace(vieja, nueva)
# El assert, justo después del JS
ancla = t[t.index('JS = r"""'):]
fin_js = t.index('"""', t.index('JS = r"""') + len('JS = r"""')) + 3
assert_txt = ("\n# H4 de la FASE 2C r5 (tanda S2a): ningún espacio especial literal, ni en el JS ni en este archivo.\n"
              "for _c in (0xA0, 0x2009, 0x202F, 0x2060):\n"
              "    assert chr(_c) not in JS, f\"U+{_c:04X} literal en el JS\"\n"
              "    assert chr(_c) not in open(__file__, encoding=\"utf-8\").read(), f\"U+{_c:04X} literal en el fuente\"\n")
t = t[:fin_js] + assert_txt + t[fin_js:]
for c in (0xA0, 0x2009, 0x202F, 0x2060):
    assert chr(c) not in t, f"U+{c:04X} literal tras la edición"
open(f, "w", encoding="utf-8").write(t)
print("md5 antes:", antes, "· después:", hashlib.md5(t.encode("utf-8")).hexdigest())
