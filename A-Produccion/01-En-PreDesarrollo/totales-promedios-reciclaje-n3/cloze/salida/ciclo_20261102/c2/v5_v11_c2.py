#!/usr/bin/env python3
"""Paso 10 (tanda C2; copia de v5_v11_r9.py de la tanda 9b): V5 a V11 del orquestador-cloze sobre el md5 vigente. Mide lo barato (V5 sobre el XML y el HTML
de coherencias_r10/) y comprueba, por cadena literal, las líneas de los artefactos que acreditan el resto.
Uso: cd cloze/salida && python3 -I ciclo_20261102/c2/v5_v11_c2.py"""
import glob, hashlib, re
RMD = "../totales_promedios_reciclaje_aleatorio_argumentacion_n3_cloze_v1.Rmd"
m = hashlib.md5(open(RMD, "rb").read()).hexdigest(); print("md5 del .Rmd:", m)
leer = lambda p: open(p, encoding="utf-8").read()
fallos = 0
def ok(c, msg):
    global fallos; fallos += not c; print(("OK    " if c else "FALLA ") + msg)
def md5_en(p): return ("md5 del .Rmd: " + m) in leer(p) or ("md5 " + m) in leer(p)
# --- V5: gráficas-opción fuera del gap (N/A por construcción: Flujo B = n), medido igual
xml = sorted(glob.glob("coherencias_r10/moodle/*.xml")) + sorted(glob.glob("coherencias_r10/moodle_aula/*.xml"))
gaps = [g for x in xml for g in re.findall(r"\{[0-9]+:(?:MULTICHOICE|MULTIRESPONSE)[^}]*", leer(x))]
img = [g for g in gaps if re.search(r"<img|@@PLUGINFILE@@", g)]
print(f"V5: {len(xml)} XML, {len(gaps)} gaps MULTICHOICE/MULTIRESPONSE (control positivo: > 0), {len(img)} con <img o @@PLUGINFILE@@")
ok(len(gaps) > 0 and not img, "V5: ningún gap contiene imágenes (N/A por construcción: sin gráficas-opción)")
html = sorted(glob.glob("coherencias_r10/html/*.html"))
n_ans = sum(leer(h).count("##ANSWER") for h in html)
ok(len(html) == 100 and n_ans == 0, f"V5 (b): {len(html)} HTML, {n_ans} ##ANSWERi## sin resolver")
png = sum(len(re.findall(r'diagrama_[^."/]+\.png', leer(x))) for x in xml)
ok(png == 0, f"fuga por nombre de archivo (§P6): {png} referencias diagrama_*.png en el XML (N/A: sin imágenes)")
# --- V6: la prosa agrupa por veredicto, emparejada por contenido (oráculo L-C8)
v = "coherencias_r10/verificar_coherencias.txt"; t = leer(v)
ok(md5_en(v) and "TOTAL: fallos por versión = 0 · fallos globales = 0 · mutaciones sin cazar o sin su sonda = 0" in t,
   "V6: M6 «Respuestas» de la Parte 6 agrupa por veredicto lo recalculado (setequal por contenido) y M4 marcas = veredicto, N = 100, 0 fallos")
src = leer("coherencias_r10/verificar_coherencias.R")
ok("M6 «Respuestas» de la Parte 6 agrupa por veredicto" in src and "setequal(sol[(i_v + 1):(i_f - 1)], ver6)" in src,
   "V6: la comprobación existe en verificar_coherencias.R y compara conjuntos, no órdenes")
# --- V7: N/A (ninguna parte ofrece opciones de fuera de su conjunto); unicidad de la Parte 2 por el oráculo
ok("S2 exactamente una opción correcta (mi juicio)" in src, "V7: N/A; unicidad de la clave de la Parte 2 comprobada por S2 (una sola correcta por juicio propio)")
# --- V8 y V9: artefactos del paso 9
for f, s in [("ciclo_20261102/c2/diversidad_sustantiva_c2.txt", "6 gap(s) medidos de 6 declarados"), ("ciclo_20261102/c2/diagnosticidad_c2.txt", "PASS: ninguna sonda superficial supera el 70 %"),
             ("ciclo_20261102/c2/diagnosticidad_por_rama_c2.txt", "N = 100 por estrato")]:
    ok(md5_en(f) and s in leer(f), f"V8/V9: {f} con el md5 vigente y «{s}»")
# --- V10 y V11: verificador de la C1e
r = "ciclo_20261102/c1e/verificar_espacio_cloze_c1e.txt"; tr = leer(r)
ok(("md5 " + m) in tr and "exit=4" in tr and "0 fallos, 0 pendientes" in tr, "verificador de la C1e: md5 vigente, exit 4, 0 fallos, 0 pendientes")
rm = leer(RMD)
ok('"V10: la marca de cada afirmación de la Parte 6 sale de su texto y de la tabla"' in rm, "V10: guarda stopifnot en data_generation")
ok("OK: MC6 (la hipótesis del que perdió dice «habría perdido» con la marca de «habría ganado») cazado por su sonda" in tr, "V10: MC6 cazado por su sonda")
ok("Parte 6: las marcas del XML = el veredicto recalculado" in leer("coherencias_r10/verificar_formatos.txt"),
   "V10: marcas del XML de Moodle = veredicto recalculado desde la tabla mostrada (100/100 y 30/30)")
for s in ["OK: V11 por valor: ninguna respuesta de las Partes 3 a 5 es un número que la Parte 1 muestra o resuelve (330 versiones)",
          "OK: V11 por valor: las Partes 2 a 6 no imprimen ninguna cifra de las opciones de la Parte 1 ni T_G, T_P o D",
          "OK: V11 por valor: los textos de las Partes 2 a 6 no llevan cifras",
          "OK: V11 por contenido: las Partes 2 a 6 no nombran sedes ni filas de la Parte 1",
          "OK: V11 por independencia"]:
    ok(s in tr, s[4:90])
print("V11 a un paso: residuo declarado (líneas «RESIDUO DECLARADO: V11 a un paso» del verificador de la C1e):", tr.count("RESIDUO DECLARADO: V11 a un paso"))
print(f"\nTOTAL fallos: {fallos}")
