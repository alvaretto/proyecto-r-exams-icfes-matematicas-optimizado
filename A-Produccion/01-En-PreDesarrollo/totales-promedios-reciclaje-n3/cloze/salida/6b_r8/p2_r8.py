#!/usr/bin/env python3
"""p2_r8.py — 6b r8: verifica la Parte 2 (solo tipo «promedio») en los 24 HTML, fugas de markup, tildes, posición de la clave,
anomalías de resumen_metricas y huérfanas de línea (r3 vs r8). Solo lee; escribe p2_r8.txt por stdout."""
import re, json, collections, sys, os
HTMLDIR = os.environ.get("HTMLDIR", "html")
from bs4 import BeautifulSoup
SEEDS = ([7,3,2,1,4,5,6]+list(range(8,25))) if HTMLDIR == "html" else list(range(1,101)); NB = " "
norm = lambda s: re.sub(r"\s+", " ", s.replace(NB, " ")).strip()
K = ["Lo que recogió ese grado dividido en partes iguales entre sus cursos.", "Lo que recogió ese grado dividido entre el número de sus cursos.",
     "Lo que recogió ese grado dividido entre la cantidad de cursos de ese mismo grado."]
D = {"entre_promedio": "Lo que recogió ese grado dividido entre el promedio de sus cursos.",
     "luego_dos": "Lo que recogió ese grado dividido en partes iguales entre sus cursos y luego entre dos.",
     "cuadrado": "Lo que recogió ese grado dividido entre el número de sus cursos al cuadrado.",
     "ambos": "Lo que recogió ese grado dividido entre la cantidad de cursos de ambos grados.",
     "moda": "Lo que recogió la mayoría de los cursos de ese mismo grado."}
EX = {"entre_promedio": "Dividir lo que recogió el grado entre su promedio da la cantidad de cursos, no el promedio: el promedio es lo recogido dividido entre el número de cursos.",
      "luego_dos": "Dividir otra vez entre dos da la mitad del promedio: el promedio es lo recogido dividido entre el número de cursos, sin más divisiones.",
      "cuadrado": "Dividir entre el número de cursos al cuadrado da el promedio dividido otra vez entre los cursos, no el promedio.",
      "ambos": "Dividir entre los cursos de los dos grados da menos que el promedio de ese grado: lo recogido se reparte solo entre sus propios cursos.",
      "moda": "Lo que recogió la mayoría de los cursos, si más de la mitad recogió lo mismo, es la moda, y puede ser distinto del promedio, que reparte por igual entre los cursos lo que recogió el grado."}
CLAVE_EXPL = "El promedio por curso es el total del grado dividido entre la cantidad de cursos: lo que le tocaría a cada curso si el total se repartiera por igual."
PROC = "Parte 2. El promedio por curso es el total del grado dividido entre la cantidad de cursos: lo que le tocaría a cada curso si el total se repartiera por igual. No dice cuánto recogió cada curso en particular."
HEAD = "Parte 2 · Lecturas del promedio que no sirven"
BAN = ["lo mismo del otro grado", "Comparaciones que no deciden", "multiplicada por el promedio de un grado", "Qué se compara", "qué grado ganó", "mismo del otro"]
src = open("analisis_texto_r3.py", encoding="utf-8").read()
SINTILDE = re.search(r'^SINTILDE = r"([^"\n]*)"', src, re.M).group(1)
LEAK = [r":::", r"\\pandocbounded", r"\\newcounter", r"&lt;(?:div|table|p|span)", r"##ANSWER", r"\[no disponible\]", r"\*\*", r"\|\s*---", r"\bNA\b", r"\bNaN\b", r"\bInf\b", r"\bNULL\b",
        r"\bTRUE\b", r"\bFALSE\b", r"\$", r"\\text", r"\\frac", r"\\times", r"\\\(", r"&amp;#", r"Ã|Â|�|â€", r"\{width", r"```", r"`r "]
issues, rows, keypos, leaks, tild = [], [], [], collections.Counter(), collections.Counter()
expl_seen, opt_seen = collections.defaultdict(set), collections.Counter()
def add(i, m): issues.append(f"v{i} (semilla {SEEDS[i-1]}): {m}")
dbg = None
for idx, seed in enumerate(SEEDS, start=1):
    raw = open(f"{HTMLDIR}/v{idx}.html", encoding="utf-8").read()
    soup = BeautifulSoup(raw, "html.parser")
    for a in soup.find_all("annotation"): a.decompose()
    lis = soup.select("body > ol > li")
    if len(lis) != 1: add(idx, f"{len(lis)} <li> de nivel superior"); continue
    li = lis[0]; kids = [c for c in li.children if getattr(c, "name", None)]
    h4s = [c for c in kids if c.name == "h4"]
    if len(h4s) != 2: add(idx, f"{len(h4s)} h4"); continue
    si = kids.index(h4s[1]); Q, S = kids[:si], kids[si+1:]
    tabs = li.find_all("table")
    hdr2 = norm(tabs[1].find_all("tr")[0].get_text(" ")) if len(tabs) > 1 else ""
    mm = re.search(r"Promedio de (\S+) (?:recogido|recolectado) por curso", hdr2); mat = mm.group(1) if mm else "?"
    Qp = [c for c in Q if c.name == "p"]
    i2 = [i for i, c in enumerate(Qp) if re.match(r"Parte 2\.", norm(c.get_text())) and c.find("strong")]
    if len(i2) != 1: add(idx, f"{len(i2)} enunciados de la Parte 2"); continue
    stem = norm(Qp[i2[0]].get_text()); nxt = Qp[i2[0]+1]
    if idx == 1: dbg = (str(Qp[i2[0]])[:500], str(nxt)[:900])
    exp_stem = f"Parte 2. ¿Qué indica, en la tabla del otro colegio, el promedio de {mat} recogido por curso de un grado?"
    if stem != exp_stem: add(idx, f"enunciado Parte 2 distinto: «{stem}» (esperado «{exp_stem}»)")
    opts = [norm(x) for x in re.split(r"\s+/\s+|\n", nxt.get_text()) if norm(x)]
    if len(opts) != 4: add(idx, f"Parte 2: {len(opts)} opciones leídas: {opts}"); continue
    if len(set(opts)) != 4: add(idx, "Parte 2: opciones duplicadas")
    nk = [o for o in opts if o in K]; nd = [o for o in opts if o in D.values()]
    if len(nk) != 1 or len(nd) != 3: add(idx, f"Parte 2: {len(nk)} claves y {len(nd)} distractores del pool de 4: {opts}")
    for o in opts:
        opt_seen[o] += 1
        if not (o.startswith("Lo que recogió ") and o.endswith(".")): add(idx, f"Parte 2: opción fuera del molde «Lo que recogió …»: «{o}»")
        if o not in K and o not in D.values(): add(idx, f"Parte 2: opción fuera del pool: «{o}»")
    key = nk[0] if nk else None; kpos = opts.index(key) if key else None; keypos.append(kpos)
    dnames = {v: k for k, v in D.items()}
    # Solution
    Sp = [c for c in S if c.name == "p"]
    resp = re.findall(r"Parte 2\. «([^»]*)»", " ".join(norm(c.get_text(" ")) for c in S))
    if len(resp) != 1 or (key and resp[0] != key): add(idx, f"Respuestas Parte 2: {resp} (clave {key})")
    hd = [c for c in Sp if norm(c.get_text()) == HEAD]
    if len(hd) != 1: add(idx, f"{len(hd)} encabezados «{HEAD}»")
    else:
        if not (hd[0].find("strong") and norm(hd[0].find("strong").get_text()) == HEAD): add(idx, "encabezado Parte 2 de la Solution sin negrita")
        ul = hd[0].find_next_sibling()
        if ul is None or ul.name != "ul": add(idx, "tras el encabezado Parte 2 no hay lista")
        else:
            bl = [norm(l.get_text()) for l in ul.find_all("li", recursive=False)]
            exp = {f"«{o}» {EX[dnames[o]]}" for o in opts if o in dnames}
            if len(bl) != 3 or set(bl) != exp: add(idx, f"viñetas Parte 2 ≠ esperado: {bl} vs {sorted(exp)}")
    pr = [norm(c.get_text()) for c in Sp if norm(c.get_text()).startswith("Parte 2. El promedio por curso")]
    if len(pr) != 1 or pr[0] != PROC: add(idx, f"párrafo de procedimiento Parte 2: {pr}")
    ol = [c for c in S if c.name == "ol" and c.get("type") == "a"]
    if len(ol) != 1: add(idx, f"{len(ol)} listas (a)-(f)")
    else:
        its = ol[0].find_all("li", recursive=False)
        if len(its) != 6: add(idx, f"lista (a)-(f) con {len(its)} ítems")
        else:
            fb = [norm(x) for x in re.split(r"\s+/\s+", norm(its[1].get_text()))]
            if len(fb) != 4: add(idx, f"retroalimentación Parte 2 con {len(fb)} entradas: {fb}")
            else:
                nv = [j for j, f in enumerate(fb) if f.startswith("Verdadero.")]; nf = [j for j, f in enumerate(fb) if f.startswith("Falso.")]
                if nv != [kpos] or len(nf) != 3: add(idx, f"retro Parte 2: Verdadero en {nv}, Falso en {nf}; clave en {kpos}")
                for j, o in enumerate(opts):
                    want = ("Verdadero. " + CLAVE_EXPL) if o in K else ("Falso. " + EX[dnames[o]]) if o in dnames else None
                    if want and fb[j] != want: add(idx, f"retro Parte 2 de la opción {j+1} ≠ esperada: «{fb[j][:90]}…»")
                    if o in dnames: expl_seen[o].add(fb[j])
    full = norm(li.get_text(" "))
    for b in BAN:
        if b.lower() in full.lower(): add(idx, f"rastro del tipo retirado: «{b}»")
    vis = li.get_text(" ")
    for pat in LEAK:
        for m in re.finditer(pat, vis): leaks[(pat, idx)] += 1
    for m in re.finditer(SINTILDE, vis, flags=re.I): tild[m.group(0).lower()] += 1
    rows.append((idx, seed, mat, kpos, key))
print("== Parte 2 (24 versiones) ==")
print("incidencias:", len(issues)); [print("  ", x[:500]) for x in issues[:40]]
print("posición de la clave (0 = primera opción):", dict(sorted(collections.Counter(r[3] for r in rows).items())), "| por versión:", [(r[0], r[3]) for r in rows])
print("uso de opciones (claves K y distractores D) sobre 96:", {o[:62]: n for o, n in sorted(opt_seen.items(), key=lambda t: -t[1])})
print("claves usadas:", collections.Counter(r[4] for r in rows).most_common())
print("retroalimentación por distractor, textos distintos vistos (esperado 1 cada uno):", {o[40:80]: len(v) for o, v in expl_seen.items()})
print("materiales:", collections.Counter(r[2] for r in rows))
print("== fugas de markup / math / mojibake en texto visible (patrón, versión) ==", dict(leaks) if leaks else "0")
print("== palabras sin tilde (SINTILDE) ==", dict(tild) if tild else "0")
print("\n== depuración v1: HTML del enunciado y de las opciones ==\n", dbg)
if HTMLDIR != "html": sys.exit(0)
# resumen_metricas: anomalías y alturas
def parse(path):
    out = []
    for l in open(path, encoding="utf-8"):
        m = re.match(r"s(\d+) (\w+)\s+H=\s*(\d+) solY=\s*(\d+) box=\s*(\d+) sw=\s*(\d+) scroll=(\d+)/(\d+) tdwrap=(\d+) spills=(\d+) leaks=(\d+) gapmin=(\d+) partes=\[(.*)\]", l)
        if m: out.append(dict(s=int(m[1]), mode=m[2], H=int(m[3]), solY=int(m[4]), scroll=int(m[7]), tdwrap=int(m[9]), spills=int(m[10]), leaks=int(m[11]), gapmin=int(m[12]), partes=m[13]))
    return out
a8, a3 = parse("resumen_metricas.txt"), parse("../6b_r3/resumen_metricas.txt")
print("\n== resumen_metricas: líneas r8/r3:", len(a8), len(a3))
for nm, a in (("r8", a8), ("r3", a3)):
    bad = [x for x in a if x["scroll"] or x["tdwrap"] or x["spills"] or x["leaks"] or x["partes"] != "1, 2, 3, 4, 5, 6"]
    print(f"  {nm}: anomalías (scroll/tdwrap/spills/leaks/partes):", len(bad), bad[:3], "| gapmin distinto de 4:", [(x['s'], x['mode'], x['gapmin']) for x in a if x['gapmin'] != 4][:5])
for md in ("plain320", "plain360", "plain1024", "box320", "box360"):
    h8 = [x["H"] for x in a8 if x["mode"] == md]; h3 = [x["H"] for x in a3 if x["mode"] == md]
    print(f"  {md}: H r3 min/media/max = {min(h3)}/{sum(h3)//len(h3)}/{max(h3)} | r8 = {min(h8)}/{sum(h8)//len(h8)}/{max(h8)} | n={len(h3)}/{len(h8)}")
# huérfanas de línea (r3 vs r8)
def lines_stats(path):
    d = json.load(open(path, encoding="utf-8")); res = {}
    for vw in (320, 360):
        E = [e for e in d if e["vw"] == vw]; o = []; hd = []; bl = []; q2 = []
        for e in E:
            m = e["m"]; opts = m["opts"]
            assert len(opts) == 12, (e["seed"], len(opts))
            o += opts[4:8]; hd.append(m["head"]); q2 += m["q2"]; bl += (m["sol"] or [])
        orf = lambda ls: len(ls) > 1 and len(ls[-1].split()) == 1
        res[vw] = dict(n_op=len(o), op_orf=sum(orf(x) for x in o), op_2=sum(len(x) == 2 for x in o), op_3=sum(len(x) == 3 for x in o), op_1=sum(len(x) == 1 for x in o),
                       head_orf=sum(orf(h) for h in hd), head_n=len(hd), q2_orf=sum(orf(x) for x in q2), q2_n=len(q2), q2_lines=collections.Counter(len(x) for x in q2),
                       bl_n=len(bl), bl_orf=sum(orf(x) for x in bl), bl_lines=collections.Counter(len(x) for x in bl), bl_max=max(len(x) for x in bl))
    return res
s8, s3 = lines_stats("cortes_p2_r3.json"), lines_stats("../6b_r3/cortes_p2_r3.json")
for vw in (320, 360):
    print(f"\n== líneas Parte 2 @ {vw} (modo box) ==")
    for k in s8[vw]: print(f"  {k}: r3 = {s3[vw][k]} | r8 = {s8[vw][k]}")
