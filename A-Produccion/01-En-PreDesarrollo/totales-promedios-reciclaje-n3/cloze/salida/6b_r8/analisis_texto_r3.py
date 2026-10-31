#!/usr/bin/env python3
"""analisis_texto_r3.py — 6b ronda 3: análisis textual de los 24 HTML (md5 42f725bf…). Solo lee html/; escribe analisis_texto_r3.txt"""
import re, collections
from bs4 import BeautifulSoup, NavigableString
SEEDS = [7,3,2,1,4,5,6]+list(range(8,25))
NB = "\u00a0"
def norm(s): return re.sub(r"\s+", " ", s.replace(NB, " ")).strip()
D_ = dict(maximo="Lo que recogió el mejor de sus cursos dentro de ese grado.", total="Lo que recogió el grado sumando lo de sus cursos.",
  iguales="Lo que recogió cualquiera de sus cursos porque en el grado recogieron lo mismo.", mediana="Lo que recogió el que queda en el medio al ordenar sus cursos en ese grado.",
  moda="Lo que recogió la mayoría de sus cursos en ese grado.")
D2 = dict(promedio="El promedio por curso de un grado y el del otro grado.", cursos="La cantidad de cursos participantes de un grado y la del otro grado.",
  resta="La diferencia entre el promedio por curso de un grado y el del otro grado.", suma="La suma de la cantidad de cursos y el promedio por curso de un grado y la suma del otro grado.",
  cociente="El promedio dividido entre la cantidad de cursos de un grado y el del otro grado.")
EXPL = dict(maximo="El promedio no es lo que recogió el curso que más recogió: ese curso pudo recoger más que el promedio.",
  total="Eso es el total del grado; el promedio por curso es ese total repartido por igual entre los cursos.",
  iguales="El promedio no dice que todos los cursos recogieron lo mismo: dice cuánto le tocaría a cada uno si el total se repartiera por igual.",
  mediana="Eso describe la mediana de lo que recogieron los cursos, no el promedio.", moda="Eso describe la moda, el valor más repetido, no el promedio.",
  promedio="Un promedio mayor no garantiza un total mayor si en ese grado participaron menos cursos.", cursos="Más cursos no garantizan un total mayor si cada curso recogió, en promedio, menos.",
  resta="La resta de los promedios compara un curso típico con otro; el concurso se decide por los totales.", suma="Sumar la cantidad de cursos y el promedio no da ningún total: el total es su producto.",
  cociente="Dividir el promedio entre la cantidad de cursos no da ningún total: el total es su producto.")
P2 = {"promedio": dict(clave=["Lo que recogió el grado dividido entre el número de sus cursos participantes.", "Lo que recogió ese grado dividido en partes iguales entre sus cursos.", "Lo que recogió ese grado dividido entre sus cursos."],
        dis=D_, head="Parte 2 · Lecturas del promedio que no sirven", qs="¿Qué indica", clx="El promedio por curso es el total del grado dividido entre la cantidad de cursos: lo que le tocaría a cada curso si el total se repartiera por igual.",
        tail="No dice cuánto recogió cada curso en particular."),
      "comparar": dict(clave=["La suma de lo recogido por los cursos de un grado y la suma del otro grado.", "El producto de cursos por promedio de un grado y el del otro grado.", "La cantidad de cursos multiplicada por el promedio de un grado y lo mismo del otro grado."],
        dis=D2, head="Parte 2 · Comparaciones que no deciden qué grado ganó", qs="¿Qué se compara", clx="Gana el grado que recogió más en total, y el total de cada grado es la cantidad de cursos multiplicada por el promedio por curso.",
        tail="Comparar solo los promedios, o solo la cantidad de cursos, puede llevar a una conclusión falsa.")}
SINTILDE = r"\b(cuantos|cuantas|cual|cuales|tambien|ademas|segun|despues|asi|gano|perdio|recogio|participo|habria|seria|tendria|podria|numero|septimo|unico|ultimo|metodo|analisis|razon|conclusion|afirmacion|informacion|operacion|seccion|opcion|multiplicacion|division|grafica|estadistica|matematica|hipotesis|interpretacion|reflexion|comparacion|dia|dias|esta tabla|mas)\b"
issues, rows = [], []
def add(idx, seed, msg): issues.append(f"v{idx} (semilla {seed}): {msg}")
def nint(s): m = re.search(r"(\d+)", s); return int(m.group(1)) if m else None
for idx, seed in enumerate(SEEDS, start=1):
    try:
        raw = open(f"html/v{idx}.html", encoding="utf-8").read()
        soup = BeautifulSoup(raw, "html.parser"); [a.decompose() for a in soup.find_all("annotation")]; lis = soup.select("body > ol > li")
        if len(lis) != 1: add(idx, seed, f"{len(lis)} <li> de nivel superior")
        li = lis[0]; kids = [c for c in li.children if getattr(c, "name", None)]
        q, sol, cur = [], [], None; cur = q
        for c in kids:
            if c.name == "h4":
                if c.get_text().strip().lower() == "solution": cur = sol
                continue
            cur.append(c)
        txt = li.get_text("\n")
        # --- fugas / mojibake / marcas ---
        pats = {"ANSWER": r"##ANSWER", "dolar": r"\$", "latex": r"\\(times|frac|text|cdot|left|right|le|ge)\b|\{=latex\}|newcounter|pandocbounded|makeatletter", "md_negrita": r"\*\*", "backtick": r"`",
                "NA/NaN/Inf/NULL": r"\bNA\b|\bNaN\b|\bInf\b|\bNULL\b|no disponible", "html_visible": r"<\s*(span|div|table|td|th|strong|p)\b|&lt;|&gt;|&amp;|&nbsp;|&#", "tabla_md": r"\|\s*-{3,}|:::", "mojibake": r"Ã|Â|â€|�|\\u[0-9a-f]{4}",
                "dos_espacios_o_espacio_signo": r"[^\s]  +[^\s]| [,.;:]", "R_fuga": r"\bpaste0?\(|\bsample\(|\bc\(\"|\bfunction\("}
        for k, p in pats.items():
            for m in re.finditer(p, txt.replace(NB, " ")):
                add(idx, seed, f"fuga/formato [{k}]: «{txt[max(0,m.start()-30):m.end()+30]!r}»"); break
        if raw.count("\\(") or raw.count("\\["): add(idx, seed, "delimitadores TeX \\( o \\[ sin convertir en el HTML")
        nmath = len(li.find_all("math"))
        for m in re.finditer(SINTILDE, txt.replace(NB, " "), flags=re.I):
            add(idx, seed, f"posible tilde faltante: «{m.group(0)}» en «{txt[max(0,m.start()-25):m.end()+25]!r}»")
        for m in re.finditer(r"\b(\w{2,})\s+\1\b", txt.replace(NB, " "), flags=re.I):
            add(idx, seed, f"palabra repetida: «{m.group(0)}»")
        if txt.count("«") != txt.count("»"): add(idx, seed, f"comillas « » desbalanceadas {txt.count('«')} vs {txt.count('»')}")
        if txt.count("(") != txt.count(")"): add(idx, seed, "paréntesis desbalanceados")
        # --- partes ---
        parts = {}
        for i, c in enumerate(q):
            if c.name == "p" and c.find("strong"):
                m = re.match(r"Parte (\d)\.", c.get_text().strip())
                if m: parts.setdefault(int(m.group(1)), []).append((c, q[i+1] if i+1 < len(q) else None))
        if sorted(parts) != [1,2,3,4,5,6] or any(len(v) != 1 for v in parts.values()): add(idx, seed, f"partes presentes/repetidas: { {k: len(v) for k, v in parts.items()} }")
        # --- tablas ---
        tabs = li.find_all("table"); T = []
        for t in tabs:
            rr = [[norm(td.get_text()) for td in tr.find_all(["th", "td"])] for tr in t.find_all("tr")]
            T.append(rr)
        if len(T) != 2 or any(len(r) != 3 or any(len(x) != 3 for x in r) for r in T): add(idx, seed, f"tablas inesperadas: {[ (len(r), [len(x) for x in r]) for r in T]}")
        t1, t2 = T[0], T[1]
        s1 = [(r[0], nint(r[1]), nint(r[2])) for r in t1[1:]]; s2 = [(r[0], nint(r[1]), nint(r[2])) for r in t2[1:]]
        for nm, tb in (("tabla1", t1), ("tabla2", t2)):
            for r in tb[1:]:
                if not re.fullmatch(r"\d+ cursos?", r[1]) or not re.fullmatch(r"\d+ kilos?", r[2]): add(idx, seed, f"{nm}: celda con formato raro {r}")
        tot1 = [n*a for _, n, a in s1]; tot2 = [n*a for _, n, a in s2]
        g1 = 0 if tot1[0] > tot1[1] else 1; g2 = 0 if tot2[0] > tot2[1] else 1; p2_ = 1 - g2
        D1 = abs(tot1[0]-tot1[1]); DD = abs(tot2[0]-tot2[1])
        rama1 = "CURSOS" if s1[g1][1] > s1[1-g1][1] else "PROMEDIO"; rama2 = "CURSOS" if s2[g2][1] > s2[p2_][1] else "PROMEDIO"
        if (s1[g1][1]-s1[1-g1][1])*(s1[g1][2]-s1[1-g1][2]) >= 0: add(idx, seed, f"tabla1: dominancia o empate {s1}")
        if (s2[g2][1]-s2[p2_][1])*(s2[g2][2]-s2[p2_][2]) >= 0: add(idx, seed, f"tabla2: dominancia o empate {s2}")
        # --- Parte 1 ---
        o1 = [norm(x) for x in parts[1][0][1].get_text().split(" / ")]
        if len(o1) != 4 or len(set(o1)) != 4: add(idx, seed, f"Parte 1: {len(o1)} opciones, {len(set(o1))} distintas")
        canon = any("hubo un curso que recogió más que los otros" in o for o in o1)
        # --- Parte 2 ---
        qtxt = norm(parts[2][0][0].get_text()); o2 = [norm(x) for x in parts[2][0][1].get_text().split(" / ")]
        tipo = "promedio" if "¿Qué indica" in qtxt else ("comparar" if "¿Qué se compara" in qtxt else "?")
        if tipo == "?": add(idx, seed, f"Parte 2: tipo no reconocido «{qtxt}»"); tipo = "promedio"
        S = P2[tipo]
        if len(o2) != 4 or len(set(o2)) != 4: add(idx, seed, f"Parte 2: {len(o2)} opciones, {len(set(o2))} distintas")
        keys = [o for o in o2 if o in S["clave"]]; diss = [o for o in o2 if o in S["dis"].values()]
        if len(keys) != 1 or len(diss) != 3: add(idx, seed, f"Parte 2 ({tipo}): {len(keys)} claves y {len(diss)} distractores reconocidos de 4: {o2}")
        for o in o2:
            if "," in o or ":" in o or "=" in o: add(idx, seed, f"Parte 2: coma/dos puntos/igual en opción «{o}»")
            if not o.endswith("."): add(idx, seed, f"Parte 2: opción sin punto final «{o}»")
            if tipo == "promedio" and not o.startswith("Lo que recogió "): add(idx, seed, f"Parte 2 promedio: no empieza igual «{o}»")
            if tipo == "comparar" and not (o.startswith("La ") or o.startswith("El ")) : add(idx, seed, f"Parte 2 comparar: no empieza con La/El «{o}»")
            if tipo == "comparar" and (" de un grado y " not in o or "del otro grado" not in o): add(idx, seed, f"Parte 2 comparar: sin 'de un grado y' / 'del otro grado' «{o}»")
        mat = (re.search(r"¿Cuántos kilos de (.+?) recogió en total el grado (\w+)\?", norm(parts[3][0][0].get_text())) or [None, "?"])[1]
        if tipo == "promedio" and f"promedio de {mat} recogido por curso" not in qtxt: add(idx, seed, f"Parte 2: la pregunta no contiene 'promedio de {mat} recogido por curso': «{qtxt}»")
        # --- Partes 3-5 ---
        g3 = re.search(r"el grado (\w+)\?", norm(parts[3][0][0].get_text())).group(1); g4 = re.search(r"el grado (\w+)\?", norm(parts[4][0][0].get_text())).group(1)
        names2 = [r[0].lower() for r in s2]
        if sorted([g3.lower(), g4.lower()]) != sorted(names2): add(idx, seed, f"Partes 3/4 preguntan {g3}, {g4}; la tabla tiene {names2}")
        for k in (3, 4, 5):
            if norm(parts[k][0][1].get_text()) != "": add(idx, seed, f"Parte {k}: el párrafo de campo no está vacío en el HTML: «{norm(parts[k][0][1].get_text())[:60]}»")
        # --- Parte 6 ---
        o6 = [norm(x) for x in parts[6][0][1].get_text().split(" / ")]
        if len(o6) != 4 or len(set(o6)) != 4: add(idx, seed, f"Parte 6: {len(o6)} opciones, {len(set(o6))} distintas")
        Gn, Pn = s2[g2], s2[p2_]
        def truth(s):
            m = re.fullmatch(r"Ganó el concurso el grado (\w+)\.", s)
            if m: return ("ganador", m.group(1).lower() == Gn[0].lower())
            if s.startswith("Si el grado que perdió hubiera tenido el mismo promedio por curso que el que ganó, conservando su cantidad de cursos"): return ("HP", Pn[1]*Gn[2] > Gn[1]*Gn[2])
            m = re.match(r"Si al grado que ganó le hubiera faltado un curso, conservando su promedio por curso, habría perdido", s)
            if m: return ("HG", (Gn[1]-1)*Gn[2] < Pn[1]*Pn[2])
            m = re.match(r"Si el grado que perdió hubiera participado con un curso más, conservando su promedio por curso, habría ganado", s)
            if m: return ("HPmas", (Pn[1]+1)*Pn[2] > Gn[1]*Gn[2])
            return ("otra", None)
        tr6 = {o: truth(o) for o in o6}
        # --- Solución ---
        secs, name = [], None
        for c in sol:
            if c.name == "p" and c.find("strong") and norm(c.get_text()) == norm(c.find("strong").get_text()): name = norm(c.get_text()); secs.append([name, []])
            elif name is not None: secs[-1][1].append(c)
        names = [s[0] for s in secs]; sd = {s[0]: s[1] for s in secs}
        esperados = ["Respuestas", "Procedimiento correcto", "Análisis de los distractores", S["head"], "Parte 6 · Afirmaciones falsas", "Propiedades del concepto", "Caso específico", "Reflexión metacognitiva", "Estrategia para evitar el error"]
        for e in esperados:
            if e not in names: add(idx, seed, f"Solution: falta el encabezado «{e}»")
        otro = P2["comparar" if tipo == "promedio" else "promedio"]["head"]
        if otro in names: add(idx, seed, f"Solution: aparece el encabezado del OTRO tipo «{otro}» (tipo {tipo})")
        if len([n for n in names if n.startswith("Parte 2 ·")]) != 1: add(idx, seed, f"Solution: encabezados 'Parte 2 ·' = {[n for n in names if n.startswith('Parte 2 ·')]}")
        if len(names) != len(set(names)): add(idx, seed, f"Solution: encabezados repetidos {[n for n, c in collections.Counter(names).items() if c > 1]}")
        for e in sd.get("Respuestas", [])[:1]:
            items = []
            for l in e.find_all("li", recursive=False):
                own = norm("".join(x.get_text() if getattr(x, "name", None) else str(x) for x in l.contents if getattr(x, "name", None) != "ul"))
                nested = l.find("ul"); subs = [norm(z.get_text()) for z in nested.find_all("li", recursive=False)] if nested else []
                items.append((own, subs))
            if [i[0][:8] for i in items[:5]] != ["Parte 1.", "Parte 2.", "Parte 3.", "Parte 4.", "Parte 5."] or len(items) != 7: add(idx, seed, f"Respuestas: estructura inesperada {[i[0][:30] for i in items]}")
            else:
                a1 = items[0][0]; k1 = re.search(r"«(.+)»", a1).group(1)
                if k1 not in o1: add(idx, seed, "Respuestas Parte 1: la clave no está entre las opciones de la Parte 1")
                nomG = s1[g1][0].lower()
                cands = [o for o in o1 if nomG in o.lower() and f"{D1} kilos" in o.replace(NB, " ")]
                if not (len(cands) >= 1 and k1 in cands): add(idx, seed, f"Respuestas Parte 1: la clave no cita al ganador ({nomG}) con {D1} kilos")
                if k1 not in [o for o in o1] : pass
                a2 = re.search(r"«(.+)»", items[1][0]).group(1)
                if a2 not in keys: add(idx, seed, f"Respuestas Parte 2: «{a2}» no es la clave detectada {keys}")
                v3 = nint(items[2][0].split(".", 1)[1]); v4 = nint(items[3][0].split(".", 1)[1]); v5 = nint(items[4][0].split(".", 1)[1])
                t3 = [r for r in s2 if r[0].lower() == g3.lower()][0]; t4 = [r for r in s2 if r[0].lower() == g4.lower()][0]
                if v3 != t3[1]*t3[2] or v4 != t4[1]*t4[2] or v5 != DD: add(idx, seed, f"Respuestas Partes 3-5: {v3},{v4},{v5} vs esperado {t3[1]*t3[2]},{t4[1]*t4[2]},{DD}")
                if g3.lower() not in items[2][0].lower() or g4.lower() not in items[3][0].lower(): add(idx, seed, f"Respuestas: el grado nombrado en Partes 3/4 no coincide ({items[2][0]} | {items[3][0]})")
                tv, fv = items[5][1], items[6][1]
                strip = lambda x: x.strip("«»")
                for s in tv:
                    if strip(s) not in o6: add(idx, seed, f"Respuestas Parte 6 (V): afirmación no está entre las opciones «{s}»")
                    elif tr6[strip(s)][1] is False: add(idx, seed, f"Respuestas Parte 6: «{strip(s)}» listada VERDADERA pero la tabla dice FALSA ({tr6[strip(s)][0]})")
                for s in fv:
                    if strip(s) not in o6: add(idx, seed, f"Respuestas Parte 6 (F): afirmación no está entre las opciones «{s}»")
                    elif tr6[strip(s)][1] is True: add(idx, seed, f"Respuestas Parte 6: «{strip(s)}» listada FALSA pero la tabla dice VERDADERA ({tr6[strip(s)][0]})")
                if sorted(map(strip, tv + fv)) != sorted(o6): add(idx, seed, "Respuestas Parte 6: V+F no cubren exactamente las 4 opciones")
                if not tv or not fv: add(idx, seed, f"Parte 6: {len(tv)} verdaderas y {len(fv)} falsas")
                nverd = len(tv)
        # Parte 2 en Procedimiento
        pr = " ".join(norm(c.get_text()) for c in sd.get("Procedimiento correcto", []))
        m = re.search(r"Parte 2\. (.+?)(?= Partes 3 a 5\.)", pr)
        if not m or m.group(1) != S["clx"] + " " + S["tail"]: add(idx, seed, f"Procedimiento Parte 2 no corresponde al tipo {tipo}: «{m.group(1) if m else None}»")
        # Análisis de distractores Parte 2
        ul = [c for c in sd.get(S["head"], []) if c.name == "ul"]
        if len(ul) != 1: add(idx, seed, f"Solution Parte 2: {len(ul)} listas")
        else:
            bl = [norm(x.get_text()) for x in ul[0].find_all("li", recursive=False)]
            if len(bl) != 3: add(idx, seed, f"Solution Parte 2: {len(bl)} viñetas (se esperaban 3)")
            seen = set()
            for b in bl:
                mm = re.fullmatch(r"«(.+?)» (.+)", b)
                if not mm: add(idx, seed, f"Solution Parte 2: viñeta sin formato «…» explicación: {b[:80]}"); continue
                opt, ex = mm.group(1), mm.group(2); seen.add(opt)
                nmk = [k for k, v in S["dis"].items() if v == opt]
                if not nmk: add(idx, seed, f"Solution Parte 2: «{opt}» no es un distractor del tipo {tipo}"); continue
                if ex != EXPL[nmk[0]]: add(idx, seed, f"Solution Parte 2: explicación de {nmk[0]} distinta: «{ex}»")
            if seen != set(diss): add(idx, seed, f"Solution Parte 2: distractores listados {sorted(seen)} ≠ los de la pregunta {sorted(diss)}")
            if any(k in seen for k in keys): add(idx, seed, "Solution Parte 2: la CLAVE aparece entre los distractores")
        # Parte 6 falsas
        ul6 = [c for c in sd.get("Parte 6 · Afirmaciones falsas", []) if c.name == "ul"]
        if len(ul6) == 1:
            b6 = [norm(x.get_text()) for x in ul6[0].find_all("li", recursive=False)]
            fl = [re.match(r"«(.+?)» ", b).group(1) if re.match(r"«(.+?)» ", b) else None for b in b6]
            if len(fl) != len(fv) or sorted(map(str, fl)) != sorted(map(strip, fv)): add(idx, seed, f"Parte 6 · Afirmaciones falsas ≠ lista de falsas de Respuestas ({len(fl)} vs {len(fv)})")
        else: add(idx, seed, f"Parte 6 · Afirmaciones falsas: {len(ul6)} listas")
        # Solution mencionan el mismo ganador/diferencia
        casos = " ".join(norm(c.get_text()) for c in sd.get("Caso específico", []))
        if f"{DD}" not in casos or f"{tot2[g2]}" not in casos or f"{tot2[p2_]}" not in casos: add(idx, seed, f"Caso específico no cita {tot2[g2]}, {tot2[p2_]}, {DD}: «{casos[:140]}»")
        if f"{D1}" not in casos or f"{tot1[g1]}" not in casos: add(idx, seed, "Caso específico no cita los totales de la Parte 1")
        verbo_enun = "recogido" if "papel recogido" in norm(parts[1][0][0].get_text()) or True else ""
        head1 = t1[0][2]
        rows.append((idx, seed, "CANON" if canon else "-", rama1, rama2, tipo, mat, [r[0] for r in s2], DD, nmath, nverd, head1))
    except Exception as e:
        import traceback; add(idx, seed, f"EXCEPCIÓN en el análisis: {e!r} {traceback.format_exc().splitlines()[-3]}")
out = ["v  sem  canon rama1    rama2(otro)  tipo_p2   material  grados            D'  math  nVerd  cabecera tabla1"]
for r in rows: out.append(f"v{r[0]:<2d} {r[1]:>3d}  {r[2]:5s} {r[3]:8s} {r[4]:9s}   {r[5]:9s} {r[6]:9s} {'/'.join(r[7]):17s} {r[8]:3d}  {r[9]:3d}   {r[10]}      {r[11]}")
out.append(""); out.append(f"Resumen: canónicas={sum(1 for r in rows if r[2]=='CANON')}; tipo promedio={sum(1 for r in rows if r[5]=='promedio')}, comparar={sum(1 for r in rows if r[5]=='comparar')}; otro colegio CURSOS={sum(1 for r in rows if r[4]=='CURSOS')}, PROMEDIO={sum(1 for r in rows if r[4]=='PROMEDIO')}; Parte 1 CURSOS={sum(1 for r in rows if r[3]=='CURSOS')}, PROMEDIO={sum(1 for r in rows if r[3]=='PROMEDIO')}")
cross = collections.Counter((r[5], r[4]) for r in rows); out.append(f"Cruce tipo_p2 x rama otro colegio: {dict(cross)}")
out.append(""); out.append(f"INCIDENCIAS ({len(issues)}):"); out += issues
open("analisis_texto_r3.txt", "w", encoding="utf-8").write("\n".join(out)); print("\n".join(out)[:12000])
