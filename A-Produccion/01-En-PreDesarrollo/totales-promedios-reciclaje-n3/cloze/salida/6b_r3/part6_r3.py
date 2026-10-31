#!/usr/bin/env python3
"""part6_r3.py — 6b r3: evaluador GENERAL de las hipótesis de la Parte 6 contra la tabla del otro colegio, y marca-vs-verdad del feedback (a)-(f)."""
import re, collections
from bs4 import BeautifulSoup
SEEDS = [7,3,2,1,4,5,6]+list(range(8,25)); NB = " "
norm = lambda s: re.sub(r"\s+", " ", s.replace(NB, " ")).strip()
PATS = [("prom",   r"Si el grado que (ganó|perdió) hubiera tenido el mismo promedio por curso que el que (?:ganó|perdió), conservando su cantidad de cursos, habría (ganado|perdido) el concurso\."),
        ("cursos", r"Si el grado que (ganó|perdió) hubiera participado con tantos cursos como el que (?:ganó|perdió), conservando su promedio por curso, habría (ganado|perdido) el concurso\."),
        ("mas1",   r"Si el grado que (ganó|perdió) hubiera participado con un curso más, conservando su promedio por curso, habría (ganado|perdido) el concurso\."),
        ("menos1", r"Si al grado que (ganó|perdió) le hubiera faltado un curso, conservando su promedio por curso, habría (ganado|perdido) el concurso\.")]
iss, freq, nverd = [], collections.Counter(), []
for idx, seed in enumerate(SEEDS, start=1):
    soup = BeautifulSoup(open(f"html/v{idx}.html", encoding="utf-8").read(), "html.parser")
    for a in soup.find_all("annotation"): a.decompose()
    li = soup.select_one("body > ol > li"); kids = [c for c in li.children if getattr(c, "name", None)]
    tabs = li.find_all("table"); rows = [[norm(td.get_text()) for td in tr.find_all(["th", "td"])] for tr in tabs[1].find_all("tr")][1:]
    G = [(r[0].lower(), int(re.search(r"\d+", r[1]).group()), int(re.search(r"\d+", r[2]).group())) for r in rows]
    tot = [n*a for _, n, a in G]; w = 0 if tot[0] > tot[1] else 1; W, L = G[w], G[1-w]; TW, TL = tot[w], tot[1-w]
    ps = [c for c in kids if c.name == "p"]; p6i = [i for i, c in enumerate(ps) if re.match(r"Parte 6\.", c.get_text().strip()) and c.find("strong")][0]
    o6 = [norm(x) for x in ps[p6i+1].get_text().split(" / ")]
    def truth(s):
        m = re.fullmatch(r"Ganó el concurso el grado (\w+)\.", s)
        if m: return "ganador", (m.group(1).lower() == W[0]), None
        for name, pat in PATS:
            m = re.fullmatch(pat, s)
            if not m: continue
            S, claim = m.group(1), m.group(2); (Sg, O) = (W, L) if S == "ganó" else (L, W); TO = TL if S == "ganó" else TW
            new = {"prom": Sg[1]*O[2], "cursos": O[1]*Sg[2], "mas1": (Sg[1]+1)*Sg[2], "menos1": (Sg[1]-1)*Sg[2]}[name]
            if new == TO: return name+"/"+S, None, (new, TO)
            wins = new > TO; return name+"/"+S, (wins if claim == "ganado" else (not wins)), (new, TO)
        return "SIN_PATRÓN", None, None
    tr = {s: truth(s) for s in o6}
    for s, (k, v, nt) in tr.items():
        freq[k] += 1
        if k == "SIN_PATRÓN" or v is None: iss.append(f"v{idx} (semilla {seed}): afirmación sin evaluar/empate [{k}] «{s}»")
    # Respuestas V/F
    sol = [c for c in kids if c.name == "h4"][1]; after = list(sol.next_siblings); after = [c for c in after if getattr(c, "name", None)]
    ul = [c for c in after if c.name == "ul"][0]; items = ul.find_all("li", recursive=False)
    V = [norm(z.get_text()) .strip("«»") for z in items[5].find("ul").find_all("li", recursive=False)]; F = [norm(z.get_text()).strip("«»") for z in items[6].find("ul").find_all("li", recursive=False)]
    nverd.append(len(V))
    for s in V:
        if tr.get(s, (0, None))[1] is not True: iss.append(f"v{idx} (semilla {seed}): listada VERDADERA pero evaluada {tr.get(s)} «{s}»")
    for s in F:
        if tr.get(s, (0, None))[1] is not False: iss.append(f"v{idx} (semilla {seed}): listada FALSA pero evaluada {tr.get(s)} «{s}»")
    # explicaciones de falsas: deben citar el nuevo total y el total del otro
    h = [c for c in after if c.name == "p" and norm(c.get_text()) == "Parte 6 · Afirmaciones falsas"][0]; fl = h.find_next_sibling("ul").find_all("li", recursive=False)
    for z in fl:
        t = norm(z.get_text()); st = re.match(r"«(.+?)» (.*)", t).group(1); ex = re.match(r"«(.+?)» (.*)", t).group(2); k, v, nt = tr[st]
        if nt and (str(nt[0]) not in ex or str(nt[1]) not in ex): iss.append(f"v{idx} (semilla {seed}): explicación de falsa [{k}] no cita {nt}: «{ex[:120]}»")
        if k == "ganador" and (str(TW) not in ex or str(TL) not in ex): iss.append(f"v{idx} (semilla {seed}): explicación de «Ganó…» no cita {TW} y {TL}: «{ex[:100]}»")
    # feedback (a)-(f): marca vs verdad
    ol = [c for c in after if c.name == "ol"]; fb = [c for c in ol if c.get("type") == "a" and len(c.find_all("li", recursive=False)) == 6]
    if len(fb) != 1: iss.append(f"v{idx} (semilla {seed}): lista de feedback (a)-(f) no encontrada ({len(fb)})"); continue
    fbi = [norm(z.get_text()).split(" / ") for z in fb[0].find_all("li", recursive=False)]
    ok1 = [norm(x) for x in ps[[i for i, c in enumerate(ps) if re.match(r"Parte 1\.", c.get_text().strip()) and c.find("strong")][0]+1].get_text().split(" / ")]
    key1 = re.search(r"«(.+)»", norm(items[0].get_text())).group(1); key2 = re.search(r"«(.+)»", norm(items[1].get_text())).group(1)
    ps2 = [i for i, c in enumerate(ps) if re.match(r"Parte 2\.", c.get_text().strip()) and c.find("strong")][0]; o2 = [norm(x) for x in ps[ps2+1].get_text().split(" / ")]
    for nm, opts, key, fbl in (("P1", ok1, key1, fbi[0]), ("P2", o2, key2, fbi[1])):
        if len(fbl) != 4: iss.append(f"v{idx} (semilla {seed}): feedback {nm} con {len(fbl)} entradas"); continue
        for o, f in zip(opts, fbl):
            if f.startswith("Verdadero.") != (o == key): iss.append(f"v{idx} (semilla {seed}): feedback {nm} marca «{f[:12]}» para «{o[:50]}» (clave={o == key})")
    if len(fbi[5]) != 4: iss.append(f"v{idx} (semilla {seed}): feedback P6 con {len(fbi[5])} entradas")
    else:
        for o, f in zip(o6, fbi[5]):
            if tr[o][1] is not None and f.startswith("Verdadero.") != tr[o][1]: iss.append(f"v{idx} (semilla {seed}): feedback P6 marca «{f[:12]}» para «{o[:60]}» (evaluada {tr[o][1]})")
print("tipos de afirmación de la Parte 6 (frecuencia):", dict(freq)); print("verdaderas por versión:", collections.Counter(nverd))
print(f"INCIDENCIAS Parte 6 / feedback ({len(iss)}):"); [print(" ", x[:300]) for x in iss]
