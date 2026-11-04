#!/usr/bin/env python3
"""FASE 2C r6: oráculo propio. Lee SOLO el texto emitido (xexams, Moodle) y recalcula desde las tablas que se muestran."""
import json, re, html, os, collections, sys
B = os.path.expanduser('~/.cache/fase2c_r6_cloze/')
SUB = '/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/totales-promedios-reciclaje-n3/'
NB = '\u00a0'
nb = lambda s: s.replace(NB, ' ')
FRASE = "Por eso, para saber si el grado que perdió habría ganado con un curso más, o si el que ganó habría perdido con uno menos, basta comparar el promedio de cada uno con la diferencia entre los totales: si el promedio es mayor, sí; si es menor, no."
PRE = "Parte 6. Cada hipótesis cambia un solo dato de la tabla. Un curso más con el mismo promedio suma ese promedio al total, y un curso menos lo resta. "
RE_PAR = re.compile('^' + re.escape(PRE + FRASE) + r' Con un curso más, el grado (\S+) habría sumado a su total (\d+) kilos, (más|menos) que la diferencia de (\d+) kilos: (habría ganado|no le habría alcanzado para ganar)\. Con un curso menos, el grado (\S+) habría restado a su total (\d+) kilos, (más|menos) que la diferencia de (\d+) kilos: (habría perdido|habría seguido ganando)\.$')
fallos = collections.Counter(); ej = {}
def F(c, s, msg=''):
    fallos[c] += 1; ej.setdefault(c, (s, msg[:300]))

def celdas(lines):
    out, cur = [], []
    for l in lines:
        if l.startswith('|'):
            if re.match(r'^\|[:\-|]+\|$', l): continue
            c = [nb(re.sub(r'<span[^>]*>|</span>|\*\*', '', x)).strip() for x in l.strip().strip('|').split('|')]
            cur.append(c)
        elif cur:
            out.append(cur); cur = []
    if cur: out.append(cur)
    return out
def tabla(c):
    T = {}
    for r in c[1:]:
        T[r[0].lower()] = (int(re.match(r'(\d+) cursos$', r[1]).group(1)), int(re.match(r'(\d+) kilos$', r[2]).group(1)))
    return T
def sujeto(t):
    m = re.match(r'^La (?:sede|jornada de la) (.+?) ganó', t); return m.group(1).lower()
RAZ = {'M': ['participaron más cursos y, aunque el promedio fue menor,', ', aunque su promedio fue menor, participaron más cursos y'],
       'P': [', aunque participaron menos cursos, tuvo mayor promedio y', 'tuvo mayor promedio y, aunque participaron menos cursos,'],
       'N': [', al sumar lo que recogieron todos y cada uno de sus cursos,', ', después de juntar lo que recogieron todos sus cursos,'],
       'U': [', al sumar solo lo que recogieron sus cursos más grandes,', 'en ella hubo un curso que recogió más que los otros, y', 'en sus cursos hay más alumnos para recoger, y por eso', 'sus cursos reciclaron durante más días que los demás, y']}
ITEM = open(SUB + 'ITEM_OFICIAL.md', encoding='utf-8').read().split('\n')
C_ENUN = ' '.join(l[2:] for l in ITEM[37:40]); C_PREG = ITEM[43][2:]
C_OPC = {}
for l in ITEM:
    m = re.match(r'^\| (\*\*)?([A-D])(\*\*)? \| (\*\*)?(.+?)(\*\*)? \|$', l)
    if m and m.group(2) not in C_OPC: C_OPC[m.group(2)] = m.group(5)
C_TAB = [[x.strip() for x in l.strip().strip('|').split('|')] for l in ITEM[47:51] if not l.startswith('|---')]
assert C_ENUN.startswith('En un colegio') and C_ENUN.endswith('secundaria.') and C_PREG.startswith('De acuerdo') and len(C_OPC) == 4 and len(C_TAB) == 3, (C_ENUN, C_PREG, C_OPC, C_TAB)
def p1_verdad(t, T):
    t = nb(t); S = sujeto(t); O = [k for k in T if k != S][0]
    ns, as_ = T[S]; no, ao = T[O]; ts, to = ns * as_, no * ao
    if t in C_OPC.values():
        L = [k for k, v in C_OPC.items() if v == t][0]
        if L == 'A': return ts > to and ts - to == 40
        if L == 'B': return ts > to and ns > no and as_ < ao and ts - to == 20
        if L == 'C': return ts > to and as_ - ao == 5
        return False
    m = re.search(r' su total fue más alto que el de la (?:sede|jornada de la) (.+?) en (\d+) kilos\.$', t)
    if not m or m.group(1).lower() != O: return None
    x = int(m.group(2))
    cuerpo = t[:m.start() + 1]
    ti = [k for k, v in RAZ.items() if any(r in cuerpo for r in v)]
    if len(ti) != 1: return None
    r = {'M': ns > no and as_ < ao, 'P': ns < no and as_ > ao, 'N': True, 'U': False}[ti[0]]
    return ts > to and ts - to == x and r
P2 = {'Lo que recogió ese grado dividido en partes iguales entre sus cursos.': lambda n, a, n2: n * a / n,
      'Lo que recogió ese grado dividido entre el número de sus cursos.': lambda n, a, n2: n * a / n,
      'Lo que recogió ese grado dividido entre la cantidad de cursos de ese mismo grado.': lambda n, a, n2: n * a / n,
      'Lo que recogió ese grado dividido entre el promedio de sus cursos.': lambda n, a, n2: n * a / a,
      'Lo que recogió ese grado dividido en partes iguales entre sus cursos y luego entre dos.': lambda n, a, n2: n * a / n / 2,
      'Lo que recogió ese grado dividido entre el número de sus cursos al cuadrado.': lambda n, a, n2: n * a / n ** 2,
      'Lo que recogió ese grado dividido entre la cantidad de cursos de ambos grados.': lambda n, a, n2: n * a / (n + n2),
      'Lo que recogió la mayoría de los cursos de ese mismo grado.': lambda n, a, n2: None}
def p2_verdad(t, T):
    if t not in P2: return None
    ks = list(T); return all(P2[t](T[k][0], T[k][1], T[[j for j in ks if j != k][0]][0]) == T[k][1] for k in ks)
def gp(T):
    g = max(T, key=lambda k: T[k][0] * T[k][1]); p = min(T, key=lambda k: T[k][0] * T[k][1]); return g, p, T[g][0], T[g][1], T[p][0], T[p][1]
def p6_verdad(t, T):
    g, p, nG, aG, nP, aP = gp(T); TG, TP = nG * aG, nP * aP
    if t.startswith('Ganó el concurso el grado '): return t[len('Ganó el concurso el grado '):-1] == g
    return {HP: (nP + 1) * aP > TG, HG: (nG - 1) * aG < TP,
            BCP: nG * aP > TG, BCG: nP * aG < TP, BPP: nP * aG > TG, BPG: nG * aP < TP}.get(t)
HP = 'Si el grado que perdió hubiera participado con un curso más, conservando su promedio por curso, habría ganado el concurso.'
HG = 'Si al grado que ganó le hubiera faltado un curso, conservando su promedio por curso, habría perdido el concurso.'
BCP = 'Si el grado que perdió hubiera participado con tantos cursos como el que ganó, conservando su promedio por curso, habría ganado el concurso.'
BCG = 'Si el grado que ganó hubiera participado con tantos cursos como el que perdió, conservando su promedio por curso, habría perdido el concurso.'
BPP = 'Si el grado que perdió hubiera tenido el mismo promedio por curso que el que ganó, conservando su cantidad de cursos, habría ganado el concurso.'
BPG = 'Si el grado que ganó hubiera tenido el mismo promedio por curso que el que perdió, conservando su cantidad de cursos, habría perdido el concurso.'
def fb6(t, T):
    g, p, nG, aG, nP, aP = gp(T); TG, TP = nG * aG, nP * aP
    if t.startswith('Ganó el concurso'): return f'Ganó el grado {g}, con {TG} kilos frente a los {TP} del grado {p}.'
    if t == HP:
        return f'Con un curso más, el grado {p} habría recogido ${TP} + {aP} = {TP + aP}$ kilos, ' + (f'más que los {TG} del grado {g}: habría ganado.' if TP + aP > TG else f'que no superan los {TG} del grado {g}: habría seguido perdiendo.')
    if t == HG:
        return f'Con un curso menos, el grado {g} habría recogido ${TG} - {aG} = {TG - aG}$ kilos, ' + (f'menos que los {TP} del grado {p}: habría perdido.' if TG - aG < TP else f'que siguen superando los {TP} del grado {p}: habría seguido ganando.')
    q, n, a, r, rn = {BCP: (p, nG, aP, TG, g), BCG: (g, nP, aG, TP, p), BPP: (p, nP, aG, TG, g), BPG: (g, nG, aP, TP, p)}[t]
    return f'Con ese cambio, el grado {q} habría recogido ${n} \\times {a} = {n * a}$ kilos, ' + (f'más que los {r} del grado {rn}: habría ganado.' if n * a > r else f'menos que los {r} del grado {rn}: habría perdido.')
def arit(txt, s, c='arit'):
    n = 0
    for m in re.finditer(r'\$([^$]+)\$', txt):
        e = re.sub(r'\\ ?\\text\{[^}]*\}|\\text\{[^}]*\}', '', m.group(1)).strip()
        r = re.fullmatch(r'(\d+) (\\times|-|\+) (\d+) = (\d+)', e)
        if not r: F(c + '_no_juzgada', s, e); continue
        a, op, b, v = int(r[1]), r[2], int(r[3]), int(r[4]); n += 1
        if {'\\times': a * b, '-': a - b, '+': a + b}[op] != v: F(c, s, e)
    return n

C = json.load(open(B + 'emit_cloze.json', encoding='utf-8')); S = json.load(open(B + 'emit_schoice.json', encoding='utf-8'))
cnt = collections.Counter()
for s, e in C.items():
    q = e['question']; ql = e['questionlist']; sl = e['solutionlist']; m = e['meta']; sol_raw = '\n'.join(e['solution']); sol = nb(sol_raw)
    an = [l.strip() for l in q if re.fullmatch(r'\s*##ANSWER\d+##\s*', l)]
    if an != [f'##ANSWER{i}##' for i in range(1, 7)]: F('answer_orden', s, str(an))
    qt = '\n'.join(q)
    for i in range(1, 7):
        if not re.search(rf'\*\*Parte {i}\.\*\*[^\n]*\n\s*\n##ANSWER{i}##', qt): F('answer_tras_su_parte', s, str(i))
    if m['clozetype'] != ['schoice', 'schoice', 'num', 'num', 'num', 'mchoice'] or m['points'] != [3, 1, 1, 1, 1, 1] or m['tolerance'] != [0] * 6 or m['shuffle'] != [True]: F('meta', s)
    if len(ql) != 15 or len(sl) != 15 or any(x for x in ql[8:11]): F('longitudes', s, f'{len(ql)} {len(sl)}')
    so = m['solution']
    if [len(x) for x in so] != [4, 4, 1, 1, 1, 4]: F('sol_longitud', s)
    g1, g2, g6 = ql[:4], ql[4:8], ql[11:15]
    tabs = celdas(q)
    if len(tabs) != 2: F('tablas', s, str(len(tabs))); continue
    T1, T2 = tabla(tabs[0]), tabla(tabs[1])
    enun = next(l for l in q if l.strip() and not l.startswith(('```', '\\', '|', ':::')))
    can = nb(enun) == C_ENUN
    cnt['canonicas'] += can
    if can:
        if [nb(x) for x in tabs[0][0]] != C_TAB[0] or [r for r in tabs[0][1:]] != C_TAB[1:]: F('canon_tabla', s, str(tabs[0]))
        if sorted(nb(x) for x in g1) != sorted(C_OPC.values()): F('canon_opciones', s, str(g1))
    if not re.search(r'\*\*Parte 1\.\*\* ' + re.escape(C_PREG), qt): F('pregunta', s)
    ## Parte 1
    v1 = [p1_verdad(x, T1) for x in g1]
    if None in v1: F('p1_no_juzgado', s, str(g1))
    if v1 != so[0]: F('p1_marca', s, str((v1, so[0], g1)))
    if sum(1 for x in v1 if x) != 1: F('p1_unica', s, str(v1))
    ## Parte 2
    v2 = [p2_verdad(x, T2) for x in g2]
    if None in v2: F('p2_no_juzgado', s, str(g2))
    if v2 != so[1] or sum(1 for x in v2 if x) != 1: F('p2_marca', s, str((v2, so[1])))
    ## Partes 3 a 5
    for i, gi in ((3, 2), (4, 3)):
        mm = re.search(rf'\*\*Parte {i}\.\*\* ¿Cuántos kilos de \S+ recogió en total el grado (\S+)\?', qt); gr = mm.group(1)
        if T2[gr][0] * T2[gr][1] != so[gi][0]: F('num_total', s, f'{i}')
    g, p, nG, aG, nP, aP = gp(T2); TG, TP = nG * aG, nP * aP; D = TG - TP
    if D != so[4][0] or not re.search(r'\*\*Parte 5\.\*\* El grado que ganó recogió más \S+ que el otro\. ¿Cuántos kilos más recogió\?', qt): F('num_dif', s)
    ## Parte 6
    v6 = [p6_verdad(x, T2) for x in g6]
    if None in v6: F('p6_no_juzgado', s, str(g6))
    if v6 != so[5]: F('p6_marca', s, str((v6, so[5])))
    if not (any(v6) and not all(v6)): F('p6_mezcla', s)
    for x in (HP, HG):
        if g6.count(x) != 1: F('p6_sin_hp_hg', s)
    vmap = dict(zip(g6, v6))
    ## string de la metainformación coherente
    ## Feedback alineado con la opción, en el orden emitido
    G1 = gp(T1); gk, pk = G1[0], G1[1]
    nom = {sujeto(nb(x)): nb(x).split(' ganó')[0] for x in g1}
    Gc, Pc = nom[gk], nom[pk]; gl, pl = 'l' + Gc[1:], 'l' + Pc[1:]
    nG1, aG1, nP1, aP1 = G1[2:]; TG1, TP1 = nG1 * aG1, nP1 * aP1; D1 = TG1 - TP1
    tipo = 'jornada' if 'jornada' in gl else 'sede'
    cursos = nG1 > nP1; k1 = abs(nG1 - nP1); d1 = abs(aG1 - aP1)
    pG1 = k1 * aG1 if cursos else nG1 * d1; pP1 = nP1 * d1 if cursos else k1 * aP1
    if D1 != pG1 - pP1: F('p1_descomposicion', s)
    roles = {}
    for x, f, v in zip(g1, sl[:4], v1):
        t = nb(x); f = nb(f); suj = sujeto(t)
        cif = [int(z) for z in re.findall(r'(\d+) kilos', t)]
        if v:
            roles['clave'] = t
            ex = f'Verdadero. {Gc} recogió ${nG1} \\times {aG1} = {TG1}\\ \\text{{kilos}}$ y {pl}, ${nP1} \\times {aP1} = {TP1}\\ \\text{{kilos:}}$ ganó por ${TG1} - {TP1} = {D1}\\ \\text{{kilos.}}$'
        elif suj == gk:
            roles['gdis'] = t; ex = f'Falso. {Gc} sí ganó, pero por {D1} kilos, no por {cif[0]}.'
        elif any(r in t for r in RAZ['U']) or 'hubo un curso que recogió' in t:
            roles['psin'] = t; ex = f'Falso. {Pc} no ganó ({TP1} kilos frente a {TG1}), y la razón que da no sale de la tabla, que solo informa la cantidad de cursos y el promedio por curso de cada {tipo}.'
        else:
            roles['padv'] = t; ex = f'Falso. {Pc} recogió {TP1} kilos, menos que los {TG1} de {gl}: ' + ('tener mayor promedio' if cursos else 'tener más cursos') + ' no le bastó.'
        if f != ex: F('fb_p1', s, f + ' ### ' + ex)
    for x, f, v in zip(g2, sl[4:8], v2):
        f = nb(f)
        if v != f.startswith('Verdadero.'): F('fb_p2_vf', s, f)
        cl = {'entre el promedio de sus cursos': 'da la cantidad de cursos', 'luego entre dos': 'la mitad del promedio', 'al cuadrado': 'al cuadrado', 'ambos grados': 'los cursos de los dos grados', 'la mayoría': 'la moda'}
        if not v:
            if not any(a in x and b in f for a, b in cl.items()): F('fb_p2_contenido', s, x + ' ## ' + f)
        elif 'lo que le tocaría a cada curso si el total se repartiera por igual' not in f: F('fb_p2_contenido', s, f)
    for i, idx in ((3, 8), (4, 9)):
        gr = re.search(rf'\*\*Parte {i}\.\*\* ¿Cuántos kilos de \S+ recogió en total el grado (\S+)\?', qt).group(1); n, a = T2[gr]
        if nb(sl[idx]) != f'En el grado {gr} participaron {n} cursos y cada uno recogió, en promedio, {a} kilos: ${n} \\times {a} = {n * a}$ kilos.': F('fb_num', s, sl[idx])
    if nb(sl[10]) != f'Ganó el grado {g}: ${TG} - {TP} = {D}$ kilos más que el grado {p}.': F('fb_num', s, sl[10])
    for x, f, v in zip(g6, sl[11:15], v6):
        if nb(f) != ('Verdadero. ' if v else 'Falso. ') + fb6(x, T2): F('fb_p6', s, nb(f) + ' ### ' + fb6(x, T2))
    ## Solution
    cnt['arit'] += arit(sol_raw, s); cnt['arit_fb'] += sum(arit(x, s, 'arit_fb') for x in sl)
    r = re.search(r'- Parte 1\. La respuesta correcta es la que afirma: «(.+?)»\n- Parte 2\. «(.+?)»\n- Parte 3\. (\d+) kilos, el total del grado (\S+)\.\n- Parte 4\. (\d+) kilos, el total del grado (\S+)\.\n- Parte 5\. (\d+) kilos\.\n- Parte 6\. Son verdaderas:\n((?:    - «.+»\n)+)- Parte 6\. Son falsas:\n((?:    - «.+»\n)+)', sol)
    if not r: F('sol_respuestas', s); continue
    if r[1] != roles['clave']: F('sol_r1', s)
    if r[2] != nb(g2[v2.index(True)]): F('sol_r2', s)
    if int(r[3]) != T2[r[4]][0] * T2[r[4]][1] or int(r[5]) != T2[r[6]][0] * T2[r[6]][1] or r[4] == r[6] or int(r[7]) != D: F('sol_r345', s)
    ver = re.findall(r'«(.+)»', r[8]); fal = re.findall(r'«(.+)»', r[9])
    if sorted(ver) != sorted(k for k, v in vmap.items() if v) or sorted(fal) != sorted(k for k, v in vmap.items() if not v): F('sol_r6', s)
    filas = re.findall(r'^- (L[ae] .+?): \$(\d+) \\times (\d+) = (\d+)\\ \\text\{kilos\.\}\$$', sol, re.M)
    if len(filas) != 2 or any((a := sujeto(x + ' ganó')) not in T1 or T1[a] != (int(n), int(c)) or int(n) * int(c) != int(t) for x, n, c, t in filas): F('sol_proc_filas', s, str(filas))
    txt_g = f'Ganó {gl}, con ${TG1} - {TP1} = {D1}\\ \\text{{kilos}}$ más. ' + (f'Su promedio era menor ({aG1} kilos frente a {aP1}), pero participaron más cursos ({nG1} frente a {nP1}), y eso pesó más.' if cursos else f'Participaron menos cursos ({nG1} frente a {nP1}), pero su promedio era mayor ({aG1} kilos frente a {aP1}), y eso pesó más.')
    if txt_g not in sol: F('sol_gano_p1', s, txt_g)
    if f'Las dos afirmaciones que dan el triunfo a {pl} son falsas, porque esa {tipo} no ganó. De las dos que se lo dan a {gl}, solo una cita la diferencia correcta, {D1} kilos.' not in sol: F('sol_dos_afirm', s)
    filas2 = re.findall(r'^- Grado (\S+): \$(\d+) \\times (\d+) = (\d+)\$ kilos\.$', sol, re.M)
    if len(filas2) != 2 or any(T2[x] != (int(n), int(c)) or int(n) * int(c) != int(t) for x, n, c, t in filas2): F('sol_proc_filas2', s)
    cursos2 = nG > nP
    txt2 = f'Ganó el grado {g}, con ${TG} - {TP} = {D}$ kilos más. ' + (f'Su promedio era menor ({aG} kilos frente a {aP}), pero participaron más cursos ({nG} frente a {nP}).' if cursos2 else f'Participaron menos cursos ({nG} frente a {nP}), pero su promedio era mayor ({aG} kilos frente a {aP}).')
    if txt2 not in sol: F('sol_gano_p2', s, txt2)
    ## Párrafo «Parte 6.»
    pr_raw = re.findall(r'^Parte 6\. Cada hipótesis.*$', sol_raw, re.M)
    if len(pr_raw) != 1: F('par_uno', s); continue
    if pr_raw[0].count(FRASE) != 1: F('par_literal', s)
    if len(re.findall(r'\d+' + NB + 'kilos', pr_raw[0])) != 4 or re.search(r'\d kilos', pr_raw[0]): F('par_L6', s)
    if not re.search('(habría' + NB + 'perdido|seguido' + NB + 'ganando)\\.$', pr_raw[0]): F('par_ed_final', s)
    ma = RE_PAR.match(nb(pr_raw[0]))
    if not ma: F('par_forma', s, pr_raw[0]); continue
    gp_, ap_, c1, d1_, cc1, gg_, ag_, c2, d2_, cc2 = ma.groups()
    ap_, ag_, d1_, d2_ = int(ap_), int(ag_), int(d1_), int(d2_)
    if gp_ != p or gg_ != g or ap_ != aP or ag_ != aG or d1_ != D or d2_ != D: F('par_cifras', s, pr_raw[0])
    if aP == D or aG == D: F('par_empate', s)
    if (c1 == 'más') != (ap_ > d1_) or (c2 == 'más') != (ag_ > d2_): F('par_compara', s)
    hp_real = (nP + 1) * aP > TG; hg_real = (nG - 1) * aG < TP
    if (cc1 == 'habría ganado') != hp_real or (cc2 == 'habría perdido') != hg_real: F('par_conclusion', s)
    if (aP > D) != hp_real or (aG > D) != hg_real: F('par_criterio', s)
    if vmap[HP] != hp_real or vmap[HG] != hg_real or so[5][g6.index(HP)] != hp_real or so[5][g6.index(HG)] != hg_real: F('par_marca', s)
    cnt['ramas_%s_%s' % (c1, c2)] += 1
    ## Parte 1: análisis de distractores
    bl = re.findall(r'\*\*Parte 1 · (ERR-[A-Z]+) — [^*]+\*\*.*?\*Afirmación:\* «(.+?)»\n\n\*Lectura que lleva a elegirla:\* [^\n]+\n\n([^\n]+)\n', sol, re.S)
    if len(bl) != 3 or sorted(b[1] for b in bl) != sorted([roles['gdis'], roles['padv'], roles['psin']]): F('dis_bloques', s, str(len(bl))); continue
    Tk = lambda: None
    for cod, af, ex in bl:
        cif = [int(z) for z in re.findall(r'(\d+) kilos', af)]; x = cif[-1] if cif else None
        if can and 'porque recogió' in af: x = 40
        if cod == 'ERR-DATO':
            q_ = gl if x == aG1 else pl if x == aP1 else None
            e_ = f'La cifra {x} kilos es el promedio de {q_}, un dato de la tabla, no la diferencia entre los totales. {Gc} recogió ${nG1} \\times {aG1} = {TG1}\\ \\text{{kilos}}$ y {pl}, ${nP1} \\times {aP1} = {TP1}\\ \\text{{kilos:}}$ la diferencia es {D1} kilos.'
        elif cod == 'ERR-RESTA':
            e_ = f'La cifra {x} sale de restar los promedios: ${max(aG1, aP1)} - {min(aG1, aP1)} = {d1}\\text{{.}}$ Esa resta compara un curso típico de cada {tipo}, no las {tipo}s completas: como participaron {nG1} y {nP1} cursos, la diferencia entre los totales es ${TG1} - {TP1} = {D1}\\ \\text{{kilos.}}$'
            if x != d1: F('dis_x', s, ex)
        elif cod == 'ERR-TOTAL':
            e_ = f'La cifra {x} es el total de {gl}: ${nG1} \\times {aG1} = {TG1}\\ \\text{{kilos.}}$ No es la diferencia: falta restarle el total de {pl}, y ${TG1} - {TP1} = {D1}\\ \\text{{kilos.}}$'
            if x != TG1: F('dis_x', s, ex)
        elif cod == 'ERR-PARCIAL':
            if x != pG1: F('dis_x', s, ex)
            if cursos:
                e_ = f'La cifra ${k1} \\times {aG1} = {x}$ cuenta solo lo que ' + ('aporta el curso' if k1 == 1 else f'aportan los {k1} cursos') + f' de más de {gl} y olvida que en {pl} cada curso recogió, en promedio, {d1} kilos más: ${nP1} \\times {d1} = {pP1}\\ \\text{{kilos}}$ que compensan parte de esa ventaja. La diferencia es ${x} - {pP1} = {D1}\\ \\text{{kilos.}}$'
            else:
                e_ = f'La cifra ${nG1} \\times {d1} = {x}$ cuenta solo la ventaja de promedio de {gl} en sus {nG1} cursos y olvida que en {pl} ' + ('participó 1 curso más, que aporta $' if k1 == 1 else f'participaron {k1} cursos más, que aportan $') + f'{k1} \\times {aP1} = {pP1}\\ \\text{{kilos.}}$ La diferencia es ${x} - {pP1} = {D1}\\ \\text{{kilos.}}$'
        elif cod == 'ERR-PROM':
            if can: e_ = f'Es cierto que el promedio de {pl} es {d1} kilos mayor, pero allí participaron menos cursos: su total, ${nP1} \\times {aP1} = {TP1}\\ \\text{{kilos,}}$ es menor que el de {gl}, {TG1} kilos. {Pc} no ganó.'
            else: e_ = f'Es cierto que el promedio de {pl} es {d1} kilos mayor. La cifra ${nP1} \\times {d1} = {x}$ suma esa ventaja en sus {nP1} cursos, pero olvida que en {gl} ' + ('participó 1 curso más, que aporta $' if k1 == 1 else f'participaron {k1} cursos más, que aportan $') + f'{k1} \\times {aG1} = {pG1}\\ \\text{{kilos.}}$ {Pc} no ganó: recogió {TP1} kilos, menos que los {TG1} de {gl}.'
        elif cod == 'ERR-CURSOS':
            e_ = f'Es cierto que en {pl} ' + ('participó 1 curso' if k1 == 1 else f'participaron {k1} cursos') + f' más. La cifra ${k1} \\times {aP1} = {x}$ cuenta solo lo que ' + ('aporta el curso' if k1 == 1 else 'aportan los cursos') + f' de más, pero olvida que en {gl} cada curso recogió, en promedio, {d1} kilos más: ${nG1} \\times {d1} = {pG1}\\ \\text{{kilos.}}$ {Pc} no ganó: recogió {TP1} kilos, menos que los {TG1} de {gl}.'
        else:
            e_ = None
            suf = (f' La cifra de {x} kilos tampoco es la diferencia entre los totales, que es de {D1} kilos.' if not can else '') + f' Además, {pl} no ganó: recogió {TP1} kilos, menos que los {TG1} de {gl}.'
            if not ex.endswith(suf): F('dis_inventa', s, ex)
            if x == D1: F('dis_inventa_x', s, ex)
        if e_ is not None and ex != e_: F('dis_' + cod, s, ex + ' ### ' + e_)
        cnt['dis_' + cod] += 1
    dis2 = re.search(r'\*\*Parte 2 · Lecturas del promedio que no sirven\*\*(.*?)\*\*Parte 6 · Afirmaciones falsas\*\*(.*?)\*\*Propiedades', sol, re.S)
    l2 = re.findall(r'^- «(.+?)» ', dis2[1], re.M); l6 = re.findall(r'^- «(.+?)» (.+)$', dis2[2], re.M)
    if sorted(l2) != sorted(x for x, v in zip(g2, v2) if not v): F('dis_p2', s)
    if sorted(x for x, _ in l6) != sorted(k for k, v in vmap.items() if not v) or any(y != fb6(x, T2) for x, y in l6): F('dis_p6', s, str(l6))
    if f'En la Parte 1, {gl} recogió ${nG1} \\times {aG1} = {TG1}\\ \\text{{kilos}}$ y {pl} recogió ${nP1} \\times {aP1} = {TP1}\\ \\text{{kilos.}}$ La diferencia es ${TG1} - {TP1} = {D1}\\ \\text{{kilos}}$ a favor de {gl}.' not in sol: F('caso1', s)
    if f'En el otro colegio, el grado {g} recogió ${nG} \\times {aG} = {TG}$ kilos y el grado {p}, ${nP} \\times {aP} = {TP}$ kilos: la diferencia es {D} kilos.' not in sol: F('caso2', s)
    ## SCHOICE, por texto emitido
    E = S[s]; a = [l for l in E['question'] if l.strip()]; tS = celdas(E['question'])
    if tS[0] != tabs[0] or nb(next(l for l in E['question'] if l.strip() and not l.startswith(('```', '\\', '|', ':::')))) != nb(enun): F('schoice_estimulo', s)
    if sorted(E['questionlist']) != sorted(g1): F('schoice_opciones', s)
    so_s = dict(zip(E['questionlist'], E['meta']['solution'])); so_c = dict(zip(g1, so[0]))
    if so_s != so_c: F('schoice_clave', s)
    if dict(zip(E['questionlist'], E['solutionlist'])) != dict(zip(g1, sl[:4])): F('schoice_feedback', s)
    cnt['versiones'] += 1
print('E1, texto emitido:', dict(cnt)); print('FALLOS emitido:', dict(fallos))
for k, v in ej.items(): print('  ej', k, v)

## ---- 1000 semillas, chunks crudos: lecturas y alcance ------------------------------------
R = json.load(open(B + 'crudo_1000.json', encoding='utf-8'))
z = collections.Counter(); ko = 0
for r in R:
    p = r['par']; T = tabla(celdas(r['tabla'].split('\n'))[0]); g, q, nG, aG, nP, aP = gp(T); TG, TP = nG * aG, nP * aP; D = TG - TP
    ma = RE_PAR.match(nb(p)); z['literal'] += p.count(FRASE) == 1
    if not ma: ko += 1; continue
    hp = (nP + 1) * aP > TG; hg = (nG - 1) * aG < TP; opc = r['opc6']; mk = r['ex'].split('|')[5]; v = dict(zip(opc, [c == '1' for c in mk]))
    z['hp_tabla'] += (aP > D) == hp; z['hg_tabla'] += (aG > D) == hg; z['hp_marca'] += v[HP] == hp; z['hg_marca'] += v[HG] == hg; z['emp'] += aP == D or aG == D
    z['hp_resuelta'] += (ma[5] == 'habría ganado') == hp; z['hg_resuelta'] += (ma[10] == 'habría perdido') == hg
    z['a_hp'] += (aG > D) != hp; z['a_hg'] += (aP > D) != hg; z['b_mixtas'] += hp != hg; z['b_vv'] += hp and hg; z['b_ff'] += (not hp) and (not hg)
    z['c_hp_si'] += hp; z['c_hg_si'] += hg
    z['d_hp'] += (aP > aG) != hp; z['d_hg'] += (aG > aP) != hg
    z['e_tabla'] += nP == nG + 1
    if nP == nG + 1: z['e_hp_v'] += hp; z['e_hg_v'] += hg
    z['e_hp_contra'] += ((nG + 1) * aP > TG) != hp; z['e_hg_contra'] += ((nP - 1) * aG < TP) != hg
    z['f_hp'] += (aP > D - aP) != hp; z['f_hg'] += (aG > D - aG) != hg
    bc = next((x for x in (BCP, BCG) if x in v), None); bp = next((x for x in (BPP, BPG) if x in v), None)
    if bc:
        z['bc'] += 1; dif = nG - nP; verd = v[bc]; crit = (aP if bc == BCP else aG) > D
        if dif == 1: z['bc_dentro'] += 1; z['bc_dentro_contra'] += crit != verd
        elif dif >= 2: z['bc_k2'] += 1; z['bc_k2_contra'] += crit != verd
        elif dif == -1: z['bc_inv1'] += 1; z['bc_inv1_contra'] += crit != verd
        else: z['bc_inv2'] += 1; z['bc_inv2_contra'] += crit != verd
    else:
        z['bp'] += 1
print('\n1000 semillas (chunks crudos): sin forma', ko, dict(z))
