"""Revisores del video: cuatro miradas ajenas a quien lo escribió. INFORMAN; no bloquean ni corrigen.

    guion        antes de gastar voz y dibujo: lee el guion como estudiante y lo contrasta con el ítem oficial
    oido         con la voz (y la mezcla, si existe): pausas, palabras pegadas y lo que oyó el transcriptor
    pantalla     con el video montado, un revisor por formato: fotogramas cada 2 s y uno por palabra ancla
    herramienta  al cerrar: compara lo que corrigió el profesor con lo que vieron los otros y propone lecciones

Este programa no revisa nada: arma el EXPEDIENTE de cada revisor con datos medidos, comprueba que las citas
de sus hallazgos existan y junta el informe para el profesor. Quien revisa es un agente distinto del autor.

    python -m video_icfes.revisores preparar   <carpeta> <guion|oido|pantalla|herramienta> [--formato h|v]
    python -m video_icfes.revisores comprobar  <carpeta> <guion|oido|pantalla_h|pantalla_v|herramienta>
    python -m video_icfes.revisores resumen    <carpeta>
    python -m video_icfes.revisores correccion <carpeta> "<lo que pidió cambiar el profesor>"
    python -m video_icfes.revisores registrar  <carpeta> P1 [P2 ...]     (solo las que el profesor acepte)
    python -m video_icfes.revisores lecciones

Códigos de salida: 0 bien · 1 hallazgos mal formados o citas que no existen · 2 uso incorrecto o falta material."""
import datetime, json, pathlib, re, subprocess, sys

from . import guion_lint, proyecto

PAQ = pathlib.Path(__file__).resolve().parent
INSTR = PAQ.parent / "revisores"
LECCIONES = INSTR / "lecciones.json"
GRAVEDADES = ("alta", "media", "baja")
MAX_HALLAZGOS = 10
CAMPOS = ("id", "escena", "donde", "cita", "que_entiende_el_estudiante", "propuesta", "gravedad")
FUENTE = "/usr/share/fonts/noto/NotoSans-Regular.ttf"
MENOS_CAMBIO = 6          # cuántas palabras ancla con menos cambio en pantalla se le señalan al revisor
CONJ = {"y", "e", "o", "u", "pero", "ni", "sino", "mientras"}


def falta(msg):
    print("FALTA MATERIAL:", msg); sys.exit(2)


def plano(t):
    return re.sub(r"\s+", " ", t).strip().lower()


def lecciones(rev=None, solo_vigentes=True):
    ls = json.loads(LECCIONES.read_text()) if LECCIONES.exists() else []
    return [l for l in ls if (rev is None or l["revisor"] == rev) and (not solo_vigentes or l["estado"] == "vigente")]


def bloque_lecciones(rev):
    ls = lecciones(rev)
    if not ls:
        return "## Lecciones de videos anteriores\n\nNinguna registrada todavía para este revisor.\n"
    return ("## Lecciones de videos anteriores (defectos que ya se escaparon: búscalos expresamente)\n\n"
            + "\n".join(f"- {l['id']} ({l['video']}, {l['fecha']}): {l['leccion']}" for l in ls) + "\n")


def ejercicio_de(d):
    for p in d.parents:
        if (p / "ITEM_OFICIAL.md").exists():
            return p
    falta(f"ninguna carpeta por encima de {d} tiene ITEM_OFICIAL.md")


def guion_numerado(g):
    out = []
    for k, frases in g.SEG.items():
        out.append(f"\n### Escena {k}\n")
        out += [f"- `{k}[{i}]` {fr[0]}" for i, fr in enumerate(frases)]
    return "\n".join(out)


def carpeta_rev(d, nombre):
    r = d / "revisores" / nombre
    r.mkdir(parents=True, exist_ok=True)
    return r


def cierre(d, nombre, R, extra=""):
    """Lo último del expediente: dónde escribir y cómo comprobarse. Devuelve el encargo para el agente."""
    cad = f"env PYTHONPATH={PAQ.parent} {sys.executable} -m video_icfes.revisores comprobar {d} {nombre}"
    base = nombre.split("_")[0]
    encargo = (f"Eres el revisor «{nombre}» de un video explicativo ICFES. No escribiste este video y no lo corriges: informas.\n"
               f"1. Lee completo {INSTR / 'comun.md'} y {INSTR / (base + '.md')}.\n"
               f"2. Lee completo tu expediente: {R / 'expediente.md'}{extra}\n"
               f"3. Escribe tus hallazgos en {R / 'hallazgos.json'} con el formato de comun.md.\n"
               f"4. Corre `{cad}` y corrige tu archivo hasta que salga con 0.\n"
               f"5. Responde con una lista corta de tus hallazgos (id, gravedad, una línea) y lo que NO pudiste revisar.")
    (R / "encargo.txt").write_text(encargo + "\n")
    print(f"Expediente: {R / 'expediente.md'}\n\n--- ENCARGO PARA EL AGENTE (Sonnet, sin name:, distinto del autor) ---\n{encargo}")


# ------------------------------------------------------------------ guion
def preparar_guion(d):
    g = proyecto.guion(d); ej = ejercicio_de(d); R = carpeta_rev(d, "guion")
    err, avisos = guion_lint.revisar(g)
    rmds = [str(r) for r in sorted(ej.rglob("*.Rmd")) if "salida" not in r.relative_to(ej).parts]
    n = sum(len(fr[0].split()) for frs in g.SEG.values() for fr in frs)
    (R / "expediente.md").write_text(f"""# Expediente del revisor «guion» — {ej.name}

Preparado el {datetime.datetime.now():%Y-%m-%d %H:%M}. El guion tiene {len(g.SEG)} escenas y {n} palabras.

## Archivos (solo lectura)
- Guion: `{d / 'guion.py'}` (lo que dice la voz está en `SEG`)
- Ítem oficial: `{ej / 'ITEM_OFICIAL.md'}`
- Ejercicio aprobado (mira su sección Solution; NO lo edites ni lo tejas): {", ".join(f"`{r}`" for r in rmds)}
- Plan de escenas, si existe: `{d / 'plan_escenas.md'}`

## El guion, frase por frase (así se cita: `guion s9[2]`)
{guion_numerado(g)}

## Lo que ya dijo la revisión automática del guion
Errores: {err or "ninguno"}
Avisos:
{chr(10).join("- " + a for a in avisos) or "- ninguno"}

{bloque_lecciones("guion")}""")
    cierre(d, "guion", R)


# ------------------------------------------------------------------ oído
def preparar_oido(d, fmt):
    g = proyecto.guion(d); R = carpeta_rev(d, "oido")
    if not (d / "audio" / "palabras.json").exists():
        falta("no hay voz todavía (corra la cadena con --etapa revisar)")
    pal = proyecto.con_signos(g.SEG, proyecto.leer("palabras.json", d))
    ini = proyecto.leer(f"inicios_{fmt}.json", d) if (d / "audio" / f"inicios_{fmt}.json").exists() else None
    T = lambda k, t: f"{ini[k] + t:.1f} s" if ini else f"{t:.1f} s de la escena"
    pausas, pegadas, sin_signo, invertidas = [], [], [], []
    for k, frases in g.SEG.items():
        ws = pal[k]; j = 0
        for i, fr in enumerate(frases):
            n = len(fr[0].split()); tramo = ws[j:j + n]; sig = ws[j + n] if j + n < len(ws) else None; j += n
            marcas, en_signo = [], []
            for a, b in zip(tramo, tramo[1:]):
                hueco = b[1] - (a[1] + a[2])
                if a[0][-1] in ",;:.?!…":
                    marcas.append(f"tras «{a[0]}» {hueco:.2f} s"); en_signo.append((hueco, a[0], b[0]))
                elif hueco > 0.25:
                    sin_signo.append(f"- `{k}[{i}]` ({T(k, a[1])}): la voz se detiene {hueco:.2f} s entre «{a[0]}» y «{b[0]}», donde el guion no tiene signo")
                x, y = proyecto.norm(a[0]), proyecto.norm(b[0])
                if hueco <= 0.03 and a[0][-1] not in ",;:.?!…" and x and y and x[-1] in "aeiouáéíóú" and y[0] in "aeiouáéíóúh":
                    pegadas.append(f"- `{k}[{i}]` ({T(k, a[1])}): «{a[0]} {b[0]}» van sin separación ({max(hueco, 0):.2f} s)")
            # una idea nueva suele empezar con «y», «o», «pero»…: ahí la pausa debería ser la más larga de la frase, no la más corta
            for hueco, x, y in en_signo:
                otras = [h for h, x2, _ in en_signo if x2 is not x]
                if proyecto.norm(y) in CONJ and otras and hueco < min(otras) - 0.04:
                    invertidas.append(f"- `{k}[{i}]` ({T(k, tramo[0][1])}): la pausa antes de «{y}» (tras «{x}», {hueco:.2f} s) es la MÁS CORTA de la frase "
                                      f"(las otras: {', '.join(f'{h:.2f}' for h in otras)} s). Frase: {fr[0]}")
            if sig is not None:
                marcas.append(f"fin de frase {sig[1] - (tramo[-1][1] + tramo[-1][2]):.2f} s")
            pausas.append(f"- `{k}[{i}]` ({T(k, tramo[0][1])}) {fr[0]}\n  - pausas: {' · '.join(marcas) or 'ninguna'}")
    mj = d / f"revision_{fmt}" / "audio_medidas.json"
    if mj.exists():
        m = json.loads(mj.read_text())
        oyo = "\n".join(f"- después de «…{c}»: el guion dice «{r}» y el transcriptor escribió «{h}»" for c, r, h in m["distintas"]) or "- ninguna"
        medidas = (f"Mezcla del formato {fmt}: {m['lufs']} LUFS, pico {m['pico_dbfs']} dBFS, ventaja de la voz en 1-3 kHz en el 10 % peor: "
                   f"{m['p10_1_3khz']:.1f} dB. Compuertas: {m['puertas']}.\n\n### Palabras que el transcriptor automático oyó distinto\n{oyo}\n\n"
                   f"Informe completo y transcripción: `{mj.with_suffix('.md')}`")
    else:
        medidas = "Todavía no hay mezcla ni transcripción (el video no se ha montado): revisa solo pausas y palabras pegadas."
    (R / "expediente.md").write_text(f"""# Expediente del revisor «oído»

Preparado el {datetime.datetime.now():%Y-%m-%d %H:%M}. Nadie ha oído este audio: todo lo de aquí son mediciones.
Los tiempos son {"segundos desde el inicio del video (formato " + fmt + ")" if ini else "segundos dentro de cada escena"}.

## Pausas de la voz en cada signo del guion (así se cita: `guion s9[2]`, o `expediente` con el texto literal)
Una pausa corta donde termina una idea y larga en medio de otra hace que el oído agrupe mal las palabras.
{chr(10).join(pausas)}

## Frases donde la pausa más corta cae justo antes de «y», «o», «pero», «ni» (calculado)
Ahí suele empezar la segunda mitad de la idea. Si esa pausa es la más corta, el oído pega el final de la primera
mitad con el comienzo de la segunda. Lee cada frase agrupándola como la agruparía el oído y di si cambia el sentido.
{chr(10).join(invertidas) or "- ninguna"}

## Pausas donde el guion no tiene signo
{chr(10).join(sin_signo) or "- ninguna mayor de 0,25 s"}

## Palabras que la voz pega (vocal con vocal, sin separación)
Pegadas pueden oírse como otra palabra («va en» → «van»). No todas son un problema: júzgalas una por una.
{chr(10).join(pegadas) or "- ninguna"}

## Mediciones de la mezcla
{medidas}

{bloque_lecciones("oido")}""")
    cierre(d, "oido", R)


# ------------------------------------------------------------------ pantalla
def hojas(imgs, rotulos, destino, prefijo, cols, ancho):
    """Junta las imágenes en hojas de cols×2 (h) o cols×1 (v), con su rótulo encima. Devuelve [(hoja, [rótulos])]."""
    from PIL import Image, ImageDraw, ImageFont
    try:
        fuente = ImageFont.truetype(FUENTE, 22)
    except OSError:
        fuente = ImageFont.load_default()
    for vieja in destino.glob(f"{prefijo}_*.png"):
        vieja.unlink()
    por_hoja = cols * (2 if ancho >= 600 else 1)
    out = []
    for h in range(0, len(imgs), por_hoja):
        grupo = imgs[h:h + por_hoja]; rs = rotulos[h:h + por_hoja]
        ims = [Image.open(p).convert("RGB") for p in grupo]
        ims = [im.resize((ancho, round(im.height * ancho / im.width))) for im in ims]
        alto = ims[0].height + 30; filas = -(-len(ims) // cols)
        hoja = Image.new("RGB", (cols * ancho, filas * alto), (255, 255, 255))
        dib = ImageDraw.Draw(hoja)
        for i, (im, r) in enumerate(zip(ims, rs)):
            x, y = (i % cols) * ancho, (i // cols) * alto
            dib.text((x + 6, y + 2), r, fill=(0, 0, 0), font=fuente); hoja.paste(im, (x, y + 30))
        ruta = destino / f"{prefijo}_{h // por_hoja + 1:02d}.png"; hoja.save(ruta); out.append((ruta, rs))
    return out


def cambio_visible(antes, despues):
    """Porcentaje del cuadro (sin la franja de subtítulos) que cambió entre dos fotogramas."""
    import numpy as np
    from PIL import Image
    # se promedia en bloques de 12 px: la cámara se mueve uno o dos píxeles y eso no es un cambio que vea el estudiante
    A, B = (Image.open(x).convert("RGB") for x in (antes, despues))
    w, h = A.size; h = int(h * 0.86)
    a, b = (np.asarray(im.crop((0, 0, w, h)).resize((w // 12, h // 12), Image.BOX), dtype=int) for im in (A, B))
    return 100 * float((np.abs(a - b).max(axis=2) > 16).mean())


def preparar_pantalla(d, fmt):
    g = proyecto.guion(d); nombre = f"pantalla_{fmt}"; R = carpeta_rev(d, nombre)
    F = d / f"revision_{fmt}" / "fotogramas"; video = d / f"{g.NOMBRE}_{fmt}.mp4"
    fotos = sorted(F.glob("seg_*.png")) if F.exists() else []
    if not fotos or not video.exists():
        falta(f"no hay video montado ni fotogramas del formato {fmt} (corra la cadena completa)")
    ini = proyecto.leer(f"inicios_{fmt}.json", d); pal = proyecto.leer("palabras.json", d)
    anc = proyecto.leer(f"anclas_{fmt}.json", d)
    A = R / "anclas"; A.mkdir(exist_ok=True)
    for vieja in A.glob("ancla_*.png"):
        vieja.unlink()
    escala = "scale=960:-1" if fmt == "h" else "scale=540:-1"
    fa, ra, tabla, quietas = [], [], [], []
    for vieja in list(A.glob("antes_*.png")) + list(A.glob("durante_*.png")):
        vieja.unlink()
    for i, (k, palabra, n, _ok) in enumerate(anc["usadas"], 1):
        t = proyecto.t_palabra(pal, k, palabra, n)
        if t is None:
            continue
        seg = ini[k] + t + 0.8
        p = A / f"ancla_{i:03d}.png"
        subprocess.run(["ffmpeg", "-y", "-v", "error", "-ss", f"{seg:.2f}", "-i", str(video), "-frames:v", "1", "-vf", escala, str(p)], check=True)
        if p.exists():
            # 0,5 s ANTES de la palabra (las animaciones se adelantan 0,25 s a su palabra): si la pantalla casi no cambió, lo atado a la palabra no se nota
            q = A / f"antes_{i:03d}.png"
            subprocess.run(["ffmpeg", "-y", "-v", "error", "-ss", f"{max(0, ini[k] + t - 0.5):.2f}", "-i", str(video), "-frames:v", "1", "-vf", escala, str(q)], check=True)
            cambio = cambio_visible(q, p) if q.exists() else None
            fa.append(p); ra.append(f"ancla_{i:03d} {k} «{palabra}» vez {n} ({seg:.1f} s)")
            tabla.append(f"- `ancla_{i:03d}`: escena {k}, 0,8 s después de que la voz dice «{palabra}» (vez {n}), en el segundo {seg:.1f}"
                         + ("" if cambio is None else f" — la pantalla cambió en el {cambio:.2f} % de su área"))
            if cambio is not None:
                quietas.append((cambio, i, k, palabra, n, ini[k] + t))
    # Las MENOS_CAMBIO anclas con menos cambio: se les saca además el instante «durante» (0,2 s después de la palabra)
    lineas_q = []
    for cambio, i, k, palabra, n, t0 in sorted(quietas)[:MENOS_CAMBIO]:
        subprocess.run(["ffmpeg", "-y", "-v", "error", "-ss", f"{t0 + 0.2:.2f}", "-i", str(video), "-frames:v", "1", "-vf", escala,
                        str(A / f"durante_{i:03d}.png")], check=True)
        lineas_q.append(f"- `ancla_{i:03d}` (escena {k}, «{palabra}» vez {n}, segundo {t0:.1f}): cambió el {cambio:.2f} % de la pantalla. "
                        f"Abre `antes_{i:03d}.png`, `durante_{i:03d}.png` y `ancla_{i:03d}.png`.")
    cols, ancho = (3, 640) if fmt == "h" else (6, 320)
    H = R / "hojas"; H.mkdir(exist_ok=True)
    h1 = hojas(fotos, [f"{p.stem} ({int(p.stem[4:])} s)" for p in fotos], H, "cada2s", cols, ancho)
    h2 = hojas(fa, [r.split(" (")[0] for r in ra], H, "anclas", cols, ancho)
    atrasos = anc.get("atrasos") or []
    (R / "expediente.md").write_text(f"""# Expediente del revisor «pantalla» — formato {"horizontal 1920×1080" if fmt == "h" else "vertical 1080×1920 (celular)"}

Preparado el {datetime.datetime.now():%Y-%m-%d %H:%M}. Video: `{video}` ({len(fotos)} fotogramas cada 2 s y {len(fa)} de palabras ancla).

## Qué mirar (con la herramienta Read, que muestra imágenes)
1. Las {len(h1)} hojas del video completo, un fotograma cada 2 s, en `{H}`: `cada2s_01.png` … `cada2s_{len(h1):02d}.png`.
   Cada fotograma lleva encima su nombre y su segundo. Así se cita: `fotograma seg_201`.
2. Las {len(h2)} hojas de palabras ancla: `anclas_01.png` … `anclas_{len(h2):02d}.png`. Cada una muestra el video 0,8 s después
   de una palabra a la que el autor ató algo que debe aparecer o resaltarse. Así se cita: `fotograma ancla_017`.
3. Si una hoja no deja ver un detalle, abre el fotograma suelto a tamaño mayor: `{F}/seg_NNN.png` o `{A}/ancla_NNN.png`.
4. La narración con tiempos (lo que se oye en cada segundo): `{d / f'revision_{fmt}' / 'narracion_con_tiempos.md'}`.
5. El código de las escenas, para saber qué pretendía el autor: `{d / 'escenas.py'}` (así se cita: `escenas.py:123`).

## Palabras ancla (qué debía pasar en pantalla y cuándo)
{chr(10).join(tabla)}

## Las {MENOS_CAMBIO} palabras ancla tras las que la pantalla menos cambia (calculado)
El autor ató algo a cada una de estas palabras, pero 0,8 s después la pantalla está casi igual que medio segundo antes
de decirla (sin contar la franja de subtítulos). Puede ser normal, o puede ser un resalte que dura un instante, uno
demasiado tenue o algo que llega tarde. Para CADA una, abre de `{A}` sus tres fotogramas (antes, durante = 0,2 s
después de la palabra, y ancla = 0,8 s después), lee en `escenas.py` qué debía pasar y da tu veredicto en `revisado`
o como hallazgo: ¿un estudiante que mira un celular notaría lo que el autor quiso mostrar, y le da tiempo de verlo?
{chr(10).join(lineas_q) or "- ninguna"}

## Avisos de la cadena
Animaciones que llegaron más de 0,6 s tarde a su palabra (escena, palabra, vez, segundos): {atrasos or "ninguna"}

{bloque_lecciones("pantalla")}""")
    cierre(d, nombre, R)


# ------------------------------------------------------------------ herramienta
def hallazgos_de(d):
    out = {}
    for p in sorted((d / "revisores").glob("*/hallazgos.json")):
        try:
            out[p.parent.name] = json.loads(p.read_text())
        except ValueError:
            out[p.parent.name] = {"hallazgos": [], "error": "JSON ilegible"}
    return out


def preparar_herramienta(d):
    R = carpeta_rev(d, "herramienta"); ej = ejercicio_de(d)
    cp = d / "revisores" / "correcciones_profesor.json"
    corr = json.loads(cp.read_text()) if cp.exists() else []
    hs = hallazgos_de(d); hs.pop("herramienta", None)
    inf = json.loads((d / "informe_cadena.json").read_text()) if (d / "informe_cadena.json").exists() else {}
    lin = []
    for rev, doc in hs.items():
        lin.append(f"\n### {rev}: {len(doc.get('hallazgos', []))} hallazgos")
        lin += [f"- `{h.get('id')}` [{h.get('gravedad')}] {h.get('escena')} · {h.get('donde')}: {h.get('que_entiende_el_estudiante')} → {h.get('propuesta')}"
                for h in doc.get("hallazgos", [])]
    (R / "expediente.md").write_text(f"""# Expediente del revisor «herramienta» — {ej.name}

Preparado el {datetime.datetime.now():%Y-%m-%d %H:%M}.

## Lo que el profesor pidió cambiar en este video ({len(corr)} correcciones; así se cita: el texto literal)
{chr(10).join(f"- ({c['fecha']}) {c['texto']}" for c in corr) or "- ninguna registrada"}

## Lo que vieron los otros revisores
{chr(10).join(lin) or "- ningún revisor dejó hallazgos"}

## Avisos que dio la cadena
- Guion: {inf.get("avisos_guion") or "ninguno"}
- Animaciones tarde: {dict((k, v) for k, v in inf.items() if k.startswith("atrasos_")) or "sin dato"}

## Lecciones ya registradas (no las repitas)
{chr(10).join(f"- {l['id']} [{l['revisor']}, {l['tipo']}, {l['estado']}] {l['leccion']}" for l in lecciones(solo_vigentes=False)) or "- ninguna"}

## La herramienta (léela para proponer DÓNDE iría cada comprobación)
- Comprobaciones del guion: `{PAQ / 'guion_lint.py'}`
- Compuertas y avisos de la cadena: `{PAQ / 'cadena.py'}`
- Medidas de audio: `{PAQ / 'medidas.py'}`
- Expedientes de los revisores: `{PAQ / 'revisores.py'}`
- Instrucciones de cada revisor: `{INSTR}`
""")
    cierre(d, "herramienta", R)


# ------------------------------------------------------------------ comprobar
def comprobar(d, nombre):
    R = d / "revisores" / nombre; hj = R / "hallazgos.json"; mal = []
    if not hj.exists():
        print(f"No existe {hj}"); sys.exit(1)
    try:
        doc = json.loads(hj.read_text())
    except ValueError as e:
        print(f"{hj} no es JSON válido: {e}"); sys.exit(1)
    if nombre == "herramienta":
        return comprobar_herramienta(d, doc)
    for c in ("revisado", "no_revisado", "hallazgos"):
        if c not in doc:
            mal.append(f"falta el campo «{c}»")
    hs = doc.get("hallazgos", [])
    if len(hs) > MAX_HALLAZGOS:
        mal.append(f"hay {len(hs)} hallazgos y el máximo es {MAX_HALLAZGOS}: deja los que más pesan")
    g = proyecto.guion(d); ej = ejercicio_de(d)
    exp = plano((R / "expediente.md").read_text())
    textos = {"item": plano((ej / "ITEM_OFICIAL.md").read_text())}
    esc = (d / "escenas.py").read_text().splitlines() if (d / "escenas.py").exists() else []
    vistos = set()
    for h in hs:
        i = h.get("id", "?")
        mal += [f"{i}: falta «{c}»" for c in CAMPOS if not str(h.get(c, "")).strip()]
        if i in vistos:
            mal.append(f"{i}: id repetido")
        vistos.add(i)
        if h.get("gravedad") not in GRAVEDADES:
            mal.append(f"{i}: gravedad debe ser una de {GRAVEDADES}")
        donde, cita = str(h.get("donde", "")), plano(str(h.get("cita", "")))
        m = re.fullmatch(r"guion (s\d+)\[(\d+)\]", donde)
        if m:
            k, j = m.group(1), int(m.group(2))
            if k not in g.SEG or j >= len(g.SEG[k]):
                mal.append(f"{i}: no existe {donde}")
            elif len(cita) < 8 or cita not in plano(g.SEG[k][j][0]):
                mal.append(f"{i}: la cita no está literal en {donde}: «{g.SEG[k][j][0]}»")
        elif donde.startswith("fotograma "):
            f = donde.split()[1]; fmt = nombre[-1] if nombre.startswith("pantalla_") else "h"
            p = (d / f"revision_{fmt}" / "fotogramas" / f"{f}.png") if f.startswith("seg_") else (R / "anclas" / f"{f}.png")
            if not p.exists():
                mal.append(f"{i}: no existe el fotograma {f} ({p})")
        elif donde in ("expediente", "medidas"):
            if len(cita) < 8 or cita not in exp:
                mal.append(f"{i}: la cita no está literal en el expediente")
        elif donde == "item":
            if len(cita) < 8 or cita not in textos["item"]:
                mal.append(f"{i}: la cita no está literal en ITEM_OFICIAL.md")
        elif re.fullmatch(r"escenas\.py:\d+", donde):
            n = int(donde.split(":")[1])
            if not any(cita and cita in plano(l) for l in esc[max(0, n - 3):n + 2]):
                mal.append(f"{i}: la cita no está en escenas.py cerca de la línea {n}")
        else:
            mal.append(f"{i}: «donde» debe ser `guion sK[i]`, `fotograma seg_NNN`, `fotograma ancla_NNN`, `expediente`, `item` o `escenas.py:N`")
    (R / "comprobacion.json").write_text(json.dumps({"fecha": f"{datetime.datetime.now():%Y-%m-%d %H:%M}", "hallazgos": len(hs), "mal": mal},
                                                    ensure_ascii=False, indent=1))
    if mal:
        print(f"HALLAZGOS DE «{nombre}» SIN ACEPTAR ({len(mal)} problemas):\n" + "\n".join("- " + x for x in mal)); sys.exit(1)
    print(f"Hallazgos de «{nombre}»: {len(hs)}, todos con cita comprobada.")


def comprobar_herramienta(d, doc):
    mal = []
    cp = d / "revisores" / "correcciones_profesor.json"
    corr = [plano(c["texto"]) for c in json.loads(cp.read_text())] if cp.exists() else []
    for b in doc.get("balance", []):
        if plano(str(b.get("correccion", ""))) not in corr:
            mal.append(f"balance: «{b.get('correccion')}» no es una corrección registrada (cópiala literal)")
        if "lo_habia_visto" not in b or not str(b.get("por_que_se_escapo", "")).strip() and not b.get("lo_habia_visto"):
            mal.append(f"balance: «{b.get('correccion')}» necesita «lo_habia_visto» (id o null) y, si es null, «por_que_se_escapo»")
    if len(doc.get("balance", [])) != len(corr):
        mal.append(f"el balance tiene {len(doc.get('balance', []))} filas y hay {len(corr)} correcciones del profesor: una fila por corrección")
    for p in doc.get("propuestas", []):
        i = p.get("id", "?")
        mal += [f"{i}: falta «{c}»" for c in ("id", "defecto", "tipo", "revisor", "leccion", "prueba") if not str(p.get(c, "")).strip()]
        if p.get("tipo") not in ("instruccion", "mecanica"):
            mal.append(f"{i}: tipo debe ser «instruccion» o «mecanica»")
        if p.get("revisor") not in ("guion", "oido", "pantalla", "cadena"):
            mal.append(f"{i}: revisor debe ser guion, oido, pantalla o cadena")
        if p.get("tipo") == "mecanica" and not str(p.get("comprobacion", "")).strip():
            mal.append(f"{i}: una propuesta mecánica dice en «comprobacion» en qué archivo y qué mediría")
    R = d / "revisores" / "herramienta"
    (R / "comprobacion.json").write_text(json.dumps({"mal": mal}, ensure_ascii=False, indent=1))
    if mal:
        print("PROPUESTAS SIN ACEPTAR:\n" + "\n".join("- " + x for x in mal)); sys.exit(1)
    print(f"Balance de {len(corr)} correcciones y {len(doc.get('propuestas', []))} propuestas, bien formadas.")


# ------------------------------------------------------------------ informe para el profesor
def resumen(d):
    hs = hallazgos_de(d); prop = hs.pop("herramienta", None)
    out = [f"# Lo que encontraron los revisores — {ejercicio_de(d).name}\n",
           f"Generado el {datetime.datetime.now():%Y-%m-%d %H:%M}. Los revisores informan: usted decide qué se corrige.\n"]
    todos = []
    for rev, doc in hs.items():
        c = d / "revisores" / rev / "comprobacion.json"
        ok = c.exists() and not json.loads(c.read_text())["mal"]
        out.append(f"\n## Revisor «{rev}»: {len(doc.get('hallazgos', []))} hallazgos"
                   f"{'' if ok else ' — OJO: sus citas NO están comprobadas'}\n")
        out.append(f"- Revisó: {doc.get('revisado', 'no lo dijo')}\n- No revisó: {doc.get('no_revisado', 'no lo dijo')}\n")
        todos += [(GRAVEDADES.index(h.get("gravedad")) if h.get("gravedad") in GRAVEDADES else 3, rev, h) for h in doc.get("hallazgos", [])]
    out.append("\n## Hallazgos, del más grave al menos grave\n")
    for _, rev, h in sorted(todos, key=lambda x: x[0]):
        out.append(f"\n### {h.get('id')} · {h.get('gravedad')} · escena {h.get('escena')} ({rev})\n"
                   f"- **Dónde:** {h.get('donde')} — «{h.get('cita')}»\n"
                   f"- **Qué entendería el estudiante:** {h.get('que_entiende_el_estudiante')}\n"
                   f"- **Propuesta:** {h.get('propuesta')}\n")
    if not todos:
        out.append("\nNinguno.\n")
    if prop:
        out.append("\n## Propuestas para mejorar la herramienta\n")
        out += [f"\n- `{p.get('id')}` ({p.get('tipo')}, {p.get('revisor')}): {p.get('leccion')}  \n  Se escapó: {p.get('defecto')}  \n  Prueba: {p.get('prueba')}\n"
                for p in prop.get("propuestas", [])]
    ruta = d / "revisores" / "informe_revisores.md"; ruta.write_text("".join(out))
    print(f"{len(todos)} hallazgos de {len(hs)} revisores -> {ruta}")


# ------------------------------------------------------------------ lecciones
def correccion(d, texto):
    cp = d / "revisores" / "correcciones_profesor.json"; cp.parent.mkdir(exist_ok=True)
    corr = json.loads(cp.read_text()) if cp.exists() else []
    corr.append({"fecha": f"{datetime.datetime.now():%Y-%m-%d %H:%M}", "texto": texto.strip()})
    cp.write_text(json.dumps(corr, ensure_ascii=False, indent=1))
    print(f"Corrección {len(corr)} registrada en {cp}")


def registrar(d, ids):
    doc = json.loads((d / "revisores" / "herramienta" / "hallazgos.json").read_text())
    props = {p["id"]: p for p in doc.get("propuestas", [])}
    desconocidas = [i for i in ids if i not in props]
    if desconocidas:
        print(f"No existen las propuestas {desconocidas}; hay {sorted(props)}"); sys.exit(2)
    ls = lecciones(solo_vigentes=False)
    for i in ids:
        p = props[i]
        ls.append({"id": f"L-{len(ls) + 1:03d}", "fecha": f"{datetime.date.today()}", "video": ejercicio_de(d).name,
                   "revisor": p["revisor"], "tipo": p["tipo"], "leccion": p["leccion"], "defecto": p["defecto"],
                   "comprobacion": p.get("comprobacion", ""), "prueba": p["prueba"],
                   "estado": "vigente" if p["tipo"] == "instruccion" else "pendiente de programar"})
        print(f"{ls[-1]['id']} registrada ({ls[-1]['estado']}): {p['leccion']}")
    LECCIONES.write_text(json.dumps(ls, ensure_ascii=False, indent=1) + "\n")


def main():
    a = sys.argv[1:]
    if len(a) == 1 and a[0] == "lecciones":
        for l in lecciones(solo_vigentes=False):
            print(f"{l['id']} [{l['revisor']}, {l['tipo']}, {l['estado']}] {l['video']} {l['fecha']}: {l['leccion']}")
        return
    if len(a) < 2:
        print(__doc__); sys.exit(2)
    orden, d = a[0], pathlib.Path(a[1]).resolve()
    if not (d / "guion.py").exists():
        print(f"{d} no contiene guion.py"); sys.exit(2)
    fmt = a[a.index("--formato") + 1] if "--formato" in a else "h"
    if orden == "preparar" and len(a) >= 3:
        {"guion": lambda: preparar_guion(d), "oido": lambda: preparar_oido(d, fmt), "pantalla": lambda: preparar_pantalla(d, fmt),
         "herramienta": lambda: preparar_herramienta(d)}.get(a[2], lambda: (print(__doc__), sys.exit(2)))()
    elif orden == "comprobar" and len(a) >= 3:
        comprobar(d, a[2])
    elif orden == "resumen":
        resumen(d)
    elif orden == "correccion" and len(a) >= 3:
        correccion(d, a[2])
    elif orden == "registrar" and len(a) >= 3:
        registrar(d, a[2:])
    else:
        print(__doc__); sys.exit(2)


if __name__ == "__main__":
    main()
