"""Dónde está el video en curso y cómo se leen su guion y sus archivos de audio.

La carpeta del video llega por la variable VIDEO_DIR y el formato por VIDEO_FORMATO
(«h» = horizontal 1920×1080, «v» = vertical 1080×1920). Las pone cadena.py."""
import hashlib, importlib.util, json, os, pathlib

FORMATOS = {"h": (1920, 1080), "v": (1080, 1920)}


def carpeta():
    return pathlib.Path(os.environ.get("VIDEO_DIR", ".")).resolve()


def formato():
    f = os.environ.get("VIDEO_FORMATO", "h")
    assert f in FORMATOS, f"VIDEO_FORMATO debe ser h o v, no {f!r}"
    return f


def guion(d=None):
    ruta = (d or carpeta()) / "guion.py"
    assert ruta.exists(), f"No existe {ruta}: copie la plantilla (ver GUIA.md)"
    spec = importlib.util.spec_from_file_location("guion_del_video", ruta)
    m = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(m)
    return m


def norm(x):
    """Una palabra como la compara la sincronía: en minúsculas y sin signos."""
    return x.lower().strip("¡!¿?.,:;«»\"'()")


def con_signos(seg, pal):
    """Copia de las palabras con los signos del guion (¿? ¡! , . : ;) para los subtítulos.
    La voz entrega las palabras sin puntuación; aquí se toma cada una tal como está escrita en SEG.
    Si en una escena el guion y la voz no casan palabra por palabra, se detiene: no adivina."""
    out = {}
    for k, ws in pal.items():
        toks = [t for f in seg[k] for t in f[0].split()]
        mal = len(toks) != len(ws) or [(t, w[0]) for t, w in zip(toks, ws) if norm(t) != norm(w[0])]
        assert not mal, f"SIGNOS: en la escena {k} el guion y la voz no casan palabra por palabra: {mal if mal is not True else (len(toks), len(ws))}"
        out[k] = [[t] + list(w[1:]) for t, w in zip(toks, ws)]
    return out


def leer(nombre, d=None):
    return json.loads(((d or carpeta()) / "audio" / nombre).read_text())


def escribir(nombre, dato, d=None):
    p = (d or carpeta()) / "audio" / nombre
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_text(json.dumps(dato, ensure_ascii=False, indent=1))


def veces(pal, k, palabra):
    return sum(1 for w in pal[k] if norm(w[0]) == norm(palabra))


def t_palabra(pal, k, palabra, n=1):
    """Segundo (dentro de la escena k) en que la voz dice «palabra» por n-ésima vez, o None."""
    h = [w for w in pal[k] if norm(w[0]) == norm(palabra)]
    return h[n - 1][1] if len(h) >= n else None


def md5(ruta):
    return hashlib.md5(pathlib.Path(ruta).read_bytes()).hexdigest()
