"""Tabla de doble entrada de fichas (pregunta 32, SAI «Nivelación 6», grado 10, Sesión 2).

Reproduce con matplotlib la tabla impresa del ítem: 6 filas x 4 columnas, «Figura geométrica»
combinada sobre las dos últimas columnas, «Blanca» y «Negra» combinadas verticalmente, bordes y
encabezados en magenta oscuro sobre rosado claro. Es una función pura: mismos argumentos, mismo PNG.

Unidad interna `u` = 1/100 de pulgada = 1 px del escaneo original (530 x 244). Con dpi = 300 cada
`u` son 3 px, y el lienzo canónico mide 1590 x 732 px.

Uso desde R (reticulate):
    reticulate::source_python("output_python_v5.py")
    dibujar_tabla_fichas(matrix(c(12,8,16,10,11,15,20,8), ncol = 2), "Figura geométrica", "Color",
                         "Categoría", c("Círculo","Cuadrado"), c("Blanca","Negra"),
                         c("Par","Impar"), "tabla.png")
"""
import os
import struct
import zlib

import numpy as np
from matplotlib import font_manager as _fm
from matplotlib.backends.backend_agg import FigureCanvasAgg
from matplotlib.figure import Figure
from matplotlib.patches import PathPatch, Rectangle
from matplotlib.path import Path
from matplotlib.textpath import TextPath
from matplotlib.transforms import Affine2D

# ---------------------------------------------------------------------------------------------
# Rejilla canónica medida sobre el escaneo (coordenadas de borde, en u; y crece hacia abajo)
# ---------------------------------------------------------------------------------------------
_LIENZO = (530.0, 244.0)
_X = (10.08, 138.33, 265.35, 392.63, 519.37)                 # ejes verticales: 4 columnas
_Y = (16.87, 52.30, 87.26, 123.37, 158.63, 194.15, 229.14)   # ejes horizontales: 6 filas

# Colores (RGB 0-255): relleno de encabezados, tinta de bordes, de encabezados y negra
_RELLENO = (249, 233, 248)
_TINTA_LINEA = (88, 24, 62)        # bordes exteriores y líneas gruesas
_TINTA_LINEA_FINA = (104, 56, 86)  # líneas interiores finas: en el impreso se ven más claras que el marco
_TINTA_ENC = (122, 8, 48)
_TINTA_NEGRA = (12, 12, 12)
_TINTA = {"tit": _TINTA_ENC, "enc": _TINTA_ENC, "col1": _TINTA_NEGRA, "cat": _TINTA_NEGRA, "num": _TINTA_NEGRA}

# Grosor de cada línea (u): ancho equivalente de tinta medido en el escaneo (luminancia), corregido por el
# contraste de la tinta elegida (la tinta más clara pide más ancho para igualar la masa).
_GROSOR = {
    "H0": 3.50, "H1": 3.27, "H2": 2.65, "H3": 2.55, "H4": 2.55, "H5": 2.75, "H6": 3.65,
    "V0": 2.84, "V1": 1.94, "V2": 1.88, "V2t": 3.27, "V3": 1.64, "V4": 3.10,
}
_FINAS = ("H2", "H3", "H4", "H5", "V1", "V2", "V3")

# Texto por categoría y familia tipográfica (calibrado contra el escaneo, ver metricas_python.json):
# em = cuerpo (u), sx = compresión horizontal, d = grosor añadido a cada lado del trazo (u),
# dx / off = desplazamiento horizontal y distancia de la línea base al centro vertical de la celda (u).
# En «num», k multiplica el ancho de tinta de cada dígito (_ANCHO_DIGITO) y gap separa los dígitos.
_ESTILOS = {
    "ss3": {
        "tit": dict(em=24.53, sx=0.810, d=0.606, dx=1.065, off=10.041),
        "enc": dict(em=24.53, sx=0.810, d=0.606, dx=0.010, off=10.382),
        "col1": dict(em=24.58, sx=0.833, d=0.558, dx=0.320, off=11.010),
        "cat": dict(em=24.07, sx=0.782, d=0.705, dx=0.971, off=10.552),
        "num": dict(em=24.85, sx=1.0, d=0.528, dx=-0.045, off=9.340, k=0.990, gap=0.0),
    },
    "noto": {
        "num": dict(em=23.81, sx=1.0, d=0.178, dx=-0.573, off=10.559, k=1.086, gap=0.0),
    },
    "dejavu": {
        "tit": dict(em=22.54, sx=0.690, d=0.622, dx=0.184, off=10.144),
        "enc": dict(em=22.54, sx=0.690, d=0.622, dx=0.068, off=10.474),
        "col1": dict(em=22.65, sx=0.697, d=0.523, dx=0.453, off=11.235),
        "cat": dict(em=21.61, sx=0.750, d=0.692, dx=1.075, off=10.597),
        "num": dict(em=21.65, sx=1.0, d=0.456, dx=0.029, off=9.469, k=0.977, gap=0.0),
    },
}
_ROL = {"tit": "bold", "enc": "bold", "col1": "bold", "cat": "reg", "num": "num"}

# Ancho de tinta (u) de cada dígito en el impreso: 0, 1, 2, 5, 6 y 8 medidos sobre el escaneo
# (los números de dos cifras se tocan); 3, 4, 7 y 9 estimados por semejanza de forma.
_ANCHO_DIGITO = {"0": 12.0, "1": 7.0, "2": 9.0, "3": 9.5, "4": 11.0, "5": 9.0, "6": 12.0, "7": 9.0, "8": 11.0,
                 "9": 12.0, ",": 4.0, ".": 4.0, "-": 7.0}
_HOLGURA_MIN = 8.0   # margen lateral mínimo del texto dentro de su celda (u)


# ---------------------------------------------------------------------------------------------
# Fuentes: Source Sans 3 para las letras, Noto Sans Medium para los dígitos; DejaVu Sans (que viene
# con matplotlib) si faltan. Cada familia usa su propia calibración de _ESTILOS.
# ---------------------------------------------------------------------------------------------
_CARPETAS = ("/usr/share/fonts/adobe-source-sans", "/usr/share/fonts/opentype/source-sans", "/usr/share/fonts/OTF",
             "/usr/share/fonts/truetype/source-sans", "/usr/share/fonts/noto", "/usr/share/fonts/truetype/noto",
             "/usr/share/fonts/TTF", os.path.expanduser("~/.fonts"), os.path.expanduser("~/.local/share/fonts"))
_FUENTES = None


def _buscar(familia, peso):
    """(familia, FontProperties) o None si esa familia no está instalada."""
    archivos = {"ss3": {400: "SourceSans3-Regular.otf", 700: "SourceSans3-Bold.otf"},
                "noto": {500: "NotoSans-Medium.ttf"}}
    nombres = {"ss3": ("Source Sans 3", "Source Sans Pro"), "noto": ("Noto Sans",), "dejavu": ("DejaVu Sans",)}
    for carpeta in _CARPETAS if familia in archivos else ():
        ruta = os.path.join(carpeta, archivos[familia][peso])
        if os.path.exists(ruta):
            return familia, _fm.FontProperties(fname=ruta)
    try:
        ruta = _fm.findfont(_fm.FontProperties(family=list(nombres[familia]), weight=peso), fallback_to_default=False)
        return familia, _fm.FontProperties(fname=ruta)
    except ValueError:
        return None


def _fuentes():
    global _FUENTES
    if _FUENTES is None:
        bold = _buscar("ss3", 700) or _buscar("dejavu", 700)
        reg = _buscar("ss3", 400) or _buscar("dejavu", 400)
        num = _buscar("noto", 500) or reg
        _FUENTES = {"bold": bold, "reg": reg, "num": num}
    return _FUENTES


def _estilo(cat):
    familia, fp = _fuentes()[_ROL[cat]]
    est = dict(_ESTILOS[familia][cat])
    est.update(fp=fp, tinta=_TINTA[cat])
    return est


# ---------------------------------------------------------------------------------------------
# Texto como trazados (permite comprimirlo y medirlo con exactitud)
# ---------------------------------------------------------------------------------------------
def _trazado(txt, est, es_numero=False):
    """Trazado del texto (en u, origen en la línea base) y los extremos x0, x1 de su tinta ya comprimida."""
    if es_numero:
        verts, codes, x = [], [], 0.0
        for ch in txt:
            tp = TextPath((0, 0), ch, size=est["em"], prop=est["fp"])
            if len(tp.vertices) == 0:
                x += 0.3 * est["em"]
                continue
            bb = tp.get_extents()
            esc = est["k"] * _ANCHO_DIGITO.get(ch, 0.8 * (bb.x1 - bb.x0)) / (bb.x1 - bb.x0)
            v = tp.vertices.copy()
            v[:, 0] = (v[:, 0] - bb.x0) * esc + x
            verts.append(v)
            codes.append(tp.codes)
            x += (bb.x1 - bb.x0) * esc + est["gap"]
        trazo = Path(np.concatenate(verts), np.concatenate(codes))
        bb = trazo.get_extents()
        return trazo, (bb.x0, bb.x1)
    trazo = TextPath((0, 0), txt, size=est["em"], prop=est["fp"])
    bb = trazo.get_extents()
    return trazo, (bb.x0 * est["sx"], bb.x1 * est["sx"])


def _ancho_tinta(txt, cat, es_numero=False):
    est = _estilo(cat)
    _, (x0, x1) = _trazado(txt, est, es_numero)
    return (x1 - x0) + 2 * est["d"]


def _fmt_num(v):
    v = float(v)
    return str(int(round(v))) if abs(v - round(v)) < 1e-9 else ("%g" % round(v, 2)).replace(".", ",")


def _guardar_png_rgb(ruta, rgb, dpi):
    """PNG RGB de 8 bits sin canal alfa, con la densidad en el chunk pHYs (solo stdlib + numpy)."""
    h, w, _ = rgb.shape
    crudo = b"".join(b"\x00" + rgb[i].tobytes() for i in range(h))

    def chunk(tipo, datos):
        return struct.pack(">I", len(datos)) + tipo + datos + struct.pack(">I", zlib.crc32(tipo + datos) & 0xFFFFFFFF)

    ppm = int(round(dpi / 0.0254))
    with open(ruta, "wb") as f:
        f.write(b"\x89PNG\r\n\x1a\n" + chunk(b"IHDR", struct.pack(">IIBBBBB", w, h, 8, 2, 0, 0, 0))
                + chunk(b"pHYs", struct.pack(">IIB", ppm, ppm, 1)) + chunk(b"IDAT", zlib.compress(crudo, 9))
                + chunk(b"IEND", b""))


def _rgb(c):
    return tuple(v / 255.0 for v in c)


# ---------------------------------------------------------------------------------------------
# Función pública
# ---------------------------------------------------------------------------------------------
def dibujar_tabla_fichas(valores, titulo_columnas, encab_col1, encab_col2, figuras, colores, categorias,
                         ruta, dpi=300):
    """Dibuja la tabla y la guarda como PNG RGB en `ruta`; devuelve `ruta`.

    valores: matriz 4 x 2 (filas: color1-cat1, color1-cat2, color2-cat1, color2-cat2; columnas: figura1, figura2).
    titulo_columnas: texto combinado sobre las dos columnas de figuras (hasta 25 caracteres).
    encab_col1, encab_col2: encabezados de las dos primeras columnas (hasta 13 caracteres).
    figuras, colores, categorias: listas de 2 textos (hasta 13 caracteres cada uno).
    Si algún texto no cabe con holgura, las cuatro columnas se ensanchan por igual (el cuerpo no cambia).
    """
    val = np.asarray(valores, dtype=float)
    if val.shape != (4, 2):
        raise ValueError("`valores` debe ser una matriz 4 x 2; llegó con forma %s" % (val.shape,))
    figuras, colores, categorias = ([str(t) for t in lista] for lista in (figuras, colores, categorias))
    for nombre, lista in (("figuras", figuras), ("colores", colores), ("categorias", categorias)):
        if len(lista) != 2:
            raise ValueError("`%s` debe tener 2 elementos" % nombre)
    titulo, enc1, enc2 = str(titulo_columnas), str(encab_col1), str(encab_col2)

    # --- anchos de columna: los canónicos, o uniformes si algún texto no cabe con holgura ---
    canon = [_X[i + 1] - _X[i] for i in range(4)]
    nums = [[_fmt_num(v) for v in val[:, j]] for j in range(2)]
    ancho_txt = [
        max([_ancho_tinta(enc1, "enc")] + [_ancho_tinta(t, "col1") for t in colores]),
        max([_ancho_tinta(enc2, "enc")] + [_ancho_tinta(t, "cat") for t in categorias]),
        max([_ancho_tinta(figuras[0], "enc")] + [_ancho_tinta(t, "num", True) for t in nums[0]]),
        max([_ancho_tinta(figuras[1], "enc")] + [_ancho_tinta(t, "num", True) for t in nums[1]]),
    ]
    necesario = max(max(a + 2 * _HOLGURA_MIN for a in ancho_txt), (_ancho_tinta(titulo, "tit") + 2 * _HOLGURA_MIN) / 2.0)
    anchos = canon if necesario <= max(canon) else [necesario] * 4
    X = [_X[0]]
    for a in anchos:
        X.append(X[-1] + a)
    Y = list(_Y)

    ancho_u = max(_LIENZO[0], X[4] + (_LIENZO[0] - _X[4]))
    alto_u = _LIENZO[1]
    px_w, px_h = int(np.ceil(ancho_u * dpi / 100.0 - 1e-9)), int(np.ceil(alto_u * dpi / 100.0 - 1e-9))
    ancho_u, alto_u = px_w * 100.0 / dpi, px_h * 100.0 / dpi

    fig = Figure(figsize=(px_w / dpi, px_h / dpi), dpi=dpi, facecolor="white")
    FigureCanvasAgg(fig)
    ax = fig.add_axes([0, 0, 1, 1])
    ax.set_xlim(0, ancho_u)
    ax.set_ylim(0, alto_u)
    ax.axis("off")

    def yd(y):  # coordenada de imagen (hacia abajo) -> coordenada de datos (hacia arriba)
        return alto_u - y

    def rect(x0, y_arriba, ancho, alto, color):
        ax.add_patch(Rectangle((x0, yd(y_arriba + alto)), ancho, alto, facecolor=_rgb(color), edgecolor="none",
                               linewidth=0, antialiased=True, snap=False))

    # --- rellenos de encabezados ---
    rect(X[2], Y[0], X[4] - X[2], Y[1] - Y[0], _RELLENO)
    rect(X[0], Y[1], X[4] - X[0], Y[2] - Y[1], _RELLENO)

    # --- líneas (rectángulos con grosor exacto, sin ajuste a píxeles) ---
    g = _GROSOR

    def horizontal(y, x0, x1, nombre):
        rect(x0, y - g[nombre] / 2, x1 - x0, g[nombre], _TINTA_LINEA_FINA if nombre in _FINAS else _TINTA_LINEA)

    def vertical(x, y0, y1, nombre):
        rect(x - g[nombre] / 2, y0, g[nombre], y1 - y0, _TINTA_LINEA_FINA if nombre in _FINAS else _TINTA_LINEA)

    vertical(X[0], Y[1] - g["H1"] / 2, Y[6] + g["H6"] / 2, "V0")
    vertical(X[1], Y[1], Y[6], "V1")
    vertical(X[2], Y[0] - g["H0"] / 2, Y[1], "V2t")
    vertical(X[2], Y[1], Y[6], "V2")
    vertical(X[3], Y[1], Y[6], "V3")
    vertical(X[4], Y[0] - g["H0"] / 2, Y[6] + g["H6"] / 2, "V4")
    horizontal(Y[0], X[2] - g["V2t"] / 2, X[4] + g["V4"] / 2, "H0")
    horizontal(Y[1], X[0] - g["V0"] / 2, X[4] + g["V4"] / 2, "H1")
    horizontal(Y[2], X[0], X[4], "H2")
    horizontal(Y[3], X[1], X[4], "H3")
    horizontal(Y[4], X[0], X[4], "H4")
    horizontal(Y[5], X[1], X[4], "H5")
    horizontal(Y[6], X[0] - g["V0"] / 2, X[4] + g["V4"] / 2, "H6")

    # --- texto ---
    def poner(txt, cat, cx, cy, es_numero=False):
        est = _estilo(cat)
        trazo, (x0, x1) = _trazado(txt, est, es_numero)
        tr = Affine2D().scale(est["sx"], 1.0).translate(cx + est["dx"] - (x0 + x1) / 2.0, yd(cy + est["off"])) + ax.transData
        lw = 2 * est["d"] * 0.72  # u -> puntos (1 u = 0.72 pt)
        ax.add_patch(PathPatch(trazo, transform=tr, facecolor=_rgb(est["tinta"]), edgecolor=_rgb(est["tinta"]),
                               linewidth=lw, joinstyle="round", capstyle="round"))

    cx_col = [(X[i] + X[i + 1]) / 2.0 for i in range(4)]
    cy_fila = [(Y[i] + Y[i + 1]) / 2.0 for i in range(6)]
    poner(titulo, "tit", (X[2] + X[4]) / 2.0, cy_fila[0])
    for i, t in enumerate((enc1, enc2, figuras[0], figuras[1])):
        poner(t, "enc", cx_col[i], cy_fila[1])
    for i, t in enumerate(colores):
        poner(t, "col1", cx_col[0], (Y[2 + 2 * i] + Y[4 + 2 * i]) / 2.0)
    for fila in range(4):
        poner(categorias[fila % 2], "cat", cx_col[1], cy_fila[2 + fila])
        for j in range(2):
            poner(nums[j][fila], "num", cx_col[2 + j], cy_fila[2 + fila], True)

    fig.canvas.draw()
    rgb = np.ascontiguousarray(np.asarray(fig.canvas.buffer_rgba())[:, :, :3])
    _guardar_png_rgb(ruta, rgb, dpi)
    fig.clear()
    return ruta
