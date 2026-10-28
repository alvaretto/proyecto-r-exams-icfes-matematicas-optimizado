"""Tabla de doble entrada de fichas (pregunta 32, SAI «Nivelación 6», grado 10, Sesión 2).

Reproduce con matplotlib la tabla impresa del ítem: 6 filas x 4 columnas, «Figura geométrica»
combinada sobre las dos últimas columnas, «Blanca» y «Negra» combinadas verticalmente, bordes y
encabezados en magenta oscuro sobre rosado claro. Es una función pura: mismos argumentos, mismo PNG.

Unidad interna `u` = 1/100 de pulgada = 1 px del escaneo original (530 x 244). Con dpi = 300 cada
`u` son 3 px, y el lienzo canónico mide 1590 x 732 px.

Uso desde R (reticulate):
    reticulate::source_python("output_python_v1.py")
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
_X = (10.08, 138.33, 265.35, 392.63, 519.37)          # ejes verticales: 4 columnas
_Y = (16.87, 52.30, 87.26, 123.37, 158.63, 194.15, 229.14)  # ejes horizontales: 6 filas

# Colores (RGB 0-255): relleno de encabezados, tinta de bordes, tinta de encabezados y tinta negra
_RELLENO = (249, 233, 248)
_TINTA_LINEA = (96, 22, 64)
_TINTA_ENC = (122, 8, 48)
_TINTA_NEGRA = (8, 8, 8)

# Grosor de cada línea (u): (eje, posición, desde, hasta, grosor). Índices sobre _X / _Y.
_GROSOR = {
    "H0": 3.10, "H1": 2.89, "H2": 2.12, "H3": 2.04, "H4": 2.04, "H5": 2.20, "H6": 3.23,
    "V0": 2.51, "V1": 1.55, "V2": 1.50, "V2t": 2.89, "V3": 1.31, "V4": 2.74,
}

# Texto por categoría: peso de Source Sans 3, cuerpo (em, en u), compresión horizontal (sx), grosor
# añadido a cada lado del trazo (d, en u) y holgura entre dígitos (gap, en u).
_ESTILO = {
    "enc": dict(peso=700, em=24.52, sx=0.811, d=0.606, tinta=_TINTA_ENC),
    "col1": dict(peso=700, em=24.71, sx=0.829, d=0.511, tinta=_TINTA_NEGRA),
    "cat": dict(peso=600, em=25.00, sx=0.731, d=0.418, tinta=_TINTA_NEGRA),
    "num": dict(peso=400, em=24.86, sx=1.00, d=0.517, tinta=_TINTA_NEGRA, gap=0.0),
}
# Ancho de tinta (u) de cada dígito en el impreso: 0, 1, 2, 5, 6 y 8 medidos sobre las tablas del escaneo
# (los números de dos cifras se tocan); 3, 4, 7 y 9 estimados por semejanza de forma.
_K_ANCHO_DIGITO = 0.991   # factor global sobre _ANCHO_DIGITO (calibración)
_ANCHO_DIGITO = {"0": 12.0, "1": 7.0, "2": 9.0, "3": 9.5, "4": 11.0, "5": 9.0, "6": 12.0, "7": 9.0, "8": 11.0,
                 "9": 12.0, ",": 4.0, ".": 4.0, "-": 7.0}
# Posición del texto por categoría: dx = desplazamiento horizontal respecto del centro de la celda; off = distancia
# de la línea base al centro vertical de la celda (positiva hacia abajo). "tit" es «Figura geométrica».
_POS = {
    "tit": dict(dx=1.00, off=10.04), "enc": dict(dx=0.02, off=10.38), "col1": dict(dx=0.38, off=11.08),
    "cat": dict(dx=1.01, off=10.82), "num": dict(dx=-0.05, off=9.35),
}
_HOLGURA_MIN = 8.0                                                      # margen lateral mínimo del texto en su celda


# ---------------------------------------------------------------------------------------------
# Fuentes
# ---------------------------------------------------------------------------------------------
_RUTAS_SS3 = ("/usr/share/fonts/adobe-source-sans", "/usr/share/fonts/opentype/source-sans",
              "/usr/share/fonts/OTF", "/usr/share/fonts/truetype/source-sans",
              os.path.expanduser("~/.fonts"), os.path.expanduser("~/.local/share/fonts"))
_NOMBRE_PESO = {400: "Regular", 600: "Semibold", 700: "Bold", 900: "Black"}
_CACHE_FUENTES = {}


def _fuente(peso):
    """FontProperties de Source Sans 3 con ese peso; si no está instalada, DejaVu Sans (viene con matplotlib)."""
    if peso in _CACHE_FUENTES:
        return _CACHE_FUENTES[peso]
    fp = None
    for carpeta in _RUTAS_SS3:
        ruta = os.path.join(carpeta, "SourceSans3-%s.otf" % _NOMBRE_PESO[peso])
        if os.path.exists(ruta):
            fp = _fm.FontProperties(fname=ruta)
            break
    if fp is None:
        for familia in ("Source Sans 3", "Source Sans Pro"):
            try:
                ruta = _fm.findfont(_fm.FontProperties(family=familia, weight=peso), fallback_to_default=False)
                fp = _fm.FontProperties(fname=ruta)
                break
            except ValueError:
                pass
    if fp is None:
        fp = _fm.FontProperties(family="DejaVu Sans", weight=700 if peso >= 600 else 400)
    _CACHE_FUENTES[peso] = fp
    return fp


# ---------------------------------------------------------------------------------------------
# Texto como trazados (permite comprimirlo y medirlo con exactitud)
# ---------------------------------------------------------------------------------------------
def _trazado(txt, est, es_numero=False):
    """Trazado del texto (en u, origen en la línea base) y su caja de tinta (x0, x1) ya comprimida."""
    fp = _fuente(est["peso"])
    if es_numero:
        verts, codes, x = [], [], 0.0
        for ch in txt:
            tp = TextPath((0, 0), ch, size=est["em"], prop=fp)
            if len(tp.vertices) == 0:
                x += 0.3 * est["em"]
                continue
            bb = tp.get_extents()
            esc = _K_ANCHO_DIGITO * _ANCHO_DIGITO.get(ch, 0.8 * (bb.x1 - bb.x0)) / (bb.x1 - bb.x0)
            v = tp.vertices.copy()
            v[:, 0] = (v[:, 0] - bb.x0) * esc + x
            verts.append(v)
            codes.append(tp.codes)
            x += (bb.x1 - bb.x0) * esc + est["gap"]
        trazo = Path(np.concatenate(verts), np.concatenate(codes))
        bb = trazo.get_extents()
        return trazo, (bb.x0, bb.x1)
    else:
        trazo = TextPath((0, 0), txt, size=est["em"], prop=fp)
    bb = trazo.get_extents()
    return trazo, (bb.x0 * est["sx"], bb.x1 * est["sx"])


def _ancho_tinta(txt, est, es_numero=False):
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
    """Dibuja la tabla y la guarda como PNG RGB en `ruta`.

    valores: matriz 4 x 2 (filas: color1-cat1, color1-cat2, color2-cat1, color2-cat2; columnas: figura1, figura2).
    titulo_columnas: texto combinado sobre las dos columnas de figuras (hasta 25 caracteres).
    encab_col1, encab_col2: encabezados de las dos primeras columnas (hasta 13 caracteres).
    figuras, colores, categorias: listas de 2 textos (hasta 13 caracteres cada uno).
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
    ancho_txt = [
        max(_ancho_tinta(enc1, _ESTILO["enc"]), max(_ancho_tinta(t, _ESTILO["col1"]) for t in colores)),
        max(_ancho_tinta(enc2, _ESTILO["enc"]), max(_ancho_tinta(t, _ESTILO["cat"]) for t in categorias)),
        max(_ancho_tinta(figuras[0], _ESTILO["enc"]), max(_ancho_tinta(_fmt_num(v), _ESTILO["num"], True) for v in val[:, 0])),
        max(_ancho_tinta(figuras[1], _ESTILO["enc"]), max(_ancho_tinta(_fmt_num(v), _ESTILO["num"], True) for v in val[:, 1])),
    ]
    necesario = max(max(a + 2 * _HOLGURA_MIN for a in ancho_txt),
                    (_ancho_tinta(titulo, _ESTILO["enc"]) + 2 * _HOLGURA_MIN) / 2.0)
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

    # --- rellenos de encabezados ---
    for (x0, x1, y0, y1) in ((X[2], X[4], Y[0], Y[1]), (X[0], X[4], Y[1], Y[2])):
        ax.add_patch(Rectangle((x0, yd(y1)), x1 - x0, y1 - y0, facecolor=_rgb(_RELLENO), edgecolor="none",
                               linewidth=0, antialiased=True))

    # --- líneas (rectángulos con grosor exacto) ---
    def horizontal(y, x0, x1, g):
        ax.add_patch(Rectangle((x0 - 0.0, yd(y) - g / 2), x1 - x0, g, facecolor=_rgb(_TINTA_LINEA),
                               edgecolor="none", linewidth=0))

    def vertical(x, y0, y1, g):
        ax.add_patch(Rectangle((x - g / 2, yd(y1)), g, y1 - y0, facecolor=_rgb(_TINTA_LINEA),
                               edgecolor="none", linewidth=0))

    g = _GROSOR
    # verticales primero extendidas media línea para cerrar las esquinas
    vertical(X[0], Y[1] - g["H1"] / 2, Y[6] + g["H6"] / 2, g["V0"])
    vertical(X[1], Y[1], Y[6], g["V1"])
    vertical(X[2], Y[0] - g["H0"] / 2, Y[1], g["V2t"])
    vertical(X[2], Y[1], Y[6], g["V2"])
    vertical(X[3], Y[1], Y[6], g["V3"])
    vertical(X[4], Y[0] - g["H0"] / 2, Y[6] + g["H6"] / 2, g["V4"])
    horizontal(Y[0], X[2] - g["V2t"] / 2, X[4] + g["V4"] / 2, g["H0"])
    horizontal(Y[1], X[0] - g["V0"] / 2, X[4] + g["V4"] / 2, g["H1"])
    horizontal(Y[2], X[0], X[4], g["H2"])
    horizontal(Y[3], X[1], X[4], g["H3"])
    horizontal(Y[4], X[0], X[4], g["H4"])
    horizontal(Y[5], X[1], X[4], g["H5"])
    horizontal(Y[6], X[0] - g["V0"] / 2, X[4] + g["V4"] / 2, g["H6"])

    # --- texto ---
    def poner(txt, clave, estilo, cx, cy, es_numero=False):
        est, pos = _ESTILO[estilo], _POS[clave]
        trazo, (x0, x1) = _trazado(txt, est, es_numero)
        tr = Affine2D().scale(est["sx"], 1.0).translate(cx + pos["dx"] - (x0 + x1) / 2.0, yd(cy + pos["off"])) + ax.transData
        lw = 2 * est["d"] * 0.72  # u -> puntos (1 u = 0.72 pt)
        ax.add_patch(PathPatch(trazo, transform=tr, facecolor=_rgb(est["tinta"]), edgecolor=_rgb(est["tinta"]),
                               linewidth=lw, joinstyle="round", capstyle="round"))

    cx_col = [(X[i] + X[i + 1]) / 2.0 for i in range(4)]
    cy_fila = [(Y[i] + Y[i + 1]) / 2.0 for i in range(6)]
    poner(titulo, "tit", "enc", (X[2] + X[4]) / 2.0, cy_fila[0])
    for i, t in enumerate((enc1, enc2, figuras[0], figuras[1])):
        poner(t, "enc", "enc", cx_col[i], cy_fila[1])
    for i, t in enumerate(colores):
        poner(t, "col1", "col1", cx_col[0], (Y[2 + 2 * i] + Y[4 + 2 * i]) / 2.0)
    for fila in range(4):
        poner(categorias[fila % 2], "cat", "cat", cx_col[1], cy_fila[2 + fila])
        for j in range(2):
            poner(_fmt_num(val[fila, j]), "num", "num", cx_col[2 + j], cy_fila[2 + fila], True)

    fig.canvas.draw()
    rgb = np.ascontiguousarray(np.asarray(fig.canvas.buffer_rgba())[:, :, :3])
    _guardar_png_rgb(ruta, rgb, dpi)
    fig.clear()
    return ruta
