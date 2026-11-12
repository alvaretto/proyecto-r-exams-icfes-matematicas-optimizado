"""Piezas de estadística: barras, línea de referencia, tabla y gráfico de líneas.

Todas se construyen al tamaño final (`ancho` y `alto` en unidades de pantalla) y con el texto a tamaño fijo
(>= 32): no se encogen para caber, así que pida un tamaño que quepa en la zona (si la escena las encoge con
`ubicar`, el texto baja de 32). Cada función devuelve una `Pieza` (un VGroup con el diccionario `partes`).
No use `.copy()` sobre una pieza: `partes` seguiría apuntando a las partes originales."""
import math

import numpy as np
from manim import *

from ..base import AMARILLO, CIAN, FONDO, FONT, GRIS, GRIS_CLARO, NARANJA, TAM_MIN, num


class Pieza(VGroup):
    """VGroup con `partes`: diccionario nombre -> submobject (para resaltar o animar una parte)."""

    def __init__(self, *mobs, partes=None, **kw):
        super().__init__(*mobs, **kw)
        self.partes = partes if partes is not None else {}


# ---------- ayudas internas (las usan también geometria.py y probabilidad.py) ----------
def _t(s, size=34, color=GRIS_CLARO):
    """Texto a tamaño fijo, nunca menor que TAM_MIN."""
    return Text(str(s), font=FONT, font_size=max(size, TAM_MIN), color=color)


def _n(x):
    """Texto de un dato: las cadenas tal cual, los números con coma decimal y espacio de miles."""
    if isinstance(x, str):
        return x
    if abs(x - round(x)) < 1e-9:
        return num(x, tex=False)
    return f"{x:,.2f}".rstrip("0").replace(",", " ").replace(".", ",")   # con espacio de miles también en decimales


def _vacio():
    return Rectangle(width=0.01, height=0.01, stroke_width=0, fill_opacity=0)


def _ajustar(texto, ancho_max, size=34, color=GRIS_CLARO):
    """Texto que se parte en líneas (por palabras) si pasa de `ancho_max`; una palabra sola nunca se parte."""
    if texto == "":
        return _vacio()
    t = _t(texto, size, color)
    if t.width <= ancho_max or " " not in texto or not any(ch.isalpha() for ch in texto):
        return t        # los números («1 850,5») no se parten
    lineas, actual = [], ""
    for p in texto.split(" "):
        cand = (actual + " " + p).strip()
        if actual and _t(cand, size).width > ancho_max:
            lineas.append(actual)
            actual = p
        else:
            actual = cand
    lineas.append(actual)
    return VGroup(*[_t(l, size, color) for l in lineas]).arrange(DOWN, buff=0.06)


def _paso_bonito(vmax, marcas=5):
    if vmax <= 0:
        return 1
    bruto = vmax / max(marcas, 1)
    e = 10 ** math.floor(math.log10(bruto))
    for m in (1, 2, 5, 10):
        if bruto <= m * e + 1e-12:
            return m * e
    return 10 * e


def _marcas_y(vmin, vmax, paso, plot_w, plot_h):
    """Rejilla, marcas y etiquetas del eje vertical; el origen local es la esquina inferior izquierda del área."""
    rejilla, marcas, etiquetas = VGroup(), VGroup(), VGroup()
    n = int(round((vmax - vmin) / paso))
    k = max(1, math.ceil(0.5 / (plot_h / max(n, 1))))      # una etiqueta cada k marcas si no caben
    for i in range(n + 1):
        v = vmin + i * paso
        y = (v - vmin) / (vmax - vmin) * plot_h
        rejilla.add(Line([0, y, 0], [plot_w, y, 0], color=GRIS, stroke_width=1.5, stroke_opacity=0.35))
        marcas.add(Line([-0.1, y, 0], [0, y, 0], color=GRIS_CLARO, stroke_width=3))
        e = _t(_n(round(v, 10)))
        e.move_to([-0.16 - e.width / 2, y, 0])
        if i % k == 0:
            etiquetas.add(e)
    return rejilla, marcas, etiquetas


def _rejilla(matriz, ancho, fs=34, relleno=0.2, estilo=None):
    """Tabla de celdas rectangulares. `estilo(r, c)` -> dict(color=texto, fondo=(color, opacidad)).
    Devuelve (grupo, celdas) con celdas[r][c] = VGroup(rectángulo, contenido)."""
    estilo = estilo or (lambda r, c: {})
    nf, nc = len(matriz), len(matriz[0])
    txt = [[_n(c) for c in fila] for fila in matriz]
    nat = [[_t(s, fs).width if s else 0 for s in fila] for fila in txt]
    ancho_col = [max(nat[i][j] for i in range(nf)) + 2 * relleno for j in range(nc)]
    if sum(ancho_col) > ancho:
        def _ancho_min(s):
            if not any(ch.isalpha() for ch in s):
                return _t(s, fs).width if s else 0
            return max((_t(p, fs).width for p in s.split(" ") if p), default=0)
        minimo = [max(_ancho_min(txt[i][j]) for i in range(nf)) + 2 * relleno for j in range(nc)]
        f = ancho / sum(ancho_col)
        ancho_col = [max(minimo[j], ancho_col[j] * f) for j in range(nc)]
    cont = [[_ajustar(txt[i][j], ancho_col[j] - 2 * relleno, fs, estilo(i, j).get("color", WHITE))
             for j in range(nc)] for i in range(nf)]
    alto_fila = [max(0.75, max(cont[i][j].height for j in range(nc)) + 0.3) for i in range(nf)]
    celdas = [[None] * nc for _ in range(nf)]
    y = 0.0
    for i in range(nf):
        x = 0.0
        for j in range(nc):
            caja = Rectangle(width=ancho_col[j], height=alto_fila[i], stroke_color=GRIS, stroke_width=2)
            fondo = estilo(i, j).get("fondo")
            if fondo:
                caja.set_fill(fondo[0], fondo[1])
            caja.move_to([x + ancho_col[j] / 2, -(y + alto_fila[i] / 2), 0])
            cont[i][j].move_to(caja)
            celdas[i][j] = VGroup(caja, cont[i][j])
            x += ancho_col[j]
        y += alto_fila[i]
    g = VGroup(*[c for fila in celdas for c in fila])
    g.move_to(ORIGIN)
    return g, celdas


# ---------- barras ----------
def barras(categorias, valores, ancho=6.5, alto=4.5, color=CIAN, paso_eje=None, titulo_eje=None,
           mostrar_valores=True, vmax=None):
    """Diagrama de barras con ejes, rejilla, marcas y valores. `partes[categoria]` = barra;
    `partes[("valor", cat)]`, `partes[("etiqueta", cat)]`, y "eje_x", "eje_y", "rejilla", "marcas_y".
    `grafica.y_de(v)` da la coordenada vertical (en pantalla) de un valor. `vmax` fija el tope del eje."""
    n = len(categorias)
    assert n == len(valores) and n > 0
    vm = max(valores)
    paso = paso_eje or _paso_bonito(vm)
    tope = vmax if vmax is not None else math.ceil(vm / paso - 1e-9) * paso
    tope = tope if tope > 0 else paso
    wy = _t(_n(tope)).width
    mi = wy + 0.3
    plot_w = ancho - mi - 0.1
    slot = plot_w / n
    et = [_ajustar(str(c), slot - 0.08, 34, GRIS_CLARO) for c in categorias]
    girar = any(e.width > slot - 0.04 for e in et)
    if girar:   # una palabra no cabe: se inclinan 40 grados, con el extremo derecho en la marca
        et = [_t(str(c)).rotate(math.radians(40)) for c in categorias]
    mb = max(e.height for e in et) + 0.25
    mt = (0.6 if mostrar_valores else 0.2) + (0.5 if titulo_eje else 0)
    plot_h = alto - mt - mb
    if paso_eje is None:
        paso = _paso_bonito(vm, min(5, max(2, int(plot_h // 0.55))))
        tope = vmax if vmax is not None else math.ceil(vm / paso - 1e-9) * paso
        tope = tope if tope > 0 else paso
    rejilla, marcas, etq_y = _marcas_y(0, tope, paso, plot_w, plot_h)
    eje_y = Line([0, 0, 0], [0, plot_h, 0], color=GRIS_CLARO, stroke_width=3)
    eje_x = Line([0, 0, 0], [plot_w, 0, 0], color=GRIS_CLARO, stroke_width=3)
    partes = {"eje_x": eje_x, "eje_y": eje_y, "rejilla": rejilla, "marcas_y": marcas, "etiquetas_y": etq_y}
    todo = [rejilla, marcas, etq_y]
    for i, (c, v) in enumerate(zip(categorias, valores)):
        h = max(v / tope * plot_h, 0.001)
        bw = min(slot * 0.62, 1.6)
        b = Rectangle(width=bw, height=h, fill_color=color, fill_opacity=0.95, stroke_width=0)
        b.move_to([(i + 0.5) * slot, h / 2, 0])
        if girar:
            et[i].move_to([(i + 0.5) * slot + 0.12 - et[i].width / 2, -0.12 - et[i].height / 2, 0])
        else:
            et[i].move_to([(i + 0.5) * slot, -0.12 - et[i].height / 2, 0])
        partes[c], partes[("etiqueta", c)] = b, et[i]
        todo += [b, et[i]]
        if mostrar_valores:
            lv = _t(_n(v), 34, WHITE)
            lv.move_to([(i + 0.5) * slot, h + 0.1 + lv.height / 2, 0])
            partes[("valor", c)] = lv
            todo.append(lv)
    todo += [eje_x, eje_y]
    if titulo_eje:
        ti = _ajustar(titulo_eje, ancho - 0.2, 32, GRIS_CLARO)
        ti.move_to([-mi + 0.1 + ti.width / 2, plot_h + mt - 0.25 - (0 if mostrar_valores else 0.1), 0])
        partes["titulo_eje"] = ti
        todo.append(ti)
    g = Pieza(*todo, partes=partes)
    g.move_to(ORIGIN)
    g.vmin, g.vmax = 0, tope

    def y_de(v):
        ey = g.partes["eje_y"]
        return ey.get_bottom()[1] + (v - g.vmin) / (g.vmax - g.vmin) * ey.height
    g.y_de = y_de
    return g


def linea_referencia(grafica, valor, texto, color=NARANJA):
    """Línea punteada horizontal a la altura de `valor` (p. ej. el promedio), con su rótulo al extremo derecho.
    Va en un grupo aparte: añádalo a la escena después de la gráfica."""
    y = grafica.y_de(valor)
    ex = grafica.partes["eje_x"]
    x0, x1 = ex.get_left()[0], ex.get_right()[0]
    linea = DashedLine([x0, y, 0], [x1, y, 0], color=color, stroke_width=4, dash_length=0.15)
    et = _t(texto, 34, color)
    # rótulo en el primer hueco libre (de derecha a izquierda; arriba de la línea y, si no, debajo)
    obst = [m for k, m in grafica.partes.items()
            if k not in ("eje_x", "eje_y", "rejilla", "marcas_y", "etiquetas_y", "titulo_eje", "linea")
            and not (isinstance(k, tuple) and k[0] == "etiqueta")]
    elegido = None
    for arriba in (True, False):
        yc = y + (0.1 + et.height / 2 if arriba else -0.1 - et.height / 2)
        xc = x1 - et.width / 2 - 0.05
        while xc - et.width / 2 >= x0 and elegido is None:
            caja = (xc - et.width / 2 - 0.08, xc + et.width / 2 + 0.08, yc - et.height / 2 - 0.05, yc + et.height / 2 + 0.05)
            if all(m.get_right()[0] < caja[0] or m.get_left()[0] > caja[1] or m.get_top()[1] < caja[2]
                   or m.get_bottom()[1] > caja[3] for m in obst):
                elegido = (xc, yc)
            xc -= 0.1
        if elegido:
            break
    if elegido is None:
        elegido = (x1 - et.width / 2 - 0.05, y + 0.1 + et.height / 2)
    et.move_to([elegido[0], elegido[1], 0])
    fondo = BackgroundRectangle(et, color=FONDO, fill_opacity=0.85, buff=0.06)
    return Pieza(linea, fondo, et, partes={"linea": linea, "rotulo": et, "fondo": fondo})


# ---------- tabla ----------
def tabla(encabezados, filas, ancho=6.5, fs=34, color_enc=CIAN):
    """Tabla de datos. Celdas de datos: `partes[(fila, col)]` (fila desde 0); encabezados: `partes[("enc", col)]`.
    Cada celda es VGroup(caja, contenido). El texto se parte por palabras si la columna es angosta."""
    matriz = [list(encabezados)] + [list(f) for f in filas]

    def estilo(r, c):
        if r == 0:
            return {"color": color_enc, "fondo": (color_enc, 0.15)}
        return {"color": WHITE}
    g, celdas = _rejilla(matriz, ancho, fs, estilo=estilo)
    partes = {("enc", j): celdas[0][j] for j in range(len(encabezados))}
    for i in range(len(filas)):
        for j in range(len(encabezados)):
            partes[(i, j)] = celdas[i + 1][j]
    return Pieza(*g, partes=partes)


def resaltar_celda(tabla_, fila, col=None, color=AMARILLO, animar=True):
    """Resalta una celda (borde grueso y fondo tenue). `fila, col` o, con `col=None`, la clave completa
    (p. ej. ("enc", 1)). Con animar=True devuelve la lista de animaciones; con False lo aplica ya."""
    celda = tabla_.partes[fila if col is None else (fila, col)]
    if animar:
        return [celda[0].animate.set_stroke(color, 7).set_fill(color, 0.25), celda[1].animate.set_color(color)]
    celda[0].set_stroke(color, 7).set_fill(color, 0.25)
    celda[1].set_color(color)
    return celda


# ---------- gráfico de líneas ----------
def grafico_lineas(xs, ys, ancho=6.5, alto=4.5, color=CIAN, paso_y=None, titulo_eje=None,
                   mostrar_valores=False, etiquetas_x=None, desde_cero=True):
    """Gráfico de líneas con puntos. `xs` numéricos (separación proporcional) o cadenas (separación pareja).
    `partes`: "linea", ("punto", i), ("valor", i), ("etiqueta_x", i), "eje_x", "eje_y".
    `g.x_de(i)` y `g.y_de(v)` dan coordenadas de pantalla."""
    n = len(xs)
    assert n == len(ys) and n > 0
    numerico = all(isinstance(x, (int, float)) for x in xs) and n > 1
    ymax_d, ymin_d = max(ys), min(ys)
    base = 0 if desde_cero else ymin_d
    paso = paso_y or _paso_bonito(ymax_d - base)
    vmin = 0 if desde_cero else math.floor(ymin_d / paso + 1e-9) * paso
    tope = math.ceil(ymax_d / paso - 1e-9) * paso
    if tope <= vmin:
        tope = vmin + paso
    wy = max(_t(_n(round(vmin, 10))).width, _t(_n(round(tope, 10))).width)
    mi = wy + 0.3
    plot_w = ancho - mi - 0.1
    anios = all(isinstance(x, int) and 1000 <= x <= 2999 for x in xs)   # los años no llevan separador de miles
    textos_x = [str(e) for e in (etiquetas_x or [str(x) if anios else _n(x) for x in xs])]
    et = [_t(s) for s in textos_x]
    mb = max(e.height for e in et) + 0.25
    mt = (0.6 if mostrar_valores else 0.3) + (0.5 if titulo_eje else 0)
    plot_h = alto - mt - mb
    if paso_y is None:
        paso = _paso_bonito(tope - vmin, min(5, max(2, int(plot_h // 0.55))))
        vmin = 0 if desde_cero else math.floor(ymin_d / paso + 1e-9) * paso
        tope = math.ceil(ymax_d / paso - 1e-9) * paso
        if tope <= vmin:
            tope = vmin + paso
    rejilla, marcas, etq_y = _marcas_y(vmin, tope, paso, plot_w, plot_h)
    eje_y = Line([0, 0, 0], [0, plot_h, 0], color=GRIS_CLARO, stroke_width=3)
    eje_x = Line([0, 0, 0], [plot_w, 0, 0], color=GRIS_CLARO, stroke_width=3)
    pad = max(0.45, max(e.width for e in et) / 2 + 0.05)
    if mostrar_valores:
        pad = max(pad, max(_t(_n(y)).width for y in ys) / 2 + 0.15)
    if numerico:
        x0, x1 = min(xs), max(xs)
        px = [pad + (x - x0) / (x1 - x0) * (plot_w - 2 * pad) for x in xs]
    elif n > 1:
        px = [pad + i / (n - 1) * (plot_w - 2 * pad) for i in range(n)]
    else:
        px = [plot_w / 2]
    py = [(y - vmin) / (tope - vmin) * plot_h for y in ys]
    pts = [np.array([a, b, 0.0]) for a, b in zip(px, py)]
    linea = VMobject(color=color, stroke_width=6)
    linea.set_points_as_corners(pts)
    partes = {"eje_x": eje_x, "eje_y": eje_y, "rejilla": rejilla, "marcas_y": marcas, "etiquetas_y": etq_y,
              "linea": linea}
    todo = [rejilla, marcas, etq_y, eje_x, eje_y, linea]
    # etiquetas del eje x: se salta alguna si no caben
    paso_et = 1
    while paso_et < n:
        ok = all(abs(px[i] - px[j]) >= (et[i].width + et[j].width) / 2 + 0.12
                 for i in range(0, n, paso_et) for j in range(i + paso_et, n, paso_et)[:1])
        if ok:
            break
        paso_et += 1
    for i in range(n):
        d = Dot(pts[i], radius=0.1, color=color)
        partes[("punto", i)] = d
        todo.append(d)
        if i % paso_et == 0:
            et[i].move_to([px[i], -0.12 - et[i].height / 2, 0])
            partes[("etiqueta_x", i)] = et[i]
            todo += [et[i], Line([px[i], 0, 0], [px[i], -0.1, 0], color=GRIS_CLARO, stroke_width=3)]
        if mostrar_valores:
            lv = _t(_n(ys[i]), 34, WHITE)
            vecinos = [ys[j] for j in (i - 1, i + 1) if 0 <= j < n]
            abajo = all(v > ys[i] for v in vecinos) and py[i] > lv.height + 0.3   # valle: el valor va debajo
            lv.move_to([px[i], py[i] + (-1 if abajo else 1) * (0.14 + lv.height / 2), 0])
            partes[("valor", i)] = lv
            todo.append(lv)
    if titulo_eje:
        ti = _ajustar(titulo_eje, ancho - 0.2, 32, GRIS_CLARO)
        ti.move_to([-mi + 0.1 + ti.width / 2, plot_h + mt - 0.25, 0])
        partes["titulo_eje"] = ti
        todo.append(ti)
    g = Pieza(*todo, partes=partes)
    g.move_to(ORIGIN)
    g.vmin, g.vmax, g._px = vmin, tope, px

    def y_de(v):
        ey = g.partes["eje_y"]
        return ey.get_bottom()[1] + (v - g.vmin) / (g.vmax - g.vmin) * ey.height
    g.y_de = y_de
    g.x_de = lambda i: g.partes[("punto", i)].get_center()[0]
    return g
