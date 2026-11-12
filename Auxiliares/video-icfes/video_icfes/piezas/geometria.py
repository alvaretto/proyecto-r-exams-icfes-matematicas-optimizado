"""Piezas de geometría: triángulo, rectángulo con medidas, plano cartesiano, ángulos y círculo con radio.

Mismas convenciones que estadistica.py: tamaño final en unidades de pantalla, texto a tamaño fijo (>= 32),
`Pieza` con `partes`. Los puntos se dan como pares (x, y) con y hacia arriba."""
import math

import numpy as np
from manim import *

from ..base import AMARILLO, CIAN, FONDO, GRIS, GRIS_CLARO, NARANJA, VERDE, num
from .estadistica import Pieza, _n, _t


def _p(q):
    return np.array([q[0], q[1], 0.0])


def _u(v):
    return v / np.linalg.norm(v)


def _caja(texto_mob, color=FONDO, op=0.85):
    return BackgroundRectangle(texto_mob, color=color, fill_opacity=op, buff=0.04)


def _dentro_tri(P, q, margen=0.04):
    """¿Está q a más de `margen` de cada lado, dentro del triángulo P (3 puntos)?"""
    signos = []
    for i in range(3):
        a, b = P[i], P[(i + 1) % 3]
        e = b - a
        c = e[0] * (q[1] - a[1]) - e[1] * (q[0] - a[0])
        signos.append(c / np.linalg.norm(e))
    return all(s > margen for s in signos) or all(s < -margen for s in signos)


def marca_angulo(vertice, p1, p2, radio=0.45, color=AMARILLO, rotulo=None):
    """Arco del ángulo menor entre las semirrectas vértice->p1 y vértice->p2, con rótulo opcional
    (fuera del arco, sobre la bisectriz). `partes`: "arco", "rotulo"."""
    v, a, b = _p(vertice), _p(p1), _p(p2)
    a1, a2 = math.atan2(*(a - v)[1::-1]), math.atan2(*(b - v)[1::-1])
    delta = (a2 - a1 + math.pi) % (2 * math.pi) - math.pi
    arco = Arc(radius=radio, start_angle=a1, angle=delta, arc_center=v, color=color, stroke_width=5)
    partes, mobs = {"arco": arco}, [arco]
    if rotulo:
        t = _t(rotulo, 34, color)
        bis = _u(_u(a - v) + _u(b - v))
        t.move_to(v + bis * (radio + 0.12 + (abs(bis[0]) * t.width + abs(bis[1]) * t.height) / 2))
        partes["rotulo"] = t
        mobs.append(t)
    return Pieza(*mobs, partes=partes)


def angulo_recto(vertice, p1, p2, lado=0.35, color=AMARILLO):
    """Cuadradito del ángulo recto en `vertice`, entre las direcciones a p1 y a p2. `partes`: "marca"."""
    v, a, b = _p(vertice), _u(_p(p1) - _p(vertice)), _u(_p(p2) - _p(vertice))
    m = VMobject(color=color, stroke_width=5)
    m.set_points_as_corners([v + a * lado, v + (a + b) * lado, v + b * lado])
    return Pieza(m, partes={"marca": m})


def triangulo(A, B, C, rotulos_lados=None, rotulos_angulos=None, ancho=4.8, alto=4.2, vertices=None,
              color=CIAN):
    """Triángulo ABC a escala: se agranda hasta que él y sus rótulos llenan `ancho` x `alto` (con texto de
    tamaño fijo, un triángulo chico no deja sitio a los rótulos: pida un área generosa).
    `rotulos_lados`: 3 textos para AB, BC, CA (None omite). `rotulos_angulos`: 3 textos para los vértices
    A, B, C; van dentro con su arco, o fuera del vértice si dentro no caben. Un ángulo de 90 grados lleva
    cuadradito. `vertices`: 3 nombres opcionales. `partes`: "borde", ("lado", i), ("rotulo_lado", i),
    ("rotulo_angulo", i), ("arco", i), ("nombre", i)."""
    P0 = [_p(A), _p(B), _p(C)]
    xs, ys = [p[0] for p in P0], [p[1] for p in P0]
    cen = np.array([(max(xs) + min(xs)) / 2, (max(ys) + min(ys)) / 2, 0])
    P0 = [p - cen for p in P0]
    w, h = max(xs) - min(xs), max(ys) - min(ys)
    lad = [(_t(t, 34, WHITE) if t else None) for t in (rotulos_lados or [None] * 3)]
    ang = [(_t(t, 34, AMARILLO) if t else None) for t in (rotulos_angulos or [None] * 3)]
    nom = [_t(t, 36, WHITE) for t in vertices] if vertices else [None] * 3

    def ext(t, d):
        return (abs(d[0]) * t.width + abs(d[1]) * t.height) / 2

    def armar(sc):
        P = [p * sc for p in P0]
        centroide = sum(P) / 3
        borde = Polygon(*P, color=color, stroke_width=6)
        partes, mobs = {"borde": borde}, [borde]
        for i in range(3):
            partes[("lado", i)] = Line(P[i], P[(i + 1) % 3], color=color, stroke_width=6)
        for i in range(3):
            if lad[i] is None:
                continue
            a, b = P[i], P[(i + 1) % 3]
            mid, e = (a + b) / 2, _u(b - a)
            nrm = np.array([-e[1], e[0], 0])
            if np.dot(nrm, mid - centroide) < 0:
                nrm = -nrm
            lad[i].move_to(mid + nrm * (0.12 + ext(lad[i], nrm)))
            partes[("rotulo_lado", i)] = lad[i]
            mobs.append(lad[i])
        for i in range(3):
            v, a, b = P[i], P[(i + 1) % 3], P[(i - 1) % 3]
            u1, u2 = _u(a - v), _u(b - v)
            theta = math.acos(max(-1, min(1, float(np.dot(u1, u2)))))
            bis = _u(u1 + u2)
            recto = abs(math.degrees(theta) - 90) < 0.5
            if recto:
                r = angulo_recto(v, a, b, 0.38)
                partes[("arco", i)] = r
                mobs.append(r)
            fuera_ext = 0.0
            t = ang[i]
            if t is not None:
                colocado, d = False, 0.65
                while d < 4.0:
                    c = v + bis * d
                    esq = [c + np.array([sx * (t.width / 2 + 0.06), sy * (t.height / 2 + 0.06), 0])
                           for sx in (-1, 1) for sy in (-1, 1)]
                    if all(_dentro_tri(P, q) and np.linalg.norm(q - v) >= 0.5 for q in esq):
                        colocado = True
                        break
                    d += 0.05
                if colocado:
                    t.move_to(c)
                    if not recto:
                        arc = marca_angulo(v, a, b, 0.4)
                        partes[("arco", i)] = arc
                        mobs.append(arc)
                else:
                    t.move_to(v - bis * (0.1 + ext(t, bis) + 0.05))
                    fuera_ext = 2 * ext(t, bis) + 0.1
                partes[("rotulo_angulo", i)] = t
                mobs.append(t)
            if nom[i] is not None:
                nom[i].move_to(v - bis * (0.14 + fuera_ext + ext(nom[i], bis)))
                partes[("nombre", i)] = nom[i]
                mobs.append(nom[i])
        for i in range(3):
            mobs.append(partes[("lado", i)])
        return mobs, partes

    lo, hi = 0.05, min(ancho / w, alto / h)
    for _ in range(14):
        mid = (lo + hi) / 2
        mobs, _p_ = armar(mid)
        g = VGroup(*mobs)
        if g.width <= ancho and g.height <= alto:
            lo = mid
        else:
            hi = mid
    mobs, partes = armar(lo)
    g = Pieza(*mobs, partes=partes)
    g.move_to(ORIGIN)
    return g


def rectangulo_medidas(base, altura, ancho=4.8, alto=3.6, rotulo_base=None, rotulo_altura=None, color=CIAN,
                       unidad="", con_recto=True):
    """Rectángulo a escala con su base (rótulo abajo) y su altura (rótulo a la izquierda). Los rótulos por
    defecto son el número con su unidad. `partes`: "rect", "rotulo_base", "rotulo_altura", "recto"."""
    rb = rotulo_base if rotulo_base is not None else f"{num(base, False)} {unidad}".strip()
    ra = rotulo_altura if rotulo_altura is not None else f"{num(altura, False)} {unidad}".strip()
    tb, ta = _t(rb, 36, WHITE), _t(ra, 36, WHITE)
    s = min((ancho - ta.width - 0.3) / base, (alto - tb.height - 0.3) / altura)
    rect = Rectangle(width=base * s, height=altura * s, color=color, stroke_width=6,
                     fill_color=color, fill_opacity=0.12)
    tb.next_to(rect, DOWN, buff=0.15)
    ta.next_to(rect, LEFT, buff=0.15)
    partes = {"rect": rect, "rotulo_base": tb, "rotulo_altura": ta}
    mobs = [rect, tb, ta]
    if con_recto:
        bl, br, tl = rect.get_corner(DL), rect.get_corner(DR), rect.get_corner(UL)
        rc = angulo_recto(bl, br, tl, min(0.3, base * s / 4, altura * s / 4))
        partes["recto"] = rc
        mobs.append(rc)
    g = Pieza(*mobs, partes=partes)
    g.move_to(ORIGIN)
    return g


# ---------- plano cartesiano ----------
def plano(x_rango, y_rango, paso=1, ancho=5.6, alto=5.6, cuadricula=True, nombres_ejes=("x", "y")):
    """Plano cartesiano con rejilla, ejes con flecha y números. Escala igual en los dos ejes.
    `g.c2p(x, y)` da el punto de pantalla; úselo (o `punto`/`segmento`) DESPUÉS de colocar la pieza.
    `partes`: "marco", "eje_x", "eje_y", "rejilla", "etiquetas_x", "etiquetas_y"."""
    x0, x1 = x_rango
    y0, y1 = y_rango
    ticks_x = [x0 + i * paso for i in range(int(round((x1 - x0) / paso)) + 1)]
    ticks_y = [y0 + i * paso for i in range(int(round((y1 - y0) / paso)) + 1)]
    wy = max(_t(_n(v)).width for v in ticks_y)
    ml = (wy + 0.25) if x0 >= 0 else 0.15
    mb = 0.55 if y0 >= 0 else 0.15
    mr, mt = 0.75, 0.75
    s = min((ancho - ml - mr) / (x1 - x0), (alto - mb - mt) / (y1 - y0))
    W, H = s * (x1 - x0), s * (y1 - y0)

    def c(x, y):
        return np.array([(x - x0) * s, (y - y0) * s, 0.0])
    marco = Rectangle(width=W, height=H, stroke_width=0, fill_opacity=0).move_to([W / 2, H / 2, 0])
    rejilla = VGroup()
    if cuadricula:
        for x in ticks_x:
            rejilla.add(Line(c(x, y0), c(x, y1), color=GRIS, stroke_width=1.5, stroke_opacity=0.35))
        for y in ticks_y:
            rejilla.add(Line(c(x0, y), c(x1, y), color=GRIS, stroke_width=1.5, stroke_opacity=0.35))
    ey0 = 0 if y0 <= 0 <= y1 else y0
    ex0 = 0 if x0 <= 0 <= x1 else x0
    eje_x = Arrow(c(x0, ey0), c(x1, ey0) + np.array([0.3, 0, 0]), buff=0, color=GRIS_CLARO, stroke_width=4,
                  tip_length=0.22, max_tip_length_to_length_ratio=1)
    eje_y = Arrow(c(ex0, y0), c(ex0, y1) + np.array([0, 0.3, 0]), buff=0, color=GRIS_CLARO, stroke_width=4,
                  tip_length=0.22, max_tip_length_to_length_ratio=1)
    etq_x, etq_y = VGroup(), VGroup()
    kx = max(1, math.ceil((max(_t(_n(v)).width for v in ticks_x) + 0.2) / (s * paso)))
    ky = max(1, math.ceil(0.5 / (s * paso)))
    for i, v in enumerate(ticks_x):
        if abs(v) < 1e-9 or i % kx:
            continue
        t = _t(_n(round(v, 10)))
        t.move_to(c(v, ey0) + np.array([0, -0.15 - t.height / 2, 0]))
        etq_x.add(VGroup(_caja(t), t))
    for i, v in enumerate(ticks_y):
        if abs(v) < 1e-9 or i % ky:
            continue
        t = _t(_n(round(v, 10)))
        t.move_to(c(ex0, v) + np.array([-0.15 - t.width / 2, 0, 0]))
        etq_y.add(VGroup(_caja(t), t))
    mobs = [rejilla, marco, eje_x, eje_y, etq_x, etq_y]
    if x0 <= 0 <= x1 and y0 <= 0 <= y1:
        cero = _t("0")
        cero.move_to(c(0, 0) + np.array([-0.2, -0.22, 0]))
        mobs.append(VGroup(_caja(cero), cero))
    nx, ny = _t(nombres_ejes[0], 36, WHITE), _t(nombres_ejes[1], 36, WHITE)
    nx.move_to(c(x1, ey0) + np.array([0.62, 0, 0]))
    ny.move_to(c(ex0, y1) + np.array([0, 0.62, 0]))
    mobs += [nx, ny]
    g = Pieza(*mobs, partes={"marco": marco, "eje_x": eje_x, "eje_y": eje_y, "rejilla": rejilla,
                             "etiquetas_x": etq_x, "etiquetas_y": etq_y})
    g.move_to(ORIGIN)

    def c2p(x, y):
        m = g.partes["marco"]
        return np.array([m.get_left()[0] + (x - x0) / (x1 - x0) * m.width,
                         m.get_bottom()[1] + (y - y0) / (y1 - y0) * m.height, 0.0])
    g.c2p = c2p
    return g


def punto(plano_, x, y, rotulo=None, color=AMARILLO, direccion=UR):
    """Punto (x, y) del plano, con rótulo en la `direccion` indicada (UR, UL, DR, DL, UP, DOWN, LEFT, RIGHT).
    `partes`: "punto", "rotulo"."""
    c = plano_.c2p(x, y)
    d = Dot(c, radius=0.12, color=color)
    partes, mobs = {"punto": d}, [d]
    if rotulo:
        t = _t(rotulo, 34, color)
        dr = np.array(direccion, dtype=float)
        t.move_to(c + np.array([dr[0] * (0.16 + t.width / 2), dr[1] * (0.16 + t.height / 2), 0]))
        mobs += [_caja(t), t]
        partes["rotulo"] = t
    return Pieza(*mobs, partes=partes)


def segmento(plano_, p, q, rotulo=None, color=NARANJA, lado=1):
    """Segmento entre los puntos p y q (pares x, y) del plano, con rótulo a un costado (`lado` = 1 o -1
    lo cambia). `partes`: "linea", "rotulo"."""
    a, b = plano_.c2p(*p), plano_.c2p(*q)
    l = Line(a, b, color=color, stroke_width=6)
    partes, mobs = {"linea": l}, [l]
    if rotulo:
        e = _u(b - a)
        nrm = np.array([-e[1], e[0], 0])
        if nrm[1] < -1e-9 or (abs(nrm[1]) < 1e-9 and nrm[0] < 0):
            nrm = -nrm
        nrm = nrm * lado
        t = _t(rotulo, 34, color)
        t.move_to((a + b) / 2 + nrm * (0.14 + (abs(nrm[0]) * t.width + abs(nrm[1]) * t.height) / 2))
        mobs += [_caja(t), t]
        partes["rotulo"] = t
    return Pieza(*mobs, partes=partes)


def circulo_radio(r, rotulo=None, ancho=4.6, alto=4.6, color=CIAN, angulo=20):
    """Círculo con su radio dibujado y rotulado (por defecto «r = valor»). El rótulo va dentro si el círculo
    es grande y, si no, a la derecha de la punta del radio (entonces `angulo` vale 0). El dibujo es a la
    escala de `ancho` x `alto`; `r` solo sirve para el rótulo. `partes`: "circulo", "radio", "centro",
    "rotulo"."""
    t = _t(rotulo if rotulo is not None else f"r = {num(r, False)}", 36, AMARILLO)
    R = (min(ancho, alto) - 0.3) / 2
    dentro = R >= 1.5
    if not dentro:
        R = min((ancho - t.width - 0.4) / 2, (alto - 0.3) / 2)
        angulo = 0
    circ = Circle(radius=R, color=color, stroke_width=6, fill_color=color, fill_opacity=0.12)
    th = math.radians(angulo)
    fin = np.array([R * math.cos(th), R * math.sin(th), 0])
    radio = Line(ORIGIN, fin, color=AMARILLO, stroke_width=6)
    centro = Dot(ORIGIN, radius=0.1, color=AMARILLO)
    if dentro:
        nrm = np.array([-math.sin(th), math.cos(th), 0])
        t.move_to(fin / 2 + nrm * (0.12 + (abs(nrm[0]) * t.width + abs(nrm[1]) * t.height) / 2))
        fondo = _caja(t)
    else:
        t.move_to(fin + np.array([0.15 + t.width / 2, 0, 0]))
        fondo = _caja(t, op=0)
    g = Pieza(circ, radio, centro, fondo, t,
              partes={"circulo": circ, "radio": radio, "centro": centro, "rotulo": t})
    g.move_to(ORIGIN)
    return g
