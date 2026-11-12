"""Piezas de probabilidad y conteo: tabla de doble entrada, diagrama de Venn de dos conjuntos, casillas
y fichas de colores.

Mismas convenciones que estadistica.py: tamaño final en unidades de pantalla, texto a tamaño fijo (>= 32),
`Pieza` con `partes`. Para resaltar celdas de la tabla use `estadistica.resaltar_celda(t, clave)`."""
import math

import numpy as np
from manim import *

from ..base import AMARILLO, CIAN, FONDO, GRIS, GRIS_CLARO, NARANJA, VERDE, num
from .estadistica import Pieza, _ajustar, _n, _rejilla, _t, _vacio


def tabla_doble_entrada(filas, columnas, datos, con_totales=True, ancho=6.8, esquina="", fs=34,
                        total="Total"):
    """Tabla de frecuencias. `filas` y `columnas`: nombres; `datos[i][j]`: la frecuencia. Con totales se
    calculan solos. `partes`: (i, j) datos; ("enc_fila", i); ("enc_col", j); ("total_fila", i) = total de la
    fila i; ("total_col", j) = total de la columna j; "total" = total general. Cada celda es VGroup(caja,
    contenido)."""
    nf, nc = len(filas), len(columnas)
    assert len(datos) == nf and all(len(f) == nc for f in datos)
    m = [[esquina] + list(columnas) + ([total] if con_totales else [])]
    for i in range(nf):
        m.append([filas[i]] + list(datos[i]) + ([sum(datos[i])] if con_totales else []))
    if con_totales:
        m.append([total] + [sum(datos[i][j] for i in range(nf)) for j in range(nc)]
                 + [sum(sum(f) for f in datos)])

    def estilo(r, c):
        es_tot = con_totales and (r == nf + 1 or c == nc + 1)
        if r == 0 and c == 0:
            return {}
        if r == 0 or c == 0:
            return {"color": CIAN, "fondo": (CIAN, 0.15)}
        if es_tot:
            return {"color": NARANJA, "fondo": (NARANJA, 0.12)}
        return {"color": WHITE}
    g, ce = _rejilla(m, ancho, fs, estilo=estilo)
    partes = {}
    for j in range(nc):
        partes[("enc_col", j)] = ce[0][j + 1]
    for i in range(nf):
        partes[("enc_fila", i)] = ce[i + 1][0]
        for j in range(nc):
            partes[(i, j)] = ce[i + 1][j + 1]
        if con_totales:
            partes[("total_fila", i)] = ce[i + 1][nc + 1]
    if con_totales:
        for j in range(nc):
            partes[("total_col", j)] = ce[nf + 1][j + 1]
        partes["total"] = ce[nf + 1][nc + 1]
        partes["enc_total_fila"], partes["enc_total_col"] = ce[0][nc + 1], ce[nf + 1][0]
    return Pieza(*g, partes=partes)


def venn2(nombre_a, nombre_b, solo_a, ambos, solo_b, fuera=None, ancho=6.8, alto=4.6):
    """Diagrama de Venn de dos conjuntos dentro de su universo. Los números van en su región. `partes`:
    "universo", "circulo_a", "circulo_b", "solo_a", "ambos", "solo_b", "fuera" (formas rellenables, sin
    relleno al inicio), ("valor", region) con el número, y "nombre_a", "nombre_b". Use `colorear_region`."""
    na = _ajustar(nombre_a, (ancho - 0.6) / 2, 34, CIAN)
    nb = _ajustar(nombre_b, (ancho - 0.6) / 2, 34, NARANJA)
    h_nom = max(na.height, nb.height) + 0.2
    r = min((alto - h_nom - 0.7) / 2, (ancho - 0.5) / 3)
    cy = -(alto / 2) + 0.35 + r
    ca = Circle(radius=r, color=CIAN, stroke_width=6).move_to([-r / 2, cy, 0])
    cb = Circle(radius=r, color=NARANJA, stroke_width=6).move_to([r / 2, cy, 0])
    uni = Rectangle(width=ancho, height=alto, color=GRIS_CLARO, stroke_width=4)
    regiones = {
        "solo_a": Difference(ca, cb), "ambos": Intersection(ca, cb), "solo_b": Difference(cb, ca),
        "fuera": Difference(uni, Union(ca, cb)),
    }
    for reg in regiones.values():
        reg.set_fill(WHITE, 0).set_stroke(width=0)
    pos = {"solo_a": [-r, cy, 0], "ambos": [0, cy, 0], "solo_b": [r, cy, 0],
           "fuera": [ancho / 2 - 0.55, -alto / 2 + 0.45, 0]}
    vals = {"solo_a": solo_a, "ambos": ambos, "solo_b": solo_b, "fuera": fuera}
    partes = {"universo": uni, "circulo_a": ca, "circulo_b": cb, **regiones}
    mobs = [*regiones.values(), uni, ca, cb]
    for k, v in vals.items():
        if v is None:
            continue
        t = _t(_n(v), 38, WHITE)
        t.move_to(pos[k])
        partes[("valor", k)] = t
        mobs.append(t)
    na.move_to([-ancho / 2 + 0.2 + na.width / 2, alto / 2 - 0.1 - na.height / 2, 0])
    nb.move_to([ancho / 2 - 0.2 - nb.width / 2, alto / 2 - 0.1 - nb.height / 2, 0])
    partes["nombre_a"], partes["nombre_b"] = na, nb
    mobs += [na, nb]
    return Pieza(*mobs, partes=partes)


def colorear_region(venn, region, color=AMARILLO, opacidad=0.45, animar=True):
    """Rellena una región ("solo_a", "ambos", "solo_b" o "fuera"). Con animar=True devuelve la animación."""
    reg = venn.partes[region]
    if animar:
        return [reg.animate.set_fill(color, opacidad)]
    reg.set_fill(color, opacidad)
    return reg


def casillas(n, contenidos=None, ancho=7.0, lado=1.1, color=CIAN):
    """`n` casillas en fila (en varias filas si no caben) para conteo y permutaciones. `contenidos`: textos
    ya puestos (None = vacías). `partes`: ("caja", i), ("contenido", i) si hay, y "todas".
    `poner_en_casilla(pieza, i, texto)` crea el texto que va dentro de la casilla i."""
    sep = 0.18
    lado = min(lado, (ancho - sep * (n - 1)) / n) if n * (lado + sep) - sep > ancho and ancho > 0 else lado
    lado = max(lado, 0.7)
    por_fila = max(1, int((ancho + sep) // (lado + sep)))
    partes, cajas = {}, VGroup()
    for i in range(n):
        f, c = divmod(i, por_fila)
        caja = RoundedRectangle(width=lado, height=lado, corner_radius=0.12, color=color, stroke_width=5)
        caja.move_to([c * (lado + sep), -f * (lado + sep), 0])
        partes[("caja", i)] = caja
        cajas.add(caja)
    todas = VGroup(cajas)
    if contenidos:
        for i, s in enumerate(contenidos):
            if s is None or s == "":
                continue
            t = _t(_n(s), 40, WHITE)
            t.move_to(partes[("caja", i)])
            partes[("contenido", i)] = t
            todas.add(t)
    partes["todas"] = cajas
    g = Pieza(*todas, partes=partes)
    g.move_to(ORIGIN)
    return g


def poner_en_casilla(pieza, i, texto, color=AMARILLO, size=40):
    """Texto (listo para FadeIn / Write) centrado en la casilla i de una pieza hecha con `casillas`."""
    t = _t(_n(texto), size, color)
    t.move_to(pieza.partes[("caja", i)])
    return t


def fichas(colores_y_cantidades, ancho=7.0, radio=0.3, con_conteo=True):
    """Fichas de colores para probabilidad: una fila (o varias) por color. Cada elemento de
    `colores_y_cantidades` es (color, cantidad) o (color, cantidad, nombre); con `con_conteo` se escribe
    «cantidad nombre» a la izquierda. `partes`: (k, i) = ficha i del color k; ("grupo", k); "todas"."""
    grupos, partes = [], {}
    etiquetas = []
    for k, e in enumerate(colores_y_cantidades):
        col, cant = e[0], e[1]
        nom = e[2] if len(e) > 2 else ""
        etiquetas.append(_t(f"{num(cant, False)} {nom}".strip(), 34, WHITE) if con_conteo else None)
    wl = max((t.width for t in etiquetas if t is not None), default=0) + (0.3 if con_conteo else 0)
    paso = 2 * radio + 0.14
    por_fila = max(1, int((ancho - wl + 0.14) // paso))
    todas = VGroup()
    y = 0.0
    for k, e in enumerate(colores_y_cantidades):
        col, cant = e[0], e[1]
        discos = VGroup()
        for i in range(cant):
            d = Circle(radius=radio, fill_color=col, fill_opacity=1, stroke_color=WHITE, stroke_width=3)
            f, c = divmod(i, por_fila)
            d.move_to([c * paso, y - f * paso, 0])
            partes[(k, i)] = d
            discos.add(d)
        grupo = VGroup(discos)
        if etiquetas[k] is not None:
            etiquetas[k].move_to([-radio - 0.25 - etiquetas[k].width / 2, discos.get_center()[1], 0])
            grupo.add(etiquetas[k])
            partes[("etiqueta", k)] = etiquetas[k]
        partes[("grupo", k)] = grupo
        todas.add(grupo)
        filas = max(1, math.ceil(cant / por_fila))
        y -= filas * paso + 0.3
    partes["todas"] = todas
    g = Pieza(*todas, partes=partes)
    g.move_to(ORIGIN)
    return g
