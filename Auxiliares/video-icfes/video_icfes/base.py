"""Base de las escenas: estilo, formato (horizontal o vertical), zonas de pantalla y sincronía con la voz.

Cada ejercicio escribe `class Video(VideoBase)` con métodos s1_…, s2_… (uno por escena del guion).
Las escenas piden ZONAS («titulo», «figura», «a», «b», «pie») en vez de coordenadas fijas: así el mismo
código sirve para 1920×1080 y para 1080×1920. Donde haga falta, `self.hv(valor_h, valor_v)`.

La escala de píxeles es la misma en los dos formatos (1 unidad = 135 px): un texto de tamaño 40 se lee
igual. Lo que cambia es el cuadro: 14,2 × 8 unidades en horizontal y 8 × 14,2 en vertical."""
import json, math, os

import numpy as np
from manim import *

from . import proyecto

DIR = proyecto.carpeta()
FMT = proyecto.formato()
VERTICAL = FMT == "v"
SOLO_ANCLAS = os.environ.get("VIDEO_SOLO_ANCLAS") == "1"     # pasada rápida: solo comprueba las palabras ancla

BORRADOR = os.environ.get("VIDEO_BORRADOR") == "1"            # baja resolución, para ajustar los dibujos
_pw, _ph = proyecto.FORMATOS[FMT]
if BORRADOR:
    _pw, _ph = _pw // 3, _ph // 3
config.pixel_width, config.pixel_height, config.frame_rate = _pw, _ph, (15 if BORRADOR else 30)
_LARGO, _CORTO = 8 * 16 / 9, 8.0
config.frame_height = _LARGO if VERTICAL else _CORTO
config.frame_width = _CORTO if VERTICAL else _LARGO
FW, FH = config.frame_width, config.frame_height

try:
    DUR = proyecto.leer("duraciones.json", DIR)
    PAL = proyecto.leer("palabras.json", DIR)
except FileNotFoundError:      # sin narración: solo sirve para las escenas de muestra de piezas/
    DUR, PAL = {}, {}

# ---------- estilo ----------
FONT = "Noto Sans"
FONDO = "#0f172a"
AMARILLO = "#facc15"
CIAN = "#22d3ee"
NARANJA = "#fb923c"
VERDE = "#4ade80"
ROJO = "#f87171"
GRIS = "#94a3b8"
GRIS_CLARO = "#cbd5e1"   # etiquetas pequeñas que hay que leer en celular: nunca más oscuro que esto
TAM_MIN = 32             # ningún texto por debajo de este tamaño (medido ilegible en celular)
config.background_color = FONDO
# La clase «standalone» por defecto de Manim pide preview.sty, que este TeX Live no trae.
config.tex_template = TexTemplate(documentclass=r"\documentclass[12pt]{article}",
                                  preamble="\\usepackage{amsmath}\n\\usepackage{amssymb}\n\\pagestyle{empty}")

# zona: (x, y, ancho, alto) en unidades de pantalla
ZONAS = {
    "h": {"titulo": (0, 3.3, 13.0, 1.0), "figura": (0, 0.35, 13.0, 4.5), "pie": (0, -2.85, 13.0, 1.5),
          "a": (-3.5, 0.35, 6.2, 4.5), "b": (3.5, 0.35, 6.2, 4.5), "todo": (0, 0, 13.0, 7.2)},
    "v": {"titulo": (0, 6.1, 7.3, 1.4), "figura": (0, 1.6, 7.3, 7.0), "pie": (0, -4.3, 7.3, 2.6),
          "a": (0, 3.4, 7.3, 3.4), "b": (0, -0.2, 7.3, 3.4), "todo": (0, 0, 7.3, 13.2)},
}[FMT]


def T(s, size=40, color=WHITE, ancho=None, **kw):
    """Texto. Si se da `ancho`, se encoge hasta caber (útil en vertical)."""
    t = Text(s, font=FONT, font_size=size, color=color, **kw)
    lim = ancho if ancho is not None else FW - 0.6
    if t.width > lim:
        t.scale_to_fit_width(lim)
    return t


def num(x, tex=True):
    """Número en notación colombiana: coma decimal y espacio de miles."""
    if abs(x - round(x)) < 1e-9:
        s = f"{int(round(x)):,}"
        return s.replace(",", r"\," if tex else " ")
    s = f"{x:.2f}".rstrip("0")
    return s.replace(".", "{,}" if tex else ",")


def es_exacto(x):
    return abs(x * 100 - round(x * 100)) < 1e-9


def hv(h, v):
    """Un valor para horizontal y otro para vertical."""
    return v if VERTICAL else h


class VideoBase(MovingCameraScene):
    hv = staticmethod(hv)

    # ---------- zonas ----------
    def zona(self, nombre):
        x, y, w, h = ZONAS[nombre]
        return np.array([x, y, 0.0]), w, h

    def ubicar(self, mob, zona="figura", llenar=False, dx=0.0, dy=0.0):
        """Mueve `mob` al centro de la zona y lo encoge si no cabe (con llenar=True, también lo agranda)."""
        c, w, h = self.zona(zona)
        if mob.width > 1e-6 and mob.height > 1e-6:
            f = min(w / mob.width, h / mob.height)
            if f < 1 or llenar:
                mob.scale(f)
        mob.move_to(c + np.array([dx, dy, 0.0]))
        return mob

    def repartir(self, mobs, zona="figura", columnas_v=2, buff=0.25):
        """En fila en horizontal; en rejilla de `columnas_v` columnas en vertical."""
        g = mobs if isinstance(mobs, VGroup) else VGroup(*mobs)
        if VERTICAL:
            g.arrange_in_grid(cols=columnas_v, buff=buff)
        else:
            g.arrange(RIGHT, buff=buff)
        return self.ubicar(g, zona)

    def apilar(self, mobs, zona="figura", buff=0.4):
        g = mobs if isinstance(mobs, VGroup) else VGroup(*mobs)
        g.arrange(DOWN, buff=buff)
        return self.ubicar(g, zona)

    def titulo(self, texto, size=50, color=AMARILLO):
        return self.ubicar(T(texto, size, color), "titulo")

    def pie(self, texto, size=44, color=WHITE):
        return self.ubicar(T(texto, size, color), "pie")

    # ---------- sincronía con la narración ----------
    def ini(self, k, empuje=True):
        self.k = k
        self.t0 = self.renderer.time
        self.Tseg = DUR[k]
        # El audio no se monta con add_sound (dejó escenas mudas): montar.py lo mezcla con estos tiempos.
        self.inicios[k] = round(self.t0, 3)
        cam = self.camera.frame
        cam.clear_updaters()
        cam.set(width=FW).move_to(ORIGIN)
        if empuje:   # acercamiento lento y continuo: la imagen nunca queda quieta
            t0, Ts = self.t0, self.Tseg
            cam.add_updater(lambda f: f.set(width=FW * (1 - 0.06 * min(1, (self.renderer.time - t0) / Ts))))
        return self.Tseg

    def hasta(self, frac):
        """Espera hasta la fracción indicada de la narración del segmento."""
        resto = self.t0 + frac * self.Tseg - self.renderer.time
        if resto > 1 / 30:
            self.wait(resto)

    def en(self, palabra, n=1, antes=0.25):
        """Espera hasta un poco antes de la n-ésima vez que la voz dice «palabra» en esta escena.
        Si la palabra no está en el guion, la cadena se detiene nombrándola."""
        t = proyecto.t_palabra(PAL, self.k, palabra, n)
        self.anclas.append([self.k, palabra, n, t is not None])
        if t is None:
            hay = proyecto.veces(PAL, self.k, palabra)
            msg = (f"PALABRA ANCLA AUSENTE: la escena {self.k} espera «{palabra}» (vez {n}) y la voz la dice "
                   f"{hay} veces. Corrija el guion o el self.en() de escenas.py.")
            if SOLO_ANCLAS:
                self.faltan.append(msg); return
            raise AssertionError(msg)
        resto = self.t0 + t - antes - self.renderer.time
        if resto < -0.6:
            self.atrasos.append([self.k, palabra, n, round(-resto, 2)])   # la animación llega tarde a su palabra
        if resto > 1 / 30:
            self.wait(resto)

    def falta(self, palabra, n=1, antes=0.25):
        """Segundos que faltan para que la voz diga «palabra» (menos `antes`). Sirve para que una animación
        larga dure justo hasta la palabra siguiente, en vez de un tiempo fijo que la haga llegar tarde."""
        t = proyecto.t_palabra(PAL, self.k, palabra, n)
        assert t is not None or SOLO_ANCLAS, f"PALABRA ANCLA AUSENTE: la escena {self.k} espera «{palabra}» (vez {n})"
        return 1.0 if t is None else self.t0 + t - antes - self.renderer.time

    def fin(self, extra=0.1):
        self.hasta(1.0)
        self.wait(extra)
        cam = self.camera.frame
        cam.clear_updaters()
        for m in self.mobjects:
            for sm in m.get_family():
                sm.clear_updaters()
        fuera = [m for m in self.mobjects if m is not cam]
        if fuera:   # transición: todo sale barrido hacia la izquierda
            self.play(*[FadeOut(m, shift=LEFT * 3) for m in fuera], run_time=0.3, rate_func=rush_into)
        cam.set(width=FW).move_to(ORIGIN)

    def construct(self):
        self.inicios, self.anclas, self.faltan, self.atrasos = {}, [], [], []
        escenas = sorted((m for m in dir(self) if m[0] == "s" and m[1:].split("_")[0].isdigit()),
                         key=lambda m: int(m[1:].split("_")[0]))
        claves = [m.split("_")[0] for m in escenas]
        assert claves == list(DUR), f"Las escenas de escenas.py ({claves}) no coinciden con las del guion ({list(DUR)})"
        for m in escenas:
            getattr(self, m)()
            assert m.split("_")[0] in self.inicios, f"{m} no llamó a self.ini(\"{m.split('_')[0]}\")"
        proyecto.escribir(f"anclas_{FMT}.json", {"usadas": self.anclas, "faltan": self.faltan,
                                                 "atrasos": self.atrasos}, DIR)
        if not SOLO_ANCLAS:
            proyecto.escribir(f"inicios_{FMT}.json", self.inicios, DIR)
