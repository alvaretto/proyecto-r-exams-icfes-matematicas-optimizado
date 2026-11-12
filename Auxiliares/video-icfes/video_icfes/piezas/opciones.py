"""Las opciones del ítem como tarjetas SIN letras (regla del repo: la respuesta nunca se identifica por
letra ni por posición) y los gestos para enfocarlas, descartarlas y marcar la correcta.
Probado en el video de la llanta (expresiones algebraicas)."""
from manim import *
from ..base import AMARILLO, GRIS, ROJO, VERDE, T


def tarjetas(contenidos, ancho=3.1, alto=1.8, fs=50):
    """`contenidos`: cadenas TeX (se dibujan con MathTex), o mobjects ya hechos (texto, figura).
    Devuelve VGroup de tarjetas; cada tarjeta es VGroup(caja, contenido). Van en el orden del cuadernillo;
    colóquelas con self.repartir(tarjetas, zona): en fila en horizontal y de dos en dos en vertical."""
    g = VGroup()
    for c in contenidos:
        m = MathTex(c, font_size=fs) if isinstance(c, str) else c
        caja = RoundedRectangle(width=ancho, height=alto, corner_radius=0.2, color=GRIS, stroke_width=3)
        if m.width > ancho - 0.3 or m.height > alto - 0.3:
            m.scale(min((ancho - 0.3) / m.width, (alto - 0.3) / m.height))
        g.add(VGroup(caja, m.move_to(caja)))
    return g


def enfocar(tarjeta):
    return [tarjeta[0].animate.set_stroke(AMARILLO, 7)]


def descartar(tarjeta):
    """Borde rojo y contenido atenuado (no más de 0,6: por debajo deja de leerse en celular)."""
    return [tarjeta[0].animate.set_stroke(ROJO, 5), tarjeta[1].animate.set_opacity(0.6)]


def acertar(tarjeta):
    return [tarjeta[0].animate.set_stroke(VERDE, 8), tarjeta[1].animate.set_color(VERDE)]


def motivo(texto, color=ROJO, size=44):
    """Frase corta con el porqué del descarte; va en la zona «pie» y se queda durante la pausa de 1 s."""
    return T(texto, size, color)
