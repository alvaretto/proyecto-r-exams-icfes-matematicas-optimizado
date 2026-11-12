"""Escenas del video de <ejercicio>. Una función por escena del guion (s1 … s9), en orden.

- Todos los números que se ven salen de las constantes de DATOS (instancia canónica del ITEM_OFICIAL.md).
- Posiciones por ZONAS («titulo», «figura», «a», «b», «pie»), para que sirva en horizontal y en vertical;
  donde no baste, hv(valor_horizontal, valor_vertical).
- self.en("palabra", n): espera a que la voz diga esa palabra por n-ésima vez EN ESTA ESCENA.
- Las opciones van sin letras (piezas.opciones.tarjetas) y en el orden del cuadernillo.
"""
from video_icfes.base import *
from video_icfes.piezas import opciones

# ---------- DATOS del ejercicio (instancia canónica) ----------
OPCIONES = [r"\frac{1}{2}", r"\frac{2}{1}", r"1+2", r"1\times 2"]     # en el orden del cuadernillo
I_CLAVE = 0                                                          # índice de la correcta (no se muestra)


class Video(VideoBase):
    def s1_gancho(self):
        self.ini("s1", empuje=False)
        dato = self.ubicar(T("75", 230, AMARILLO, weight=BOLD), "figura")
        self.play(FadeIn(dato, scale=1.4), run_time=0.5)
        self.en("cómo")
        self.play(FadeIn(self.pie("¿Cómo lo sé?", 60)), run_time=0.4)
        self.fin()

    def s2_problema(self):
        self.ini("s2")
        cab = self.titulo("El reto")
        cajas = self.repartir(opciones.tarjetas(OPCIONES), "figura")
        self.play(FadeIn(cab), LaggedStart(*[FadeIn(c, shift=UP * 0.3) for c in cajas], lag_ratio=0.15), run_time=1.0)
        self.fin()

    def s3_concepto(self):
        self.ini("s3"); self.play(FadeIn(self.titulo("El concepto")), run_time=0.4); self.fin()

    def s4_casos(self):
        self.ini("s4"); self.play(FadeIn(self.titulo("Dos casos")), run_time=0.4); self.fin()

    def s5_generalizar(self):
        self.ini("s5"); self.play(FadeIn(self.titulo("La regla")), run_time=0.4); self.fin()

    def s6_respuesta(self):
        self.ini("s6"); self.play(FadeIn(self.titulo("La respuesta")), run_time=0.4); self.fin()

    def s7_comprobacion(self):
        self.ini("s7"); self.play(FadeIn(self.titulo("Comprobemos")), run_time=0.4); self.fin()

    def s8_descartes(self):
        self.ini("s8")
        cab = self.titulo("Probamos todas")
        cajas = self.repartir(opciones.tarjetas(OPCIONES), "figura")
        self.play(FadeIn(cab), FadeIn(cajas), run_time=0.6)
        self.en("porque")
        mot = self.ubicar(opciones.motivo("El porqué del descarte"), "pie")
        self.play(*opciones.descartar(cajas[1]), FadeIn(mot), run_time=0.4)     # el motivo queda durante la pausa
        self.en("solo")
        self.play(FadeOut(mot), *opciones.acertar(cajas[I_CLAVE]), run_time=0.5)
        self.fin(0.4)

    def s9_cierre(self):
        self.ini("s9"); self.play(FadeIn(self.titulo("Tu reto", 70, CIAN)), run_time=0.4); self.fin(1.7)
