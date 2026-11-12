"""Escenas de muestra de las piezas (sin narración): sirven para mirar el último fotograma en los dos formatos.
Uso: manim render -s ... muestra.py MuestraEstadistica   (con VIDEO_FORMATO=h o v)"""
from video_icfes.base import *
from video_icfes.piezas import estadistica, geometria, probabilidad


class MuestraEstadistica(VideoBase):
    def construct(self):
        self.add(self.titulo("Ventas de la semana"))
        cats = ["Lunes", "Martes", "Miércoles", "Jueves", "Viernes"]
        vals = [12, 18, 15, 22.5, 9]
        w = hv(6.2, 7.2)
        b = estadistica.barras(cats, vals, ancho=w, alto=hv(4.5, 4.8), paso_eje=5,
                               titulo_eje=hv(None, "Unidades"))
        ref = estadistica.linea_referencia(b, sum(vals) / len(vals), "Promedio")
        b_ref = VGroup(b, ref)
        l = estadistica.grafico_lineas([2019, 2020, 2021, 2022, 2023], [1200, 1850, 1500, 2300, 2150],
                                       ancho=w, alto=hv(4.5, 3.6), mostrar_valores=True)
        if VERTICAL:
            t = estadistica.tabla(["Mes", "Ventas"], [["Enero", 1200], ["Febrero", 1850.5], ["Marzo", 1500]],
                                  ancho=6.0)
        else:
            t = estadistica.tabla(["Enero", "Febrero", "Marzo"], [[1200, 1850.5, 1500]], ancho=9.0)
        estadistica.resaltar_celda(t, 0, 1, animar=False)
        l.partes[("punto", 3)].set_color(AMARILLO)
        b.partes["Jueves"].set_color(AMARILLO)
        self.add(b_ref, l, t)
        if VERTICAL:
            self.apilar([b_ref, l, t], "todo", buff=0.3).shift(DOWN * 0.6)
        else:
            self.repartir([b_ref, l], "figura", buff=0.4)
            self.ubicar(t, "pie")


class MuestraGeometria(VideoBase):
    def construct(self):
        self.add(self.titulo("Figuras y medidas"))
        tri = geometria.triangulo((0, 0), (6, 0), (2.5, 3.2), rotulos_lados=["6 cm", "4,7 cm", "4,1 cm"],
                                  rotulos_angulos=["52°", "42°", "86°"], ancho=hv(5.2, 7.0), alto=hv(4.4, 3.4),
                                  vertices=["P", "Q", "R"])
        rec = geometria.rectangulo_medidas(8, 5, ancho=hv(3.5, 3.4), alto=hv(2.4, 2.6), unidad="m")
        pl = geometria.plano((0, 8), (0, 6), 1, ancho=hv(4.6, 4.0), alto=hv(4.4, 4.0))
        circ = geometria.circulo_radio(5, ancho=hv(3.5, 3.1), alto=hv(2.1, 2.9))
        if VERTICAL:
            self.ubicar(tri, "a")
            VGroup(pl, circ).arrange(RIGHT, buff=0.2).move_to([0, -1.0, 0])
            rec.move_to([0, -5.0, 0])
        else:
            col = VGroup(rec, circ).arrange(DOWN, buff=0.2)
            self.ubicar(VGroup(tri, pl, col).arrange(RIGHT, buff=0.2), "figura")
        p1 = geometria.punto(pl, 2, 2, "(2, 2)", direccion=DR)
        p2 = geometria.punto(pl, 7, 5, "(7, 5)", direccion=UL, color=VERDE)
        sg = geometria.segmento(pl, (2, 2), (7, 5), "5,8")
        self.add(tri, rec, pl, circ, sg, p1, p2)


class MuestraProbabilidad(VideoBase):
    def construct(self):
        self.add(self.titulo("Conteo y probabilidad"))
        td = probabilidad.tabla_doble_entrada(["Mujeres", "Hombres"], ["Fútbol", "Música", "Arte"],
                                              [[12, 8, 5], [15, 6, 4]], ancho=hv(5.8, 7.2))
        from video_icfes.piezas.estadistica import resaltar_celda
        resaltar_celda(td, ("total_fila", 0), animar=False)
        v = probabilidad.venn2("Juegos de mesa", "Videojuegos", 14, 9, 7, fuera=5, ancho=hv(5.6, 7.2),
                               alto=hv(3.8, 3.9))
        probabilidad.colorear_region(v, "ambos", AMARILLO, 0.5, animar=False)
        cas = probabilidad.casillas(4, ["3", None, "5", None], ancho=hv(5.6, 7.0))
        fi = probabilidad.fichas([(ROJO, 5, "rojas"), (CIAN, 3, "azules"), (VERDE, 2, "verdes")],
                                 ancho=hv(6.2, 7.0))
        self.add(cas, fi, td, v)
        if VERTICAL:
            self.ubicar(td, "a")
            self.ubicar(v, "b")
            g = VGroup(cas, fi).arrange(DOWN, buff=0.4)
            self.ubicar(g, "pie")
        else:
            izq = VGroup(td, cas).arrange(DOWN, buff=0.5)
            der = VGroup(v, fi).arrange(DOWN, buff=0.4)
            g = VGroup(izq, der).arrange(RIGHT, buff=0.5)
            self.ubicar(g, "figura")
