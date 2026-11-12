"""Escenas del video «vueltas de una llanta» (MAT-2026-07-028) sobre la base común.
Todos los números que se ven en pantalla salen de las constantes de abajo."""
from video_icfes.base import *
from video_icfes import base as _base

_T_base = _base.T


def T(s, size=40, color=WHITE, ancho=None, **kw):
    """Texto de la base; si trae saltos de línea (solo en vertical), cada línea se centra por separado."""
    if "\n" not in s:
        return _T_base(s, size, color, ancho, **kw)
    return VGroup(*[_T_base(l, size, color, ancho, **kw) for l in s.split("\n")]).arrange(DOWN, buff=0.12 * size / 40)

# ---------- datos del ejercicio (instancia canónica) ----------
K = 100          # centímetros en un metro
D = 150          # metros que pide el ítem
P_CH = 50        # perímetro de la llanta pequeña (cm)
P_GR = 200       # perímetro de la llanta grande (cm), valor de prueba
D2 = 300         # otra distancia para el cierre

V_CH = K / P_CH                # vueltas en 1 m, llanta pequeña
V_GR = K / P_GR                # vueltas en 1 m, llanta grande
CM_TOT = K * D                 # 150 m en cm
V_CLAVE = K / P_GR * D         # expresión correcta con P = 200
V_CONTEO = CM_TOT / P_GR       # conteo directo
assert V_CLAVE == V_CONTEO
V_A = K / D                    # 100/150
V_B = D * P_GR / K             # 150 × P/100
V_D = K / P_GR                 # 100/P
EXPRS = [(rf"\frac{{{K}}}{{{D}}}", V_A), (rf"{D}\times\frac{{P}}{{{K}}}", V_B),
         (rf"\frac{{{K}}}{{P}}\times {D}", V_CLAVE), (rf"\frac{{{K}}}{{P}}", V_D)]
I_CLAVE = 2


def rueda(r, color=WHITE, rayos=8, grosor=8):
    g = VGroup(Circle(radius=r, color=color, stroke_width=grosor))
    for i in range(rayos):
        a = i * TAU / rayos
        g.add(Line(ORIGIN, r * np.array([math.cos(a), math.sin(a), 0]), stroke_width=2.5, color=GRIS))
    g.add(Circle(radius=max(r * 0.1, 0.05), color=color, fill_opacity=1, stroke_width=0))
    g.add(Dot(r * DOWN, color=AMARILLO, radius=max(0.07, r * 0.06)))  # válvula: muestra el giro
    return g


def poner_a_rodar(mob, r, tracker, x0, y):
    """mob construido en el origen; rueda sin deslizar según tracker (distancia)."""
    mob.shift(np.array([x0, y, 0]))
    mob.s_prev = 0.0

    def upd(m):
        s = tracker.get_value()
        ds = s - m.s_prev
        centro = np.array([x0 + m.s_prev, y, 0])
        m.rotate(-ds / r, about_point=centro)
        m.shift(ds * RIGHT)
        m.s_prev = s

    mob.add_updater(upd)
    return mob


def bicicleta(r, tracker, x0, y_piso, color=CIAN):
    """Bicicleta esquemática; x0 = centro de la rueda trasera."""
    sep = 2.9 * r
    yc = y_piso + r
    tras = poner_a_rodar(rueda(r, grosor=6), r, tracker, x0, yc)
    dela = poner_a_rodar(rueda(r, grosor=6), r, tracker, x0 + sep, yc)
    A = np.array([x0, yc, 0])
    B = np.array([x0 + sep, yc, 0])
    C = A + np.array([sep * 0.42, 0, 0])
    S = A + np.array([sep * 0.28, r * 1.35, 0])
    H = B + np.array([-sep * 0.22, r * 1.45, 0])
    kw = dict(color=color, stroke_width=7)
    marco = VGroup(Line(A, C, **kw), Line(C, S, **kw), Line(A, S, **kw), Line(S, H, **kw), Line(C, H, **kw),
                   Line(H, B, **kw), Line(S + LEFT * r * 0.3, S + RIGHT * r * 0.3, color=WHITE, stroke_width=9),
                   Line(H, H + np.array([r * 0.35, r * 0.2, 0]), color=WHITE, stroke_width=7))
    marco.s_prev = 0.0

    def upd(m):
        s = tracker.get_value()
        m.shift((s - m.s_prev) * RIGHT)
        m.s_prev = s

    marco.add_updater(upd)
    return VGroup(tras, dela, marco)



class Video(VideoBase):
    def tarjetas(self, y, fs=50, sep_v=0.25):
        """Las cuatro expresiones del ítem, en el orden del cuadernillo y sin letras.
        En vertical, rejilla de 2 × 2 (sep_v = espacio entre las dos filas)."""
        g = VGroup()
        for e, _ in EXPRS:
            caja = RoundedRectangle(width=hv(3.1, 3.3), height=hv(1.8, 2.3), corner_radius=0.2, color=GRIS, stroke_width=3)
            g.add(VGroup(caja, MathTex(e, font_size=hv(fs, 54))))
        if VERTICAL:
            g.arrange_in_grid(rows=2, cols=2, buff=(0.25, sep_v)).move_to([0, y, 0])
        else:
            g.arrange(RIGHT, buff=0.25).move_to([0, y, 0])
        return g

    # ---------- 1. gancho: el resultado primero, luego el enigma ----------
    def s1_gancho(self):
        self.ini("s1", empuje=False)
        cam = self.camera.frame
        ll = rueda(2.0, grosor=10).move_to([hv(-3.7, 0), hv(0.4, 3.4), 0])
        centro = ll[0].get_center()
        ll.add_updater(lambda m, dt: m.rotate(-7 * dt, about_point=centro))
        n = ValueTracker(0)
        cuenta = always_redraw(lambda: T(f"{int(n.get_value())}", 230, AMARILLO, weight=BOLD).move_to([hv(2.6, 0), hv(0.9, -1.3), 0]))
        vueltas = T("vueltas", 64).move_to([hv(2.6, 0), hv(-1.0, -3.3), 0])
        self.add(ll, cuenta, vueltas)
        cam.set(width=FW * 0.8)
        self.play(n.animate.set_value(V_CLAVE), cam.animate.set(width=FW), run_time=0.6, rate_func=rush_from)
        self.play(Flash(cuenta, color=AMARILLO, line_length=0.6, flash_radius=2.2, num_lines=16), run_time=0.5)
        self.en("llanta")
        borde = T(f"llanta de {P_GR} cm de borde", hv(34, 36), NARANJA).move_to([hv(-3.6, 0), hv(-2.3, 0.7), 0])
        self.play(FadeIn(borde, shift=UP * 0.3), ll[0].animate.set_color(NARANJA), run_time=0.4)
        self.en("apenas")
        dist = T(f"en {D} metros", 54, CIAN).next_to(vueltas, DOWN, buff=0.35)
        self.play(FadeIn(dist, shift=UP * 0.3), run_time=0.4)
        self.en("cómo")
        ll.clear_updaters()
        enigma = T(hv("¿Cómo lo sé sin contarlas?", "¿Cómo lo sé\nsin contarlas?"), hv(72, 68), WHITE, weight=BOLD).move_to(UP * hv(0.5, 0.8))
        self.play(FadeOut(VGroup(ll, borde), shift=LEFT * 3), FadeOut(VGroup(cuenta, vueltas, dist), shift=RIGHT * 3),
                  run_time=0.3)
        self.play(FadeIn(enigma, scale=1.4), run_time=0.35)
        self.en("con")
        resp = T(hv("¡Con una sola expresión!", "¡Con una sola\nexpresión!"), 60, AMARILLO).next_to(enigma, DOWN, buff=0.6)
        self.play(FadeIn(resp, shift=UP * 0.3), cam.animate.set(width=FW * 0.92), run_time=0.5)
        self.fin()

    # ---------- 2. el reto: la regla, la letra P y las cuatro expresiones leídas una por una ----------
    def s2_problema(self):
        self.ini("s2")
        cab = T("El reto", 52, AMARILLO).to_edge(UP, buff=0.5)
        texto = VGroup(T("Vueltas en 1 metro", 46), MathTex("=", font_size=64)).arrange(RIGHT, buff=0.4)
        frac = MathTex(rf"\frac{{{K}}}{{P}}", font_size=80, color=CIAN)
        fila = VGroup(texto, frac).arrange(hv(RIGHT, DOWN), buff=0.4).shift(UP * hv(0.9, 2.4))
        self.play(FadeIn(cab), FadeIn(texto), run_time=0.4)
        self.en("cien")
        self.play(Write(frac), run_time=0.9)
        self.en("perímetro", antes=0.1)
        nota = VGroup(MathTex("P", font_size=64, color=CIAN),
                      T(hv("= perímetro = borde de la llanta (en cm)", "= perímetro = borde\nde la llanta (en cm)"), 40)) \
            .arrange(RIGHT, buff=0.3).next_to(fila, DOWN, buff=hv(0.8, 0.6))
        self.play(FadeIn(nota, shift=UP * 0.2), Indicate(frac, color=CIAN), run_time=0.8)
        self.en("pregunta")
        chico = VGroup(T("En 1 metro:", 40, GRIS_CLARO), MathTex(rf"\frac{{{K}}}{{P}}", font_size=48, color=CIAN)) \
            .arrange(RIGHT, buff=0.3).next_to(cab, DOWN, buff=0.3)
        preg = T(hv(f"¿Cuál da las vueltas en {D} metros?", f"¿Cuál da las vueltas\nen {D} metros?"), 46).move_to(UP * hv(0.85, 3.9))
        cajas = self.tarjetas(hv(-0.95, 0.2))
        # primero sale la frase anterior y solo después entra la pregunta (antes se cruzaban encimadas)
        self.play(FadeOut(nota), ReplacementTransform(fila, chico), run_time=0.45)
        self.play(FadeIn(preg), run_time=0.3)
        self.play(LaggedStart(*[FadeIn(c, shift=UP * 0.3) for c in cajas], lag_ratio=0.15), run_time=1.0)
        lectura = [("cien", 2), ("ciento", 3), ("cien", 4), ("cien", 5)]

        def marca(c, leida):
            # Un solo movimiento por tarjeta: con dos «animate» a la vez (borde y tamaño), el del tamaño
            # anulaba el del borde y el amarillo no se veía.
            c.generate_target()
            c.target.scale(1.08 if leida else 1 / 1.08)
            c.target[0].set_stroke(AMARILLO if leida else GRIS, 7 if leida else 3)
            return MoveToTarget(c)

        ant = None
        for c, (pal, n) in zip(cajas, lectura):
            self.en(pal, n)
            anims = [marca(c, True)]
            if ant is not None:
                anims.append(marca(ant, False))
            self.play(*anims, run_time=0.3)
            ant = c
        self.en("ojo")
        lee = MathTex(rf"\frac{{{K}}}{{P}}", "=", rf"{K}\div P", font_size=60, color=CIAN).move_to(DOWN * hv(2.85, 3.8))
        self.play(marca(ant, False), FadeIn(lee, shift=UP * 0.2),
                  run_time=0.5)
        self.fin()

    # ---------- 3. simulación 1: una vuelta = un perímetro ----------
    def s3_perimetro(self):
        Ts = self.ini("s3", empuje=False)
        cam = self.camera.frame
        y = hv(-2.4, 0.0)
        r = hv(1.05, 0.8)
        x0 = -TAU * r / 2
        piso = Line(LEFT * 7, RIGHT * 7, color=GRIS, stroke_width=4).shift(UP * y)
        cab = T(hv("Perímetro: la medida del borde", "Perímetro: la\nmedida del borde"), 50, AMARILLO).to_edge(UP, buff=0.6)
        s = ValueTracker(0)
        ll = poner_a_rodar(rueda(r), r, s, x0, y + r)
        borde = always_redraw(lambda: Arc(radius=r, start_angle=-PI / 2, angle=max(TAU - s.get_value() / r, 0.001),
                                          arc_center=[x0 + s.get_value(), y + r, 0], color=NARANJA, stroke_width=14))
        marca = always_redraw(lambda: Line([x0, y, 0], [x0 + max(s.get_value(), 0.001), y, 0],
                                           color=NARANJA, stroke_width=14))
        self.play(FadeIn(cab), Create(piso), FadeIn(ll), run_time=0.4)
        if VERTICAL:   # el título acompaña a la cámara mientras se acerca (en horizontal queda fuera de cuadro)
            w_cab = cab.width
            cab.add_updater(lambda m: m.set(width=w_cab * cam.width / FW).move_to(
                cam.get_center() + UP * (cam.height / 2 - 1.2 * cam.width / FW)))
        self.en("pintamos")
        self.play(Create(borde), cam.animate.set(width=FW * hv(0.5, 0.55)).move_to([x0, y + r + 0.2, 0]), run_time=1.6)
        self.add(marca)
        self.en("rodar")
        cam.add_updater(lambda f: f.move_to([x0 + s.get_value(), y + r + 0.2, 0]))
        self.play(s.animate.set_value(TAU * r), run_time=2.4, rate_func=smooth)
        cam.clear_updaters()
        self.play(cam.animate.set(width=FW).move_to(ORIGIN), run_time=0.7)
        cab.clear_updaters()
        llave = Brace(Line([x0, y, 0], [x0 + TAU * r, y, 0]), DOWN, color=NARANJA)
        etq = MathTex("P", font_size=72, color=NARANJA).next_to(llave, DOWN, buff=0.15)
        self.play(GrowFromCenter(llave), FadeIn(etq), run_time=0.9)
        self.en("una", 2)
        concl = T(hv("1 vuelta = avanza un perímetro", "1 vuelta = avanza\nun perímetro"), 46).next_to(cab, DOWN, buff=0.55)
        self.play(FadeIn(concl, shift=UP * 0.2), run_time=0.8)
        self.fin()

    # ---------- 4. simulación 2: un metro, dos llantas ----------
    def s4_un_metro(self):
        Ts = self.ini("s4")
        y = hv(-2.9, 1.4)
        L = hv(7.0, 4.2)                      # unidades de pantalla para 100 cm
        u = L / K
        xa = -L / 2
        cab = T(f"Un metro = {K} cm", 50, AMARILLO).to_edge(UP, buff=hv(0.45, 0.55))
        regla = VGroup(Line([xa, y, 0], [xa + L, y, 0], color=WHITE, stroke_width=6))
        for cm in range(0, K + 1, 10):
            h = 0.28 if cm % 50 == 0 else 0.16
            regla.add(Line([xa + cm * u, y, 0], [xa + cm * u, y - h, 0], color=WHITE, stroke_width=3))
        for cm in (0, 50, K):
            regla.add(T(f"{cm}", 32, GRIS_CLARO).move_to([xa + cm * u, y - 0.58, 0]))
        self.play(FadeIn(cab), Create(regla), run_time=1.6)

        def corrida(P, col, pos_etq, palabra, t_dur):
            r = P * u / TAU
            s = ValueTracker(0)
            ll = poner_a_rodar(rueda(r, rayos=8 if P > 100 else 6), r, s, xa, y + r)
            rastro = always_redraw(lambda: Line([xa, y, 0], [xa + max(s.get_value(), 0.001), y, 0],
                                                color=col, stroke_width=12))
            etq = T(f"P = {P} cm", hv(38, 44), col).move_to(pos_etq)
            vt = T("Vueltas:", hv(38, 44)).next_to(etq, DOWN, buff=0.22).shift(LEFT * 0.45)
            cont = always_redraw(lambda: T(f"{s.get_value() / (P * u):.1f}".replace(".", ","), hv(44, 50), col)
                                 .next_to(vt, RIGHT, buff=0.25))
            self.en(palabra)
            self.play(FadeIn(ll), FadeIn(etq), FadeIn(vt), run_time=0.8)
            self.add(rastro, cont)
            self.play(s.animate.set_value(L), run_time=t_dur, rate_func=smooth)
            final = T(num(K / P, tex=False), hv(44, 50), col).next_to(vt, RIGHT, buff=0.25)
            cont.clear_updaters()
            self.remove(cont)
            self.add(final)
            self.play(Indicate(final, color=col, scale_factor=1.4), run_time=0.8)
            ll.clear_updaters()
            rastro.clear_updaters()
            return ll, rastro

        ll1, ra1 = corrida(P_CH, CIAN, [hv(-4.7, 0), hv(2.75, -1.8), 0], "si", 2.6)
        self.en("pero", antes=0.8)
        self.play(FadeOut(ll1), FadeOut(ra1), run_time=0.6)
        corrida(P_GR, NARANJA, [hv(4.7, 0), hv(2.75, -4.1), 0], "pero", 2.2)
        self.en("entre")
        concl = T(hv("Más grande, menos vueltas", "Más grande,\nmenos vueltas"), 50, AMARILLO).move_to(cab).shift(DOWN * hv(0, 0.4))
        self.play(FadeOut(cab), run_time=0.3)
        self.play(FadeIn(concl), run_time=0.4)
        self.fin()

    # ---------- 5. ¿cuántas veces cabe el perímetro en el metro? eso es dividir ----------
    def s5_expresion(self):
        self.ini("s5")
        u = hv(4.0, 3.65) / K                         # 100 cm = 4 unidades (3,65 en vertical: 200 cm deben caber)
        xa = hv(-6.3, -3.65)
        cab = T(hv("¿Cuántas veces cabe el perímetro?", "¿Cuántas veces cabe\nel perímetro?"), 48, AMARILLO).to_edge(UP, buff=hv(0.45, 0.55))
        self.play(FadeIn(cab), run_time=0.5)

        def barra(largo_cm, y, col, rot):
            b = Rectangle(width=largo_cm * u, height=0.5, color=col, stroke_width=3, fill_color=col, fill_opacity=0.25)
            b.move_to([xa + largo_cm * u / 2, y, 0])
            return VGroup(b, T(rot, 32, col).move_to(b))

        m1 = barra(K, hv(2.2, 3.9), WHITE, f"1 metro = {K} cm")
        ch = VGroup(*[barra(P_CH, hv(1.55, 3.1), CIAN, f"{P_CH}").shift(RIGHT * i * P_CH * u) for i in range(int(V_CH))])
        f1 = MathTex(f"{K}", r"\div", f"{P_CH}", "=", num(V_CH), font_size=70).move_to([hv(3.4, 0), hv(1.9, 2.0), 0])
        f1[2].set_color(CIAN); f1[4].set_color(CIAN)
        m2 = barra(K, hv(0.2, 0.6), WHITE, f"1 metro = {K} cm")
        gr = barra(P_GR, hv(-0.45, -0.2), NARANJA, f"{P_GR}")
        gr[1].shift(LEFT * hv(2.5, 2.0))
        corte = DashedLine([xa + K * u, hv(0.6, 1.0), 0], [xa + K * u, hv(-0.9, -0.65), 0], color=WHITE)
        f2 = MathTex(f"{K}", r"\div", f"{P_GR}", "=", num(V_GR), font_size=70).move_to([hv(3.7, 0), hv(0.5, -1.3), 0])
        f2[2].set_color(NARANJA); f2[4].set_color(NARANJA)
        self.play(FadeIn(m1), run_time=0.4)
        self.en("cabe", antes=0.9)
        self.play(LaggedStart(*[FadeIn(c, shift=RIGHT * 0.4) for c in ch], lag_ratio=0.5), run_time=1.0)
        self.en("cien", 2)
        self.play(Write(f1), run_time=1.0)
        self.en("cien", 3)
        self.play(FadeIn(m2), FadeIn(gr, shift=RIGHT * 0.4), Create(corte), run_time=0.8)
        self.play(Write(f2), run_time=1.0)
        self.en("por")
        rot = T("Vueltas en 1 metro", 44)
        g1 = MathTex("=", rf"{K}\div P", font_size=60, color=AMARILLO)
        g2 = MathTex("=", rf"\frac{{{K}}}{{P}}", font_size=68, color=AMARILLO)
        if VERTICAL:
            gen = VGroup(rot, VGroup(g1, g2).arrange(RIGHT, buff=0.4)).arrange(DOWN, buff=0.35).move_to([0, -4.0, 0])
        else:
            gen = VGroup(rot, g1, g2).arrange(RIGHT, buff=0.3).move_to([0, -2.35, 0])
        self.play(FadeIn(rot), Write(g1), run_time=1.0)
        self.en("escribe")
        self.play(Write(g2), run_time=0.8)
        marco = SurroundingRectangle(gen, color=AMARILLO, buff=0.22, corner_radius=0.2)
        self.play(Create(marco), run_time=0.5)
        self.fin()

    # ---------- 6. simulación 3: 150 metros ----------
    def s6_ciento_cincuenta(self):
        Ts = self.ini("s6", empuje=False)
        cam = self.camera.frame
        cab = T(hv(f"No es 1 metro: son {D} metros", f"No es 1 metro:\nson {D} metros"), 50, AMARILLO).to_edge(UP, buff=hv(0.4, 0.2))
        n = hv(7, 4)          # en vertical caben 4 bloques y los puntos suspensivos
        w = hv(1.6, 1.45)
        bloques = VGroup()
        for i in range(n):
            caja = Rectangle(width=w, height=1.3, color=CIAN, stroke_width=3, fill_color=CIAN, fill_opacity=0.12)
            bloques.add(VGroup(caja, MathTex(rf"\frac{{{K}}}{{P}}", font_size=44).move_to(caja),
                               T("1 m", 36, WHITE).next_to(caja, DOWN, buff=0.12)))
        bloques.arrange(RIGHT, buff=0).move_to([-0.6, 0.75, 0])
        puntos = MathTex(r"\cdots", font_size=hv(90, 70)).next_to(bloques[-1][0], RIGHT, buff=0.3)
        if VERTICAL:
            VGroup(bloques, puntos).move_to([0, 0.9, 0])
        s = ValueTracker(0)
        bici = bicicleta(0.34, s, bloques.get_left()[0] + 0.1, bloques[0][0].get_top()[1], color=NARANJA)
        self.play(FadeIn(cab), FadeIn(bloques[0]), FadeIn(bici), run_time=0.4)
        w_cab = cab.width     # el título (con el 150) sigue a la cámara: nunca sale del cuadro
        cab.add_updater(lambda m: m.set(width=w_cab * cam.width / FW).move_to(
            cam.get_center() + UP * (cam.height / 2 - hv(0.6, 0.95) * cam.width / FW)))
        self.en("en", antes=0.5)
        self.play(cam.animate.set(width=FW * 0.65).move_to(bloques[0]), run_time=0.8)
        self.en("entonces")   # el recorrido termina antes de «veces» (antes llegaba 1,7 s tarde)
        self.play(LaggedStart(*[FadeIn(b, shift=RIGHT * 0.3) for b in bloques[1:]], FadeIn(puntos), lag_ratio=0.3),
                  s.animate.set_value(n * w - 1.4), cam.animate.move_to(bloques[-1]),
                  run_time=max(1.5, self.falta("veces") - 0.5), rate_func=linear)
        self.play(cam.animate.set(width=FW).move_to(ORIGIN), run_time=0.5)
        self.en("veces")
        llave = Brace(VGroup(bloques, puntos), DOWN, buff=0.1, color=WHITE)
        veces = T(f"{D} veces", 42).next_to(llave, DOWN, buff=0.1)
        self.play(GrowFromCenter(llave), FadeIn(veces), run_time=0.7)
        self.en("expresión")
        expr = MathTex(rf"\frac{{{K}}}{{P}}", r"\times", f"{D}", font_size=72, color=AMARILLO).move_to([0, hv(-2.75, -3.3), 0])
        marco = SurroundingRectangle(expr, color=AMARILLO, buff=0.2, corner_radius=0.2)
        self.play(FadeOut(bici), Write(expr), run_time=1.2)
        self.play(Create(marco), run_time=0.5)
        self.fin()

    # ---------- 7. comprobación con un número (comprueba; no demuestra) ----------
    def s7_comprobacion(self):
        self.ini("s7")
        ll = rueda(0.42, color=NARANJA, rayos=6, grosor=5)
        cab = VGroup(ll, T(hv(f"Probemos con P = {P_GR} cm", f"Probemos con\nP = {P_GR} cm"), 50, AMARILLO)) \
            .arrange(RIGHT, buff=0.35).to_edge(UP, buff=hv(0.4, 0.5))
        tA = T("Paso a paso", 40, CIAN)
        a1 = MathTex(rf"{D}\ \text{{m}} = {num(CM_TOT)}\ \text{{cm}}", font_size=56)
        a2 = MathTex(rf"{num(CM_TOT)} \div {P_GR} =", num(V_CONTEO), font_size=56)
        izq = VGroup(tA, a1, a2).arrange(DOWN, buff=0.5).move_to([hv(-3.5, 0), hv(0.5, 3.6), 0])
        tB = T("Con la expresión", 40, NARANJA)
        b1 = MathTex(rf"\frac{{{K}}}{{{P_GR}}}", rf"\times {D}", font_size=60)
        b2 = MathTex(rf"{num(V_GR)}", rf"\times {D} =", num(K / P_GR * D), font_size=56)
        der = VGroup(tB, b1, b2).arrange(DOWN, buff=0.45).move_to([hv(3.5, 0), hv(0.5, -0.2), 0])
        sep = DashedLine(hv(UP * 2.2, LEFT * 3.4 + UP * 1.85), hv(DOWN * 1.3, RIGHT * 3.4 + UP * 1.85), color=GRIS)
        grande = rueda(1.25, color=NARANJA, rayos=8, grosor=9).move_to([0, hv(0.3, 1.3), 0])
        c_gr = grande[0].get_center()
        grande.add_updater(lambda m, dt: m.rotate(-3 * dt, about_point=c_gr))
        r_gr = T(f"{P_GR} cm de borde", 44, NARANJA).move_to([0, hv(-1.7, -0.7), 0])
        self.play(FadeIn(cab), FadeIn(grande), run_time=0.4)
        self.en("doscientos")
        self.play(FadeIn(r_gr, shift=UP * 0.2), run_time=0.3)
        self.en("paso", antes=0.45)
        grande.clear_updaters()
        self.play(FadeOut(grande), FadeOut(r_gr), FadeIn(tA), run_time=0.35)
        self.en("ciento")
        self.play(FadeIn(a1, shift=UP * 0.2), run_time=0.6)
        self.en("quince", 2)
        self.play(Write(a2), run_time=1.2)
        self.en("ahora")
        self.play(Create(sep), FadeIn(tB), run_time=0.4)
        self.en("cien")
        self.play(Write(b1), run_time=0.8)
        self.en("cero")
        self.play(Indicate(b1[0], color=NARANJA), FadeIn(b2[0], shift=UP * 0.2), run_time=0.6)
        self.en("por")
        self.play(FadeIn(b2[1]), run_time=0.4)
        self.en("setenta", 2)
        self.play(FadeIn(b2[2], scale=1.6), a2[1].animate.set_color(VERDE), b2[2].animate.set_color(VERDE), run_time=0.4)
        ok = T(f"¡Coincide! {num(V_CLAVE, tex=False)} vueltas", hv(52, 44), VERDE).move_to([0, hv(-2.4, -3.0), 0])
        self.play(FadeIn(ok, scale=1.6), Flash(ok, color=VERDE, flash_radius=3.6, line_length=0.5, num_lines=18),
                  run_time=0.6)
        self.en("pero")
        self.play(FadeOut(ok), run_time=0.3)
        aviso = T(hv("Un número comprueba, no demuestra", "Un número comprueba,\nno demuestra"), 42, ROJO).move_to([0, hv(-1.75, -2.8), 0])
        self.play(FadeIn(aviso), run_time=0.4)
        self.en("idea")
        if VERTICAL:
            idea = VGroup(T(f"La idea: {D} metros,\ncada uno con", 40),
                          VGroup(MathTex(rf"\frac{{{K}}}{{P}}", font_size=44, color=AMARILLO), T("vueltas", 40))
                          .arrange(RIGHT, buff=0.25)).arrange(DOWN, buff=0.15).move_to([0, -5.0, 0])
        else:
            idea = VGroup(T(f"La idea: {D} metros, cada uno con", 40), MathTex(rf"\frac{{{K}}}{{P}}", font_size=56, color=AMARILLO),
                          T("vueltas", 40)).arrange(RIGHT, buff=0.25).move_to([0, -2.85, 0])
        self.play(FadeIn(idea, shift=UP * 0.2), run_time=0.5)
        self.fin()

    # ---------- 8. un número sí descarta: las cuatro tarjetas otra vez, con la meta a la vista ----------
    def s8_descartes(self):
        self.ini("s8")
        cab = T(hv(f"Probamos todas con P = {P_GR} cm", f"Probamos todas con\nP = {P_GR} cm"), 48, AMARILLO).to_edge(UP, buff=hv(0.4, 0.55))
        cajas = self.tarjetas(hv(0.75, 0.0), sep_v=hv(0.25, 1.25))
        self.play(FadeIn(cab), LaggedStart(*[FadeIn(c) for c in cajas], lag_ratio=0.1), run_time=0.9)
        self.en("setenta")
        meta = T(f"Debe dar {num(V_CLAVE, tex=False)} vueltas", 46, VERDE).next_to(cab, DOWN, buff=hv(0.3, 0.5))
        marco_meta = SurroundingRectangle(meta, color=VERDE, buff=0.15, corner_radius=0.15, stroke_width=3)
        self.play(FadeIn(meta, scale=1.3), Create(marco_meta), run_time=0.5)

        def valor(i, col):
            v = EXPRS[i][1]
            signo = "=" if es_exacto(v) else r"\approx"
            return MathTex(signo, num(round(v, 2)), font_size=60, color=col).next_to(cajas[i], DOWN, buff=0.3)

        def motivo(txt, col=ROJO):
            return T(txt, hv(44, 40), col).move_to([0, hv(-2.6, -4.6), 0])

        def cambiar(viejo, nuevo):
            self.play(FadeOut(viejo), run_time=0.2)
            self.play(FadeIn(nuevo), run_time=0.3)

        def tumbar(i, val, mot):
            self.play(cajas[i][0].animate.set_stroke(ROJO, 5), cajas[i][1].animate.set_opacity(0.6),
                      val.animate.set_color(ROJO), FadeOut(mot), run_time=0.4)

        def foco(i):
            self.play(cajas[i][0].animate.set_stroke(AMARILLO, 7), run_time=0.25)

        # 100/150
        self.en("cien")
        foco(0)
        self.en("no")
        v0 = valor(0, WHITE)
        m0 = motivo("Ni una vuelta: no es 75")
        self.play(FadeIn(v0), FadeIn(m0), run_time=0.4)
        self.en("siquiera")
        m0b = motivo("Y no usa el perímetro")
        cambiar(m0, m0b)
        # 150 × P/100
        self.en("ciento", 2)
        tumbar(0, v0, m0b)
        foco(1)
        self.en("trescientas")
        v1 = valor(1, WHITE)
        self.play(FadeIn(v1), run_time=0.4)
        self.en("no", 2)
        m1 = motivo(f"{num(V_B, tex=False)} no es {num(V_CLAVE, tex=False)}")
        self.play(FadeIn(m1), run_time=0.3)
        self.en("arriba")
        m1a = motivo(hv("P arriba: más llanta, más vueltas. ¡Al revés!", "P arriba: más llanta,\nmás vueltas. ¡Al revés!"))
        cambiar(m1, m1a)
        p_arriba = cajas[1][1][0][len(str(D)) + 1]      # la P de «D × P/K»
        self.play(p_arriba.animate.set_color(AMARILLO).scale(1.5), run_time=0.4)
        self.play(p_arriba.animate.scale(1 / 1.5), run_time=0.3)
        self.en("con", 2)
        m1b = motivo(hv("Más perímetro, menos vueltas: P va abajo", "Más perímetro, menos\nvueltas: P va abajo"), CIAN)
        cambiar(m1a, m1b)
        # 100/P
        self.en("cien", 3)
        tumbar(1, v1, m1b)
        foco(3)
        self.en("cero")
        v3 = valor(3, WHITE)
        self.play(FadeIn(v3), run_time=0.4)
        self.en("solo")
        m3 = motivo(hv(f"Es un solo metro: faltó × {D}", f"Es un solo metro:\nfaltó × {D}"))
        self.play(FadeIn(m3), run_time=0.4)
        # la correcta
        self.en("solo", 2)
        tumbar(3, v3, m3)
        self.en("cien", 4)
        v2 = valor(I_CLAVE, VERDE)
        self.play(cajas[I_CLAVE][0].animate.set_stroke(VERDE, 8), cajas[I_CLAVE][1].animate.set_color(VERDE),
                  FadeIn(v2), run_time=0.5)
        self.play(VGroup(cajas[I_CLAVE], v2).animate.scale(1.12),
                  Flash(v2, color=VERDE, flash_radius=1.6, line_length=0.4, num_lines=14), run_time=0.5)
        self.fin(0.4)

    # ---------- 9. cierre corto: regla general con unidades y reto ----------
    def s9_cierre(self):
        self.ini("s9")
        cab = T(hv("Otra distancia, el mismo método", "Otra distancia,\nel mismo método"), 50, AMARILLO).to_edge(UP, buff=0.5)
        e1 = MathTex(rf"\frac{{{K}}}{{P}}", r"\times", f"{D}", font_size=96).shift(UP * hv(0.9, 2.0))
        e2 = MathTex(rf"\frac{{{K}}}{{P}}", r"\times", f"{D2}", font_size=96).shift(UP * hv(0.9, 2.0))
        e2[2].set_color(CIAN)
        d2 = T(f"{D2} metros", 40, CIAN).next_to(e2, DOWN, buff=0.4)
        self.play(FadeIn(cab), FadeIn(e1), run_time=0.5)
        self.en("trescientos")
        self.play(TransformMatchingTex(e1, e2), FadeIn(d2), run_time=0.7)
        self.en("siempre")
        gen = VGroup(MathTex(rf"\frac{{{K}}}{{P}}", r"\times", font_size=hv(96, 84)), T("distancia", hv(64, 56), CIAN)).arrange(RIGHT, buff=0.35)
        gen.shift(UP * hv(0.9, 2.0))
        uP = T("P en centímetros", 40, NARANJA).move_to([hv(-3.2, 0), hv(-1.2, -0.3), 0])
        uD = T("distancia en metros", 40, CIAN).move_to([hv(3.2, 0), hv(-1.2, -1.3), 0])
        self.play(FadeOut(e2), FadeOut(d2), run_time=0.25)
        self.play(FadeIn(gen), run_time=0.3)
        self.en("perímetro")
        self.play(FadeIn(uP, shift=UP * 0.2), run_time=0.3)
        self.en("distancia", 2)
        self.play(FadeIn(uD, shift=UP * 0.2), run_time=0.3)
        self.en("cien", 2)      # «El cien ya hace el cambio de unidad»: se señala el 100 de la expresión
        # el 100 queda en amarillo y más grande mientras dura la frase (sale con el resto en «reto»)
        self.play(gen[0][0][:len(str(K))].animate.set_color(AMARILLO).scale(1.3, about_edge=DOWN), run_time=0.35)
        self.en("reto")
        self.play(FadeOut(VGroup(cab, gen, uP, uD)), run_time=0.3)
        titulo = T("Tu reto", 70, CIAN, weight=BOLD).move_to(UP * hv(2.3, 4.2))
        ll = rueda(1.0, grosor=7).move_to([hv(-4.8, 0), hv(-0.3, 1.6), 0])
        cuerda = Circle(radius=1.16, color=NARANJA, stroke_width=9).move_to(ll[0].get_center())
        l1 = T(hv("Mide ALREDEDOR de tu llanta, en cm", "Mide ALREDEDOR de tu\nllanta, en cm"), 38).move_to([hv(1.9, 0), hv(0.4, -0.9), 0])
        l1b = T(hv("(con una cuerda; no de lado a lado)", "(con una cuerda;\nno de lado a lado)"), 34, NARANJA).next_to(l1, DOWN, buff=0.25)
        l2 = T(hv("y calcula tus vueltas hasta el colegio", "y calcula tus vueltas\nhasta el colegio"), 38).next_to(l1b, DOWN, buff=0.5)
        self.play(FadeIn(titulo, scale=1.3), FadeIn(ll), run_time=0.4)
        self.en("mide", 2)      # la vez 1 es «se mide en centímetros»
        self.play(Create(cuerda), FadeIn(l1), run_time=0.9)
        self.en("no")
        self.play(FadeIn(l1b), run_time=0.3)
        self.en("calcula")
        self.play(FadeIn(l2, shift=UP * 0.2), run_time=0.4)
        self.fin(1.7)
