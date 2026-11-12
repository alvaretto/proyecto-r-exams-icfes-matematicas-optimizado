"""Guion del video «vueltas de una llanta» (MAT-2026-07-028). Prueba de la cadena común:
mismo texto que salida/video/narracion.py."""
NOMBRE = "vueltas_llanta_explicacion"
VOZ = "es-CO-SalomeNeural"
ENTRADA = {"s1": 0.35}        # la voz del gancho entra justo después del golpe musical
VOL_MUSICA = 0.36             # perilla del profesor: se ajusta oyendo

# (texto, velocidad %, tono Hz, pausa después en s)
SEG = {
 "s1": [("¡Setenta y cinco vueltas!", 6, 18, 0.45),
        ("Eso gira una llanta de bici, de doscientos centímetros de borde, en apenas ciento cincuenta metros.", 16, 12, 0.35),
        ("¿Cómo lo sé sin contarlas?", 8, 16, 0.4),
        ("¡Con una sola expresión!", 12, 18, 0.2)],
 "s2": [("¡Este es el reto!", 14, 16, 0.25),
        ("Nos dicen cuántas vueltas da una llanta para recorrer un metro: cien dividido entre su perímetro.", 2, 10, 0.35),
        ("Al perímetro, que es el borde de la llanta, lo llamamos P, y va en centímetros.", 0, 10, 0.4),
        ("La pregunta: ¿cuál de estas cuatro expresiones da las vueltas para recorrer ciento cincuenta metros?", 12, 14, 0.35),
        ("Cien sobre ciento cincuenta.", 8, 10, 0.3),
        ("Ciento cincuenta por P sobre cien.", 8, 12, 0.3),
        ("Cien sobre P, multiplicado por ciento cincuenta.", 8, 10, 0.3),
        ("O cien sobre P.", 8, 12, 0.4),
        ("Y ojo: cien sobre P, es lo mismo que cien dividido entre P.", 0, 10, 0.2)],
 "s3": [("Primero: ¿qué es el perímetro?", 12, 16, 0.25),
        ("¡Es la medida del borde de la llanta!", 8, 14, 0.35),
        ("Mira esto: pintamos el borde y hacemos rodar la llanta una vuelta completa.", 14, 12, 0.9),
        ("¡La marca que deja en el piso mide exactamente el perímetro!", 8, 16, 0.35),
        ("Una vuelta, un perímetro de avance.", 2, 10, 0.2)],
 "s4": [("Ahora pongamos un metro en el piso, o sea, cien centímetros.", 14, 12, 0.3),
        ("Si el perímetro de la llanta es de cincuenta centímetros, ¡necesita dos vueltas para recorrerlo!", 10, 14, 0.9),
        ("Pero una llanta más grande, de doscientos centímetros de perímetro, ¡apenas da media vuelta!", 10, 16, 0.8),
        ("Entre más grande la llanta, menos vueltas.", 2, 10, 0.2)],
 "s5": [("¿Viste lo que pasó las dos veces?", 16, 16, 0.25),
        ("Contamos cuántas veces cabe el perímetro en los cien centímetros del metro.", 6, 10, 0.3),
        ("¡Y eso es dividir!", 10, 18, 0.4),
        ("Cien entre cincuenta: dos.", 8, 12, 0.35),
        ("Cien entre doscientos: cero coma cinco.", 8, 12, 0.45),
        ("Por eso, las vueltas en un metro son cien dividido entre P,", 6, 12, 0.25),
        ("que se escribe cien sobre P.", 2, 10, 0.2)],
 "s6": [("Pero espera: no queremos un metro, ¡queremos ciento cincuenta!", 16, 18, 0.3),
        ("En cada metro la llanta da cien sobre P vueltas.", 0, 10, 0.35),
        ("Entonces, en ciento cincuenta metros da ciento cincuenta veces esa cantidad.", 6, 12, 0.5),
        ("La expresión es:", 4, 14, 0.5),
        ("¡cien sobre P, multiplicado por ciento cincuenta!", 2, 18, 0.2)],
 "s7": [("¿Y cómo comprobarlo? Con la llanta de la bici: doscientos centímetros.", 14, 14, 0.3),
        ("Paso a paso: ciento cincuenta metros son quince mil centímetros.", 8, 10, 0.35),
        ("Quince mil entre doscientos: setenta y cinco vueltas.", 8, 12, 0.45),
        ("Ahora la expresión: cien entre doscientos es cero coma cinco.", 8, 12, 0.3),
        ("Multiplicado por ciento cincuenta:", 6, 12, 0.45),
        ("¡setenta y cinco también! El número del principio.", 10, 18, 0.5),
        ("Pero un solo número no demuestra la fórmula.", 4, 8, 0.3),
        ("La explica la idea: ciento cincuenta metros, y en cada uno, cien sobre P vueltas.", 4, 10, 0.2)],
 "s8": [("¡Un número sí sirve para tumbar fórmulas malas!", 16, 18, 0.25),
        ("Con la llanta de doscientos, tiene que dar setenta y cinco.", 10, 12, 0.4),
        ("Cien sobre ciento cincuenta: no llega ni a una vuelta.", 10, 12, 0.25),
        ("Y ni siquiera usa el perímetro.", 12, 14, 1.0),
        ("Ciento cincuenta por P sobre cien: da trescientas.", 10, 12, 0.25),
        ("No es setenta y cinco.", 6, 10, 0.3),
        ("Tiene la P, arriba: una llanta más grande daría más vueltas. ¡Al revés!", 12, 16, 0.3),
        ("Con más perímetro son menos vueltas: la P, va abajo.", 4, 10, 1.0),
        ("Cien sobre P: da cero coma cinco.", 10, 12, 0.25),
        ("Son las vueltas de un solo metro. Faltó multiplicar por ciento cincuenta.", 12, 12, 1.0),
        ("Solo una expresión llega a setenta y cinco vueltas:", 6, 14, 0.5),
        ("¡cien sobre P, multiplicado por ciento cincuenta!", 2, 18, 0.2)],
 "s9": [("¿Otra distancia? ¡Mismo método!", 16, 18, 0.25),
        ("Para trescientos metros: cien sobre P, multiplicado por trescientos.", 10, 12, 0.35),
        ("Siempre así: el perímetro se mide en centímetros.", -2, 10, 0.5),
        ("La distancia, en metros.", -2, 10, 0.5),
        ("El cien ya hace el cambio de unidad.", 2, 12, 0.55),
        ("Tu reto:", 8, 18, 0.3),
        ("mide alrededor de tu llanta con una cuerda.", 2, 12, 0.3),
        ("De lado a lado no sirve.", 2, 12, 0.3),
        ("¡Y calcula tus vueltas hasta el colegio!", 10, 18, 0.2)],
}

# frases que cierran el descarte de una opción: llevan 1 s de pausa
CIERRES_DE_DESCARTE = [("s8", 3), ("s8", 7), ("s8", 9)]

MUSICA = {
    "nivel": {"s1": 2, "s2": 1, "s3": 1, "s4": 2, "s5": 0, "s6": 1, "s7": 2, "s8": 2, "s9": 2},   # 0 tranquilo, 2 completo
    "cambios_nivel": [("s6", "expresión", 1, 2), ("s5", "por", 1, 1)],   # desde esa palabra, ese nivel
    "sube_tono": "s8",
    "efectos": [
        {"tipo": "subida", "escena": "s6", "palabra": "expresión", "desfase": -1.3, "gan": 0.5},
        {"tipo": "golpe", "escena": "s6", "palabra": "expresión", "gan": 0.7},
        {"tipo": "campana", "escena": "s7", "palabra": "también", "desfase": 0.9, "gan": 0.55},      # «¡Coincide!»
        {"tipo": "zumbido", "escena": "s8", "palabra": "ciento", "n": 2, "desfase": -0.15, "gan": 0.45},  # opciones que caen
        {"tipo": "zumbido", "escena": "s8", "palabra": "cien", "n": 3, "desfase": -0.15, "gan": 0.45},
        {"tipo": "zumbido", "escena": "s8", "palabra": "solo", "n": 2, "desfase": -0.15, "gan": 0.45},
        {"tipo": "subida", "escena": "s8", "palabra": "setenta", "n": 3, "desfase": -0.4, "dur": 1.0, "gan": 0.22},
        {"tipo": "campana", "escena": "s8", "palabra": "setenta", "n": 3, "desfase": 0.6, "gan": 0.4},   # la respuesta
    ],
}
