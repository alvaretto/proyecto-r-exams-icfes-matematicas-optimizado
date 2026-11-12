"""Guion del video de <ejercicio> (<código del ítem>).

Lo que DICE la voz. Reglas (las comprueba `--etapa revisar`):
- Números en letras («ciento cincuenta», no «150»).
- Una letra suelta lleva coma después: «cien sobre P, multiplicado por…».
- Las opciones se nombran por su contenido, nunca por letra ni por posición.
- Cada descarte de una opción termina con 1 s de pausa (ver CIERRES_DE_DESCARTE).
- Una errata del ítem oficial no se dice ni se muestra.
"""
NOMBRE = "nombre_del_video"          # los archivos serán NOMBRE_h.mp4 y NOMBRE_v.mp4
VOZ = "es-CO-SalomeNeural"
ENTRADA = {"s1": 0.35}               # la voz del gancho entra justo después del golpe musical
VOL_MUSICA = 0.36                    # perilla del profesor: SOLO él la cambia, oyendo el video

# (texto, velocidad %, tono Hz, pausa después en s). Exclamaciones y preguntas: tono 14-18; explicación: 8-12.
SEG = {
 "s1": [("¡El resultado sorprendente primero!", 6, 18, 0.45),      # 1. gancho: el resultado y el enigma
        ("¿Cómo lo sé?", 8, 16, 0.3)],
 "s2": [("¡Este es el reto!", 14, 16, 0.25)],                       # 2. enunciado y las opciones leídas una a una
 "s3": [("Primero: ¿qué es…?", 12, 16, 0.25)],                      # 3. el concepto, con un dibujo
 "s4": [("Un caso concreto.", 10, 12, 0.9)],                        # 4. dos casos con números
 "s5": [("¿Viste lo que pasó las dos veces?", 16, 16, 0.25)],       # 5. generalización
 "s6": [("La expresión es:", 4, 14, 0.5)],                          # 6. construcción de la respuesta
 "s7": [("¿Y cómo comprobarlo?", 14, 14, 0.3)],                     # 7. comprobación con un número
 "s8": [("Esta no sirve, porque…", 10, 12, 1.0),                    # 8. cada distractor, por su error
        ("Solo una llega al resultado:", 6, 14, 0.5)],
 "s9": [("Tu reto:", 8, 18, 0.3)],                                  # 9. transferencia y reto
}

# (escena, índice de la frase) que cierra el descarte de una opción: su pausa debe ser de 1 s
CIERRES_DE_DESCARTE = [("s8", 0)]

MUSICA = {
    "nivel": {"s1": 2, "s2": 1, "s3": 1, "s4": 2, "s5": 0, "s6": 1, "s7": 2, "s8": 2, "s9": 2},   # 0 tranquilo · 1 medio · 2 completo
    "cambios_nivel": [],            # (escena, palabra, vez, nivel): desde esa palabra, ese nivel
    "sube_tono": "s8",              # desde esta escena, la base sube un tono (o None)
    # Efectos atados a una palabra de la voz. tipo: golpe · subida · campana · zumbido.
    # Si la palabra no está en el guion, la cadena se detiene nombrándola.
    "efectos": [
        # {"tipo": "zumbido", "escena": "s8", "palabra": "porque", "n": 1, "desfase": -0.15, "gan": 0.45},
        # {"tipo": "campana", "escena": "s8", "palabra": "resultado", "desfase": 0.6, "gan": 0.4},
    ],
}
