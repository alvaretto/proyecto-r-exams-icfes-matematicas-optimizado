"""Narración por FRASES (edge-tts): cada frase lleva su velocidad, su tono y la pausa que la sigue,
para que la voz no suene pareja. Guarda por escena el audio, los tiempos de cada palabra (subtítulos
y sincronía de las animaciones) y la duración útil."""
import asyncio, json, pathlib, subprocess
import edge_tts

VOZ = "es-CO-SalomeNeural"
AQUI = pathlib.Path(__file__).parent / "audio"
FR = AQUI / "frases"
FR.mkdir(parents=True, exist_ok=True)
ENTRADA = {"s1": 0.35}        # la voz del gancho entra justo después del golpe musical

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


async def frase(texto, vel, tono, ruta):
    com = edge_tts.Communicate(texto, VOZ, rate=f"{vel:+d}%", pitch=f"{tono:+d}Hz", boundary="WordBoundary")
    ws = []
    with open(ruta, "wb") as f:
        async for ch in com.stream():
            if ch["type"] == "audio":
                f.write(ch["data"])
            elif ch["type"] == "WordBoundary":
                ws.append([ch["text"], ch["offset"] / 1e7, ch["duration"] / 1e7])
    return ws


async def main():
    dur, palabras = {}, {}
    for k, frases in SEG.items():
        pos = ENTRADA.get(k, 0.0)
        cmd, filtros, todas = ["ffmpeg", "-y", "-v", "error"], [], []
        for i, (texto, vel, tono, pausa) in enumerate(frases):
            ruta = FR / f"{k}_{i:02d}.mp3"
            ws = await frase(texto, vel, tono, ruta)
            a = max(0.0, ws[0][1] - 0.04)                    # se recorta el silencio de entrada y de cola
            b = ws[-1][1] + ws[-1][2] + 0.10
            cmd += ["-i", str(ruta)]
            filtros.append(f"[{i}:a]atrim=start={a:.3f}:end={b:.3f},asetpts=PTS-STARTPTS,"
                           f"aresample=48000,adelay={int(pos * 1000)}:all=1[f{i}]")
            todas += [[w[0], round(pos + w[1] - a, 3), round(w[2], 3)] for w in ws]
            pos += (b - a) + pausa
        n = len(frases)
        filtros.append("".join(f"[f{i}]" for i in range(n)) + f"amix=inputs={n}:normalize=0[o]")
        cmd += ["-filter_complex", ";".join(filtros), "-map", "[o]", "-c:a", "libmp3lame", "-b:a", "160k",
                str(AQUI / f"{k}.mp3")]
        subprocess.run(cmd, check=True)
        palabras[k] = todas
        dur[k] = round(todas[-1][1] + todas[-1][2] + 0.25, 3)
    (AQUI / "duraciones.json").write_text(json.dumps(dur, indent=1))
    (AQUI / "palabras.json").write_text(json.dumps(palabras, ensure_ascii=False))
    print(dur, "total", round(sum(dur.values()), 1))

asyncio.run(main())
