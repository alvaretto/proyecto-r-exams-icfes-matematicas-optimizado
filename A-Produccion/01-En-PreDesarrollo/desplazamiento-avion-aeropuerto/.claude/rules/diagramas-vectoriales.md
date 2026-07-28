# Regla Local — Diagramas Vectoriales de Opción (desplazamiento-avion-aeropuerto)

## Principio Fundamental

**Todo diagrama vectorial usado como opción gráfica en este ejercicio (distancia + dirección
sobre ejes cardinales) DEBE construirse con cinco garantías simultáneas: (1) la etiqueta del
ángulo nunca se solapa con la línea ni con el eje en ningún punto del rango de ángulos
aleatorizados, (2) la escala del dibujo se deriva del máximo valor efectivamente dibujado —
nunca de una constante ni del valor de un distractor concreto —, (3) la orientación global
(cuadrante) se aleatoriza de forma idéntica para las cuatro opciones, de modo que la posición
visual de la respuesta correcta no sea predecible entre versiones, (4) el umbral de legibilidad
se aplica como CASCADA de escalones, nunca como umbral único con `stopifnot`, y (5) varios
distractores conservan la magnitud de la correcta, de modo que el rótulo numérico no permita
descartar opciones sin analizar la representación.**

Esta regla es **local a este subproyecto** (no forma parte de la numeración de reglas críticas
del repositorio raíz) y complementa, sin sustituir, las reglas heredadas de
`RR/.claude/rules/graficos-como-opciones.md` (regla #4) y
`RR/.claude/rules/diversidad-sustantiva.md` (regla #22). Aplica a cualquier función de dibujo
`grid`/`ggplot2`/TikZ que este ejercicio use para generar sus opciones, presentes o futuras.

---

## Origen

Esta regla formaliza dos incidentes documentados en `RR/.claude/docs/patrones-errores-conocidos.md`
que se originaron en este ejercicio y cuyo fix vive en el chunk `data_generation` del `.Rmd`:

- **Error 23** (L2432): etiquetas de ángulo solapadas con la línea/eje en ángulos grandes.
- **Error 24** (L2485): predictibilidad posicional de la respuesta correcta pese a que su
  *valor* variaba correctamente entre versiones (regla #22 §P4 del repo raíz).

Y un tercer hallazgo (2026-07-28, no elevado aún a error numerado del catálogo global): una
escala de dibujo acoplada al valor de un distractor concreto producía una longitud de píxel
constante para esa opción en toda semilla — una regularidad visual detectable sin leer los datos.

---

## Patrones Aceptados

### 1. Etiqueta del ángulo — radio con piso, no fórmula pura

```r
# ✓ CORRECTO — radio mínimo que garantiza holgura en TODO el rango de ángulos (30°-70° aquí)
semi  <- (angulo/2) * pi/180
R_fit <- max(50, (8 + 11 * cos(semi)) / sin(semi))
rang  <- if (abs(Lpx - R_fit) < 22) Lpx + 24 else R_fit
tx(paste0(angulo,"°"), cx+rang*cos(lab), cy-rang*sin(lab), 13, .cols$orange, "centre", 2)
```

**Por qué el piso es 50 y no la fórmula sin piso**: la cuña entre el eje cardinal y la línea se
estrecha con ángulos pequeños (el radio "natural" crece), pero con ángulos **grandes** (cuña
ancha, p. ej. 70°) la fórmula `(8 + 11*cos(semi))/sin(semi)` da un radio pequeño (~30) y la línea
casi horizontal clipa el texto. El piso 50 da holgura en ambos extremos del rango. El primer
intento con piso 34 fue insuficiente — verificado visualmente en ángulos ≥65° a escala ×2.4.

**Verificación obligatoria**: antes de cambiar esta fórmula, renderizar diagramas con el ángulo
en el mínimo, el máximo y al menos 3 puntos intermedios del rango aleatorizado declarado en
`data_generation` (actualmente `seq(30, 70, by = 5)`), e inspeccionar visualmente cada etiqueta
a escala ampliada (≥×2). Un preview en miniatura puede ocultar el solape.

### 2. Escala global — derivada del máximo dibujado, nunca de un distractor

```r
# ✓ CORRECTO — la escala se deriva de la suma de las dos magnitudes máximas presentes en
#    CUALQUIER opción (aquí: distancia_total + distancia_avanzada, que es el valor más grande
#    entre las 4 opciones dibujadas), no de una constante ni del valor de una opción concreta
escala_px_km <- 120 / (distancia_total + distancia_avanzada)
```

```r
# ❌ PROHIBIDO — acoplar la escala a la magnitud de UN distractor específico
# (ejemplo del hallazgo 2026-07-28: si la escala se deriva exactamente del valor que se
# dibuja SOLO en el distractor "suma", ese distractor mide siempre 120 px exactos en
# CUALQUIER semilla — un patrón visual constante detectable sin leer los datos)
escala_px_km <- 120 / (distancia_total + distancia_avanzada)  # ok si "total+avanzada" es
                                                                # también el máximo entre TODAS
                                                                # las opciones, no solo esa
```

La distinción no es la fórmula en sí, sino que el denominador debe representar el **máximo
efectivo entre todas las opciones dibujadas** (de modo que todas comparten la misma escala y
ninguna queda con una longitud de píxel "mágica"), no el valor aislado de un distractor elegido
porque "es el más grande y conviene para el layout".

### 3. Orientación global — mismo pool, misma transformación para las 4 opciones

```r
# ✓ CORRECTO — un solo sorteo de orientación por versión, aplicado a las 4 llamadas de dibujo
orientaciones <- list(
  list(quad = "NE", th_axis = 90,  dir_sign = -1, eje = "norte", lado = "este"),
  list(quad = "NO", th_axis = 90,  dir_sign =  1, eje = "norte", lado = "oeste"),
  list(quad = "SE", th_axis = 270, dir_sign =  1, eje = "sur",   lado = "este"),
  list(quad = "SO", th_axis = 270, dir_sign = -1, eje = "sur",   lado = "oeste")
)
orient <- orientaciones[[sample(length(orientaciones), 1)]]
# ... las 4 llamadas a dibujar_diagrama() usan orient$th_axis / orient$dir_sign (o su espejo
#     para el distractor direccional), NUNCA un cuadrante fijo hardcoded
```

El texto del enunciado (`dir_desc`) DEBE derivarse del mismo `orient` sorteado, para que la
descripción textual de la dirección sea coherente con el cuadrante dibujado en esa versión.

---

### 4. Umbral de legibilidad en CASCADA, no umbral único

```r
# ✓ CORRECTO — escalones descendentes: cada versión se queda en el más alto que le sea factible
RATIOS_LEGIBILIDAD <- c(0.40, 0.35, 0.30, 0.25)
pares_validos  <- list(); ratio_aplicado <- NA_real_
for (r_min in RATIOS_LEGIBILIDAD) {
  pares_validos <- filtrar_pares(r_min)
  if (length(pares_validos) > 0) { ratio_aplicado <- r_min; break }
}
stopifnot(length(pares_validos) > 0, !is.na(ratio_aplicado))
```

Un umbral único falla por los dos extremos: si es bajo (0.25) hay versiones con el vector más
corto en 30 px, donde el arco, la etiqueta del ángulo y el rótulo de distancia quedan todos FUERA
del vector; si es alto (≥0.45) hay versiones donde **ninguna** pareja de distractores cumple y el
`stopifnot` revienta el render. Barrido de 40 semillas por valor (2026-07-28): 0.30 → 40 px,
0.35 → 43,6 px, 0.40 → 48 px, todos sin fallos; 0.45 y 0.50 → 2/40 versiones sin pareja válida.

Verificación tras el cambio (80 semillas): 0.40 aplicado en 79/80, mínimo 48 px, 0 fallos.

### 5. Varios distractores deben CONSERVAR la magnitud de la correcta

Cada diagrama muestra su distancia ("40 km"). Si solo la correcta y su espejo comparten ese
rótulo, el estudiante calcula el valor y descarta la mitad de las opciones sin mirar el dibujo.
El pool debe incluir **varios** errores que produzcan la MISMA magnitud y difieran solo en la
dimensión evaluada (lado del eje, eje de referencia, eje perpendicular). Hoy son tres:
`GEO-DES-01` (espejo del lado), `GEO-DES-05` (ángulo desde el eje perpendicular) y `GEO-DES-07`
(ángulo desde el eje cardinal opuesto).

Reparto verificado sobre 80 semillas: **2 opciones comparten el rótulo en el 24 % de las
versiones, 3 en el 60 % y 4 en el 16 %**. En ese último 16 % el filtro numérico no descarta nada.

Antes de añadir un distractor que conserve la magnitud hay que **demostrar** que su dirección no
colisiona con ninguna otra opción en ningún cuadrante ni ángulo del rango (para `GEO-DES-07`: con
la correcta exigiría un ángulo de 90°, fuera del rango 30-70; con el espejo difieren exactamente
180°; con `GEO-DES-05`, 90° o 270° según el cuadrante). Sin esa demostración, la precondición no
puede ser `TRUE`.

---

## Patrones PROHIBIDOS

### ❌ P1: Fórmula de radio sin piso mínimo

```r
# ❌ PROHIBIDO — sin max(), falla en ángulos grandes del rango aleatorizado
R_fit <- (8 + 11 * cos(semi)) / sin(semi)
```

### ❌ P2: Escala hardcodeada o acoplada a un solo distractor

```r
# ❌ PROHIBIDO — constante fija, no se adapta a los valores aleatorizados de la versión
escala_px_km <- 0.8

# ❌ PROHIBIDO — acoplada al valor de UN distractor sin garantizar que sea el máximo global
escala_px_km <- 120 / distancia_avanzada
```

### ❌ P3: Cuadrante fijo o distinto por opción

```r
# ❌ PROHIBIDO — la correcta siempre en el mismo cuadrante (predictibilidad posicional, Error 24)
dibujar_diagrama("diagrama_correcta.png", ..., th_axis = 90, dir_sign = -1)  # fijo NE siempre

# ❌ PROHIBIDO — cada opción con su propia orientación independiente (rompe la comparabilidad
#    y puede introducir asimetrías involuntarias entre opciones)
dibujar_diagrama("diagrama_correcta.png", ..., th_axis = sample(c(90,270),1), ...)
dibujar_diagrama("diagrama_suma.png",     ..., th_axis = sample(c(90,270),1), ...)  # sorteo distinto
```

### ❌ P5: Umbral único de legibilidad con `stopifnot`

```r
# ❌ PROHIBIDO — o deja versiones degeneradas (umbral bajo) o revienta el render (umbral alto)
RATIO_MIN_LEGIBILIDAD <- 0.40
pares_validos <- filtrar_pares(RATIO_MIN_LEGIBILIDAD)
stopifnot(length(pares_validos) > 0)   # falla en las versiones sin ninguna pareja válida
```

### ❌ P6: `set.seed()` dentro del chunk

```r
# ❌ PROHIBIDO — pisa la semilla que fija el llamador vía el argumento `seed` de exams2*/xexams,
#    con lo que ninguna validación multi-semilla puede reproducir un fallo detectado
seed_global <- as.integer(Sys.time()) %% 100000 + sample(1:99999, 1)
set.seed(seed_global)
```

Verificado en el fuente de `exams:::xexams()`: sin argumento `seed` (por defecto) NO se fija
semilla por versión y el flujo RNG global avanza solo — las versiones ya difieren entre sí; con
`seed` (matriz o TRUE) se ejecuta `set.seed()` por versión y se restaura `.Random.seed` después.
El régimen (b) es el mecanismo documentado de reproducibilidad, y el reseed por reloj lo anula.

### ❌ P4: Distractor con rasgo saliente que lo hace un outlier obvio

```r
# ❌ PROHIBIDO — el distractor de dirección a otra distancia (no distancia_restante) es
#    eliminable de un vistazo por longitud de píxel distinta a las demás opciones
dibujar_diagrama("diagrama_perp.png", km(otra_distancia), otra_distancia, escala_px_km,
                  angulo_direccion, th_axis_espejo, dir_sign_espejo)
```

Ver regla #22 §P5 del repo raíz — el distractor direccional debe compartir la magnitud
(`distancia_restante`) con la correcta y diferenciarse SOLO en el lado del eje.

---

## Verificación Obligatoria

Antes de aprobar cualquier cambio a `dibujar_diagrama()`, `escala_px_km` o al pool
`orientaciones`:

1. **Rango de ángulos**: renderizar diagramas con el ángulo mínimo, máximo y ≥3 valores
   intermedios del rango declarado en `data_generation`; inspeccionar la etiqueta a escala
   ampliada (≥×2) — un preview en miniatura puede ocultar el solape (lección del Error 23).
2. **Rango posicional (Error 24)**: renderizar **≥40 versiones** del ejercicio y registrar, para
   cada una, el cuadrante (`orient$quad`) en el que cae la opción correcta. Las 4 orientaciones
   (NE/NO/SE/SO) deben aparecer con frecuencia razonablemente equilibrada — no basta con que el
   *valor* de la respuesta varíe; su *posición* también debe hacerlo. Comando de referencia:

   ```bash
   Rscript ../../../../../.claude/scripts/validar_diversidad_sustantiva.R \
     desplazamiento_avion_aeropuerto_metacognitivo_interpretacion_n3_schoice_v1.Rmd --n 40
   ```

   Este script valida diversidad **por valor**. La diversidad **posicional** (cuadrante) requiere
   inspección adicional porque el fingerprint del validador captura el valor de la respuesta
   correcta, no su orientación visual — ver `RR/.claude/rules/diversidad-sustantiva.md` §P4.
3. **Escala compartida**: sobre las mismas ≥40 versiones, verificar que ninguna opción mantiene
   una longitud de píxel (`Lpx`) idéntica en todas las semillas — señal de que la escala quedó
   acoplada a un valor constante o a un distractor específico.
4. **Cuatro formatos**: renderizar HTML, PDF, DOCX y NOPS tras cualquier cambio — el patrón
   `{=latex}\newcounter{none}` y los atributos `{width=...}` (reglas #18/#20 del repo raíz) deben
   seguir intactos.

---

**Versión:** 2.0
**Fecha:** 2026-07-28 (v2.0 — garantías 4 y 5: cascada de umbrales de legibilidad [H7] y
distractores que conservan la magnitud [H4]; antipatrones P5 y P6; v1.0 mismo día)
**Estado:** ACTIVO (regla local de subproyecto)
**Excepciones:** NINGUNA
**Aplica a:** el chunk `data_generation` de
`desplazamiento_avion_aeropuerto_metacognitivo_interpretacion_n3_schoice_v1.Rmd` y cualquier
ejercicio derivado que reutilice `dibujar_diagrama()` como helper.
