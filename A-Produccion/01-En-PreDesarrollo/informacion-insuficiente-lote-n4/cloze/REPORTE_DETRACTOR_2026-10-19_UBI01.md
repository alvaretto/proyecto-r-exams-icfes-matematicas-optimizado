# Detractor independiente: UBI-01 en el CLOZE (informacion-insuficiente-lote-n4)

Fecha: 2026-10-19. Solo lectura. Scripts propios: medir.R, analizar.R, render.R (esta carpeta).
Muestra: semillas naturales set.seed(s), s = 1..450 (0 errores de generación). Estratos -TAN y LTA-: las 100 primeras semillas de cada uno (N = 100, regla #23). Prevalencias globales sobre n = 450 (36 canónicas), estratos -T-N 63, -TAN 110, L--N 103, LT-- 48, LTA- 126: ninguno con n < 20.
Mapeo de semillas verificado: exams2html(seed = s) reproduce lo predicho en las 7 semillas renderizadas (9, 37, 91, 12, 56, 5, 30).

## H1: PARCIAL
- El texto existe tal como se describe: det (línea 355), valor (359-360), razon (361-363; solo invoca trazado y orden) y motivo de T∧A (364-368: «sin ese número no se sabe dónde empieza ni dónde termina la franja»).
- La dicotomía O/F no es exhaustiva. Hay una tercera lectura, P: la región relativa al lote, es decir, el tramo entre (k-1)/n y k/n del lado perpendicular, contado desde la carretera. Exige T, A y N, pero no L. Es exactamente DEP = "TAN". Bajo P, -TAN queda determinada y LTA- indeterminada, las dos correctamente.
- Lo que sí es incoherente es el texto que se muestra. valor y razon de -TAN están redactados en O («la franja que le corresponda por su orden en el acta») y afirman que T∧A basta («así que la posición… queda determinada»), cuando el modelo exige además N. LTA- es el contraejemplo: tiene T∧A y es indeterminada.
- Además, el valor de -TAN se puede deducir literalmente del enunciado de LTA- (semilla 12: «Las franjas se asignan a los hermanos en el orden en que aparecen en el acta, empezando por la que da a la carretera»).

## H2: CONFIRMADA (cifras idénticas)
- -TAN (n = 100, semillas 5-418): UBI-01 aparece en 58. P4: 13, todas marcadas V. P5: 26, todas como distractor con h_p5 = L. P6: 23, todas como clave (A 9, N 6, T 8). P1: 0. Solapamiento P5∩P6 = 4.
- LTA- (n = 100, semillas 3-346): aparece en 42. P4: 36, todas marcadas F. P5: 6, todas como clave con h_p5 = N. P1 y P6: 0.
- Render HTML: semilla 37 (-TAN, P4 «Sí se puede responder: la franja que le corresponda…»), semilla 9 (-TAN, P5 distractor h = L y P6 clave con hecho A), semilla 12 (LTA-, P4 «No se puede responder. El enunciado fija por dónde pasan los cortes… sin ese número no se sabe dónde empieza ni dónde termina la franja de cada hermano»), semilla 56 (LTA-, P5 clave).

## H3: REFUTADA tal como está formulada («con cualquier lectura única hay defecto de CORRECCIÓN»)
- Bajo P, las 100 + 100 versiones quedan bien marcadas. Ahí no hay clave falsa ni segunda clave.
- Cifras propias (n = 450): bajo F, 63 versiones con marca discutida (14,0 %); bajo O, 58 (12,9 %). Concuerdan con el ≈ 13,9 % y el ≈ 13,6 % del otro agente.
- Residuo real, de severidad ALTA y no de CORRECCIÓN: la pregunta oficial admite O, y el valor del propio ítem es la respuesta O.
  - En LTA- P4 (36/100), quien lea O y marque UBI-01 pierde crédito.
  - En -TAN P5 (26/100), quien lea F ve una segunda candidata. Aun así, la clave (LOT-04, «medida del lado más largo») sigue siendo la mejor respuesta.
- La justificación oficial («hace falta saber cómo se realizan los cortes») no distingue entre O, P y F.
- Evidencia en contra de P dentro del propio código: el comentario del SCHOICE (líneas 416-419) llama «contradictorio» que el ancho sea desconocido y la ubicación conocida, y en -TAN el ancho en metros (POR-01) es desconocido.

## Hallazgo nuevo (las celdas P6 de UBI-01 lo contienen): la Solution de la Parte 6 culpa al hecho equivocado. Es CORRECCIÓN (Solution falsa) y no depende de la lectura
- Causa: la línea 1742 y fb_p6 (1809) imprimen clave_p6$motivo, calculado con K_ACTUAL, no con k_p6. Cuando la clave de P6 depende de varios hechos, el motivo culpa a uno que no es hecho_p6 y que el enunciado SÍ declara.
- Semilla 91 (-TAN, «entre los 4 socios»): «Si el enunciado no hubiera dicho por dónde pasan los cortes, la pregunta "¿Cuál es la ubicación…?" dejaría de tener respuesta: El enunciado fija por dónde pasan los cortes y a quién le toca cada franja, pero no dice en cuántas partes se reparte el lote…». El retroalimento de la opción correcta repite lo mismo.
- Semilla 9 (hecho A): el mismo texto.
- Semilla 5 (POR-07, hecho T, «entre los 8 beneficiarios»): «…dejaría de tener respuesta: El enunciado no dice en cuántas partes se reparte el lote».
- Semilla 30 (POR-09, hecho L, LTA-, «paralelas al lado de 150 metros»): «…dejaría de tener respuesta: La longitud de cada corte depende de por dónde pasen: el enunciado no lo dice».
- Frecuencia en celdas UBI-01: 17/100 de -TAN (T 8, A 9).
- Frecuencia global (n = 450): 35 (7,8 %) con UBI-01 T/A, POR-07 T y POR-09 L; 49 (10,9 %) si se suma UBI-02 T. Por rama: -T-N 6/63, -TAN 29/110, LTA- 14/126, L--N 0/103, LT-- 0/48.
- El detractor de confirmación (REPORTE_DETRACTOR_FASE2C_CONFIRMACION.md:121) vio solo el desfase de tiempo verbal, lo calificó BAJA y dijo que «el marco condicional lo hace legible». Eso vale cuando el motivo cita el hecho quitado. Aquí cita OTRO hecho, que tanto el enunciado como la hipótesis siguen declarando, y el condicional lo vuelve autocontradictorio.

## H4: CONFIRMADA, con un agravante
- En el CLOZE no existe `codigos` (solo aparece en comentarios). Copiar G8 tal cual daría «objeto no encontrado» en el 94 % no canónico.
- Adaptada a codigos_p1, es vacua: UBI-01 aparece 0/100 en P1 en ambas ramas.
- Adaptada a todas las partes, aborta 115/450 semillas (25,6 %).
- G8 tiene que ser un FILTRO del pool, no un stopifnot.

## Correcciones
1. (CORRECCIÓN) Que la Parte 6 explique el hecho quitado: un motivo por hecho, por ejemplo `motivo_sin[[hecho_p6]]` para UBI-01, UBI-02, POR-07, POR-09 y POR-01..05. Añadir una guarda por palabra clave del hecho y un mutante (al revertir, deben reaparecer ≈ 49/450). No cambia el consumo de RNG.
2. (ALTA) Opción (a): retirar UBI-01 de los pools cuando !is_canonical && info_T && info_A, que son las únicas ramas donde O, P y F discrepan. La (b), LTAN, adopta F, la lectura más débil, y deja UBI-01 como clave en -TAN aunque O y P la dan por respondible; además, su motivo T∧A («no dice en cuántas partes») sería falso en -TAN. No se recomienda.
3. Mediciones a repetir: tasa de fallo de generación por rama (N = 100), validar_multisemilla, §P7 por parte y rama con la batería congelada, H1/H2/H3b por rama, fuga por conjunto (DETM/realizable), diversidad sustantiva, auditoria_propia.R, marca-vs-verdad en el XML de Moodle, los 5 formatos, una nueva FASE 2C y la reaprobación del profesor (pasos 7-11).

VEREDICTO_DETRACTOR: RECHAZAR
