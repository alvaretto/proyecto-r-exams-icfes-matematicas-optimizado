# Auditoría Detractor, FASE 2C r3: CLOZE hermano de `diagrama-venn-juegos-mesa-n3` (MAT-2026-07-014)

**Fecha**: 2026-11-09
**Agente**: `AgenteDetractor` nuevo. No escribí ni corregí nada de lo auditado y no participé en las rondas r1 ni r2.
**Repo**: no lo modifiqué. Todo lo que ejecuté corrió en `/tmp/claude-1000/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/3530c5eb-2f14-461c-b6c9-761d2d82ab0b/scratchpad/detractor_r3/`.

**Objetos auditados.** Los md5 en disco coinciden con el encargo:
- `.Rmd`: `889d828e…`
- verificador: `d489bafb…`
- plantilla: `290a3f79…`
- semillero: `69194749…`

**Objeciones**: 5, ninguna de CORRECCIÓN. Hay 1 MEDIA, 3 BAJAS y 1 sin confirmar (H5, que no cuenta para el veredicto).

## Resumen ejecutivo

No encontré ningún defecto de CORRECCIÓN, pero mi cobertura propia es menor que N = 100.

**Lo que verifiqué a mano.** Comprobé las seis claves de tres versiones desde el texto que ve el estudiante:
- **Pregunta 1 del PDF del semillero** (lo comprobé todo):
  - Datos: a = 7, b = 13, c = 10, d = 9; totales 31, 31 y 21.
  - Regiones correctas: centro 6, zonas dobles 7, 3 y 4, zonas de un conjunto 15, 14 y 8. Coinciden con el Diagrama II impreso, el que marca la hoja.
  - Partes 2 a 5: 20, 5, 22 y 127.
  - Parte 6: solo (b) es verdadera.
- **Semillas 4 y 5 del XML de Moodle**: las regiones y las cuatro marcas del MULTIRESPONSE, una por una.

**Mi oráculo automático no llegó a una conclusión** (detalle en «No auditado»):
- Tesseract no leyó las cifras de los PNG.
- El parser de texto tuvo errores propios.

**Apoyo para no ver un defecto de CORRECCIÓN:**
- La r2 auditó este mismo md5 con un oráculo independiente: 0 fallas en 120 versiones.
- La r6 terminó con 0 rojos.
- El `.Rmd` no cambió desde entonces.

**Esta tanda: el núcleo es correcto.**
- La plantilla difiere de `../solpcielo_cloze.tex` solo en el comentario y en `\clearpage`.
- Ejecuté el semillero entero como lo correría el profesor, en modo no interactivo y con `archivos = 2` y `preguntas_pdf = 3`. Resultado:
  - exit 0, con HTML, Moodle y PDF generados;
  - hoja de 612,283 × 935,433 pt, es decir 216 × 330 mm;
  - las figuras de las preguntas empiezan en las páginas 3, 6 y 9, con las cuatro juntas;
  - las imágenes son distintas en cada pregunta (objetos 40-43, 65-68 y 89-92), como pide la regla #4;
  - «Cómo marcar» está en la portada;
  - `info_sesion.rds` guarda la semilla 30510678: `config$semilla` llega a `semilla_base` por coincidencia parcial de nombre.
- Los dos cambios del semillero que la decisión no nombra son correctos (ver «Verificado sin objeción»).

**Hallazgo principal (H1, MEDIA).** En el PDF impreso, las opciones del gap de la Parte 1 salen barajadas, por ejemplo «Diagrama I / Diagrama IV / Diagrama III / Diagrama II», y la hoja de respuestas pide la letra según la posición. Un estudiante que lee el numeral como letra (I → a, II → b…) marca mal la clave en cerca de 3 de cada 4 versiones.

## Hallazgos

### H1 · MEDIA · OTRO (validez del instrumento impreso) · En papel, los numerales I-IV chocan con las letras (a)-(d) de la hoja

**Qué pasa:**
- Las cuatro figuras se rotulan «Diagrama I…IV» por la posición que tienen después de la mezcla (`textos_p1 <- paste("Diagrama", ROMANOS)`).
- Con `exshuffle: TRUE`, R/exams baraja esos textos dentro del gap.
- En el PDF se imprimen separados por «/», y la hoja de respuestas pide (a)-(d) según esa posición. Así lo dice el recuadro: «la opción (a) es la primera, (b) la segunda…».

**Medido en el render del semillero**, pregunta 1:
- El gap imprime «Diagrama I / Diagrama IV / Diagrama III / Diagrama II».
- La clave es el Diagrama II y la hoja la marca como (d).
- Un estudiante que identifica bien el Diagrama II y marca (b) pierde la Parte 1.

**Cuánto pasa.** El barajado es uniforme, así que la clave cae en la posición de su propio numeral con probabilidad 1/4. En cerca del 75 % de las versiones impresas, leer el numeral como letra lleva a otra casilla.
- En Moodle no pasa: el estudiante elige el texto.
- Ninguna sonda lo mide.
- Lo que sí está declarado en «CLOZE hermano» es que se barajan los textos del gap (conflicto 3). La consecuencia en papel no aparece.

**Riesgo.** Es una dificultad ajena al constructo que puede bajar la nota impresa de quien sabe la respuesta. El recuadro «Cómo marcar» la mitiga, pero es genérico. No toca la clave.

**Propuesta.** Las dos opciones necesitan decisión del profesor, porque ya no quedan pasadas:
- **(a)** Declararlo como residuo y añadir a «Cómo marcar» una línea para este ítem: «en la Parte 1, la letra NO es el número del diagrama». Cambia la plantilla, no el `.Rmd`, y se mide con `papel`/`uso`.
- **(b)** Usar `exshuffle: FALSE` en este CLOZE. La Parte 6 ya sale en orden aleatorio por construcción (`pares6` y `v6` sorteados), así que no se pierde nada, y el gap se imprime «I / II / III / IV». Pero choca con la regla 6 de `codigo-rmd.md` y cambia el `.Rmd`: sería una 4.ª pasada.

### H2 · BAJA · OTRO · El semillero nuevo exporta 250 versiones, contra la instrucción del profesor de 100 como máximo

- `config$archivos = 250` en HTML, Moodle y Canvas.
- La instrucción del 2026-08-15, guardada en la memoria del proyecto, dice: «Son y serán máximo 100». Nombra justamente el `n = 250` de `SemilleroCloze.R` como fuera de estándar.
- La tensión con la regla #3 (250 o más únicas sobre 300) está declarada y pendiente. Aun así, este archivo se creó en esta tanda y heredó el 250 sin decirlo.
- `uso` no vigila ese literal.

**Propuesta:** `archivos = 100` con su literal en `USO_LITERALES`, o declarar la tensión al profesor en el WAIT_USER.

### H3 · BAJA · COBERTURA · `uso` no vigila el flujo de arriba del semillero, y USO_SEMILLERO tiene componentes sin mutante propio

**(a) El flujo de arriba no se ejecuta.**
- `cargar_semillero()` evalúa solo 7 asignaciones.
- Nunca corre `validar_archivo`, `prueba_rapida`, `limpiar_salidas_anteriores`, `crear_directorios`, `generar_todos_formatos`, la elección de formatos ni `saveRDS`.
- Hoy funcionan: mi ejecución completa terminó con exit 0.
- Pero nada impide que se quite `"pdf"` del default no interactivo (`formatos_seleccionados <- c("html", "moodle", "pdf")`).
- En modo interactivo, el PDF solo sale con la opción 4, que arrastra Canvas, o con la 5. Canvas, `prueba_rapida` y la limpieza no tienen vigilancia. No importan para la decisión 1 b.

**(b) Obligación 7 del contrato: cada componente con un mutante que la mueva sola.**
- USO_SEMILLERO tiene 7 componentes: 4 literales y 3 cadenas prohibidas.
- U1 mueve una.
- U2 inserta `archivo_simple <- "test_simple_pdf.Rmd"`, que tumba **dos** prohibidas a la vez.
- `pdf_basico` y tres literales no tienen mutante.

**Propuesta:**
- Un mutante por literal y por cadena prohibida, y que U2 inserte una sola.
- Una guarda que exija `"pdf"` en el default no interactivo.
- Si no se hace, declararlo como residuo.

### H4 · BAJA · OTRO · El HANDOFF describe el estado anterior a la tanda 4

- Dice que el verificador tiene md5 `76493b2c…` y 11 secciones, y que «faltan la corrida r6».
- En disco ya está `d489bafb…` con 12 secciones, y la r6 terminó con exit 4.
- Tampoco recoge la decisión «1 b · semillero sí».

**Propuesta:** actualizarlo antes del paso 8.

### H5 · SIN CONFIRMAR (no cuenta para el veredicto) · Puntaje del MULTIRESPONSE en Moodle

**Lo que vi.** En mi XML, cada afirmación verdadera lleva 100 % y cada falsa −50 %:
- semilla 4: 100, 100, −50, −50;
- semilla 5: lo mismo;
- es coherente con el «=» que describe la línea 658 del verificador.

**Lo que no sé.** Si Moodle suma las fracciones marcadas y las acota a [0, 1], marcar una sola de dos verdaderas daría la nota completa de la Parte 6. No tengo una fuente de Nivel 1 ni medición en el servidor.

**Prueba:** importar una pregunta en `lab` y responder solo una verdadera. Si se confirma, es MEDIA y viene del exportador (afecta a todos los CLOZE del repo), no de este `.Rmd`.

## Verificado sin objeción

**Plantilla.**
- `diff` contra `../solpcielo_cloze.tex`: solo el comentario y `\clearpage`.
- «Cómo marcar» se imprime: lo vi en mi render.
- La sonda tiene control positivo (100 de 100 y 34 de 34) y muere con PP5 y U5.

**Cambios no nombrados en la decisión: los dos son correctos.**
- `cloze_schoice_display = "MULTICHOICE_V"` alinea la exportación con la forma que validan `moodle` y `render_cloze.R`.
- Las funciones que se quitaron imprimían otro ejercicio, un archivo de prueba de «costo promedio». Quitarlas corrige un defecto heredado.
- `svg = TRUE` no afecta: las figuras son PNG que escribe `dibujar_venn`.

**`uso` ejecuta el código del semillero**, extraído de su texto, no una reimplementación.

**Mutantes PP4, PP5 y U1-U9, frente al contrato:**
- Cumplen: sonda declarada (1), muerte solo por su sonda (2), patrón único con `mutar` (4), semillas con tope (5) y línea base (8).
- La materialización (3) es real: en disco o en el render.
- La excepción del comentario (6) es un comentario de TeX, así que es inocua.
- La obligación 7 está incompleta (H3).

**C-4.** E1 en verde, md5 cableado coincide, r6 con exit 4, 0 rojos y 0 pendientes. La firma no cubre la CORRECCIÓN, y no la encontré.

**Observación sin severidad.** El semillero genera solo la versión con soluciones: X en la portada y «Retroalimentación». Es la plantilla que eligió el profesor.

## No auditado

- **Oráculo propio sobre N = 100 + la canónica: no concluyó.**
  - Tesseract leyó 3 de los 7 números del primer diagrama.
  - El parser de texto falló en 85 de 110 versiones.
  - En las otras 25, las únicas fallas fueron la retroalimentación de cada gap, que en el XML sale vacía. Lo leo como un supuesto erróneo de mi parser, no como un defecto.
  - No pude ver el resto de la tabla de códigos. La corrección descansa en las tres versiones a mano, en la r2 sobre el mismo md5 y en la r6.
- Dónde llega a Moodle la retroalimentación de cada gap. En mi XML no está en el gap, y la línea 472 del `.Rmd` dice que «viaja a los gaps».
- Canvas con CLOZE, la 6b visual y el DOCX.

## Próximos pasos

1. **H1** al profesor: opción (a), residuo más una línea en «Cómo marcar», u opción (b), `exshuffle: FALSE` como 4.ª pasada firmada.
2. **H2**: 100 versiones, o declarar la tensión.
3. **H3 y H4**: mutantes y guarda del default, o residuo; y actualizar el HANDOFF.
4. **H5**: medirlo en `lab`.

CITAS_DETRACTOR
H1 | A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/solpcielo_cloze_una_por_pagina.tex:142 | la opción (a) es la primera, (b) la segunda
H1 | A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_cloze_v1.Rmd:341 | textos_p1 <- paste("Diagrama", ROMANOS)
H2 | A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/SemilleroCloze.R:47 | Número de versiones a generar (HTML, Moodle y Canvas)
H2 | /home/bootcamp/.claude/projects/-home-bootcamp-Proyectos-2026-RepositorioMatematicasICFES-R-Exams/memory/feedback_maximo_100_tambien_en_exportaciones.md:11 | Ya no se necesitan 300 preguntas. Son y serán máximo 100.
H3 | A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R:1501-1631 | USO_PROHIBIDOS <- c("archivo_simple", "pdf_basico", "test_simple_pdf")
H3 | A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R:1501-1631 | archivo_simple <- "test_simple_pdf.Rmd"
H3 | A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/SemilleroCloze.R:501 | formatos_seleccionados <- c("html", "moodle", "pdf")
H4 | A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/HANDOFF.md:6 | está aplicada solo al verificador, que ahora tiene md5
H5 | A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/Cloze/verificar_espacio_cloze.R:658 | el «=» de cada afirmación del MULTIRESPONSE
FIN_CITAS_DETRACTOR
VEREDICTO_DETRACTOR: APROBAR_CON_CAMBIOS
