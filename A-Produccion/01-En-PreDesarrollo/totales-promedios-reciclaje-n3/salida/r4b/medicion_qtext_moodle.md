# Ancho real de la caja de la pregunta en el Moodle del colegio (H2 de la FASE 2C r4b)

Medido el 2026-10-30 sobre el `.Rmd` aprobado para el aula (md5 `e293d5c812b1f9790e2bb777b62de9a5`, ronda 4b).

## Qué faltaba

Residuo BAJA H2 de la FASE 2C r4b (`../fase2c_r4b_detractor.md`): el espacio duro de «El Bosque» y
«La Pradera» sube el ancho mínimo de la tabla en las 12/100 versiones de ese conjunto (hasta +21,4 px). Con
cajas de 240 px o más, el desborde es igual al de la versión aprobada antes; con cajas más estrechas, mayor
(a 230 px, 49 contra 37). Faltaba medir el ancho real de `.qtext` en el Moodle del colegio a 320 px.

## Cómo se midió

El servidor solo se leyó: no se crearon intentos ni se importaron preguntas.

- **Servidor:** «icfes» (`aula.senodeteta.com`), Moodle 5.1.7 (Build: 20260914), tema Boost.
- **Página:** sesión con la cuenta de prueba del profesor (rol `teacher` en el curso oculto `lab`, id 7) y
  revisión (`review.php`) del intento 4658 del cuestionario «¿Alcanzan los datos? — verificación» (cmid 476),
  que ya existía.
- **Navegador:** Chromium headless con emulación de celular (`Emulation.setDeviceMetricsOverride`,
  `mobile: true`, factor 2), a 320 y 360 px.
- **Versiones:** las 100 de la muestra estándar (regla #23), semillas 1 a 100, una por semilla, generadas con
  `exams2moodle()` por `generar_moodle_100.R`. Dos corridas del generador dieron los mismos 100 XML (md5).
- **Medida:** el `questiontext` de cada versión, con el `<style>` que trae del XML, se pone en `.qtext` dentro
  del navegador (`Runtime.evaluate`). Se compara el `scrollWidth` del `div` `overflow-x:auto` con su
  `clientWidth`, y se mide el ancho mínimo de la tabla (`width: min-content`).
- **Fuente:** Moodle 5.1 pide primero `system-ui`. En Android esa fuente es Roboto; en Linux, fontconfig la
  resuelve a Noto Sans. Se midió con las dos:
  - **Roboto:** TTF de `/usr/share/doc/pdfio/examples/` (Regular y Bold), con `FONTCONFIG_FILE` apuntando a
    un `fonts.conf` que incluye `/etc/fonts/fonts.conf` y hace alias fuertes de `sans-serif`, `sans` y
    `system-ui` hacia Roboto. `CSS.getPlatformFontsForNode` confirma Roboto en las celdas.
  - **Noto Sans:** la configuración por defecto del equipo.

## Resultados

| Pantalla | `#region-main` | `.formulation` | Caja de la tabla (`.qtext`) | Tabla fuera de su caja (Roboto) | Tabla fuera de su caja (Noto Sans) | Página desbordada |
|---|---|---|---|---|---|---|
| 320 px | 304 px | 304 px | **270 px** | 0/100 (0/12 de «El Bosque»/«La Pradera») | 0/100 (0/12) | 0/100 |
| 360 px | 344 px | 344 px | **310 px** | 0/100 (0/12) | 0/100 (0/12) | 0/100 |

`.qtext` no tiene relleno propio: la caja de la tabla mide 50 px menos que la pantalla.

**Ancho mínimo de la tabla en las 100 versiones** (no depende de la pantalla):

| Fuente | Mínimo | Mediana | Máximo | Máximo con «El Bosque»/«La Pradera» |
|---|---|---|---|---|
| Roboto | 174,9 px | 190,9 px | 225,6 px | 209,9 px |
| Noto Sans | 192,7 px | 209,5 px | 246,4 px | 228,7 px |

- **Las más anchas** (225,6 px con Roboto) son las 9 canónicas de la muestra (s007, s027, s042, s047, s054,
  s063, s066, s068 y s072), con el encabezado del impreso «Promedio de papel recolectado por curso».
- **Margen a 320 px con Roboto:** 44,4 px para la canónica y 60,1 px para «El Bosque»/«La Pradera».

**Control del medidor** (Roboto, `medicion_qtext_moodle_control_250_280.json`):

| Pantalla | Caja | Tablas fuera de su caja | De ellas, «El Bosque»/«La Pradera» |
|---|---|---|---|
| 250 px | 200 px | 49/100 | 12/12 |
| 280 px | 230 px | 0/100 | 0/12 |

## Veredicto

A 320 px, la caja de la pregunta mide 270 px en el Moodle del colegio, por encima del umbral de 240 px: el
residuo H2 r4b no aparece en el aula. Tampoco aparecen los 9/100 desbordes que el arnés de `exams2html` daba a
320 px: ahí la caja medía 249 px, con 15 px de barra, y la letra era otra. En Moodle salen 0/100.

## Lo que no cubre

- Celulares con la letra del sistema agrandada: no se midieron.
- La vista del estudiante (`attempt.php`): se midió `review.php`, que usa la misma región `#region-main` del
  tema. Abrir el cuestionario como estudiante habría creado un intento.
- La letra de cada celular: Roboto es la de Android, pero algunos fabricantes la cambian.
- Ajeno al ejercicio: a 320 px, los botones flotantes de los cajones de Boost tapan el borde del texto en
  cualquier pregunta (`medicion_qtext_moodle_s006_320.png`, la versión s006 en el Moodle real).

## Archivos

| Archivo | Contenido |
|---|---|
| `generar_moodle_100.R` | las 100 versiones en XML de Moodle; verifica el md5 del `.Rmd` |
| `medir_qtext_moodle.py` | el medidor (CDP); su docstring trae el uso |
| `medicion_qtext_moodle_roboto.json` | 320 y 360 px con Roboto: caja vacía y una fila por versión |
| `medicion_qtext_moodle_notosans.json` | lo mismo con Noto Sans |
| `medicion_qtext_moodle_control_250_280.json` | el control a 250 y 280 px, con Roboto |
| `medicion_qtext_moodle_s006_320.png` | captura a 320 px, con Roboto |
