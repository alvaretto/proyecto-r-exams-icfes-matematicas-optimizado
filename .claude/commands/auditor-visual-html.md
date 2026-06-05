Lanza el agente `auditor-visual-html`: renderiza varias decenas de versiones HTML de un `.Rmd`, las captura a viewports móvil (360px) y desktop (1024px), y las inspecciona VISUALMENTE para detectar errores de todo tipo (fugas de markup, math sin renderizar, incoherencias tabla-texto, partes faltantes, desbordes/responsividad, mojibake, anomalías cross-versión).

## Input recibido

$ARGUMENTS

## Instrucciones

Si el input está vacío, responde SOLO con:
"Uso: `/auditor-visual-html <ruta/al/ejercicio.Rmd> [N]` — N = nº de versiones a auditar (default 24). También acepta una carpeta con HTMLs ya renderizados."

Si NO está vacío:

1. **Resolver el target.** Toma el primer token como ruta (`.Rmd` o carpeta). Si es relativo, resuélvelo. Verifica que existe con Glob/Read. El segundo token (opcional) es `N`.

2. **Lanzar el agente** con Agent tool, `subagent_type="auditor-visual-html"`, `model="sonnet"`. En el prompt incluye:
   - La ruta absoluta del `.Rmd` (o carpeta de HTMLs).
   - El `N` solicitado (o 24 por defecto).
   - Una carpeta de salida temporal sugerida, p.ej. `/tmp/auditshots_<nombre-ejercicio>`.

   El agente ya tiene su metodología interna (render+captura vía `.claude/scripts/render_html_shots.R`, triaje con contact sheets, inspección por lotes, catálogo de 7 categorías, reporte con severidad binaria). NO le repitas la metodología; solo pásale el target y N.

3. **Presentar el reporte** del agente tal cual (sin atenuar). Luego agrega tu lectura:
   - ¿Cuántos hallazgos CRÍTICOS vs NO-CRÍTICOS?
   - ¿El veredicto es APTO_VISUAL / APTO_CON_OBSERVACIONES / NO_APTO_VISUAL?
   - Recomendación: corregir ahora (qué), o promover.

4. **Si el usuario pide corregir** ("Proceder", "Corregir"): filtrar los hallazgos CRÍTICOS reproducibles, delegar a un `implementador` (Sonnet, máx 5 fixes/lote, lee el archivo antes de cada Edit), y **re-auditar** visualmente tras los fixes (volver al paso 2 con menos versiones para confirmar). NUNCA `git diff` en archivos untracked; verificar con Grep.

## Notas

- Revisa la salida de `exams2html()` (contenido/layout/coherencia/responsividad), no los widgets interactivos de Moodle.
- Complementa al `adversario` (lógica/matemática del código) y al `AgenteValidadorVisual` (ciclo de validación 4 formatos): este se especializa en la **revisión visual masiva multi-semilla del HTML renderizado**.
- Requiere `chromium`/`google-chrome-stable` + `magick` (ya presentes en el entorno).
