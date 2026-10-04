# MEGA-PROMPT — Endurecimiento integral: barras-campeonato-baloncesto-n3

> Ciclo de cierre de un ejercicio ICFES R/exams. Uso: pegar este archivo al inicio de una
> sesión nueva, o decir «ejecuta mega-prompt-barras-campeonato-baloncesto.md».
> Versión genérica anterior (Horarios-PCielo): en el historial de git de este archivo, si se versionó.

## 0. Variables

```yaml
EJERCICIO:        barras-campeonato-baloncesto-n3
RAIZ:             /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/01-En-PreDesarrollo/barras-campeonato-baloncesto-n3
REPO_GIT:         /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams   # repo con MUCHOS subproyectos
RAMA:             feat/plantillas-oficiales-rexams
RMD_SCHOICE:      barras_campeonato_baloncesto_aleatorio_interpretacion_representacion_n3_schoice_v1.Rmd
RMD_CLOZE:        (no existe; ver PREGUNTA_AL_RETOMAR)
ITEM_FUENTE:      MAT-2026-1-015, cuadernillo Matemáticas 2026-1, pp. 16-17, clave oficial C
JPG_ORIGINALES:   /home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Matematicas/Alineacion-Curricular-de-Items-Matematicas-2026-1/Originales/pagina_01{6,7}.jpg  # recortes en graficador/originales/
ESTADO_WORKFLOW:  ejercicio_state.json        # 11/11 desde 2026-10-03
ARCHIVO_RETORNO:  HANDOFF.md                  # convención del repo; crearlo si no existe
FRASE_DISPARADORA: "Continúa con barras-campeonato-baloncesto-n3"
MEMORIA_SLUG:     project_objetivos_barras_campeonato_baloncesto_n3
IDIOMA:           español con tildes (regla #7), sin excepciones
```

## 1. Rol y Gran Objetivo

Eres el responsable de calidad de este ejercicio ICFES y trabajas con el rigor de un
evaluador psicométrico. El **Gran Objetivo Principal** (no el único) es que el `.Rmd` que
llegue al aula tenga **cero defectos** en matemática, visualización, documentación ICFES,
Solution, ortografía y render, en **todas** sus versiones y en **todos** los formatos.
Los otros objetivos son: conservar la fidelidad al ítem oficial y dejar el subproyecto
retomable con una sola frase.

«Cero defectos» no es una sensación: significa que **la batería D-1…D-14 de la §4 da verde
con salida real pegada**. Un defecto que la batería no detecta se convierte en un check nuevo
y en un test de regresión, y no se da por cerrado hasta que el check falla con el defecto y
pasa sin él.

## 2. Contrato innegociable

1. **Las fases van en orden estricto**, cada una con su puerta. Si una puerta no pasa, se
   corrige el problema; no basta con documentarlo.
2. **Solo cuenta lo verificado en el artefacto.** El PDF/HTML/XML renderizado, la salida del
   validador o el test que pasa. Que el archivo exista, que el subagente diga «listo» o que
   un flag esté en `true` NO es evidencia. El `ejercicio_state.json` se audita por
   artefacto, no por flag.
3. **Las reglas del repo están por encima de este prompt.** `.claude/CLAUDE.md` y
   `.claude/rules/*.md`, y ante un caso no trivial su texto íntegro en `.claude/docs/reglas/`.
4. **Decisiones humanas firmadas que NO se «corrigen»** (H-5: relajar nunca sin el profesor):
   - Flujo B en TikZ con 94 % de similitud, elegido por el usuario el 2026-10-02.
   - La opción B de la instancia canónica conserva el **4,5 LITERAL** del impreso (total 9,5).
   - La trampa del impreso se reproduce tal cual (H-2), sin normalizarla.
   Si crees que alguna está mal, se lo propones al usuario y no la tocas.
5. **Zonas inmutables:** `A-Produccion/03-En-Produccion/`, `Ejemplos-Funcionales-Rmd/`,
   `graficador/originales/`. Los cambios en `.claude/` exigen backup e invariantes (regla #17).
6. **Prohibido:** `git commit --no-verify`, relajar asserts o umbrales, mockear para poner en
   verde, bajar el timeout del hook (300 s) o usar N ≠ 100 en mediciones (regla #23).
7. **Límite de iteración (§P7-D):** como máximo 3 pasadas de corrección de diagnosticidad.
   Después se cierra con el residuo declarado. Tras CADA corrección se vuelve a verificar que
   la clave sigue siendo verdadera en el 100 % de las semillas.
8. Si algo queda bloqueado, se completa todo lo demás y se declara qué quedó fuera y por qué.

## 3. Fase 0 — Reanudación (SIEMPRE primero)

1. Lee `RAIZ/ARCHIVO_RETORNO` (si existe), `ESTADO_WORKFLOW`, el `.claude/CLAUDE.md` del
   repo y, si existe, `RAIZ/.claude/CLAUDE.md`, que puede declarar invariantes locales
   que hay que respetar.
2. `git -C REPO_GIT status --short -- RAIZ` y `git -C REPO_GIT log --oneline -8 -- RAIZ`.
   Si el estado escrito y el repo no coinciden, gana el repo: compara mtimes y no reviertas
   un cambio deliberado.
3. Revisa los cambios **sin commitear** en `RMD_SCHOICE`. Pregunta al usuario por cualquier
   cosa que no hayas hecho tú, por ejemplo un encabezado YAML `output:` añadido al usar
   «Knit» en RStudio.
4. Haz un arranque de 10 líneas o menos: estado, siguiente paso y bloqueos.
   **PREGUNTA_AL_RETOMAR (hacerla antes de ejecutar nada):** «¿Este ciclo incluye crear la
   versión CLOZE (mínimo 6 partes) o se limita a endurecer el SCHOICE?»

## 4. Batería de defectos (fuente única de «cero errores»)

Cada check tiene su comando. Un check sin comando se marca `SIN COBERTURA` y no cuenta como
verde.

| ID | Dominio | Qué garantiza | Cómo se verifica |
|---|---|---|---|
| D-1 | Matemática | La clave es la ÚNICA matriz idéntica a M; 0 segundas claves; distractores únicos | `validar_multisemilla.R` (N=100, 100 %) + test propio que **enumere** formatos × topologías × errores |
| D-2 | Matemática | Instancia canónica (1/8) = datos, formatos y orden del impreso, clave C, B con 4,5 literal | Forzar la rama canónica y compararla celda a celda con `graficador/originales/` |
| D-3 | Matemática | Cada distractor = el error del pool que dice la Solution (`calcula()` pura, `precondicion`) | `validar_coherencia_matematica.R` Niveles 4-5 (ERR_SEM_*, ERR_ANS_*) |
| D-4 | Visual | Las barras dibujadas = la matriz de datos (alturas, segmentos apilados, leyenda, colores) | Muestra de render real con Read de los PNG; declarar N y la razón (Familia B) |
| D-5 | Visual | **Varias copias:** cada pregunta muestra SUS figuras | `exams2pdf(rep(RMD,3))` + `pdfimages -list`: 18 imágenes distintas (6 × 3); igual para NOPS y DOCX |
| D-6 | Visual | Sin «0.0.N» ni encabezados numerados; `{width=}` en toda imagen; sin pie «Figure N» | `pdftotext \| grep '0\.0\.'` vacío; hook FASE 2I |
| D-7 | Render | Los 4 formatos compilan, también con el pandoc de RStudio | `exams2html/pdf/pandoc(docx)/nops` + `RSTUDIO_PANDOC=...` (regla #20) |
| D-8 | Moodle | Sin fuga por nombre de archivo; la marca coincide con la verdad | `exams2moodle` + grep de la regla #4 v6.1 (debe salir vacío) |
| D-9 | Solution | No depende de la letra (#19), no enumera las opciones en orden, la figura de la clave coincide con el texto | Hook FASE 2J + lectura del PDF en una versión apilada y una agrupada |
| D-10 | ICFES | `exextra` literal del catálogo oficial (Competencia, Afirmación, Evidencia, Descriptor D3.1, Estándar); `DOK ≥ 3 ⇒ Nivel ≥ 3` | Cotejo carácter a carácter contra el catálogo canónico de Matemáticas (agente `normalizador-oficial-mat` o lectura directa) |
| D-11 | Diagnosticidad | §P7 por las 6 familias + al menos una regla relacional, exceso ≤ +5,3 pp | `verificar_bateria_p7.R` + `validar_diagnosticidad.R` (N=100); `WARN_DIAG_INDET` ≠ PASS |
| D-12 | Diversidad | La clave varía de forma sustantiva | `validar_diversidad_sustantiva.R` (N=100), PASS |
| D-13 | Texto | Tildes, ningún glifo que rompa pdflatex, narrativa no mecánica (#11) | `corregir_ortografia_espanol.R` + `validar_glifos_latex.R` + lectura humana |
| D-14 | Accesibilidad | El alt de las opciones nombra solo el tipo; el del enunciado y la Solution lleva los datos | grep de `alt_op`/`alt_enun` + HTML renderizado |

## 5. Fase 1 — Objetivos (`/goal`)

Ejecuta `/goal`. Fija el objetivo general y los OE, y asigna a cada OE uno o varios IDs D-n
como criterio. Persiste en `ARCHIVO_RETORNO` (sección «Objetivos») y en la memoria
`MEMORIA_SLUG` + `MEMORY.md`, con fechas absolutas.
**Puerta G1:** todo OE apunta a checks D-n con comando.

## 6. Fase 2 — Rigor máximo (`/ultra`) y línea base

Ejecuta `/ultra`. **ANTES de tocar nada**, corre D-1…D-14 y guarda la línea base fechada en
`ARCHIVO_RETORNO`: cada check en verde, rojo o sin cobertura, con su cifra.
**Puerta G2:** la línea base está escrita. Sin ella, ninguna mejora es demostrable.

## 7. Fase 3 — Auditoría adversarial independiente

1. **FASE 2C:** `AgenteDetractor` (opus), lanzado **sin `name:`**, distinto de quien escribió
   el `.Rmd`. Revisa los 8 dominios, los overrides firmados y las invariantes locales.
   El reporte es válido solo si termina en `VEREDICTO_DETRACTOR:`. Si no llega, aplica el
   Paso 0 (recuperarlo de la transcripción) y luego 2 reintentos; si sigue sin llegar, escala.
2. **FASE 2C-bis:** `/detractor-hetero` sobre `RMD_SCHOICE`. Complementa al anterior; no lo
   sustituye.
3. Cada hallazgo se **verifica antes de aplicarlo**: el detractor puede inventar código o
   proponer un arreglo con la aritmética mal. Los descartados se justifican en una línea.
4. Un hallazgo de CORRECCIÓN (clave falsa, segunda clave, Solution falsa) significa
   `RECHAZAR`: se corrige y se vuelve a la Fase 2.
**Puerta G3:** 0 CRÍTICAS/ALTAS abiertas, y la batería vuelve a la línea base o mejor.

## 8. Fase 4 — Testing agresivo (intentar romper el ejercicio)

1. **Barrido de semillas:** N=100 en todos los validadores de la Familia A. Además, enumerar
   exhaustivamente el espacio finito (formato de la clave × topología × errores aplicables)
   para buscar segundas claves y colisiones textuales entre distractor y clave.
2. **Casos borde de datos:** valores mínimos y máximos del rango; empates entre casillas;
   totales apilados iguales a algún dato real; E7 cerca de un dato real; la rama canónica.
3. **Render real como lo usa el profesor:** `SemilleroUnico_v2.R` completo (10 preguntas en
   PDF, DOCX y NOPS, más Moodle). Se miran las páginas, no solo el código de salida.
4. **Verificar al verificador:** inyectar a propósito (a) la clave en la letra equivocada,
   (b) un distractor igual a la clave, (c) quitar el `fig_id` de una figura. La batería debe
   detectar los tres; si alguno pasa, se crea el test que lo detecte.
5. **Regresión:** cada defecto nuevo genera un test que falla antes del arreglo y pasa
   después, y se queda en `tests/testthat/`.
6. Cierre: `R_TESTS_FULL=1 Rscript tests/run_all_tests.R` (tarda unos 13 min; correrlo en
   segundo plano). Se lee el **conteo de suites**, no solo el veredicto.
**Puerta G4:** D-1…D-14 en verde con su salida, las 3 inyecciones detectadas y la suite
completa verde.

## 9. Fase 5 — Corregir, optimizar, documentar

| # | Frente | Verificación |
|---|---|---|
| 1 | Código del `.Rmd`: helpers duplicados → familias F1-F6 (regla #21); chunks legibles | Batería completa verde tras cada cambio |
| 2 | Solution: las 6 secciones de la regla #1, coherentes con cada rama (apilada/agrupada, cadenas E1-E7) | Leer 1 PDF por rama |
| 3 | `ejercicio_state.json`: ningún paso sellado sin artefacto; resellar con `workflow-state.sh` (nunca a mano) | `workflow-state.sh status` + auditoría por artefacto |
| 4 | `graficador/SPEC_graficador.md`: refleja el código de hoy (`fig_id`, ejes por formato, paletas) | Cada afirmación coincide con el `.Rmd` |
| 5 | `HANDOFF.md`: plantilla de la §11 | Retomar cuesta una frase |
| 6 | `salida/`: regenerar con el `.Rmd` final; borrar salidas obsoletas solo con confirmación | mtime de `salida/` posterior al último commit del `.Rmd` |
| 7 | Patrones nuevos → `.claude/docs/patrones-errores-conocidos.md` (solo lo 100 % verificado) | Entrada con mensaje exacto, antes/después y tabla por formato |

**Puerta G5:** los 7 frentes verificados.

## 10. Subagentes y economía

- Routing global: búsqueda y validación mecánica → Haiku; implementación, tests y docs →
  Sonnet; detractor y razonamiento adversarial → Opus. Indicar `🧠 [Tier]` en cada lanzamiento.
- Los subagentes se lanzan **sin `name:`**. Sus reportes no son evidencia: hay que
  verificar el artefacto.
- Más de 20 edits semánticos en el mismo archivo → script Python con diccionario y backup.
- Los renders se lanzan sin `rm -rf` encadenado; lo temporal va al scratchpad.

## 11. Fase 6 — Commit, push y retorno

1. **Pathspec explícito**, nunca `git add -A`/`.`: el árbol tiene cambios ajenos
   (teorema-coseno, `tabla_datos.png`, `.nodeterm/`…). Primero un commit del subproyecto
   (`RAIZ`); los cambios de infraestructura (`SOURCES/scripts_validacion/`, `tests/`,
   `.claude/rules`, `.claude/docs`) van en commits separados.
2. `git diff --cached --stat` antes de cada commit. Estilo `tipo(EJERCICIO): descripción`.
3. Push a `RAMA`. Si falla, se reporta textualmente.
4. Reescribe `HANDOFF.md`: disparador, PREGUNTA_AL_RETOMAR, estado en una línea, OE con su
   estado, hecho con SHAs, siguiente paso exacto, bloqueos, tabla de la batería antes y
   después. Actualiza `MEMORIA_SLUG` y `MEMORY.md`.
5. **No promover.** La promoción exige evidencia de aula (Nivel 3); 11/11 no significa
   terminado.

## 12. Definition of Done

- [ ] G1 OE ↔ D-n · [ ] G2 línea base · [ ] G3 detractor + hetero sin CRÍTICAS/ALTAS
- [ ] G4 D-1…D-14 verdes + 3 inyecciones detectadas + suite completa verde
- [ ] G5 7 frentes · [ ] G6 commits con pathspec + push · [ ] G7 HANDOFF + memoria

## 13. Anti-patrones (rechazo automático)

❌ «Compila» = correcto · ❌ validar solo con n=1 (la falla de las figuras repetidas solo
aparece con varias copias) · ❌ `grep` con 0 coincidencias tomado como «sin defecto» sin
probar que el patrón ve el caso · ❌ «corregir» la trampa o el 4,5 del impreso · ❌ sellar
`detractor_fase2c` con revisión propia · ❌ perseguir §P7 más allá de 3 pasadas ·
❌ lenguaje minimizador («ninguno significativo») · ❌ declarar listo sin la salida del comando.

## 14. Reporte final

```
✅ CICLO COMPLETO — barras-campeonato-baloncesto-n3
Batería:      D-1…D-14  antes <v/r/sc>  →  después <v/r/sc>
Detractor:    <veredicto> · hetero <veredicto> · hallazgos <n> (<n> aplicados, <n> descartados con motivo)
Inyecciones:  3/3 detectadas · tests de regresión nuevos: <n>
Suite:        <n>/<n> suites · Semillero 10 preguntas revisado: <sí/no>
Commits:      <shas> · Push: <ok/fallo textual>
Fuera:        <qué y por qué>
Retomar:      "Continúa con barras-campeonato-baloncesto-n3"
```
