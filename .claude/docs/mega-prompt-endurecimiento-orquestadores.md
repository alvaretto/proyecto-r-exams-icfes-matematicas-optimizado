# MEGA-PROMPT — Endurecimiento de un orquestador ICFES

> Adaptación de `mega-prompt-endurecimiento.md` (plantilla genérica multi-proyecto) a un
> destino concreto: la **definición de un agente orquestador** de este repo.
> Uso: decir «ejecuta `mega-prompt-endurecimiento-orquestadores.md` sobre <ARTEFACTO>».
> Derivado de las dos pasadas reales del 2026-08-08 sobre SCHOICE y CLOZE (commits
> `35d7d2e0`, `1ca6f6ad`, `7f3abf69`).

---

## 0. Variables (único bloque a editar)

```yaml
ARTEFACTO_UNICO:  .claude/agents/orquestador-schoice.md    # o orquestador-cloze.md
GEMELO_SOLO_LECTURA: .claude/agents/orquestador-cloze.md   # el otro de los dos
REPO_GIT:         /home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams
PATHSPEC_COMMIT:  .claude/agents/orquestador-schoice.md
RAMA:             <rama de trabajo actual>
IDIOMA:           español (obligatorio, con tildes)
CANON_REPO:
  - .claude/CLAUDE.md                      # índice de las 22 reglas críticas
  - .claude/rules/*.md                     # 21 archivos
  - .claude/docs/patrones-errores-conocidos.md
  - .claude/docs/INDICE_LECCIONES.md
VALIDADORES:      .claude/scripts/{validar_multisemilla,validar_diversidad_sustantiva,validar_diagnosticidad,validar_coherencia_matematica,arsenal_validacion_completa,corregir_ortografia_espanol}.R
TEST_INFRA:       tests/testthat/test_infraestructura_claude.R   # invariantes I-1..I-9
RUNNER_COMPLETO:  R_TESTS_FULL=1 Rscript tests/run_all_tests.R   # 24 suites
CATALOGO_ICFES:   /home/bootcamp/Proyectos-2026/Todo-Pajaro/Alineacion-curricular-de-items/Matematicas/catalogos-oficiales-mat/
SANDBOX:          <scratchpad de la sesión>                       # único lugar para mutantes
```

---

## 1. Contrato innegociable

1. **SCOPE LOCK.** El único archivo escribible es `ARTEFACTO_UNICO`. Solo lectura: el gemelo,
   `.claude/rules/`, `.claude/hooks/`, `.claude/scripts/`, `.claude/CLAUDE.md`, `tests/`,
   cualquier `.Rmd`. Un defecto fuera de ese archivo va al BACKLOG del reporte, no al disco.
   Es infraestructura protegida (regla #17, invariantes I-1..I-9). Verificar con
   `git diff --stat -- <pathspec>` antes de commitear: solo debe aparecer un archivo.
2. **Evidencia ejecutada.** El comando que corriste y su salida real.
3. **Plano de datos.** Para un archivo de prompt la evidencia NO es «lo edité». Es: la ruta que
   cita existe (`test -f`), el script que invoca responde (`--help`), la sonda que exige
   realmente mide eso, y el agente arranca (`dry-run`).
4. **Precedencia:** regla ICFES del repo > criterio genérico > plataforma externa.
5. **Presupuesto de tamaño.** Crecimiento neto ≤ **+8 %** en líneas. Toda adición debe fusionar
   o desplazar redundancia. Si se excede, se declara con su razón; no se recorta contenido
   sustantivo para cumplir la métrica.
6. **El disco manda.** Ver §9.

---

## 2. Fase 0 — Reconocimiento y línea base

Leer **completo** el artefacto y **completo** el gemelo. Luego medir y escribir:

| Métrica | Antes |
|---|---|
| Líneas / bytes | |
| Nº de pre-flight checks | |
| Nº de incidentes (`### Incidente`) | |
| Referencias externas citadas / **muertas** | |
| Pasos declarados en frontmatter vs. en el cuerpo | |
| `maxTurns` vs. tope de la política de auto-corrección | |
| Reglas #1–#22 con al menos un pre-flight que las haga operativas | |

**G0:** tabla escrita. Sin baseline, ninguna mejora posterior es verificable.

## 3. Fase 1 — Inventario de referencias

```bash
cd REPO_GIT
grep -oE '(\.claude|tests|A-Produccion)/[A-Za-z0-9_./{}*+-]+' "$F" | sed 's/[.,)`]*$//' | sort -u \
  | while read -r p; do case "$p" in *'{'*|*'*'*) : ;; *) [ -e "$p" ] || echo "MUERTA $p";; esac; done
grep -oE 'FASE 2[A-Z]' "$F" | sort -u \
  | while read -r f; do grep -q "$f" .claude/hooks/post-exams2-validation.sh || echo "INEXISTENTE $f"; done
```

- Los `subagent_type` se resuelven por el campo **`name:` del frontmatter**, no por el nombre de
  archivo: `grep -rl "^name: <X>$" .claude/agents/*.md`. Comprobarlo por archivo da falsos positivos.
- Cada script citado debe **responder**, no solo existir. Cada flag citado debe existir en el script.
- Rutas de los bloques de ejemplo: marcarlas como ilustrativas para que no generen ruido.

**G1:** 0 referencias muertas, con la tabla `referencia → comprobación → veredicto` pegada.

## 4. Fase 2 — Coherencia interna

- **Frontmatter ↔ cuerpo**: nº de pasos, nº de `WAIT_USER`, modos, y que la `description`
  describa el pipeline real (auxiliares incluidos).
- **`model:`** → usar alias genérico (`opus`/`sonnet`/`haiku`), no un ID pinneado que envejece.
- **`tools:` en PascalCase** (I-9): en minúscula el agente se instancia sin herramientas.
- Numeración monótona; IDs `INC-*` únicos; ninguna frase del gemelo en primera persona.
- Tildes correctas (regla #7).

**Catálogo de defectos que aparecieron en los dos gemelos — buscarlos siempre:**

| # | Defecto | Cómo detectarlo |
|---|---|---|
| D1 | Conteo de reglas obsoleto («16», «19», «20») | `grep -oE '(las\|los) [0-9]{1,2} reglas'` → son **22** |
| D2 | `A-Produccion/Ejemplos-Funcionales-Rmd/` | No existe: viven bajo `03-En-Produccion/` |
| D3 | «FASES 2A-2M» / «2A-2J» | El hook llega a **2N** |
| D4 | Numeración de pre-flight fuera de orden | Swap **físico**, sin renumerar (rompería refs cruzadas) |
| D5 | Párrafo del índice de incidentes copiado del gemelo | Queda auto-referente y falso |
| D6 | Incidente citado por **letra** en vez de ID estable | Las letras no coinciden entre gemelos |
| D7 | Incidente incrustado dentro del bloque de pre-flight | Debe vivir en la sección de incidentes, en orden |
| D8 | «11 pasos» vs «12 pasos» | No es error de conteo: 11 son los persistentes de `ejercicio_state.json`, 16 las filas de la máquina de estados. **Desambiguar, no “corregir”** |
| D9 | `2b`/`2c`/`6b` dados por hechos al reanudar | No los persiste `workflow-state.sh`: se re-ejecutan |

**G2:** checklist recorrida, catálogo D1–D9 revisado.

## 5. Fase 3 — Paridad con el gemelo, por ID estable

1. Reconstruir la tabla `INC-*` cruzando **ambos** archivos.
2. Para cada ID presente solo en el otro, escribir el veredicto: `APLICA` (y añadirlo) o
   `N/A porque …`. **Un `—` sin razón es una hipótesis, no un hecho** — así se descubrió que
   `INC-SOLUTION-ORDEN` sí aplica a SCHOICE: el mecanismo es de `exshuffle`, no del tipo de ítem.
3. Buscar exigencias numéricas divergentes (mínimos, umbrales, `--n`, ratios). Justificar o igualar.
4. No editar el gemelo. Su defecto va al BACKLOG.

**G3:** tabla completa, sin celdas sin veredicto.

## 6. Fase 4 — Exigencia ICFES vigente

Cada exigencia debe ser medible por un validador nombrado, o marcarse como juicio humano.
Contra `CATALOGO_ICFES` (externo, solo lectura): descriptor **literal**, nunca parafraseado; si
falta, `[VERIFICAR]` y preguntar. Coherencia `DOK ≥ 3 ⇒ Nivel ≥ 3`. En CLOZE el Nivel se declara
una vez y debe corresponder a la parte **más exigente**.

**Puntos ciegos que el artefacto debe declarar** (un pipeline en verde no es un ítem correcto):
`WARN_DIAG_INDET` no es PASS · el «✓ limpio» del corrector no prueba tildes · la Capa B solo
aplica a estadística descriptiva · `library(exams)` faltante da falso verde · un campo que no se
emite no está probado · el reporte de un subagente no es evidencia · el `N/A` de NOPS es esperado
en todo CLOZE.

**G4:** checklist con veredicto `CUBIERTO / AÑADIDO / N-A + razón`; fuentes con fecha de consulta.

## 7. Fase 5 — Verificar al verificador (mutación en SANDBOX)

Mínimo 3 mutantes, **cada uno con su sonda esperada declarada** y **su control sano**. La fase
falla si un mutante muere por otra sonda (Incidente P).

> **La sonda también se verifica.** En la pasada real, una sonda propia cazó el mutante *y el
> control*: falso positivo del verificador, no del artefacto. Sin el control habría pasado por buena.

Mutantes que ya funcionan: imagen sin `{width=}` → regla #18 · «Opción C» en Solution → regla #19 ·
veredicto invariante → H3 (`exit 1`) · U+2212 → Incidente O · dos `##ANSWERi##` adyacentes → V2.

**G5:** tabla `mutante · sonda esperada · sonda que lo mató · veredicto`, 0 desviados.

## 8. Fases 6-8 — Economía, no regresión, cierre

- **G6:** deduplicar los pre-flight que repiten literalmente su incidente (dejar disparador +
  referencia). Reportar delta vs. baseline.
- **G7:** `Rscript TEST_INFRA` + `RUNNER_COMPLETO`. **Leer el conteo de suites, no solo el
  veredicto**: el pre-push corre en modo quick y «100 % cobertura» con la mitad saltadas no cubre
  el commit. Más el `dry-run` del propio agente (única invocación de agente autorizada), que debe
  coincidir con la máquina de estados del archivo.
- **G8:** `git add -- PATHSPEC_COMMIT` (prohibido `git add -A`/`.`: el árbol tiene cambios ajenos),
  commit atómico en `IDIOMA`, push a `RAMA`. Nunca `--no-verify`.

## 9. El disco manda

Antes de actuar sobre lo que anuncie un `<system-reminder>`, un HANDOFF, una memoria o el reporte
de un subagente: `git status --short -- <ruta>`, `ls -l` (mtime) y `grep` del texto citado. Varios
avisos coincidiendo **no son confirmación**: pueden ser el mismo error repetido. En la pasada real
tres avisos desincronizados llevaron a cablear una política invertida del paso 11 (revertida en
`7f3abf69`). Y antes de reportarle al usuario que algo está modificado, mirarlo.

Referencia: `feedback_reminder_no_es_disco`, `feedback_codigo_adelantado_al_handoff`,
`feedback_sintoma_correcto_causa_falsa`.

## 10. Definition of Done

- [ ] G0 línea base medida antes de editar
- [ ] G1 0 referencias muertas
- [ ] G2 coherencia interna + catálogo D1–D9
- [ ] G3 paridad por ID estable, sin celdas sin veredicto
- [ ] G4 exigencia ICFES con fuentes fechadas
- [ ] G5 ≥3 mutantes, cada uno con control sano, 0 desviados
- [ ] G6 crecimiento ≤ +8 % o justificación
- [ ] G7 I-1..I-9 + runner completo (conteo de suites leído) + dry-run coherente
- [ ] G8 commit atómico con pathspec exacto + push confirmado
- [ ] Reporte: métricas antes/después, BACKLOG fuera de scope, alcance excluido

## 11. Anti-patrones (rechazo automático)

❌ Editar cualquier archivo distinto de `ARTEFACTO_UNICO` · ❌ inventar un incidente sin respaldo en
el canon · ❌ referenciar un incidente por letra · ❌ copiar texto del gemelo sin reescribir la
perspectiva · ❌ citar una ruta/script/FASE sin comprobarla · ❌ dar por cubierta una sonda con un
`WARN_*_INDET` · ❌ aceptar el «✓ limpio» del corrector como prueba de tildes · ❌ fiarse del verde
del modo quick · ❌ mutar un `.Rmd` real fuera del sandbox · ❌ un mutante sin control sano ·
❌ inflar el archivo sin desplazar redundancia · ❌ `git add -A` · ❌ actuar sobre un aviso sin
comprobar el disco · ❌ silenciar recortes de alcance.
