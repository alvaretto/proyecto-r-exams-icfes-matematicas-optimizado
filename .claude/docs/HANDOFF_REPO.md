# HANDOFF — repositorio (infraestructura `.claude/`, tests y documentación)

> Archivo de retorno para el trabajo que no pertenece a un ejercicio; cada ejercicio lleva su propio
> `HANDOFF.md`. Frase disparadora: «Continúa con el repo». Última actualización: 2026-10-28, al cierre de la tarde.
> Si este archivo y el código discrepan, manda el código: verificar antes de actuar.

## Estado al cierre del 2026-10-28

- Rama `feat/plantillas-oficiales-rexams`, empujada a `origin`; local y remoto en `80d8e0ac`.
- Suite completa: 44/44 antes de los commits de la mañana (1424 s). Después del merge del hook v6.3
  corrió dos veces y dio 45/46 en las dos. Cada vez falló una suite distinta y ninguna por el hook
  (ver «Suite completa tras el merge»).
- Cada push pasó su pre-push en modo rápido. El último ejecutó 18 suites y saltó 28. Las 18 pasaron,
  entre ellas las dos que habían fallado.

| Commit | Hora | Qué cambió |
|---|---|---|
| `54f2a05e` | 05:30 | Hook post-render v6.1: la activación, la 2N y la 2O ya no deciden con `echo \| grep -q` bajo pipefail |
| `f737fbf8` | 09:00 | Hook v6.2: el informe llega al modelo como JSON; aviso si no encuentra el `.Rmd`; 2I-2L con ruta relativa; la 2G dice «100/100» |
| `abf79173` | 09:01 | Tabla de suites generada y vigilada; deriva documental corregida en 31 archivos (índice v3.30.0) |
| `61fe57eb` | 09:08 | MANUAL con cifras vigentes; el Incidente A del orquestador SCHOICE, alineado con la regla #19 |
| `6d4bc9ba` | 09:12 | MANUAL v1.1: hooks reales y comandos que faltaban en la sección 6 |
| `8678846a` | 09:22 | La skill del detractor se registra como `/detractor` (antes `skill-detractor`, y `/detractor` no existía) |
| `9ec30a9a` | 09:28 | HANDOFF de `probabilidad-fichas-tabla-doble-entrada-n2`: versionado e infraestructura |
| `f080964d` … `c622dea0` | 12:35-13:34 | Hook post-render v6.3. Se hizo en un worktree y se integró con merge fast-forward a las 15:42. Ver «Auto-sello del hook post-render» |
| `6f0a4d3a` | 17:07 | CLOZE de `probabilidad-fichas-tabla-doble-entrada-n2`, aprobado para el aula. Su retorno está en `cloze/HANDOFF.md` |
| `80d8e0ac` | 17:37 | `test_cloze_n3.R` sin falsos rojos por empates de redondeo. En los orquestadores: patrón de nombre, 6 partes y pre-flight 14 |

## Qué cambió y dónde está el detalle

### Hook post-render (`.claude/hooks/post-exams2-validation.sh`, v6.2)

- **Corregido después, en la v6.3:** el hook deduce el ejercicio del `.Rmd`, no sella y no escribe en el
  repo. Ver «Auto-sello del hook post-render (corregido el 2026-10-28)», más abajo.
- **El informe llega al modelo.**
  - Con exit 0, Claude Code manda el texto plano de un `PostToolUse` al log de depuración. Hasta la
    v6.1, nadie leía el informe.
  - Ahora el hook escribe un único JSON:
    - `additionalContext` con el informe, de 9 000 caracteres como máximo;
    - `decision: "block"` + `reason` si hay errores;
    - `systemMessage` para el usuario.
  - El informe completo queda en `~/.cache/post-exams2-validation/<nombre>.txt`.
- **Silencio tras un render significa que el hook no corrió.** Las causas posibles:
  - el comando Bash falló: Claude Code dispara `PostToolUseFailure` y el hook no está registrado ahí,
    por decisión;
  - venció el timeout de 300 s: la salida se descarta;
  - el comando no traía un `exams2*("x.Rmd")` literal.

  Si el `.Rmd` no aparece desde el cwd de la sesión, ahora avisa.
- **Detalle y contrato de Claude Code:** `.claude/docs/VALIDATOR_COBERTURA_ICFES.md`, sección del hook.
- **Tests:** `tests/testthat/test_post_exams2_pipefail.R` y `tests/testthat/test_post_exams2_informe.R`
  (suite 44). Este último corre el hook real con un proyecto falso. La v6.3 añade
  `tests/testthat/test_post_exams2_cwd.R` (suite 45).
- **Historial:** `.claude/docs/CHANGELOG_CLAUDE_MD.md`, v3.30.0.

### Documentación atada al código

- `tests/sincronizar_doc_suites.R` genera desde `tests/run_all_tests.R` la tabla y las cifras del
  documento de cobertura (hoy 46 suites y 417 bloques `test_that`). Con `--check` no escribe.
- `tests/testthat/test_doc_suites_sincronizada.R` (suite 46) falla en cuatro casos:
  - si el documento diverge del runner;
  - si las fases del hook no coinciden con las del documento;
  - si la versión del hook no coincide con la del documento;
  - si un documento vivo dice «FASES 2A-2X» y X no es la última fase del hook.
- **Al añadir una suite o un bloque `test_that`: `Rscript tests/sincronizar_doc_suites.R`.** Si no se
  hace, el pre-push falla con esa instrucción.
- **Al añadir una fase al hook:** actualizar la sección del hook del documento de cobertura y los rangos
  «2A-2X» de los documentos vivos. La guardia los enumera.

### Otros cambios del día

- `tests/testthat/test_nomenclatura_rmd.R` ignora `.claude/backups/`. Las copias de la regla #17 daban
  rojo en local y verde en la CI.
- `SOURCES/scripts_validacion/validar_multisemilla.R`: el comentario de cabecera dice el N real (100).
- `.claude/docs/MANUAL_USUARIO.md` v1.1.
- `.claude/agents/orquestador-schoice.md`, Incidente A: la Solution no cita la letra; `exshuffle: TRUE`
  con opciones de texto.
- La skill `.claude/skills/detractor/` (renombrada con `git mv`).
- `.claude/agents/orquestador-schoice.md` y `orquestador-cloze.md`, en `80d8e0ac`:
  - el patrón de nombre del JSON de entrada pasa a `<tema>_<componente>_<competencia>`, y los ejemplos
    llevan `aleatorio`;
  - en el CLOZE, la plantilla del WAIT_USER #1 y el ejemplo del Incidente A muestran 6 partes;
  - el pre-flight 14 busca por el metadato `extype`. El `ls …/**/…` anterior no encontraba nada sin
    `globstar`.

  Los subagentes cargan su definición desde HEAD, así que esto rige desde la próxima sesión.
- `tests/testthat/test_cloze_n3.R`, en `80d8e0ac`: ver «Suite completa tras el merge».

## Auto-sello del hook post-render (corregido el 2026-10-28)

**Causa.** Hasta la v6.2 el hook post-render tomaba el `cwd` de la sesión por la carpeta del ejercicio.
Medido el 2026-10-28 sobre copias, con los validadores reales:

- Tras cualquier render con 0 errores sellaba `renderizado_4_formatos` y `arsenal_post_render` en el
  `ejercicio_state.json` de esa carpeta, sin nota y con 40-80 ms entre los dos sellos. Esos sellos no
  prueban que alguien renderizara los cuatro formatos ni revisara el informe; con la sesión en otra
  carpeta, sellaba el ejercicio equivocado. En la copia de `probabilidad-fichas…` volvió a fechar un sello
  humano (con su nota) 14 horas después.
- Dejaba en el `cwd` el stress test (`stress_test_output/`: 34 archivos y 3,3 MB) y las vistas previas.
- Dejaba allí lo que escriben los chunks: con la sesión en la carpeta de `proceso_recaudacion…n2_v1`,
  sobrescribía su `diagrama_flujo.png`, un hermano que el ejercicio ya tenía, con el `ggsave` de un chunk.

**Corrección (v6.3).** Rama `fix/hook-post-render-cwd`, base `e7402c0d`:

| Commit | Qué cambió |
|---|---|
| `f080964d` | Hook v6.3: `RMD_ABS` manda; corrida en `~/.cache/post-exams2-validation/<nombre>/<corrida>/` (`work/`, `preview/`, `stress_test_output/`, `ORIGEN.txt`, `informe.txt`; se conservan 5); no sella: sugiere el `complete` con nota |
| `4dc5ebdb` | Validadores con el `.Rprofile` y el `.Renviron` del proyecto; `dir=` con borde de palabra; la nota cita `<corrida>/informe.txt` |
| `3eedd9fc` | Suite 45 `test_post_exams2_cwd.R` (7 casos y 6 mutantes entonces) |
| `43d4775a` | `workflow-state.sh status` marca la firma del auto-sello; suite 40 `test_workflow_state_status_sello.R` |
| `c19675c7` | Cabecera con el caso medido; «ya está sellado» en la sugerencia (T8, M11, M12); el rótulo del paso 6 deja de decir «automático via hook» |
| `c622dea0` | Índice 3.31.0, documento de cobertura 1.3, HOOKS_Y_TESTING, `CLAUDE.md` raíz y este archivo |

- **Equivalencia medida** (copias de tres ejercicios reales, v6.2 frente a v6.3): los mismos conteos por
  fase (E1 0 errores y 6 advertencias; E2 1 y 6, por un falso positivo propio de la 2D; E3 0 y 6), el mismo
  informe salvo las rutas de las previews, la línea de salida del stress test y el bloque de sellado, y la
  misma duración (34 s y 35 s). Con la v6.3 las
  copias y el worktree quedan intactos; la v6.2 escribía en ellos.
- **Integrado el 2026-10-28 a las 15:42.**
  - Respaldo de la regla #17: `.claude.pre-hookcwd-20261028-154216.tar.gz`.
  - Merge fast-forward a `c622dea0`.
  - Invariantes I-1 a I-10 en el checkout principal: 214/214. Incluyen I-6b, I-7 e I-8, que el
    worktree no podía comprobar.
  - Subido en `e7402c0d..6f0a4d3a`.
- **Primera prueba en vivo:** sobre el CLOZE de probabilidad-fichas, el arsenal corrió solo, dejó todo
  en la caché y no selló nada.

**Estados con la firma.** Criterio, el mismo que aplica `workflow-state.sh status`: los pasos 5 y 6
completados, el 6 de 40 a 80 ms después del 5 y ninguno de los dos con más claves que `completado` y
`timestamp` (es decir, sin nota). Calculados en el worktree de la rama (base `e7402c0d`) son **19**; en el
checkout principal son **20**. La diferencia es el 20.º, `migraciones-exteriores-lineas-n2/Cloze`, un estado
sin versionar que no está en ningún commit de ninguna rama (y por eso no existe en la base). Otros 5 estados
(6 en el principal) tienen también los dos sellos a 40-80 ms pero con claves extra (`nota`, `formatos`,
`detalle`…): alguien las añadió a mano, y `status` no los marca.

 1. `A-Produccion/01-En-PreDesarrollo/Lentes-radio/ejercicio_state.json`
 2. `A-Produccion/01-En-PreDesarrollo/Pearson/cloze_v1/ejercicio_state.json`
 3. `A-Produccion/01-En-PreDesarrollo/Pearson/cloze_v2/ejercicio_state.json`
 4. `A-Produccion/01-En-PreDesarrollo/Pearson/ejercicio_state.json`
 5. `A-Produccion/01-En-PreDesarrollo/Rango-Colesterol-Pacientes/Cloze/ejercicio_state.json`
 6. `A-Produccion/01-En-PreDesarrollo/SAI2-PS-26/Cloze/ejercicio_state.json`
 7. `A-Produccion/01-En-PreDesarrollo/SAI2-PS-26/Neg/ejercicio_state.json`
 8. `A-Produccion/01-En-PreDesarrollo/SAI2-PS-26/ejercicio_state.json`
 9. `A-Produccion/01-En-PreDesarrollo/area-jardin-lote-porcentaje-n4/cloze/ejercicio_state.json`
10. `A-Produccion/01-En-PreDesarrollo/area-jardin-lote-porcentaje-n4/ejercicio_state.json`
11. `A-Produccion/01-En-PreDesarrollo/excedente-almuerzo-proporcional-n4/cloze/ejercicio_state.json`
12. `A-Produccion/01-En-PreDesarrollo/excedente-almuerzo-proporcional-n4/cloze_v2/ejercicio_state.json`
13. `A-Produccion/01-En-PreDesarrollo/excedente-almuerzo-proporcional-n4/ejercicio_state.json`
14. `A-Produccion/01-En-PreDesarrollo/migraciones-exteriores-lineas-n2/Cloze/ejercicio_state.json` — **sin versionar** (solo existe en el checkout principal)
15. `A-Produccion/01-En-PreDesarrollo/plano-cartesiano-barco-n2/ejercicio_state.json`
16. `A-Produccion/01-En-PreDesarrollo/sistema-ecuaciones-eliminacion-n4/cloze/ejercicio_state.json`
17. `A-Produccion/01-En-PreDesarrollo/tasa-caminata-velocidad-maxima-n4/ejercicio_state.json`
18. `A-Produccion/02-En-Desarrollo/permutaciones-pescadores-venia-n4/cloze/ejercicio_state.json`
19. `A-Produccion/02-En-Desarrollo/permutaciones-pescadores-venia-n4/ejercicio_state.json`
20. `A-Produccion/03-En-Produccion/06-Estadística-Y-Probabilidad/Pensamiento-Aleatorio/01-Variables-Cualitativas_Distribucion-De-Frecuencias/ExportacionesGraficos-Tebailandia/exportaciones_graficos_tebailandia_aleatorio_interpretacion_representacion_n3_v1/ejercicio_state.json`

**Qué hacer con ellos: no se editan.** Al retomar cada ejercicio se re-verifican por artefacto (los cuatro
formatos existen y abren, y el informe del hook se leyó); `workflow-state.sh status <carpeta>` los marca
con ⚠. El estado de `03-En-Produccion/` está en una carpeta inmutable: su re-verificación también es por
artefacto, no por edición. Un sello nuevo lleva `--nota`; el hook v6.3 imprime el comando.

## Suite completa tras el merge (45/46, dos veces)

1. **Primera corrida: `test_nomenclatura_rmd.R:277`.**
   - El worktree del agente vive *dentro* del repo, en `.claude/worktrees/agent-<id>/`, y el test
     barre el árbol.
   - Marcó la copia de un `HANDOFF.md`, porque esa ruta no está en su lista de legados.
   - Con el worktree retirado, y sus dos ramas ya integradas, pasa 25/25.

   **Antes de una suite completa, integrar y retirar los worktrees.**
2. **Segunda corrida: `test_cloze_n3.R:97`, inestable.**
   - El ejercicio, inmutable en `03-En-Produccion`, calcula la respuesta 6 con `round()` de Python.
     En un empate `x.xx5`, esa función redondea al par sobre el valor binario.
   - El test esperaba «.5 hacia arriba» con tolerancia 1e-8. Por eso fallaban 1616 de las 49410
     combinaciones posibles (3,3 %), todas en empates.
   - La semilla no se puede fijar: el ejercicio resiembra el RNG con el reloj (`INC-RNG-RESEED`).
   - Corregido en `80d8e0ac`: el test exige el valor exacto fuera de un empate y acepta cualquiera de los
     dos vecinos en un empate. Fallos con la aserción nueva: 0 de 49410.

La suite completa no se volvió a correr después de `80d8e0ac`. El pre-push de ese commit ejecutó 18
suites, entre ellas las dos de arriba, y todas pasaron.

## Cómo verificar el estado

```bash
R_TESTS_FULL=1 Rscript tests/run_all_tests.R        # suite completa (~24 min el 2026-10-28)
Rscript tests/sincronizar_doc_suites.R --check     # tabla de suites al día
Rscript tests/testthat/test_post_exams2_informe.R  # entrega del informe del hook
Rscript tests/testthat/test_post_exams2_cwd.R      # el ejercicio sale del .Rmd; no escribe en el repo ni sella
```

**Prueba viva del hook, sin efectos laterales en el repo:**

1. Crear un `.Rmd` de prueba **fuera del repo** que haga fallar la 2A (un `stop()` en el chunk de datos).
   Así la 2G y la 2H no corren.
2. Correr `echo '… exams2html("<ruta absoluta del .Rmd>")'`. Tienen que llegar a la sesión el aviso
   «hook blocking error» y el contexto adicional con el informe.
3. El mismo `echo` seguido de `; false` no debe producir nada: es el caso `PostToolUseFailure`.
4. Borrar el `.Rmd` de prueba, su informe y su carpeta de corridas en `~/.cache/post-exams2-validation/`.

## Pendientes (deciden el usuario o el dueño de cada cambio)

1. **Auditoría de arquitectura del 2026-10-26.** Los ADR 002 a 007 siguen en `.claude/docs/ADR/` sin
   commit. Antes de retomarla, revisar sus pendientes con el usuario.
2. **Espejo de skills para Codex** en `.agents/skills/`, sin versionar: conserva el nombre
   `skill-detractor`. Nada lo referencia.
3. **Restos documentales sin corregir:**
   - `REGLAS_CRITICAS.md` afirma «100% de cobertura se mantiene» sin fuente.
   - El bloque de fases de `COMANDOS_Y_SKILLS.md` termina en la 2H: está incompleto, no es falso.
   - `mega-prompt-endurecimiento-orquestadores.md` conserva cifras de su fecha («22 reglas»,
     «I-1..I-9»).
   - El MANUAL cuenta «60+ Ruflo» agentes y «25+ ICFES» skills sin verificar.
4. **Cambios ajenos sin commit en el árbol de trabajo.** No son de esta línea de trabajo y no se tocan
   sin su dueño:
   - `A-Produccion/01-En-PreDesarrollo/informacion-insuficiente-lote-n4/…` y
     `…/teorema-coseno-datos-suficientes-n3/…`;
   - `A-Produccion/perifericos/` (tres `Semillero*.R` y `solpcielo.tex`);
   - `docs/guias/` (dos guías);
   - `tabla_datos.png`;
   - en la raíz, artefactos sin rastrear de junio a octubre (`diagrama_*.png`/`.pdf`, etc.). El
     `stress_test_output/` de la raíz se borró el 2026-10-28: era el stress test del CLOZE de barras
     (2026-10-04), que había dejado ahí el hook v6.2.
5. **Ramas y worktrees ajenos.** Las ramas `worktree-agent-a03756997b0850963` y
   `worktree-agent-a550e94592d0354aa` son de sesiones anteriores. Además hay siete worktrees de Cursor en
   `~/.cursor/worktrees/`. No se tocaron.
6. **Una suite completa limpia (46/46) después de `80d8e0ac`.**

## Respaldos del día (fuera de git, regla #17)

- `.claude/backups/2026-10-28-pipefail-activacion-2o/`: el hook con solo la 2N arreglada, el runner, el
  test anterior de pipefail y el HANDOFF de probabilidad-fichas, antes de `54f2a05e`.
- `.claude/backups/2026-10-28-informe-visible/`: hook v6.1, runner, índices y los 22 documentos del
  barrido. Su README tiene la reversión y los hashes.
- `.claude/backups/2026-10-28-manual-incidente-a/`: MANUAL y orquestador antes de la segunda ronda.
- `.claude/backups/2026-10-28-renombre-detractor/`: la carpeta `skill-detractor/` antes del renombrado.
- En la raíz del repo, ignorados por git:
  - `.claude.pre-hookcwd-20261028-154216.tar.gz`: `.claude/` antes del merge del hook v6.3;
  - `.claude.pre-docfix-20261028-171539.tar.gz`: antes de los arreglos de los orquestadores;
  - `.claude.pre-docfix2-20261028-173626.tar.gz`: antes del pre-flight 14 y del Incidente A.
