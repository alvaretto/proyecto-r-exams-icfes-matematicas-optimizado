# HANDOFF — repositorio (infraestructura `.claude/`, tests y documentación)

> Archivo de retorno para el trabajo que no pertenece a un ejercicio; cada ejercicio lleva su propio
> `HANDOFF.md`. Frase disparadora: «Continúa con el repo». Última actualización: 2026-10-28.
> Si este archivo y el código discrepan, manda el código: verificar antes de actuar.

## Estado al cierre del 2026-10-28

- Rama `feat/plantillas-oficiales-rexams`, empujada a `origin`; local y remoto en `9ec30a9a`.
- Antes de los commits del día corrió la suite completa: 44/44 suites en 1424 s.
- Cada push pasó su pre-push en modo rápido.

| Commit | Hora | Qué cambió |
|---|---|---|
| `54f2a05e` | 05:30 | Hook post-render v6.1: la activación, la 2N y la 2O ya no deciden con `echo \| grep -q` bajo pipefail |
| `f737fbf8` | 09:00 | Hook v6.2: el informe llega al modelo como JSON; aviso si no encuentra el `.Rmd`; 2I-2L con ruta relativa; la 2G dice «100/100» |
| `abf79173` | 09:01 | Tabla de suites generada y vigilada; deriva documental corregida en 31 archivos (índice v3.30.0) |
| `61fe57eb` | 09:08 | MANUAL con cifras vigentes; el Incidente A del orquestador SCHOICE, alineado con la regla #19 |
| `6d4bc9ba` | 09:12 | MANUAL v1.1: hooks reales y comandos que faltaban en la sección 6 |
| `8678846a` | 09:22 | La skill del detractor se registra como `/detractor` (antes `skill-detractor`, y `/detractor` no existía) |
| `9ec30a9a` | 09:28 | HANDOFF de `probabilidad-fichas-tabla-doble-entrada-n2`: versionado e infraestructura |

## Qué cambió y dónde está el detalle

### Hook post-render (`.claude/hooks/post-exams2-validation.sh`, v6.2)

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
  (suite 43). Este último corre el hook real con un proyecto falso.
- **Historial:** `.claude/docs/CHANGELOG_CLAUDE_MD.md`, v3.30.0.

### Documentación atada al código

- `tests/sincronizar_doc_suites.R` genera desde `tests/run_all_tests.R` la tabla y las cifras del
  documento de cobertura (hoy 44 suites y 400 bloques `test_that`). Con `--check` no escribe.
- `tests/testthat/test_doc_suites_sincronizada.R` (suite 44) falla en cuatro casos:
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

## Cómo verificar el estado

```bash
R_TESTS_FULL=1 Rscript tests/run_all_tests.R        # suite completa (~24 min el 2026-10-28)
Rscript tests/sincronizar_doc_suites.R --check     # tabla de suites al día
Rscript tests/testthat/test_post_exams2_informe.R  # entrega del informe del hook
```

**Prueba viva del hook, sin efectos laterales en el repo:**

1. Crear un `.Rmd` de prueba **fuera del repo** que haga fallar la 2A (un `stop()` en el chunk de datos).
   Así la 2G y la 2H no corren.
2. Correr `echo '… exams2html("<ruta absoluta del .Rmd>")'`. Tienen que llegar a la sesión el aviso
   «hook blocking error» y el contexto adicional con el informe.
3. El mismo `echo` seguido de `; false` no debe producir nada: es el caso `PostToolUseFailure`.
4. Borrar el `.Rmd` de prueba y su informe en `~/.cache/post-exams2-validation/`.

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
   - en la raíz, artefactos sin rastrear de junio a octubre (`diagrama_*.png`/`.pdf`,
     `stress_test_output/`, etc.).

## Respaldos del día (fuera de git, regla #17)

- `.claude/backups/2026-10-28-pipefail-activacion-2o/`: el hook con solo la 2N arreglada, el runner, el
  test anterior de pipefail y el HANDOFF de probabilidad-fichas, antes de `54f2a05e`.
- `.claude/backups/2026-10-28-informe-visible/`: hook v6.1, runner, índices y los 22 documentos del
  barrido. Su README tiene la reversión y los hashes.
- `.claude/backups/2026-10-28-manual-incidente-a/`: MANUAL y orquestador antes de la segunda ronda.
- `.claude/backups/2026-10-28-renombre-detractor/`: la carpeta `skill-detractor/` antes del renombrado.
