# Regla #9 — Detractor Obligatorio en Fases de Revisión (versión compacta)

> Texto íntegro —dominios detallados, formato de reporte, configuración, mediciones y cambios v1.1→v1.5—: `.claude/docs/reglas/detractor-obligatorio.md`.

**Principio.** El detractor se invoca AUTOMÁTICAMENTE en toda fase de revisión: post-generación, **FASE 2C** (tras 2A matemática y 2B visual, antes de la FASE 3) y pre-promoción. Revisa 8 dominios: código R-exams, pedagógico, visual, gramática, coherencia matemática, ICFES metacognitivo, testing, semántico (Nivel 4). Desde v1.5 también audita **overrides firmados e invariantes locales** del subproyecto, no sólo el `.Rmd`.

## Severidad
- **CORRECCIÓN** (clave falsa, segunda clave, Solution falsa, distractor correcto): binario → `RECHAZAR`.
- **DIAGNOSTICIDAD**: gradual; sólo obliga por encima de +8 pp (§P7-A); siempre con su margen. Agotadas 3 pasadas → `APROBAR_CON_CAMBIOS` con residuo declarado.
- Objeciones CRÍTICAS/ALTAS bloquean FASE 3 y la promoción.

## Independencia del detractor
El detractor DEBE ser un agente **distinto del que escribió o corrigió** el artefacto. Autoevaluación, auditoría propia del coordinador o un detractor que auditó una versión anterior NO son FASE 2C. Canónico: `AgenteDetractor` (`.claude/agents/agente-detractor.md`, opus). Alternativa: `adversario`. **FASE 2C-bis** (`/detractor-hetero`, otra familia de modelos): complementa, nunca sustituye ni sella `detractor_fase2c`; un hallazgo de CORRECCIÓN suyo bloquea igual.

**Spawn SIN `name:`** — con `name` el agente es *teammate* y su reporte no llega (sólo "Spawned successfully"); eso es error de invocación: relanzar, no reclamar.

## Protocolo de no-entrega
El reporte está entregado sólo si su última línea es `VEREDICTO_DETRACTOR: APROBAR | APROBAR_CON_CAMBIOS | RECHAZAR`.
0. **Recuperar antes de reclamar**: buscar el reporte en la transcripción del subagente (`.jsonl`); si está con marcador, la FASE 2C está cumplida.
1. Reclamar al mismo agente (`SendMessage`). 2. Lanzar un agente nuevo. 3. Tras 2 fallos, escalar al usuario.

PROHIBIDO sustituir el detractor por la auditoría propia, sellar `detractor_fase2c` o declarar el ejercicio listo. La revisión propia se permite sólo declarada como **no independiente**, dejando la FASE 2C abierta.

## Citas verificables (v1.6)
Antes del marcador, el bloque `CITAS_DETRACTOR` … `FIN_CITAS_DETRACTOR`: por hallazgo (ID H1, H2…) al menos una línea `ID | ruta:línea[-línea] | fragmento literal` (≥ 15 caracteres), `ID | ruta | AUSENTE: texto` o `ID | SIN_ARCHIVO | razón`; sin hallazgos, `NINGUNA`. `.claude/scripts/verificar_citas_detractor.py` las contrasta con los archivos: como hook `SubagentStop` **no deja terminar** al detractor mientras una cita no coincida (tope de 3 bloqueos, registro en `~/.claude/hooks/logs/citas-detractor.log`), y `detractor-hetero.sh` no acepta un reporte con citas que fallan. Quien recibe un reporte con hallazgos de código lo verifica con ese script antes de actuar; «TOPE ALCANZADO» en el registro = reporte NO entregado. El `settings.json` del repo solo se carga si la sesión arranca en la raíz: por eso el hook está también en `~/.claude/settings.json` (comando en el texto íntegro), y el script evalúa una sola vez si corren las dos copias.

**Tests:** `tests/testthat/test_contrato_detractor.R`, `tests/testthat/test_citas_detractor.R`.

**Versión**: 1.6 · compacta desde 2026-09-29 · Excepciones: NINGUNA.
