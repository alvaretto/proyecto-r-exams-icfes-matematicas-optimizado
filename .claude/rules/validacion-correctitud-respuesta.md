# Regla #13 — Validación de Correctitud de Respuesta, Nivel 5 (versión compacta)

> Texto íntegro —variables detectadas, bug EST-MTC-03, modos—: `.claude/docs/reglas/validacion-correctitud-respuesta.md`.

**Principio.** Se verifica automáticamente que la opción marcada ES la correcta, que los distractores son únicos y distintos de ella y que los valores están en rango.

| Código | Qué detecta (todos bloqueantes salvo WARN) |
|---|---|
| `ERR_ANS_A` | `exsolution` dinámico evalúa a formato inválido |
| `ERR_ANS_B` | opción marcada ≠ valor correcto (`sol`, `opciones*`, `valor_correcto`…) |
| `ERR_ANS_C` | opciones duplicadas (o patrón `_neg_` roto) |
| `ERR_ANS_D` | fuera de rango (mediana en [min,max], cuartiles ordenados, prob ∈ [0,1], % ∈ [0,100]) |
| `ERR_ANS_E` | distractor idéntico a la correcta |
| `ERR_SEM_D` / `WARN_SEM_D` | `calcula()` no determinista (seleccionado / latente en el pool) |

Multi-semilla: `Rscript .claude/scripts/validar_multisemilla.R archivo.Rmd --n 100` (FASE 2G del hook; tasa de éxito exigida 100 %). Test: `test_correctitud_respuesta.R`.

**Versión**: 1.0 · compacta desde 2026-09-29 · Excepciones: NINGUNA.
