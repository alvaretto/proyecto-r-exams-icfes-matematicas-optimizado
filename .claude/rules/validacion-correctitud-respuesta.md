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

## Afirmaciones y prosa de la Solution (v1.1, 2026-10-29)
Esos códigos comparan **valores**: no evalúan si un **texto** es verdadero (el arsenal dio 0 errores con dos defectos dentro: una Solution falsa en el 2,84 % de las versiones y una opción que otra lectura volvía verdadera). Por eso:
- **Opciones-afirmación** (`evaluacion_afirmacion`): cada afirmación tiene una **función de verdad pura** sobre los datos de la versión, y `data_generation` lleva `stopifnot(sum(verdaderas) == 1, verdaderas[clave])`. La unicidad se construye (F1) y se verifica enumerando el espacio (si no es finito, N = 100), con un mutante que revierta la construcción.
- **Toda lectura razonable cuenta:** si una frase admite otra lectura que cambia su verdad («fue de N kilos más que» leída como «fue de N kilos»), se reescribe, y la reescritura se mide con la batería: en el ítem 50, «superó en N kilos a» abrió un canal léxico de +18,1 pp y «fue más alto que el de O en N kilos» no.
- **Instancia canónica (v1.2):** el texto del impreso no se reescribe, porque prevalece la regla #24 H-2. Su lectura alternativa se declara como residuo, siempre que la función de verdad confirme que no crea una segunda clave.
- **Afirmaciones absolutas de la Solution** sobre cifras sorteadas («no sale de ningún cálculo», «no está en la tabla») se garantizan por construcción con su `stopifnot`, o se reescriben en términos verificables.

**Versión**: 1.2 (2026-10-29: afirmaciones y prosa de la Solution; la canónica conserva el impreso) · compacta desde 2026-09-29 · Excepciones: la instancia canónica, por H-2.
