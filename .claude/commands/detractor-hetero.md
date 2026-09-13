---
description: Segundo detractor de otra familia de modelos (DeepSeek → GLM → GPT → local) sobre un .Rmd o directorio. FASE 2C-bis, complemento del AgenteDetractor
allowed-tools: Bash(bash .claude/scripts/detractor-hetero.sh *), Read, Glob
---
Lanza el **detractor heterogéneo** (FASE 2C-bis) sobre `$ARGUMENTS`.

Qué es: una segunda auditoría adversarial hecha por un modelo de **otra familia** (no Anthropic),
con el mismo prompt de sistema, los mismos 8 dominios y el mismo marcador que el `AgenteDetractor`.
No sustituye la FASE 2C (regla #9): la complementa, porque dos familias distintas no comparten los
mismos puntos ciegos. Detalle y cuándo usarlo: `.claude/rules/detractor-obligatorio.md`
§ «Segundo detractor heterogéneo».

Pasos:

1. Ejecuta con el Bash tool, **en segundo plano** (`run_in_background: true`, puede tardar 5-20 min):
   `bash .claude/scripts/detractor-hetero.sh $ARGUMENTS`
   El script prueba en cascada `deepseek → ollama-cloud → codex → local` y se detiene en el primer
   motor que entrega un reporte cuya última línea es `VEREDICTO_DETRACTOR:`. Opciones útiles:
   `--motor deepseek`, `--modelo deepseek-v4-pro`, `--sin-codex`, `--max-turnos 30`.
2. Cuando termine, la última línea de stdout es la ruta del reporte
   (`<dir>/detractor-hetero-<motor>-<fecha>.md`). Léelo con Read.
3. Verifica mecánicamente que la última línea del reporte sea el marcador. Si el script salió con
   código 3, ningún motor entregó: informa qué motores estaban saturados (lo dice en stderr) y
   **no** audites tú en su lugar (sería revisión propia, no independiente).
4. Resume al usuario las objeciones por severidad, distinguiendo CORRECCIÓN (bloqueante) de
   DIAGNOSTICIDAD (gradual). **No corrijas nada** en este comando: la corrección es otra pasada.
