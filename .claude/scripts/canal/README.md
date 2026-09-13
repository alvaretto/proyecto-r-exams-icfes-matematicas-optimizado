# canal/ — reportes por archivo entre subagente y padre

**Motivo (medido 2026-09-11):** el despertar del carril del padre al ceder el turno con
`sessions_yield` falla con `embedded tool authority is no longer active`; se agotaron los dos
reintentos (30 s y 120 s) en 2 de 2 episodios → **4/4 despertares fallidos**. El canal de mensajes
no es fiable para entregar resultados. El archivo en disco sí: verificado con `agents_wait` +
lectura del archivo y re-ejecución independiente del validador.

## Uso

```bash
export REPORTES_DIR="$(git rev-parse --show-toplevel)/.reportes"   # buzón del flujo ICFES

bash .claude/scripts/canal/paso-pipeline.sh <runId> "<nombre del paso>" -- <comando...>
bash .claude/scripts/canal/reporte-agente.sh read <runId>
```

## Garantías

- **Un paso roto sella reporte igual.** `exit != 0` deja constancia con `estado: fallo`.
- **`exit 0` sin el entregable prometido es `fallo`** (declarar con `PASO_ARTEFACTOS=a:b`).
- **Si el reporte no se puede sellar, el paso no se declara bueno** (exit 3).
- Buzón con escritura atómica (tmp + `mv`), validación de esquema y `pending` para detectar
  subagentes caídos antes de entregar.
