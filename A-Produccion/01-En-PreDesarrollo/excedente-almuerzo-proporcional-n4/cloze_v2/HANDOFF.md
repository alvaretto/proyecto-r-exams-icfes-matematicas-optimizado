# HANDOFF -- excedente_almuerzo_numerico_variacional_argumentacion_n4_cloze_v2

## Origen

Variante CLOZE v2 (6 partes) del item oficial **MAT-2026-1-017** (ERA-2026, Sesion 1,
pregunta 17). Hermana del SCHOICE que vive en el directorio padre. Texto puro, sin
figura. Descriptor **D4.9** (suficiencia de informacion + proporcionalidad).

El v1 vive en `../cloze/` y es la linea base comparable. No fue tocado.

## Que cambio respecto del v1 (ESPECIFICACION_CLOZE_V2.md)

### D1 -- Diagnosticidad (C-4)

Opciones de p3, p5 y p6 redactadas con longitudes parejas por diseno.

| Gap | v1 H1 mas-larga | v2 H1 mas-larga | v1 margen | v2 margen |
|---|---|---|---|---|
| p3 | 100% | 2% | 7% | 9% |
| p5 | 72% | 0% | 12% | 6% |
| p6 | 100% | 0% | 5% | 7% |

Nueva invariante **C-4**: unica-mas-larga <= 60%, unica-mas-corta <= 60%,
margen per-seed < 30% (verificar_render.R). H1 mediano < 15%
(validar_diagnosticidad.R).

### D2 -- Ortografia

0 correcciones automaticas. 1 caso REVISION_MANUAL: linea 596, "como" en
"¿Cuanto debe pagar ... como aporte al excedente?" -- **falso positivo**
(uso comparativo/adverbial "como" = "as", no interrogativo "como" = "how").

### D3 -- Pools ampliados

| Parte | v1 pool | v2 pool | Seleccion v2 |
|---|---|---|---|
| 3 (schoice) | 3 fijos | 5 errores | 3 por version con sample() |
| 4 (mchoice) | 5 fijos | 3 necesarios + 5 no necesarios | 3 + 2 con sample() |
| 5 (schoice) | 4 falsas + 3 verdaderas fijas | 5 falsas + 6 verdaderas | 1 + 3 con sample() |
| 6 (schoice) | 3 fijos por caso | 4 por caso | 3 con sample() |

### D4 -- Verificador

1. Guarda inalcanzable (Fase 4): eliminada. El mutante B ahora corrompe
   TAMBIEN Ttotal (no solo E) para que I-1 recalcule E = Ttotal - P < 0.
2. I-1 recalculada desde el entorno (Ttotal - P), no desde combos.
3. KEY_P5 sondas extraidas del entorno (sel_verdaderas), no transcritas a mano.
4. KEY_P4 verifica por etiqueta de contenido ("total de la cuenta",
   "presupuesto", "consumo individual"), no por cifra formateada (evita falsos
   positivos cuando E coincide numericamente con P o T).

### D5 -- Diversidad por gap

| Gap | Valores unicos / 40 | Estado | Declaracion |
|---|---|---|---|
| p1 (num, E) | 9 | baja | **variable** -- E = T - P varia con los parametros; rango limitado (10 valores distintos en 3948 combos) |
| p2 (num, a) | 15 | ok | **variable** -- a = c*E/T varia con los parametros |
| p3 (schoice) | 17 | ok | **variable** (concepto) -- 5 errores distintos, 3 por version |
| p4 (mchoice) | 40 | ok | **variable** (concepto) -- pool de 8, seleccion de 5 por version |
| p5 (schoice) | 23 | ok | **variable** (concepto) -- 5 falsas x 6 verdaderas, seleccion cruzada |
| p6 (schoice) | 10 | baja | **variable** (concepto) -- 3 valores de dato_retirado x distractores variables |

Cobertura: 6 gaps medidos de 6 declarados. Ningun gap sin medir.

p1 y p6 marcan "baja" por el rango limitado del parametro (E tiene 10 valores
distintos; dato_retirado tiene 3 valores), no porque la seleccion sea fija.
El concepto correcto varia con los parametros.

## Invariantes

### Heredadas (I-1..I-7, I-8 N/A por C-1)

Todas verificadas: 300 semillas, 0 errores.

### Propias del CLOZE (C-1..C-4)

| # | Invariante | Estado |
|---|---|---|
| C-1 | Siempre TIPO 1 (todos los datos dados) | Verificada |
| C-2 | Part 6 aleatoriza dato retirado: T/c/n balanceado | Verificada (T=90, c=107, n=103) |
| C-3 | mchoice Part 4 tiene >=1 TRUE y >=1 FALSE, 5 opciones | Verificada |
| C-4 | Paridad de longitudes gaps schoice (D1) | Verificada (ver tabla arriba) |

## Resultados de verificacion

- **verificar_render.R**: 300 semillas, 0 errores. 4 mutantes, todos cazados
  por su sonda propia.
- **Formatos**: HTML, PDF, DOCX, Moodle OK (4/4). NOPS: N/A (exams2nops
  rechaza extype cloze por diseno).
- **Diversidad**: 300/300 versiones unicas. validar_diversidad_sustantiva.R
  --n 40: 6/6 gaps medidos, PASS (WARN_DIV_BAJA en p1 y p6, justificado
  arriba).
- **Diagnosticidad**: validar_diagnosticidad.R --n 40: PASS. Ninguna sonda
  superficial supera el 70%.
- **Ortografia**: 0 correcciones automaticas. 1 REVISION_MANUAL = falso
  positivo (declarado).
- **Letter-independence**: limpio (0 coincidencias en Solution).

## Mutaciones

| Mutante | Sonda esperada | Sonda real | Veredicto |
|---|---|---|---|
| A (sol_p3 invertido) | KEY_P3 | KEY_P3 | cazado_por_su_sonda |
| B (E negativo post-hoc) | I-1 | I-1 | cazado_por_su_sonda |
| C (dato_retirado=n con No) | KEY_P6 | KEY_P6 | cazado_por_su_sonda |
| D (paridad p3 corrompida) | C-4 | C-4 | cazado_por_su_sonda |

Invariantes sin prueba de deteccion propia: ninguna (las 4 invariantes criticas
tienen mutante propio).

## Destino en 03-En-Produccion (por confirmar)

Candidato (mismo que v1 y el hermano SCHOICE):
`03-En-Produccion/01-Numeros-Reales/Pensamiento-Numerico/07-Proporcionalidad-Directa-E-Inversa/excedente_almuerzo_proporcional_n4/cloze_v2/`.
Falta evidencia Nivel 3 en aula.

## Fecha

2026-08-06
