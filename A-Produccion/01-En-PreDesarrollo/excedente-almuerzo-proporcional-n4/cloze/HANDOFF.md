# HANDOFF -- excedente_almuerzo_numerico_variacional_argumentacion_n4_cloze_v1

## Origen

Variante CLOZE (6 partes) del item oficial **MAT-2026-1-017** (ERA-2026, Sesion 1,
pregunta 17). Hermana del SCHOICE que vive en el directorio padre. Texto puro, sin
figura. Descriptor **D4.9** (suficiencia de informacion + proporcionalidad).

## Decision de diseno -- Ruta (B)

**Invariante C-1**: El CLOZE siempre usa TIPO 1 (los cuatro parametros P, n, T, c
dados). La suficiencia se evalua en gaps de razonamiento (Partes 3-6), no omitiendo
datos del escenario. Justificacion: Ruta (A) era inviable para los gaps `num` porque
en TIPO 2a el excedente E no es computable (T omitido) y en TIPO 2b el aporte a no es
computable (c omitido).

## Estructura

| Parte | Tipo | Pregunta | Nivel cognitivo |
|---|---|---|---|
| 1 | num | Calcular E = T - P | Aplicar |
| 2 | num | Calcular a = c/T x E | Aplicar |
| 3 | schoice | Justificacion correcta de determinabilidad | Evaluar |
| 4 | mchoice | Datos necesarios para la formula (5 opciones) | Analizar |
| 5 | schoice | Afirmacion INCORRECTA (1 falsa + 3 verdaderas) | Evaluar |
| 6 | schoice | Transferencia: que pasaria si faltara un dato | Transferir |

exclozetype: `num|num|schoice|mchoice|schoice|schoice`

## Parametros

Heredados del hermano SCHOICE. Mismo espacio parametrico: 3948 combinaciones TIPO 1.

| Parametro | Descripcion | Rango |
|---|---|---|
| P | Presupuesto de la entidad | 150.000--600.000 (mult. 50.000) |
| n | Numero de miembros | {5, 6, 8, 10, 12, 15, 20} |
| T | Total de la cuenta | P+50.000 -- min(P+500.000, 1.200.000) (mult. 50.000) |
| c | Consumo individual | mult. 500, < q = P/n |
| a | Aporte proporcional | c*E/T, entero, > 0 |
| dato_retirado | Dato hipotetico faltante (Parte 6) | {T, c, n} (~33% cada uno) |

## Invariantes

### Heredadas (I-1..I-8)

| # | Invariante | Estado |
|---|---|---|
| I-1 | E = T - P > 0 | Verificada (300 semillas, 0 errores) |
| I-2 | q = P/n entero y c < q | Verificada |
| I-3 | a = c*E/T entero | Verificada |
| I-4 | a > 0 | Verificada |
| I-5 | P, T mult. 50.000; c mult. 500 | Verificada |
| I-6 | Instancia canonica (P=300k, n=10, T=500k, c=5k) | Verificada incondicionalmente (seed 4516) |
| I-7 | Sin Unicode problematico | Verificada |
| I-8 | Dato omitido no aparece | N/A (siempre TIPO 1) |

Ninguna invariante heredada fue relajada.

### Propias del CLOZE (C-1..C-3)

| # | Invariante | Estado |
|---|---|---|
| C-1 | Siempre TIPO 1 (todos los datos dados) | Verificada |
| C-2 | Part 6 aleatoriza dato retirado: T/c/n balanceado | Verificada (T=96, c=107, n=97 sobre 300) |
| C-3 | mchoice Part 4 tiene >=1 TRUE y >=1 FALSE, 5 opciones | Verificada |

## Resultados de verificacion

- **verificar_render.R**: 300 semillas, 0 errores (I-1..I-5, I-7, C-1..C-3, KEY_P3,
  KEY_P4, KEY_P5, KEY_P6, unicidad). dato_retirado: T=96, c=107, n=97. E: 10 valores
  unicos. a: 20 valores unicos.
- **I-6 (canonico, incondicional)**: seed 4516. E=200.000, a=2.000. Enunciado verificado.
- **Mutacion A** (clave falsa, sol_p3 invertido): rechazada por KEY_P3.
- **Mutacion B** (E negativo, post-hoc): rechazada por I-1.
- **Mutacion C** (dato_retirado=n con respuesta "No", post-hoc): rechazada por KEY_P6.
- **Contrato de sonda**: cada mutante declara la sonda que debe matarlo; la fase falla
  si muere por una sonda distinta.
- **Formatos**: HTML, PDF, DOCX, Moodle OK (4/4). NOPS: N/A (exams2nops rechaza extype
  cloze por diseno).
- **Diversidad**: 203/300 versiones unicas. `validar_diversidad_sustantiva.R --n 40`
  con modo CLOZE: **6 gaps medidos de 6 declarados, PASS**.
- **Ortografia**: limpia (31 correcciones automaticas aplicadas, 0 residuales).
- **Letter-independence**: limpio (FASE 2J sin coincidencias).
- **pandocbounded**: 0 ocurrencias en .tex (5 semillas PDF).

## Caveats del verificador (calidad del verificador, no del ejercicio)

1. **Guarda de "mutante mal construido" inalcanzable en Fase 4.** El codigo hace
   `env_mut_b$E <- -50000L` y justo despues comprueba `if (env_mut_b$E > 0)`. La
   condicion no puede fallar: se evalua sobre un dato al que se le acaba de forzar la
   propiedad contraria. Es el patron "guarda inalcanzable" (memoria
   `feedback_guarda_inalcanzable`). La obligacion de "morir por su propia sonda" SI hace
   trabajo real; la guarda es vestigial. Tenia sentido contra mutaciones de TEXTO (donde
   un gsub puede no matchear); contra corrupcion post-hoc del entorno, la propagacion
   esta garantizada por construccion.

2. **I-1 es tautologica en la Fase 1.** `E` se lee de `combos`, tabla ya filtrada por
   `E > 0`, asi que sobre las 300 semillas I-1 no puede fallar. El fix de Fase 4
   demuestra que la SONDA I-1 funciona (defensa en profundidad), pero "I-1: 0 errores en
   300 semillas" no debe leerse como que este discriminando algo.

## Destino en 03-En-Produccion (por confirmar)

Candidato (mismo que el hermano SCHOICE):
`03-En-Produccion/01-Numeros-Reales/Pensamiento-Numerico/07-Proporcionalidad-Directa-E-Inversa/excedente_almuerzo_proporcional_n4/cloze/`.
Falta evidencia Nivel 3 en aula.

## Fecha

2026-08-06
