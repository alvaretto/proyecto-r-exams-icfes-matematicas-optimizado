# HANDOFF -- excedente_almuerzo_numerico_variacional_argumentacion_n4_v1

## Origen

Item oficial **MAT-2026-1-017** (ERA-2026, Sesion 1, pregunta 17). Verbatim de MAT-2026-1-111.
Texto puro, sin figura. Descriptor **D4.9** (suficiencia de informacion).

## Parametros

| Parametro | Descripcion | Rango |
|---|---|---|
| `P` | Presupuesto de la entidad | 150.000 -- 600.000 (mult. 50.000) |
| `n` | Numero de miembros | {5, 6, 8, 10, 12, 15, 20} |
| `T` | Total de la cuenta | P+50.000 -- min(P+500.000, 1.200.000) (mult. 50.000) |
| `c` | Consumo individual | mult. 500, < q = P/n, **<= $15.000** (`C_MAX_VEROSIMIL`) |
| `a` | Aporte proporcional | c*E/T, entero, > 0 (solo TIPO 1) |
| `tipo` | {1, 2a, 2b} | ~33% cada uno |

Espacio enumerado TIPO 1: **2291 combinaciones validas** (auditoria 2026-08-09).

> Eran 3948 mientras `c` solo estaba acotado por `c < q = P/n`. Esa cota es
> matematica, no de verosimilitud: dejaba llegar el consumo hasta **$117.000**
> (mediana $13.500, p90 $32.650) y producia enunciados como "consumio solamente
> un jugo de $51.000". Con `C_MAX_VEROSIMIL = 15.000` la mediana de `c` baja a
> $8.000, se conservan la instancia canonica (c = $5.000) y la cobertura
> completa de P (10), n (7) y T (19), y quedan 2291 combinaciones -- muy por
> encima de las 250 versiones exigidas.

## Decision de diseno -- RUTA (a)

Alterna el caso logico por version:
- **TIPO 1** (determinable, clave "Si"): P, n, T, c dados. a = c/T * (T-P).
- **TIPO 2a** (NO determinable, clave "No"): T omitido. Sin T no hay razon c/T ni excedente.
- **TIPO 2b** (NO determinable, clave "No"): c omitido. Sin c no hay razon c/T.

Omitir n NO es sub-variante valida (a = c/T * E no depende de n). Verificado por la prueba de
mutacion **C**, que construye esa variante prohibida y comprueba que `KEY_SUFF` la rechaza.

## Invariantes

| # | Invariante | Aplica a |
|---|---|---|
| I-1 | E = T - P > 0 | tipo 1, 2b |
| I-2 | q = P/n entero y c < q | tipo 1, 2a |
| I-3 | a = c*E/T entero | tipo 1 |
| I-4 | a > 0 | tipo 1 |
| I-5 | P, T mult. 50.000; c mult. 500 | todos |
| I-6 | Instancia canonica (P=300k, n=10, T=500k, c=5k) reproducible | tipo 1 |
| I-7 | Sin Unicode problematico (U+2212 etc.) | todos |
| I-8 | Dato omitido no aparece en enunciado | tipo 2a, 2b |

## Pool de errores (8)

PRO-SUF-01..08. Seleccion con `sample()` sobre aplicables. Tipo de error varia entre versiones.

> Eran 6 hasta la auditoria del 2026-08-09. El pool NOMINAL cumplia la regla
> (>= 4-6), pero el pool **aplicable por categoria** colapsaba al numero de
> ranuras: la seleccion es 2 "Si" + 2 "No", y en TIPO 1 solo habia 2 errores
> "No" aplicables y en TIPO 2b solo 2 "Si". Resultado medido: **2 combinaciones
> posibles** de distractores en tipo 1 y 2 en tipo 2b (PRO-SUF-02 y 03 aparecian
> en el 100 % de las versiones tipo 1), es decir, en el 67 % de las versiones dos
> de los tres distractores eran fijos. Ningun validador del arsenal lo detecta.
> Con PRO-SUF-07 ("no", supone impuestos/propina no declarados) y PRO-SUF-08
> ("si", confunde la cuota equitativa q = P/n con el aporte proporcional), las
> combinaciones pasan a **9 / 30 / 9** por tipo.

## Nomenclatura y ubicacion (decision del usuario, 2026-08-06)

El archivo se llamo primero `excedente_almuerzo_metacognitivo_argumentacion_n4_schoice_v1.Rmd`,
segun el regex que impone el orquestador
(`^[a-z0-9_]+_metacognitivo_[a-z]+_n[234]_schoice_v[0-9]+$`), y se renombro a
`excedente_almuerzo_numerico_variacional_argumentacion_n4_v1.Rmd` para cumplir la nomenclatura
documentada en `Auxiliares/Estructura-Repositorio/Estructura_Repositorio.md`
(`[tema]_[categoria]_[competencia]_n[nivel]_v[version].Rmd`).

**Consecuencia operativa:** el nombre actual **NO** matchea el regex del orquestador. Si se
relanza `/orquestador-schoice` sobre este directorio, el wrapper emitira un warning de
nomenclatura (es warning, no bloqueo). Tambien diverge del hermano
`permutaciones_pescadores_metacognitivo_formulacion_n4_schoice_v1.Rmd`, que sigue la convencion
de `.claude/`. Las dos fuentes se contradicen; **esta decision es deliberada y a favor de
`Estructura_Repositorio`**, no un descuido. Reconciliar ambos documentos queda como tarea
abierta del repositorio, fuera del alcance de este ejercicio.

Destino reservado en produccion (subtema creado en esta sesion, con `.gitkeep`):
`03-En-Produccion/01-Numeros-Reales/Pensamiento-Numerico/07-Proporcionalidad-Directa-E-Inversa/excedente_almuerzo_proporcional_n4/`.
El ejercicio vive por ahora en `A-Produccion/01-En-PreDesarrollo/excedente-almuerzo-proporcional-n4/`.

## Resultados de verificacion

> **Trazabilidad.** `verificar_render.R` paso por tres rondas de rechazo. En la primera version
> I-6 era condicional (solo se comprobaba si el sorteo sacaba la instancia canonica, ~1 de 3948
> por semilla: nunca corrio) y las dos "mutaciones" no mutaban el artefacto — la A comparaba dos
> cadenas distintas consigo mismas y la B era un chequeo de cobertura. La segunda version corrigio
> I-6 y la mutacion A, anadio `KEY_SUFF`, pero reutilizo la copia del mutante B como mutante C.
> La **Fase 5 actual (mutante C independiente) fue escrita en la sesion principal**, no por el
> orquestador; el `.Rmd` no fue tocado en ninguna de las tres rondas. Salida literal del
> verificador tras el cierre: Fases 0-5 con 0 errores, mutantes A/B/C los tres rechazados.

- **verificar_render.R**: 300 semillas, 0 errores en la bateria (I-1..I-5, I-8 reforzado,
  KEY_CAT, KEY_PHRASE, KEY_SUFF, unicidad, sol). Tipo distribution: 92/101/107 (31/34/36%).
  KEY_SUFF verifica suficiencia real: deriva del TEXTO del enunciado si T y c estan
  presentes (= a determinable) y compara contra la categoria de la clave; independiente
  de la etiqueta tipo.
- **I-6 (canonico, incondicional)**: semilla **13355** produce la instancia canonica
  (era 43914 sobre el espacio de 3948; la Fase 0 la recalcula ahora sobre el espacio
  real leido del `.Rmd`, no sobre una copia de la enumeracion).
  La reproduccion verbatim del item oficial ya **no depende del sorteo**: una
  EXCEPCION CANONICA fija los tres distractores oficiales (PRO-SUF-01/02/03).
  Antes salia bien 107/200 veces (54 %); ahora 200/200.
  Enunciado + 4 opciones verificados caracter por caracter contra ESPECIFICACION.md §3.
  0 errores.
- **Mutacion A (clave falsa)**: copia del .Rmd con `sol` invertido. Rechazada por
  `KEY_PHRASE: tipo=1 sin frase clave`.
- **Mutacion B (fuga I-8)**: copia del .Rmd donde TIPO 2a provee T en el enunciado.
  Rechazada por `I-8_phrase` + `I-8_value` + `KEY_SUFF`.
- **Mutacion C (omision de n en vez de T/c)**: copia **independiente** del .Rmd (no reutiliza
  la del mutante B). Dos modificaciones: (1) se pasa `""` como numero de miembros a
  `ctx$plantilla()`, de modo que `n` desaparece del enunciado; (2) la rama que emite la frase
  del consumo deja de excluir a TIPO 2b, de modo que `c` si aparece. Resultado: el enunciado
  entrega P, T y c —suficientes para a = c/T x (T-P)— y omite `n`, que es irrelevante para la
  formula; la clave "No, porque sin conocer el valor del consumo..." es FALSA. Semilla 4.
  Rechazada especificamente por `KEY_SUFF: datos en enunciado bastan para a, pero clave dice No`.
  La Fase 5 incluye dos guardas de auto-verificacion (`MUTANTE C MAL CONSTRUIDO`) que fallan
  si el mutante no omite `n` o no incluye `c`, para que no pueda "pasar" por no ser el mutante
  que dice ser.

  > **Limitacion conocida de KEY_SUFF.** Detecta la presencia de T y de c mediante las mismas
  > cadenas que usa I-8 (`"total de la cuenta por pagar fue de \$"` y `"consumió solamente"`).
  > Aporta bidireccionalidad (tambien caza una clave "Si" con datos insuficientes) e
  > independencia de la etiqueta `tipo`, pero **no aporta una sonda de deteccion nueva**: si una
  > edicion futura reescribiera esas frases del enunciado, `determinable` pasaria a FALSE de
  > forma espuria. El modo de fallo resultante es ruidoso, no silencioso (una version TIPO 1
  > legitima disparia `KEY_SUFF: datos insuficientes pero clave dice Si`), asi que se detecta;
  > pero quien reescriba el enunciado debe actualizar las dos sondas a la vez.
- **Formatos**: HTML, PDF, DOCX, NOPS, Moodle OK (5/5).
- **Coherencia (validar_coherencia_matematica.R)**: APROBADO (0 errores, Nivel 5A-5E OK,
  Capa A-C OK).
- **Diversidad sustantiva**: `validar_diversidad_sustantiva.R --n 40` devuelve WARN_DIV_INDET
  evaluando 0/40 versiones (exit 0, no bloqueante) porque las opciones son texto y el script
  no sabe hacer fingerprint de opciones de texto; no midio nada. La evidencia de variacion
  sustantiva de la clave es la distribucion de tipos (92/101/107 sobre 300 semillas)
  validada por KEY_CAT/KEY_PHRASE, no el script.
- **Letter-independence**: limpio (FASE 2J sin coincidencias).
- **Ortografia**: limpio (4 errores en comentarios R corregidos; 0 en texto emitido).
- **pandocbounded**: 0 ocurrencias en .tex (5 semillas PDF verificadas).

## Destino en 03-En-Produccion (por confirmar)

Candidato: `03-En-Produccion/01-Numeros-Reales/` (Numerico-variacional / Argumentacion).

## Fecha

2026-08-06
