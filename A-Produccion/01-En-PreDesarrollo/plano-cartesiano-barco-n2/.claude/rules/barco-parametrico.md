# Regla local — Casco paramétrico del barco (`dibujar_barco()`)

**Alcance:** solo `A-Produccion/01-En-PreDesarrollo/plano-cartesiano-barco-n2/`.
**Estado:** ACTIVO Y OBLIGATORIO dentro de ese alcance.

---

## Principio

**El dibujo del barco no es decoración: es el enunciado.** La respuesta correcta del ítem es el
*bounding box* del casco. Por tanto, toda modificación de `dibujar_barco()` debe preservar la
identidad:

```
bounding box del casco dibujado  ==  [x_min, x_max] × [y_min, y_max]  ==  respuesta correcta
```

Si esa identidad se rompe, el ejercicio sigue compilando, sigue pasando los validadores
sintácticos y semánticos, y **entrega una clave falsa al estudiante**. No existe ningún hook ni
test del repo raíz que lo detecte.

---

## El contrato en tres condiciones

Sea `h = y_max - y_min`, `cy = (y_min + y_max)/2`, `t ∈ [0,1]` el parámetro longitudinal.

| # | Condición | Dónde | Consecuencia si se rompe |
|---|---|---|---|
| **C1** | `prof(t) = h/2` para todo `t` en un subintervalo no vacío de `[0,1]` | líneas 225-231 | El casco no toca `y_min`/`y_max`: la clave sobrestima la altura del barco |
| **C2** | `x` recorre linealmente `[x_min, x_max]` cuando `t` recorre `[0,1]` | línea 233 | El casco no toca `x_min`/`x_max`: la clave sobrestima el ancho |
| **C3** | `dibujar_barco()` no llama a `sample()`, `runif()`, `rnorm()` ni `set.seed()` | líneas 215-320 | El dibujo deja de corresponder a los parámetros con que se calculó la clave |

Las capas decorativas (línea interior, ojos de buey, bandas oscuras, puente) **pueden** modificarse
libremente: no participan del *bounding box*. Solo el polígono `hull_df` (línea 235) lo determina.

**Condición de forma (no de corrección), añadida el 2026-07-28.** El espacio de parámetros garantiza
`ratio = ancho/alto ≥ 2` (particularidad 12 de [`../CLAUDE.md`](../CLAUDE.md), invariante I-9). Esa
restricción **no** forma parte del contrato C1-C3 —la clave era correcta también sin ella— pero sí
acota el rango de formas que `dibujar_barco()` debe soportar: ya no tiene que verse bien a
`ratio 1.5`. Si un rediseño futuro del perfil resolviera ese caso (opción B del BACKLOG), la
restricción podría levantarse, pero solo tras re-medir.

---

## Verificación obligatoria tras cualquier cambio en `dibujar_barco()`

**Guarda automática (desde 2026-07-28).** El contrato C1-C2 tiene test de regresión en el repo raíz:

```bash
Rscript tests/run_one_suite.R tests/testthat/test_barco_bbox_invariante.R   # ~2 s
```

No reimplementa el dibujo: **extrae `dibujar_barco()` del `.Rmd` real** (todo lo anterior al
`p <- ggplot()`), lo ejecuta sobre las 318 combinaciones y compara el *bounding box* del casco con la
clave. Verificado por mutación: perturbar el tramo central de `prof()` a `h/2 * 0.97` lo hace fallar
en **318/318** — mientras `validar_coherencia_matematica.R` sigue diciendo APROBADO y
`validar_diversidad_sustantiva.R` sigue diciendo PASS. Está enganchado a `tests/run_all_tests.R`
(suite «Invariante I-2 barco», con `watch`).

> **No confíes solo en el pre-push para esta suite.** El runner corre en modo *quick* durante el
> push y salta las suites con `watch` cuyo patrón no coincida con los archivos detectados. Esa
> detección hoy está **degradada**: `.git/hooks/pre-push` llama a `git lfs pre-push` (git-lfs está
> instalado) **antes** del `while read local_ref local_sha …`, y git-lfs consume el stdin con la
> lista de refs, así que el hook nunca ve los commits que se están empujando. Observado el
> 2026-07-28: en el push de `8bfff28a..b92f2f44` se saltaron las **9** suites con `watch`, esta
> incluida, pese a que los commits tocaban `plano-cartesiano-barco-n2/`. Con la lista correcta la
> suite sí corre (2,12 s, en verde). Ejecútala a mano tras tocar `dibujar_barco()`, o fuerza la
> suite completa con `R_TESTS_FULL=1`.

Si cambias el espacio de parámetros, actualiza `N_ESPERADO` y `combinaciones()` en ese test; sus
anclas de texto sobre el `.Rmd` fallan a propósito para avisarte.

Además, no basta con mirar un PNG. Ejecutar la enumeración exhaustiva del espacio de parámetros:

```r
grid_max <- 10L
prof <- function(t, h) ifelse(t < 0.15, (h/2)*(t/0.15)^0.7,
                       ifelse(t < 0.85, h/2, (h/2)*((1-t)/0.15)^0.5))   # <- copiar del .Rmd
mal <- 0L; n <- 0L
for (ancho in 3:6) {
  alto_pool <- if (ancho >= 4L) 1L:2L else 1L    # A' (P1.1): ratio >= 2
  for (alto in alto_pool) for (x_min in 1:(grid_max - ancho)) {
    x_max <- x_min + ancho
    for (y_min in 1L:(grid_max - alto)) {   # sin exclusiones desde P2.7
      y_max <- y_min + alto; h <- y_max - y_min; cy <- (y_min + y_max)/2
      n <- n + 1L
      tf <- seq(0, 1, length.out = 400)
      yt <- cy + prof(tf, h); yb <- cy - prof(tf, h); xt <- x_min + tf*(x_max - x_min)
      if (!isTRUE(all.equal(max(yt), y_max)) || !isTRUE(all.equal(min(yb), y_min)) ||
          !isTRUE(all.equal(min(xt), x_min)) || !isTRUE(all.equal(max(xt), x_max))) mal <- mal + 1L
    }
  }
}
stopifnot(mal == 0L, n == 318L)   # 318 combinaciones, 0 desajustes (medido 2026-07-28, tras P1.1/A')
```

Si cambias el espacio de parámetros, **actualiza también el `n == 318L`**: es lo que detecta que el
bucle de verificación se ha desincronizado del `.Rmd` y está midiendo un espacio que ya no existe.

Además, inspeccionar visualmente **los dos casos extremos de forma**, no una semilla cualquiera:

| Caso extremo | Parámetros | Qué mirar |
|---|---|---|
| Barco más alargado | `ancho = 6`, `alto = 1` (`ratio 6`) | Que el casco siga siendo reconocible y que ojos de buey / puente no desborden |
| Barco más compacto | `ancho = 4`, `alto = 2` (`ratio 2`) | Que las bandas oscuras y el puente no se solapen entre sí |

> El caso `ancho = 3, alto = 2` (`ratio 1.5`) era el más compacto hasta el 2026-07-28. **Ya no
> pertenece al espacio de versiones** (A′). No usarlo como caso de prueba: verificar una combinación
> inalcanzable da una falsa sensación de cobertura y puede llevar a "arreglar" algo que nadie ve.

---

## Prohibido

- Sustituir el perfil por una curva suave global (elipse, spline, `sin`) que solo **tienda** a `h/2`
  sin alcanzarlo: rompe C1 de forma silenciosa.
- Añadir margen visual al casco (`h/2 * 0.95`, `x_min + 0.1`) "para que respire": rompe C1/C2.
- Mover la llamada `dibujar_barco()` (línea 322) fuera del chunk `data_generation`, o hacerla
  condicional al formato de salida.
- Extraer la función a un archivo externo (ver particularidad 1 de [`../CLAUDE.md`](../CLAUDE.md)).
- Relajar `ratio ≥ 2` "porque el casco ya se ve bien": esa condición se levanta rediseñando el
  perfil (opción B) y re-midiendo, no quitando el `stopifnot` (particularidad 12).

---

**Versión:** 1.3 · **Fecha:** 2026-07-28 (v1.3 — citas de línea y script de verificación
actualizados tras P1.1/A′: `ratio ≥ 2` por construcción, espacio 374 → **318** combinaciones, guarda
`n == 318L` en el script, casos extremos re-definidos a `6×1` y `4×2`; v1.2 — actualizados tras P2.7:
retiradas las exclusiones de `y_pool`, espacio 222 → 374; v1.1 — citas tras sustituir el tercer
distractor y acotar el radio de las bandas)
