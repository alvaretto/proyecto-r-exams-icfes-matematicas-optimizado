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
| **C1** | `prof(t) = h/2` para todo `t` en un subintervalo no vacío de `[0,1]` | líneas 208-214 | El casco no toca `y_min`/`y_max`: la clave sobrestima la altura del barco |
| **C2** | `x` recorre linealmente `[x_min, x_max]` cuando `t` recorre `[0,1]` | línea 216 | El casco no toca `x_min`/`x_max`: la clave sobrestima el ancho |
| **C3** | `dibujar_barco()` no llama a `sample()`, `runif()`, `rnorm()` ni `set.seed()` | líneas 198-303 | El dibujo deja de corresponder a los parámetros con que se calculó la clave |

Las capas decorativas (línea interior, ojos de buey, bandas oscuras, puente) **pueden** modificarse
libremente: no participan del *bounding box*. Solo el polígono `hull_df` (línea 218) lo determina.

---

## Verificación obligatoria tras cualquier cambio en `dibujar_barco()`

No basta con mirar un PNG. Ejecutar la enumeración exhaustiva del espacio de parámetros:

```r
grid_max <- 10L
prof <- function(t, h) ifelse(t < 0.15, (h/2)*(t/0.15)^0.7,
                       ifelse(t < 0.85, h/2, (h/2)*((1-t)/0.15)^0.5))   # <- copiar del .Rmd
mal <- 0L
for (ancho in 3:6) for (alto in 1:2) for (x_min in 1:(grid_max - ancho)) {
  x_max <- x_min + ancho
  for (y_min in 1L:(grid_max - alto)) {   # sin exclusiones desde P2.7
    y_max <- y_min + alto; h <- y_max - y_min; cy <- (y_min + y_max)/2
    tf <- seq(0, 1, length.out = 400)
    yt <- cy + prof(tf, h); yb <- cy - prof(tf, h); xt <- x_min + tf*(x_max - x_min)
    if (!isTRUE(all.equal(max(yt), y_max)) || !isTRUE(all.equal(min(yb), y_min)) ||
        !isTRUE(all.equal(min(xt), x_min)) || !isTRUE(all.equal(max(xt), x_max))) mal <- mal + 1L
  }
}
stopifnot(mal == 0L)   # 374 combinaciones, 0 desajustes (medido 2026-07-28, tras P2.7)
```

Además, inspeccionar visualmente **los dos casos extremos de forma**, no una semilla cualquiera:

| Caso extremo | Parámetros | Qué mirar |
|---|---|---|
| Barco más aplastado | `ancho = 6`, `alto = 1` | Que el casco siga siendo reconocible y que ojos de buey / puente no desborden |
| Barco más compacto | `ancho = 3`, `alto = 2` | Que las bandas oscuras y el puente no se solapen entre sí |

---

## Prohibido

- Sustituir el perfil por una curva suave global (elipse, spline, `sin`) que solo **tienda** a `h/2`
  sin alcanzarlo: rompe C1 de forma silenciosa.
- Añadir margen visual al casco (`h/2 * 0.95`, `x_min + 0.1`) "para que respire": rompe C1/C2.
- Mover la llamada `dibujar_barco()` (línea 305) fuera del chunk `data_generation`, o hacerla
  condicional al formato de salida.
- Extraer la función a un archivo externo (ver particularidad 1 de [`../CLAUDE.md`](../CLAUDE.md)).

---

**Versión:** 1.2 · **Fecha:** 2026-07-28 (v1.2 — citas de línea y script de verificación
actualizados tras P2.7: retiradas las exclusiones de `y_pool`, espacio 222 → 374 combinaciones;
v1.1 — citas tras sustituir el tercer distractor y acotar el radio de las bandas)
