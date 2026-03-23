# Pool de Errores Conceptuales — SCHOICE

## Estructura obligatoria de cada error

```r
errores_conceptuales <- list(
  list(
    codigo = "XXX-YYY-01",          # Ej: ALG-OPE-01
    nombre = "Nombre descriptivo",
    descripcion_corta = "...",       # Para opciones (max 80 chars)
    descripcion_larga = "...",       # Para solucion (detallada)
    causa_raiz = "...",              # Diagnostico pedagogico
    precondicion = function(params) TRUE,  # Cuando aplica este error
    calcula = function(datos_ord, datos_presentados = NULL) {
      # Retorna el valor erroneo que produciria este error
      # PROHIBIDO: sample(), runif(), rnorm() — calcula() debe ser funcion pura
    }
  ),
  # Minimo 4-6 errores por ejercicio
)
```

## Seleccion generica por precondiciones (patron obligatorio)

```r
params <- list(n = n, datos_ord = datos_ord)
errores_aplicables_idx <- which(sapply(errores_conceptuales, function(err) {
  if (is.null(err$precondicion)) return(TRUE)
  err$precondicion(params)
}))
error_idx <- sample(errores_aplicables_idx, 1)
error_sel <- errores_conceptuales[[error_idx]]
```

## Taxonomia de codigos

| Prefijo | Area | Ejemplo |
|---------|------|---------|
| ALG | Algebra | ALG-OPE-01 (Inversion de operacion) |
| ARI | Aritmetica | ARI-FRA-01 (Suma fracciones incorrecta) |
| EST | Estadistica | EST-MTC-01 (Confusion medidas centrales) |
| GEO | Geometria | GEO-ARE-01 (Confusion area-perimetro) |
| FUN | Funciones | FUN-PEN-01 (Confusion pendiente-intercepto) |

## Reglas criticas para calcula()

- `calcula()` DEBE ser funcion pura (determinista)
- Firma estandar: `function(datos_ord, datos_presentados = NULL)`
- PROHIBIDO dentro de `calcula()`: `sample()`, `runif()`, `rnorm()`, `rbinom()` u otras `r*()`
- Si el error depende del orden de presentacion, usar `datos_presentados` (no `sample(datos_ord)`)
- La Capa D de validacion semantica detecta no-determinismo automaticamente (`ERR_SEM_D`)

Ver regla completa: `.claude/rules/ejercicios-metacognitivos.md` — seccion "Pool de Errores Conceptuales"
