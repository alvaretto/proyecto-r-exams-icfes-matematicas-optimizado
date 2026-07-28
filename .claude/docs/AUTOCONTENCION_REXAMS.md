# Auto-Contención en R/exams — Cómo Modularizar Código Sin Romper el Render

## Propósito

Documentar, con evidencia de código fuente verificada, el mecanismo interno que usa
R/exams para copiar y aislar los archivos de un ejercicio antes de renderizarlo. Esto
determina qué patrones de modularización (extraer código a archivos `.R` externos,
reutilizar helpers entre ejercicios) funcionan y cuáles fallan de forma silenciosa o
con errores confusos ("cannot open file", "object not found").

Consultado y verificado: 2026-07-28.

---

## El mecanismo verificado: copia a un directorio temporal + `setwd()`

**Fuente**: `R/xexams.R` del paquete `exams` (CRAN) —
https://github.com/cran/exams/blob/master/R/xexams.R

```r
dir_temp <- if(is.null(tdir)) tempfile() else file_path_as_absolute(tdir)
dir_exrc <- if(is.null(edir)) getwd() else file_path_as_absolute(edir)
file_path <- search_files(file_Rnw, dir_exrc, recursive = !is.null(edir))
file.copy(file_path, file.path(dir_temp, file_Rnw))
setwd(dir_temp)
```

Lectura del bloque:

- `dir_exrc` (el "exercise directory", parámetro `edir` de `xexams()`) es el directorio
  ORIGINAL donde vive el `.Rmd` del ejercicio.
- `dir_temp` (parámetro `tdir`, o un `tempfile()` si no se especifica) es un directorio
  de trabajo **temporal y aislado** que R/exams crea para renderizar.
- R/exams busca el archivo del ejercicio (`file_Rnw`, que en la práctica es el `.Rmd`)
  dentro de `dir_exrc`, lo **copia** (`file.copy`) al directorio temporal, y hace
  `setwd(dir_temp)` — el resto del render ocurre con el cwd apuntando al temporal.

**Consecuencia directa**: solo el archivo del ejercicio se copia. Cualquier otro
archivo `.R`/`.RData`/imagen que NO esté referenciado por el mecanismo oficial de
R/exams **nunca llega al directorio temporal**. Un `source("helper.R")` con ruta
relativa dentro del `.Rmd` falla (o, peor, encuentra silenciosamente un archivo
homónimo obsoleto de un render anterior en el mismo `tempfile()`) porque el helper
nunca se copió junto con el ejercicio.

---

## El mecanismo oficial para incluir archivos adicionales: `include_supplement()`

**Fuente**: documentación oficial del paquete —
https://rdrr.io/rforge/exams/man/include_supplement.html

```r
include_supplement(file, dir = NULL, recursive = FALSE, target = NULL)
```

`include_supplement()` es la función que R/exams expone específicamente para este
caso: copiar un archivo suplementario (código, datos, imágenes) desde el directorio
ORIGINAL del ejercicio hacia el directorio de trabajo temporal actual, ANTES de
usarlo.

Internamente resuelve el directorio de origen con
`.exams_get_internal("xexams_dir_exercises")`, que es precisamente el `edir` del
bloque de `xexams.R` citado arriba — es decir, usa la misma noción de "directorio
original del ejercicio" que el motor de render, no una ruta hardcoded ni el cwd
del momento en que se llama.

### Patrón recomendado para modularizar código de un ejercicio

```r
# Al inicio del chunk data_generation (o antes de usar el helper):
include_supplement("helpers_diagramas.R")   # copia el helper al directorio temporal actual
source("helpers_diagramas.R")               # ya está disponible por nombre base
```

Este patrón funciona sea cual sea el `edir`/`tdir` real usado en la corrida (terminal,
RStudio, `exams2pdf()`, `exams2nops()` multi-ítem, etc.), porque delega la resolución
de rutas al mismo mecanismo interno que usa R/exams para el propio `.Rmd`.

### Patrón que falla (o es frágil)

```r
# ❌ Ruta relativa directa — depende de que el cwd sea el directorio original,
# lo cual deja de ser cierto en cuanto xexams.R hace setwd(dir_temp)
source("helpers_diagramas.R")

# ❌ Ruta absoluta hardcoded — rompe en cualquier máquina/usuario distinto,
# y en exams2nops() con múltiples ítems puede colisionar entre ejecuciones paralelas
source("/home/usuario/mi-proyecto/helpers_diagramas.R")
```

---

## Limitación conocida: incompatibilidad con el validador de diversidad

**Verificado: 2026-07-28**, en el ejercicio
`A-Produccion/01-En-PreDesarrollo/desplazamiento-avion-aeropuerto`.

El patrón recomendado arriba (`include_supplement()` + `source()`) funciona correctamente para
**renderizar** con `xexams()` (`exams2html/pdf/pandoc/nops/moodle`) en los 5 formatos probados.
**Pero rompe** el validador obligatorio de la regla #22
(`.claude/scripts/validar_diversidad_sustantiva.R`):

```r
# Patrón que falla específicamente contra este validador (aunque renderiza bien con xexams()):
include_supplement("helpers_diagramas.R", dir = "R")
source("helpers_diagramas.R")
```

**Por qué**: `validar_diversidad_sustantiva.R` NO usa `xexams()` — evalúa el chunk
`data_generation` de forma aislada para medir la diversidad sustantiva de la respuesta correcta
(ver `.claude/rules/diversidad-sustantiva.md`, regla #22). Su mecanismo (líneas 100-109 del
script) crea un directorio temporal, hace `setwd()` a ese directorio y evalúa el chunk en un
`new.env()`, **fuera** del pipeline de `xexams()`:

```r
tmp <- file.path(tempdir(), paste0("divsust_", Sys.getpid())); dir.create(tmp, ...)
old <- getwd(); on.exit({ setwd(old); unlink(tmp, recursive = TRUE) }, add = TRUE)
for (i in seq_len(n)) {
  setwd(tmp)
  env <- new.env(parent = globalenv())
  ...
  okr <- tryCatch({ suppressWarnings(suppressMessages(eval(expr, envir = env))); TRUE },
                   error = function(e) FALSE)
  setwd(old)
  ...
```

En ese contexto, `include_supplement()` no tiene el estado interno que necesita
(`.exams_get_internal("xexams_dir_exercises")`) porque nunca se llamó dentro de un `xexams()`
real — y falla. Un fallback con `source()` de ruta relativa (`if (file.exists("R/helpers.R"))
source(...) else include_supplement(...)`) **tampoco** funciona: el validador ya hizo
`setwd(tmp)` antes de evaluar el chunk, así que la ruta relativa se resuelve contra el directorio
temporal vacío, no contra el directorio del ejercicio.

**Estado actual**: **bloqueado**. Mientras el validador no soporte ejercicios modularizados, todo
ejercicio con opciones gráficas generadas dinámicamente debe permanecer **auto-contenido** (sin
`source()`/`include_supplement()` de helpers propios en `data_generation`). Ver
`A-Produccion/01-En-PreDesarrollo/desplazamiento-avion-aeropuerto/docs/BACKLOG.md` (ítem P1.1)
para el detalle completo del intento, la medición y el criterio de desbloqueo.

---

## Advertencia honesta: hecho verificado vs. deducción

Es importante distinguir dos niveles de certeza en este documento:

1. **Hecho verificado en código fuente**: el bloque de `xexams.R` citado arriba
   (copia selectiva + `setwd()`) y la firma/mecanismo de `include_supplement()`
   están confirmados contra el código y la documentación oficial del paquete.
2. **Deducción, no cita textual**: la documentación oficial de r-exams.org **no
   contiene una advertencia explícita en prosa** del tipo "no uses `source()` con
   ruta relativa directa". La recomendación de usar `include_supplement()` en vez
   de `source()` directo es una **conclusión derivada** del mecanismo de copia
   verificado (punto 1), no una cita textual de un manual que lo prohíba
   explícitamente.

Si en el futuro aparece una fuente oficial que contradiga esta deducción (por
ejemplo, algún modo de `xexams()` que preserve el cwd original), esta sección debe
revisarse.

---

## Relación con las reglas del repositorio

- **Regla `codigo-rmd.md`** referencia este documento como guía para modularizar
  código común (helpers de gráficos, funciones de validación) entre ejercicios sin
  romper el render en `exams2pdf()`/`exams2nops()`.
- El patrón `include_supplement()` es compatible con la auto-contención exigida
  implícitamente por el resto del pipeline (regla #21, familias de soluciones): los
  helpers de `.claude/scripts/snippets_familias_rmd.R` se **copian dentro** del
  chunk `data_generation` del `.Rmd` en vez de `source()`-arse por ruta, precisamente
  para evitar depender de este mecanismo cuando no es necesario. `include_supplement()`
  es la alternativa correcta cuando SÍ se necesita un archivo externo real (por
  ejemplo, un dataset `.RData`, o un módulo `.R` compartido entre varios ejercicios).

---

## Referencias

- `R/xexams.R` (paquete `exams`, CRAN/rforge mirror en GitHub) —
  https://github.com/cran/exams/blob/master/R/xexams.R
- `include_supplement()` — https://rdrr.io/rforge/exams/man/include_supplement.html
- Regla `.claude/rules/codigo-rmd.md`
- Regla `.claude/rules/familias-soluciones-rmd.md` (regla #21)
- Fecha de consulta de estas fuentes: 2026-07-28

---

**Versión:** 1.0
**Fecha:** 2026-07-28
**Estado:** Documentación de referencia (no es una regla obligatoria per se; ver
`codigo-rmd.md` para la aplicación práctica)
