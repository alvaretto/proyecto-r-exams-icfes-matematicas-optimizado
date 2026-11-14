## Prueba de CAPACIDAD: una sesión de 25 preguntas con 25 versiones (un grupo completo). Se corre UNA vez para medir
## el tiempo total y por etapa; el de un simulacro de 2 sesiones se extrapola (× 2) y se declara como extrapolado.
## Uso, desde 00-Cuadernillos-Oficio-2Col/:   Rscript generar_cuadernillos.R configuraciones/capacidad_1x25.R --limpiar
## NO es un examen para usar: repite los 11 ejercicios de examen_real_11.R hasta completar 25.
source(file.path(DIR_CONFIG, "examen_real_11.R"), local = TRUE)      # trae ejercicios_11

config <- list(
  titulo      = "Simulacro Saber 11: Matemáticas",
  institucion = "I. E. Pedacito de Cielo, La Tebaida, Quindío",
  fecha       = as.Date("2026-11-13"),
  estudiantes = 25L,
  semilla     = 20261113L,
  sesiones    = list(list(nombre = "Sesión 1", startid = 1L, ejercicios = rep_len(ejercicios_11, 25))),
  salidas     = c("cuadernillo", "hoja", "clave", "retro", "word"),
  dir_salida  = "../salida/capacidad-1x25"
)
