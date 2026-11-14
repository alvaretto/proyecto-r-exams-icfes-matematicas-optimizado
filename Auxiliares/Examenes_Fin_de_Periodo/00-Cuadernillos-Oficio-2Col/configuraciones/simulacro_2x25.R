## Humo de un simulacro: DOS sesiones de 25 preguntas, 2 versiones. Mide que los IDs de las dos sesiones no chocan.
## Uso, desde 00-Cuadernillos-Oficio-2Col/:   Rscript generar_cuadernillos.R configuraciones/simulacro_2x25.R --limpiar
## NO es un examen para usar: repite los 11 ejercicios de examen_real_11.R hasta completar 25 en cada sesión
## (lista fija; todos schoice, así el límite de tipos de la hoja NOPS no puede fallar por un ejercicio ajeno).
## La salida queda en salida/simulacro-2x25/sesion-1/ y sesion-2/.
source(file.path(DIR_CONFIG, "examen_real_11.R"), local = TRUE)      # trae ejercicios_11
ejercicios_25 <- rep_len(ejercicios_11, 25)

config <- list(
  titulo      = "Simulacro Saber 11: Matemáticas",
  institucion = "I. E. Pedacito de Cielo, La Tebaida, Quindío",
  fecha       = as.Date("2026-11-13"),
  estudiantes = 2L,
  semilla     = 20261113L,
  sesiones    = list(list(nombre = "Sesión 1", startid = 1L,   ejercicios = ejercicios_25),
                     list(nombre = "Sesión 2", startid = 101L, ejercicios = ejercicios_25)),
  salidas     = c("cuadernillo", "hoja", "clave", "retro", "word"),
  dir_salida  = "../salida/simulacro-2x25"
)
