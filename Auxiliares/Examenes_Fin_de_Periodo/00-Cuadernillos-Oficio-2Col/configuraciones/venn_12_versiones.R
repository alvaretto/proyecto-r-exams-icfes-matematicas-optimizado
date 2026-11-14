## Un cuadernillo en oficio a dos columnas con 12 versiones distintas de un mismo ejercicio (el SCHOICE del diagrama de Venn).
## Uso, desde 00-Cuadernillos-Oficio-2Col/:   Rscript generar_cuadernillos.R configuraciones/venn_12_versiones.R --limpiar
## La salida queda en salida/venn-12-versiones/sesion-1/:
##   cuadernillos/cuadernillo_<ID>.pdf       cuadernillo del ESTUDIANTE (sin retroalimentación)
##   docente/cuadernillo_docente_<ID>.pdf    cuadernillo del DOCENTE (las mismas preguntas, con la clave y la retroalimentación)
##   y además la hoja de respuestas, la clave, la retroalimentación para el celular y el Word.
## Los dos cuadernillos salen siempre juntos, de las mismas semillas.
## El motor solo LEE el .Rmd: no lo modifica ni cambia su md5 firmado.
##
## - 12 versiones en UN cuadernillo: el ejercicio se repite 12 veces y cada pregunta sale con su propia semilla.
##   Con semilla = 20261114 las 12 son distintas (medido el 2026-11-14). Si cambia la semilla, compruébelo:
##     f <- readRDS("salida/venn-12-versiones/sesion-1/firmas.rds"); anyDuplicated(as.vector(f$huella$cuadernillo))   # debe dar 0
## - Para dar a 12 estudiantes un cuadernillo de UNA pregunta, cada uno con su versión: estudiantes = 12L y el ejercicio una sola vez.
##
## partir_preguntas (medido el 2026-11-14 con este ejercicio):
## - TRUE, el valor por defecto: una pregunta puede seguir en la columna o en la página siguiente. Las 12 preguntas ocupan
##   8 caras en el cuadernillo del estudiante (7 con contenido) y 18 en el del docente. Las 12 empiezan en una columna y
##   terminan en la siguiente, y 6 de ellas (las pares) terminan en la página siguiente.
## - FALSE: cada pregunta va entera en una columna. Con este ejercicio NO sirve: la pregunta mide casi una columna, en la
##   primera página (que lleva el encabezado) no cabe y la tinta llega a 7 mm del borde inferior (la guarda C3 lo reprueba),
##   y no ahorra papel: 8 caras igual.
## La condición de impresión que el profesor fijó para este ítem (I-5: las cuatro opciones en una misma página) la cumple
## su propio semillero, que imprime una pregunta por página.
venn <- "../../../../A-Produccion/01-En-PreDesarrollo/diagrama-venn-juegos-mesa-n3/diagrama_venn_juegos_mesa_aleatorio_interpretacion_representacion_n3_schoice_v1.Rmd"

config <- list(
  titulo           = "Diagramas de Venn: juegos de mesa",
  institucion      = "I. E. Pedacito de Cielo, La Tebaida, Quindío",
  fecha            = as.Date("2026-11-14"),
  estudiantes      = 1L,
  semilla          = 20261114L,
  partir_preguntas = TRUE,
  sesiones         = list(list(nombre = "Sesión 1", startid = 1L, ejercicios = rep(venn, 12))),
  salidas          = c("cuadernillo", "hoja", "clave", "retro", "word"),
  dir_salida       = "../salida/venn-12-versiones"
)
