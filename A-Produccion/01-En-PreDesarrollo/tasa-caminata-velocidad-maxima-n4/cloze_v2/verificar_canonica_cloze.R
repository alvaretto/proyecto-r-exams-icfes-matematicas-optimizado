## Comprueba que la instancia canónica del CLOZE reproduce MAT-2026-1-047
## VERBATIM. LÍMITE DECLARADO (HANDOFF §8): esto compara contra un texto
## codificado AQUÍ, así que prueba ausencia de DERIVA, no fidelidad. La
## fidelidad la sostiene la lectura visual de `pagina_014.jpg`, hecha por el
## orquestador el 2026-08-22 sobre la pregunta IMPRESA 47.
rmd <- "tasa_caminata_velocidad_maxima_numerico_variacional_argumentacion_n4_cloze_v2.Rmd"
ln  <- readLines(rmd, warn = FALSE)
i0  <- grep("^```\\{r data_generation", ln)[1]
i1  <- i0 + which(grepl("^```\\s*$", ln[(i0+1):length(ln)]))[1]
expr <- parse(text = paste(ln[(i0+1):(i1-1)], collapse="\n"))

ENUN <- paste0(
  "En un conocido juego virtual, una forma de progresar es caminar y que un dispositivo mida ",
  "la distancia recorrida. Lo importante es que debe ser a pie y no en un medio motorizado ",
  "de transporte.\n\n",
  "La empresa administradora del juego, preocupada por la conducta irregular de algunos de ",
  "sus participantes, suspendió la cuenta de uno de sus jugadores por reportar 300",
  " kilómetros caminados en un día, dando como argumento: \"Es conocido que a cualquier ",
  "persona le toma, en trayectos medios a largos, más de 6 minutos caminar un kilómetro\".")
PREG <- "¿Cuál de los siguientes argumentos sustenta la decisión de la suspensión del jugador?"
OPC <- c(
 B = "Lo dicho implica que la velocidad máxima es menos de 10 kilómetros por hora y menos de 240 kilómetros al día; un recorrido mayor corresponde entonces a un medio motorizado.",
 C = "Lo dicho implica que el jugador debe reportar mayor distancia porque un kilómetro se cubre en apenas 6 minutos; un recorrido así, corresponde entonces a un medio motorizado.",
 A = "Lo dicho implica que 300 kilómetros en un día son posibles pero requieren caminar sin descanso y eso es increíble para una persona.",
 D = "Lo dicho implica una falta de relación con los 300 kilómetros reportados por el jugador y es solo un mensaje automático del juego.")

div <- 0L; n_can <- 0L
for (s in 1:600) {
  set.seed(s * 977L)
  e <- new.env(parent = globalenv()); eval(expr, envir = e)
  if (!isTRUE(e$is_canonical)) next
  n_can <- n_can + 1L
  if (!identical(e$enunciado, ENUN))      { div <- div+1L; cat("s",s,"DIVERGE enunciado\n") }
  if (!identical(e$pregunta_p5, PREG))    { div <- div+1L; cat("s",s,"DIVERGE pregunta\n") }
  if (!identical(e$texto_clave, unname(OPC["B"]))) { div <- div+1L; cat("s",s,"DIVERGE clave\n") }
  for (nm in c("C","A","D")) if (!(OPC[[nm]] %in% e$opciones_p5)) {
    div <- div+1L; cat("s",s,"FALTA opcion oficial",nm,"\n") }
  if (e$tasa != 6L || e$H != 24L || e$reporte != 300L || e$vel != 10L || e$cota != 240L) {
    div <- div+1L; cat("s",s,"DIVERGE parametros\n") }
}
cat("instancias canonicas halladas:", n_can, "/ 600 corridas | divergencias:", div, "\n")
cat("veredicto:", if (div == 0L && n_can > 0L) "SIN DERIVA (verbatim conservado)" else "REVISAR", "\n")
