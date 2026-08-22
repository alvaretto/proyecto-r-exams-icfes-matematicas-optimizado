rmd <- "tasa_caminata_velocidad_maxima_numerico_variacional_argumentacion_n4_schoice_v1.Rmd"
ln <- readLines(rmd, warn = FALSE)
i0 <- grep("^```\\{r data_generation", ln)[1]
i1 <- i0 + which(grepl("^```\\s*$", ln[(i0 + 1):length(ln)]))[1]
expr <- parse(text = paste(ln[(i0 + 1):(i1 - 1)], collapse = "\n"))

## --- (1) UNICIDAD DE PRODUCTO (regla #3): 300 intentos ---------------------
firmas <- character(0)
for (s in 1:300) {
  set.seed(s * 31L + 5L); e <- new.env(parent = globalenv()); eval(expr, envir = e)
  firmas <- c(firmas, paste(e$enunciado, paste(e$opciones, collapse = "|"),
                            paste(e$sol, collapse = ""), e$reflexion, sep = "##"))
}
cat("UNICIDAD DE PRODUCTO:", length(unique(firmas)), "/ 300 versiones únicas",
    if (length(unique(firmas)) >= 250L) "-> CUMPLE (umbral 250)" else "-> NO CUMPLE", "\n\n")

## --- (2) INSTANCIA CANÓNICA verbatim --------------------------------------
OFICIAL <- list(
  enun = paste0(
    "En un conocido juego virtual, una forma de progresar es caminar y que un dispositivo mida ",
    "la distancia recorrida. Lo importante es que debe ser a pie y no en un medio motorizado ",
    "de transporte.\n\n",
    "La empresa administradora del juego, preocupada por la conducta irregular de algunos de ",
    "sus participantes, suspendió la cuenta de uno de sus jugadores por reportar 300 kilómetros ",
    "caminados en un día, dando como argumento: \"Es conocido que a cualquier persona le toma, ",
    "en trayectos medios a largos, más de 6 minutos caminar un kilómetro\"."),
  preg = "¿Cuál de los siguientes argumentos sustenta la decisión de la suspensión del jugador?",
  A = "Lo dicho implica que 300 kilómetros en un día son posibles pero requieren caminar sin descanso y eso es increíble para una persona.",
  B = "Lo dicho implica que la velocidad máxima es menos de 10 kilómetros por hora y menos de 240 kilómetros al día; un recorrido mayor corresponde entonces a un medio motorizado.",
  C = "Lo dicho implica que el jugador debe reportar mayor distancia porque un kilómetro se cubre en apenas 6 minutos; un recorrido así, corresponde entonces a un medio motorizado.",
  D = "Lo dicho implica una falta de relación con los 300 kilómetros reportados por el jugador y es solo un mensaje automático del juego.")

hallada <- 0L; fallos <- character(0)
for (s in 1:400) {
  set.seed(s * 7919L); e <- new.env(parent = globalenv()); eval(expr, envir = e)
  if (!isTRUE(e$is_canonical)) next
  hallada <- hallada + 1L
  if (!identical(e$enunciado, OFICIAL$enun)) fallos <- c(fallos, sprintf("s%d: enunciado", s))
  if (!identical(e$pregunta, OFICIAL$preg))  fallos <- c(fallos, sprintf("s%d: pregunta", s))
  for (L in c("A","B","C","D"))
    if (!(OFICIAL[[L]] %in% e$opciones)) fallos <- c(fallos, sprintf("s%d: falta opción %s", s, L))
  if (!identical(e$opciones[e$sol == 1L], OFICIAL$B)) fallos <- c(fallos, sprintf("s%d: la clave no es B", s))
  if (hallada == 1L) {
    cat("--- INSTANCIA CANÓNICA (semilla", s, ") ---\n"); cat(e$enunciado, "\n\n"); cat(e$pregunta, "\n")
    for (o in e$opciones) cat("  * ", o, "\n", sep = "")
    cat("  clave marcada -> ", which(e$sol == 1L), "\n\n", sep = "")
  }
}
cat("instancias canónicas halladas en 400 semillas:", hallada, "| divergencias verbatim:", length(fallos), "\n")
if (length(fallos)) print(unique(fallos))
