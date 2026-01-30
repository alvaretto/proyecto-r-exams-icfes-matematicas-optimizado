# Función para generar datos válidos
generate_valid_data <- function(max_attempts = 1000) {
  for (attempt in 1:max_attempts) {
    # Generar números aleatorios
    n_total <- sample(30:50, 1)
    n_A <- sample(15:25, 1)
    n_B <- sample(15:25, 1)
    n_C <- sample(20:30, 1)

    # Calcular intersecciones
    n_AB <- sample(3:8, 1)
    n_AC <- sample(5:10, 1)
    n_BC <- sample(3:8, 1)
    n_all <- sample(2:5, 1)

    # Calcular valores para cada región
    n_only_A <- n_A - n_AB - n_AC + n_all
    n_only_B <- n_B - n_AB - n_BC + n_all
    n_only_C <- n_C - n_AC - n_BC + n_all
    n_AB_only <- n_AB - n_all
    n_AC_only <- n_AC - n_all
    n_BC_only <- n_BC - n_all

    # Calcular el número de personas que no escuchan ninguno
    n_none <- n_total - (n_A + n_B + n_C - n_AB - n_AC - n_BC + n_all)

    # Verificar que todos los valores sean no negativos
    if (all(c(n_only_A, n_only_B, n_only_C, n_AB_only, n_AC_only, n_BC_only, n_all, n_none) >= 0)) {
      # Devolver los valores generados
      return(list(
        n_total = n_total, n_A = n_A, n_B = n_B, n_C = n_C,
        n_AB = n_AB, n_AC = n_AC, n_BC = n_BC, n_all = n_all,
        n_only_A = n_only_A, n_only_B = n_only_B, n_only_C = n_only_C,
        n_AB_only = n_AB_only, n_AC_only = n_AC_only, n_BC_only = n_BC_only,
        n_none = n_none
      ))
    }
  }
  stop("No se pudieron generar datos válidos después de ", max_attempts, " intentos")
}

# Función para generar géneros musicales aleatorios
generate_random_genres <- function() {
  genres <- c("Vallenato", "Salsa", "Merengue", "Reggaeton", "Bachata", "Cumbia",
              "Rap", "Electrónica")
  sample(genres, 3)
}

# Función para generar texto aleatorio con números consistentes
generate_random_text <- function(data, A, B, C) {
  templates <- c(
    "- %d de ellos escuchan %s y %s,\n- %d escuchan %s y %s, y\n- %d %s y %s.",
    "- %d de ellos escuchan solamente %s y %s,\n- %d escuchan %s y %s, y\n- %d %s y %s.",
    "- %d de ellos escuchan %s y %s,\n- %d solo escuchan %s y %s, y\n- %d %s y %s.",
    "- %d de ellos escuchan %s y %s,\n- %d escuchan %s y %s, y\n- %d solamente %s y %s."
  )
  
  template <- sample(templates, 1)
  values <- list(
    c(data$n_AB, data$n_AC, data$n_BC),
    c(data$n_AB_only, data$n_AC, data$n_BC),
    c(data$n_AB, data$n_AC_only, data$n_BC),
    c(data$n_AB, data$n_AC, data$n_BC_only)
  )
  chosen_values <- switch(which(templates == template),
                       values[[1]],
                       values[[2]],
                       values[[3]],
                       values[[4]])
  
  sprintf(
    "Se ha entrevistado a %d adolescentes sobre los géneros musicales que escuchan.\n\n- %d de ellos escuchan %s, \n- %d escuchan %s, y \n- %d %s. \n\nAdemás sabemos que \n\n%s",
    data$n_total, data$n_A, A, data$n_B, B, data$n_C, C,
    sprintf(template, chosen_values[1], A, B, chosen_values[2], A, C, chosen_values[3], B, C)
  )
}