# Script para probar la diversidad de versiones
library(digest)
library(testthat)

# Función generar_datos() (copiada del .Rnw para prueba independiente)
generar_datos <- function() {
  # Contextos aleatorios
  contextos <- c("ahorro", "inversión", "préstamo", "negocio", "mesada", "proyecto")
  contexto <- sample(contextos, 1)
  
  # Nombres aleatorios
  nombres <- c("Natalia", "María", "Carlos", "Ana", "Luis", "Sofia", "Diego", "Laura")
  nombre <- sample(nombres, 1)
  
  # Familiares aleatorios
  familiares <- list(
    c("mamá", "papá"),
    c("abuelo", "abuela"),
    c("tío", "tía"),
    c("padrino", "madrina")
  )
  familiar <- sample(familiares, 1)[[1]]
  
  # Cantidades base aleatorias
  cantidades_base <- c(50000, 75000, 100000, 125000, 150000)
  cantidad_base <- sample(cantidades_base, 1)
  
  # Número de meses aleatorio
  num_meses <- sample(3:6, 1)
  
  # Generar datos para opción 1 (porcentaje constante)
  porcentaje_constante <- sample(c(8, 10, 12, 15), 1)
  
  # Generar datos para opción 2 (porcentajes crecientes)
  progresiones <- list(
    c(2, 4, 16),
    c(1, 3, 20),
    c(3, 5, 18),
    c(2, 6, 14),
    c(1, 4, 18),
    c(3, 4, 17)
  )
  
  if(num_meses == 3) {
    porcentajes_variables <- sample(progresiones, 1)[[1]]
  } else {
    # Para más de 3 meses, generar progresión apropiada
    porcentajes_variables <- c(sample(1:3, 1), sample(3:6, 1), sample(15:20, 1))
    if(num_meses > 3) {
      porcentajes_variables <- c(porcentajes_variables, sample(8:12, num_meses - 3))
    }
  }
  
  # Calcular ahorros acumulados
  ahorros_acumulados <- cumsum(rep(cantidad_base, num_meses))
  
  # Calcular regalos opción 1
  regalos_opcion1 <- ahorros_acumulados * (porcentaje_constante / 100)
  total_regalos_opcion1 <- sum(regalos_opcion1)
  
  # Calcular regalos opción 2
  regalos_opcion2 <- ahorros_acumulados[1:length(porcentajes_variables)] * (porcentajes_variables / 100)
  total_regalos_opcion2 <- sum(regalos_opcion2)
  
  # Determinar respuesta correcta
  opcion_mejor <- if(total_regalos_opcion1 > total_regalos_opcion2) 1 else 2
  
  return(list(
    contexto = contexto,
    nombre = nombre,
    familiar1 = familiar[1],
    familiar2 = familiar[2],
    cantidad_base = cantidad_base,
    num_meses = num_meses,
    porcentaje_constante = porcentaje_constante,
    porcentajes_variables = porcentajes_variables,
    ahorros_acumulados = ahorros_acumulados,
    regalos_opcion1 = regalos_opcion1,
    regalos_opcion2 = regalos_opcion2,
    total_regalos_opcion1 = total_regalos_opcion1,
    total_regalos_opcion2 = total_regalos_opcion2,
    opcion_mejor = opcion_mejor
  ))
}

# Prueba de diversidad
cat("Iniciando prueba de diversidad de versiones...\n")

versiones <- list()
for(i in 1:1000) {
  datos_test <- generar_datos()
  # Crear hash único basado en parámetros clave
  hash_datos <- digest::digest(list(
    datos_test$contexto,
    datos_test$nombre,
    datos_test$familiar1,
    datos_test$familiar2,
    datos_test$cantidad_base,
    datos_test$num_meses,
    datos_test$porcentaje_constante,
    datos_test$porcentajes_variables
  ))
  versiones[[i]] <- hash_datos
  
  if(i %% 100 == 0) {
    cat("Procesadas", i, "versiones...\n")
  }
}

n_versiones_unicas <- length(unique(versiones))
cat("\n=== RESULTADOS ===\n")
cat("Versiones únicas generadas:", n_versiones_unicas, "\n")
cat("Objetivo mínimo: 300\n")
cat("Estado:", if(n_versiones_unicas >= 300) "✅ APROBADO" else "❌ RECHAZADO", "\n")

# Análisis detallado de diversidad
cat("\n=== ANÁLISIS DE DIVERSIDAD ===\n")

# Probar algunos ejemplos
ejemplos <- list()
for(i in 1:10) {
  datos_ejemplo <- generar_datos()
  ejemplos[[i]] <- datos_ejemplo
  cat("Ejemplo", i, ":", datos_ejemplo$nombre, "-", datos_ejemplo$contexto, 
      "- Base:", datos_ejemplo$cantidad_base, "- Meses:", datos_ejemplo$num_meses, "\n")
}

cat("\nPrueba completada.\n")
