# Librerías requeridas
library(exams)
library(ggplot2)  # Para gráficos avanzados
library(reshape2) # Para manipulación de datos
library(tidyr)    # Para limpieza de datos
library(scales)   # Para mejores escalas en gráficos
library(testthat) # Para pruebas unitarias
library(digest)   # Para pruebas de diversidad de versiones

# Función de generación de datos
generar_datos <- function() {
  # Precio base (sin IVA)
  precio_base <- sample(seq(50000, 100000, by = 1000), 1)

  # Tasa de IVA (19%)
  tasa_iva <- 0.19

  # Calcular el valor del IVA
  valor_iva <- precio_base * tasa_iva

  # Calcular el precio final (con IVA)
  precio_final <- precio_base + valor_iva

  # Retornar los datos
  datos <- list(
    precio_base = precio_base,
    tasa_iva = tasa_iva,
    valor_iva = valor_iva,
    precio_final = precio_final
  )

  return(datos)
}

# Generar datos
# set.seed() debe estar comentado para asegurar verdadera aleatoriedad
# set.seed(123)
datos <- generar_datos()

# Cálculos adicionales y validaciones
# Documentar cada paso
precio_redondeado <- round(datos$precio_final, 0)
