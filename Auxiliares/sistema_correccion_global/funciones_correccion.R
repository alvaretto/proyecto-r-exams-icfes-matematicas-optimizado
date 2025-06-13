# ===============================================================================
# SISTEMA GLOBAL DE CORRECCIÓN SEMÁNTICA Y GRAMATICAL PARA R-EXAMS
# ===============================================================================
# Archivo: funciones_correccion.R
# Propósito: Funciones centralizadas para corrección automática de errores
# Uso: source("Auxiliares/sistema_correccion_global/funciones_correccion.R")
# ===============================================================================

# Cargar librerías necesarias
if (!require(stringr)) install.packages("stringr")
library(stringr)

# ===============================================================================
# DICCIONARIOS DE CORRECCIÓN
# ===============================================================================

# ===============================================================================
# SISTEMA DE CONCORDANCIA POR GÉNERO Y NÚMERO - VERSIÓN MEJORADA
# ===============================================================================

# Diccionario de elementos por género
ELEMENTOS_GENERO <- list(
  masculinos = c("vehículos", "hogares", "estudiantes", "centros médicos",
                "hospitales", "colegios", "bancos", "comercios"),
  femeninos = c("familias", "empresas", "instituciones", "organizaciones",
               "compañías", "entidades", "corporaciones")
)

# Diccionario de adjetivos con formas simplificadas (masculino/femenino plural)
ADJETIVOS_FORMAS <- list(
  "matriculado" = list(masc = "matriculados", fem = "matriculadas"),
  "registrado" = list(masc = "registrados", fem = "registradas"),
  "certificado" = list(masc = "certificados", fem = "certificadas"),
  "beneficiario" = list(masc = "beneficiarios", fem = "beneficiarias"),
  "asegurado" = list(masc = "asegurados", fem = "aseguradas"),
  "acreditado" = list(masc = "acreditados", fem = "acreditadas"),
  "becado" = list(masc = "becados", fem = "becadas"),
  "autorizado" = list(masc = "autorizados", fem = "autorizadas"),
  "habilitado" = list(masc = "habilitados", fem = "habilitadas")
)

# Diccionario de coherencias entidad-elemento
COHERENCIAS_ENTIDAD_ELEMENTO <- list(
  "ICBF" = c("familias"),
  "Ministerio de Transporte" = c("vehículos"),
  "DANE" = c("hogares", "familias"),
  "Ministerio de Educación" = c("estudiantes", "colegios"),
  "Superintendencia Financiera" = c("empresas", "bancos"),
  "Ministerio de Salud" = c("centros médicos", "hospitales"),
  "DIAN" = c("empresas", "comercios"),
  "Superintendencia de Sociedades" = c("empresas", "corporaciones")
)

# ===============================================================================
# FUNCIONES PRINCIPALES DE CORRECCIÓN
# ===============================================================================

#' Obtener la forma correcta de un adjetivo según el elemento
#' @param elemento El sustantivo (ej: "familias", "vehículos")
#' @param adjetivo_base El adjetivo base (ej: "registrado")
#' @return Adjetivo con concordancia correcta
obtener_forma_adjetivo <- function(elemento, adjetivo_base) {
  es_femenino <- elemento %in% ELEMENTOS_GENERO$femeninos

  if (adjetivo_base %in% names(ADJETIVOS_FORMAS)) {
    formas <- ADJETIVOS_FORMAS[[adjetivo_base]]
    return(if (es_femenino) formas$fem else formas$masc)
  }

  return(adjetivo_base)
}

#' Crear variables textuales contextuales con concordancia correcta
#' @param elemento El sustantivo principal
#' @param condicion La condición específica
#' @return Lista con variables textuales contextuales
crear_variables_textuales <- function(elemento, condicion) {
  es_femenino <- elemento %in% ELEMENTOS_GENERO$femeninos

  # Crear todas las formas necesarias
  variables <- list(
    elemento = elemento,
    condicion = condicion,

    # Formas específicas para diferentes contextos
    registrados = obtener_forma_adjetivo(elemento, "registrado"),
    matriculados = obtener_forma_adjetivo(elemento, "matriculado"),
    certificados = obtener_forma_adjetivo(elemento, "certificado"),
    beneficiarios = obtener_forma_adjetivo(elemento, "beneficiario"),
    asegurados = obtener_forma_adjetivo(elemento, "asegurado"),
    acreditados = obtener_forma_adjetivo(elemento, "acreditado"),
    autorizados = obtener_forma_adjetivo(elemento, "autorizado"),
    habilitados = obtener_forma_adjetivo(elemento, "habilitado"),

    # Información de género para condicionales
    es_femenino = es_femenino,
    es_masculino = !es_femenino
  )

  return(variables)
}

# Función de compatibilidad con versión anterior
corregir_concordancia_genero <- function(elemento, adjetivo) {
  return(obtener_forma_adjetivo(elemento, adjetivo))
}

#' Corregir todos los errores de concordancia en un texto
#' @param texto El texto a corregir
#' @return Texto con errores de concordancia corregidos
corregir_todos_errores_concordancia <- function(texto) {
  # Generar dinámicamente todas las correcciones posibles
  errores_concordancia <- list()
  
  # Para cada elemento femenino
  for (elemento_fem in GENEROS_ELEMENTOS$femeninos) {
    for (adj_base in names(ADJETIVOS_FORMAS)) {
      formas <- ADJETIVOS_FORMAS[[adj_base]]
      # Error: elemento femenino + adjetivo masculino plural
      error_key <- paste(elemento_fem, formas$masc_plur)
      correccion <- paste(elemento_fem, formas$fem_plur)
      errores_concordancia[[error_key]] <- correccion
    }
  }
  
  # Aplicar todas las correcciones
  for (error in names(errores_concordancia)) {
    texto <- str_replace_all(texto, fixed(error), errores_concordancia[[error]])
  }
  
  return(texto)
}

#' Validar coherencia entre entidad, elemento y condición
#' @param entidad La entidad gubernamental
#' @param elemento El elemento a evaluar
#' @param condicion La condición específica
#' @param total El número total
#' @return Vector de errores detectados
validar_coherencia <- function(entidad, elemento, condicion, total) {
  errores <- c()
  
  # Validar coherencia entidad-elemento
  if (entidad %in% names(COHERENCIAS_ENTIDAD_ELEMENTO)) {
    if (!(elemento %in% COHERENCIAS_ENTIDAD_ELEMENTO[[entidad]])) {
      errores <- c(errores, paste("Incoherencia:", entidad, "no maneja", elemento))
    }
  }
  
  # Validar rango realista del total
  if (total < 1000 || total > 50000000) {
    errores <- c(errores, "Total fuera de rango realista")
  }
  
  return(errores)
}

#' Aplicar todas las correcciones de estilo y semántica
#' @param texto El texto a corregir
#' @return Texto completamente corregido
aplicar_correcciones_completas <- function(texto) {
  # 1. Corregir errores de concordancia
  texto <- corregir_todos_errores_concordancia(texto)
  
  # 2. Corregir espaciado
  texto <- str_replace_all(texto, "\\s+", " ")
  texto <- str_trim(texto)
  
  # 3. Corregir puntuación
  texto <- str_replace_all(texto, "\\s+\\.", ".")
  texto <- str_replace_all(texto, "\\s+,", ",")
  texto <- str_replace_all(texto, "\\s+:", ":")
  
  # 4. Asegurar mayúscula después de punto
  texto <- str_replace_all(texto, "\\. ([a-z])", ". \\U\\1")
  
  return(texto)
}

# ===============================================================================
# FUNCIONES DE APLICACIÓN AUTOMÁTICA
# ===============================================================================

#' Procesar datos generados aplicando correcciones automáticas (VERSIÓN MEJORADA)
#' @param datos Lista de datos generados por función generar_datos()
#' @return Lista de datos con correcciones aplicadas y variables contextuales
procesar_datos_con_correcciones <- function(datos) {
  # Validar coherencia
  errores_coherencia <- validar_coherencia(datos$entidad, datos$elemento,
                                          datos$condicion, datos$total)
  if (length(errores_coherencia) > 0) {
    warning("Errores de coherencia detectados: ",
            paste(errores_coherencia, collapse = ", "))
  }

  # Crear variables textuales contextuales (NUEVO SISTEMA)
  variables_texto <- crear_variables_textuales(datos$elemento, datos$condicion)

  # Agregar variables textuales a los datos
  datos$variables_texto <- variables_texto
  datos$elemento_texto <- variables_texto$elemento
  datos$condicion_texto <- variables_texto$condicion

  # Mantener compatibilidad con versión anterior
  datos$condicion_corregida <- variables_texto$condicion

  return(datos)
}

#' Mensaje de confirmación de carga del sistema
cat("✅ Sistema Global de Corrección Semántica cargado exitosamente\n")
cat("🆕 VERSIÓN MEJORADA: Sistema de Concordancia por Género y Número\n")
cat("📋 Funciones principales:\n")
cat("   - crear_variables_textuales() [NUEVA - Recomendada]\n")
cat("   - obtener_forma_adjetivo() [NUEVA]\n")
cat("   - procesar_datos_con_correcciones() [MEJORADA]\n")
cat("   - corregir_concordancia_genero() [Compatibilidad]\n")
cat("   - corregir_todos_errores_concordancia()\n")
cat("   - validar_coherencia()\n")
cat("   - aplicar_correcciones_completas()\n")
cat("🎯 Listo para usar en cualquier archivo .Rmd\n")
cat("💡 Usar crear_variables_textuales() para el nuevo sistema mejorado\n\n")
