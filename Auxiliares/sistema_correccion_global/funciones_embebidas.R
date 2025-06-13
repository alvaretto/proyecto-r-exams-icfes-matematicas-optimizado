# ===============================================================================
# FUNCIONES DE CORRECCIÓN EMBEBIDAS PARA ARCHIVOS .RMD
# ===============================================================================
# Archivo: funciones_embebidas.R
# Propósito: Funciones que se pueden incluir directamente en archivos .Rmd
# Uso: Copiar y pegar en el chunk de inicio de archivos .Rmd
# ===============================================================================

# FUNCIONES DE CORRECCIÓN EMBEBIDAS - VERSIÓN MEJORADA
# =====================================================

# ===============================================================================
# SISTEMA DE CONCORDANCIA POR GÉNERO Y NÚMERO - VERSIÓN EMBEBIDA
# ===============================================================================

# Diccionario de elementos por género
elementos_genero_embebido <- list(
  masculinos = c("vehículos", "hogares", "estudiantes", "centros médicos", "hospitales"),
  femeninos = c("familias", "empresas", "instituciones", "organizaciones", "compañías")
)

# Diccionario de adjetivos con formas
adjetivos_formas_embebido <- list(
  "matriculado" = list(masc = "matriculados", fem = "matriculadas"),
  "registrado" = list(masc = "registrados", fem = "registradas"),
  "certificado" = list(masc = "certificados", fem = "certificadas"),
  "beneficiario" = list(masc = "beneficiarios", fem = "beneficiarias"),
  "asegurado" = list(masc = "asegurados", fem = "aseguradas"),
  "acreditado" = list(masc = "acreditados", fem = "acreditadas"),
  "autorizado" = list(masc = "autorizados", fem = "autorizadas"),
  "habilitado" = list(masc = "habilitados", fem = "habilitadas")
)

# Función para obtener la forma correcta de un adjetivo
obtener_forma_adjetivo_embebida <- function(elemento, adjetivo_base) {
  es_femenino <- elemento %in% elementos_genero_embebido$femeninos

  if (adjetivo_base %in% names(adjetivos_formas_embebido)) {
    formas <- adjetivos_formas_embebido[[adjetivo_base]]
    return(if (es_femenino) formas$fem else formas$masc)
  }

  return(adjetivo_base)
}

# Función para crear variables textuales contextuales
crear_variables_textuales_embebida <- function(elemento, condicion) {
  es_femenino <- elemento %in% elementos_genero_embebido$femeninos

  variables <- list(
    elemento = elemento,
    condicion = condicion,

    # Formas específicas para diferentes contextos
    registrados = obtener_forma_adjetivo_embebida(elemento, "registrado"),
    matriculados = obtener_forma_adjetivo_embebida(elemento, "matriculado"),
    certificados = obtener_forma_adjetivo_embebida(elemento, "certificado"),
    beneficiarios = obtener_forma_adjetivo_embebida(elemento, "beneficiario"),
    asegurados = obtener_forma_adjetivo_embebida(elemento, "asegurado"),

    # Información de género para condicionales
    es_femenino = es_femenino,
    es_masculino = !es_femenino
  )

  return(variables)
}

# Función de compatibilidad con versión anterior
corregir_concordancia_genero_embebida <- function(elemento, adjetivo) {
  return(obtener_forma_adjetivo_embebida(elemento, adjetivo))
}

# Función para corregir todos los errores de concordancia
corregir_errores_concordancia_embebida <- function(texto) {
  if (!require(stringr, quietly = TRUE)) {
    # Fallback sin stringr
    texto <- gsub("familias matriculados", "familias matriculadas", texto, fixed = TRUE)
    texto <- gsub("familias registrados", "familias registradas", texto, fixed = TRUE)
    texto <- gsub("familias certificados", "familias certificadas", texto, fixed = TRUE)
    texto <- gsub("empresas matriculados", "empresas matriculadas", texto, fixed = TRUE)
    texto <- gsub("empresas registrados", "empresas registradas", texto, fixed = TRUE)
    texto <- gsub("empresas certificados", "empresas certificadas", texto, fixed = TRUE)
    return(texto)
  }
  
  # Con stringr (preferido)
  errores_comunes <- c(
    "familias matriculados" = "familias matriculadas",
    "familias registrados" = "familias registradas", 
    "familias certificados" = "familias certificadas",
    "familias beneficiarios" = "familias beneficiarias",
    "familias asegurados" = "familias aseguradas",
    "empresas matriculados" = "empresas matriculadas",
    "empresas registrados" = "empresas registradas",
    "empresas certificados" = "empresas certificadas",
    "empresas beneficiarios" = "empresas beneficiarias",
    "empresas asegurados" = "empresas aseguradas"
  )
  
  for (error in names(errores_comunes)) {
    texto <- str_replace_all(texto, fixed(error), errores_comunes[[error]])
  }
  
  return(texto)
}

# Función para validar coherencia básica
validar_coherencia_embebida <- function(entidad, elemento, condicion, total) {
  errores <- c()
  
  # Coherencias básicas
  if (entidad == "ICBF" && elemento != "familias") {
    errores <- c(errores, "ICBF debe manejar familias")
  }
  
  if (entidad == "Ministerio de Transporte" && elemento != "vehículos") {
    errores <- c(errores, "Ministerio de Transporte debe manejar vehículos")
  }
  
  if (entidad == "DANE" && !(elemento %in% c("hogares", "familias"))) {
    errores <- c(errores, "DANE debe manejar hogares o familias")
  }
  
  # Validar rango del total
  if (total < 1000 || total > 50000000) {
    errores <- c(errores, "Total fuera de rango realista")
  }
  
  return(errores)
}

# Función para procesar datos con correcciones embebidas (VERSIÓN MEJORADA)
procesar_datos_con_correcciones_embebida <- function(datos) {
  # Validar coherencia
  errores_coherencia <- validar_coherencia_embebida(datos$entidad, datos$elemento,
                                                   datos$condicion, datos$total)
  if (length(errores_coherencia) > 0) {
    warning("Errores de coherencia detectados: ",
            paste(errores_coherencia, collapse = ", "))
  }

  # Crear variables textuales contextuales (NUEVO SISTEMA)
  variables_texto <- crear_variables_textuales_embebida(datos$elemento, datos$condicion)

  # Agregar variables textuales a los datos
  datos$variables_texto <- variables_texto
  datos$elemento_texto <- variables_texto$elemento
  datos$condicion_texto <- variables_texto$condicion

  # Mantener compatibilidad con versión anterior
  datos$condicion_corregida <- variables_texto$condicion

  return(datos)
}

# Función para cargar sistema con fallback
cargar_sistema_correccion_con_fallback <- function() {
  # Intentar cargar sistema completo
  rutas_posibles <- c(
    "Auxiliares/sistema_correccion_global/funciones_correccion.R",
    "../Auxiliares/sistema_correccion_global/funciones_correccion.R",
    "../../Auxiliares/sistema_correccion_global/funciones_correccion.R",
    "../../../Auxiliares/sistema_correccion_global/funciones_correccion.R"
  )
  
  for (ruta in rutas_posibles) {
    if (file.exists(ruta)) {
      tryCatch({
        source(ruta)
        cat("✅ Sistema completo de corrección cargado desde:", ruta, "\n")
        return(TRUE)
      }, error = function(e) {
        # Continuar con la siguiente ruta
      })
    }
  }
  
  # Si no se puede cargar el sistema completo, usar funciones embebidas
  cat("ℹ️  Usando funciones de corrección embebidas (sistema completo no disponible)\n")
  
  # Crear aliases para compatibilidad
  if (!exists("corregir_concordancia_genero")) {
    corregir_concordancia_genero <<- corregir_concordancia_genero_embebida
  }
  if (!exists("corregir_todos_errores_concordancia")) {
    corregir_todos_errores_concordancia <<- corregir_errores_concordancia_embebida
  }
  if (!exists("validar_coherencia")) {
    validar_coherencia <<- validar_coherencia_embebida
  }
  if (!exists("procesar_datos_con_correcciones")) {
    procesar_datos_con_correcciones <<- procesar_datos_con_correcciones_embebida
  }
  
  return(FALSE)
}

# Mensaje de confirmación
cat("📦 Funciones de corrección embebidas cargadas\n")
cat("💡 Usar cargar_sistema_correccion_con_fallback() para cargar con fallback\n")
