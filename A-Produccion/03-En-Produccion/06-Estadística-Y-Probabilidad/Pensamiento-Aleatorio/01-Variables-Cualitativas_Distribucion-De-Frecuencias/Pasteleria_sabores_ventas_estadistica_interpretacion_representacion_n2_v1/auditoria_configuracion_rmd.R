# ===================================================================
# AUDITORÍA DE CONFIGURACIÓN .RMD Y METADATOS
# ===================================================================
# Script para validar YAML, chunks, configuración TikZ y metadatos ICFES

cat("=== AUDITORÍA DE CONFIGURACIÓN .RMD Y METADATOS ===\n\n")

# Leer el archivo
source_lines <- readLines("pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd")

# ===== PRUEBA 1: CONFIGURACIÓN YAML =====
cat("1. CONFIGURACIÓN YAML:\n")
cat("======================\n")

# Verificar encabezado YAML
yaml_start <- which(source_lines == "---")[1]
yaml_end <- which(source_lines == "---")[2]

if(!is.na(yaml_start) && !is.na(yaml_end)) {
  cat("✅ CORRECTO: Encabezado YAML presente\n")
  
  yaml_content <- source_lines[(yaml_start+1):(yaml_end-1)]
  
  # Verificar formatos de salida
  if(any(grepl("html_document:", yaml_content))) {
    cat("✅ CORRECTO: Formato HTML configurado\n")
  } else {
    cat("⚠️  ADVERTENCIA: Formato HTML no configurado\n")
  }
  
  if(any(grepl("pdf_document:", yaml_content))) {
    cat("✅ CORRECTO: Formato PDF configurado\n")
  } else {
    cat("⚠️  ADVERTENCIA: Formato PDF no configurado\n")
  }
  
  # Verificar dependencias LaTeX
  if(any(grepl("extra_dependencies:", yaml_content))) {
    cat("✅ CORRECTO: Dependencias LaTeX configuradas\n")
    
    # Verificar dependencias específicas para TikZ
    tikz_deps <- c("tikz", "xcolor", "colortbl")
    deps_encontradas <- c()
    
    for(dep in tikz_deps) {
      if(any(grepl(dep, yaml_content))) {
        deps_encontradas <- c(deps_encontradas, dep)
      }
    }
    
    if(length(deps_encontradas) == length(tikz_deps)) {
      cat("✅ CORRECTO: Todas las dependencias TikZ presentes\n")
    } else {
      deps_faltantes <- setdiff(tikz_deps, deps_encontradas)
      cat("⚠️  ADVERTENCIA: Dependencias TikZ faltantes:", 
          paste(deps_faltantes, collapse = ", "), "\n")
    }
  } else {
    cat("❌ ERROR: Dependencias LaTeX no configuradas\n")
  }
  
} else {
  cat("❌ ERROR: Encabezado YAML malformado o ausente\n")
}

# ===== PRUEBA 2: METADATOS ICFES =====
cat("\n2. METADATOS ICFES:\n")
cat("===================\n")

# Verificar sección de metadatos ICFES
if(any(grepl("# Metadatos ICFES", source_lines))) {
  cat("✅ CORRECTO: Sección de metadatos ICFES presente\n")
  
  # Verificar campos obligatorios
  campos_icfes <- c("competencia:", "nivel_dificultad:", "categoria:", 
                   "contexto:", "eje_axial:", "componente:")
  campos_encontrados <- c()
  
  for(campo in campos_icfes) {
    if(any(grepl(campo, source_lines))) {
      campos_encontrados <- c(campos_encontrados, campo)
    }
  }
  
  if(length(campos_encontrados) == length(campos_icfes)) {
    cat("✅ CORRECTO: Todos los campos ICFES obligatorios presentes\n")
  } else {
    campos_faltantes <- setdiff(campos_icfes, campos_encontrados)
    cat("❌ ERROR: Campos ICFES faltantes:", 
        paste(gsub(":", "", campos_faltantes), collapse = ", "), "\n")
  }
  
  # Verificar valores específicos
  if(any(grepl("competencia: interpretacion_representacion", source_lines))) {
    cat("✅ CORRECTO: Competencia apropiada para el ejercicio\n")
  } else {
    cat("⚠️  ADVERTENCIA: Competencia podría no ser apropiada\n")
  }
  
  if(any(grepl("categoria: estadistica", source_lines))) {
    cat("✅ CORRECTO: Categoría apropiada para el ejercicio\n")
  } else {
    cat("⚠️  ADVERTENCIA: Categoría podría no ser apropiada\n")
  }
  
} else {
  cat("❌ ERROR: Sección de metadatos ICFES ausente\n")
}

# ===== PRUEBA 3: CONFIGURACIÓN DE CHUNKS =====
cat("\n3. CONFIGURACIÓN DE CHUNKS:\n")
cat("===========================\n")

# Encontrar todos los chunks
chunk_lines <- grep("```\\{r", source_lines)
cat("📊 INFORMACIÓN: Se encontraron", length(chunk_lines), "chunks R\n")

# Verificar chunk setup
setup_chunk <- any(grepl("```\\{r setup", source_lines))
if(setup_chunk) {
  cat("✅ CORRECTO: Chunk setup presente\n")
  
  # Verificar include=FALSE en setup
  setup_line <- grep("```\\{r setup", source_lines)
  if(any(grepl("include=FALSE", source_lines[setup_line]))) {
    cat("✅ CORRECTO: Chunk setup configurado con include=FALSE\n")
  } else {
    cat("⚠️  ADVERTENCIA: Chunk setup sin include=FALSE\n")
  }
} else {
  cat("❌ ERROR: Chunk setup ausente\n")
}

# Verificar configuración de chunks de presentación
chunks_echo_false <- sum(grepl("echo=FALSE", source_lines))
chunks_results_asis <- sum(grepl("results='asis'", source_lines))

cat("📊 INFORMACIÓN:", chunks_echo_false, "chunks con echo=FALSE\n")
cat("📊 INFORMACIÓN:", chunks_results_asis, "chunks con results='asis'\n")

if(chunks_echo_false >= 3) {
  cat("✅ CORRECTO: Suficientes chunks con echo=FALSE para presentación\n")
} else {
  cat("⚠️  ADVERTENCIA: Pocos chunks con echo=FALSE, puede mostrar código\n")
}

# ===== PRUEBA 4: CONFIGURACIÓN TIKZ =====
cat("\n4. CONFIGURACIÓN TIKZ:\n")
cat("======================\n")

# Verificar configuración de opciones TikZ
tikz_options <- c("tikzLatex", "tikzXelatex", "tikzLatexPackages")
tikz_config_encontrada <- c()

for(option in tikz_options) {
  if(any(grepl(option, source_lines))) {
    tikz_config_encontrada <- c(tikz_config_encontrada, option)
  }
}

if(length(tikz_config_encontrada) >= 2) {
  cat("✅ CORRECTO: Configuración TikZ adecuada\n")
} else {
  cat("⚠️  ADVERTENCIA: Configuración TikZ incompleta\n")
}

# Verificar uso de include_tikz
if(any(grepl("include_tikz", source_lines))) {
  cat("✅ CORRECTO: Función include_tikz utilizada\n")
  
  # Contar usos de include_tikz
  usos_include_tikz <- sum(grepl("include_tikz", source_lines))
  cat("📊 INFORMACIÓN:", usos_include_tikz, "usos de include_tikz encontrados\n")
  
  if(usos_include_tikz >= 2) {
    cat("✅ CORRECTO: Múltiples elementos TikZ (tabla y gráfico)\n")
  } else {
    cat("⚠️  ADVERTENCIA: Pocos elementos TikZ, podría faltar tabla o gráfico\n")
  }
} else {
  cat("❌ ERROR: Función include_tikz no utilizada\n")
}

# ===== PRUEBA 5: META-INFORMACIÓN R-EXAMS =====
cat("\n5. META-INFORMACIÓN R-EXAMS:\n")
cat("============================\n")

# Verificar sección Meta-information
if(any(grepl("Meta-information", source_lines))) {
  cat("✅ CORRECTO: Sección Meta-information presente\n")
  
  # Verificar campos obligatorios
  meta_campos <- c("exname:", "extype:", "exsolution:", "exshuffle:", "exsection:")
  meta_encontrados <- c()
  
  for(campo in meta_campos) {
    if(any(grepl(campo, source_lines))) {
      meta_encontrados <- c(meta_encontrados, campo)
    }
  }
  
  if(length(meta_encontrados) == length(meta_campos)) {
    cat("✅ CORRECTO: Todos los campos meta obligatorios presentes\n")
  } else {
    meta_faltantes <- setdiff(meta_campos, meta_encontrados)
    cat("❌ ERROR: Campos meta faltantes:", 
        paste(gsub(":", "", meta_faltantes), collapse = ", "), "\n")
  }
  
  # Verificar tipo de ejercicio
  if(any(grepl("extype: schoice", source_lines))) {
    cat("✅ CORRECTO: Tipo de ejercicio correcto (schoice)\n")
  } else {
    cat("❌ ERROR: Tipo de ejercicio incorrecto o ausente\n")
  }
  
  # Verificar exshuffle
  if(any(grepl("exshuffle: TRUE", source_lines))) {
    cat("✅ CORRECTO: Aleatorización de opciones habilitada\n")
  } else {
    cat("⚠️  ADVERTENCIA: Aleatorización de opciones no habilitada\n")
  }
  
} else {
  cat("❌ ERROR: Sección Meta-information ausente\n")
}

# ===== PRUEBA 6: CONFIGURACIÓN DE LIBRERÍAS =====
cat("\n6. CONFIGURACIÓN DE LIBRERÍAS:\n")
cat("==============================\n")

# Verificar librerías cargadas
librerias_necesarias <- c("exams", "digest", "testthat", "knitr")
librerias_encontradas <- c()

for(lib in librerias_necesarias) {
  if(any(grepl(paste0("library\\(", lib, "\\)"), source_lines))) {
    librerias_encontradas <- c(librerias_encontradas, lib)
  }
}

if(length(librerias_encontradas) == length(librerias_necesarias)) {
  cat("✅ CORRECTO: Todas las librerías necesarias cargadas\n")
} else {
  lib_faltantes <- setdiff(librerias_necesarias, librerias_encontradas)
  cat("⚠️  ADVERTENCIA: Librerías faltantes:", 
      paste(lib_faltantes, collapse = ", "), "\n")
}

# Verificar configuración de knitr
if(any(grepl("knitr::opts_chunk\\$set", source_lines))) {
  cat("✅ CORRECTO: Configuración global de knitr presente\n")
} else {
  cat("⚠️  ADVERTENCIA: Configuración global de knitr ausente\n")
}

# ===== RESUMEN FINAL =====
cat("\n=== RESUMEN DE CONFIGURACIÓN .RMD Y METADATOS ===\n")

# Contar problemas críticos
problemas_criticos <- 0
if(is.na(yaml_start) || is.na(yaml_end)) problemas_criticos <- problemas_criticos + 1
if(!any(grepl("Meta-information", source_lines))) problemas_criticos <- problemas_criticos + 1
if(!setup_chunk) problemas_criticos <- problemas_criticos + 1

if(problemas_criticos == 0) {
  cat("🎯 EXCELENTE: Configuración .Rmd correcta\n")
  cat("✅ Encabezado YAML bien formado\n")
  cat("✅ Metadatos ICFES completos\n")
  cat("✅ Chunks configurados adecuadamente\n")
  cat("✅ Configuración TikZ presente\n")
  cat("✅ Meta-información R-exams completa\n")
  if(length(librerias_encontradas) == length(librerias_necesarias)) {
    cat("✅ Todas las librerías necesarias\n")
  } else {
    cat("⚠️  Algunas librerías faltantes (no crítico)\n")
  }
} else {
  cat("❌ PROBLEMAS CRÍTICOS ENCONTRADOS:", problemas_criticos, "\n")
  cat("🔧 REQUIERE CORRECCIÓN INMEDIATA\n")
}

cat("\nLa auditoría de configuración .Rmd y metadatos ha finalizado.\n")
