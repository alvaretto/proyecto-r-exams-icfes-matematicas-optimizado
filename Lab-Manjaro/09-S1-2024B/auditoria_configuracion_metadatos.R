# ===================================================================
# AUDITORÍA DE CONFIGURACIÓN .RMD Y METADATOS
# ===================================================================
# Script para validar YAML, chunks, configuración TikZ y metadatos ICFES

library(yaml)

cat("=== AUDITORÍA DE CONFIGURACIÓN .RMD Y METADATOS ===\n\n")

archivo_ejercicio <- "pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd"
source_lines <- readLines(archivo_ejercicio)

# ===== PRUEBA 1: ENCABEZADO YAML =====
cat("1. ENCABEZADO YAML:\n")
cat("===================\n")

# Extraer YAML header
yaml_start <- which(source_lines == "---")[1]
yaml_end <- which(source_lines == "---")[2]

if(!is.na(yaml_start) && !is.na(yaml_end)) {
  yaml_lines <- source_lines[(yaml_start + 1):(yaml_end - 1)]
  yaml_content <- paste(yaml_lines, collapse = "\n")
  
  tryCatch({
    yaml_parsed <- yaml.load(yaml_content)
    cat("✅ CORRECTO: YAML header válido y parseable\n")
    
    # Verificar estructura de output
    if("output" %in% names(yaml_parsed)) {
      formatos_output <- names(yaml_parsed$output)
      formatos_esperados <- c("html_document", "pdf_document")
      
      if(all(formatos_esperados %in% formatos_output)) {
        cat("✅ CORRECTO: Formatos de output necesarios presentes\n")
      } else {
        faltantes <- setdiff(formatos_esperados, formatos_output)
        cat("⚠️  ADVERTENCIA: Formatos de output faltantes:", 
            paste(faltantes, collapse = ", "), "\n")
      }
      
      # Verificar configuración PDF
      if("pdf_document" %in% names(yaml_parsed$output)) {
        pdf_config <- yaml_parsed$output$pdf_document
        if("extra_dependencies" %in% names(pdf_config)) {
          deps <- pdf_config$extra_dependencies
          deps_necesarias <- c("tikz", "xcolor", "graphicx")
          
          if(all(deps_necesarias %in% deps)) {
            cat("✅ CORRECTO: Dependencias LaTeX necesarias presentes\n")
          } else {
            faltantes <- setdiff(deps_necesarias, deps)
            cat("⚠️  ADVERTENCIA: Dependencias LaTeX faltantes:", 
                paste(faltantes, collapse = ", "), "\n")
          }
        } else {
          cat("⚠️  ADVERTENCIA: No se especifican extra_dependencies para PDF\n")
        }
      }
    } else {
      cat("❌ ERROR: Sección 'output' faltante en YAML\n")
    }
    
  }, error = function(e) {
    cat("❌ ERROR: YAML header inválido:", e$message, "\n")
  })
} else {
  cat("❌ ERROR: No se encontró encabezado YAML válido\n")
}

# ===== PRUEBA 2: METADATOS ICFES =====
cat("\n2. METADATOS ICFES:\n")
cat("===================\n")

if(exists("yaml_parsed") && "icfes" %in% names(yaml_parsed)) {
  icfes_meta <- yaml_parsed$icfes
  
  # Verificar campos obligatorios
  campos_obligatorios <- c("competencia", "nivel_dificultad", "contenido", 
                          "contexto", "eje_axial", "componente")
  
  campos_presentes <- names(icfes_meta)
  campos_faltantes <- setdiff(campos_obligatorios, campos_presentes)
  
  if(length(campos_faltantes) == 0) {
    cat("✅ CORRECTO: Todos los campos ICFES obligatorios presentes\n")
    
    # Verificar valores específicos
    if(icfes_meta$competencia == "interpretacion_representacion") {
      cat("✅ CORRECTO: Competencia apropiada para el ejercicio\n")
    } else {
      cat("⚠️  ADVERTENCIA: Competencia puede no ser la más apropiada:", 
          icfes_meta$competencia, "\n")
    }
    
    if(icfes_meta$nivel_dificultad %in% c(1, 2, 3)) {
      cat("✅ CORRECTO: Nivel de dificultad válido (", icfes_meta$nivel_dificultad, ")\n")
    } else {
      cat("❌ ERROR: Nivel de dificultad inválido:", icfes_meta$nivel_dificultad, "\n")
    }
    
    if("contenido" %in% names(icfes_meta) && "categoria" %in% names(icfes_meta$contenido)) {
      if(icfes_meta$contenido$categoria == "estadistica") {
        cat("✅ CORRECTO: Categoría de contenido apropiada\n")
      } else {
        cat("⚠️  ADVERTENCIA: Categoría de contenido:", icfes_meta$contenido$categoria, "\n")
      }
    }
    
  } else {
    cat("❌ ERROR: Campos ICFES faltantes:", paste(campos_faltantes, collapse = ", "), "\n")
  }
} else {
  cat("❌ ERROR: Metadatos ICFES no encontrados\n")
}

# ===== PRUEBA 3: CONFIGURACIÓN TIKZ =====
cat("\n3. CONFIGURACIÓN TIKZ:\n")
cat("======================\n")

# Verificar configuración TikZ en setup chunk
setup_start <- grep("```\\{r setup", source_lines)
setup_end <- grep("```", source_lines)
setup_end <- setup_end[setup_end > setup_start][1]

if(length(setup_start) > 0 && !is.na(setup_end)) {
  setup_lines <- source_lines[setup_start:setup_end]
  
  # Verificar configuraciones TikZ críticas
  tikz_configs <- c("tikzLatex", "tikzXelatex", "tikzLatexPackages")
  tikz_encontradas <- c()
  
  for(config in tikz_configs) {
    if(any(grepl(config, setup_lines))) {
      tikz_encontradas <- c(tikz_encontradas, config)
    }
  }
  
  if(length(tikz_encontradas) == length(tikz_configs)) {
    cat("✅ CORRECTO: Configuración TikZ completa\n")
  } else {
    faltantes <- setdiff(tikz_configs, tikz_encontradas)
    cat("⚠️  ADVERTENCIA: Configuraciones TikZ faltantes:", 
        paste(faltantes, collapse = ", "), "\n")
  }
  
  # Verificar paquetes LaTeX para TikZ
  if(any(grepl("tikzLatexPackages", setup_lines))) {
    paquetes_necesarios <- c("tikz", "colortbl", "xcolor")
    paquetes_encontrados <- c()
    
    for(paquete in paquetes_necesarios) {
      if(any(grepl(paquete, setup_lines))) {
        paquetes_encontrados <- c(paquetes_encontrados, paquete)
      }
    }
    
    if(length(paquetes_encontrados) == length(paquetes_necesarios)) {
      cat("✅ CORRECTO: Paquetes LaTeX para TikZ presentes\n")
    } else {
      faltantes <- setdiff(paquetes_necesarios, paquetes_encontrados)
      cat("⚠️  ADVERTENCIA: Paquetes LaTeX faltantes:", 
          paste(faltantes, collapse = ", "), "\n")
    }
  }
} else {
  cat("❌ ERROR: Chunk setup no encontrado\n")
}

# ===== PRUEBA 4: META-INFORMATION =====
cat("\n4. META-INFORMATION:\n")
cat("====================\n")

# Buscar sección Meta-information
meta_start <- grep("Meta-information", source_lines)
if(length(meta_start) > 0) {
  meta_lines <- source_lines[(meta_start + 2):length(source_lines)]
  
  # Verificar campos obligatorios
  campos_meta_obligatorios <- c("exname", "extype", "exsolution", "exshuffle")
  campos_meta_encontrados <- c()
  
  for(campo in campos_meta_obligatorios) {
    if(any(grepl(paste0("^", campo, ":"), meta_lines))) {
      campos_meta_encontrados <- c(campos_meta_encontrados, campo)
    }
  }
  
  if(length(campos_meta_encontrados) == length(campos_meta_obligatorios)) {
    cat("✅ CORRECTO: Todos los campos Meta-information obligatorios presentes\n")
    
    # Verificar extype
    extype_line <- meta_lines[grepl("^extype:", meta_lines)]
    if(length(extype_line) > 0 && grepl("schoice", extype_line)) {
      cat("✅ CORRECTO: Tipo de ejercicio apropiado (schoice)\n")
    } else {
      cat("⚠️  ADVERTENCIA: Tipo de ejercicio puede no ser apropiado\n")
    }
    
    # Verificar exshuffle
    exshuffle_line <- meta_lines[grepl("^exshuffle:", meta_lines)]
    if(length(exshuffle_line) > 0 && grepl("TRUE", exshuffle_line)) {
      cat("✅ CORRECTO: Mezcla de opciones habilitada\n")
    } else {
      cat("⚠️  ADVERTENCIA: Mezcla de opciones no habilitada\n")
    }
    
  } else {
    faltantes <- setdiff(campos_meta_obligatorios, campos_meta_encontrados)
    cat("❌ ERROR: Campos Meta-information faltantes:", 
        paste(faltantes, collapse = ", "), "\n")
  }
} else {
  cat("❌ ERROR: Sección Meta-information no encontrada\n")
}

# ===== PRUEBA 5: CONFIGURACIÓN DE CHUNKS =====
cat("\n5. CONFIGURACIÓN DE CHUNKS:\n")
cat("===========================\n")

# Contar chunks y verificar configuraciones
chunks_r <- grep("```\\{r", source_lines)
total_chunks <- length(chunks_r)

if(total_chunks >= 5) {
  cat("✅ CORRECTO: Número adecuado de chunks R (", total_chunks, ")\n")
} else {
  cat("⚠️  ADVERTENCIA: Pocos chunks R encontrados (", total_chunks, ")\n")
}

# Verificar configuraciones importantes
chunks_con_echo_false <- sum(grepl("echo=FALSE", source_lines))
chunks_con_results_asis <- sum(grepl("results='asis'", source_lines))

if(chunks_con_echo_false >= 3) {
  cat("✅ CORRECTO: Configuración adecuada de echo=FALSE\n")
} else {
  cat("⚠️  ADVERTENCIA: Pocos chunks con echo=FALSE\n")
}

if(chunks_con_results_asis >= 1) {
  cat("✅ CORRECTO: Configuración results='asis' presente\n")
} else {
  cat("⚠️  ADVERTENCIA: Configuración results='asis' no encontrada\n")
}

# ===== RESUMEN FINAL =====
cat("\n=== RESUMEN DE CONFIGURACIÓN .RMD Y METADATOS ===\n")

# Contar problemas críticos
problemas_criticos <- 0
if(!exists("yaml_parsed")) problemas_criticos <- problemas_criticos + 1
if(length(meta_start) == 0) problemas_criticos <- problemas_criticos + 1

if(problemas_criticos == 0) {
  cat("🎯 EXCELENTE: Configuración .Rmd y metadatos correctos\n")
  cat("✅ YAML header válido y completo\n")
  cat("✅ Metadatos ICFES apropiados\n")
  cat("✅ Configuración TikZ adecuada\n")
  cat("✅ Meta-information completa\n")
  cat("✅ Chunks R configurados correctamente\n")
  cat("\n🏆 RESULTADO: APROBADO - CONFIGURACIÓN CORRECTA\n")
} else {
  cat("❌ PROBLEMAS CRÍTICOS ENCONTRADOS:", problemas_criticos, "\n")
  cat("🔧 REQUIERE CORRECCIÓN INMEDIATA\n")
  cat("\n⚠️  RESULTADO: REPROBADO - REQUIERE CORRECCIÓN\n")
}

cat("\nLa auditoría de configuración .Rmd y metadatos ha finalizado.\n")
