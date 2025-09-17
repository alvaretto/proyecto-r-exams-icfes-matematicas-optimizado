#!/usr/bin/env Rscript

# Script para previsualizar archivos XML de Moodle generados por R-exams
# Uso: Rscript preview_moodle_xml_simple.R input.xml output.html

# Configuración inicial - solo xml2 es necesario
if (!require("xml2", quietly = TRUE)) {
  cat("Instalando paquete xml2...\n")
  install.packages("xml2", repos = "https://cran.r-project.org/")
  library(xml2)
}

cat("[1] \"C\"\n")
cat("✅ Configuración R cargada correctamente\n")

# Función para obtener argumentos de línea de comandos
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("Uso: Rscript preview_moodle_xml_simple.R input.xml output.html")
}

xml_file <- args[1]
html_file <- args[2]

# Verificar que el archivo XML existe
if (!file.exists(xml_file)) {
  stop("El archivo XML no existe: ", xml_file)
}

# Leer y parsear el XML
doc <- read_xml(xml_file)

# Extraer preguntas
questions <- xml_find_all(doc, ".//question[@type='cloze']")

if (length(questions) == 0) {
  stop("No se encontraron preguntas de tipo 'cloze' en el XML")
}

# Función para procesar imágenes @@PLUGINFILE@@
process_images <- function(text, files_data) {
  if (is.null(text) || length(text) == 0) return("")
  
  # Buscar referencias @@PLUGINFILE@@/filename
  pattern <- "@@PLUGINFILE@@/([A-Za-z0-9_.-]+)"
  matches <- gregexpr(pattern, text, perl = TRUE)
  
  if (matches[[1]][1] != -1) {
    for (i in seq_along(matches[[1]])) {
      match_start <- matches[[1]][i]
      match_length <- attr(matches[[1]], "match.length")[i]
      full_match <- substr(text, match_start, match_start + match_length - 1)
      
      # Extraer nombre del archivo
      filename <- gsub("@@PLUGINFILE@@/", "", full_match)
      
      # Buscar el archivo en files_data
      if (filename %in% names(files_data)) {
        data_url <- files_data[[filename]]
        text <- gsub(full_match, data_url, text, fixed = TRUE)
      }
    }
  }
  
  return(text)
}

# Función para escapar HTML
htmlEscape <- function(text) {
  text <- gsub("&", "&amp;", text)
  text <- gsub("<", "&lt;", text)
  text <- gsub(">", "&gt;", text)
  text <- gsub('"', "&quot;", text)
  text <- gsub("'", "&#39;", text)
  return(text)
}

# Extraer archivos base64
files_data <- list()
file_nodes <- xml_find_all(doc, ".//file[@encoding='base64']")

for (file_node in file_nodes) {
  filename <- xml_attr(file_node, "name")
  if (!is.na(filename)) {
    base64_content <- xml_text(file_node)
    # Determinar MIME type
    ext <- tolower(tools::file_ext(filename))
    mime_type <- switch(ext,
      "png" = "image/png",
      "jpg" = "image/jpeg",
      "jpeg" = "image/jpeg",
      "svg" = "image/svg+xml",
      "application/octet-stream"
    )
    data_url <- paste0("data:", mime_type, ";base64,", base64_content)
    files_data[[filename]] <- data_url
  }
}

# Procesar cada pregunta
question_html <- ""

for (i in seq_along(questions)) {
  question <- questions[[i]]
  
  # Extraer información básica
  name_node <- xml_find_first(question, ".//name/text")
  name <- if (!is.null(name_node)) xml_text(name_node) else paste("Pregunta", i)
  
  questiontext_node <- xml_find_first(question, ".//questiontext/text")
  questiontext <- if (!is.null(questiontext_node)) xml_text(questiontext_node) else ""
  
  # Procesar imágenes en el texto
  questiontext <- process_images(questiontext, files_data)
  
  # Crear bloque HTML para la pregunta
  question_html <- paste0(question_html, 
    '<div class="question-block">',
    '<div class="qname">Pregunta: ', htmlEscape(name), '</div>',
    '<div class="qtext">', questiontext, '</div>',
    '</div>\n'
  )
}



# CSS y JavaScript
css <- '
body{font-family:system-ui,-apple-system,Segoe UI,Roboto,Arial,sans-serif;margin:24px;}
.question-block{border:1px solid #ddd;border-radius:8px;padding:16px;margin:16px 0;}
.qname{color:#555;margin-bottom:8px;}
.qtext img{max-width:100%;height:auto;}
.cloze-mc{background:#fafafa;border:1px solid #eee;padding:12px;border-radius:6px;margin:8px 0;}
.cloze-mc label{font-weight:600;margin-right:8px;}
.cloze-mc .choices{margin-top:8px;}
.cloze-mc .choice{display:flex;align-items:flex-start;gap:8px;margin:6px 0;}
.cloze-mc .choice img{display:block;}
input.cloze-numerical{padding:6px 8px;font-size:1rem;width:10rem;}
select.cloze-select{padding:6px 8px;font-size:1rem;}
.tip{font-size:.92rem;color:#666;margin:8px 0 0;}
'

js_code <- '
document.addEventListener("DOMContentLoaded", function() {
  function transformCloze(container) {
    var html = container.innerHTML;
    var regex = /\\{(\\d+):(MULTICHOICE|NUMERICAL):([\\s\\S]*?)\\}/g;
    
    html = html.replace(regex, function(match, idx, typ, content) {
      if (typ === "NUMERICAL") {
        return "<input class=\\"cloze-numerical\\" data-index=\\"" + idx + "\\" type=\\"text\\" />";
      } else if (typ === "MULTICHOICE") {
        var parts = content.split("~");
        var opts = ["<option value=\\"\\">—</option>"];
        var items = [];
        
        for (var i = 0; i < parts.length; i++) {
          var part = parts[i];
          var clean = part.replace(/^=/, "").trim();
          var label = String.fromCharCode(65 + i);
          
          // Extraer etiqueta de <strong>
          var strongMatch = clean.match(/<strong>([^<]+)<\\/strong>/);
          if (strongMatch) {
            label = strongMatch[1].trim();
            clean = clean.replace(/<strong>[^<]+<\\/strong>\\s*:?\\s*/, "");
          }
          
          opts.push("<option value=\\"" + (i + 1) + "\\">" + label + "</option>");
          items.push("<div class=\\"choice\\"><strong>" + label + ":</strong> " + clean + "</div>");
        }
        
        return "<div class=\\"cloze-mc\\"><label>Respuesta:</label> " +
               "<select class=\\"cloze-select\\" data-index=\\"" + idx + "\\">" + opts.join("") + "</select>" +
               "<div class=\\"choices\\">" + items.join("") + "</div></div>";
      }
      return match;
    });
    
    container.innerHTML = html;
  }
  
  var qtexts = document.querySelectorAll(".qtext");
  for (var i = 0; i < qtexts.length; i++) {
    transformCloze(qtexts[i]);
  }
});
'

# Generar HTML completo
html_content <- paste0(
  '<!DOCTYPE html>',
  '<html>',
  '<head>',
  '<meta charset="utf-8">',
  '<title>Preview Moodle XML (R-exams)</title>',
  '<style>', css, '</style>',
  '</head>',
  '<body>',
  '<h2>Previsualización de preguntas (Moodle XML)</h2>',
  '<p class="tip">Vista previa local y rápida. Los campos CLOZE se convierten a entradas/selects, y las imágenes @@PLUGINFILE@@ se incrustan como data URLs.</p>',
  question_html,
  '<script>', js_code, '</script>',
  '</body>',
  '</html>'
)

# Guardar archivo HTML
tryCatch({
  # Crear directorio si no existe
  output_dir <- dirname(html_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Escribir HTML
  writeLines(html_content, html_file, useBytes = TRUE)
  
  cat("✅ Vista previa generada:", html_file, "\n")
  
}, error = function(e) {
  cat("Error al guardar el archivo HTML:", e$message, "\n")
  stop("No se pudo guardar el archivo HTML")
})
