################################################################################
# SCRIPT: Generar Demos para GitHub Pages
# Propósito: Crear versiones HTML interactivas para publicar en GitHub Pages
# Autor: Proyecto Matemáticas ICFES R-Exams
# Fecha: Diciembre 2025
################################################################################

# Cargar librerías necesarias
library(exams)
library(exams2forms)

# Configuración
setwd("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/A-Produccion/En-Desarrollo/consumo_telefonico_adicional")

archivo_ejercicio <- "consumo_telefonico_adicional_n2_v1.Rmd"
dir_github_pages <- "docs"
dir_demo <- file.path(dir_github_pages, "demo")
dir_ejemplos <- file.path(dir_github_pages, "ejemplos")
dir_capturas <- file.path(dir_ejemplos, "capturas")

cat("================================================================================\n")
cat("GENERANDO DEMOS PARA GITHUB PAGES\n")
cat("================================================================================\n\n")

################################################################################
# 1. CREAR ESTRUCTURA DE DIRECTORIOS
################################################################################

cat("1. Creando estructura de directorios...\n")

directorios <- c(dir_github_pages, dir_demo, dir_ejemplos, dir_capturas)

for (dir in directorios) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat("   ✓ Creado:", dir, "\n")
  } else {
    cat("   ℹ Ya existe:", dir, "\n")
  }
}

cat("\n")

################################################################################
# 2. GENERAR DEMOS HTML INTERACTIVAS
################################################################################

cat("2. Generando demos HTML interactivas con exams2webquiz...\n")

# Generar 3 versiones interactivas para GitHub Pages
exams2webquiz(archivo_ejercicio,
              n = 3,
              dir = dir_demo,
              name = "version",
              edir = ".",
              encoding = "UTF-8",
              title = "Demo: Evaluación Matemática - Consumo Telefónico",
              solution = TRUE,
              shuffle = TRUE,
              mathjax = TRUE,
              browse = FALSE)

cat("   ✓ 3 demos interactivas generadas en:", dir_demo, "\n\n")

################################################################################
# 3. GENERAR EJEMPLOS PDF
################################################################################

cat("3. Generando ejemplos PDF...\n")

exams2pdf(archivo_ejercicio,
          n = 2,
          name = "ejemplo_pdf",
          dir = dir_ejemplos,
          edir = ".",
          encoding = "UTF-8",
          template = "plain.tex")

cat("   ✓ 2 ejemplos PDF generados en:", dir_ejemplos, "\n\n")

################################################################################
# 4. GENERAR EJEMPLO MOODLE XML
################################################################################

cat("4. Generando ejemplo Moodle XML...\n")

exams2moodle(archivo_ejercicio,
             n = 5,
             name = "ejemplo_moodle",
             dir = dir_ejemplos,
             edir = ".",
             encoding = "UTF-8")

cat("   ✓ Archivo Moodle XML generado en:", dir_ejemplos, "\n\n")

################################################################################
# 5. CREAR LANDING PAGE (index.html)
################################################################################

cat("5. Creando landing page (index.html)...\n")

index_html <- file.path(dir_github_pages, "index.html")

html_content <- '<!DOCTYPE html>
<html lang="es">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta name="description" content="Ejercicio interactivo R/exams para evaluación matemática ICFES">
    <title>Ejercicio R/exams: Consumo Telefónico Adicional</title>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body { 
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
            line-height: 1.6;
            color: #2D3142;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            min-height: 100vh;
            padding: 20px;
        }
        .container {
            max-width: 900px;
            margin: 0 auto;
            background: white;
            border-radius: 15px;
            box-shadow: 0 20px 60px rgba(0,0,0,0.3);
            padding: 40px;
        }
        h1 { 
            color: #2E86AB;
            font-size: 2.5em;
            margin-bottom: 20px;
            border-bottom: 4px solid #F18F01;
            padding-bottom: 15px;
        }
        h2 {
            color: #A23B72;
            margin-top: 30px;
            margin-bottom: 15px;
            font-size: 1.8em;
        }
        p { margin-bottom: 15px; font-size: 1.1em; }
        .demo-buttons {
            display: flex;
            gap: 15px;
            flex-wrap: wrap;
            margin: 25px 0;
        }
        .demo-button {
            background: linear-gradient(135deg, #F18F01 0%, #D17A01 100%);
            color: white;
            padding: 15px 30px;
            text-decoration: none;
            border-radius: 8px;
            font-weight: bold;
            font-size: 1.1em;
            transition: transform 0.2s, box-shadow 0.2s;
            box-shadow: 0 4px 15px rgba(241, 143, 1, 0.3);
        }
        .demo-button:hover {
            transform: translateY(-3px);
            box-shadow: 0 6px 20px rgba(241, 143, 1, 0.5);
        }
        ul { margin-left: 30px; margin-bottom: 20px; }
        li { margin-bottom: 10px; font-size: 1.05em; }
        .tech-stack {
            background: #f8f9fa;
            padding: 20px;
            border-radius: 8px;
            border-left: 5px solid #2E86AB;
            margin: 20px 0;
        }
        .tech-stack strong {
            color: #2E86AB;
            font-size: 1.1em;
        }
        footer {
            margin-top: 50px;
            padding-top: 20px;
            border-top: 2px solid #e9ecef;
            color: #6c757d;
            text-align: center;
        }
        .badge {
            display: inline-block;
            background: #28a745;
            color: white;
            padding: 5px 12px;
            border-radius: 20px;
            font-size: 0.9em;
            margin-right: 10px;
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>🎯 Evaluación Matemática Dinámica: Consumo Telefónico</h1>
        
        <p><span class="badge">✨ Nuevo</span> Ejercicio interactivo que evalúa <strong>interpretación de gráficos</strong> 
        y <strong>cálculos con unidades de tiempo</strong> en el contexto de facturas telefónicas.</p>
        
        <h2>🔴 Prueba las Demos Interactivas</h2>
        <div class="demo-buttons">
            <a href="demo/version1.html" class="demo-button">📊 Demo Versión 1</a>
            <a href="demo/version2.html" class="demo-button">📊 Demo Versión 2</a>
            <a href="demo/version3.html" class="demo-button">📊 Demo Versión 3</a>
        </div>
        
        <h2>✨ Características Principales</h2>
        <ul>
            <li>✅ <strong>300+ versiones únicas</strong> generables automáticamente</li>
            <li>✅ <strong>Gráficos dinámicos</strong> con Python/matplotlib</li>
            <li>✅ <strong>Tablas profesionales</strong> con TikZ/LaTeX</li>
            <li>✅ <strong>Multi-formato</strong>: HTML, PDF, Moodle, DOCX</li>
            <li>✅ <strong>Evaluación automática</strong> con retroalimentación detallada</li>
            <li>✅ <strong>Nivel ICFES 2</strong>: Secundaria/Preparación universitaria</li>
        </ul>
        
        <h2>🔗 Recursos Descargables</h2>
        <ul>
            <li><a href="ejemplos/ejemplo_pdf1.pdf" target="_blank">📄 Ejemplo PDF (Versión 1)</a></li>
            <li><a href="ejemplos/ejemplo_pdf2.pdf" target="_blank">📄 Ejemplo PDF (Versión 2)</a></li>
            <li><a href="ejemplos/ejemplo_moodle.xml" download>📦 Archivo Moodle XML (5 versiones)</a></li>
        </ul>
        
        <div class="tech-stack">
            <h2>📚 Stack Tecnológico</h2>
            <p><strong>R/exams</strong> • <strong>Python</strong> • <strong>matplotlib</strong> • 
               <strong>LaTeX/TikZ</strong> • <strong>Reticulate</strong> • <strong>exams2forms</strong></p>
        </div>
        
        <h2>🎓 Competencias Evaluadas</h2>
        <ul>
            <li><strong>Competencia ICFES:</strong> Interpretación y Representación</li>
            <li><strong>Componente:</strong> Numérico-Variacional</li>
            <li><strong>Pensamiento:</strong> Estadístico y Numérico</li>
            <li><strong>Contexto:</strong> Aplicado/Familiar</li>
        </ul>
        
        <footer>
            <p><strong>Proyecto:</strong> Matemáticas ICFES R-Exams</p>
            <p><strong>Licencia:</strong> MIT | <strong>Año:</strong> 2025</p>
            <p>Desarrollado con ❤️ para la comunidad educativa</p>
        </footer>
    </div>
</body>
</html>'

writeLines(html_content, index_html)

cat("   ✓ Landing page creada:", index_html, "\n\n")

################################################################################
# 6. CREAR README PARA GITHUB PAGES
################################################################################

cat("6. Creando README.md para GitHub Pages...\n")

readme_content <- '# 🎯 Demo: Ejercicio R/exams - Consumo Telefónico Adicional

## 📊 Demos Interactivas

- [Demo Versión 1](demo/version1.html)
- [Demo Versión 2](demo/version2.html)
- [Demo Versión 3](demo/version3.html)

## 📄 Ejemplos Descargables

- [Ejemplo PDF 1](ejemplos/ejemplo_pdf1.pdf)
- [Ejemplo PDF 2](ejemplos/ejemplo_pdf2.pdf)
- [Archivo Moodle XML](ejemplos/ejemplo_moodle.xml)

## 🚀 Características

- ✅ 300+ versiones únicas generables
- ✅ Gráficos dinámicos con Python/matplotlib
- ✅ Tablas profesionales con TikZ/LaTeX
- ✅ Compatible con múltiples formatos
- ✅ Evaluación automática

## 📚 Tecnologías

**R/exams** • **Python** • **matplotlib** • **LaTeX/TikZ** • **Reticulate**

---

**Proyecto:** Matemáticas ICFES R-Exams | **Licencia:** MIT
'

writeLines(readme_content, file.path(dir_github_pages, "README.md"))

cat("   ✓ README.md creado\n\n")

################################################################################
# 7. RESUMEN Y PRÓXIMOS PASOS
################################################################################

cat("================================================================================\n")
cat("RESUMEN DE ARCHIVOS GENERADOS\n")
cat("================================================================================\n\n")

cat("📁 Estructura creada:\n")
cat("docs/\n")
cat("├── index.html (Landing page)\n")
cat("├── README.md\n")
cat("├── demo/\n")
cat("│   ├── version1.html\n")
cat("│   ├── version2.html\n")
cat("│   ├── version3.html\n")
cat("│   ├── webex.css\n")
cat("│   └── webex.js\n")
cat("└── ejemplos/\n")
cat("    ├── ejemplo_pdf1.pdf\n")
cat("    ├── ejemplo_pdf2.pdf\n")
cat("    ├── ejemplo_moodle.xml\n")
cat("    └── capturas/ (vacío - agregar capturas manualmente)\n\n")

cat("✅ Proceso completado exitosamente!\n\n")

cat("================================================================================\n")
cat("PRÓXIMOS PASOS PARA ACTIVAR GITHUB PAGES\n")
cat("================================================================================\n\n")

cat("1. Agregar archivos a Git:\n")
cat("   git add docs/\n")
cat("   git commit -m \"Add GitHub Pages demo for LinkedIn publication\"\n\n")

cat("2. Subir a GitHub:\n")
cat("   git push origin main\n\n")

cat("3. Configurar GitHub Pages:\n")
cat("   - Ir a Settings > Pages\n")
cat("   - Source: Deploy from a branch\n")
cat("   - Branch: main\n")
cat("   - Folder: /docs\n")
cat("   - Save\n\n")

cat("4. Esperar 1-2 minutos y visitar:\n")
cat("   https://[tu-usuario].github.io/proyecto-r-exams-icfes-matematicas-optimizado/\n\n")

cat("5. Copiar URL para usar en publicación de LinkedIn\n\n")

cat("🔗 Ver guía completa en: GUIA_PUBLICACION_LINKEDIN.md\n\n")

