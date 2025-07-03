# testeo_visual.R - Pruebas unitarias para validación de calidad visual de imágenes
# Este script contiene pruebas para asegurar la coherencia visual y profesionalidad
# de las imágenes generadas por diferentes lenguajes y formatos

library(testthat)
library(magick)
library(png)
library(jpeg)
library(ggplot2)

# Configuración global para las pruebas
context("Pruebas de Calidad Visual")

# Función auxiliar para calcular el contraste de una imagen
get_image_contrast <- function(img_path) {
  img <- image_read(img_path)
  stats <- image_statistics(img)
  return(stats$contrast)
}

# Función auxiliar para verificar la resolución de una imagen
check_image_resolution <- function(img_path, min_width = 800, min_height = 600) {
  img <- image_read(img_path)
  dims <- image_info(img)
  return(dims$width >= min_width && dims$height >= min_height)
}

# Pruebas para imágenes generadas con TikZ (PDF)
test_that("Imágenes TikZ tienen calidad profesional", {
  # Verificar que las imágenes TikZ tienen resolución adecuada
  expect_true(check_image_resolution("output/tikz_output.pdf"),
              "La imagen TikZ no cumple con la resolución mínima requerida")
  
  # Verificar que las líneas tienen grosor adecuado
  expect_true(image_read("output/tikz_output.pdf") %>% 
                image_statistics()$stroke_width >= 0.5,
              "Las líneas en la imagen TikZ son demasiado delgadas")
  
  # Verificar que los textos son legibles
  expect_true(image_read("output/tikz_output.pdf") %>% 
                image_statistics()$font_size >= 10,
              "El tamaño del texto en la imagen TikZ es demasiado pequeño")
})

# Pruebas para imágenes generadas con Python (PNG)
test_that("Imágenes Python tienen calidad profesional", {
  # Verificar que las imágenes PNG tienen resolución adecuada
  expect_true(check_image_resolution("output/python_output.png"),
              "La imagen Python no cumple con la resolución mínima requerida")
  
  # Verificar que el contraste es adecuado
  expect_true(get_image_contrast("output/python_output.png") > 0.5,
              "El contraste de la imagen Python es demasiado bajo")
  
  # Verificar que no hay artefactos visuales
  img <- readPNG("output/python_output.png")
  expect_true(mean(img[,,4]) > 0.8,
              "La imagen Python tiene demasiados artefactos visuales")
})

# Pruebas para gráficas R (PNG)
test_that("Gráficas R tienen calidad profesional", {
  # Verificar que las gráficas R tienen resolución adecuada
  expect_true(check_image_resolution("output/r_plot.png"),
              "La gráfica R no cumple con la resolución mínima requerida")
  
  # Verificar que los ejes están bien etiquetados
  img <- image_read("output/r_plot.png")
  expect_true(grepl("xlabel|ylabel", image_text(img)),
              "Los ejes de la gráfica R no están correctamente etiquetados")
  
  # Verificar que la leyenda es legible
  expect_true(grepl("legend", image_text(img)),
              "La leyenda de la gráfica R no es legible")
})

# Pruebas de coherencia visual general
test_that("Coherencia visual general", {
  # Verificar que todas las imágenes tienen un estilo consistente
  img1 <- image_read("output/tikz_output.pdf")
  img2 <- image_read("output/python_output.png")
  img3 <- image_read("output/r_plot.png")
  
  # Comparar paletas de colores
  colors1 <- image_colors(img1)
  colors2 <- image_colors(img2)
  colors3 <- image_colors(img3)
  
  expect_true(length(intersect(colors1, colors2)) > 0,
              "Las imágenes no comparten colores comunes")
  expect_true(length(intersect(colors2, colors3)) > 0,
              "Las imágenes no comparten colores comunes")
  
  # Verificar que las fuentes son consistentes
  fonts1 <- image_fonts(img1)
  fonts2 <- image_fonts(img2)
  fonts3 <- image_fonts(img3)
  
  expect_true(length(intersect(fonts1, fonts2)) > 0,
              "Las fuentes no son consistentes entre las imágenes")
  expect_true(length(intersect(fonts2, fonts3)) > 0,
              "Las fuentes no son consistentes entre las imágenes")
})

# Pruebas de formatos de salida
test_that("Formatos de salida son adecuados", {
  # Verificar que los formatos PNG tienen transparencia correcta
  png_img <- image_read("output/png_output.png")
  expect_true(mean(png_img[,,4]) > 0.8,
              "El formato PNG tiene problemas con la transparencia")
  
  # Verificar que los formatos PDF son vectoriales
  pdf_img <- image_read("output/pdf_output.pdf")
  expect_true(image_info(pdf_img)$format == "PDF",
              "El formato PDF no es vectorial")
  
  # Verificar que los formatos SVG mantienen la calidad
  svg_img <- image_read("output/svg_output.svg")
  expect_true(image_info(svg_img)$format == "SVG",
              "El formato SVG no es vectorial")
})

# Ejecutar todas las pruebas
if (interactive()) {
  test_dir(".")
}
