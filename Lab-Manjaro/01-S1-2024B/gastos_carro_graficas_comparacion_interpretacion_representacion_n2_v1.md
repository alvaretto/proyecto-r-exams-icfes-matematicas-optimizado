---
output:
  html_document: default
  word_document: default
  pdf_document:
    keep_tex: true
    extra_dependencies: ["graphicx", "float", "tikz", "xcolor", "colortbl", "array"]

# Metadatos ICFES
icfes:
  competencia: interpretacion_representacion
  nivel_dificultad: 2
  contenido:
    categoria: estadistica
    tipo: generico
  contexto: familiar
  eje_axial: eje3
  componente: aleatorio
---




``` r
options(OutDec = ".")

# Función para generar datos aleatorios del ejercicio
generar_datos <- function() {
  # Aleatorización de contextos de gastos
  contextos_gastos <- list(
    list(vehiculo = "carro", categorias = c("Gasolina", "Parqueadero", "Peajes")),
    list(vehiculo = "motocicleta", categorias = c("Gasolina", "Parqueadero", "Mantenimiento")),
    list(vehiculo = "vehículo", categorias = c("Combustible", "Estacionamiento", "Peajes")),
    list(vehiculo = "automóvil", categorias = c("Gasolina", "Parqueo", "Peajes"))
  )
  contexto_sel <- sample(contextos_gastos, 1)[[1]]
  
  # Generar gastos aleatorios para 4 semanas
  # Rangos realistas para cada categoría
  rangos_gastos <- list(
    gasolina = c(25000, 45000),
    parqueadero = c(15000, 30000),
    peajes = c(8000, 25000)
  )
  
  gastos_semanas <- list()
  for(semana in 1:4) {
    gastos_semanas[[semana]] <- list(
      gasolina = sample(seq(rangos_gastos$gasolina[1], rangos_gastos$gasolina[2], 1000), 1),
      parqueadero = sample(seq(rangos_gastos$parqueadero[1], rangos_gastos$parqueadero[2], 1000), 1),
      peajes = sample(seq(rangos_gastos$peajes[1], rangos_gastos$peajes[2], 1000), 1)
    )
  }
  
  # Calcular totales por semana y por categoría
  totales_semana <- sapply(gastos_semanas, function(s) sum(unlist(s)))
  totales_categoria <- list(
    gasolina = sum(sapply(gastos_semanas, function(s) s$gasolina)),
    parqueadero = sum(sapply(gastos_semanas, function(s) s$parqueadero)),
    peajes = sum(sapply(gastos_semanas, function(s) s$peajes))
  )
  
  return(list(
    contexto = contexto_sel,
    gastos_semanas = gastos_semanas,
    totales_semana = totales_semana,
    totales_categoria = totales_categoria
  ))
}

# Generar datos para este ejercicio
datos <- generar_datos()

# Formatear números sin notación científica
formatear_numero <- function(num) {
  formatC(num, format = "d", big.mark = ".", decimal.mark = ",")
}

# Validaciones matemáticas
test_that("Los datos generados son coherentes", {
  expect_true(length(datos$gastos_semanas) == 4)
  expect_true(all(datos$totales_semana > 0))
  expect_true(sum(unlist(datos$totales_categoria)) == sum(datos$totales_semana))
})
```

Test passed 🥇

``` r
# Test de diversidad de versiones
test_that("Prueba de diversidad de versiones", {
  versiones <- list()
  for(i in 1:1000) {
    datos_test <- generar_datos()
    versiones[[i]] <- digest::digest(datos_test)
  }
  
  n_versiones_unicas <- length(unique(versiones))
  expect_true(n_versiones_unicas >= 300,
              info = paste("Solo se generaron", n_versiones_unicas,
                          "versiones únicas. Se requieren al menos 300."))
})
```

Test passed 🌈


``` r
# Crear tabla de datos usando TikZ simplificado
tabla_gastos <- c(
  "\\begin{tikzpicture}",
  "\\node[inner sep=0pt] {",
  "  \\begin{tabular}{|c|c|c|c|}",
  "    \\hline",
  "    \\textbf{} & \\textbf{Gasolina} & \\textbf{Parqueadero} & \\textbf{Peajes} \\\\",
  "    \\hline",
  paste0("    \\textbf{Semana 1} & \\$", formatear_numero(datos$gastos_semanas[[1]]$gasolina), " & \\$", formatear_numero(datos$gastos_semanas[[1]]$parqueadero), " & \\$", formatear_numero(datos$gastos_semanas[[1]]$peajes), " \\\\"),
  "    \\hline",
  paste0("    \\textbf{Semana 2} & \\$", formatear_numero(datos$gastos_semanas[[2]]$gasolina), " & \\$", formatear_numero(datos$gastos_semanas[[2]]$parqueadero), " & \\$", formatear_numero(datos$gastos_semanas[[2]]$peajes), " \\\\"),
  "    \\hline",
  paste0("    \\textbf{Semana 3} & \\$", formatear_numero(datos$gastos_semanas[[3]]$gasolina), " & \\$", formatear_numero(datos$gastos_semanas[[3]]$parqueadero), " & \\$", formatear_numero(datos$gastos_semanas[[3]]$peajes), " \\\\"),
  "    \\hline",
  paste0("    \\textbf{Semana 4} & \\$", formatear_numero(datos$gastos_semanas[[4]]$gasolina), " & \\$", formatear_numero(datos$gastos_semanas[[4]]$parqueadero), " & \\$", formatear_numero(datos$gastos_semanas[[4]]$peajes), " \\\\"),
  "    \\hline",
  "  \\end{tabular}",
  "};",
  "\\end{tikzpicture}"
)
```


``` r
# Preparar datos para las gráficas con Python

# Opción A: Gráfica circular por categoría (porcentajes del total)
total_general <- sum(unlist(datos$totales_categoria))
porc_gasolina <- round((datos$totales_categoria$gasolina / total_general) * 100, 1)
porc_parqueadero <- round((datos$totales_categoria$parqueadero / total_general) * 100, 1)
porc_peajes <- round((datos$totales_categoria$peajes / total_general) * 100, 1)

# Opción C: Gráfica circular por semana (porcentajes por semana)
porc_semanas <- round((datos$totales_semana / sum(datos$totales_semana)) * 100, 1)

# Determinar cuál opción es la correcta (B - barras apiladas por semana)
respuesta_correcta <- 2  # Opción B
solucion <- c(0, 1, 0, 0)
```



Question
========

La tabla muestra el registro semanal que lleva una persona de los gastos relacionados con su carro.

![](tabla_gastos.png){width=10cm}

Si la persona quiere realizar una comparación entre los gastos totales por semana, ¿cuál de las siguientes gráficas le permite hacer esto directamente?

Answerlist
----------

- **Opción A:** Gráfica circular por categoría

\includegraphics[width=0.7\textwidth]{grafica_a.png}

- **Opción B:** Gráfica de barras apiladas por semana

\includegraphics[width=0.8\textwidth]{grafica_b.png}

- **Opción C:** Gráfica circular por semana

\includegraphics[width=0.7\textwidth]{grafica_c.png}

- **Opción D:** Gráfica de barras agrupadas por categoría

\includegraphics[width=0.9\textwidth]{grafica_d.png}

Solution
========

Para resolver este problema, necesitamos identificar qué tipo de gráfica permite comparar directamente los **gastos totales por semana**.

### Análisis de los datos

Primero, calculemos los gastos totales por semana:

* **Semana 1:** $31.000 + $19.000 + $9.000 = $59.000
* **Semana 2:** $42.000 + $17.000 + $22.000 = $81.000
* **Semana 3:** $40.000 + $24.000 + $15.000 = $79.000
* **Semana 4:** $43.000 + $17.000 + $22.000 = $82.000

### Análisis de cada opción

**Opción A:** Gráfica circular por categoría

* Muestra la proporción de cada tipo de gasto en el total general
* NO permite comparar gastos totales por semana

**Opción B:** Gráfica de barras apiladas por semana

* Cada barra representa una semana
* La altura total de cada barra muestra el gasto total de esa semana
* Permite comparar directamente los gastos totales entre semanas
* **Esta es la respuesta correcta**

**Opción C:** Gráfica circular por semana

* Muestra la proporción de gastos de cada semana respecto al total del mes
* No muestra los valores absolutos de cada semana claramente

**Opción D:** Gráfica de barras agrupadas por categoría

* Agrupa las barras por tipo de gasto (gasolina, parqueadero, peajes)
* Dificulta la comparación de gastos totales por semana

### Conclusión

La **Opción B** (gráfica de barras apiladas por semana) es la única que permite comparar directamente los gastos totales por semana, ya que la altura de cada barra representa el gasto total de cada semana.

Answerlist
----------
- Falso
- Verdadero
- Falso
- Falso

Meta-information
================
exname: gastos_carro_graficas_comparacion
extype: schoice
exsolution: 0100
exshuffle: TRUE
exsection: Estadística|Gráficas|Interpretación|Comparación de datos
exextra[Type]: Interpretación y representación
exextra[Level]: 2
exextra[Language]: es
exextra[Course]: Matemáticas ICFES
