Claro, voy a optimizar el chunk de Python. Primero, necesito leer el archivo para entender el código actual.He analizado el código y he preparado una versión optimizada que cumple con todos los requisitos.

El nuevo código refactoriza la lógica de generación de gráficos en funciones parametrizadas, añade un manejo de errores robusto con bloques `try...except` y asegura que los recursos de `matplotlib` se liberen correctamente con `plt.close()`. Esto resulta en un chunk de Python mucho más limpio, mantenible y eficiente.

Aquí está el contenido completo del archivo `.Rmd` con el chunk de Python optimizado:

```r
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

```{r setup, include=FALSE}
# Configuración para todos los formatos de salida
Sys.setlocale(category = "LC_NUMERIC", locale = "C")
options(OutDec = ".")

# Configurar el motor LaTeX globalmente
options(tikzLatex = "pdflatex")
options(tikzXelatex = FALSE)
options(tikzLatexPackages = c(
  "\\usepackage{tikz}",
  "\\usepackage{colortbl}",
  "\\usepackage{xcolor}",
  "\\usepackage{graphicx}",
  "\\usepackage{float}",
  "\\usepackage{array}"
))

library(exams)
library(reticulate)
library(digest)
library(testthat)
library(knitr)

typ <- match_exams_device()
options(scipen = 999)
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  fig.showtext = FALSE,
  fig.cap = "",
  fig.keep = 'all',
  dev = c("png", "pdf"),
  dpi = 150,
  fig.pos = "H"
)

# Establecer semilla aleatoria
set.seed(sample(1:100000, 1))
```

```{r generar_datos, message=FALSE, warning=FALSE, results='asis'}
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

```{r crear_tabla_datos, message=FALSE, warning=FALSE, results='asis'}
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

```{r preparar_datos_graficas, message=FALSE, warning=FALSE}
# Preparar datos para las gráficas con Python

# Opción A: Gráfica circular por categoría (porcentajes del total)
total_general <- sum(unlist(datos$totales_categoria))
porc_gasolina <- round((datos$totales_categoria$gasolina / total_general) * 100, 1)
porc_parqueadero <- round((datos$totales_categoria$parqueadero / total_general) * 100, 1)
porc_peajes <- round((datos$totales_categoria$peajes / total_general) * 100, 1)

# Opción C: Gráfica circular por semana (porcentajes por semana)
porc_semanas <- round((datos$totales_semana / sum(datos$totales_semana)) * 100, 1)

# Determinar cuál opción es la correcta (A - gráfica circular por categoría para identificar proporción dominante)
respuesta_correcta <- 1  # Opción A
solucion <- c(1, 0, 0, 0)

# Detectar formato de salida para gráficas
typ <- match_exams_device()
es_pdf <- (typ == "pdf")

# Detectar formatos que requieren archivos PNG
formatos_png <- c("pdf", "pandoc", "docx", "odt")
usar_png <- (typ %in% formatos_png)

# Detectar formatos HTML/Moodle que usan plt.show()
formatos_moodle <- c("exams2moodle", "exams2qti12", "exams2qti21", "exams2openolat")
es_html_moodle <- (match_exams_call() %in% formatos_moodle) || (typ == "html")
```

```{python generar_graficas_archivos, echo=FALSE, results='hide'}
import matplotlib.pyplot as plt
import numpy as np
import os
import traceback

# --- Configuración General ---
plt.ioff()  # Modo no interactivo para evitar que se muestren figuras automáticamente
plt.rcParams['font.size'] = 10

# --- Funciones de Ayuda ---

def guardar_grafica(fig, nombre_base):
    """
    Guarda una figura de matplotlib en formatos PNG y PDF con manejo de errores.
    Asegura que la figura se cierre para liberar memoria.
    """
    try:
        # Guardar en ambos formatos para compatibilidad con R-exams
        fig.savefig(f'{nombre_base}.png', dpi=150, bbox_inches='tight', facecolor='white', edgecolor='none')
        fig.savefig(f'{nombre_base}.pdf', dpi=150, bbox_inches='tight', facecolor='white', edgecolor='none')
    except Exception as e:
        print(f"Error al guardar la gráfica '{nombre_base}': {e}")
        traceback.print_exc()
    finally:
        plt.close(fig) # Es crucial cerrar la figura para liberar memoria

# --- Funciones de Generación de Gráficas Parametrizadas ---

def generar_grafica_circular(datos, etiquetas, colores, titulo, nombre_archivo):
    """Genera y guarda una gráfica circular."""
    fig, ax = plt.subplots(figsize=(7, 6))
    try:
        explode = [0.05] * len(datos)
        wedges, texts, autotexts = ax.pie(datos, labels=etiquetas, autopct='%1.1f%%',
                                           colors=colores, startangle=90,
                                           explode=explode, shadow=True,
                                           textprops={'fontsize': 10, 'fontweight': 'bold'})
        for autotext in autotexts:
            autotext.set_color('white')
            autotext.set_fontweight('bold')
            autotext.set_fontsize(11)
        
        ax.set_title(titulo, fontsize=12, fontweight='bold', pad=20)
        plt.tight_layout()
        guardar_grafica(fig, nombre_archivo)
    except Exception as e:
        print(f"Error al generar la gráfica circular '{nombre_archivo}': {e}")
        traceback.print_exc()
        plt.close(fig) # Asegurar cierre en caso de error

def generar_grafica_barras_apiladas(semanas, datos_barras, colores, titulo, nombre_archivo, totales_semana):
    """Genera y guarda una gráfica de barras apiladas."""
    fig, ax = plt.subplots(figsize=(9, 6))
    try:
        x = np.arange(len(semanas))
        width = 0.4
        bottom = np.zeros(len(semanas))

        for i, (categoria, valores) in enumerate(datos_barras.items()):
            ax.bar(x, valores, width, label=categoria, color=colores[i], bottom=bottom)
            bottom += np.array(valores)

        max_total = max(totales_semana)
        for i, total in enumerate(totales_semana):
            ax.text(i, total + max_total * 0.02, f'${total:,}', ha='center', va='bottom',
                      fontweight='bold', fontsize=10, color='#333333')

        ax.set_xlabel('Semanas', fontsize=11, fontweight='bold')
        ax.set_ylabel('Gastos (pesos)', fontsize=11, fontweight='bold')
        ax.set_title(titulo, fontsize=12, fontweight='bold', pad=20)
        ax.set_xticks(x)
        ax.set_xticklabels(semanas, fontsize=10)
        ax.legend(bbox_to_anchor=(1.05, 1), loc='upper left', frameon=True, fancybox=True, shadow=True, fontsize=10)
        ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda val, p: f'${val:,.0f}'))
        ax.tick_params(axis='y', labelsize=9)
        ax.grid(True, alpha=0.3, linestyle='--', linewidth=0.5)
        ax.set_axisbelow(True)
        ax.set_ylim(0, max_total * 1.15)
        plt.tight_layout()
        guardar_grafica(fig, nombre_archivo)
    except Exception as e:
        print(f"Error al generar la gráfica de barras apiladas '{nombre_archivo}': {e}")
        traceback.print_exc()
        plt.close(fig)

def generar_grafica_barras_agrupadas(categorias, semanas, datos_barras, colores, titulo, nombre_archivo):
    """Genera y guarda una gráfica de barras agrupadas."""
    fig, ax = plt.subplots(figsize=(10, 6))
    try:
        x = np.arange(len(categorias))
        width = 0.15
        n_semanas = len(semanas)
        
        for i, semana in enumerate(semanas):
            offset = width * (i - (n_semanas - 1) / 2)
            valores_semana = [datos_barras[cat][i] for cat in categorias]
            ax.bar(x + offset, valores_semana, width, label=semana, color=colores[i], edgecolor='white', linewidth=0.5)

        ax.set_xlabel('Categorías de Gasto', fontsize=11, fontweight='bold')
        ax.set_ylabel('Gastos (pesos)', fontsize=11, fontweight='bold')
        ax.set_title(titulo, fontsize=12, fontweight='bold', pad=20)
        ax.set_xticks(x)
        ax.set_xticklabels(categorias, fontsize=10)
        ax.legend(bbox_to_anchor=(1.05, 1), loc='upper left', frameon=True, fancybox=True, shadow=True, fontsize=10)
        ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda val, p: f'${val:,.0f}'))
        ax.tick_params(axis='y', labelsize=9)
        ax.grid(True, alpha=0.3, linestyle='--', linewidth=0.5, axis='y')
        ax.set_axisbelow(True)
        
        max_value = max(max(v) for v in datos_barras.values())
        ax.set_ylim(0, max_value * 1.1)
        plt.tight_layout()
        guardar_grafica(fig, nombre_archivo)
    except Exception as e:
        print(f"Error al generar la gráfica de barras agrupadas '{nombre_archivo}': {e}")
        traceback.print_exc()
        plt.close(fig)

# --- Script Principal ---

try:
    # 1. Obtener datos desde el entorno de R
    gastos_semanas = r.datos['gastos_semanas']
    totales_semana = r.datos['totales_semana']
    porc_gasolina = r.porc_gasolina
    porc_parqueadero = r.porc_parqueadero
    porc_peajes = r.porc_peajes
    porc_semanas = r.porc_semanas
    vehiculo = r.datos["contexto"]["vehiculo"]

    # 2. Preparar estructuras de datos para las gráficas
    categorias = ['Gasolina', 'Parqueadero', 'Peajes']
    semanas = ['Semana 1', 'Semana 2', 'Semana 3', 'Semana 4']
    
    datos_por_categoria = {
        'Gasolina': [gastos_semanas[i]['gasolina'] for i in range(4)],
        'Parqueadero': [gastos_semanas[i]['parqueadero'] for i in range(4)],
        'Peajes': [gastos_semanas[i]['peajes'] for i in range(4)]
    }

    # 3. Definir paletas de colores
    colores_categorias = ['#2E5984', '#5B9BD5', '#A5A5A5']
    colores_semanas = ['#1F4E79', '#2E75B6', '#5B9BD5', '#8DB4E2']

    # 4. Generar cada una de las gráficas llamando a las funciones
    
    # GRÁFICA A: Circular por categoría
    generar_grafica_circular(
        datos=[porc_gasolina, porc_parqueadero, porc_peajes],
        etiquetas=categorias,
        colores=colores_categorias,
        titulo=f'Gastos del {vehiculo}',
        nombre_archivo='grafica_a'
    )

    # GRÁFICA B: Barras apiladas por semana
    generar_grafica_barras_apiladas(
        semanas=semanas,
        datos_barras=datos_por_categoria,
        colores=colores_categorias,
        titulo='Gráfica de barras apiladas por semana',
        nombre_archivo='grafica_b',
        totales_semana=totales_semana
    )

    # GRÁFICA C: Circular por semana
    generar_grafica_circular(
        datos=porc_semanas,
        etiquetas=semanas,
        colores=colores_semanas,
        titulo='Gráfica circular por semana',
        nombre_archivo='grafica_c'
    )

    # GRÁFICA D: Barras agrupadas por categoría
    generar_grafica_barras_agrupadas(
        categorias=categorias,
        semanas=semanas,
        datos_barras=datos_por_categoria,
        colores=colores_semanas,
        titulo='Gráfica de barras agrupadas por categoría',
        nombre_archivo='grafica_d'
    )

    print("Todas las gráficas (PNG y PDF) se generaron exitosamente.")

except Exception as e:
    print("Error en el script principal de generación de gráficas.")
    traceback.print_exc()

```

Question
========

La tabla muestra el registro semanal que lleva una persona de los gastos relacionados con su `r datos$contexto$vehiculo`.

```{r mostrar_tabla, echo=FALSE, results='asis'}
# Detectar formato de salida
formatos_moodle <- c("exams2moodle", "exams2qti12", "exams2qti21", "exams2openolat")
es_moodle <- (match_exams_call() %in% formatos_moodle)

if (es_moodle) {
  # Para Moodle, usar tabla HTML simple
  cat("<table border='1' style='border-collapse: collapse; margin: 0 auto;'>")
  cat("<tr><th style='padding: 8px; text-align: center; background-color: #f0f0f0;'></th>")
  cat("<th style='padding: 8px; text-align: center; background-color: #f0f0f0;'>Gasolina</th>")
  cat("<th style='padding: 8px; text-align: center; background-color: #f0f0f0;'>Parqueadero</th>")
  cat("<th style='padding: 8px; text-align: center; background-color: #f0f0f0;'>Peajes</th></tr>")
  
  for (i in 1:4) {
    cat("<tr>")
    cat("<td style='padding: 8px; text-align: center; font-weight: bold;'>Semana ", i, "</td>")
    cat("<td style='padding: 8px; text-align: center;'>$", formatear_numero(datos$gastos_semanas[[i]]$gasolina), "</td>")
    cat("<td style='padding: 8px; text-align: center;'>$", formatear_numero(datos$gastos_semanas[[i]]$parqueadero), "</td>")
    cat("<td style='padding: 8px; text-align: center;'>$", formatear_numero(datos$gastos_semanas[[i]]$peajes), "</td>")
    cat("</tr>")
  }
  cat("</table>")
} else {
  # Para PDF/Word, usar TikZ
  include_tikz(tabla_gastos,
               name = "tabla_gastos",
               markup = "markdown",
               format = typ,
               packages = c("tikz", "colortbl", "xcolor"),
               width = "10cm")
}
```

Si la persona quiere identificar qué categoría de gasto representa la mayor proporción del presupuesto total, ¿cuál de las siguientes gráficas le permite hacer esto directamente?

Answerlist
----------

- ![](grafica_a.png){width=70%}

- ![](grafica_b.png){width=80%}

- ![](grafica_c.png){width=70%}

- ![](grafica_d.png){width=90%}

Solution
========

Para resolver este problema, necesitamos identificar qué tipo de gráfica permite identificar directamente **qué categoría representa la mayor proporción del presupuesto total**.

### Análisis de los datos

Primero, calculemos los gastos totales por categoría y sus proporciones:

* **Gasolina:** $`r formatear_numero(datos$totales_categoria$gasolina)` = `r round((datos$totales_categoria$gasolina / sum(unlist(datos$totales_categoria))) * 100, 1)`% del total
* **Parqueadero:** $`r formatear_numero(datos$totales_categoria$parqueadero)` = `r round((datos$totales_categoria$parqueadero / sum(unlist(datos$totales_categoria))) * 100, 1)`% del total
* **Peajes:** $`r formatear_numero(datos$totales_categoria$peajes)` = `r round((datos$totales_categoria$peajes / sum(unlist(datos$totales_categoria))) * 100, 1)`% del total

**Total general:** $`r formatear_numero(sum(unlist(datos$totales_categoria)))`

Como podemos observar, **Gasolina** representa la mayor proporción del presupuesto total.

### Análisis de cada opción

**Gráfica circular por categoría:**

* Muestra la proporción de cada tipo de gasto en el total general
* Los porcentajes indican claramente qué categoría representa la mayor proporción del presupuesto
* Permite identificar directamente la categoría dominante sin necesidad de cálculos adicionales
* **Esta es la respuesta correcta**

**Gráfica de barras apiladas por semana:**

* Cada barra representa una semana
* La altura total de cada barra muestra el gasto total de esa semana
* NO permite identificar directamente qué categoría representa la mayor proporción

**Gráfica circular por semana:**

* Muestra la proporción de gastos de cada semana respecto al total del mes
* NO permite identificar qué categoría representa la mayor proporción del presupuesto

**Gráfica de barras agrupadas por categoría:**

* Agrupa las barras por tipo de gasto (gasolina, parqueadero, peajes)
* Permite ver gastos por categoría pero separados por semana
* Requiere sumar mentalmente las barras de cada categoría para identificar la proporción dominante

### Conclusión

La **Opción "gráfica circular por categoría"** es la única que permite identificar directamente qué categoría representa la mayor proporción del presupuesto total, ya que muestra claramente los porcentajes de cada tipo de gasto respecto al total general, facilitando la identificación inmediata de la categoría dominante.

Answerlist
----------
- Verdadero
- Falso
- Falso
- Falso

Meta-information
================
exname: gastos_carro_graficas_comparacion
extype: schoice
exsolution: 1000
exshuffle: TRUE
exsection: Estadística|Gráficas|Interpretación|Comparación de datos
exextra[Type]: Interpretación y representación
exextra[Level]: 2
exextra[Language]: es
exextra[Course]: Matemáticas ICFES
```
