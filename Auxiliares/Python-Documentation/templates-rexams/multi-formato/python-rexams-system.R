# Sistema Completo Python-Reticulate para R-exams - Basado en Archivos Exitosos
# Fuente: Análisis de archivos validados FUERA de Lab
# Propósito: Máxima compatibilidad y facilidad de uso

# =============================================================================
# CONFIGURACIÓN INICIAL PARA PYTHON-RETICULATE
# =============================================================================

configurar_python_rexams <- function(python_path = NULL) {
  
  cat("🐍 CONFIGURANDO PYTHON PARA R-EXAMS\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  
  # Cargar bibliotecas necesarias
  if (!require("reticulate", quietly = TRUE)) {
    stop("❌ ERROR: Instalar reticulate con install.packages('reticulate')")
  }
  
  # Configurar Python (patrón exitoso validado)
  if (is.null(python_path)) {
    # Intentar configuraciones en orden de preferencia
    python_configs <- c(
      "/usr/bin/python3",           # Linux estándar
      Sys.which("python"),          # Sistema automático
      Sys.which("python3"),         # Python 3 específico
      "/usr/local/bin/python3"      # macOS Homebrew
    )
    
    for (config in python_configs) {
      if (config != "" && file.exists(config)) {
        python_path <- config
        break
      }
    }
  }
  
  if (is.null(python_path) || python_path == "") {
    stop("❌ ERROR: No se encontró Python. Instalar Python 3.x")
  }
  
  # Configurar reticulate (patrón exitoso)
  use_python(python_path, required = TRUE)
  
  # Configurar chunks de Python (patrón validado)
  knitr::knit_engines$set(python = function(options) {
    knitr::engine_output(options, options$code, '')
  })
  
  # Configurar knitr para Python (patrón exitoso)
  knitr::opts_chunk$set(
    warning = FALSE,
    message = FALSE,
    fig.showtext = FALSE,
    fig.cap = "",
    fig.keep = 'all',
    dev = c("png", "pdf"),
    dpi = 150
  )
  
  cat("✅ Python configurado:", python_path, "\n")
  cat("✅ Reticulate configurado exitosamente\n")
  
  # Verificar bibliotecas Python esenciales
  verificar_bibliotecas_python()
  
  return(invisible(TRUE))
}

# =============================================================================
# VERIFICACIÓN DE BIBLIOTECAS PYTHON
# =============================================================================

verificar_bibliotecas_python <- function() {
  
  cat("\n📦 VERIFICANDO BIBLIOTECAS PYTHON\n")
  cat(paste(rep("-", 30), collapse = ""), "\n")
  
  bibliotecas_esenciales <- c("matplotlib", "numpy")
  bibliotecas_opcionales <- c("scipy", "pandas")
  
  # Verificar bibliotecas esenciales
  for (lib in bibliotecas_esenciales) {
    tryCatch({
      py_module_available(lib)
      cat("✅", lib, "disponible\n")
    }, error = function(e) {
      cat("❌", lib, "NO disponible - INSTALAR:", paste0("pip install ", lib), "\n")
    })
  }
  
  # Verificar bibliotecas opcionales
  for (lib in bibliotecas_opcionales) {
    if (py_module_available(lib)) {
      cat("✅", lib, "disponible (opcional)\n")
    } else {
      cat("⚠️", lib, "no disponible (opcional)\n")
    }
  }
  
  return(invisible(TRUE))
}

# =============================================================================
# FUNCIONES PARA GENERAR GRÁFICOS PYTHON
# =============================================================================

# Función para gráficos de barras
generar_grafico_barras <- function(categorias, valores, 
                                  titulo_x = "Categorías", 
                                  titulo_y = "Valores",
                                  tipo = "vertical",
                                  colores = NULL) {
  
  # Validar entrada
  if (length(categorias) != length(valores)) {
    stop("❌ ERROR: categorias y valores deben tener la misma longitud")
  }
  
  # Preparar datos para Python
  datos_python <- list(
    categorias = categorias,
    valores = valores,
    titulo_x = titulo_x,
    titulo_y = titulo_y,
    tipo_grafico = tipo
  )
  
  # Asignar a entorno global para acceso desde Python
  for (nombre in names(datos_python)) {
    assign(nombre, datos_python[[nombre]], envir = .GlobalEnv)
  }
  
  # Código Python (basado en patrones exitosos)
  codigo_python <- '
import matplotlib.pyplot as plt
import numpy as np

# Transferir datos de R a Python
categorias_py = r.categorias
valores_py = r.valores
titulo_x_py = r.titulo_x
titulo_y_py = r.titulo_y
tipo_py = r.tipo_grafico

# Colores estándar validados
colores_azules = ["#00B3E6", "#0066CC", "#000099", "#003366", "#001133"]
colores_seleccionados = colores_azules[:len(categorias_py)]

# Crear figura
fig, ax = plt.subplots(figsize=(6, 5))

if tipo_py == "vertical":
    bars = ax.bar(categorias_py, valores_py, color=colores_seleccionados, width=0.5)
    ax.yaxis.grid(True, linestyle="--", linewidth=0.7, color="darkgray")
    plt.xticks(fontweight="bold")
    plt.yticks(fontweight="bold")
    plt.xlabel(titulo_x_py, fontweight="bold")
    plt.ylabel(titulo_y_py, fontweight="bold")
else:
    bars = ax.barh(categorias_py, valores_py, color=colores_seleccionados, height=0.6)
    ax.xaxis.grid(True, linestyle="--", linewidth=0.7, color="darkgray")
    plt.yticks(fontweight="bold")
    plt.xticks(fontweight="bold")
    plt.ylabel(titulo_x_py, fontweight="bold")
    plt.xlabel(titulo_y_py, fontweight="bold")

# Configuración estándar
ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)
ax.spines["left"].set_linewidth(2)
ax.spines["bottom"].set_linewidth(2)

plt.show()
'
  
  return(codigo_python)
}

# Función para gráficos circulares
generar_grafico_circular <- function(etiquetas, valores, 
                                    mostrar_valores = TRUE,
                                    mostrar_porcentajes = FALSE) {
  
  # Validar entrada
  if (length(etiquetas) != length(valores)) {
    stop("❌ ERROR: etiquetas y valores deben tener la misma longitud")
  }
  
  # Preparar datos para Python
  datos_python <- list(
    etiquetas = etiquetas,
    valores = valores,
    mostrar_valores = mostrar_valores,
    mostrar_porcentajes = mostrar_porcentajes
  )
  
  # Asignar a entorno global
  for (nombre in names(datos_python)) {
    assign(nombre, datos_python[[nombre]], envir = .GlobalEnv)
  }
  
  # Código Python (basado en patrones exitosos)
  codigo_python <- '
from matplotlib import pyplot as plt
import numpy as np

# Transferir datos de R a Python
etiquetas_py = r.etiquetas
valores_py = r.valores
mostrar_valores_py = r.mostrar_valores
mostrar_porcentajes_py = r.mostrar_porcentajes

# Colores estándar validados
colores_estandar = ["#1f77b4", "#aec7e8", "#ff7f0e", "#ffbb78", "#2ca02c", 
                   "#98df8a", "#d62728", "#ff9999", "#9467bd", "#c5b0d5"]
colores_seleccionados = colores_estandar[:len(etiquetas_py)]

# Preparar etiquetas
if mostrar_valores_py and mostrar_porcentajes_py:
    total = sum(valores_py)
    porcentajes = [100 * v / total for v in valores_py]
    pie_labels = [f"{etiqueta}\\n{int(valor)} ({porcentaje:.1f}%)" 
                  for etiqueta, valor, porcentaje in zip(etiquetas_py, valores_py, porcentajes)]
elif mostrar_valores_py:
    pie_labels = [f"{etiqueta}\\n{int(valor)}" 
                  for etiqueta, valor in zip(etiquetas_py, valores_py)]
elif mostrar_porcentajes_py:
    total = sum(valores_py)
    porcentajes = [100 * v / total for v in valores_py]
    pie_labels = [f"{etiqueta}\\n{porcentaje:.1f}%" 
                  for etiqueta, porcentaje in zip(etiquetas_py, porcentajes)]
else:
    pie_labels = etiquetas_py

# Crear figura
fig, ax = plt.subplots(figsize=(6, 6), tight_layout=True)

# Crear gráfico circular
explode = tuple([0] * len(etiquetas_py))
wedges, texts, autotexts = ax.pie(valores_py, 
                                  explode=explode, 
                                  labels=pie_labels, 
                                  colors=colores_seleccionados, 
                                  shadow=True,
                                  startangle=90,
                                  autopct=None)

ax.axis("equal")
plt.subplots_adjust(left=0.1, top=0.9, right=0.9, bottom=0.1)
plt.show()
'
  
  return(codigo_python)
}

# Función para gráficos de funciones lineales
generar_grafico_funcion <- function(x_inicial, x_final, y_inicial, y_final,
                                   puntos_cambio = NULL, valores_cambio = NULL,
                                   titulo_x = "X", titulo_y = "Y",
                                   color_linea = "red") {
  
  # Preparar datos para Python
  datos_python <- list(
    x_inicial = x_inicial,
    x_final = x_final,
    y_inicial = y_inicial,
    y_final = y_final,
    puntos_cambio = if (is.null(puntos_cambio)) numeric(0) else puntos_cambio,
    valores_cambio = if (is.null(valores_cambio)) numeric(0) else valores_cambio,
    titulo_x = titulo_x,
    titulo_y = titulo_y,
    color_linea = color_linea
  )
  
  # Asignar a entorno global
  for (nombre in names(datos_python)) {
    assign(nombre, datos_python[[nombre]], envir = .GlobalEnv)
  }
  
  # Código Python (basado en patrones exitosos)
  codigo_python <- '
import matplotlib.pyplot as plt
import numpy as np

# Transferir datos de R a Python
x_inicial_py = r.x_inicial
x_final_py = r.x_final
y_inicial_py = r.y_inicial
y_final_py = r.y_final
puntos_cambio_py = r.puntos_cambio
valores_cambio_py = r.valores_cambio
titulo_x_py = r.titulo_x
titulo_y_py = r.titulo_y
color_linea_py = r.color_linea

# Configuración
plt.rcParams["figure.figsize"] = (8, 5)
fig = plt.figure()
ax = fig.add_subplot(111)

# Función por segmentos
if len(puntos_cambio_py) > 0:
    # Primer segmento
    x1 = np.array([x_inicial_py, puntos_cambio_py[0]])
    y1 = np.array([y_inicial_py, valores_cambio_py[0]])
    ax.plot(x1, y1, color=color_linea_py, linewidth=2)
    
    # Segmentos intermedios
    for i in range(len(puntos_cambio_py) - 1):
        x_seg = np.array([puntos_cambio_py[i], puntos_cambio_py[i + 1]])
        y_seg = np.array([valores_cambio_py[i], valores_cambio_py[i + 1]])
        ax.plot(x_seg, y_seg, color=color_linea_py, linewidth=2)
    
    # Último segmento
    x_ultimo = np.array([puntos_cambio_py[-1], x_final_py])
    y_ultimo = np.array([valores_cambio_py[-1], y_final_py])
    ax.plot(x_ultimo, y_ultimo, color=color_linea_py, linewidth=2)
else:
    # Función lineal simple
    x_simple = np.array([x_inicial_py, x_final_py])
    y_simple = np.array([y_inicial_py, y_final_py])
    ax.plot(x_simple, y_simple, color=color_linea_py, linewidth=2)

# Configuración estándar
ax.set_xlabel(titulo_x_py, fontweight="bold")
ax.set_ylabel(titulo_y_py, fontweight="bold")
ax.grid(True, linestyle="--", alpha=0.7)

plt.subplots_adjust(top=1, bottom=0.1, left=0.1, right=0.9)
plt.show()
'
  
  return(codigo_python)
}

# =============================================================================
# FUNCIÓN PRINCIPAL PARA SETUP DE ARCHIVOS .RMD
# =============================================================================

setup_python_rexams <- function() {
  
  cat("🚀 SETUP COMPLETO PYTHON R-EXAMS\n")
  cat(paste(rep("=", 40), collapse = ""), "\n")
  
  # Configurar Python
  configurar_python_rexams()
  
  # Configurar exams
  if (!require("exams", quietly = TRUE)) {
    stop("❌ ERROR: Instalar exams con install.packages('exams')")
  }
  
  # Configurar variables globales
  typ <<- match_exams_device()
  options(scipen = 999)
  
  cat("\n✅ SETUP COMPLETO EXITOSO\n")
  cat("💡 Usar funciones: generar_grafico_barras(), generar_grafico_circular(), generar_grafico_funcion()\n")
  cat("📚 Consultar: Auxiliares/Python-Documentation/\n")
  
  return(invisible(TRUE))
}

# =============================================================================
# EJEMPLO DE USO COMPLETO
# =============================================================================

if (FALSE) {
  
  # En chunk setup del archivo .Rmd:
  setup_python_rexams()
  
  # En chunk de datos:
  categorias <- c("Perros", "Gatos", "Aves")
  valores <- c(45, 30, 25)
  
  # En chunk Python:
  codigo_barras <- generar_grafico_barras(categorias, valores, 
                                         "Tipos de Mascotas", 
                                         "Número de Adopciones",
                                         "vertical")
  # Ejecutar: py_run_string(codigo_barras)
  
  # O usar directamente en chunk Python con variables R:
  # ```{python}
  # import matplotlib.pyplot as plt
  # import numpy as np
  # categorias_py = r.categorias
  # valores_py = r.valores
  # # ... resto del código
  # ```
}

# Mensaje de carga
cat("🐍 Sistema Python R-exams cargado exitosamente\n")
cat("💡 Uso: setup_python_rexams() en chunk setup\n")
cat("📊 Funciones disponibles: generar_grafico_barras(), generar_grafico_circular(), generar_grafico_funcion()\n")
cat("📚 Documentación: Auxiliares/Python-Documentation/\n")