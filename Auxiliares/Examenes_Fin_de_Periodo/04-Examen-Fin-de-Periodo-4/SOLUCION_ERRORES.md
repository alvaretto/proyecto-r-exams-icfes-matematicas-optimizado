# 🔧 SOLUCIÓN DE ERRORES COMUNES

## ❌ Error: ModuleNotFoundError: No module named 'matplotlib'

### Descripción del Problema

Algunos ejercicios utilizan Python con matplotlib para generar gráficos. Si matplotlib no está instalado en el entorno Python que usa reticulate, se producirá este error.

### Solución 1: Instalar matplotlib en el sistema

```bash
# Opción A: Usando pip del sistema
pip install matplotlib numpy

# Opción B: Usando pip3
pip3 install matplotlib numpy

# Opción C: En Manjaro/Arch
sudo pacman -S python-matplotlib python-numpy
```

### Solución 2: Configurar reticulate para usar un entorno específico

Agregar al inicio del script `SemilleroFinDePeriodo_v4.R`:

```r
library(reticulate)

# Opción A: Usar Python del sistema
use_python("/usr/bin/python3", required = TRUE)

# Opción B: Crear y usar un entorno virtual
# virtualenv_create("r-exams-env")
# virtualenv_install("r-exams-env", c("matplotlib", "numpy"))
# use_virtualenv("r-exams-env", required = TRUE)
```

### Solución 3: Excluir ejercicios que requieren Python

Si no deseas instalar matplotlib, puedes excluir manualmente los ejercicios que usan Python:

1. Identificar ejercicios con Python (buscar "```{python" en los archivos)
2. Renombrarlos sin el prefijo numérico o moverlos a otro directorio
3. El script automáticamente los ignorará

### Ejercicios que Requieren matplotlib

Los siguientes ejercicios utilizan Python con matplotlib:

- `008-gastos_carro_graficas_comparacion_interpretacion_representacion_n2_opD_v1.Rmd`
- `009-gastos_carro_graficas_comparacion_interpretacion_representacion_n2_opC_v1.Rmd`
- `010-gastos_carro_graficas_comparacion_interpretacion_representacion_n2_opB_v1.Rmd`
- `011-gastos_carro_graficas_comparacion_interpretacion_representacion_n2_opA_v1.Rmd`
- `020-Adopcion_Mascotas_Aleatorio_Interpretacion_n3_v1-Opc-D2v2.Rmd` ✅ CORREGIDO
- `021-Adopcion_Mascotas_Aleatorio_Interpretacion_n3_v1-Opc-D2.Rmd` ✅ CORREGIDO
- `022-Adopcion_Mascotas_Aleatorio_Interpretacion_n3_v1-Opc-C2v2.Rmd` ✅ CORREGIDO
- `023-Adopcion_Mascotas_Aleatorio_Interpretacion_n3_v1-Opc-C2.Rmd` ✅ CORREGIDO
- `024-Adopcion_Mascotas_Aleatorio_Interpretacion_n3_v1-Opc-B2v2.Rmd` ✅ CORREGIDO
- `025-Adopcion_Mascotas_Aleatorio_Interpretacion_n3_v1-Opc-B2.Rmd` ✅ CORREGIDO
- `026-Adopcion_Mascotas_Aleatorio_Interpretacion_n3_v1-Opc-A.Rmd` ✅ CORREGIDO
- `027-Adopcion_Mascotas_Aleatorio_Interpretacion_n3_v1-Opc-A2v2.Rmd` ✅ CORREGIDO
- Y otros ejercicios de gráficos estadísticos

**NOTA**: Los archivos marcados con ✅ CORREGIDO ya incluyen la configuración
`use_python("/usr/bin/python3", required = TRUE)` y deberían funcionar correctamente
si tienes matplotlib instalado en tu sistema.

---

## ❌ Error: "No hay suficientes ejercicios disponibles"

### Descripción del Problema

El script requiere al menos 15 ejercicios con prefijo numérico (001-, 002-, etc.).

### Solución

1. Verificar que los archivos .Rmd tengan el prefijo numérico correcto
2. Reducir `NUM_EJERCICIOS` en el script si hay menos de 15 ejercicios
3. Agregar más ejercicios al directorio

---

## ❌ Error: LaTeX no encontrado

### Descripción del Problema

La generación de PDF requiere LaTeX instalado en el sistema.

### Solución

```bash
# Manjaro/Arch
sudo pacman -S texlive-most

# Ubuntu/Debian
sudo apt-get install texlive-full

# Verificar instalación
pdflatex --version
```

---

## ❌ Error: "Error en setwd(script_dir)"

### Descripción del Problema

El script no puede cambiar al directorio correcto.

### Solución

Ejecutar el script desde el directorio correcto:

```r
# En RStudio
setwd("Auxiliares/Examenes_Fin_de_Periodo/04-Examen-Fin-de-Periodo-4")
source("SemilleroFinDePeriodo_v4.R")
```

O desde terminal:

```bash
cd Auxiliares/Examenes_Fin_de_Periodo/04-Examen-Fin-de-Periodo-4
Rscript SemilleroFinDePeriodo_v4.R
```

---

## ✅ Verificación del Sistema

### Script de Verificación

Crear un archivo `verificar_sistema.R`:

```r
# Verificar instalación de paquetes R
cat("Verificando paquetes R...\n")
if (require("exams")) {
  cat("✓ Paquete 'exams' instalado\n")
} else {
  cat("✗ Paquete 'exams' NO instalado\n")
  cat("  Instalar con: install.packages('exams')\n")
}

# Verificar Python y matplotlib
cat("\nVerificando Python...\n")
if (require("reticulate")) {
  tryCatch({
    py_config()
    cat("✓ Python configurado\n")
    
    # Intentar importar matplotlib
    tryCatch({
      py_run_string("import matplotlib")
      cat("✓ matplotlib instalado\n")
    }, error = function(e) {
      cat("✗ matplotlib NO instalado\n")
      cat("  Instalar con: pip install matplotlib\n")
    })
    
    # Intentar importar numpy
    tryCatch({
      py_run_string("import numpy")
      cat("✓ numpy instalado\n")
    }, error = function(e) {
      cat("✗ numpy NO instalado\n")
      cat("  Instalar con: pip install numpy\n")
    })
  }, error = function(e) {
    cat("✗ Error al configurar Python\n")
  })
} else {
  cat("✗ Paquete 'reticulate' NO instalado\n")
}

# Verificar LaTeX
cat("\nVerificando LaTeX...\n")
latex_check <- system("pdflatex --version", ignore.stdout = TRUE, ignore.stderr = TRUE)
if (latex_check == 0) {
  cat("✓ LaTeX instalado\n")
} else {
  cat("✗ LaTeX NO instalado\n")
  cat("  Instalar según tu sistema operativo\n")
}

# Verificar archivos .Rmd
cat("\nVerificando ejercicios .Rmd...\n")
rmd_files <- list.files(pattern = "^[0-9]{3}-.*\\.Rmd$")
cat(sprintf("✓ %d ejercicios encontrados\n", length(rmd_files)))

if (length(rmd_files) >= 15) {
  cat("✓ Suficientes ejercicios para generar examen\n")
} else {
  cat(sprintf("⚠️  Solo %d ejercicios (se requieren al menos 15)\n", length(rmd_files)))
}
```

Ejecutar:

```r
source("verificar_sistema.R")
```

---

## 🔄 Regenerar Examen con Semilla Específica

Si deseas regenerar exactamente el mismo examen que se generó anteriormente:

1. Buscar la semilla en la salida del script (ej: `Semilla aleatoria utilizada: 18328424`)
2. Editar `SemilleroFinDePeriodo_v4.R` línea 26-27:

```r
# semilla <- sample(100:1e8, 1)  # Comentar
semilla <- 18328424  # Usar la semilla específica
```

3. Ejecutar el script nuevamente

---

## 📝 Registro de Errores

Si encuentras un error no documentado aquí:

1. Copiar el mensaje de error completo
2. Verificar qué ejercicio causó el error
3. Revisar el archivo .Rmd problemático
4. Reportar el error con:
   - Mensaje de error
   - Nombre del ejercicio
   - Versión de R y paquetes

---

## 🆘 Soporte Adicional

Para problemas no resueltos:

1. Revisar `README.md` para documentación completa
2. Consultar `RESUMEN_PROYECTO.md` para detalles técnicos
3. Verificar logs en el directorio `salida/`

---

**Última actualización**: 2025-01-30

