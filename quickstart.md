# Guía de Inicio Rápido para RepositorioMatematicasICFES_R_Exams

Esta guía te ayudará a comenzar a trabajar con el repositorio de Matemáticas ICFES utilizando R y RStudio.

## 1. Requisitos previos

- R 4.x instalado
- RStudio instalado
- Paquetes necesarios instalados (ver `install_packages.R`)

## 2. Configuración inicial

1. Abre RStudio
2. Abre el proyecto haciendo clic en `File > Open Project...` y selecciona el archivo `RepositorioMatematicasICFES_R_Exams.Rproj`
3. Ejecuta el script de configuración para verificar que todo está correctamente instalado:

```r
source("setup_project.R")
```

## 3. Estructura del repositorio

El repositorio está organizado por áreas temáticas según el currículo de matemáticas ICFES:

- `01-Numeros-Reales`: Pensamiento Numérico
- `02-Funciones`: Pensamiento Variacional y Espacial
- `03-Razones-Trigonometricas`: Pensamiento Espacial Métrico y Variacional
- `04-Funciones-Identidades-Trigonometricas`: Pensamiento Espacial y Variacional
- `05-Geometria-Analitica`: Pensamiento Espacial
- `06-Estadística-Y-Probabilidad`: Pensamiento Aleatorio
- `Lab`: Directorio para pruebas y desarrollo

## 4. Trabajando con ejercicios

### 4.1 Crear un nuevo ejercicio

Para crear un nuevo ejercicio, utiliza la plantilla proporcionada:

1. Copia el archivo `plantilla_ejercicio_icfes.Rmd` a la carpeta correspondiente al tema
2. Renombra el archivo con un nombre descriptivo
3. Edita el archivo para crear tu ejercicio
4. Asegúrate de incluir los metadatos ICFES según el formato establecido

### 4.2 Ejecutar un ejercicio existente

Para ejecutar un ejercicio existente y generar diferentes formatos de salida:

```r
# Desde la consola de R
source("run_example.R")
# O desde la línea de comandos
Rscript run_example.R ruta/al/ejercicio.Rmd output 1
```

### 4.3 Actualizar metadatos ICFES

Si necesitas actualizar los metadatos ICFES en ejercicios existentes:

```r
source("actualizar_metadatos_icfes.R")
```

## 5. Consejos útiles

- Utiliza RStudio para editar los archivos .Rmd, ya que proporciona vista previa y resaltado de sintaxis
- Antes de crear un nuevo ejercicio, revisa los existentes para mantener un estilo consistente
- Utiliza el sistema de etiquetado ICFES para facilitar la búsqueda y clasificación de ejercicios
- Para generar exámenes completos, consulta la documentación del paquete `exams`

## 6. Recursos adicionales

- [Documentación oficial de R-exams](http://www.r-exams.org/)
- [Guía de R Markdown](https://rmarkdown.rstudio.com/lesson-1.html)
- [Marco de referencia ICFES](matriz_alineacion_icfes.md)

## 7. Solución de problemas

Si encuentras problemas al ejecutar los scripts:

1. Verifica que todos los paquetes necesarios estén instalados ejecutando `install_packages.R`
2. Asegúrate de estar en el directorio correcto del proyecto
3. Verifica que los archivos .Rmd tengan la estructura correcta según la plantilla

Para problemas específicos, consulta la documentación o busca ayuda en la comunidad de R.
