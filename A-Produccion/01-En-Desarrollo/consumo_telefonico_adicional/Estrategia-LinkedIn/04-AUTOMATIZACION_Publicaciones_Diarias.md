# 🤖 AUTOMATIZACIÓN DE PUBLICACIONES DIARIAS EN LINKEDIN

## 🎯 OBJETIVO
Crear un sistema automatizado para generar y publicar contenido matemático diariamente en LinkedIn, manteniendo calidad y engagement alto.

---

## 📋 ARQUITECTURA DEL SISTEMA

### Componentes Principales

```
┌─────────────────────────────────────────────────────────────┐
│                    SISTEMA DE AUTOMATIZACIÓN                 │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  1. GENERADOR DE EJERCICIOS (R/exams)                       │
│     ├── Selección automática de ejercicio del día           │
│     ├── Generación de 5 demos HTML                          │
│     └── Creación de recursos descargables                   │
│                                                               │
│  2. PROCESADOR DE CONTENIDO                                  │
│     ├── Generación de texto del post                        │
│     ├── Creación de imágenes/carrusel                       │
│     └── Preparación de primer comentario                    │
│                                                               │
│  3. PUBLICADOR (GitHub Actions + Manual)                     │
│     ├── Actualización de GitHub Pages                       │
│     ├── Preparación de post LinkedIn                        │
│     └── Notificación para publicación manual                │
│                                                               │
│  4. MONITOR DE MÉTRICAS                                      │
│     ├── Tracking de engagement                              │
│     ├── Análisis de rendimiento                             │
│     └── Optimización de estrategia                          │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

---

## 🔧 PASO 1: CREAR BIBLIOTECA DE EJERCICIOS

### 1.1 Estructura de Directorio

```
A-Produccion/
├── Biblioteca-Ejercicios-LinkedIn/
│   ├── 01-Estadistica/
│   │   ├── consumo_telefonico_adicional_n2_v1.Rmd
│   │   ├── analisis_datos_climaticos_n2_v1.Rmd
│   │   └── interpretacion_graficos_n3_v1.Rmd
│   ├── 02-Algebra/
│   │   ├── ecuaciones_lineales_n2_v1.Rmd
│   │   └── sistemas_ecuaciones_n3_v1.Rmd
│   ├── 03-Geometria/
│   │   ├── areas_perimetros_n2_v1.Rmd
│   │   └── teorema_pitagoras_n3_v1.Rmd
│   └── catalogo_ejercicios.csv
```

### 1.2 Catálogo de Ejercicios

Crear archivo: `Biblioteca-Ejercicios-LinkedIn/catalogo_ejercicios.csv`

```csv
id,categoria,nombre_archivo,titulo_corto,nivel,competencia,estado,fecha_publicacion
1,Estadistica,consumo_telefonico_adicional_n2_v1.Rmd,Consumo Telefónico,2,Interpretación,publicado,2025-12-24
2,Estadistica,analisis_datos_climaticos_n2_v1.Rmd,Datos Climáticos,2,Interpretación,pendiente,
3,Algebra,ecuaciones_lineales_n2_v1.Rmd,Ecuaciones Lineales,2,Formulación,pendiente,
4,Geometria,areas_perimetros_n2_v1.Rmd,Áreas y Perímetros,2,Interpretación,pendiente,
5,Estadistica,interpretacion_graficos_n3_v1.Rmd,Gráficos Estadísticos,3,Argumentación,pendiente,
```

---

## 🤖 PASO 2: SCRIPT DE GENERACIÓN AUTOMÁTICA

### 2.1 Script Principal de Automatización

Crear archivo: `Estrategia-LinkedIn/scripts/automatizacion_diaria.R`

```r
# ============================================================================
# SCRIPT: Automatización de Generación Diaria de Contenido LinkedIn
# FECHA: Diciembre 2025
# ============================================================================

library(exams2forms)
library(tidyverse)
library(lubridate)
library(glue)

# Configuración
setwd("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams")

# Cargar catálogo de ejercicios
catalogo <- read_csv("A-Produccion/Biblioteca-Ejercicios-LinkedIn/catalogo_ejercicios.csv")

# Función para seleccionar ejercicio del día
seleccionar_ejercicio_del_dia <- function(catalogo) {
  # Filtrar ejercicios pendientes
  pendientes <- catalogo %>% filter(estado == "pendiente")
  
  if (nrow(pendientes) == 0) {
    stop("No hay ejercicios pendientes. Agregar más al catálogo.")
  }
  
  # Seleccionar el primero pendiente (o aleatorio)
  ejercicio <- pendientes[1, ]
  
  return(ejercicio)
}

# Función para generar demos HTML
generar_demos_automatico <- function(ejercicio) {
  cat(sprintf("\n=== Generando demos para: %s ===\n", ejercicio$titulo_corto))
  
  # Ruta completa del archivo
  ruta_ejercicio <- file.path(
    "A-Produccion/Biblioteca-Ejercicios-LinkedIn",
    ejercicio$categoria,
    ejercicio$nombre_archivo
  )
  
  # Directorio de salida
  fecha_hoy <- format(Sys.Date(), "%Y-%m-%d")
  dir_salida <- file.path(
    "A-Produccion/Publicaciones-LinkedIn",
    fecha_hoy,
    "demos-html"
  )
  dir.create(dir_salida, recursive = TRUE, showWarnings = FALSE)
  
  # Generar 5 demos
  for (i in 1:5) {
    nombre_salida <- sprintf("demo_%s_v%d", 
                            gsub(" ", "_", tolower(ejercicio$titulo_corto)), 
                            i)
    
    exams2webquiz(
      file = ruta_ejercicio,
      n = 1,
      name = nombre_salida,
      dir = dir_salida,
      edir = dirname(ruta_ejercicio),
      solution = TRUE,
      mathjax = TRUE,
      title = sprintf("Demo Interactivo - %s (Versión %d)", 
                     ejercicio$titulo_corto, i),
      encoding = "UTF-8"
    )
  }
  
  cat(sprintf("✓ 5 demos generados en: %s\n", dir_salida))
  return(dir_salida)
}

# Función para generar recursos descargables
generar_recursos_automatico <- function(ejercicio) {
  cat("\n=== Generando recursos descargables ===\n")
  
  ruta_ejercicio <- file.path(
    "A-Produccion/Biblioteca-Ejercicios-LinkedIn",
    ejercicio$categoria,
    ejercicio$nombre_archivo
  )
  
  fecha_hoy <- format(Sys.Date(), "%Y-%m-%d")
  dir_salida <- file.path(
    "A-Produccion/Publicaciones-LinkedIn",
    fecha_hoy,
    "recursos"
  )
  dir.create(dir_salida, recursive = TRUE, showWarnings = FALSE)
  
  # PDF con 10 versiones
  exams2pdf(
    file = ruta_ejercicio,
    n = 10,
    name = sprintf("muestra_10_versiones_%s", 
                  gsub(" ", "_", tolower(ejercicio$titulo_corto))),
    dir = dir_salida,
    edir = dirname(ruta_ejercicio),
    template = "plain"
  )
  
  # Moodle XML
  exams2moodle(
    file = ruta_ejercicio,
    n = 5,
    name = sprintf("%s_moodle", gsub(" ", "_", tolower(ejercicio$titulo_corto))),
    dir = dir_salida,
    edir = dirname(ruta_ejercicio)
  )
  
  cat(sprintf("✓ Recursos generados en: %s\n", dir_salida))
  return(dir_salida)
}

# Función para generar texto del post
generar_texto_post <- function(ejercicio) {
  # Templates de texto según categoría
  templates <- list(
    Estadistica = glue("
🎯 Nuevo ejercicio interactivo: {ejercicio$titulo_corto}

¿Sabías que la interpretación de gráficos es una de las competencias 
más evaluadas en el ICFES?

Hoy comparto un ejercicio de nivel {ejercicio$nivel} que evalúa:
✅ {ejercicio$competencia}
✅ Análisis de datos visuales
✅ Cálculos con contexto real

Como siempre, incluye:
→ 300+ versiones únicas
→ 5 demos interactivos
→ Verificación automática
→ Explicaciones paso a paso

¿Te atreves a resolverlo?
👇 Enlaces en el primer comentario

#EducaciónMatemática #ICFES #Estadística
    "),
    
    Algebra = glue("
🎯 Nuevo desafío matemático: {ejercicio$titulo_corto}

El álgebra no tiene que ser aburrida.

Este ejercicio de nivel {ejercicio$nivel} combina:
✅ {ejercicio$competencia}
✅ Resolución de problemas
✅ Aplicación en contextos reales

Sistema R/exams en acción:
→ 300+ versiones diferentes
→ Datos aleatorios pero coherentes
→ Distractores pedagógicos
→ Múltiples formatos de salida

Pruébalo en tu navegador 👇
Enlaces en el primer comentario

#Matemáticas #Álgebra #ICFES
    "),
    
    Geometria = glue("
🎯 Geometría aplicada: {ejercicio$titulo_corto}

La geometría cobra vida cuando se aplica a situaciones reales.

Ejercicio nivel {ejercicio$nivel} que evalúa:
✅ {ejercicio$competencia}
✅ Visualización espacial
✅ Cálculos geométricos

Características del sistema:
→ Gráficos generados con TikZ
→ 300+ configuraciones únicas
→ Explicaciones visuales
→ Compatible con Moodle

¿Listo para el desafío?
👇 Enlaces en comentarios

#Geometría #Matemáticas #ICFES
    ")
  )
  
  texto <- templates[[ejercicio$categoria]]
  return(as.character(texto))
}

# EJECUCIÓN PRINCIPAL
main <- function() {
  cat("\n╔════════════════════════════════════════════════════════╗\n")
  cat("║  AUTOMATIZACIÓN DIARIA - CONTENIDO LINKEDIN           ║\n")
  cat("╚════════════════════════════════════════════════════════╝\n")
  
  # 1. Seleccionar ejercicio
  ejercicio <- seleccionar_ejercicio_del_dia(catalogo)
  cat(sprintf("\n✓ Ejercicio seleccionado: %s\n", ejercicio$titulo_corto))
  
  # 2. Generar demos HTML
  dir_demos <- generar_demos_automatico(ejercicio)
  
  # 3. Generar recursos
  dir_recursos <- generar_recursos_automatico(ejercicio)
  
  # 4. Generar texto del post
  texto_post <- generar_texto_post(ejercicio)
  
  # 5. Guardar texto del post
  fecha_hoy <- format(Sys.Date(), "%Y-%m-%d")
  archivo_post <- file.path(
    "A-Produccion/Publicaciones-LinkedIn",
    fecha_hoy,
    "texto_post.txt"
  )
  writeLines(texto_post, archivo_post)
  
  cat("\n╔════════════════════════════════════════════════════════╗\n")
  cat("║  GENERACIÓN COMPLETADA                                 ║\n")
  cat("╚════════════════════════════════════════════════════════╝\n")
  cat(sprintf("\n📁 Demos HTML: %s\n", dir_demos))
  cat(sprintf("📁 Recursos: %s\n", dir_recursos))
  cat(sprintf("📝 Texto post: %s\n", archivo_post))
  cat("\n🎯 PRÓXIMOS PASOS:\n")
  cat("1. Revisar demos generados\n")
  cat("2. Copiar archivos a docs/ para GitHub Pages\n")
  cat("3. Crear imágenes para carrusel\n")
  cat("4. Publicar en LinkedIn manualmente\n")
  cat("5. Actualizar catálogo (marcar como publicado)\n\n")
}

# Ejecutar
main()
```

---

## 🔄 PASO 3: GITHUB ACTIONS PARA AUTOMATIZACIÓN

### 3.1 Workflow de GitHub Actions

Crear archivo: `.github/workflows/publicacion-diaria.yml`

```yaml
name: Generación Diaria de Contenido LinkedIn

on:
  schedule:
    # Ejecutar todos los días a las 6:00 AM (hora UTC)
    # Ajustar según zona horaria (Colombia: UTC-5)
    - cron: '0 11 * * *'  # 6:00 AM Colombia
  workflow_dispatch:  # Permitir ejecución manual

jobs:
  generar-contenido:
    runs-on: ubuntu-latest

    steps:
    - name: Checkout repositorio
      uses: actions/checkout@v3
      with:
        ref: gh-pages

    - name: Configurar R
      uses: r-lib/actions/setup-r@v2
      with:
        r-version: '4.3.0'

    - name: Configurar Python
      uses: actions/setup-python@v4
      with:
        python-version: '3.11'

    - name: Instalar dependencias R
      run: |
        Rscript -e 'install.packages(c("exams", "exams2forms", "tidyverse", "lubridate", "glue"))'

    - name: Instalar dependencias Python
      run: |
        pip install matplotlib numpy

    - name: Ejecutar script de generación
      run: |
        Rscript Estrategia-LinkedIn/scripts/automatizacion_diaria.R

    - name: Copiar archivos a docs/
      run: |
        bash Estrategia-LinkedIn/scripts/copiar_a_docs_automatico.sh

    - name: Commit y push
      run: |
        git config --local user.email "action@github.com"
        git config --local user.name "GitHub Action"
        git add docs/
        git commit -m "🤖 Contenido diario generado automáticamente - $(date +'%Y-%m-%d')" || echo "No changes to commit"
        git push

    - name: Crear issue con recordatorio
      uses: actions/github-script@v6
      with:
        script: |
          const fecha = new Date().toISOString().split('T')[0];
          github.rest.issues.create({
            owner: context.repo.owner,
            repo: context.repo.repo,
            title: `📅 Publicación LinkedIn pendiente - ${fecha}`,
            body: `## 🎯 Contenido generado para hoy

            ✅ Demos HTML generados
            ✅ Recursos descargables creados
            ✅ Texto del post preparado

            ### 📋 Tareas pendientes:

            - [ ] Revisar demos en GitHub Pages
            - [ ] Crear imágenes para carrusel
            - [ ] Publicar en LinkedIn (8:00 AM)
            - [ ] Agregar enlaces en primer comentario
            - [ ] Actualizar catálogo (marcar como publicado)
            - [ ] Cerrar este issue

            ### 🔗 Enlaces:

            - [Ver demos](https://alvaretto.github.io/proyecto-r-exams-icfes-matematicas-optimizado/)
            - [Texto del post](A-Produccion/Publicaciones-LinkedIn/${fecha}/texto_post.txt)

            ---

            *Issue creado automáticamente por GitHub Actions*`,
            labels: ['publicacion-linkedin', 'automatico']
          });
```

### 3.2 Script de Copia Automática

Crear archivo: `Estrategia-LinkedIn/scripts/copiar_a_docs_automatico.sh`

```bash
#!/bin/bash
# ============================================================================
# SCRIPT: Copia automática de archivos generados a docs/
# ============================================================================

FECHA_HOY=$(date +'%Y-%m-%d')
BASE_DIR="A-Produccion/Publicaciones-LinkedIn/$FECHA_HOY"

echo "📅 Procesando contenido del día: $FECHA_HOY"

# Crear directorios en docs/
mkdir -p docs/demos/$FECHA_HOY
mkdir -p docs/recursos/$FECHA_HOY

# Copiar demos HTML
if [ -d "$BASE_DIR/demos-html" ]; then
  echo "📊 Copiando demos HTML..."
  cp -v "$BASE_DIR/demos-html"/*.html docs/demos/$FECHA_HOY/
fi

# Copiar recursos
if [ -d "$BASE_DIR/recursos" ]; then
  echo "📥 Copiando recursos..."
  cp -v "$BASE_DIR/recursos"/*.pdf docs/recursos/$FECHA_HOY/
  cp -v "$BASE_DIR/recursos"/*.xml docs/recursos/$FECHA_HOY/
fi

# Actualizar index.html con nuevo contenido
echo "🔄 Actualizando index.html..."
# (Aquí se podría agregar lógica para actualizar automáticamente el index)

echo "✅ Copia completada"
```

---

## 📊 PASO 4: SISTEMA DE TRACKING Y MÉTRICAS

### 4.1 Script de Análisis de Métricas

Crear archivo: `Estrategia-LinkedIn/scripts/analizar_metricas.R`

```r
# ============================================================================
# SCRIPT: Análisis de Métricas de Publicaciones LinkedIn
# ============================================================================

library(tidyverse)
library(lubridate)

# Cargar datos de métricas (manual o API)
metricas <- read_csv("Estrategia-LinkedIn/datos/metricas_publicaciones.csv")

# Análisis de rendimiento
analisis <- metricas %>%
  mutate(
    engagement_rate = (comentarios + compartidos + reacciones) / impresiones * 100,
    ctr = clicks_enlaces / impresiones * 100
  ) %>%
  group_by(categoria) %>%
  summarise(
    publicaciones = n(),
    impresiones_promedio = mean(impresiones),
    engagement_promedio = mean(engagement_rate),
    ctr_promedio = mean(ctr),
    mejor_dia = names(which.max(table(dia_semana))),
    mejor_hora = names(which.max(table(hora_publicacion)))
  )

# Visualización
ggplot(metricas, aes(x = fecha, y = engagement_rate, color = categoria)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Evolución del Engagement por Categoría",
    x = "Fecha",
    y = "Engagement Rate (%)"
  )

# Guardar reporte
write_csv(analisis, "Estrategia-LinkedIn/reportes/analisis_mensual.csv")
```

### 4.2 Plantilla de Registro de Métricas

Crear archivo: `Estrategia-LinkedIn/datos/metricas_publicaciones.csv`

```csv
fecha,categoria,titulo,impresiones,visualizaciones,comentarios,compartidos,reacciones,clicks_enlaces,dia_semana,hora_publicacion
2025-12-24,Estadistica,Consumo Telefónico,5234,1245,23,12,156,67,Martes,08:00
```

---

## 🎨 PASO 5: GENERACIÓN AUTOMÁTICA DE IMÁGENES

### 5.1 Script para Crear Carruseles

Crear archivo: `Estrategia-LinkedIn/scripts/generar_imagenes_carrusel.R`

```r
# ============================================================================
# SCRIPT: Generación Automática de Imágenes para Carrusel LinkedIn
# ============================================================================

library(ggplot2)
library(magick)
library(glue)

generar_carrusel <- function(ejercicio, dir_salida) {

  # Slide 1: Portada
  slide1 <- image_blank(1080, 1080, color = "#667eea") %>%
    image_annotate(
      ejercicio$titulo_corto,
      size = 80,
      color = "white",
      font = "Arial-Bold",
      gravity = "center",
      location = "+0-200"
    ) %>%
    image_annotate(
      "Sistema R/exams para Matemáticas ICFES",
      size = 40,
      color = "white",
      gravity = "center",
      location = "+0+200"
    )

  image_write(slide1, file.path(dir_salida, "slide1_portada.png"))

  # Slide 2: Características
  slide2 <- image_blank(1080, 1080, color = "#764ba2") %>%
    image_annotate(
      "✅ 300+ versiones únicas\n✅ Verificación automática\n✅ Explicaciones paso a paso\n✅ Compatible con Moodle\n✅ 100% código abierto",
      size = 50,
      color = "white",
      gravity = "center"
    )

  image_write(slide2, file.path(dir_salida, "slide2_caracteristicas.png"))

  # Slides 3-5: Screenshots de demos (requiere captura manual o automatizada)

  cat(sprintf("✓ Carrusel generado en: %s\n", dir_salida))
}
```

---

## 📅 PASO 6: CALENDARIO EDITORIAL

### 6.1 Plantilla de Calendario

Crear archivo: `Estrategia-LinkedIn/calendario_editorial.csv`

```csv
semana,fecha,categoria,ejercicio,estado,notas
1,2025-12-24,Estadistica,Consumo Telefónico,publicado,Primera publicación
1,2025-12-25,Estadistica,Datos Climáticos,programado,
1,2025-12-26,Algebra,Ecuaciones Lineales,programado,
1,2025-12-27,Geometria,Áreas y Perímetros,programado,
2,2025-12-30,Estadistica,Gráficos Estadísticos,pendiente,
2,2025-12-31,Algebra,Sistemas de Ecuaciones,pendiente,
```

### 6.2 Script de Planificación

```r
# Generar calendario para el próximo mes
generar_calendario_mes <- function() {
  catalogo <- read_csv("A-Produccion/Biblioteca-Ejercicios-LinkedIn/catalogo_ejercicios.csv")

  # Fechas de publicación (lunes a viernes)
  fechas <- seq(Sys.Date(), Sys.Date() + 30, by = "day") %>%
    .[wday(.) %in% 2:6]  # Solo días laborables

  # Asignar ejercicios rotativamente por categoría
  calendario <- tibble(
    fecha = fechas,
    categoria = rep(c("Estadistica", "Algebra", "Geometria"), length.out = length(fechas))
  ) %>%
    left_join(catalogo, by = "categoria") %>%
    select(fecha, categoria, titulo_corto, nivel, competencia)

  write_csv(calendario, "Estrategia-LinkedIn/calendario_proximo_mes.csv")
  return(calendario)
}
```

---

## 🔔 PASO 7: SISTEMA DE NOTIFICACIONES

### 7.1 Notificaciones por Email

```r
# Enviar email recordatorio diario
enviar_recordatorio <- function(ejercicio, fecha) {
  library(mailR)

  send.mail(
    from = "tu-email@gmail.com",
    to = "tu-email@gmail.com",
    subject = sprintf("📅 Publicación LinkedIn - %s", fecha),
    body = sprintf("
      Recordatorio: Publicar ejercicio de hoy

      Ejercicio: %s
      Categoría: %s
      Nivel: %s

      Tareas pendientes:
      - Revisar demos en GitHub Pages
      - Crear imágenes para carrusel
      - Publicar en LinkedIn a las 8:00 AM
      - Agregar enlaces en comentarios

      ¡Éxito!
    ", ejercicio$titulo_corto, ejercicio$categoria, ejercicio$nivel),
    smtp = list(
      host.name = "smtp.gmail.com",
      port = 465,
      user.name = "tu-email@gmail.com",
      passwd = "tu-password",
      ssl = TRUE
    ),
    authenticate = TRUE,
    send = TRUE
  )
}
```

---

## ✅ CHECKLIST DE IMPLEMENTACIÓN

### Fase 1: Configuración Inicial (Semana 1)

```
□ Crear estructura de directorios
□ Configurar catálogo de ejercicios
□ Instalar dependencias (R, Python, paquetes)
□ Configurar GitHub Actions
□ Probar script de generación manual
□ Configurar sistema de notificaciones
```

### Fase 2: Biblioteca de Contenido (Semana 2-3)

```
□ Crear 10 ejercicios de Estadística
□ Crear 10 ejercicios de Álgebra
□ Crear 10 ejercicios de Geometría
□ Validar funcionamiento de cada ejercicio
□ Generar demos de prueba
□ Documentar cada ejercicio
```

### Fase 3: Automatización (Semana 4)

```
□ Configurar GitHub Actions workflow
□ Probar ejecución automática
□ Configurar sistema de tracking
□ Crear templates de imágenes
□ Establecer calendario editorial
□ Realizar publicación de prueba
```

### Fase 4: Optimización (Mes 2+)

```
□ Analizar métricas de primeras publicaciones
□ Ajustar horarios según engagement
□ Optimizar templates de texto
□ Mejorar calidad de imágenes
□ Expandir biblioteca de ejercicios
□ Automatizar más procesos
```

---

## 📊 MÉTRICAS DE ÉXITO DEL SISTEMA

### KPIs del Sistema de Automatización

- ⏱️ **Tiempo de generación**: < 5 minutos por publicación
- 🎯 **Tasa de éxito**: 95%+ de generaciones sin errores
- 📈 **Consistencia**: Publicación diaria sin fallos
- 💾 **Biblioteca**: 30+ ejercicios únicos en 3 meses
- 📊 **Engagement promedio**: Mantener o mejorar métricas

---

## 🆘 SOLUCIÓN DE PROBLEMAS

### Error: GitHub Actions falla

**Solución**: Verificar logs en Actions tab, revisar permisos del workflow

### Error: Generación de demos falla

**Solución**: Ejecutar script localmente, verificar dependencias R/Python

### Error: Notificaciones no llegan

**Solución**: Verificar configuración SMTP, revisar spam

---

## 🎯 ROADMAP FUTURO

### Mes 1-2: Establecer Rutina

- ✅ Publicación diaria consistente
- ✅ Biblioteca de 30 ejercicios
- ✅ Sistema de tracking funcionando

### Mes 3-4: Optimización

- 🔄 Análisis de métricas y ajustes
- 🔄 Automatización de imágenes
- 🔄 Integración con API de LinkedIn (si disponible)

### Mes 5-6: Expansión

- 🚀 Publicaciones en múltiples plataformas
- 🚀 Colaboraciones con educadores
- 🚀 Webinars y tutoriales en vivo

---

**FECHA DE CREACIÓN**: Diciembre 2025
**ÚLTIMA ACTUALIZACIÓN**: Diciembre 2025
**ESTADO**: ✅ Sistema listo para implementación

