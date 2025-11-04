# 📚 ÍNDICE DE SCRIPTS Y DOCUMENTACIÓN - EXAMEN FIN DE PERIODO 4

## 📁 Estructura de Archivos

```
04-Examen-Fin-de-Periodo-4/
├── 📄 SemilleroFinDePeriodo4.R          ⭐ SCRIPT PRINCIPAL
├── 📄 verificar_requisitos.R            🔍 Verificación previa
├── 📄 prueba_rapida.R                   🧪 Prueba rápida (1 versión)
├── 📖 00-RESUMEN_ADAPTACION_SCRIPT.md   📋 Resumen ejecutivo
├── 📖 README_GENERACION_EXAMENES.md     📚 Guía completa de uso
├── 📖 INDICE_SCRIPTS.md                 📑 Este archivo
└── 📂 salida/                           📦 Archivos generados
```

## 🎯 Flujo de Trabajo Recomendado

### 1️⃣ VERIFICAR REQUISITOS
```r
source("verificar_requisitos.R")
```
**Propósito:** Verificar que todo esté listo antes de generar exámenes

### 2️⃣ PRUEBA RÁPIDA (Opcional pero recomendado)
```r
source("prueba_rapida.R")
```
**Propósito:** Generar 1 versión de prueba para verificar funcionamiento

### 3️⃣ GENERAR EXÁMENES COMPLETOS
```r
source("SemilleroFinDePeriodo4.R")
```
**Propósito:** Generar las 5 versiones completas en todos los formatos

## �� Descripción de Archivos

### ⭐ SemilleroFinDePeriodo4.R
**Tipo:** Script R principal  
**Función:** Genera 5 versiones del examen en 5 formatos diferentes  
**Salidas:**
- Examen_Periodo4_pandoc_sin_soluciones.docx
- Examen_Periodo4_pandoc_con_soluciones.docx
- Examen_Periodo4_pdf_sin_soluciones.pdf (5 versiones)
- Examen_Periodo4_pdf_con_soluciones.pdf (5 versiones)
- Examen_Periodo4_nops.pdf (5 versiones)

**Características:**
- ✅ Usa semilla única para consistencia
- ✅ Verifica archivos antes de generar
- ✅ Mensajes informativos de progreso
- ✅ Genera 15 preguntas (001-015)

### 🔍 verificar_requisitos.R
**Tipo:** Script R de verificación  
**Función:** Verifica que todos los requisitos estén cumplidos  
**Verifica:**
- ✅ Paquete 'exams' instalado
- ✅ Archivos .Rmd (001-015) disponibles
- ✅ Templates LaTeX disponibles
- ✅ LaTeX instalado en el sistema
- ✅ Directorio de salida

**Uso:**
```r
source("verificar_requisitos.R")
```

### 🧪 prueba_rapida.R
**Tipo:** Script R de prueba  
**Función:** Genera 1 versión de prueba para verificar funcionamiento  
**Salidas:**
- PRUEBA_pandoc_sin_soluciones.docx
- PRUEBA_pdf_sin_soluciones.pdf

**Uso:**
```r
source("prueba_rapida.R")
```

**Limpiar archivos de prueba:**
```r
unlink("salida_prueba", recursive = TRUE)
```

### 📋 00-RESUMEN_ADAPTACION_SCRIPT.md
**Tipo:** Documentación Markdown  
**Función:** Resumen ejecutivo de la adaptación realizada  
**Contenido:**
- Objetivo cumplido
- Archivos creados/modificados
- Formatos de salida
- Características implementadas
- Comparación con script original
- Validación realizada

### �� README_GENERACION_EXAMENES.md
**Tipo:** Documentación Markdown  
**Función:** Guía completa de uso del sistema  
**Contenido:**
- Descripción general
- Formatos de salida generados
- Características clave
- Instrucciones paso a paso
- Personalización
- Solución de problemas
- Información técnica

### 📑 INDICE_SCRIPTS.md
**Tipo:** Documentación Markdown  
**Función:** Índice de todos los archivos y scripts  
**Contenido:**
- Este archivo que estás leyendo

## 🎓 Guía Rápida de Uso

### Para Usuarios Nuevos

1. **Lee primero:** `README_GENERACION_EXAMENES.md`
2. **Verifica requisitos:** `source("verificar_requisitos.R")`
3. **Prueba rápida:** `source("prueba_rapida.R")`
4. **Genera exámenes:** `source("SemilleroFinDePeriodo4.R")`

### Para Usuarios Experimentados

```r
# Verificar y generar en un solo paso
source("verificar_requisitos.R")
source("SemilleroFinDePeriodo4.R")
```

## 📊 Preguntas Incluidas (001-015)

| # | Archivo | Tema |
|---|---------|------|
| 001 | muestreo_sesgo_municipio_aleatorio_argumentacion_n2_v1.Rmd | Muestreo y sesgo |
| 002 | cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_2.Rmd | Teorema de Pitágoras |
| 003 | cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd | Teorema de Pitágoras |
| 004 | pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd | Estadística - ventas |
| 005 | estadistica_media_calificaciones_n2_v1.Rmd | Estadística - media |
| 006 | ganancias_comerciales_formulacion_ejecucion_n2_v1.Rmd | Ganancias comerciales |
| 007 | proporcionalidad_empresarial_formulacion_ejecucion_n2_v1.Rmd | Proporcionalidad |
| 008 | funciones_lineales_interpretacion_grafica_v2.Rmd | Funciones lineales |
| 009 | funciones_lineales_interpretacion_grafica_v1.Rmd | Funciones lineales |
| 010 | empaques_tetra_pak_argumentacion_n3_v1.Rmd | Geometría - volumen |
| 011 | probabilidad_extraccion_bolas_v1.Rmd | Probabilidad |
| 012 | probabilidad_combinaciones_v1.Rmd | Probabilidad |
| 013 | parabrisas.Rmd | Geometría |
| 014 | parabrisas-2.Rmd | Geometría |
| 015 | volumen_cilindro_hueco_R_v1.Rmd | Geometría - volumen |

## �� Personalización

### Cambiar Número de Versiones

Editar `SemilleroFinDePeriodo4.R` línea 12:
```r
copias <- 5  # Cambiar a 10, 20, etc.
```

### Modificar Preguntas

Editar `SemilleroFinDePeriodo4.R` líneas 27-43:
```r
archivo_examen <- c(
  "001-...",
  "002-...",
  # Agregar, quitar o reordenar
)
```

## 📞 Soporte

Para más información, consulta:
- **Guía completa:** `README_GENERACION_EXAMENES.md`
- **Resumen ejecutivo:** `00-RESUMEN_ADAPTACION_SCRIPT.md`
- **Documentación R-exams:** `?exams2pandoc`, `?exams2pdf`, `?exams2nops`

---

**Última actualización:** 2025-11-04  
**Versión:** 1.0  
**Estado:** ✅ Operativo
