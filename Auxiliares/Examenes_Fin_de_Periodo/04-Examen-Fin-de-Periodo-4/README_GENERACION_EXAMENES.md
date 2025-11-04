# 📚 GUÍA DE GENERACIÓN DE EXÁMENES - FIN DE PERIODO 4

## 📋 Descripción General

Este script genera **5 versiones diferentes** del Examen Final de Periodo 4 utilizando las primeras **15 preguntas** (archivos .Rmd numerados del 001 al 015).

## 🎯 Formatos de Salida Generados

El script `SemilleroFinDePeriodo4.R` genera automáticamente **5 archivos diferentes**:

1. **📄 Examen_Periodo4_pandoc_sin_soluciones.docx**
   - Formato: DOCX (Word)
   - Contenido: Solo preguntas, sin soluciones
   - Template: `pcielo_nosol.tex`

2. **📄 Examen_Periodo4_pandoc_con_soluciones.docx**
   - Formato: DOCX (Word)
   - Contenido: Preguntas con soluciones detalladas
   - Template: `pcielo.tex`

3. **📄 Examen_Periodo4_pdf_sin_soluciones.pdf**
   - Formato: PDF
   - Contenido: 5 versiones del examen sin soluciones
   - Template: `exam.tex`

4. **📄 Examen_Periodo4_pdf_con_soluciones.pdf**
   - Formato: PDF
   - Contenido: 5 versiones del examen con soluciones
   - Template: `solpcielo.tex`

5. **📄 Examen_Periodo4_nops.pdf**
   - Formato: PDF escaneable (NOPS)
   - Contenido: 5 versiones con formato de hoja de respuestas escaneable
   - Ideal para: Corrección automática con escáner

## 🔑 Características Clave

### ✅ Consistencia de Semillas Aleatorias

**IMPORTANTE:** Todas las 5 versiones utilizan **exactamente la misma semilla aleatoria** para cada pregunta.

**Esto garantiza que:**
- La versión 1 del examen pandoc sin soluciones tiene las mismas preguntas que la versión 1 del PDF sin soluciones
- La versión 2 del examen pandoc con soluciones tiene las mismas preguntas que la versión 2 del PDF con soluciones
- Y así sucesivamente para todas las 5 versiones

**Beneficio:** Puedes entregar el examen en formato PDF y las soluciones en formato DOCX, sabiendo que corresponden exactamente a las mismas versiones de las preguntas.

### 📊 Preguntas Incluidas

El examen incluye las siguientes 15 preguntas:

1. `001-muestreo_sesgo_municipio_aleatorio_argumentacion_n2_v1.Rmd`
2. `002-cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_2.Rmd`
3. `003-cateto_teorema_pitagoras_geometrico_metrico_formulacion_ejecucion_n2_v1_1.Rmd`
4. `004-pasteleria_sabores_ventas_estadistica_interpretacion_representacion_n2_v1.Rmd`
5. `005-estadistica_media_calificaciones_n2_v1.Rmd`
6. `006-ganancias_comerciales_formulacion_ejecucion_n2_v1.Rmd`
7. `007-proporcionalidad_empresarial_formulacion_ejecucion_n2_v1.Rmd`
8. `008-funciones_lineales_interpretacion_grafica_v2.Rmd`
9. `009-funciones_lineales_interpretacion_grafica_v1.Rmd`
10. `010-empaques_tetra_pak_argumentacion_n3_v1.Rmd`
11. `011-probabilidad_extraccion_bolas_v1.Rmd`
12. `012-probabilidad_combinaciones_v1.Rmd`
13. `013-parabrisas.Rmd`
14. `014-parabrisas-2.Rmd`
15. `015-volumen_cilindro_hueco_R_v1.Rmd`

## 🚀 Instrucciones de Uso

### Requisitos Previos

1. **RStudio** instalado
2. **Paquete exams** instalado en R:
   ```r
   install.packages("exams")
   ```
3. **LaTeX** instalado (para generación de PDFs):
   - En Linux: `sudo apt-get install texlive-full`
   - En Windows: Instalar MiKTeX o TeX Live
   - En macOS: Instalar MacTeX

### Pasos para Generar los Exámenes

1. **Abrir RStudio**

2. **Establecer el directorio de trabajo:**
   ```r
   setwd("/home/bootcamp/Proyectos-2026/RepositorioMatematicasICFES_R_Exams/Auxiliares/Examenes_Fin_de_Periodo/04-Examen-Fin-de-Periodo-4")
   ```

3. **Ejecutar el script:**
   ```r
   source("SemilleroFinDePeriodo4.R")
   ```

4. **Esperar a que termine la generación**
   - El script mostrará mensajes de progreso para cada formato
   - Al finalizar, mostrará un resumen completo

5. **Revisar los archivos generados:**
   - Los archivos se guardarán en la carpeta `./salida/`

## 📁 Estructura de Archivos de Salida

```
salida/
├── Examen_Periodo4_pandoc_sin_soluciones.docx
├── Examen_Periodo4_pandoc_con_soluciones.docx
├── Examen_Periodo4_pdf_sin_soluciones1.pdf
├── Examen_Periodo4_pdf_sin_soluciones2.pdf
├── Examen_Periodo4_pdf_sin_soluciones3.pdf
├── Examen_Periodo4_pdf_sin_soluciones4.pdf
├── Examen_Periodo4_pdf_sin_soluciones5.pdf
├── Examen_Periodo4_pdf_con_soluciones1.pdf
├── Examen_Periodo4_pdf_con_soluciones2.pdf
├── Examen_Periodo4_pdf_con_soluciones3.pdf
├── Examen_Periodo4_pdf_con_soluciones4.pdf
├── Examen_Periodo4_pdf_con_soluciones5.pdf
├── Examen_Periodo4_nops1.pdf
├── Examen_Periodo4_nops2.pdf
├── Examen_Periodo4_nops3.pdf
├── Examen_Periodo4_nops4.pdf
└── Examen_Periodo4_nops5.pdf
```

## 🔧 Personalización

### Cambiar el Número de Versiones

Edita la línea 11 del script:
```r
copias <- 5  # Cambiar este número para generar más o menos versiones
```

### Modificar las Preguntas Incluidas

Edita el vector `archivo_examen` (líneas 27-43) para agregar, quitar o reordenar preguntas.

### Cambiar Información del Encabezado

Modifica los parámetros `header` en cada sección de generación:
```r
header = list(
  Date = format(Sys.Date(), "%d de %B de %Y"),
  Title = "Tu Título Personalizado"
)
```

## ⚠️ Solución de Problemas

### Error: "Faltan archivos .Rmd necesarios"

**Causa:** Uno o más archivos .Rmd del 001 al 015 no existen en el directorio.

**Solución:** Verifica que todos los archivos estén presentes ejecutando:
```bash
ls -1 *.Rmd | grep -E '^0(0[1-9]|1[0-5])-'
```

### Error en la compilación de LaTeX

**Causa:** LaTeX no está instalado o falta algún paquete.

**Solución:** 
- Instala LaTeX completo (texlive-full en Linux)
- Verifica que el PATH incluya los binarios de LaTeX

### Error: "cannot open file 'pcielo.tex'"

**Causa:** Los templates LaTeX no están en el directorio.

**Solución:** Verifica que existan los archivos:
- `pcielo.tex`
- `pcielo_nosol.tex`
- `solpcielo.tex`
- `exam.tex`

## 📊 Información Técnica

- **Resolución de imágenes:** 150 DPI
- **Formato de codificación:** UTF-8
- **Idioma NOPS:** Español (es)
- **Modo NOPS:** Duplex (impresión a doble cara)

## 📞 Soporte

Para problemas o preguntas, consulta la documentación del paquete exams:
```r
?exams2pandoc
?exams2pdf
?exams2nops
```

---

**Última actualización:** 2025-11-04

