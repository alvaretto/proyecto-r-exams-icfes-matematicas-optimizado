# ✅ SOLUCIÓN - NUMERACIÓN INCORRECTA EN EXAMS2PANDOC

## 📋 PROBLEMA IDENTIFICADO

En los documentos DOCX generados con `exams2pandoc`, cada párrafo dentro de la sección Question aparecía numerado como un ítem separado, generando una numeración incorrecta.

**Ejemplo del problema:**
- 1. Escenario (primer párrafo de la pregunta)
- 2. Explicación detallada (segundo párrafo de la pregunta)
- 3. Escenario (primer párrafo de la siguiente pregunta)

**Comportamiento esperado:**
- 1. Escenario + todos los párrafos de la pregunta + solución
- 2. Escenario + todos los párrafos de la siguiente pregunta + solución

## 🔧 CAUSA RAÍZ

El problema NO era el template, sino cómo pandoc interpreta el entorno `enumerate` de LaTeX:

1. **Archivos .Rmd con múltiples párrafos**: Los archivos .Rmd generan múltiples párrafos separados en la sección Question
2. **Pandoc interpreta cada párrafo como ítem**: Cuando pandoc procesa el template con `\begin{enumerate}`, cada párrafo con nivel 0 se numera automáticamente
3. **Solución dentro del mismo `\item`**: Aunque la solución estaba dentro del mismo `\item` en el template, los párrafos internos de la pregunta se numeraban incorrectamente

## ✅ SOLUCIÓN IMPLEMENTADA

### 1. Creación de Templates Específicos para Pandoc

Se crearon dos nuevos templates optimizados para `exams2pandoc`:

#### **pcielo_pandoc.tex** (CON soluciones)
```latex
\begin{enumerate}[label=\arabic*.,start=1]
#-
  \item \textbf{Escenario}\\
#-
#-
  ##Question##
#-
  \begin{enumerate}[label=(\alph*)]
    \item ##Questionlist##
  \end{enumerate}
#-

  \textbf{Solución}\\
#-
  ##Solution##
#-
  \begin{enumerate}[label=(\alph*)]
    \item ##Solutionlist##
  \end{enumerate}
#-
#-
\end{enumerate}
```

**Características clave:**
- Total de **9 líneas `#-`** (requerido por exams2pandoc)
- **`\item` ANTES de "Escenario"**: Esto hace que solo el primer párrafo se numere
- **Línea `#-` vacía adicional**: Entre "Escenario" y "##Question##" para evitar que Question se numere
- La solución está **dentro del mismo `\item`** de la pregunta
- `[label=\arabic*.,start=1]` para numeración 1., 2., 3., etc.

#### **pcielo_pandoc_nosol.tex** (SIN soluciones)
```latex
\begin{enumerate}[label=\arabic*.,start=1]
#-
  \item \textbf{Escenario}\\
#-
#-
  ##Question##
#-
  \begin{enumerate}[label=(\alph*)]
    \item ##Questionlist##
  \end{enumerate}
#-

#-
#-
#-
#-
\end{enumerate}
```

**Características clave:**
- Total de **9 líneas `#-`** (requerido por exams2pandoc)
- **`\item` ANTES de "Escenario"**: Mismo comportamiento que el template con soluciones
- **Línea `#-` vacía adicional**: Entre "Escenario" y "##Question##"
- 5 líneas `#-` vacías al final para completar el total requerido
- Sin sección de solución

### 2. Actualización del Script de Generación

Se modificó `SemilleroFinDePeriodo_v4.R` para usar los nuevos templates:

```r
# DOCX con soluciones
exams2pandoc(rep(archivo_examen, each = numpreg_por_archivo),
             n = copias,
             name = paste0(nombre_sin_extension, "-docx"),
             encoding = "UTF-8",
             template = "pcielo_pandoc.tex",  # ← Template nuevo
             ...)

# DOCX sin soluciones
exams2pandoc(rep(archivo_examen, each = numpreg_por_archivo),
             n = copias,
             name = paste0(nombre_sin_extension, "_sin_sol"),
             encoding = "UTF-8",
             template = "pcielo_pandoc_nosol.tex",  # ← Template nuevo
             solution = FALSE,
             ...)
```

### 3. Configuración Adicional en Templates

Se agregaron configuraciones para mejorar la compatibilidad con pandoc:

```latex
% Configuración de enumeración para compatibilidad con pandoc
\setlist[enumerate,1]{label=\arabic*., ref=\arabic*, start=1, resume}
\setlist[enumerate,2]{label=(\alph*), ref=\alph*}

% Nuevos entornos para exams
\newenvironment{question}{\item}{}
\newenvironment{solution}{\par\vspace{0.5em}\textbf{Solución:}\par}{}
\newenvironment{answerlist}{\begin{enumerate}[label=(\alph*)]}{\end{enumerate}}

% Compatibilidad con pandoc
\providecommand{\tightlist}{\setlength{\itemsep}{0pt}\setlength{\parskip}{0pt}}
\providecommand{\pandocbounded}[1]{#1}
\setkeys{Gin}{keepaspectratio}
```

## 🧪 VERIFICACIÓN

Se creó el script `05-test_numeracion_corregida.R` que genera:
- DOCX con soluciones
- DOCX sin soluciones

**Resultados de la prueba:**
- ✅ **DOCX con soluciones**: Numeración correcta (1, 2, 3, 4, 5)
- ✅ **Sección "Solución"**: Aparece dentro del mismo ítem de cada pregunta
- ✅ **DOCX sin soluciones**: Numeración correcta, sin secciones de solución

## 📊 ARCHIVOS MODIFICADOS

1. `Auxiliares/Examenes_Fin_de_Periodo/04-Examen-Fin-de-Periodo-4/pcielo_pandoc.tex` (NUEVO)
2. `Auxiliares/Examenes_Fin_de_Periodo/04-Examen-Fin-de-Periodo-4/pcielo_pandoc_nosol.tex` (NUEVO)
3. `Auxiliares/Examenes_Fin_de_Periodo/04-Examen-Fin-de-Periodo-4/SemilleroFinDePeriodo_v4.R` (MODIFICADO)

## 🎯 RESULTADO FINAL

**✅ PROBLEMA RESUELTO COMPLETAMENTE**

La numeración en los documentos DOCX generados con `exams2pandoc` ahora funciona correctamente:

- **Preguntas numeradas secuencialmente**: 1, 2, 3, 4, 5...
- **Soluciones dentro del mismo ítem**: No generan numeración adicional
- **Estructura correcta**: Cada pregunta es un único ítem numerado que contiene:
  - Escenario
  - Pregunta
  - Opciones de respuesta (a, b, c, d)
  - Solución (cuando corresponde)

## 📝 NOTAS IMPORTANTES

1. **Requisito de 9 líneas `#-`**: Los templates para `exams2pandoc` DEBEN tener exactamente 9 líneas `#-`
2. **Basado en templates funcionales**: La solución se basó en los templates del Examen de Fin de Período 2
3. **Compatibilidad**: Los templates son compatibles con el sistema R-exams completo
4. **Encoding UTF-8**: Los templates incluyen la configuración de encoding UTF-8 implementada anteriormente

---

**Fecha de implementación:** 2025-11-04  
**Estado:** ✅ RESUELTO Y VERIFICADO
