# Guía para Tablas en Formato Legal de Dos Columnas

## Problema Identificado

Las tablas generadas por R/exams no se adaptan automáticamente al ancho de las columnas en el formato legal de dos columnas, causando que se desborden y no se visualicen correctamente.

## Soluciones Implementadas

### 1. Modificaciones en las Plantillas - VERSIÓN CORREGIDA

Se han agregado las siguientes configuraciones a todas las plantillas oficio_*.tex:

```latex
% Paquetes adicionales para tablas
\usepackage{array}
\usepackage{tabularx}
\usepackage{ltxtable}
\usepackage{adjustbox}  % CLAVE para forzar adaptación

% Configuración de tablas para dos columnas - FORZAR ADAPTACIÓN
\renewcommand{\arraystretch}{1.0}
\setlength{\tabcolsep}{1pt}

% Redefinir TODOS los entornos de tabla para forzar adaptación
\let\oldtabular\tabular
\let\endoldtabular\endtabular
\renewenvironment{tabular}[1]{%
  \footnotesize
  \setlength{\tabcolsep}{1pt}
  \begin{adjustbox}{width=\columnwidth,center}
  \begin{oldtabular}{#1}
}{%
  \end{oldtabular}
  \end{adjustbox}
}
```

### 2. Plantilla Especializada para Tablas

Se creó `oficio_solpcielo_tablas.tex` con configuraciones ultra-compactas:

- **Tamaño de fuente**: `\tiny` para tablas
- **Espaciado**: `\tabcolsep` reducido a 1.5pt
- **Separación de columnas**: Reducida a 18pt
- **Imágenes**: Reducidas a 0.75\columnwidth

### 3. Comandos Personalizados

```latex
% Para tablas pequeñas
\newcommand{\smalltable}[2]{%
  \footnotesize
  \begin{tabularx}{\columnwidth}{#1}
    #2
  \end{tabularx}
}

% Para tablas muy compactas
\newcommand{\tinytable}[2]{%
  \tiny
  \begin{tabularx}{\columnwidth}{#1}
    #2
  \end{tabularx}
}
```

## Opciones de Uso

### Opción 1: Plantilla Estándar
Para documentos con pocas tablas o tablas simples:
```r
template = "oficio_solpcielo"
```

### Opción 2: Plantilla Especializada
Para documentos con muchas tablas o tablas complejas:
```r
template = "oficio_solpcielo_tablas"
```

### Opción 3: Modificación Manual en Rmd
Si necesitas control específico sobre una tabla, puedes usar en tu archivo .Rmd:

```markdown
\footnotesize
\begin{tabularx}{\columnwidth}{|X|c|}
\hline
Animal & Porcentaje \\
\hline
conejo & 40 \\
tortuga & 35 \\
cerdo & 25 \\
\hline
\end{tabularx}
```

## Configuraciones por Plantilla

| Plantilla | Tamaño Fuente | tabcolsep | Uso Recomendado |
|-----------|---------------|-----------|-----------------|
| oficio_pcielo.tex | small | 3pt | Documentos simples |
| oficio_pcielo_nosol.tex | small | 3pt | Exámenes sin soluciones |
| oficio_solpcielo.tex | footnotesize | 2pt | Uso general |
| oficio_solpcielo_tablas.tex | tiny | 1.5pt | Muchas tablas |

## Parámetros de R/exams Ajustados

Para documentos con tablas, se recomienda:

```r
exams2pdf(...,
          width = 3.5,    # Reducido para columnas
          height = 3.5,   # Reducido para columnas
          resolution = 100)
```

## Troubleshooting

### Problema: Tabla aún se desborda
**Solución**: 
1. Usar `oficio_solpcielo_tablas.tex`
2. Reducir el contenido de las celdas
3. Usar abreviaciones en los encabezados

### Problema: Texto muy pequeño
**Solución**:
1. Usar `oficio_solpcielo.tex` en lugar de la versión tablas
2. Dividir tablas grandes en múltiples tablas más pequeñas
3. Usar `\columnbreak` para forzar salto de columna

### Problema: Tablas no centradas
**Solución**:
Las plantillas ya incluyen `\centering` automático. Si persiste:
```latex
\begin{center}
\footnotesize
\begin{tabular}{...}
...
\end{tabular}
\end{center}
```

## Ejemplos de Uso

### Tabla Simple
```latex
\begin{tabular}{|l|c|}
\hline
Animal & \% \\
\hline
Conejo & 40 \\
Tortuga & 35 \\
\hline
\end{tabular}
```

### Tabla Adaptable
```latex
\begin{tabularx}{\columnwidth}{|X|c|}
\hline
Animal & Porcentaje de personas interesadas \\
\hline
conejo & 40 \\
tortuga & 35 \\
cerdo & 25 \\
\hline
\end{tabularx}
```

## Recomendaciones Generales

1. **Usar abreviaciones** en encabezados largos
2. **Dividir tablas grandes** en múltiples tablas
3. **Probar ambas plantillas** para ver cuál funciona mejor
4. **Verificar siempre** el resultado final en PDF
5. **Considerar orientación** de datos (vertical vs horizontal)

## Archivos Relacionados

- `oficio_pcielo.tex` - Plantilla básica con tablas mejoradas
- `oficio_pcielo_nosol.tex` - Sin soluciones, tablas mejoradas  
- `oficio_solpcielo.tex` - Plantilla completa con tablas mejoradas
- `oficio_solpcielo_tablas.tex` - Especializada para tablas complejas
- `SemilleroUnico_Oficio_v1_modificado.R` - Script con opciones para tablas
