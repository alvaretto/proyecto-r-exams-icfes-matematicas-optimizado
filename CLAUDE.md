# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This repository contains R-exams based math exercises for ICFES test preparation, with dynamic question generation that creates highly randomized variants of each exercise (minimum 300 variants per exercise). The exercises use R, LaTeX (TikZ), and Python (via reticulate/matplotlib) to generate dynamic content including text, variables, formulas, tables, and graphs.

## Key Commands

### Setup and Installation

```r
# Install required packages (first time setup)
source("Auxiliares/install_packages.R")

# Configure work environment
source("Auxiliares/setup_project.R")
```

### Exercise Generation

```r
# Load R-exams
library(exams)

# Generate PDF versions
exams2pdf("path/to/exercise.Rmd", n = 5)  # Generate 5 random variants

# Generate HTML versions
exams2html("path/to/exercise.Rmd", n = 5)

# Generate NOPS versions for automatic scanning/correction
exams2nops("path/to/exercise.Rmd", n = 30, language = "es")

# Generate Moodle quiz versions
exams2moodle("path/to/exercise.Rmd", n = 30)
```

### Running Unit Tests

```r
# Run unit tests for a specific exercise
source("path/to/exercise/ejecutar_pruebas.R")

# Run specific test type
source("path/to/exercise/verificar_diversidad.R")
```

## Project Structure

The repository follows the thematic structure of the ICFES mathematics curriculum:

- **01-Numeros-Reales/**
- **02-Funciones/**
- **03-Razones-Trigonometricas/**
- **04-Funciones-Identidades-Trigonometricas/**
- **05-Geometria-Analitica/**
- **06-Estadística-Y-Probabilidad/**
- **Auxiliares/** - Helper scripts and documentation
- **Lab/** - Experimental exercises and tests
- **General/Plantillas/** - Templates for new exercises

Each exercise follows this structure:

- **ejercicios/** - .Rmd source files
- **docus/** - Additional documentation
- **salida/** - Generated output files
- **erres/** - R scripts for generation and testing

## Exercise Structure (.Rmd files)

Each exercise .Rmd file has this basic structure:

1. **Metadata section**
   - YAML header with output formats
   - ICFES metadata (competence, difficulty level, content type, etc.)

2. **Setup code chunk**
   - Libraries (exams, ggplot2, dplyr, knitr, etc.)
   - Configuration settings

3. **Data generation code chunk**
   - Random generation of variables, parameters, context
   - Solution calculation
   - Generation of answer options

4. **Visualization code chunk**
   - Charts, tables, or diagrams generation

5. **Question section**
   - Problem statement
   - Answer options

6. **Solution section**
   - Detailed explanation
   - Answer key

7. **Meta-information section**
   - Exercise name, type, solution, tolerance, etc.

## Key Development Patterns

1. **Exercise Creation**:
   - Start by copying a template: `cp Auxiliares/plantilla_ejercicio_icfes.Rmd my_new_exercise.Rmd`
   - Each exercise should generate at least 300 unique variants

2. **Testing Methodology**:
   - Unit tests validate mathematical coherence and diversity
   - Check data consistency, realistic proportions, temporal trends
   - Verify at least 300 possible variants

3. **Graphics Generation**:
   - Use TikZ for vector graphics (preferred for mathematical diagrams)
   - Use ggplot2 for statistical visualization
   - Use matplotlib via reticulate for complex Python-based visualizations

4. **ICFES Metadata System**:
   - Tag exercises with competence, difficulty level, content category
   - Align with ICFES reference framework

## Dependencies

- **R**: v4.0+ (with exams, knitr, rmarkdown, ggplot2, dplyr, testthat, etc.)
- **RStudio**: Recommended for development
- **LaTeX**: Required for PDF generation
- **TikZ**: For vector graphics
- **Python**: Optional for matplotlib plots via reticulate

## Output Formats

- **PDF**: High-quality printable documents
- **HTML**: Web-based interactive versions
- **NOPS**: Scannable versions for automatic correction
- **Moodle**: LMS-compatible question format