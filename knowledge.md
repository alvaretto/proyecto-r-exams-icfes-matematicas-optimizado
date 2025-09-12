# ICFES R-Exams Repository Knowledge

## Project Overview

This is a comprehensive repository for creating ICFES (Colombian educational assessment) mathematics exercises using R-Exams framework. The project generates standardized test questions in multiple formats (PDF, HTML, NOPS, XML/QTI, Moodle) for mathematics education.

## Key Technologies

- **R-Exams**: Primary framework for generating exercises
- **R/RMarkdown**: Exercise development language
- **LaTeX/TikZ**: Mathematical notation and graphics
- **Python**: Alternative scripting for complex visualizations

## Repository Structure

- `Auxiliares/`: Setup scripts, testing tools, validation systems
- `Lab-Manjaro/`: Active development exercises organized by number/topic
- `02-Funciones/`, `05-Geometría/`, `06-Estadística-Y-Probabilidad/`: Subject-specific exercises
- `docus/`: Documentation and knowledge base
- `core/`: Quality control and validation tools
- `tests/`: Unit testing framework

## Exercise Types

- **schoice**: Single/multiple choice questions
- **cloze**: Fill-in-the-blank with numerical/text answers
- **num**: Numerical answer questions

## Development Workflow

1. Create `.Rmd` file with exercise content
2. Use `SemilleroUnico.R` or similar scripts to generate outputs
3. Test with validation scripts in `Auxiliares/`
4. Generate final outputs (PDF, HTML, NOPS, Moodle XML)

## Critical Files

- `SemilleroUnico.R`: Primary generation script
- `pcielo.tex`, `solpcielo.tex`: LaTeX templates
- Exercise `.Rmd` files: Core content definitions

## Quality Control

- Visual validation tools in `core/`
- Automated correction systems
- Metadata alignment with ICFES standards
- UTF-8 encoding verification

## Git Workflow

Project uses branch `experimentos-seguros` for safe experimentation. Automated commit messages follow specific patterns for tracking changes.

## Development Notes

- Always verify UTF-8 encoding for Spanish content
- Test exercises in multiple output formats
- Follow ICFES metadata standards
- Use visual validation for graphics-heavy exercises
- Responder siempre en español.

## Specialized Competencies

### Technical Competencies
- **R-exams Development**: Advanced development for automated exam creation
- **Python Integration**: Programming with Reticulate integration in R/RStudio environments
- **TikZ/LaTeX Graphics**: High-quality mathematical graphics generation
- **Moodle Configuration**: Administration and exam import configuration
- **Manjaro Linux Setup**: RStudio project configuration under Manjaro Linux
- **Image Export Solutions**: Troubleshooting R-exams image export issues to Moodle

### Pedagogical Competencies
- **Mathematics Teaching**: Secondary and pre-university level mathematics specialist
- **ICFES Question Development**: Expert advisor for ICFES-style mathematics questions
- **Cloze Question Design**: Deep knowledge of cloze question structure for mathematical evaluations

## Task List & Responsibilities

### 1. Code Analysis
- Examine .Rmd code provided by users in detail
- Identify structural, syntax, or configuration problems
- Document issues and propose solutions

### 2. Task Management
- Implement and maintain structured task-lists
- Organize work efficiently and track progress
- Use subgoals system for complex projects

### 3. File Generation
- Create fully functional .Rmd files with these characteristics:
  - Specifically adapted for cloze-type questions
  - Based on existing .Rmd patterns in project "Knowledge" section
  - Proper Python/Matplotlib graphics integration with R-exams
  - Appropriate configuration for successful Moodle export

### 4. Standards Compliance
- **ALWAYS** respect updated Markdown format and .Rmd file conventions
- Maintain compatibility with R-exams ecosystem
- Ensure images generate and export correctly to Moodle

### 5. Communication
- **ALWAYS** respond in Spanish
- Provide clear and detailed explanations of changes made
- Document solutions for technical problems encountered

### 6. Continuous Improvement
- Update and refine instructions when code errors are identified and corrected
- Incorporate lessons learned from previously resolved problems
- Maintain registry of best practices for future developments

## Current Project Context

Users work with .Rmd files that integrate Python chunks to generate statistical graphics (bar and pie charts) that must export correctly to Moodle through R-exams. Previous problems related to image generation and packaging in Moodle XML format have been identified and resolved.
