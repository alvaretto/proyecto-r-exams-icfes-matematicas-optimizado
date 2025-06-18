



















































# R-exams Expertise
- Expert in creating ICFES mathematics exercises using R-exams with mandatory YAML headers, metadata, data generation functions (300+ unique versions), diversity tests, advanced randomization, R-Python integration via reticulate, TikZ diagrams, and complete .Rmd structure following RepositorioMatematicasICFES_R_Exams project standards.

# ICFES Exercise Preferences and Best Practices
- User prefers ICFES mathematics exercises to include both odd and even-numbered datasets for median calculations, not just odd numbers.
- For ICFES mathematics exercises: avoid identical answer options, ensure no duplicate data values (mode issues), and don't reveal the correct order in table displays as it gives away the solution method.
- For ICFES R-exams exercises: always ensure the 4 answer options are different from each other to avoid duplicate choices that could confuse students or create invalid questions.
- User prefers to keep TikZ functionality in R-exams exercises and find working examples rather than removing TikZ when encountering compilation issues.
- User recommends searching the web for information about ICFES 2025 Mathematics Argumentation competency to better understand requirements for exercise development, prioritizing official sources (icfes.gov.co, MEN) with specific searches and validation criteria for competencies, content, and mathematical contexts.

# Advanced Distractor System for R-exams ICFES
- For ICFES R-exams exercises: create 5+ different distractors and randomly select 3 to combine with the correct answer, ensuring varied explanations for different statistical concepts like median calculation to avoid predictable patterns.
- For ICFES R-exams exercises: user wants the distractor system to sometimes generate options with the same justification text but different numerical values (e.g., two options both saying 'because it is the average of the two central values' but with different numbers).
- Sistema avanzado de distractores para R-exams ICFES: implementar 30% probabilidad de valores duplicados con justificaciones diferentes (ej: "mediana es 30 porque promedio centrales" vs "mediana es 30 porque suma/división"), usar 8+ tipos distractores, selección estratégica 1 duplicado + 2 diferentes, verificación textual única, justificaciones alternativas ampliadas para mayor diversidad pedagógica.

# R-exams ICFES Exercise Development Workflow
- For R-exams ICFES exercises: ALWAYS consult functional examples in '/home/pequeniomanjaro/Documentos/proyecto-r-exams-icfes-matematicas-optimizado/Auxiliares/Ejemplos-Funcionales-Rmd/' before generating, correcting, or optimizing any .Rmd file - these examples contain proven solutions from countless error corrections and code optimizations.
- For R-exams ICFES exercises: during error correction (task 6.2), always re-consult the functional examples in addition to error-specific documentation to ensure corrections follow proven patterns.

# Task Management and Preferences
- User wants to create tasks that follow their specific user guidelines and prefers task management to align with their established workflow requirements.
- Master task plan for generating/correcting ICFES R-exams exercises: 6 phases (Analysis+Functional Examples, Technical Configuration, Data Generation+300 versions, Visualizations, Content, Validation+Testing) with 21 specific subtasks following User Guidelines and mandatory functional examples, including mandatory web research for updated official ICFES theoretical information, Python/TikZ graphics prioritization, and comprehensive quality criteria for mathematics exercise development following RepositorioMatematicasICFES_R_Exams standards.
- User prefers Spanish language responses when working with ICFES R-exams exercises and wants to focus using the TEMPLATE_Plan_Tareas_ICFES_R_Exams.md task template.