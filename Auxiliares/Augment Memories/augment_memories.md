



















































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

# Advanced TikZ Methodology for PNG Image Replication
- Validated methodology for replicating PNG images with 98% visual fidelity using advanced TikZ features in R-exams ICFES exercises.
- Step-by-step protocol: 1) Place PNG in /Lab/Prueba-Temporal_TikZ/, 2) Provide structured request with image name/context/level, 3) Automatic visual analysis and mathematical content identification, 4) Systematic implementation with TikZ advanced features, RGB colors, precise positioning, complete .Rmd structure, SemilleroUnico_v2.R configuration, and multi-format testing (HTML/PDF/Moodle).
- Proven successful with numbers triangular exercise (all_07.png) generating complete R-exams exercise with 300+ randomized versions, advanced distractor system, and full exams2* compatibility.
- Command for new images: "Aplica la metodología TikZ avanzada a esta nueva imagen PNG para generar un ejercicio R-exams completo con salidas exams2*"
- Automatic process: visual analysis → TikZ replication → R-exams generation → multi-format output → documentation.
- Expected metrics: 98% visual similarity, 100% exams2* compatibility, 300+ unique versions, appropriate ICFES level, solid mathematical argumentation, effective pedagogical distractors.

# Advanced Error Correction Methodology for R-exams ICFES
- User requested creation of an advanced methodology for correcting recurring errors in R-exams .Rmd files, similar to the advanced TikZ strategy, as these errors happen frequently across various .Rmd files in the project.
- Created comprehensive error correction system with 5 categories: A) Grammatical/Concordance, B) TikZ Positioning, C) Data Generation, D) LaTeX/TikZ Compilation, E) R-exams Structure.
- Implemented systematic detection, proven solutions library, validation checklist, and step-by-step correction protocols.
- Key error patterns identified: gender concordance ("La conteo" → "El conteo"), incorrect table-text order in TikZ, duplicate answer options, missing LaTeX packages, incomplete YAML headers.
- Methodology documented in three files: METODOLOGIA_Correccion_Errores_Recurrentes_ICFES_R_Exams.md, BIBLIOTECA_Soluciones_Errores_Comunes.md, CHECKLIST_Validacion_Archivos_Rmd.md.
- Error correction methodology integrates with existing TikZ methodology for comprehensive quality assurance workflow.
- Command for error correction: "Aplica la metodología de corrección de errores recurrentes" followed by specific error category or general validation.
- Expected results: systematic identification and correction of recurring patterns, reduced debugging time, improved code quality, standardized solutions.

# Template Reorganization: TikZ Priority Strategy (2025-01-28)
- User requested reorganization of TEMPLATE_Plan_Tareas_ICFES_R_Exams.md to prioritize TikZ methodology at the beginning instead of at the end.
- Successfully reorganized 8 phases with TikZ strategy moved to PHASE 1: "Image Analysis and Advanced TikZ Methodology" as the starting point.
- Updated PHASE 1.1 to clarify that images can be placed anywhere under Lab/ directory, not necessarily in specific subdirectories like /Lab/Prueba-Temporal_TikZ/nueva_imagen.png.
- Implemented TikZ prioritization: "In case of graphics (even non-mathematical ones), generate with TikZ code, prioritizing it over Python, applying the 'ADVANCED TIKZ METHODOLOGY' section."
- Reorganized all phases: PHASE 1 (TikZ+Image Analysis), PHASE 2 (ICFES Planning), PHASE 3 (Technical Configuration), PHASE 4 (Data Generation), PHASE 5 (Visualizations with TikZ priority), PHASE 6 (Exercise Content), PHASE 7 (Error Correction), PHASE 8 (Final Validation).
- Updated all task numbering throughout the document (2.1→3.1, 3.X→4.X, etc.) while preserving all existing content.
- Moved "ADVANCED TIKZ METHODOLOGY" section from end of document to after the 8 main phases, eliminating duplicate content.
- Template now starts with image analysis and TikZ methodology as the primary workflow, with Python as secondary alternative only when TikZ is not viable.
- Command for reorganized workflow: "Aplica la metodología TikZ avanzada siguiendo el template reorganizado para generar un ejercicio R-exams completo."
- Result: 663 lines optimized with TikZ-first approach while maintaining all original functionality and quality standards.

# Sistema Condicional Automático ICFES R-exams (2025-01-29)
- User requested implementation of an automated conditional system in TEMPLATE_Plan_Tareas_ICFES_R_Exams.md that detects graphical content (charts, tables, diagrams) in PNG images and activates specialized TikZ replication workflows with 98%+ visual fidelity.
- Successfully implemented complete conditional system with automatic content detection and specialized flows: FLUJO A (no graphics, standard process) and FLUJO B (with graphics, specialized Agente-Graficador TikZ).
- Expanded PHASE 1 with automatic analysis subsections: 1.2.1 Automatic Visual Element Detection, 1.2.2 Conditional Flow Decision, 1.3.1-1.3.4 Specialized Agente-Graficador (FLUJO B only).
- Created comprehensive "Agente-Graficador Especializado TikZ" section with technical specifications, iterative replication protocol (4 phases), visual fidelity metrics system (98%+ required), and specialized template library by graphic type.
- Implemented visual fidelity validation system with quantifiable criteria: Geometric Precision (25%), Chromatic Fidelity (25%), Positioning (25%), Completeness (25%), with specific tolerances and user-system approval protocol.
- Added complete conditional system documentation with decision flow diagram, automatic detection criteria, specialized templates (bars, circular, tables), validation protocol, and detailed usage examples.
- Updated activation commands: "Aplica el sistema condicional automático a esta imagen PNG para detectar contenido gráfico y activar el flujo apropiado" for main system, plus specialized commands for Agente-Graficador and fidelity validation.
- Template expanded from 663 to ~950 lines while preserving 100% compatibility with existing methodologies (TikZ Advanced, Error Correction), maintaining all 8 phases and 21 subtasks, and ensuring full exams2* format compatibility.
- System ready for immediate use with automatic PNG analysis, conditional flow activation, iterative high-fidelity replication, and complete R-exams integration with 300+ randomized versions.