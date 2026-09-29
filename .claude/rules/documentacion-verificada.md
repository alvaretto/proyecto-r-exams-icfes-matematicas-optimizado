# Principio de Documentación Verificada (versión compacta)

> Texto íntegro —estructura obligatoria de un patrón, tabla de resultados—: `.claude/docs/reglas/documentacion-verificada.md`.

**Sólo se documenta lo 100 % verificado y funcionando.** Nada de errores sin solución confirmada, soluciones parciales, "posibles soluciones" ni hipótesis.

Para documentar un patrón en `.claude/docs/patrones-errores-conocidos.md`: error reproducible → solución aplicada en un `.Rmd` real → validada al menos con `exams2pdf()` y `exams2html()` (idealmente DOCX y NOPS) → entrada con mensaje exacto, causa raíz, código antes/después, validación, checklist, ejemplo funcional e historial con tabla de resultados por formato.

Al actualizar: probar, subir versión, añadir historial, **no borrar** la solución anterior. Un patrón obsoleto se marca `⚠️ OBSOLETO` y se conserva.
