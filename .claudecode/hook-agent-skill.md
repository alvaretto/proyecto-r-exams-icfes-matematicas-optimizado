En el contexto de **Claude Code** (la herramienta de línea de comandos de Anthropic para programación agéntica), estos tres conceptos son piezas fundamentales para automatizar y personalizar cómo la IA interactúa con tu proyecto.

Dado que tu repositorio (`proyecto-r-exams-icfes-matematicas-optimizado`) parece estar enfocado en generar exámenes de matemáticas tipo ICFES usando el paquete `exams` de R, aquí te explico qué es cada componente y cómo podrías aplicarlo específicamente para potenciar tu flujo de trabajo en ese repositorio.

### 1. Hook (Gancho)
Un **Hook** es un script o comando que se ejecuta **automáticamente** cuando ocurre un evento específico en Claude Code (por ejemplo, antes de que Claude edite un archivo, después de guardar, o antes de enviar un mensaje). Sirven para imponer reglas, formateo o chequeos de seguridad sin que tengas que pedírselo.

*   **Concepto:** "Si pasa X, haz Y automáticamente".
*   **Ejemplo aplicado a tu proyecto:**
    *   **Hook de validación de RMarkdown:** Podrías crear un hook `PostWrite` (después de escribir) que, cada vez que Claude modifique un archivo de pregunta (`.Rmd`), ejecute automáticamente un script de R para verificar que el código dentro del archivo compila sin errores.
    *   **Hook de estilo:** Un hook que ejecute el paquete `styler` de R automáticamente sobre cualquier archivo `.R` modificado para asegurar que el código siga el estándar de estilo tidyverse antes de que tú lo revises.

### 2. Agent (o Subagent)
Un **Agent** (o Subagente en la arquitectura de Claude Code) es una instancia especializada de la IA a la que se le delega una tarea compleja completa. A diferencia del chat principal, un agente puede trabajar en paralelo o enfocarse en una misión grande (como "refactorizar todo el directorio de geometría") sin "contaminar" el contexto de tu conversación principal.

*   **Concepto:** "Contrata a un experto temporal para hacer una tarea grande y avísame cuando termine".
*   **Ejemplo aplicado a tu proyecto:**
    *   **Agente "Creador de Preguntas":** Podrías invocar un subagente y decirle: *"Genera 5 variaciones de preguntas de cálculo diferencial siguiendo la plantilla del ICFES"*. Este agente leería tus plantillas actuales, crearía los 5 archivos `.Rmd`, probaría que generan el PDF correctamente y solo te reportaría el resultado final, en lugar de mostrarte paso a paso la creación de cada archivo en tu terminal principal.

### 3. Agent Skill (Habilidad de Agente)
Un **Agent Skill** es una capacidad o herramienta personalizada que le das a Claude. Se define mediante un archivo (usualmente `SKILL.md` en una carpeta `.claude/skills`) que le enseña a Claude **cómo** usar una herramienta específica o realizar un procedimiento técnico que no conoce por defecto. A diferencia de los comandos, Claude decide por sí mismo cuándo usar una Skill según el contexto.

*   **Concepto:** "Enséñale a Claude a usar una herramienta específica de tu proyecto para que la use cuando la necesite".
*   **Ejemplo aplicado a tu proyecto:**
    *   **Skill `compilar_examen`:** Podrías crear una Skill que le enseñe a Claude cómo ejecutar el script principal de generación de exámenes (ej. `exams2pdf(...)`).
        *   *Contenido de la Skill:* Le explicarías que para compilar, debe ejecutar `Rscript generar_examen.R --args [tema]`.
        *   *Uso:* Si tú le dices en el chat: *"El examen de álgebra no se ve bien, ¿puedes arreglarlo?"*, Claude automáticamente usará la Skill `compilar_examen` para regenerar el PDF después de intentar arreglar el código, sin que tú le tengas que dar el comando exacto de R.

### Resumen de aplicación para tu repositorio

| Concepto | Función | Ejemplo en tu Proyecto R-Exams |
| :--- | :--- | :--- |
| **Hook** | Automatización reactiva | Ejecutar `knitr::purl()` automáticamente para extraer el código R de un `.Rmd` cada vez que se guarda, para verificar sintaxis. |
| **Agent** | Delegación de tareas grandes | "Revisa las 50 preguntas de la carpeta `/geometria` y asegúrate de que todas tengan la etiqueta `exsolution` correcta". |
| **Skill** | Nueva capacidad/herramienta | Enseñar a Claude a interpretar los logs de error específicos del paquete `exams` de R para que sepa cómo corregir fallos de compilación LaTeX por su cuenta. |