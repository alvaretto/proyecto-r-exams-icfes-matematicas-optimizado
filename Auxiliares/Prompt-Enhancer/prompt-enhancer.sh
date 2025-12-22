#!/bin/bash

# ============================================================================
# PROMPT ENHANCER - Sistema de Mejora de Prompts para IA Genérica
# ============================================================================
# Descripción: Mejora prompts del usuario añadiendo contexto del proyecto
#              ICFES R-Exams desde cualquier ubicación en el repositorio.
#              Genera outputs optimizados para cualquier IA.
# Autor: Sistema ICFES R-Exams
# Fecha: 2025-12-20
# Última modificación: 2025-12-21 (Optimizado para IA genérica)
# ============================================================================

set -euo pipefail

# ============================================================================
# CONSTANTES
# ============================================================================

# Colores para output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly CYAN='\033[0;36m'
readonly NC='\033[0m' # No Color

# Límites de lectura de archivos
readonly MAX_LINES_WORKFLOW=80
readonly MAX_LINES_MAIN_DOCS=50
readonly MAX_LINES_TROUBLESHOOTING=40
readonly MAX_LINES_ERROR_PATTERNS=60
readonly MAX_EXAMPLES=10

# Directorios del proyecto (solo .claude/)
readonly CLAUDE_DIR=".claude"
readonly PRODUCTION_DIR="A-Produccion"

# ============================================================================
# FUNCIONES AUXILIARES
# ============================================================================

# Imprime un mensaje de error y termina el script
die() {
    echo -e "${RED}❌ Error: $1${NC}" >&2
    exit "${2:-1}"
}

# Imprime un mensaje de advertencia
warn() {
    echo -e "${YELLOW}⚠ Advertencia: $1${NC}" >&2
}

# Imprime un mensaje de éxito
success() {
    echo -e "${GREEN}✓ $1${NC}"
}

# Imprime un mensaje informativo
info() {
    echo -e "${CYAN}$1${NC}"
}

# Lee un archivo con límite de líneas (si existe)
read_file_limited() {
    local file_path="$1"
    local max_lines="${2:-0}"

    if [[ ! -f "$file_path" ]]; then
        return 1
    fi

    if [[ "$max_lines" -gt 0 ]]; then
        head -n "$max_lines" "$file_path"
    else
        cat "$file_path"
    fi
}

# ============================================================================
# FUNCIÓN: Encontrar la raíz del proyecto
# ============================================================================
find_project_root() {
    local current_dir="$PWD"

    while [[ "$current_dir" != "/" ]]; do
        # Buscar marcadores de raíz del proyecto (solo .claude/)
        if [[ -d "$current_dir/.git" ]] || \
           [[ -d "$current_dir/$CLAUDE_DIR" ]] || \
           [[ -f "$current_dir/README.md" && -d "$current_dir/$PRODUCTION_DIR" ]]; then
            echo "$current_dir"
            return 0
        fi
        current_dir="$(dirname "$current_dir")"
    done

    return 1
}

# ============================================================================
# FUNCIÓN: Obtener contexto de la ubicación actual
# ============================================================================
get_current_context() {
    local project_root="$1"
    local current_dir="$PWD"
    local relative_path="${current_dir#"$project_root"/}"

    # Si estamos en la raíz, relative_path será igual a current_dir
    [[ "$relative_path" == "$current_dir" ]] && relative_path="."

    # Determinar el tipo de contexto según la ubicación
    local context_type="general"
    local context_description=""

    if [[ "$relative_path" == *"$PRODUCTION_DIR/En-Produccion"* ]]; then
        context_type="produccion"
        context_description="Ejercicios en producción (100% funcionales y validados)"
    elif [[ "$relative_path" == *"$PRODUCTION_DIR/En-PreDesarrollo"* ]]; then
        context_type="predesarrollo"
        context_description="Ejercicios en pre-desarrollo (funcionales, en proceso de validación)"
    elif [[ "$relative_path" == *"$PRODUCTION_DIR/En-Desarrollo"* ]]; then
        context_type="desarrollo"
        context_description="Ejercicios en desarrollo activo"
    elif [[ "$relative_path" == *"Auxiliares"* ]]; then
        context_type="auxiliares"
        context_description="Herramientas, scripts y documentación auxiliar"
    elif [[ "$relative_path" == *"$PRODUCTION_DIR/Templates"* ]]; then
        context_type="templates"
        context_description="Plantillas y templates de ejercicios"
    fi

    echo "$context_type|$context_description|$relative_path"
}

# ============================================================================
# FUNCIÓN: Leer workflow paso a paso (prioridad)
# ============================================================================
read_workflow_documentation() {
    local project_root="$1"
    local workflow_content=""
    local claude_dir="$project_root/$CLAUDE_DIR"

    [[ ! -d "$claude_dir" ]] && return 0

    # Priorizar WORKFLOW_PASO_A_PASO.md
    local workflow_file="$claude_dir/docs/WORKFLOW_PASO_A_PASO.md"
    if [[ -f "$workflow_file" ]]; then
        workflow_content+="## 🔄 WORKFLOW DEL PROYECTO\n\n"
        if content=$(read_file_limited "$workflow_file" "$MAX_LINES_WORKFLOW"); then
            workflow_content+="$content\n\n"
        fi
    fi

    echo -e "$workflow_content"
}

# ============================================================================
# FUNCIÓN: Leer hooks disponibles
# ============================================================================
list_available_hooks() {
    local claude_dir="$1"
    local hooks_dir="$claude_dir/hooks"
    local output=""

    [[ ! -d "$hooks_dir" ]] && return 0

    output+="### Hooks de Automatización\n"

    local hooks
    if hooks=$(find "$hooks_dir" -name "*.md" ! -name "README.md" -exec basename {} .md \; 2>/dev/null | sort); then
        if [[ -n "$hooks" ]]; then
            while IFS= read -r hook; do
                output+="- $hook\n"
            done <<< "$hooks"
            output+="\n"
        fi
    fi

    echo -e "$output"
}

# ============================================================================
# FUNCIÓN: Listar skills disponibles
# ============================================================================
list_available_skills() {
    local claude_dir="$1"
    local skills_dir="$claude_dir/skills"
    local output=""

    [[ ! -d "$skills_dir" ]] && return 0

    output+="### Skills Disponibles\n"

    local skills
    if skills=$(find "$skills_dir" -maxdepth 1 -type d ! -path "$skills_dir" -exec basename {} \; 2>/dev/null | sort); then
        if [[ -n "$skills" ]]; then
            while IFS= read -r skill; do
                output+="- /$skill\n"
            done <<< "$skills"
            output+="\n"
        fi
    fi

    echo -e "$output"
}

# ============================================================================
# FUNCIÓN: Listar comandos disponibles
# ============================================================================
list_available_commands() {
    local claude_dir="$1"
    local commands_dir="$claude_dir/commands"
    local output=""

    [[ ! -d "$commands_dir" ]] && return 0

    output+="### Comandos Disponibles\n"

    local commands
    if commands=$(find "$commands_dir" -name "*.md" -exec basename {} .md \; 2>/dev/null | sort); then
        if [[ -n "$commands" ]]; then
            while IFS= read -r cmd; do
                output+="- /$cmd\n"
            done <<< "$commands"
            output+="\n"
        fi
    fi

    echo -e "$output"
}

# ============================================================================
# FUNCIÓN: Leer documentación técnica de .claude/
# ============================================================================
read_claude_documentation() {
    local project_root="$1"
    local docs_content=""
    local claude_dir="$project_root/$CLAUDE_DIR"

    [[ ! -d "$claude_dir" ]] && return 0

    docs_content+="## 🔧 HERRAMIENTAS Y RECURSOS DISPONIBLES\n\n"

    # Leer README principal de .claude/docs
    local readme_file="$claude_dir/docs/README.md"
    if [[ -f "$readme_file" ]]; then
        docs_content+="### Documentación Principal\n"
        if content=$(read_file_limited "$readme_file" "$MAX_LINES_MAIN_DOCS"); then
            docs_content+="$content\n\n"
        fi
    fi

    # Listar skills disponibles
    docs_content+="$(list_available_skills "$claude_dir")"

    # Listar comandos disponibles
    docs_content+="$(list_available_commands "$claude_dir")"

    # Listar hooks disponibles
    docs_content+="$(list_available_hooks "$claude_dir")"

    echo -e "$docs_content"
}

# ============================================================================
# FUNCIÓN: Leer contenido del proyecto (función principal de agregación)
# ============================================================================
read_project_context() {
    local project_root="$1"
    local context_content=""

    # Priorizar workflow paso a paso
    context_content+="$(read_workflow_documentation "$project_root")"

    # Agregar documentación técnica
    context_content+="$(read_claude_documentation "$project_root")"

    echo -e "$context_content"
}

# ============================================================================
# FUNCIÓN: Detectar patrones problemáticos comunes en .Rmd
# ============================================================================
detect_common_errors() {
    local file_path="$1"
    local errors=""
    
    [[ ! -f "$file_path" ]] && return 0
    
    # Error 1: abs() sobre variable formateada
    if grep -nE "abs\([^)]*_formateado\)" "$file_path" 2>/dev/null | grep -vE "^\s*#|#.*abs"; then
        errors+="⚠️  **Error detectado**: Uso de abs() sobre variable formateada\n"
        errors+="   Patrón problemático: abs(variable_formateada)\n"
        errors+="   Solución: Aplicar abs() sobre valor numérico, luego formatear\n"
        errors+="   Documentación: .claude/docs/patrones-errores-conocidos.md#error-2\n\n"
    fi
    
    # Error 2: round(), floor(), ceiling() sobre variable formateada
    if grep -nE "(round|floor|ceiling)\([^)]*_formateado\)" "$file_path" 2>/dev/null | grep -vE "^\s*#"; then
        errors+="⚠️  **Error detectado**: Función matemática sobre variable formateada\n"
        errors+="   Patrón problemático: funcion_matematica(variable_formateada)\n"
        errors+="   Solución: Aplicar función sobre valor numérico, luego formatear\n"
        errors+="   Documentación: .claude/docs/patrones-errores-conocidos.md#error-2\n\n"
    fi
    
    # Advertencia 3: include_tikz() en chunk de generación
    if grep -n "include_tikz" "$file_path" 2>/dev/null | grep -E "generar|data_generation|generar_datos" | grep -vE "^\s*#"; then
        errors+="⚠️  **Advertencia**: include_tikz() en chunk de generación\n"
        errors+="   Puede causar errores 'File not found' en compilación PDF\n"
        errors+="   Solución: Usar renderizado condicional con knitr::is_latex_output()\n"
        errors+="   Documentación: .claude/docs/patrones-errores-conocidos.md#error-1\n\n"
    fi
    
    if [[ -n "$errors" ]]; then
        echo -e "## 🚨 ERRORES DETECTADOS EN ARCHIVO .RMD\n"
        echo -e "$errors"
    fi
}

# ============================================================================
# FUNCIÓN: Leer resumen de errores conocidos
# ============================================================================
read_error_patterns() {
    local project_root="$1"
    local error_file="$project_root/.claude/docs/patrones-errores-conocidos.md"
    
    [[ ! -f "$error_file" ]] && return 0
    
    local output=""
    output+="## 🚨 ERRORES CONOCIDOS Y SOLUCIONES\n\n"
    output+="**Referencia completa:** .claude/docs/patrones-errores-conocidos.md\n\n"
    
    # Extraer resúmenes de errores (primeros 2 errores documentados)
    local error_count=0
    local in_error_section=false
    local current_error=""
    
    while IFS= read -r line || [[ -n "$line" ]]; do
        # Detectar inicio de un error
        if [[ "$line" =~ ^##[[:space:]]Error[[:space:]][0-9]+: ]]; then
            error_count=$((error_count + 1))
            if [[ $error_count -le 2 ]]; then
                in_error_section=true
                current_error=$(echo "$line" | sed 's/^## //')
                output+="### $current_error\n"
            else
                in_error_section=false
            fi
        elif [[ $in_error_section == true ]] && [[ $error_count -le 2 ]]; then
            # Capturar mensaje de error
            if [[ "$line" =~ ^###[[:space:]]❌[[:space:]]Mensaje[[:space:]]de[[:space:]]Error ]]; then
                output+="#### $(echo "$line" | sed 's/^### //')\n"
            elif [[ "$line" =~ ^\`\`\` ]]; then
                # Capturar bloque de código del mensaje de error
                output+="$line\n"
            elif [[ "$line" =~ ^###[[:space:]]✅[[:space:]]Solución ]]; then
                # Detener en la sección de solución para mantener el resumen corto
                break
            fi
        fi
    done < "$error_file"
    
    output+="\n"
    echo -e "$output"
}

# ============================================================================
# FUNCIÓN: Encontrar ejemplos funcionales relevantes
# ============================================================================
find_relevant_examples() {
    local project_root="$1"
    local context_type="$2"
    local examples_dir="$project_root/$PRODUCTION_DIR"
    local examples=""

    [[ ! -d "$examples_dir" ]] && return 0

    examples+="## 📚 EJEMPLOS FUNCIONALES DISPONIBLES\n"
    examples+="Ubicación: $PRODUCTION_DIR/\n\n"

    # Buscar archivos .Rmd en ejemplos funcionales (máximo definido)
    local rmd_files
    if rmd_files=$(find "$examples_dir" -name "*.Rmd" -type f 2>/dev/null | head -n "$MAX_EXAMPLES"); then
        if [[ -n "$rmd_files" ]]; then
            examples+="Archivos de ejemplo:\n"
            while IFS= read -r file; do
                local rel_path="${file#"$project_root"/}"
                examples+="- $rel_path\n"
            done <<< "$rmd_files"
        fi
    fi

    echo -e "$examples"
}

# ============================================================================
# FUNCIÓN: Generar recomendaciones según contexto
# ============================================================================
generate_context_recommendations() {
    local context_type="$1"
    local recommendations=""

    recommendations+="## 💡 RECOMENDACIONES SEGÚN CONTEXTO\n"

    case "$context_type" in
        produccion)
            recommendations+="- Consultar ejemplos funcionales antes de hacer cambios\n"
            recommendations+="- Mantener compatibilidad con sistema exams2*\n"
            recommendations+="- Validar cambios con auditorías completas\n"
            ;;
        predesarrollo)
            recommendations+="- Seguir estructura de ejemplos en producción\n"
            recommendations+="- Validar diversidad de versiones 300+ mínimo\n"
            recommendations+="- Preparar para promoción a producción\n"
            ;;
        desarrollo)
            recommendations+="- Consultar templates y plantillas\n"
            recommendations+="- Seguir metodologías establecidas\n"
            recommendations+="- Documentar cambios y decisiones\n"
            ;;
        auxiliares)
            recommendations+="- Mantener compatibilidad con scripts existentes\n"
            recommendations+="- Documentar funcionalidad claramente\n"
            recommendations+="- Probar en entorno real antes de integrar\n"
            ;;
        templates)
            recommendations+="- Mantener estructura estándar de templates\n"
            recommendations+="- Asegurar compatibilidad multi-formato\n"
            recommendations+="- Documentar parámetros configurables\n"
            ;;
        *)
            recommendations+="- Consultar documentación del proyecto\n"
            recommendations+="- Seguir convenciones establecidas\n"
            ;;
    esac
    recommendations+="\n"

    echo -e "$recommendations"
}

# ============================================================================
# FUNCIÓN: Mejorar el prompt del usuario
# ============================================================================
enhance_prompt() {
    local user_prompt="$1"
    local project_root="$2"

    # Obtener contexto
    local context_info
    context_info=$(get_current_context "$project_root")

    local context_type context_desc relative_path
    IFS='|' read -r context_type context_desc relative_path <<< "$context_info"

    # Construir prompt mejorado con formato markdown estándar para IA genérica
    local enhanced_prompt=""

    enhanced_prompt+="# PROMPT MEJORADO - PROYECTO ICFES R-EXAMS\n\n"

    # Sección 1: Contexto del proyecto
    enhanced_prompt+="## 📍 CONTEXTO DEL PROYECTO\n\n"
    enhanced_prompt+="**Proyecto:** Sistema de generación de ejercicios matemáticos ICFES usando R/exams\n"
    enhanced_prompt+="**Ubicación actual:** \`$relative_path\`\n"
    enhanced_prompt+="**Tipo de contexto:** $context_type\n"
    if [[ -n "$context_desc" ]]; then
        enhanced_prompt+="**Descripción:** $context_desc\n"
    fi
    enhanced_prompt+="\n"

    # Sección 2: Workflow y documentación
    local context
    context=$(read_project_context "$project_root")
    [[ -n "$context" ]] && enhanced_prompt+="$context"

    # Sección 3: Patrones de errores conocidos (solo si el prompt menciona errores)
    if echo "$user_prompt" | grep -qiE "error|problema|falla|corregir|fix|debug"; then
        local error_patterns
        error_patterns=$(read_error_patterns "$project_root")
        if [[ -n "$error_patterns" ]]; then
            enhanced_prompt+="$error_patterns\n"
        fi
    fi

    # Detectar errores si se menciona un archivo .Rmd
    if echo "$user_prompt" | grep -qiE "\.Rmd|\.rmd|archivo.*rmd|error.*rmd|corregir.*rmd"; then
        local rmd_file=""
        rmd_file=$(echo "$user_prompt" | grep -oE "[^\s\"']+\.Rmd" | head -1)

        if [[ -z "$rmd_file" ]]; then
            rmd_file=$(find "$PWD" -maxdepth 2 -name "*.Rmd" -type f 2>/dev/null | head -1)
        fi

        if [[ -n "$rmd_file" && -f "$rmd_file" ]]; then
            local detected_errors
            detected_errors=$(detect_common_errors "$rmd_file")
            if [[ -n "$detected_errors" ]]; then
                enhanced_prompt+="$detected_errors\n"
            fi
        fi
    fi

    # Sección 4: Ejemplos funcionales
    local examples
    examples=$(find_relevant_examples "$project_root" "$context_type")
    [[ -n "$examples" ]] && enhanced_prompt+="$examples\n"

    # Sección 5: Recomendaciones según contexto
    enhanced_prompt+="$(generate_context_recommendations "$context_type")"

    # Sección 6: Solicitud del usuario (destacada)
    enhanced_prompt+="---\n\n"
    enhanced_prompt+="## 🎯 SOLICITUD DEL USUARIO\n\n"
    enhanced_prompt+="> $user_prompt\n\n"

    # Sección 7: Instrucciones para la IA
    enhanced_prompt+="## 📋 INSTRUCCIONES PARA LA IA\n\n"
    enhanced_prompt+="1. **Priorizar** el uso de skills y comandos disponibles en \`.claude/\`\n"
    enhanced_prompt+="2. **Consultar** ejemplos funcionales antes de generar código nuevo\n"
    enhanced_prompt+="3. **Validar** compatibilidad con sistema exams2* (html, pdf, docx, nops)\n"
    enhanced_prompt+="4. **Seguir** el workflow documentado paso a paso\n"
    enhanced_prompt+="5. **Documentar** errores nuevos si se resuelven\n\n"

    enhanced_prompt+="---\n"
    enhanced_prompt+="*Prompt mejorado automáticamente desde: \`$relative_path\`*\n"
    enhanced_prompt+="*Generado: $(date '+%Y-%m-%d %H:%M:%S')*\n"

    echo -e "$enhanced_prompt"
}

# ============================================================================
# FUNCIÓN: Copiar al portapapeles
# ============================================================================
copy_to_clipboard() {
    local content="$1"

    if command -v xclip &> /dev/null; then
        echo -e "$content" | xclip -selection clipboard
        success "Prompt mejorado copiado al portapapeles (xclip)"
    elif command -v pbcopy &> /dev/null; then
        echo -e "$content" | pbcopy
        success "Prompt mejorado copiado al portapapeles (pbcopy)"
    elif command -v wl-copy &> /dev/null; then
        echo -e "$content" | wl-copy
        success "Prompt mejorado copiado al portapapeles (wl-copy)"
    else
        warn "xclip, pbcopy o wl-copy no están instalados"
        warn "No se pudo copiar al portapapeles"
        return 1
    fi
}

# ============================================================================
# FUNCIÓN: Mostrar ayuda
# ============================================================================
show_help() {
    cat << EOF
${CYAN}╔════════════════════════════════════════════════════════════════╗
║    PROMPT ENHANCER - Optimizador para IA Genérica              ║
║    Proyecto ICFES R-Exams                                      ║
╚════════════════════════════════════════════════════════════════╝${NC}

${GREEN}DESCRIPCIÓN:${NC}
  Mejora prompts del usuario añadiendo contexto del proyecto ICFES R-Exams.
  Genera outputs optimizados en formato markdown estándar para cualquier IA.
  SIEMPRE genera un archivo .txt automáticamente.

${GREEN}USO:${NC}
  $0 [OPCIONES] [PROMPT]

  Aliases disponibles: pe, pec, pei, prompt-enhancer

${GREEN}OPCIONES:${NC}
  -h, --help              Mostrar esta ayuda
  -i, --interactive       Modo interactivo (por defecto)
  -f, --file FILE         Leer prompt desde archivo
  -o, --output FILE       Guardar copia adicional en archivo específico
  -c, --clipboard         Copiar también al portapapeles

${GREEN}SALIDA AUTOMÁTICA:${NC}
  Siempre genera: prompt_mejorado_YYYYMMDD_HHMMSS.txt
  Ubicación: Directorio actual de trabajo

${GREEN}EJEMPLOS:${NC}
  # Modo interactivo (genera archivo automáticamente)
  pe

  # Prompt directo
  pe "Genera un ejercicio de geometría nivel 2"

  # Con copia al portapapeles
  pe "Corrige errores TikZ" -c

  # Con archivo adicional
  pe -f mi_prompt.txt -o copia_adicional.txt

${GREEN}CONTEXTO INCLUIDO:${NC}
  El prompt mejorado incluye automáticamente:
  - Workflow paso a paso desde .claude/docs/
  - Skills y comandos disponibles desde .claude/
  - Hooks de automatización
  - Patrones de errores conocidos (si aplica)
  - Ejemplos funcionales relevantes
  - Instrucciones específicas para la IA

${GREEN}COMPATIBILIDAD:${NC}
  El archivo generado es compatible con cualquier IA:
  - ChatGPT, Claude, Gemini, Copilot, etc.
  - Formato markdown estándar
  - Sin dependencias de herramientas específicas

EOF
}

# ============================================================================
# FUNCIÓN: Procesar argumentos de línea de comandos
# ============================================================================
process_arguments() {
    local -n _user_prompt=$1
    local -n _output_file=$2
    local -n _use_clipboard=$3
    local -n _interactive=$4

    shift 4

    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                show_help
                exit 0
                ;;
            -i|--interactive)
                _interactive=true
                shift
                ;;
            -f|--file)
                [[ -z "${2:-}" ]] && die "Opción -f requiere un argumento (archivo)"
                [[ ! -f "$2" ]] && die "Archivo no encontrado: $2"
                _user_prompt=$(cat "$2")
                _interactive=false
                shift 2
                ;;
            -o|--output)
                [[ -z "${2:-}" ]] && die "Opción -o requiere un argumento (archivo de salida)"
                _output_file="$2"
                shift 2
                ;;
            -c|--clipboard)
                _use_clipboard=true
                shift
                ;;
            -*)
                die "Opción desconocida: $1\nUsa -h para ver la ayuda"
                ;;
            *)
                _user_prompt="$1"
                _interactive=false
                shift
                ;;
        esac
    done
}

# ============================================================================
# FUNCIÓN: Leer prompt en modo interactivo
# ============================================================================
read_interactive_prompt() {
    info "╔════════════════════════════════════════════════════════════════╗"
    info "║         MODO INTERACTIVO - PROMPT ENHANCER                     ║"
    info "╚════════════════════════════════════════════════════════════════╝\n"

    echo -e "${YELLOW}Ingresa tu prompt (presiona Ctrl+D cuando termines):${NC}"
    cat
}

# ============================================================================
# FUNCIÓN PRINCIPAL
# ============================================================================
main() {
    local user_prompt=""
    local output_file=""
    local use_clipboard=false
    local interactive=true

    # Procesar argumentos
    process_arguments user_prompt output_file use_clipboard interactive "$@"

    # Encontrar raíz del proyecto
    info "🔍 Buscando raíz del proyecto..."

    local project_root
    project_root=$(find_project_root) || \
        die "No se pudo encontrar la raíz del proyecto\n   Asegúrate de estar dentro del repositorio RepositorioMatematicasICFES_R_Exams"

    success "Raíz del proyecto encontrada: $project_root\n"

    # Modo interactivo si no se proporcionó prompt
    if [[ "$interactive" == true && -z "$user_prompt" ]]; then
        user_prompt=$(read_interactive_prompt)
        echo ""
    fi

    # Validar que hay un prompt
    [[ -z "$user_prompt" ]] && die "No se proporcionó ningún prompt\n   Usa -h para ver la ayuda"

    # Mejorar el prompt
    info "🚀 Mejorando prompt con contexto del proyecto...\n"

    local enhanced
    enhanced=$(enhance_prompt "$user_prompt" "$project_root")

    # Mostrar resultado
    echo -e "${GREEN}╔════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║              PROMPT MEJORADO GENERADO                          ║${NC}"
    echo -e "${GREEN}╚════════════════════════════════════════════════════════════════╝${NC}\n"

    echo -e "$enhanced"

    # SIEMPRE generar archivo .txt automáticamente
    local timestamp
    timestamp=$(date '+%Y%m%d_%H%M%S')
    local auto_output_file="prompt_mejorado_${timestamp}.txt"

    echo -e "$enhanced" > "$auto_output_file" || warn "No se pudo escribir archivo automático: $auto_output_file"
    echo ""
    success "📄 Archivo generado automáticamente: $auto_output_file"

    # Guardar en archivo adicional si se especificó con -o
    if [[ -n "$output_file" ]]; then
        echo -e "$enhanced" > "$output_file" || die "No se pudo escribir en el archivo: $output_file"
        success "📄 Archivo adicional guardado en: $output_file"
    fi

    # Copiar al portapapeles (adicional, no reemplazo)
    if [[ "$use_clipboard" == true ]]; then
        echo ""
        copy_to_clipboard "$enhanced"
    fi

    echo -e "\n${CYAN}═══════════════════════════════════════════════════════════════${NC}"
    success "Proceso completado exitosamente"
    info "📁 Archivo listo para usar con cualquier IA: $auto_output_file"
    echo -e "${CYAN}═══════════════════════════════════════════════════════════════${NC}\n"
}

# ============================================================================
# EJECUTAR FUNCIÓN PRINCIPAL
# ============================================================================
main "$@"
