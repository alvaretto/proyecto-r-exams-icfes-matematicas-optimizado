#!/bin/bash

# ============================================================================
# PROMPT ENHANCER - Sistema de Mejora de Prompts con Contexto del Proyecto
# ============================================================================
# Descripción: Mejora prompts del usuario añadiendo contexto del proyecto
#              ICFES R-Exams desde cualquier ubicación en el repositorio
# Autor: Sistema ICFES R-Exams
# Fecha: 2025-12-20
# Última modificación: 2025-12-21 (Refactorización)
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
readonly MAX_LINES_GENERAL_RULES=100
readonly MAX_LINES_MAIN_DOCS=50
readonly MAX_LINES_TROUBLESHOOTING=30
readonly MAX_LINES_STYLE_GUIDE=50
readonly MAX_EXAMPLES=10

# Directorios y archivos del proyecto
readonly CLAUDE_DIR=".claude"
readonly CLAUDEDOC_DIR=".claudedoc"
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
        # Buscar marcadores de raíz del proyecto
        if [[ -d "$current_dir/.git" ]] || \
           [[ -d "$current_dir/$CLAUDE_DIR" ]] || \
           [[ -d "$current_dir/.augment" ]] || \
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
# FUNCIÓN: Leer reglas de .claude/
# ============================================================================
read_claude_rules() {
    local project_root="$1"
    local rules_content=""
    local augment_rules_dir="$project_root/$CLAUDE_DIR"

    [[ ! -d "$augment_rules_dir" ]] && return 0

    rules_content+="## 📋 REGLAS GENERALES DEL PROYECTO $CLAUDE_DIR\n\n"

    # Leer reglas generales
    local general_rules_file="$augment_rules_dir/reglas-generales.md"
    if [[ -f "$general_rules_file" ]]; then
        rules_content+="### Reglas Generales\n"
        if content=$(read_file_limited "$general_rules_file" "$MAX_LINES_GENERAL_RULES"); then
            rules_content+="$content\n\n"
        fi
    fi

    # Leer reglas siempre
    local always_rules_file="$augment_rules_dir/siempre.md"
    if [[ -f "$always_rules_file" ]]; then
        rules_content+="### Reglas Siempre Aplicables\n"
        if content=$(read_file_limited "$always_rules_file"); then
            rules_content+="$content\n\n"
        fi
    fi

    echo -e "$rules_content"
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
# FUNCIÓN: Leer documentación de .claude/
# ============================================================================
read_claude_documentation() {
    local project_root="$1"
    local docs_content=""
    local claude_dir="$project_root/$CLAUDE_DIR"

    [[ ! -d "$claude_dir" ]] && return 0

    docs_content+="## 🔧 DOCUMENTACIÓN TÉCNICA $CLAUDE_DIR/\n\n"

    # Leer README principal de .claude
    local readme_file="$claude_dir/docs/README.md"
    if [[ -f "$readme_file" ]]; then
        docs_content+="### Documentación Principal\n"
        if content=$(read_file_limited "$readme_file" "$MAX_LINES_MAIN_DOCS"); then
            docs_content+="$content\n\n"
        fi
    fi

    # Leer TROUBLESHOOTING
    local troubleshooting_file="$claude_dir/TROUBLESHOOTING.md"
    if [[ -f "$troubleshooting_file" ]]; then
        docs_content+="### Solución de Problemas\n"
        if content=$(read_file_limited "$troubleshooting_file" "$MAX_LINES_TROUBLESHOOTING"); then
            docs_content+="$content\n\n"
        fi
    fi

    # Listar skills y comandos disponibles
    docs_content+="$(list_available_skills "$claude_dir")"
    docs_content+="$(list_available_commands "$claude_dir")"

    echo -e "$docs_content"
}

# ============================================================================
# FUNCIÓN: Leer guía de estilo de .claudedoc/
# ============================================================================
read_style_guide() {
    local project_root="$1"
    local style_content=""
    local claudedoc_dir="$project_root/$CLAUDEDOC_DIR"

    [[ ! -d "$claudedoc_dir" ]] && return 0

    style_content+="## 🎨 GUÍA DE ESTILO ICFES $CLAUDEDOC_DIR/\n\n"

    local style_guide_file="$claudedoc_dir/guia_estilo_icfes.md"
    if [[ -f "$style_guide_file" ]]; then
        style_content+="### Guía de Estilo ICFES\n"
        if content=$(read_file_limited "$style_guide_file" "$MAX_LINES_STYLE_GUIDE"); then
            style_content+="$content\n\n"
        fi
    fi

    echo -e "$style_content"
}

# ============================================================================
# FUNCIÓN: Leer reglas del proyecto (función principal de agregación)
# ============================================================================
read_project_rules() {
    local project_root="$1"
    local rules_content=""

    # Agregar todas las reglas y documentación
    rules_content+="$(read_claude_rules "$project_root")"
    rules_content+="$(read_claude_documentation "$project_root")"
    rules_content+="$(read_style_guide "$project_root")"

    echo -e "$rules_content"
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

    # Construir prompt mejorado
    local enhanced_prompt=""

    enhanced_prompt+="# PROMPT MEJORADO CON CONTEXTO DEL PROYECTO\n\n"
    enhanced_prompt+="## 📍 CONTEXTO DE UBICACIÓN\n"
    enhanced_prompt+="- **Proyecto**: RepositorioMatematicasICFES_R_Exams\n"
    enhanced_prompt+="- **Ubicación actual**: $relative_path\n"
    enhanced_prompt+="- **Tipo de contexto**: $context_type\n"
    enhanced_prompt+="- **Descripción**: $context_desc\n\n"

    # Añadir reglas del proyecto
    local rules
    rules=$(read_project_rules "$project_root")
    [[ -n "$rules" ]] && enhanced_prompt+="$rules"

    # Añadir ejemplos relevantes
    local examples
    examples=$(find_relevant_examples "$project_root" "$context_type")
    [[ -n "$examples" ]] && enhanced_prompt+="$examples\n"

    # Añadir recomendaciones según el contexto
    enhanced_prompt+="$(generate_context_recommendations "$context_type")"

    # Añadir el prompt original del usuario
    enhanced_prompt+="## 🎯 SOLICITUD DEL USUARIO\n"
    enhanced_prompt+="$user_prompt\n\n"

    enhanced_prompt+="---\n"
    enhanced_prompt+="**Nota**: Este prompt ha sido mejorado automáticamente con contexto del proyecto.\n"
    enhanced_prompt+="Generado desde: $relative_path\n"

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
║         PROMPT ENHANCER - Sistema ICFES R-Exams                ║
╚════════════════════════════════════════════════════════════════╝${NC}

${GREEN}DESCRIPCIÓN:${NC}
  Mejora prompts del usuario añadiendo contexto del proyecto ICFES R-Exams.
  Funciona desde cualquier ubicación dentro del repositorio.

${GREEN}USO:${NC}
  $0 [OPCIONES] [PROMPT]

${GREEN}OPCIONES:${NC}
  -h, --help              Mostrar esta ayuda
  -i, --interactive       Modo interactivo (por defecto)
  -f, --file FILE         Leer prompt desde archivo
  -o, --output FILE       Guardar prompt mejorado en archivo
  -c, --clipboard         Copiar prompt mejorado al portapapeles

${GREEN}EJEMPLOS:${NC}
  # Modo interactivo (por defecto)
  $0

  # Prompt directo
  $0 "Genera un ejercicio de geometría nivel 2"

  # Desde archivo
  $0 -f mi_prompt.txt -o prompt_mejorado.txt

  # Copiar al portapapeles
  $0 "Corrige errores TikZ" -c

${GREEN}CONTEXTO DETECTADO:${NC}
  El script detecta automáticamente:
  - Ubicación actual en el proyecto
  - Tipo de contenido (producción, desarrollo, auxiliares, etc.)
  - Reglas aplicables del proyecto
  - Ejemplos funcionales relevantes

${GREEN}SALIDA:${NC}
  Prompt mejorado con:
  - Contexto de ubicación
  - Reglas del proyecto
  - Ejemplos funcionales
  - Recomendaciones específicas
  - Solicitud original del usuario

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

    # Guardar en archivo si se especificó
    if [[ -n "$output_file" ]]; then
        echo -e "$enhanced" > "$output_file" || die "No se pudo escribir en el archivo: $output_file"
        echo ""
        success "Prompt mejorado guardado en: $output_file"
    fi

    # Copiar al portapapeles si se solicitó
    if [[ "$use_clipboard" == true ]]; then
        echo ""
        copy_to_clipboard "$enhanced"
    fi

    echo -e "\n${CYAN}═══════════════════════════════════════════════════════════════${NC}"
    success "Proceso completado exitosamente"
    echo -e "${CYAN}═══════════════════════════════════════════════════════════════${NC}\n"
}

# ============================================================================
# EJECUTAR FUNCIÓN PRINCIPAL
# ============================================================================
main "$@"
