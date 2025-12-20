#!/bin/bash

# ============================================================================
# PROMPT ENHANCER - Sistema de Mejora de Prompts con Contexto del Proyecto
# ============================================================================
# Descripción: Mejora prompts del usuario añadiendo contexto del proyecto
#              ICFES R-Exams desde cualquier ubicación en el repositorio
# Autor: Sistema ICFES R-Exams
# Fecha: 2025-12-20
# ============================================================================

set -euo pipefail

# Colores para output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly CYAN='\033[0;36m'
readonly NC='\033[0m' # No Color

# ============================================================================
# FUNCIÓN: Encontrar la raíz del proyecto
# ============================================================================
find_project_root() {
    local current_dir="$PWD"
    
    while [[ "$current_dir" != "/" ]]; do
        # Buscar marcadores de raíz del proyecto
        if [[ -d "$current_dir/.git" ]] || \
           [[ -d "$current_dir/.augment" ]] || \
           [[ -f "$current_dir/README.md" && -d "$current_dir/A-Produccion" ]]; then
            echo "$current_dir"
            return 0
        fi
        current_dir="$(dirname "$current_dir")"
    done
    
    echo ""
    return 1
}

# ============================================================================
# FUNCIÓN: Obtener contexto de la ubicación actual
# ============================================================================
get_current_context() {
    local project_root="$1"
    local current_dir="$PWD"
    local relative_path="${current_dir#$project_root/}"
    
    # Determinar el tipo de contexto según la ubicación
    local context_type="general"
    local context_description=""
    
    if [[ "$relative_path" == *"A-Produccion/En-Produccion"* ]]; then
        context_type="produccion"
        context_description="Ejercicios en producción (100% funcionales y validados)"
    elif [[ "$relative_path" == *"A-Produccion/En-PreDesarrollo"* ]]; then
        context_type="predesarrollo"
        context_description="Ejercicios en pre-desarrollo (funcionales, en proceso de validación)"
    elif [[ "$relative_path" == *"A-Produccion/En-Desarrollo"* ]]; then
        context_type="desarrollo"
        context_description="Ejercicios en desarrollo activo"
    elif [[ "$relative_path" == *"Auxiliares"* ]]; then
        context_type="auxiliares"
        context_description="Herramientas, scripts y documentación auxiliar"
    elif [[ "$relative_path" == *"A-Produccion/Templates"* ]]; then
        context_type="templates"
        context_description="Plantillas y templates de ejercicios"
    fi
    
    echo "$context_type|$context_description|$relative_path"
}

# ============================================================================
# FUNCIÓN: Leer reglas del proyecto
# ============================================================================
read_project_rules() {
    local project_root="$1"
    local rules_content=""

    # ========== REGLAS DE .augment/rules/ ==========
    local augment_rules_dir="$project_root/.augment/rules"
    if [[ -d "$augment_rules_dir" ]]; then
        rules_content+="## 📋 REGLAS GENERALES DEL PROYECTO (.augment/rules/)\n\n"

        # Leer reglas generales
        if [[ -f "$augment_rules_dir/reglas-generales.md" ]]; then
            rules_content+="### Reglas Generales\n"
            rules_content+="$(head -n 100 "$augment_rules_dir/reglas-generales.md")\n\n"
        fi

        # Leer reglas siempre
        if [[ -f "$augment_rules_dir/siempre.md" ]]; then
            rules_content+="### Reglas Siempre Aplicables\n"
            rules_content+="$(cat "$augment_rules_dir/siempre.md")\n\n"
        fi
    fi

    # ========== DOCUMENTACIÓN DE .claude/ ==========
    local claude_dir="$project_root/.claude"
    if [[ -d "$claude_dir" ]]; then
        rules_content+="## 🔧 DOCUMENTACIÓN TÉCNICA (.claude/)\n\n"

        # Leer README principal de .claude
        if [[ -f "$claude_dir/docs/README.md" ]]; then
            rules_content+="### Documentación Principal\n"
            rules_content+="$(head -n 50 "$claude_dir/docs/README.md")\n\n"
        fi

        # Leer TROUBLESHOOTING
        if [[ -f "$claude_dir/TROUBLESHOOTING.md" ]]; then
            rules_content+="### Solución de Problemas\n"
            rules_content+="$(head -n 30 "$claude_dir/TROUBLESHOOTING.md")\n\n"
        fi

        # Listar skills disponibles
        if [[ -d "$claude_dir/skills" ]]; then
            rules_content+="### Skills Disponibles\n"
            local skills=$(find "$claude_dir/skills" -maxdepth 1 -type d ! -path "$claude_dir/skills" -exec basename {} \; 2>/dev/null)
            if [[ -n "$skills" ]]; then
                while IFS= read -r skill; do
                    rules_content+="- /$skill\n"
                done <<< "$skills"
                rules_content+="\n"
            fi
        fi

        # Listar comandos disponibles
        if [[ -d "$claude_dir/commands" ]]; then
            rules_content+="### Comandos Disponibles\n"
            local commands=$(find "$claude_dir/commands" -name "*.md" -exec basename {} .md \; 2>/dev/null)
            if [[ -n "$commands" ]]; then
                while IFS= read -r cmd; do
                    rules_content+="- /$cmd\n"
                done <<< "$commands"
                rules_content+="\n"
            fi
        fi
    fi

    # ========== GUÍA DE ESTILO DE .claudedoc/ ==========
    local claudedoc_dir="$project_root/.claudedoc"
    if [[ -d "$claudedoc_dir" ]]; then
        rules_content+="## 🎨 GUÍA DE ESTILO ICFES (.claudedoc/)\n\n"

        # Leer guía de estilo
        if [[ -f "$claudedoc_dir/guia_estilo_icfes.md" ]]; then
            rules_content+="### Guía de Estilo ICFES\n"
            rules_content+="$(head -n 50 "$claudedoc_dir/guia_estilo_icfes.md")\n\n"
        fi
    fi

    echo -e "$rules_content"
}

# ============================================================================
# FUNCIÓN: Encontrar ejemplos funcionales relevantes
# ============================================================================
find_relevant_examples() {
    local project_root="$1"
    local context_type="$2"
    local examples_dir="$project_root/A-Produccion/En-Produccion/Ejemplos-Funcionales-Rmd"
    local examples=""
    
    if [[ -d "$examples_dir" ]]; then
        # Listar algunos ejemplos funcionales
        examples="## EJEMPLOS FUNCIONALES DISPONIBLES\n"
        examples+="Ubicación: A-Produccion/En-Produccion/Ejemplos-Funcionales-Rmd/\n\n"
        
        # Buscar archivos .Rmd en ejemplos funcionales (máximo 10)
        local rmd_files=$(find "$examples_dir" -name "*.Rmd" -type f 2>/dev/null | head -n 10)
        
        if [[ -n "$rmd_files" ]]; then
            examples+="Archivos de ejemplo:\n"
            while IFS= read -r file; do
                local rel_path="${file#$project_root/}"
                examples+="- $rel_path\n"
            done <<< "$rmd_files"
        fi
    fi
    
    echo -e "$examples"
}

# ============================================================================
# FUNCIÓN: Mejorar el prompt del usuario
# ============================================================================
enhance_prompt() {
    local user_prompt="$1"
    local project_root="$2"
    
    # Obtener contexto
    local context_info=$(get_current_context "$project_root")
    IFS='|' read -r context_type context_desc relative_path <<< "$context_info"
    
    # Construir prompt mejorado
    local enhanced_prompt=""
    
    enhanced_prompt+="# PROMPT MEJORADO CON CONTEXTO DEL PROYECTO\n\n"
    enhanced_prompt+="## CONTEXTO DE UBICACIÓN\n"
    enhanced_prompt+="- **Proyecto**: RepositorioMatematicasICFES_R_Exams\n"
    enhanced_prompt+="- **Ubicación actual**: $relative_path\n"
    enhanced_prompt+="- **Tipo de contexto**: $context_type\n"
    enhanced_prompt+="- **Descripción**: $context_desc\n\n"

    # Añadir reglas del proyecto
    local rules=$(read_project_rules "$project_root")
    if [[ -n "$rules" ]]; then
        enhanced_prompt+="$rules"
    fi

    # Añadir ejemplos relevantes
    local examples=$(find_relevant_examples "$project_root" "$context_type")
    if [[ -n "$examples" ]]; then
        enhanced_prompt+="$examples\n"
    fi

    # Añadir recomendaciones según el contexto
    enhanced_prompt+="## RECOMENDACIONES SEGÚN CONTEXTO\n"
    case "$context_type" in
        produccion)
            enhanced_prompt+="- Consultar ejemplos funcionales antes de hacer cambios\n"
            enhanced_prompt+="- Mantener compatibilidad con sistema exams2*\n"
            enhanced_prompt+="- Validar cambios con auditorías completas\n"
            ;;
        predesarrollo)
            enhanced_prompt+="- Seguir estructura de ejemplos en producción\n"
            enhanced_prompt+="- Validar diversidad de versiones (300+ mínimo)\n"
            enhanced_prompt+="- Preparar para promoción a producción\n"
            ;;
        desarrollo)
            enhanced_prompt+="- Consultar templates y plantillas\n"
            enhanced_prompt+="- Seguir metodologías establecidas\n"
            enhanced_prompt+="- Documentar cambios y decisiones\n"
            ;;
        auxiliares)
            enhanced_prompt+="- Mantener compatibilidad con scripts existentes\n"
            enhanced_prompt+="- Documentar funcionalidad claramente\n"
            enhanced_prompt+="- Probar en entorno real antes de integrar\n"
            ;;
        templates)
            enhanced_prompt+="- Mantener estructura estándar de templates\n"
            enhanced_prompt+="- Asegurar compatibilidad multi-formato\n"
            enhanced_prompt+="- Documentar parámetros configurables\n"
            ;;
        *)
            enhanced_prompt+="- Consultar documentación del proyecto\n"
            enhanced_prompt+="- Seguir convenciones establecidas\n"
            ;;
    esac
    enhanced_prompt+="\n"

    # Añadir el prompt original del usuario
    enhanced_prompt+="## SOLICITUD DEL USUARIO\n"
    enhanced_prompt+="$user_prompt\n\n"

    enhanced_prompt+="---\n"
    enhanced_prompt+="**Nota**: Este prompt ha sido mejorado automáticamente con contexto del proyecto.\n"
    enhanced_prompt+="Generado desde: $relative_path\n"

    echo -e "$enhanced_prompt"
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
# FUNCIÓN PRINCIPAL
# ============================================================================
main() {
    local user_prompt=""
    local output_file=""
    local use_clipboard=false
    local interactive=true

    # Procesar argumentos
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                show_help
                exit 0
                ;;
            -i|--interactive)
                interactive=true
                shift
                ;;
            -f|--file)
                if [[ -f "$2" ]]; then
                    user_prompt=$(cat "$2")
                    interactive=false
                else
                    echo -e "${RED}Error: Archivo no encontrado: $2${NC}" >&2
                    exit 1
                fi
                shift 2
                ;;
            -o|--output)
                output_file="$2"
                shift 2
                ;;
            -c|--clipboard)
                use_clipboard=true
                shift
                ;;
            *)
                user_prompt="$1"
                interactive=false
                shift
                ;;
        esac
    done

    # Encontrar raíz del proyecto
    echo -e "${CYAN}🔍 Buscando raíz del proyecto...${NC}"
    local project_root=$(find_project_root)

    if [[ -z "$project_root" ]]; then
        echo -e "${RED}❌ Error: No se pudo encontrar la raíz del proyecto${NC}" >&2
        echo -e "${YELLOW}   Asegúrate de estar dentro del repositorio RepositorioMatematicasICFES_R_Exams${NC}" >&2
        exit 1
    fi

    echo -e "${GREEN}✓ Raíz del proyecto encontrada: $project_root${NC}\n"

    # Modo interactivo si no se proporcionó prompt
    if [[ "$interactive" == true && -z "$user_prompt" ]]; then
        echo -e "${CYAN}╔════════════════════════════════════════════════════════════════╗${NC}"
        echo -e "${CYAN}║         MODO INTERACTIVO - PROMPT ENHANCER                     ║${NC}"
        echo -e "${CYAN}╚════════════════════════════════════════════════════════════════╝${NC}\n"

        echo -e "${YELLOW}Ingresa tu prompt (presiona Ctrl+D cuando termines):${NC}"
        user_prompt=$(cat)
        echo ""
    fi

    # Validar que hay un prompt
    if [[ -z "$user_prompt" ]]; then
        echo -e "${RED}❌ Error: No se proporcionó ningún prompt${NC}" >&2
        echo -e "${YELLOW}   Usa -h para ver la ayuda${NC}" >&2
        exit 1
    fi

    # Mejorar el prompt
    echo -e "${CYAN}🚀 Mejorando prompt con contexto del proyecto...${NC}\n"
    local enhanced=$(enhance_prompt "$user_prompt" "$project_root")

    # Mostrar resultado
    echo -e "${GREEN}╔════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║              PROMPT MEJORADO GENERADO                          ║${NC}"
    echo -e "${GREEN}╚════════════════════════════════════════════════════════════════╝${NC}\n"

    echo -e "$enhanced"

    # Guardar en archivo si se especificó
    if [[ -n "$output_file" ]]; then
        echo -e "$enhanced" > "$output_file"
        echo -e "\n${GREEN}✓ Prompt mejorado guardado en: $output_file${NC}"
    fi

    # Copiar al portapapeles si se solicitó
    if [[ "$use_clipboard" == true ]]; then
        if command -v xclip &> /dev/null; then
            echo -e "$enhanced" | xclip -selection clipboard
            echo -e "\n${GREEN}✓ Prompt mejorado copiado al portapapeles${NC}"
        elif command -v pbcopy &> /dev/null; then
            echo -e "$enhanced" | pbcopy
            echo -e "\n${GREEN}✓ Prompt mejorado copiado al portapapeles${NC}"
        else
            echo -e "\n${YELLOW}⚠ Advertencia: xclip o pbcopy no están instalados${NC}"
            echo -e "${YELLOW}   No se pudo copiar al portapapeles${NC}"
        fi
    fi

    echo -e "\n${CYAN}═══════════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}✓ Proceso completado exitosamente${NC}"
    echo -e "${CYAN}═══════════════════════════════════════════════════════════════${NC}\n"
}

# ============================================================================
# EJECUTAR FUNCIÓN PRINCIPAL
# ============================================================================
main "$@"

