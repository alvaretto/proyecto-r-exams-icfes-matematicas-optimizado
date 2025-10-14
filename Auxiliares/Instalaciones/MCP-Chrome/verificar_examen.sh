#!/bin/bash

# Script para generar prompts de verificación de exámenes R/exams
# Uso: ./verificar_examen.sh [archivo.html]

# Colores
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Directorio base del proyecto
PROJECT_DIR="/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams"

function mostrar_ayuda() {
    echo -e "${BLUE}╔══════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║                                                              ║${NC}"
    echo -e "${BLUE}║     Verificador de Exámenes R/exams con Chrome DevTools     ║${NC}"
    echo -e "${BLUE}║                                                              ║${NC}"
    echo -e "${BLUE}╚══════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    echo "Este script genera prompts para usar con Copilot en VS Code Insiders"
    echo ""
    echo "Uso:"
    echo "  ./verificar_examen.sh [archivo.html]"
    echo "  ./verificar_examen.sh                    # Modo interactivo"
    echo ""
    echo "Ejemplos:"
    echo "  ./verificar_examen.sh out_html_cloze/render11.html"
    echo "  ./verificar_examen.sh                    # Selecciona de una lista"
    echo ""
}

function listar_examenes() {
    echo -e "${YELLOW}Buscando exámenes HTML...${NC}"
    echo ""
    
    # Buscar archivos HTML en directorios de salida
    EXAMENES=()
    
    if [ -d "$PROJECT_DIR/out_html_cloze" ]; then
        while IFS= read -r file; do
            EXAMENES+=("$file")
        done < <(find "$PROJECT_DIR/out_html_cloze" -name "*.html" -type f)
    fi
    
    if [ -d "$PROJECT_DIR/salida_hibrida" ]; then
        while IFS= read -r file; do
            EXAMENES+=("$file")
        done < <(find "$PROJECT_DIR/salida_hibrida" -name "*.html" -type f)
    fi
    
    if [ ${#EXAMENES[@]} -eq 0 ]; then
        echo -e "${RED}No se encontraron exámenes HTML${NC}"
        exit 1
    fi
    
    echo -e "${GREEN}Exámenes encontrados:${NC}"
    echo ""
    
    for i in "${!EXAMENES[@]}"; do
        BASENAME=$(basename "${EXAMENES[$i]}")
        DIRNAME=$(dirname "${EXAMENES[$i]}" | sed "s|$PROJECT_DIR/||")
        echo "  $((i+1))) $DIRNAME/$BASENAME"
    done
    
    echo ""
}

function generar_prompt_verificacion_basica() {
    local archivo="$1"
    local url="file://$archivo"
    
    echo -e "${GREEN}═══════════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}PROMPT: Verificación Básica${NC}"
    echo -e "${GREEN}═══════════════════════════════════════════════════════════════${NC}"
    echo ""
    echo "Copia y pega esto en Copilot (VS Code Insiders):"
    echo ""
    echo -e "${YELLOW}───────────────────────────────────────────────────────────────${NC}"
    cat << EOF
Abre $url y realiza una verificación básica:
1. Verifica errores de JavaScript en la consola
2. Toma un screenshot de la página completa
3. Lista todas las imágenes y verifica que carguen
4. Dame un resumen de cualquier problema encontrado
EOF
    echo -e "${YELLOW}───────────────────────────────────────────────────────────────${NC}"
    echo ""
}

function generar_prompt_analisis_completo() {
    local archivo="$1"
    local url="file://$archivo"
    
    echo -e "${GREEN}═══════════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}PROMPT: Análisis Completo${NC}"
    echo -e "${GREEN}═══════════════════════════════════════════════════════════════${NC}"
    echo ""
    echo "Copia y pega esto en Copilot (VS Code Insiders):"
    echo ""
    echo -e "${YELLOW}───────────────────────────────────────────────────────────────${NC}"
    cat << EOF
Realiza un análisis completo de $url:

1. ERRORES:
   - Errores de JavaScript en consola
   - Warnings importantes
   - Recursos que no cargaron (404, 500)

2. IMÁGENES:
   - Lista todas las imágenes
   - Verifica que todas carguen correctamente
   - Identifica imágenes rotas

3. MATEMÁTICAS:
   - Espera 3 segundos para que MathJax cargue
   - Verifica errores de MathJax
   - Cuenta fórmulas renderizadas correctamente

4. RENDIMIENTO:
   - Tiempo de carga total
   - Tamaño de recursos
   - Recursos más pesados

5. VISUAL:
   - Toma screenshot de página completa
   - Toma screenshot de la primera pregunta

Dame un reporte detallado con todos los hallazgos.
EOF
    echo -e "${YELLOW}───────────────────────────────────────────────────────────────${NC}"
    echo ""
}

function generar_prompt_responsive() {
    local archivo="$1"
    local url="file://$archivo"
    
    echo -e "${GREEN}═══════════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}PROMPT: Verificación Responsive${NC}"
    echo -e "${GREEN}═══════════════════════════════════════════════════════════════${NC}"
    echo ""
    echo "Copia y pega esto en Copilot (VS Code Insiders):"
    echo ""
    echo -e "${YELLOW}───────────────────────────────────────────────────────────────${NC}"
    cat << EOF
Verifica el diseño responsive de $url:

1. Móvil (375x667 - iPhone SE):
   - Cambia viewport a 375x667
   - Toma screenshot
   - Verifica que el contenido sea legible

2. Tablet (768x1024 - iPad):
   - Cambia viewport a 768x1024
   - Toma screenshot
   - Verifica que el layout se adapte

3. Desktop (1920x1080):
   - Cambia viewport a 1920x1080
   - Toma screenshot
   - Verifica que use bien el espacio

Dame un reporte con los screenshots y cualquier problema de diseño.
EOF
    echo -e "${YELLOW}───────────────────────────────────────────────────────────────${NC}"
    echo ""
}

function generar_prompt_mathjax() {
    local archivo="$1"
    local url="file://$archivo"
    
    echo -e "${GREEN}═══════════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}PROMPT: Verificación MathJax${NC}"
    echo -e "${GREEN}═══════════════════════════════════════════════════════════════${NC}"
    echo ""
    echo "Copia y pega esto en Copilot (VS Code Insiders):"
    echo ""
    echo -e "${YELLOW}───────────────────────────────────────────────────────────────${NC}"
    cat << EOF
Verifica el renderizado de fórmulas matemáticas en $url:

1. Abre el archivo
2. Espera 5 segundos para que MathJax cargue completamente
3. Ejecuta este código JavaScript:
   {
     mathjaxLoaded: typeof MathJax !== 'undefined',
     errores: document.querySelectorAll('.MathJax_Error').length,
     formulas: document.querySelectorAll('.MathJax').length,
     previews: document.querySelectorAll('.MathJax_Preview').length
   }
4. Toma un screenshot de las fórmulas
5. Dame un reporte del estado de MathJax
EOF
    echo -e "${YELLOW}───────────────────────────────────────────────────────────────${NC}"
    echo ""
}

function generar_prompt_metadatos_icfes() {
    local archivo="$1"
    local url="file://$archivo"
    
    echo -e "${GREEN}═══════════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}PROMPT: Verificación Metadatos ICFES${NC}"
    echo -e "${GREEN}═══════════════════════════════════════════════════════════════${NC}"
    echo ""
    echo "Copia y pega esto en Copilot (VS Code Insiders):"
    echo ""
    echo -e "${YELLOW}───────────────────────────────────────────────────────────────${NC}"
    cat << EOF
Extrae y verifica los metadatos ICFES de $url:

Ejecuta este código JavaScript:
{
  metadatos: Array.from(document.querySelectorAll('meta[name^="icfes"]')).map(m => ({
    nombre: m.name,
    contenido: m.content
  })),
  competencias: Array.from(document.querySelectorAll('[data-competencia]')).map(el => ({
    competencia: el.dataset.competencia,
    componente: el.dataset.componente,
    afirmacion: el.dataset.afirmacion,
    nivel: el.dataset.nivel
  })),
  distribucionNiveles: Array.from(document.querySelectorAll('[data-nivel]')).reduce((acc, el) => {
    acc[el.dataset.nivel] = (acc[el.dataset.nivel] || 0) + 1;
    return acc;
  }, {})
}

Dame un reporte estructurado de los metadatos encontrados.
EOF
    echo -e "${YELLOW}───────────────────────────────────────────────────────────────${NC}"
    echo ""
}

function menu_prompts() {
    local archivo="$1"
    
    while true; do
        clear
        echo -e "${BLUE}╔══════════════════════════════════════════════════════════════╗${NC}"
        echo -e "${BLUE}║          Generador de Prompts para Copilot                  ║${NC}"
        echo -e "${BLUE}╚══════════════════════════════════════════════════════════════╝${NC}"
        echo ""
        echo -e "${GREEN}Archivo seleccionado:${NC}"
        echo "  $(basename "$archivo")"
        echo ""
        echo "Selecciona el tipo de verificación:"
        echo ""
        echo "  1) Verificación Básica"
        echo "  2) Análisis Completo"
        echo "  3) Verificación Responsive"
        echo "  4) Verificación MathJax"
        echo "  5) Metadatos ICFES"
        echo "  6) Generar TODOS los prompts"
        echo "  0) Volver"
        echo ""
        echo -n "Opción: "
        read opcion
        
        case $opcion in
            1)
                clear
                generar_prompt_verificacion_basica "$archivo"
                echo ""
                echo "Presiona Enter para continuar..."
                read
                ;;
            2)
                clear
                generar_prompt_analisis_completo "$archivo"
                echo ""
                echo "Presiona Enter para continuar..."
                read
                ;;
            3)
                clear
                generar_prompt_responsive "$archivo"
                echo ""
                echo "Presiona Enter para continuar..."
                read
                ;;
            4)
                clear
                generar_prompt_mathjax "$archivo"
                echo ""
                echo "Presiona Enter para continuar..."
                read
                ;;
            5)
                clear
                generar_prompt_metadatos_icfes "$archivo"
                echo ""
                echo "Presiona Enter para continuar..."
                read
                ;;
            6)
                clear
                generar_prompt_verificacion_basica "$archivo"
                echo ""
                generar_prompt_analisis_completo "$archivo"
                echo ""
                generar_prompt_responsive "$archivo"
                echo ""
                generar_prompt_mathjax "$archivo"
                echo ""
                generar_prompt_metadatos_icfes "$archivo"
                echo ""
                echo "Presiona Enter para continuar..."
                read
                ;;
            0)
                return
                ;;
            *)
                echo "Opción inválida"
                sleep 2
                ;;
        esac
    done
}

# Main
if [ "$1" == "-h" ] || [ "$1" == "--help" ]; then
    mostrar_ayuda
    exit 0
fi

if [ -n "$1" ]; then
    # Archivo especificado como argumento
    if [ ! -f "$1" ]; then
        echo -e "${RED}Error: Archivo no encontrado: $1${NC}"
        exit 1
    fi
    ARCHIVO_ABSOLUTO=$(realpath "$1")
    menu_prompts "$ARCHIVO_ABSOLUTO"
else
    # Modo interactivo
    listar_examenes
    echo -n "Selecciona un examen (número): "
    read seleccion
    
    if ! [[ "$seleccion" =~ ^[0-9]+$ ]] || [ "$seleccion" -lt 1 ] || [ "$seleccion" -gt ${#EXAMENES[@]} ]; then
        echo -e "${RED}Selección inválida${NC}"
        exit 1
    fi
    
    ARCHIVO_SELECCIONADO="${EXAMENES[$((seleccion-1))]}"
    menu_prompts "$ARCHIVO_SELECCIONADO"
fi

