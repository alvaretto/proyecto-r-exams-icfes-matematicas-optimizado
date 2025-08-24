#!/bin/bash

# Comandos de Ejemplo para Gemini CLI - R-exams ICFES
# Proyecto: RepositorioMatematicasICFES_R_Exams
# Versión: 1.0 | Fecha: Agosto 2025

echo "🚀 COMANDOS DE EJEMPLO PARA GEMINI CLI - R-EXAMS ICFES"
echo "====================================================="
echo ""
echo "Copia y pega estos comandos según tu necesidad:"
echo ""

# =============================================================================
# COMANDOS BÁSICOS
# =============================================================================

echo "📋 COMANDOS BÁSICOS:"
echo "==================="
echo ""

echo "# 1. Verificar configuración completa"
echo ".gemini/scripts/verify_setup.sh"
echo ""

echo "# 2. Iniciar Gemini CLI (modo básico)"
echo "gemini-icfes --basic"
echo ""

echo "# 3. Iniciar Gemini CLI (modo optimizado)"
echo "gemini-icfes --optimized"
echo ""

echo "# 4. Ayuda del sistema"
echo "gemini-icfes --help"
echo ""

# =============================================================================
# ANÁLISIS DE EJERCICIOS
# =============================================================================

echo "🔍 ANÁLISIS DE EJERCICIOS:"
echo "=========================="
echo ""

echo "# Analizar ejercicio específico"
echo 'gemini --context-file "ruta/ejercicio.Rmd" \'
echo '       --context-file ".gemini/rules-gemini.md" \'
echo '       "Analiza este ejercicio R-exams: estructura técnica, competencia ICFES, nivel de dificultad y calidad de distractores"'
echo ""

echo "# Validar estándares ICFES"
echo 'gemini --context-file "GEMINI.md" \'
echo '       --context-file "ejercicio.Rmd" \'
echo '       "Valida este ejercicio contra los estándares ICFES matemáticas: competencia, nivel, contexto colombiano y distractores"'
echo ""

echo "# Optimizar aleatorización"
echo 'gemini --context-file "ejercicio.Rmd" \'
echo '       --context-file ".gemini/rules-gemini.md" \'
echo '       "Optimiza este ejercicio R-exams: mejora la aleatorización para generar 300+ versiones únicas, optimiza el código R"'
echo ""

# =============================================================================
# GENERACIÓN DE CÓDIGO TIKZ
# =============================================================================

echo "🎨 GENERACIÓN DE CÓDIGO TIKZ:"
echo "============================="
echo ""

echo "# Generar TikZ desde imagen"
echo 'gemini --image "ruta/imagen.png" \'
echo '       --context-file ".gemini/rules-gemini.md" \'
echo '       "Analiza esta imagen matemática y genera código TikZ equivalente con fidelidad 98%. Usa elementos en negrita cursiva y escala apropiada."'
echo ""

echo "# Crear gráfico de función cuadrática"
echo 'gemini --context-file ".gemini/rules-gemini.md" \'
echo '       "Genera código TikZ para una función cuadrática f(x)=ax²+bx+c con vértice visible, ejes etiquetados y elementos en negrita cursiva"'
echo ""

echo "# Crear diagrama geométrico"
echo 'gemini --context-file ".gemini/rules-gemini.md" \'
echo '       "Genera código TikZ para un triángulo rectángulo con catetos a=3 y b=4, hipotenusa c=5, ángulos marcados y medidas etiquetadas"'
echo ""

# =============================================================================
# CREACIÓN DE EJERCICIOS
# =============================================================================

echo "📝 CREACIÓN DE EJERCICIOS:"
echo "=========================="
echo ""

echo "# Crear ejercicio de álgebra"
echo 'gemini --context-file "GEMINI.md" \'
echo '       --context-file ".gemini/rules-gemini.md" \'
echo '       "Crea un ejercicio R-exams sobre ecuaciones cuadráticas con: aleatorización de coeficientes, 4 opciones sobre discriminante, competencia formulación y ejecución, nivel 2 ICFES, contexto colombiano"'
echo ""

echo "# Crear ejercicio de geometría"
echo 'gemini --context-file "GEMINI.md" \'
echo '       --context-file ".gemini/rules-gemini.md" \'
echo '       "Crea un ejercicio R-exams sobre área de triángulos con: aleatorización de medidas, gráfico TikZ, 4 opciones de respuesta, competencia interpretación y representación, nivel 1-2 ICFES"'
echo ""

echo "# Crear ejercicio de estadística"
echo 'gemini --context-file "GEMINI.md" \'
echo '       --context-file ".gemini/rules-gemini.md" \'
echo '       "Crea un ejercicio R-exams sobre medidas de tendencia central con: datos aleatorios, tabla de frecuencias, 4 opciones sobre media/mediana, competencia argumentación, nivel 3 ICFES"'
echo ""

# =============================================================================
# CONSULTAS DE CONTEXTO
# =============================================================================

echo "📚 CONSULTAS DE CONTEXTO:"
echo "========================="
echo ""

echo "# Explicar competencias ICFES"
echo 'gemini --context-file "GEMINI.md" \'
echo '       "Explica las 3 competencias ICFES matemáticas con ejemplos específicos para cada nivel de dificultad"'
echo ""

echo "# Consultar metodologías del proyecto"
echo 'gemini --context-file "GEMINI.md" \'
echo '       "Resume las metodologías principales del proyecto R-exams ICFES y sus mejores prácticas"'
echo ""

echo "# Obtener ejemplos de TikZ"
echo 'gemini --context-file ".gemini/rules-gemini.md" \'
echo '       "Proporciona 3 ejemplos de código TikZ para gráficos matemáticos comunes en ejercicios ICFES"'
echo ""

# =============================================================================
# COMANDOS DE VERIFICACIÓN
# =============================================================================

echo "🔧 COMANDOS DE VERIFICACIÓN:"
echo "============================"
echo ""

echo "# Verificar setup completo"
echo ".gemini/scripts/verify_setup.sh"
echo ""

echo "# Verificar configuración de imágenes"
echo ".gemini/scripts/verify_gitignore_images.sh"
echo ""

echo "# Verificar API Key"
echo 'echo $GEMINI_API_KEY'
echo ""

echo "# Verificar configuración Pro"
echo 'cat ~/.config/gemini/icfes-config.json | grep -E "(model|max_tokens|temperature)"'
echo ""

# =============================================================================
# COMANDOS DE PRODUCTIVIDAD
# =============================================================================

echo "⚡ COMANDOS DE PRODUCTIVIDAD:"
echo "============================"
echo ""

echo "# Crear alias útiles (agregar a ~/.bashrc)"
echo "alias gicfes='gemini-icfes --basic'"
echo "alias ganalizar='gemini --context-file \".gemini/rules-gemini.md\"'"
echo "alias gvalidar='gemini --context-file \"GEMINI.md\" --context-file \".gemini/rules-gemini.md\"'"
echo ""

echo "# Script para análisis rápido de ejercicio"
echo "cat > analizar_ejercicio.sh << 'EOF'"
echo "#!/bin/bash"
echo "if [ \$# -eq 0 ]; then"
echo "    echo \"Uso: ./analizar_ejercicio.sh archivo.Rmd\""
echo "    exit 1"
echo "fi"
echo "gemini --context-file \"\$1\" \\"
echo "       --context-file \".gemini/rules-gemini.md\" \\"
echo "       \"Analiza este ejercicio R-exams: estructura, ICFES, optimizaciones\""
echo "EOF"
echo "chmod +x analizar_ejercicio.sh"
echo ""

# =============================================================================
# COMANDOS DE TESTING
# =============================================================================

echo "🧪 COMANDOS DE TESTING:"
echo "======================="
echo ""

echo "# Compilar ejercicio R-exams a HTML"
echo "R -e \"library(exams); exams2html('ejercicio.Rmd')\""
echo ""

echo "# Generar múltiples versiones"
echo "R -e \"library(exams); set.seed(123); exams2html('ejercicio.Rmd', n=5)\""
echo ""

echo "# Verificar metadatos"
echo "grep -E \"^ex(name|type|solution|competencia|nivel):\" ejercicio.Rmd"
echo ""

echo "# Contar líneas de código TikZ"
echo "grep -c \"tikzpicture\" ejercicio.Rmd"
echo ""

# =============================================================================
# COMANDOS DE EXTENSIÓN VSCODE COMPANION
# =============================================================================

echo "🎯 COMANDOS DE EXTENSIÓN VSCODE COMPANION:"
echo "=========================================="
echo ""

echo "# IMPORTANTE: Estos comandos se ejecutan en el terminal integrado de VSCode Insiders"
echo "# Primero abrir VSCode Insiders: code-insiders ."
echo ""

echo "# Activar Gemini CLI en el proyecto (ejecutar en terminal VSCode)"
echo "/id install"
echo ""

echo "# Inicializar memoria semántica del proyecto"
echo "/init"
echo ""

echo "# Gestión de memoria avanzada"
echo "/memory show"
echo "/memory add \"Configuración exitosa para ejercicios de álgebra\""
echo "/memory add \"TikZ fidelidad 98%: scale=1.0, font=\\\\bfseries\\\\itshape\""
echo "/memory add \"ICFES competencia interpretación: contextos colombianos, nivel 1-2\""
echo "/memory refresh"
echo ""

echo "# Gestión de conversaciones"
echo "/chat save \"desarrollo_ejercicio_geometria\""
echo "/chat save \"optimizacion_tikz_funciones\""
echo "/chat list"
echo "/chat resume \"desarrollo_ejercicio_geometria\""
echo "/chat delete \"sesion_antigua\""
echo ""

echo "# Comandos combinados para desarrollo R-exams"
echo "/memory add \"Patrón exitoso: aleatorización con sample() para generar 300+ versiones únicas\""
echo "/memory add \"Distractores efectivos: opciones plausibles pero incorrectas, evitar triviales\""
echo "/memory add \"Contexto colombiano: usar nombres, lugares y situaciones familiares\""
echo ""

echo "# Workflow completo de desarrollo con extensión VSCode"
echo "# 1. /id install"
echo "# 2. /init"
echo "# 3. /memory add \"contexto específico del ejercicio\""
echo "# 4. Desarrollar ejercicio con asistencia directa en VSCode"
echo "# 5. /chat save \"nombre_ejercicio\""
echo "# 6. /memory add \"patrón exitoso identificado\""
echo ""

# =============================================================================
# INFORMACIÓN FINAL
# =============================================================================

echo ""
echo "📖 RECURSOS ADICIONALES:"
echo "========================"
echo ""
echo "• Manual completo: Auxiliares/Instalaciones/Ais/Gemini_CLI/manual-usuario-gemini-cli-r-exams-icfes.md"
echo "• Tutorial detallado: Auxiliares/Instalaciones/Ais/Gemini_CLI/gemini-cli-r-exams.md"
echo "• Contexto del proyecto: GEMINI.md"
echo "• Reglas específicas: .gemini/rules-gemini.md"
echo "• Ejemplos funcionales: Auxiliares/Ejemplos-Funcionales-Rmd/"
echo ""
echo "🎯 Para usar estos comandos:"
echo "1. Comandos básicos: Copia y ejecuta desde terminal"
echo "2. Comandos VSCode Companion: Ejecuta en terminal integrado de VSCode Insiders"
echo "3. Modifica las rutas según tu caso específico"
echo "4. Ejecuta desde el directorio raíz del proyecto"
echo ""
echo "🚀 MODOS DISPONIBLES:"
echo "• Modo básico: gemini-icfes --basic"
echo "• Modo con MCPs: gemini-icfes --mcps"
echo "• Modo experto: VSCode Insiders + Extensión Companion"
echo ""
echo "¡Listo para crear ejercicios R-exams ICFES de alta calidad con Gemini CLI!"
