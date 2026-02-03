#!/bin/bash
# =============================================================================
# Script de Reorganización del Repositorio ICFES
# Generado por Plutarco - 2026-02-03
# =============================================================================

set -e  # Detener si hay error

echo "🧹 REORGANIZACIÓN DEL REPOSITORIO ICFES"
echo "========================================"
echo ""

# Verificar que estamos en el directorio correcto
if [ ! -f "README.md" ] || [ ! -d "A-Produccion" ]; then
    echo "❌ Error: Ejecuta este script desde la raíz del repositorio"
    echo "   cd /ruta/a/proyecto-r-exams-icfes-matematicas-optimizado"
    exit 1
fi

echo "📁 Creando directorios..."
mkdir -p docs/guias
mkdir -p Auxiliares/scripts
mkdir -p Auxiliares/scripts-r

# =============================================================================
# FASE 1: ELIMINAR ARCHIVOS BASURA
# =============================================================================
echo ""
echo "🗑️  FASE 1: Eliminando archivos innecesarios..."

# Binario grande
[ -f "Ordenar/acli" ] && rm -v "Ordenar/acli"

# HTMLs generados en Ordenar/
rm -fv Ordenar/*.html 2>/dev/null || true

# Archivos de prueba
rm -fv Ordenar/test_*.png Ordenar/ejemplo_vscode_demo.png 2>/dev/null || true
rm -fv Ordenar/grafica_exportaciones*.png Ordenar/trapecios_replica_original.png 2>/dev/null || true
rm -fv Ordenar/figura_geometrica.svg Ordenar/test_final1.pdf 2>/dev/null || true

# Configs de otras herramientas
rm -fv Ordenar/codebuff.json Ordenar/playwright.config.js Ordenar/supermemory-mcp-launcher.js 2>/dev/null || true

# HTMLs en raíz
rm -fv 01-TUTORIAL_EXPERTO_R_EXAMS_ICFES.html avances-2025.html 2>/dev/null || true
rm -fv avances-2025.docx .claude.zip .claudedoc.zip 2>/dev/null || true

# Backup de .claude
rm -rf .claude-backup-20251229-103305 2>/dev/null || true

# HTMLs en AnythingLLM-Config y docus
echo "   Eliminando HTMLs generados en AnythingLLM-Config/..."
find AnythingLLM-Config -name "*.html" -type f -delete 2>/dev/null || true

echo "   Eliminando HTMLs generados en docus/..."
find docus -name "*.html" -type f -delete 2>/dev/null || true

# =============================================================================
# FASE 2: MOVER DOCUMENTACIÓN
# =============================================================================
echo ""
echo "📄 FASE 2: Moviendo documentación a docs/guias/..."

[ -f "Ordenar/GUIA_AUTENTICACION_GITHUB.md" ] && mv -v "Ordenar/GUIA_AUTENTICACION_GITHUB.md" docs/guias/
[ -f "Ordenar/GUIA_GIT_PASO_A_PASO.md" ] && mv -v "Ordenar/GUIA_GIT_PASO_A_PASO.md" docs/guias/
[ -f "Ordenar/INSTRUCCIONES_GIT.md" ] && mv -v "Ordenar/INSTRUCCIONES_GIT.md" docs/guias/
[ -f "Ordenar/INSTALACION_EXITOSA_RESUMEN_FINAL.md" ] && mv -v "Ordenar/INSTALACION_EXITOSA_RESUMEN_FINAL.md" docs/guias/
[ -f "Ordenar/INSTALACION_MANJARO_RESUMEN.md" ] && mv -v "Ordenar/INSTALACION_MANJARO_RESUMEN.md" docs/guias/
[ -f "Ordenar/RESUMEN_INSTALACION_COMPLETA.md" ] && mv -v "Ordenar/RESUMEN_INSTALACION_COMPLETA.md" docs/guias/
[ -f "Ordenar/CONFIGURACION_PYTHON_APLICADA.md" ] && mv -v "Ordenar/CONFIGURACION_PYTHON_APLICADA.md" docs/guias/
[ -f "Ordenar/solucion_languageserver_vscode.md" ] && mv -v "Ordenar/solucion_languageserver_vscode.md" docs/guias/
[ -f "Ordenar/walkthrough.md" ] && mv -v "Ordenar/walkthrough.md" docs/guias/
[ -f "Ordenar/gemini-all-cli.md" ] && mv -v "Ordenar/gemini-all-cli.md" docs/guias/
[ -f "Ordenar/FUNCIONALIDAD_IMAGENES.md" ] && mv -v "Ordenar/FUNCIONALIDAD_IMAGENES.md" docs/guias/
[ -f "Ordenar/README_REVISOR.md" ] && mv -v "Ordenar/README_REVISOR.md" docs/guias/
[ -f "Ordenar/README_Templates_Rnw.md" ] && mv -v "Ordenar/README_Templates_Rnw.md" docs/guias/

# =============================================================================
# FASE 3: MOVER SOLUCIONES
# =============================================================================
echo ""
echo "📋 FASE 3: Moviendo soluciones a SOLUCIONES-DOCUMENTADAS/..."

[ -f "Ordenar/SOLUCION_DEFINITIVA_FINAL.md" ] && mv -v "Ordenar/SOLUCION_DEFINITIVA_FINAL.md" SOLUCIONES-DOCUMENTADAS/
[ -f "Ordenar/SOLUCION_FINAL_RETICULATE.md" ] && mv -v "Ordenar/SOLUCION_FINAL_RETICULATE.md" SOLUCIONES-DOCUMENTADAS/
[ -f "Ordenar/RESUMEN_CAMBIOS_ALEATORIZACION.md" ] && mv -v "Ordenar/RESUMEN_CAMBIOS_ALEATORIZACION.md" SOLUCIONES-DOCUMENTADAS/
[ -f "Ordenar/RESUMEN_CAMBIOS_HTML_CONSOLIDADO.md" ] && mv -v "Ordenar/RESUMEN_CAMBIOS_HTML_CONSOLIDADO.md" SOLUCIONES-DOCUMENTADAS/
[ -f "Ordenar/RESUMEN_OPTIMIZACION_GIT.md" ] && mv -v "Ordenar/RESUMEN_OPTIMIZACION_GIT.md" SOLUCIONES-DOCUMENTADAS/
[ -f "Ordenar/REPORTE_FINAL_PROCESO_METICULOSO.md" ] && mv -v "Ordenar/REPORTE_FINAL_PROCESO_METICULOSO.md" SOLUCIONES-DOCUMENTADAS/
[ -f "Ordenar/CAMBIOS_APLICADOS_ARCHIVO.md" ] && mv -v "Ordenar/CAMBIOS_APLICADOS_ARCHIVO.md" SOLUCIONES-DOCUMENTADAS/
[ -f "Ordenar/cronologia.md" ] && mv -v "Ordenar/cronologia.md" SOLUCIONES-DOCUMENTADAS/

# =============================================================================
# FASE 4: MOVER SCRIPTS
# =============================================================================
echo ""
echo "🔧 FASE 4: Moviendo scripts..."

# Scripts bash
[ -f "Ordenar/actualizar_repositorio.sh" ] && mv -v "Ordenar/actualizar_repositorio.sh" Auxiliares/scripts/
[ -f "Ordenar/subir_cambios.sh" ] && mv -v "Ordenar/subir_cambios.sh" Auxiliares/scripts/
[ -f "Ordenar/git_commit_smart.sh" ] && mv -v "Ordenar/git_commit_smart.sh" Auxiliares/scripts/
[ -f "Ordenar/comandos_git_configuracion.sh" ] && mv -v "Ordenar/comandos_git_configuracion.sh" Auxiliares/scripts/
[ -f "Ordenar/diagnostico_r_vscode.sh" ] && mv -v "Ordenar/diagnostico_r_vscode.sh" Auxiliares/scripts/
[ -f "Ordenar/install_rstudio_icfes.sh" ] && mv -v "Ordenar/install_rstudio_icfes.sh" Auxiliares/scripts/
[ -f "Ordenar/install_rstudio_ubuntu.sh" ] && mv -v "Ordenar/install_rstudio_ubuntu.sh" Auxiliares/scripts/
[ -f "Ordenar/limpiar_augment_cursor.sh" ] && mv -v "Ordenar/limpiar_augment_cursor.sh" Auxiliares/scripts/
[ -f "Ordenar/visual_analyzer.py" ] && mv -v "Ordenar/visual_analyzer.py" Auxiliares/scripts/

# Scripts R
[ -f "Ordenar/configuracion_python_reticulate.R" ] && mv -v "Ordenar/configuracion_python_reticulate.R" Auxiliares/scripts-r/
[ -f "Ordenar/solucion_definitiva_matplotlib.R" ] && mv -v "Ordenar/solucion_definitiva_matplotlib.R" Auxiliares/scripts-r/
[ -f "Ordenar/solucion_matplotlib_reticulate.R" ] && mv -v "Ordenar/solucion_matplotlib_reticulate.R" Auxiliares/scripts-r/
[ -f "Ordenar/verificar_configuracion_python.R" ] && mv -v "Ordenar/verificar_configuracion_python.R" Auxiliares/scripts-r/
[ -f "Ordenar/verificar_utf8.R" ] && mv -v "Ordenar/verificar_utf8.R" Auxiliares/scripts-r/
[ -f "Ordenar/validacion_final.R" ] && mv -v "Ordenar/validacion_final.R" Auxiliares/scripts-r/
[ -f "Ordenar/codigo_corregido_proyecto.R" ] && mv -v "Ordenar/codigo_corregido_proyecto.R" Auxiliares/scripts-r/

# =============================================================================
# FASE 5: MOVER ARCHIVOS ESPECIALES
# =============================================================================
echo ""
echo "⭐ FASE 5: Moviendo archivos especiales..."

[ -f "Ordenar/control-calidad-global.R" ] && mv -v "Ordenar/control-calidad-global.R" core/
[ -f "Ordenar/CHANGELOG.md" ] && mv -v "Ordenar/CHANGELOG.md" Graficador-Experto/
[ -f "Ordenar/GEMINI.md" ] && mv -v "Ordenar/GEMINI.md" Auxiliares/Agentes-IA/
[ -f "Ordenar/copilot-instructions.md" ] && mv -v "Ordenar/copilot-instructions.md" Auxiliares/Agentes-IA/
[ -f "Ordenar/knowledge.md" ] && mv -v "Ordenar/knowledge.md" Auxiliares/Agentes-IA/

# Tests
[ -f "Ordenar/test_aleatorization.R" ] && mv -v "Ordenar/test_aleatorization.R" tests/
[ -f "Ordenar/test_graficos_visualizacion.R" ] && mv -v "Ordenar/test_graficos_visualizacion.R" tests/
[ -f "Ordenar/test_html_consolidado.R" ] && mv -v "Ordenar/test_html_consolidado.R" tests/
[ -f "Ordenar/test_reticulate_in_rmd.Rmd" ] && mv -v "Ordenar/test_reticulate_in_rmd.Rmd" tests/
[ -f "Ordenar/test_simple.Rmd" ] && mv -v "Ordenar/test_simple.Rmd" tests/
[ -f "Ordenar/test_visual.py" ] && mv -v "Ordenar/test_visual.py" tests/

# Ejemplo y .Rproj
[ -f "Ordenar/proporcionalidad_empresarial_formulacion_ejecucion_n2_v1.Rmd" ] && mv -v "Ordenar/proporcionalidad_empresarial_formulacion_ejecucion_n2_v1.Rmd" ejemplos/
[ -f "Ordenar/RepositorioMatematicasICFES_R_Exams.Rproj" ] && mv -v "Ordenar/RepositorioMatematicasICFES_R_Exams.Rproj" ./

# =============================================================================
# FASE 6: ELIMINAR CARPETA ORDENAR SI ESTÁ VACÍA
# =============================================================================
echo ""
echo "🗂️  FASE 6: Verificando carpeta Ordenar/..."

if [ -d "Ordenar" ]; then
    remaining=$(ls -A Ordenar 2>/dev/null | wc -l)
    if [ "$remaining" -eq 0 ]; then
        rmdir Ordenar
        echo "✅ Carpeta Ordenar/ eliminada (estaba vacía)"
    else
        echo "⚠️  Ordenar/ todavía tiene $remaining archivos:"
        ls -la Ordenar/
    fi
fi

# =============================================================================
# FASE 7: ACTUALIZAR .gitignore
# =============================================================================
echo ""
echo "📝 FASE 7: Actualizando .gitignore..."

# Verificar si ya tiene las líneas nuevas
if ! grep -q "ARCHIVOS GENERADOS Y BINARIOS" .gitignore 2>/dev/null; then
cat >> .gitignore << 'GITIGNORE_ADDITIONS'

# =============================================================================
# ARCHIVOS GENERADOS Y BINARIOS (añadido 2026-02-03)
# =============================================================================

# HTML generados desde Markdown (se regeneran fácilmente)
*.html
!docs/**/*.html

# Binarios de herramientas IA/CLI (nunca en Git)
acli
augment-cli

# Backups de configuración Claude
.claude-backup*/
.claude-backup-*/

# Archivos zip de configuración
.claude.zip
.claudedoc.zip

# Archivos docx vacíos/placeholder
avances-*.docx
GITIGNORE_ADDITIONS
echo "✅ .gitignore actualizado"
else
echo "ℹ️  .gitignore ya estaba actualizado"
fi

# =============================================================================
# RESUMEN
# =============================================================================
echo ""
echo "========================================"
echo "✅ REORGANIZACIÓN COMPLETADA"
echo "========================================"
echo ""
echo "Ahora ejecuta:"
echo "  git add -A"
echo "  git commit -m '🧹 Reorganización masiva del repositorio'"
echo "  git push origin main"
echo ""
