#!/usr/bin/env python3
"""
Script de prueba para corrección automática de decimales
"""

import sys
import os
import json
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from decimal_corrector import DecimalCorrector

def test_decimal_correction():
    """Probar corrección automática de errores de decimales"""
    
    test_file = "/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/Auxiliares/Ejemplos-Funcionales-Rmd/Plantillas/tex/Graficos_Estadisticos_Adopcion_Mascotas/I_1796473-Opc-A2v2_optimizado.Rmd"
    
    if not os.path.exists(test_file):
        print(f"❌ Archivo de prueba no encontrado: {test_file}")
        return False
    
    print("🛠️ CORRECCIÓN AUTOMÁTICA DE ERRORES DE DECIMALES")
    print(f"📁 Archivo: {os.path.basename(test_file)}")
    print("=" * 80)
    
    try:
        # PASO 1: Aplicar corrección automática
        print("\n🔧 PASO 1: APLICANDO CORRECCIONES AUTOMÁTICAS")
        corrector = DecimalCorrector()
        results = corrector.correct_decimal_errors(test_file)
        
        print(f"   📁 Backup creado: {os.path.basename(results['backup_file'])}")
        print(f"   📝 Archivo corregido: {os.path.basename(results['corrected_file'])}")
        print(f"   ✅ Correcciones aplicadas: {results['summary']['total_corrections_applied']}")
        print(f"   ❌ Correcciones fallidas: {results['summary']['total_corrections_failed']}")
        print(f"   📊 Tasa de éxito: {results['summary']['success_rate']:.1f}%")
        
        # PASO 2: Mostrar correcciones aplicadas
        print("\n🔧 PASO 2: CORRECCIONES APLICADAS")
        if results['corrections_applied']:
            for i, correction in enumerate(results['corrections_applied'], 1):
                print(f"\n   {i}. {correction['type'].upper()}")
                print(f"      📝 Mensaje: {correction['message']}")
                if 'corrections_count' in correction:
                    print(f"      🔢 Cambios: {correction['corrections_count']}")
        else:
            print("   ℹ️ No se aplicaron correcciones automáticas")
        
        # PASO 3: Mostrar correcciones fallidas
        if results['corrections_failed']:
            print("\n❌ PASO 3: CORRECCIONES FALLIDAS")
            for i, failure in enumerate(results['corrections_failed'], 1):
                print(f"\n   {i}. {failure['type'].upper()}")
                print(f"      📝 Mensaje: {failure['message']}")
        
        # PASO 4: Generar HTML del archivo corregido para verificar
        print("\n🌐 PASO 4: GENERANDO HTML PARA VERIFICACIÓN")
        if results['corrected_file']:
            html_result = generate_corrected_html(results['corrected_file'])
            if html_result['success']:
                print(f"   ✅ HTML generado: {html_result['html_file']}")
                print(f"   🔗 URL: {html_result['url']}")
            else:
                print(f"   ❌ Error generando HTML: {html_result['error']}")
        
        # PASO 5: Instrucciones de verificación
        print("\n🔍 PASO 5: VERIFICACIÓN MANUAL REQUERIDA")
        print("   📊 Abrir el HTML generado y verificar:")
        print("   1. ¿La torta ahora muestra decimales cuando corresponde?")
        print("   2. ¿Los valores como 25.3% aparecen correctamente?")
        print("   3. ¿Los valores como 25.0% aparecen como 25% (sin decimal)?")
        print("   4. ¿Hay consistencia entre tabla y torta?")
        
        # PASO 6: Resumen final
        print("\n📋 RESUMEN FINAL:")
        print(f"   📁 Archivo original: {os.path.basename(test_file)}")
        print(f"   📁 Archivo corregido: {os.path.basename(results['corrected_file'])}")
        print(f"   📁 Backup: {os.path.basename(results['backup_file'])}")
        print(f"   🎯 Estado: {results['summary']['status'].upper()}")
        
        # Guardar resultados completos
        results_file = "/tmp/decimal_correction_results.json"
        with open(results_file, 'w', encoding='utf-8') as f:
            json.dump(results, f, indent=2, ensure_ascii=False)
        
        print(f"   💾 Resultados completos: {results_file}")
        
        return True
        
    except Exception as e:
        print(f"❌ Error en la corrección: {e}")
        import traceback
        traceback.print_exc()
        return False

def generate_corrected_html(corrected_rmd_path):
    """Generar HTML del archivo corregido para verificación"""
    
    try:
        import subprocess
        
        # Crear script R para generar HTML
        r_script = f'''
suppressPackageStartupMessages({{
    library(exams)
    library(knitr)
    library(kableExtra)
}})

corrected_file <- "{corrected_rmd_path}"
output_dir <- dirname(corrected_file)

setwd(output_dir)

tryCatch({{
    html_files <- exams2html(
        file = corrected_file,
        n = 1,
        name = "corrected_decimals",
        dir = ".",
        template = "plain.html",
        encoding = "UTF-8"
    )
    
    cat("SUCCESS: HTML generated\\n")
    
}}, error = function(e) {{
    cat("ERROR:", e$message, "\\n")
}})
'''
        
        # Ejecutar script R
        result = subprocess.run(['R', '--slave', '-e', r_script], 
                              capture_output=True, text=True, timeout=60)
        
        if "SUCCESS" in result.stdout:
            # Buscar archivo HTML generado
            output_dir = os.path.dirname(corrected_rmd_path)
            html_files = [f for f in os.listdir(output_dir) if f.startswith('corrected_decimals') and f.endswith('.html')]
            
            if html_files:
                html_file = os.path.join(output_dir, html_files[0])
                return {
                    'success': True,
                    'html_file': html_file,
                    'url': f"file://{html_file}"
                }
        
        return {
            'success': False,
            'error': f"Error en R: {result.stderr}"
        }
        
    except Exception as e:
        return {
            'success': False,
            'error': str(e)
        }

if __name__ == "__main__":
    success = test_decimal_correction()
    sys.exit(0 if success else 1)
