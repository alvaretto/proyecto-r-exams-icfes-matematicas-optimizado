#!/usr/bin/env python3
"""
Script de prueba para el corrector automático
"""

import sys
import os
import json
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from visual_analyzer import VisualAnalyzer
from auto_corrector import AutoCorrector

def test_auto_corrector():
    """Probar el corrector automático con archivo problemático"""
    
    test_file = "/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/Auxiliares/Ejemplos-Funcionales-Rmd/Plantillas/tex/Graficos_Estadisticos_Adopcion_Mascotas/I_1796473-Opc-A2v2_optimizado.Rmd"
    
    if not os.path.exists(test_file):
        print(f"❌ Archivo de prueba no encontrado: {test_file}")
        return False
    
    print("🚀 INICIANDO PROCESO COMPLETO: ANÁLISIS + CORRECCIÓN")
    print(f"📁 Archivo: {os.path.basename(test_file)}")
    print("=" * 80)
    
    try:
        # PASO 1: Análisis de errores
        print("\n🔍 PASO 1: ANÁLISIS DE ERRORES")
        analyzer = VisualAnalyzer()
        analysis_results = analyzer.analyze_rmd_file(test_file)
        
        print(f"   ❌ Errores matemáticos detectados: {analysis_results['summary']['total_math_errors']}")
        
        # Mostrar errores principales
        critical_errors = [e for e in analysis_results['mathematical_inconsistencies'] 
                          if 'CRÍTICO' in e['message'] or 'ERROR LÓGICO' in e['message']]
        
        print(f"   🎯 Errores críticos: {len(critical_errors)}")
        for error in critical_errors[:3]:  # Mostrar solo los primeros 3
            print(f"      • {error['message'][:80]}...")
        
        # PASO 2: Corrección automática
        print("\n🛠️ PASO 2: CORRECCIÓN AUTOMÁTICA")
        corrector = AutoCorrector()
        correction_results = corrector.correct_rmd_file(test_file, analysis_results)
        
        print(f"   📁 Backup creado: {os.path.basename(correction_results['backup_file'])}")
        print(f"   📝 Archivo corregido: {os.path.basename(correction_results['corrected_file'])}")
        print(f"   ✅ Correcciones aplicadas: {correction_results['summary']['total_corrections_applied']}")
        print(f"   ❌ Correcciones fallidas: {correction_results['summary']['total_corrections_failed']}")
        print(f"   📊 Tasa de éxito: {correction_results['summary']['success_rate']:.1f}%")
        
        # Mostrar correcciones aplicadas
        if correction_results['corrections_applied']:
            print("\n🔧 CORRECCIONES APLICADAS:")
            for correction in correction_results['corrections_applied'][:5]:  # Mostrar solo las primeras 5
                print(f"      • {correction['message']}")
        
        # PASO 3: Re-análisis del archivo corregido
        print("\n🔍 PASO 3: VERIFICACIÓN POST-CORRECCIÓN")
        if correction_results['corrected_file']:
            reanalysis_results = analyzer.analyze_rmd_file(correction_results['corrected_file'])
            
            print(f"   ❌ Errores restantes: {reanalysis_results['summary']['total_math_errors']}")
            
            # Comparar antes vs después
            errors_before = analysis_results['summary']['total_math_errors']
            errors_after = reanalysis_results['summary']['total_math_errors']
            improvement = errors_before - errors_after
            
            print(f"   📈 Mejora: {improvement} errores corregidos ({improvement/errors_before*100:.1f}%)")
            
            if errors_after == 0:
                print("   🎉 ¡ARCHIVO COMPLETAMENTE CORREGIDO!")
            elif improvement > 0:
                print("   ✅ Mejora significativa detectada")
            else:
                print("   ⚠️ No se detectó mejora")
        
        # PASO 4: Resumen final
        print("\n📋 RESUMEN FINAL:")
        print(f"   📁 Archivo original: {os.path.basename(test_file)}")
        print(f"   📁 Archivo corregido: {os.path.basename(correction_results['corrected_file'])}")
        print(f"   📁 Backup: {os.path.basename(correction_results['backup_file'])}")
        print(f"   🎯 Estado: {correction_results['summary']['status'].upper()}")
        
        # Guardar resultados completos
        results_file = "/tmp/correction_results.json"
        with open(results_file, 'w', encoding='utf-8') as f:
            json.dump({
                'analysis_before': analysis_results,
                'corrections': correction_results,
                'analysis_after': reanalysis_results if 'reanalysis_results' in locals() else None
            }, f, indent=2, ensure_ascii=False)
        
        print(f"   💾 Resultados completos: {results_file}")
        
        return True
        
    except Exception as e:
        print(f"❌ Error en la prueba: {e}")
        import traceback
        traceback.print_exc()
        return False

if __name__ == "__main__":
    success = test_auto_corrector()
    sys.exit(0 if success else 1)
