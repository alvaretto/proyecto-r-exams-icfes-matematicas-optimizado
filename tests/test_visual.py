#!/usr/bin/env python3
"""
Script de prueba para el analizador visual
"""

import sys
import os
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from visual_analyzer import VisualAnalyzer
import json

def test_visual_analyzer():
    """Probar el analizador visual con archivo de ejemplo"""
    
    # Archivo de prueba
    test_file = "/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/Auxiliares/Ejemplos-Funcionales-Rmd/Plantillas/tex/Graficos_Estadisticos_Adopcion_Mascotas/I_1796473-Opc-A2v2_optimizado.Rmd"
    
    if not os.path.exists(test_file):
        print(f"❌ Archivo de prueba no encontrado: {test_file}")
        return False
    
    print("🚀 Iniciando prueba del analizador visual...")
    print(f"📁 Archivo: {os.path.basename(test_file)}")
    
    try:
        # Crear analizador
        analyzer = VisualAnalyzer()
        
        # Ejecutar análisis
        results = analyzer.analyze_rmd_file(test_file)
        
        # Mostrar resultados
        print("\n📊 RESULTADOS:")
        print(f"   Gráficas generadas: {results['summary']['total_graphics']}")
        print(f"   Errores visuales: {results['summary']['total_errors']}")
        print(f"   Errores matemáticos: {results['summary']['total_math_errors']}")
        print(f"   Estado: {results['summary']['status']}")
        
        # Mostrar errores si los hay
        if results['mathematical_inconsistencies']:
            print("\n❌ ERRORES MATEMÁTICOS DETECTADOS:")
            for error in results['mathematical_inconsistencies']:
                print(f"   • {error['message']}")
                if 'values' in error:
                    print(f"     Valores: {error['values']}")
        
        if results['visual_errors']:
            print("\n⚠️ ERRORES VISUALES:")
            for error in results['visual_errors']:
                print(f"   • {error['message']}")
        
        # Guardar resultados completos
        output_file = "/tmp/test_visual_results.json"
        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(results, f, indent=2, ensure_ascii=False)
        
        print(f"\n💾 Resultados completos guardados en: {output_file}")
        
        return True
        
    except Exception as e:
        print(f"❌ Error en la prueba: {e}")
        import traceback
        traceback.print_exc()
        return False

if __name__ == "__main__":
    success = test_visual_analyzer()
    sys.exit(0 if success else 1)
