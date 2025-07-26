#!/usr/bin/env python3
"""
Script de prueba para el validador de gráficas real
"""

import sys
import os
import json
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from graphics_validator import GraphicsValidator

def test_graphics_validator():
    """Probar el validador de gráficas con ejecución real de código R"""
    
    test_file = "/home/proyectos/Insync/alvaroangelm@iepedacitodecielo.edu.co/Google Drive/RepositorioMatematicasICFES_R_Exams/Auxiliares/Ejemplos-Funcionales-Rmd/Plantillas/tex/Graficos_Estadisticos_Adopcion_Mascotas/I_1796473-Opc-A2v2_optimizado.Rmd"
    
    if not os.path.exists(test_file):
        print(f"❌ Archivo de prueba no encontrado: {test_file}")
        return False
    
    print("🎨 INICIANDO VALIDACIÓN DE GRÁFICAS CON EJECUCIÓN REAL")
    print(f"📁 Archivo: {os.path.basename(test_file)}")
    print("=" * 80)
    
    try:
        # PASO 1: Validar gráficas ejecutando código R real
        print("\n🔍 PASO 1: EJECUTAR CÓDIGO R Y GENERAR GRÁFICAS")
        validator = GraphicsValidator()
        results = validator.validate_graphics_in_rmd(test_file)
        
        print(f"   📊 Gráficas generadas: {results['summary']['total_graphics_generated']}")
        print(f"   ❌ Errores de gráficas: {results['summary']['total_graphics_errors']}")
        print(f"   ⚠️ Errores de consistencia: {results['summary']['total_consistency_errors']}")
        
        # PASO 2: Mostrar datos originales extraídos
        print("\n📋 PASO 2: DATOS ORIGINALES EXTRAÍDOS")
        if 'original_data' in results:
            original_data = results['original_data']
            if 'percentages' in original_data:
                percentages = original_data['percentages']
                total = sum(percentages)
                print(f"   📊 Porcentajes: {percentages}")
                print(f"   🧮 Suma total: {total}%")
                if abs(total - 100) > 1:
                    print(f"   ❌ ERROR: Los porcentajes no suman 100%")
                else:
                    print(f"   ✅ Porcentajes suman correctamente")
        
        # PASO 3: Mostrar gráficas generadas
        print("\n🎨 PASO 3: GRÁFICAS GENERADAS")
        for i, graphic in enumerate(results['graphics_generated']):
            print(f"   {i+1}. {graphic['name']} - Tipo: {results['data_extracted'][graphic['name']].get('graphic_type', 'unknown')}")
            print(f"      📁 Imagen: {os.path.basename(graphic['image_path'])}")
        
        # PASO 4: Mostrar errores específicos
        print("\n❌ PASO 4: ERRORES DETECTADOS")
        
        if results['graphics_errors']:
            print("   🎨 Errores de gráficas:")
            for error in results['graphics_errors']:
                print(f"      • {error['message']}")
        
        if results['consistency_errors']:
            print("   🔗 Errores de consistencia:")
            for error in results['consistency_errors']:
                print(f"      • {error['message']}")
        
        if not results['graphics_errors'] and not results['consistency_errors']:
            print("   ✅ No se detectaron errores automáticamente")
        
        # PASO 5: Verificación manual requerida
        print("\n👁️ PASO 5: VERIFICACIÓN MANUAL REQUERIDA")
        print("   🔍 TABLA: ¿Muestra datos confusos?")
        print("   🔍 TORTA: ¿Muestra datos confusos?")
        print("   🎯 OBJETIVO: Identificar exactamente 1 gráfica incorrecta")
        
        # PASO 6: Guardar resultados
        results_file = "/tmp/graphics_validation_results.json"
        with open(results_file, 'w', encoding='utf-8') as f:
            json.dump(results, f, indent=2, ensure_ascii=False)
        
        print(f"\n💾 Resultados completos guardados en: {results_file}")
        
        # PASO 7: Resumen final
        print("\n📋 RESUMEN FINAL:")
        print(f"   🎨 Gráficas ejecutadas: {results['summary']['total_graphics_generated']}")
        print(f"   ❌ Errores detectados: {results['summary']['total_graphics_errors'] + results['summary']['total_consistency_errors']}")
        print(f"   👁️ Verificación manual: {'REQUERIDA' if results['summary']['requires_manual_verification'] else 'NO NECESARIA'}")
        print(f"   🎯 Estado: {results['summary']['status'].upper()}")
        
        return True
        
    except Exception as e:
        print(f"❌ Error en la validación: {e}")
        import traceback
        traceback.print_exc()
        return False

if __name__ == "__main__":
    success = test_graphics_validator()
    sys.exit(0 if success else 1)
