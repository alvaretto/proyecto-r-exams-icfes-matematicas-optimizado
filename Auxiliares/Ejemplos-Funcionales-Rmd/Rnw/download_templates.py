#!/usr/bin/env python3
"""
Script para descargar todos los templates .Rnw de r-exams.org
y organizarlos por tipo de ejercicio (schoice, mchoice, cloze, num, string, essay)
"""

import requests
import os
import re
from urllib.parse import urljoin
import time

# Mapeo de templates con sus tipos según la documentación de r-exams.org
TEMPLATES_INFO = {
    # SCHOICE (Single Choice)
    'flags': 'schoice',
    'fruit2': 'schoice', 
    'swisscapital': 'schoice',
    'tstat2': 'schoice',
    'dist3': 'schoice',
    'deriv2': 'schoice',
    'hessian': 'schoice',
    
    # MCHOICE (Multiple Choice)
    'capitals': 'mchoice',
    'switzerland': 'mchoice',
    'gaussmarkov': 'mchoice',
    'anova': 'mchoice',
    'ttest': 'mchoice',
    'scatterplot': 'mchoice',
    'relfreq': 'mchoice',
    'boxplots': 'mchoice',
    'Rlogo': 'mchoice',
    'logic': 'mchoice',
    'automaton': 'mchoice',
    
    # CLOZE (Multiple parts)
    'vowels': 'cloze',
    'lm3': 'cloze',
    'lm2': 'cloze', 
    'essayreg2': 'cloze',
    'dist2': 'cloze',
    'fourfold2': 'cloze',
    'fourfold': 'cloze',
    'boxhist2': 'cloze',
    'boxhist': 'cloze',
    
    # NUM (Numeric)
    'sumdiff': 'num',
    'fruit': 'num',
    'tstat': 'num',
    'dist': 'num',
    'deriv': 'num',
    'currency8': 'num',
    'confint3': 'num',
    'confint2': 'num',
    'cholesky': 'num',
    'regression': 'num',
    'lm': 'num',
    'lagrange': 'num',
    
    # STRING
    'function': 'string',
    'countrycodes': 'string',
    
    # ESSAY
    'essayreg': 'essay'
}

BASE_URL = "https://www.r-exams.org"

def download_file(url, filepath):
    """Descarga un archivo desde una URL"""
    try:
        print(f"Descargando: {url}")
        response = requests.get(url, timeout=30)
        response.raise_for_status()
        
        # Crear directorio si no existe
        os.makedirs(os.path.dirname(filepath), exist_ok=True)
        
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(response.text)
        
        print(f"✅ Guardado: {filepath}")
        return True
        
    except Exception as e:
        print(f"❌ Error descargando {url}: {e}")
        return False

def main():
    """Función principal"""
    print("🎯 DESCARGANDO TEMPLATES .RNW DE R-EXAMS.ORG")
    print("=" * 50)
    
    base_dir = "Auxiliares/Ejemplos-Funcionales-Rmd/Rnw"
    downloaded = 0
    failed = 0
    
    for template_name, exercise_type in TEMPLATES_INFO.items():
        # Construir URL del archivo .Rnw
        rnw_url = f"{BASE_URL}/assets/posts/2017-08-14-{template_name}/{template_name}.Rnw"
        
        # Ruta de destino
        dest_path = os.path.join(base_dir, exercise_type, f"{template_name}.Rnw")
        
        # Descargar archivo
        if download_file(rnw_url, dest_path):
            downloaded += 1
        else:
            failed += 1
        
        # Pausa para no sobrecargar el servidor
        time.sleep(1)
    
    print("\n📊 RESUMEN:")
    print(f"✅ Descargados: {downloaded}")
    print(f"❌ Fallidos: {failed}")
    print(f"📁 Organizados en: {base_dir}/")
    
    # Mostrar estructura de directorios
    print("\n📂 ESTRUCTURA CREADA:")
    for exercise_type in set(TEMPLATES_INFO.values()):
        type_dir = os.path.join(base_dir, exercise_type)
        if os.path.exists(type_dir):
            files = [f for f in os.listdir(type_dir) if f.endswith('.Rnw')]
            print(f"  📁 {exercise_type}/: {len(files)} archivos")

if __name__ == "__main__":
    main()
