#!/usr/bin/env python3
"""
Script para intentar descargar los templates .Rnw que fallaron
usando URLs alternativas y fechas diferentes
"""

import requests
import os
import time

# Templates que fallaron con URLs alternativas
MISSING_TEMPLATES = {
    'flags': ('schoice', ['2017-08-14', '2018-01-08', '2019-01-01']),
    'fruit2': ('schoice', ['2017-08-14', '2018-01-08', '2019-01-01']),
    'capitals': ('mchoice', ['2017-08-14', '2018-01-08', '2019-01-01']),
    'logic': ('mchoice', ['2017-08-14', '2018-01-08', '2019-01-01']),
    'automaton': ('mchoice', ['2017-08-14', '2018-01-08', '2019-01-01']),
    'vowels': ('cloze', ['2017-08-14', '2018-01-08', '2019-01-01']),
    'lm3': ('cloze', ['2017-08-14', '2018-01-08', '2019-01-01']),
    'lm2': ('cloze', ['2017-08-14', '2018-01-08', '2019-01-01']),
    'essayreg2': ('cloze', ['2017-08-14', '2018-01-08', '2019-01-01']),
    'sumdiff': ('num', ['2017-08-14', '2018-01-08', '2019-01-01']),
    'fruit': ('num', ['2017-08-14', '2018-01-08', '2019-01-01']),
    'confint3': ('num', ['2017-08-14', '2018-01-08', '2019-01-01'])
}

BASE_URL = "https://www.r-exams.org"

def try_download_template(template_name, exercise_type, dates):
    """Intenta descargar un template usando diferentes fechas"""
    
    for date in dates:
        url = f"{BASE_URL}/assets/posts/{date}-{template_name}/{template_name}.Rnw"
        dest_path = f"{exercise_type}/{template_name}.Rnw"
        
        try:
            print(f"Intentando: {url}")
            response = requests.get(url, timeout=30)
            
            if response.status_code == 200:
                os.makedirs(exercise_type, exist_ok=True)
                with open(dest_path, 'w', encoding='utf-8') as f:
                    f.write(response.text)
                print(f"✅ Descargado: {dest_path}")
                return True
                
        except Exception as e:
            print(f"❌ Error: {e}")
            continue
    
    print(f"❌ No se pudo descargar {template_name}")
    return False

def main():
    print("🔄 INTENTANDO DESCARGAR TEMPLATES FALTANTES")
    print("=" * 50)
    
    downloaded = 0
    
    for template_name, (exercise_type, dates) in MISSING_TEMPLATES.items():
        if try_download_template(template_name, exercise_type, dates):
            downloaded += 1
        time.sleep(1)
    
    print(f"\n📊 DESCARGADOS ADICIONALES: {downloaded}")

if __name__ == "__main__":
    main()
