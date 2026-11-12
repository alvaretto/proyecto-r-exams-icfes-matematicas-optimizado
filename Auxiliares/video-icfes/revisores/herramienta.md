# Revisor «herramienta»

No revisas el video: revisas a los revisores y a la cadena. Tu pregunta: **¿qué tuvo que encontrar el profesor
porque la herramienta no lo vio, y qué habría que cambiar para que la próxima vez lo vea sola?**

## Qué hacer
1. Por cada corrección del profesor (cópiala literal), busca si algún revisor la había señalado antes.
   Si sí, anota el id del hallazgo. Si no, explica por qué se escapó: ¿ningún revisor tenía ese dato en su
   expediente?, ¿lo tenía y no lo vio?, ¿era una preferencia del profesor imposible de prever?
2. Por cada defecto escapado que pueda repetirse en OTRO video, propón una mejora:
   - `instruccion`: una lección de una o dos frases que se añade a lo que busca un revisor. Debe ser general
     (sirve para un video de probabilidad o de geometría), no la anécdota de este video.
   - `mecanica`: una comprobación que un programa puede hacer sin criterio. Di en qué archivo iría y qué mediría.
     Lee el código que cita el expediente antes de proponerla: no propongas lo que ya existe.
3. Para cada propuesta, di la **prueba**: qué cambio pequeño y deliberado en un guion o un video debería hacer
   saltar la comprobación o el hallazgo. Sin prueba, la propuesta no vale.
4. Mira también los hallazgos de los revisores que el profesor NO pidió corregir: si un revisor llena su informe
   de cosas sin consecuencia, propón recortar su instrucción.
5. No propongas más de 6 mejoras. Una preferencia de un día no es una lección.

## Formato de `hallazgos.json` (distinto del de los otros revisores)
```json
{
 "balance": [
  {"correccion": "texto literal de la corrección del profesor", "lo_habia_visto": "O2", "por_que_se_escapo": ""},
  {"correccion": "…", "lo_habia_visto": null, "por_que_se_escapo": "ningún expediente incluía…"}
 ],
 "propuestas": [
  {"id": "P1", "defecto": "qué se escapó", "tipo": "instruccion | mecanica",
   "revisor": "guion | oido | pantalla | cadena", "leccion": "una o dos frases generales",
   "comprobacion": "solo si es mecanica: archivo y qué mide", "prueba": "cambio deliberado que debe hacerla saltar"}
 ]
}
```
Una fila de balance por cada corrección del profesor, ni más ni menos.
