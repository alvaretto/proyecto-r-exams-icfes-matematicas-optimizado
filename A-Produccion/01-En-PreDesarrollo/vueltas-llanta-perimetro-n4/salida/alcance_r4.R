## ALCANCE a firmar (§P7-E) de la ronda 4, regla por regla, con la tasa EXACTA de la enumeración del diseño
## (salida/verificar_vueltas.R en modo «proponer», 2026-11-10; salida en salida/r4_proponer.txt), redondeada hacia arriba a
## 6 decimales. Escritura de la ronda 2 restaurada (decisión del profesor del 2026-11-10: «Volver a la de la ronda 2»).
## Pendiente de la firma del profesor en la pausa 3. Una tasa exacta por encima de la cableada sale del alcance.
## Familias: B = batería §P7 congelada (más las 2 reglas del estímulo y la anchura y la estrechez en el render de TeX);
## V1 = vector del detractor r1; V2 = «contiene ÷» y marcas únicas; V3 = reglas de conjunto del detractor r3 (H2).
## La canónica reproduce el impreso (regla #24 H-2): sus reglas se firman como heredadas del cuadernillo.
a <- function(muestra, familia, regla, tasa) list(muestra = muestra, familia = familia, regla = regla, tasa = tasa)
ALCANCE <- list(
  a("agregado",       "V3", "ancho TeX (tol 1 pt)",                                 0.600001),
  a("agregado",       "B",  "la mas ancha (TeX)",                                   0.600001),
  a("agregado",       "V3", "letra abajo (conjunto)",                               0.564584),
  a("agregado",       "V3", "contiene x (conjunto)",                                0.422917),
  a("canonica",       "V2", "marca: unica sin termina_fraccion",                    1.000000),
  a("canonica",       "B",  "estimulo: contiene la relacion K/V y otra operacion",  1.000000),
  a("canonica",       "V1", "tres_datos",                                           0.500000),
  a("canonica",       "V3", "ancho TeX (tol 1 pt)",                                 0.500000),
  a("canonica",       "V3", "letra abajo (conjunto)",                               0.500000),
  a("canonica",       "V3", "contiene x (conjunto)",                                0.500000),
  a("canonica",       "V3", "un operador",                                          0.500000),
  a("canonica",       "V3", "contiene K/V del enunciado",                           0.500000),
  a("canonica",       "B",  "la mas ancha (TeX)",                                   0.500000),
  a("canonica",       "B",  "la que MAS comparte",                                  0.500000),
  a("canonica",       "B",  "ultimo numero mayor",                                  0.500000),
  a("fraccion_izq",   "V3", "contiene x (conjunto)",                                0.666667),
  a("fraccion_izq",   "V2", "marca: unica con producto",                            0.625000),
  a("fraccion_izq",   "V1", "centralidad",                                          0.500000),
  a("fraccion_izq",   "V1", "medoide",                                              0.500000),
  a("fraccion_izq",   "V3", "un operador",                                          0.416667),
  a("fraccion_izq",   "V1", "tres_datos",                                           0.375000),
  a("fraccion_izq",   "B",  "estimulo: contiene la relacion K/V y otra operacion",  0.416667),
  a("fraccion_unica", "V3", "ancho TeX (tol 1 pt)",                                 0.750001),
  a("fraccion_unica", "B",  "la mas ancha (TeX)",                                   0.750000),
  a("fraccion_unica", "V3", "letra abajo (conjunto)",                               0.672620),
  a("numero_izq",     "V3", "empieza por la distancia",                             0.750000),
  a("numero_izq",     "V3", "contiene x (conjunto)",                                0.666667),
  a("numero_izq",     "V3", "multiplica por una fraccion",                          0.666667),
  a("numero_izq",     "V2", "marca: unica con producto",                            0.625000),
  a("numero_izq",     "V2", "marca: unica sin empieza_fraccion",                    0.625000),
  a("numero_izq",     "V2", "marca: unica con empieza_numero",                      0.625000),
  a("numero_izq",     "V1", "centralidad",                                          0.500000),
  a("numero_izq",     "V1", "medoide",                                              0.500000),
  a("numero_izq",     "V3", "un operador",                                          0.416667),
  a("numero_izq",     "V1", "tres_datos",                                           0.375000),
  a("numero_izq",     "B",  "estimulo: contiene la relacion K/V y otra operacion",  0.416667))
