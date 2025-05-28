# Script de verificación automática
load_common_packages()

# Probar Python-R
library(reticulate)
use_python("/usr/bin/python3")

py_run_string("
import matplotlib.pyplot as plt
import numpy as np
x = np.linspace(0, 10, 100)
y = np.sin(x)
plt.figure(figsize=(8, 6))
plt.plot(x, y, 'b-', linewidth=2)
plt.title('Verificación: Gráfico Python')
plt.savefig('verificacion_python.png', dpi=150, bbox_inches='tight')
plt.close()
print('✓ Gráfico Python generado')
")

# Probar R nativo
x <- 1:10
y <- x^2
png("verificacion_r.png", width=800, height=600)
plot(x, y, type="b", main="Verificación: Gráfico R", col="blue", pch=16)
dev.off()

cat("✓ Gráfico R generado\n")
cat("✓ Verificación completada exitosamente\n")
cat("Versión R:", R.version.string, "\n")
