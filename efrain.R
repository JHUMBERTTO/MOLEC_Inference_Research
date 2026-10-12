# Guardar los datos en un datafram y identificar las variables
# Con las que debo trabajar
datos = read.csv("MOLEC_2025_limpia.csv")
vars = c("m_p38d", "m_p38d_1", "m_p39_1", "m_p39_4")


# -----------------
# Convertir el valor 9 a NA
# -----------------

# Contar cuantas veces aparece el valor 9 en cada variable
conteo_9 = sapply(datos[vars], function(x) sum(x == 9, na.rm = TRUE))

# Calcular el porcentaje de veces que aparece el 9 en cada variable
pct_9 = sapply(datos[vars], function(x) round(mean(x == 9, na.rm = TRUE) * 100, 2))

# Imprimir los resultados en un data frame 
reporte_9 = data.frame(Variable = vars, Conteo_9 = conteo_9, Porcentaje = paste0(pct_9, "%"))
print(reporte_9, row.names = FALSE)

# En cada variable, convertir el valor 9 a NA
datos[vars] <- lapply(datos[vars], function(x) ifelse(x == 9, NA, x))


# -----------------
# Descripcion de variables
# -----------------

# Data frame que describe cada variable
descripcion <- data.frame(
  Variable = vars,
  Pregunta = c(
    "¿Padres lo llevaban a bibliotecas/librerías de niño?",
    "Frecuencia con que lo llevaban a bibliotecas/librerías",
    "¿Maestros de primaria pedían leer libros no de texto?",
    "¿Maestros de primaria motivaban a leer libros?"
  ),
  Tipo = c("Dicotómica (1=Sí)", "Ordinal (1=Semanal a 5=Ocasional)", "Dicotómica (1=Sí, 2=No)", "Dicotómica (1=Sí, 2=No)"),
  Pct_NoRecuerda = paste0(pct_9, "%"),
  Validos_N = sapply(datos[vars], function(x) sum(!is.na(x)))
)
print(descripcion, row.names = FALSE)

# -----------------
# Tablas de frecuencias
# -----------------
for (v in vars) {
  cat("\nVariable:", v, "\n")
  print(round(prop.table(table(datos[[v]])) * 100, 2))
}

# -----------------
# Graficos univariados
# -----------------
par(mfrow = c(2, 2))
barplot(table(datos$m_p38d), main = "m_p38d: Bibl. niñez", col = "steelblue", names.arg = "Sí")
barplot(table(datos$m_p38d_1), main = "m_p38d_1: Frecuencia visitas", col = "orange", xlab = "1:Semanal a 5:Ocasional")
barplot(table(datos$m_p39_1), main = "m_p39_1: Petición libros no texto", col = c("forestgreen", "tomato"), names.arg = c("Sí", "No"))
barplot(table(datos$m_p39_4), main = "m_p39_4: Motivación docente a leer", col = c("forestgreen", "tomato"), names.arg = c("Sí", "No"))
par(mfrow = c(1, 1))

# Hacer los cruces de cada variables con las variables: m_p33, m_p36_2 y niv
cruces = c("m_p33", "m_p36_2", "niv")
for (cruz in cruces) {
  cat("\n-------------------------------------------------------------")
  cat("\n>>> CRUCES CONTRA:", cruz, "\n")
  for (v in c("m_p38d_1", "m_p39_1", "m_p39_4")) {
    cat("\n--", v, "vs", cruz, "--\n")
    tab <- table(datos[[v]], datos[[cruz]])
    print(round(prop.table(tab, 1) * 100, 1))
    chi <- suppressWarnings(chisq.test(tab))
    cat(sprintf("Chi2 = %.2f, p-valor = %.4g\n", chi$statistic, chi$p.value))
  }
}

# Graficos de los cruces
par(mfrow = c(1, 3))
barplot(prop.table(table(datos$m_p33, datos$m_p39_1), 2) * 100, beside = TRUE,
        col = c("salmon", "gold", "skyblue", "springgreen3"),
        main = "Comprensión (33) vs 39_1", xlab = "39_1 (1=Sí, 2=No)",
        legend.text = c("Poca", "Mitad", "Mayor", "Toda"))

barplot(prop.table(table(datos$m_p36_2, datos$m_p39_4), 2) * 100, beside = TRUE,
        col = c("salmon", "gold", "springgreen3"),
        main = "Concentración (36_2) vs 39_4", xlab = "39_4 (1=Sí, 2=No)",
        legend.text = c("Difícil", "Regular", "Fácil"))

barplot(prop.table(table(datos$niv, datos$m_p38d_1), 2) * 100,
        col = terrain.colors(11),
        main = "Escolaridad (niv) vs 38d_1", xlab = "Frecuencia (1 a 5)")
par(mfrow = c(1, 1))

# -----------------
# Interpretacion de los resultados
# -----------------
cat("1. Los estímulos escolares de primaria (m_p39_1 y m_p39_4) se asocian de forma estadísticamente\n",
    "   significativa con una mejor comprensión (p = 0.004) y mayor facilidad para concentrarse (p < 0.001).\n",
    "2. En la infancia, toda la muestra analizada asistió a bibliotecas o librerías (m_p38d = 100%),\n",
    "   predominando una frecuencia semanal (69.7%) y registrándose sólo 0.64% que 'no recuerda' (convertido a NA).\n",
    "3. La frecuencia de asistencia a bibliotecas (m_p38d_1) se asocia con un mayor nivel de escolaridad (p < 0.001),\n",
    "   donde más de la mitad de los visitantes frecuentes alcanzó estudios profesionales o de posgrado.\n",
    "4. En conclusión, los estímulos lectores tempranos (en casa y primaria) impactan positivamente tanto en el\n",
    "   nivel de escolaridad alcanzado como en las competencias de comprensión y atención en la vida adulta.\n")
