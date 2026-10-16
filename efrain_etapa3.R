
# Cargar base de datos
datos <- read.csv("MOLEC_2025_limpia.csv", stringsAsFactors = FALSE)

# Inspección inicial
cat("Dimensiones de la base limpia:", dim(datos), "\n")


datos_niv <- datos[datos$niv != 99, ]

# Funcion general para la prueba usando la distribucion F
ejecutar_prueba_f <- function(x1, x2, var_nombre, etiqueta_g1, etiqueta_g2, alfa = 0.05) {
  x1 <- na.omit(x1)
  x2 <- na.omit(x2)
  
  n1 <- length(x1); n2 <- length(x2)
  media1 <- mean(x1); media2 <- mean(x2)
  var1 <- var(x1); var2 <- var(x2)
  sd1 <- sd(x1); sd2 <- sd(x2)
  
  # Prueba F (var.test en R)
  prueba <- var.test(x1, x2, alternative = "two.sided", conf.level = 1 - alfa)
  
  f_calc <- unname(prueba$statistic)
  gl1 <- prueba$parameter[1]
  gl2 <- prueba$parameter[2]
  pval <- prueba$p.value
  ic_inf <- prueba$conf.int[1]
  ic_sup <- prueba$conf.int[2]
  
  # Valores críticos
  f_crit_inf <- qf(alfa / 2, gl1, gl2)
  f_crit_sup <- qf(1 - alfa / 2, gl1, gl2)
  
  decision <- ifelse(pval < alfa, "Rechazar H0", "No rechazar H0")
  
  resultado <- list(
    Variable = var_nombre,
    Grupo1 = etiqueta_g1,
    Grupo2 = etiqueta_g2,
    n1 = n1, media1 = media1, var1 = var1, sd1 = sd1,
    n2 = n2, media2 = media2, var2 = var2, sd2 = sd2,
    F_calc = f_calc,
    gl1 = gl1, gl2 = gl2,
    F_crit_inf = f_crit_inf,
    F_crit_sup = f_crit_sup,
    p_valor = pval,
    IC_95 = c(ic_inf, ic_sup),
    Decision = decision,
    prueba_obj = prueba
  )
  return(resultado)
}


# Escolaridad según si los maestros pedian otros libros
d1 <- datos_niv[datos_niv$m_p39_1 %in% c(1, 2), ]
res1 <- ejecutar_prueba_f(
  x1 = d1$niv[d1$m_p39_1 == 1],
  x2 = d1$niv[d1$m_p39_1 == 2],
  var_nombre = "Nivel de Escolaridad (niv)",
  etiqueta_g1 = "Maestros pedían otros libros (Sí)",
  etiqueta_g2 = "Maestros no pedían (No)"
)


# Escolaridad según si los maestros animaban a visitar bibliotecas
d2 <- datos_niv[datos_niv$m_p39_4 %in% c(1, 2), ]
res2 <- ejecutar_prueba_f(
  x1 = d2$niv[d2$m_p39_4 == 1],
  x2 = d2$niv[d2$m_p39_4 == 2],
  var_nombre = "Nivel de Escolaridad (niv)",
  etiqueta_g1 = "Maestros animaban bibl. (Sí)",
  etiqueta_g2 = "Maestros no animaban (No)"
)

# Escolaridad según la capacidad de concentrarse
datos_niv$capaz_num <- ifelse(datos_niv$m_p36_2 == 3, "Capaz", "Dificultad")
res3 <- ejecutar_prueba_f(
  x1 = datos_niv$niv[datos_niv$m_p36_2 == 3],
  x2 = datos_niv$niv[datos_niv$m_p36_2 %in% c(1, 2)],
  var_nombre = "Nivel de Escolaridad (niv)",
  etiqueta_g1 = "Capaz de concentrarse (No difícil, 3)",
  etiqueta_g2 = "Dificultad para concentrarse (1 o 2)"
)


# Velocidad de lectura según si el alumno veía a sus padres leer
d4 <- datos[datos$m_p38b %in% c(1, 2), ]
res4 <- ejecutar_prueba_f(
  x1 = d4$m_p32[d4$m_p38b == 1],
  x2 = d4$m_p32[d4$m_p38b == 2],
  var_nombre = "Velocidad de Lectura (m_p32)",
  etiqueta_g1 = "Veía a sus padres leer (Sí)",
  etiqueta_g2 = "No veía a sus padres leer (No)"
)

# Velocidad de lectura según si los padres le leían
d5 <- datos[datos$m_p38c %in% c(1, 2), ]
res5 <- ejecutar_prueba_f(
  x1 = d5$m_p32[d5$m_p38c == 1],
  x2 = d5$m_p32[d5$m_p38c == 2],
  var_nombre = "Velocidad de Lectura (m_p32)",
  etiqueta_g1 = "Padres le leían de niño (Sí)",
  etiqueta_g2 = "Padres no le leían (No)"
)

# Tablas resumen
lista_pruebas <- list(res1, res2, res3, res4, res5)
tabla_consolidada <- data.frame(
  Contraste = paste0("Prueba ", 1:5),
  Variable = sapply(lista_pruebas, function(x) x$Variable),
  Comparacion = sapply(lista_pruebas, function(x) paste(x$Grupo1, "vs", x$Grupo2)),
  n1 = sapply(lista_pruebas, function(x) x$n1),
  Var1 = sapply(lista_pruebas, function(x) round(x$var1, 4)),
  n2 = sapply(lista_pruebas, function(x) x$n2),
  Var2 = sapply(lista_pruebas, function(x) round(x$var2, 4)),
  F_calc = sapply(lista_pruebas, function(x) round(x$F_calc, 4)),
  gl1 = sapply(lista_pruebas, function(x) x$gl1),
  gl2 = sapply(lista_pruebas, function(x) x$gl2),
  F_crit_inf = sapply(lista_pruebas, function(x) round(x$F_crit_inf, 4)),
  F_crit_sup = sapply(lista_pruebas, function(x) round(x$F_crit_sup, 4)),
  p_valor = sapply(lista_pruebas, function(x) format.pval(x$p_valor, digits = 4, eps = 0.0001)),
  IC_95 = sapply(lista_pruebas, function(x) paste0("[", round(x$IC_95[1], 4), ", ", round(x$IC_95[2], 4), "]")),
  Decision = sapply(lista_pruebas, function(x) x$Decision)
)

print(tabla_consolidada)

# Guardar resultados en un archivo CSV para consulta rápida
write.csv(tabla_consolidada, "efrain_etapa3_resumen.csv", row.names = FALSE)
cat("\nResultados guardados exitosamente en efrain_etapa3_resumen.csv\n")
