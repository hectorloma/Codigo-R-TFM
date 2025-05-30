# Cargar paquetes
library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)

# Cargar datos
datos <- read_excel("Encuesta_Bioinformatica_Resultados.xlsx")

# Crear carpeta para gráficos
dir.create("graficos_preguntas", showWarnings = FALSE)

# Preguntas (todas menos la última)
preguntas <- names(datos)[1:(ncol(datos) - 1)]

# Tablas de resultados
resultados_normalidad <- data.frame()
resultados_test <- data.frame()
comparaciones_salud_tecnologia <- data.frame()

# Bucle por pregunta
for (i in seq_along(preguntas)) {
  pregunta <- preguntas[i]
  
  # Gráfico de caja simplificado
  grafico <- ggplot(datos, aes(x = Vertical, y = .data[[pregunta]], fill = Vertical)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = paste0("Pregunta ", i),
         y = "Puntuación Likert", x = "Vertical")
  
  ggsave(filename = paste0("graficos_preguntas/Pregunta_", i, ".png"),
         plot = grafico, width = 6, height = 4, bg = "white")
  
  # Evaluar normalidad por grupo
  cumple_normalidad <- TRUE
  for (grupo in unique(datos$Vertical)) {
    valores <- datos %>%
      filter(Vertical == grupo) %>%
      pull(pregunta)
    
    if (length(valores) >= 3) {
      p_shapiro <- shapiro.test(valores)$p.value
      resultados_normalidad <- rbind(resultados_normalidad, data.frame(
        Pregunta = paste0("Pregunta ", i),
        Vertical = grupo,
        p_shapiro = round(p_shapiro, 4),
        Cumple_normalidad = p_shapiro > 0.05
      ))
      if (p_shapiro <= 0.05) cumple_normalidad <- FALSE
    }
  }
  
  # ANOVA o Kruskal-Wallis
  formula <- as.formula(paste0("`", pregunta, "` ~ Vertical"))
  if (cumple_normalidad) {
    modelo <- aov(formula, data = datos)
    p_valor <- summary(modelo)[[1]]$'Pr(>F)'[1]
    test_usado <- "ANOVA"
  } else {
    p_valor <- kruskal.test(formula, data = datos)$p.value
    test_usado <- "Kruskal-Wallis"
  }
  
  resultados_test <- rbind(resultados_test, data.frame(
    Pregunta = paste0("Pregunta ", i),
    Test_usado = test_usado,
    p_value = round(p_valor, 4)
  ))
  # Comparaciones 2 a 2 entre verticales para esta pregunta
  comparaciones_pares <- combn(unique(datos$Vertical), 2, simplify = FALSE)
  
  for (par in comparaciones_pares) {
    grupo1 <- par[1]
    grupo2 <- par[2]
    
    datos_par <- datos %>%
      filter(Vertical %in% c(grupo1, grupo2)) %>%
      mutate(Vertical = factor(Vertical))
    
    formula_par <- as.formula(paste0("`", pregunta, "` ~ Vertical"))
    p_val_par <- kruskal.test(formula_par, data = datos_par)$p.value
    
    comparaciones_salud_tecnologia <- rbind(comparaciones_salud_tecnologia, data.frame(
      Pregunta = paste0("Pregunta ", i),
      Comparacion = paste(grupo1, "vs", grupo2),
      p_value = round(p_val_par, 4)
    ))
  }
  
  
}

# Calcular medias y desviaciones por vertical
media_sd <- datos %>%
  group_by(Vertical) %>%
  summarise(across(all_of(preguntas), list(Media = ~round(mean(.), 2),
                                           SD = ~round(sd(.), 2))))
# Añadir columna con media total por estudiante
datos$Media_Total <- rowMeans(datos[, preguntas], na.rm = TRUE)

# Evaluar normalidad global por grupo
test_normalidad_media <- datos %>%
  group_by(Vertical) %>%
  summarise(
    p_shapiro = round(shapiro.test(Media_Total)$p.value, 4),
    Cumple_normalidad = shapiro.test(Media_Total)$p.value > 0.05
  )

# Decidir test global
formula_media <- Media_Total ~ Vertical
if (all(test_normalidad_media$Cumple_normalidad)) {
  modelo_global <- aov(formula_media, data = datos)
  p_global <- summary(modelo_global)[[1]]$'Pr(>F)'[1]
  test_global <- "ANOVA"
} else {
  p_global <- kruskal.test(formula_media, data = datos)$p.value
  test_global <- "Kruskal-Wallis"
}

# Resultado resumen
resultado_global <- data.frame(
  Test_usado = test_global,
  p_value = round(p_global, 4)
)

# Calcular media total por vertical (Media_Total)
media_total_por_vertical <- datos %>%
  group_by(Vertical) %>%
  summarise(
    Media = round(mean(Media_Total), 2),
    SD = round(sd(Media_Total), 2)
  )

grafico_media_total <- ggplot(media_total_por_vertical, aes(x = Vertical, y = Media, fill = Vertical)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = Media - SD, ymax = Media + SD), width = 0.2) +
  labs(
    title = "Interés General en Bioinformática por Vertical",
    y = "Media Total (escala 1–5)",
    x = "Vertical"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Guardar gráfico
ggsave("grafico_media_total.png", plot = grafico_media_total, width = 6, height = 4, bg = "white")


# Crear combinaciones 2 a 2
grupos <- unique(datos$Vertical)
comparaciones_2a2 <- combn(grupos, 2, simplify = FALSE)

# Comparaciones de Media_Total por pares (Mann-Whitney)
resultados_pares <- data.frame()

for (par in comparaciones_2a2) {
  grupo1 <- par[1]
  grupo2 <- par[2]
  
  df_par <- datos %>%
    filter(Vertical %in% c(grupo1, grupo2)) %>%
    mutate(Vertical = factor(Vertical))
  
  test_par <- wilcox.test(Media_Total ~ Vertical, data = df_par)
  
  resultados_pares <- rbind(resultados_pares, data.frame(
    Comparacion = paste(grupo1, "vs", grupo2),
    Test = "Wilcoxon Rank-Sum",
    p_value = round(test_par$p.value, 4)
  ))
}




# Agregar al Excel final
write_xlsx(list(
  "Normalidad" = resultados_normalidad,
  "Test_Estadistico_Global" = resultados_test,
  "Comparacion_Salud_vs_Tecnologia" = comparaciones_salud_tecnologia,
  "Medias_y_Desviaciones" = media_sd,
  "Media_Total_por_Vertical" = media_total_por_vertical,
  "Normalidad_Media_Total" = test_normalidad_media,
  "Test_Global_Media_Total" = resultado_global,
  "Comparacion_Media_Total_ST" = resultados_pares
), "Resultados_Normalidad_y_Test_Estadistico.xlsx")
