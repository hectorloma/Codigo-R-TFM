# Cargar paquetes necesarios
library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)

# Cargar datos
datos <- read_excel("Encuesta_Bioinformatica_AP_LCB.xlsx")

# Crear carpeta para guardar gráficos
dir.create("graficos_preguntas_AP_LCB", showWarnings = FALSE)

# Definir preguntas (todas menos Vertical, Grupo, Momento)
preguntas <- names(datos)[!(names(datos) %in% c("Vertical", "Grupo", "Momento"))]

# Inicializar resultados
resultados_normalidad <- data.frame()
resultados_test <- data.frame()

# Bucle por pregunta
for (i in seq_along(preguntas)) {
  pregunta <- preguntas[i]
  
  # Gráfico de caja
  grafico <- ggplot(datos, aes(x = Momento, y = .data[[pregunta]], fill = Momento)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = paste0("Pregunta ", i),
         y = "Puntuación Likert", x = "Momento")
  
  ggsave(filename = paste0("graficos_preguntas/Pregunta_", i, ".png"),
         plot = grafico, width = 6, height = 4, bg = "white")
  
  # Test de normalidad por momento
  cumple_normalidad <- TRUE
  for (grupo in unique(datos$Momento)) {
    valores <- datos %>%
      filter(Momento == grupo) %>%
      pull(pregunta)
    
    if (length(valores) >= 3) {
      p_shapiro <- shapiro.test(valores)$p.value
      resultados_normalidad <- rbind(resultados_normalidad, data.frame(
        Pregunta = paste0("Pregunta ", i),
        Momento = grupo,
        p_shapiro = round(p_shapiro, 4),
        Cumple_normalidad = p_shapiro > 0.05
      ))
      if (p_shapiro <= 0.05) cumple_normalidad <- FALSE
    }
  }
  
  # Usar Wilcoxon siempre porque los datos son ordinales
  formula <- as.formula(paste0("`", pregunta, "` ~ Momento"))
  p_valor <- wilcox.test(formula, data = datos)$p.value
  
  resultados_test <- rbind(resultados_test, data.frame(
    Pregunta = paste0("Pregunta ", i),
    Test_usado = "Wilcoxon Rank-Sum",
    p_value = round(p_valor, 4)
  ))
}

# Calcular medias y SD por pregunta y momento
media_sd <- datos %>%
  group_by(Momento) %>%
  summarise(across(all_of(preguntas), list(Media = ~round(mean(.), 2),
                                           SD = ~round(sd(.), 2))))

# Calcular media total
datos$Media_Total <- rowMeans(datos[, preguntas], na.rm = TRUE)

# Test de normalidad en Media Total
normalidad_media_total <- datos %>%
  group_by(Momento) %>%
  summarise(
    p_shapiro = round(shapiro.test(Media_Total)$p.value, 4),
    Cumple_normalidad = shapiro.test(Media_Total)$p.value > 0.05
  )

# Comparación Media Total entre momentos (Wilcoxon)
test_global <- wilcox.test(Media_Total ~ Momento, data = datos)
resultado_global <- data.frame(
  Test_usado = "Wilcoxon Rank-Sum",
  p_value = round(test_global$p.value, 4)
)

# Media y SD total por momento
media_total_por_momento <- datos %>%
  group_by(Momento) %>%
  summarise(
    Media = round(mean(Media_Total), 2),
    SD = round(sd(Media_Total), 2)
  )

# Gráfico Media Total
grafico_media_total <- ggplot(media_total_por_momento, aes(x = Momento, y = Media, fill = Momento)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = Media - SD, ymax = Media + SD), width = 0.2) +
  labs(
    title = "Media Total de Puntuaciones Antes vs. Después",
    y = "Media Total (1–5)",
    x = "Momento"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("grafico_media_total.png", plot = grafico_media_total, width = 6, height = 4, bg = "white")

# Guardar todo en un archivo Excel
write_xlsx(list(
  "Normalidad_por_Pregunta" = resultados_normalidad,
  "Test_Wilcoxon_por_Pregunta" = resultados_test,
  "Medias_y_Desviaciones" = media_sd,
  "Normalidad_Media_Total" = normalidad_media_total,
  "Test_Global_Media_Total" = resultado_global,
  "Media_Total_por_Momento" = media_total_por_momento
), "Resultados_Pre_Post_Bioinformatica.xlsx")
