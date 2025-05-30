library(ggplot2)
library(dplyr)

# --- Anillo externo: vertical formativa ---
outer_data <- data.frame(
  categoria = c("Empresa", "Tecnología", "Salud"),
  porcentaje = c(52, 26, 22),
  etiqueta = c("60%", "21%", "19%")
) %>%
  mutate(
    grupo = "Vertical",
    ymax = cumsum(porcentaje),
    ymin = lag(ymax, default = 0),
    label_pos = (ymin + ymax) / 2,
    xmin = 3,
    xmax = 4
  )

# --- Anillo interno: sólo 2/3 de Salud (39 alumnos), dividido en AP y LCB ---
total_salud <- 71
desglosado <- 39  # 2/3 de Salud

# Fracción de salud desglosada respecto al total (%)
salud_pct <- 22
salud_2_tercios_pct <- salud_pct * 2 / 3  # ≈ 14.67%

# Porcentaje de cada categoría dentro de esos 2/3
AP_pct <- (18 / desglosado) * salud_2_tercios_pct  # ≈ 6.77%
LCB_pct <- (21 / desglosado) * salud_2_tercios_pct # ≈ 7.89%
resto_pct <- 100 - (AP_pct + LCB_pct)              # ≈ 85.33%

inner_data <- data.frame(
  categoria = c("Relleno", "AP", "LCB"),
  porcentaje = c(resto_pct, AP_pct, LCB_pct),
  etiqueta = c("", "18", "21")
) %>%
  mutate(
    grupo = "Ciclo",
    ymax = cumsum(porcentaje),
    ymin = lag(ymax, default = 0),
    label_pos = (ymin + ymax) / 2,
    xmin = 2,
    xmax = 3
  )

# --- Unión de ambos donuts ---
donut_data <- bind_rows(outer_data, inner_data)

# --- Gráfico completo ---
ggplot(donut_data) +
  geom_rect(aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax,
                fill = categoria), color = "white") +
  # Etiquetas externas (porcentajes)
  geom_text(
    data = donut_data %>% filter(grupo == "Vertical"),
    aes(x = 3.5, y = label_pos, label = etiqueta),
    color = "white", size = 5
  ) +
  # Etiquetas internas (número de alumnos)
  geom_text(
    data = donut_data %>% filter(grupo == "Ciclo" & categoria != "Relleno"),
    aes(x = 2.5, y = label_pos, label = etiqueta),
    color = "black", size = 5, fontface = "bold"
  ) +
  coord_polar(theta = "y") +
  xlim(c(1, 4)) +
  scale_fill_manual(values = c(
    "Empresa" = "seagreen",
    "Tecnología" = "darkorange",
    "Salud" = "steelblue",
    "AP" = "orchid",
    "LCB" = "gold",
    "Relleno" = "white"
  ),
  breaks = c("Empresa", "Tecnología", "Salud", "AP", "LCB")) +
  theme_void() +
  labs(
    title = "Distribución del alumnado por vertical y ciclos dentro de Salud",
    fill = "Categoría"
  ) +
  theme(plot.title = element_text(hjust = 0.5))
